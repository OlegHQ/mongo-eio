let env name default =
  match Sys.getenv_opt name with Some "" | None -> default | Some value -> value

let host = env "POSTER_MONGO_HOST" "oracle-vm"
let port = env "POSTER_MONGO_PORT" "27017" |> int_of_string

let db =
  Printf.sprintf "poster_eio_direct_e2e_%d_%d" (Unix.getpid ())
    (Random.bits ())

let collection = "direct_client"

let doc fields =
  List.fold_right
    (fun (name, element) acc -> Bson.add_element name element acc)
    fields Bson.empty

let string name value = (name, Bson.create_string value)
let int32 name value = (name, Bson.create_int32 (Int32.of_int value))
let bool name value = (name, Bson.create_boolean value)

let assert_true label condition =
  if condition then Printf.printf "PASS %s\n%!" label
  else failwith ("FAIL " ^ label)

let ( let* ) result f =
  match result with Ok value -> f value | Error err -> Error err

let command_event_name = function
  | Mongo_command.Command_started event -> "started:" ^ event.command_name
  | Command_succeeded event -> "succeeded:" ^ event.command_name
  | Command_failed event -> "failed:" ^ event.command_name

let monitored_ping client =
  let events = ref [] in
  let command_event_handler event = events := event :: !events in
  let* _response =
    Mongo_eio.direct_run_command ~command_event_handler client db
      [ ("ping", Bson.create_int32 1l) ]
  in
  let event_names = List.rev !events |> List.map command_event_name in
  assert_true "direct command monitoring"
    (event_names = [ "started:ping"; "succeeded:ping" ]);
  Ok ()

let run_flow client =
  let filter = doc [ string "id" "eio-direct-1" ] in
  let initial =
    doc
      [
        string "id" "eio-direct-1";
        string "body" "created";
        int32 "score" 1;
        bool "done" false;
      ]
  in
  let update =
    doc
      [
        ( "$set",
          Bson.create_doc_element
            (doc [ string "body" "updated"; bool "done" true ]) );
      ]
  in
  let* insert_result =
    Mongo_eio.direct_insert_one client ~db ~collection initial
  in
  let* () = monitored_ping client in
  assert_true "direct insert_one" (insert_result.inserted_count = 1);
  let* found = Mongo_eio.direct_find_one client ~db ~collection filter in
  assert_true "direct find_one"
    (match found with
    | Some doc -> Bson.get_string (Bson.get_element "body" doc) = "created"
    | None -> false);
  let* update_result =
    Mongo_eio.direct_update_one client ~db ~collection ~upsert:false filter
      update
  in
  assert_true "direct update_one" (update_result.matched_count = 1);
  let* updated = Mongo_eio.direct_find_one client ~db ~collection filter in
  assert_true "direct find_one after update"
    (match updated with
    | Some doc -> Bson.get_boolean (Bson.get_element "done" doc)
    | None -> false);
  let* count = Mongo_eio.direct_count_documents client ~db ~collection () in
  assert_true "direct count_documents" (count = 1);
  let* delete_result =
    Mongo_eio.direct_delete_many client ~db ~collection Bson.empty
  in
  assert_true "direct delete_many" (delete_result.deleted_count = 1);
  Ok ()

let cleanup client =
  Mongo_eio.direct_run_command client db [ ("dropDatabase", Bson.create_int32 1l) ]
  |> Result.map (fun _ -> ())

let () =
  Random.self_init ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let config =
    {
      (Mongo_config.default ~host ~port ~database:db ())
      with
      max_pool_size = 4;
      min_pool_size = 1;
      socket_timeout_ms = Some 10_000;
      wait_queue_timeout_ms = 10_000;
    }
  in
  let result =
    Mongo_eio.with_direct_client ~sw ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~config (fun client ->
        Fun.protect
          ~finally:(fun () -> ignore (cleanup client))
          (fun () -> run_flow client))
  in
  match result with
  | Ok () -> ()
  | Error err -> failwith (Mongo_error.to_string err)
