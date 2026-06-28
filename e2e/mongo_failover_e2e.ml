let env name default =
  match Sys.getenv_opt name with Some "" | None -> default | Some value -> value

let split_on_char delimiter value =
  let rec loop start index acc =
    if index >= String.length value then
      List.rev (String.sub value start (index - start) :: acc)
    else if value.[index] = delimiter then
      loop (index + 1) (index + 1)
        (String.sub value start (index - start) :: acc)
    else loop start (index + 1) acc
  in
  if value = "" then [] else loop 0 0 []

let parse_host_port value =
  match String.rindex_opt value ':' with
  | None -> (value, Mongo_config.default_port)
  | Some index ->
      ( String.sub value 0 index,
        int_of_string
          (String.sub value (index + 1) (String.length value - index - 1)) )

let hosts =
  env "MONGO_FAILOVER_HOSTS"
    "127.0.0.1:27023,127.0.0.1:27024,127.0.0.1:27025"
  |> split_on_char ','
  |> List.map parse_host_port

let replica_set = env "MONGO_FAILOVER_RS_NAME" "failoverRs"

let db =
  Printf.sprintf "poster_failover_e2e_%d_%d" (Unix.getpid ()) (Random.bits ())

let collection = "failover_smoke"

let doc fields =
  List.fold_right
    (fun (name, element) acc -> Bson.add_element name element acc)
    fields Bson.empty

let string name value = (name, Bson.create_string value)
let int32 name value = (name, Bson.create_int32 value)
let bool name value = (name, Bson.create_boolean value)

let assert_true label condition =
  if condition then Printf.printf "PASS %s\n%!" label
  else failwith ("FAIL " ^ label)

let config () =
  match hosts with
  | [] -> failwith "MONGO_FAILOVER_HOSTS must not be empty"
  | (host, port) :: _ ->
      {
        (Mongo_config.default ~host ~port ~database:db ())
        with
        hosts;
        replica_set = Some replica_set;
        connect_timeout_ms = 2_000;
        socket_timeout_ms = Some 5_000;
        server_selection_timeout_ms = 20_000;
      }

let connect () =
  match Mongo_connection.connect (config ()) with
  | Ok conn -> conn
  | Error err -> failwith (Mongo_error.to_string err)

let expect_ok label = function
  | Ok value ->
      assert_true label true;
      value
  | Error err -> failwith (Mongo_error.to_string err)

let run_stepdown conn =
  match
    Mongo_connection.run_command conn "admin"
      [
        int32 "replSetStepDown" 20l;
        int32 "secondaryCatchUpPeriodSecs" 1l;
        bool "force" true;
      ]
  with
  | Ok _ -> assert_true "primary stepdown command accepted" true
  | Error (Mongo_error.Network _) | Error (Mongo_error.Timeout _) ->
      assert_true "primary stepdown disconnected old primary" true
  | Error (Mongo_error.Command command)
    when String.contains command.Mongo_error.message 's' ->
      assert_true "primary stepdown command returned stepdown error" true
  | Error err -> failwith (Mongo_error.to_string err)

let rec retry_until deadline_seconds label action =
  if Unix.gettimeofday () > deadline_seconds then
    failwith ("FAIL " ^ label ^ ": timed out");
  match action () with
  | Ok value ->
      assert_true label true;
      value
  | Error _ ->
      Unix.sleepf 0.5;
      retry_until deadline_seconds label action

let () =
  Random.self_init ();
  let first = connect () in
  let old_primary = first.Mongo_connection.port in
  Fun.protect
    ~finally:(fun () -> Mongo_connection.close first)
    (fun () ->
      assert_true "initial primary selected"
        (Mongo_connection.server_info first).is_writable_primary;
      expect_ok "initial insert"
        (Mongo_crud.insert_one first ~db ~collection
           (doc [ string "phase" "before-stepdown" ]))
      |> ignore;
      run_stepdown first);

  let deadline = Unix.gettimeofday () +. 30.0 in
  let second =
    retry_until deadline "reconnected after stepdown" (fun () ->
        match Mongo_connection.connect (config ()) with
        | Ok conn when conn.Mongo_connection.port <> old_primary -> Ok conn
        | Ok conn ->
            Mongo_connection.close conn;
            Error (Mongo_error.Server_selection "old primary was reselected")
        | Error err -> Error err)
  in
  Fun.protect
    ~finally:(fun () ->
      ignore
        (Mongo_connection.run_command second db
           [ ("dropDatabase", Bson.create_int32 1l) ]);
      Mongo_connection.close second)
    (fun () ->
      assert_true "new primary selected"
        (Mongo_connection.server_info second).is_writable_primary;
      expect_ok "post-failover insert"
        (Mongo_crud.insert_one second ~db ~collection
           (doc [ string "phase" "after-stepdown" ]))
      |> ignore;
      let found =
        expect_ok "post-failover find"
          (Mongo_crud.find_one second ~db ~collection
             (doc [ string "phase" "after-stepdown" ]))
      in
      assert_true "post-failover find returns document" (Option.is_some found))
