let env name default =
  match Sys.getenv_opt name with Some "" | None -> default | Some value -> value

let host = env "MONGO_RS_HOST" "127.0.0.1"
let port = env "MONGO_RS_PORT" "27020" |> int_of_string
let replica_set = env "MONGO_RS_NAME" "rs0"

let db =
  Printf.sprintf "poster_rs_e2e_%d_%d" (Unix.getpid ()) (Random.bits ())

let collection = "rs_smoke"

let doc fields =
  List.fold_right
    (fun (name, element) acc -> Bson.add_element name element acc)
    fields Bson.empty

let string name value = (name, Bson.create_string value)

let assert_true label condition =
  if condition then Printf.printf "PASS %s\n%!" label
  else failwith ("FAIL " ^ label)

let config ?(replica_set_name = replica_set) () =
  {
    (Mongo_config.default ~host ~port ~database:db ())
    with
    replica_set = Some replica_set_name;
    socket_timeout_ms = Some 10_000;
  }

let connect config =
  match Mongo_connection.connect config with
  | Ok conn -> conn
  | Error err -> failwith (Mongo_error.to_string err)

let expect_ok label = function
  | Ok value ->
      assert_true label true;
      value
  | Error err -> failwith (Mongo_error.to_string err)

let expect_wrong_set_name_failure () =
  match Mongo_connection.connect (config ~replica_set_name:"wrong-rs" ()) with
  | Ok conn ->
      Mongo_connection.close conn;
      failwith "replicaSet mismatch unexpectedly connected"
  | Error (Mongo_error.Server_selection _) ->
      assert_true "replicaSet mismatch is rejected" true
  | Error err -> failwith (Mongo_error.to_string err)

let () =
  Random.self_init ();
  expect_wrong_set_name_failure ();
  let conn = connect (config ()) in
  Fun.protect
    ~finally:(fun () ->
      ignore
        (Mongo_connection.run_command conn db
           [ ("dropDatabase", Bson.create_int32 1l) ]);
      Mongo_connection.close conn)
    (fun () ->
      assert_true "replica set name parsed"
        ((Mongo_connection.server_info conn).set_name = Some replica_set);
      ignore
        (expect_ok "replica set ping"
           (Mongo_connection.run_command conn "admin"
              [ ("ping", Bson.create_int32 1l) ]));
      expect_ok "replica set insert"
        (Mongo_crud.insert_one conn ~db ~collection
           (doc [ string "kind" "replicaSet"; string "status" "ok" ]))
      |> ignore;
      let found =
        expect_ok "replica set find"
          (Mongo_crud.find_one conn ~db ~collection
             (doc [ string "kind" "replicaSet" ]))
      in
      assert_true "replica set find returns document" (Option.is_some found))
