let env name default =
  match Sys.getenv_opt name with Some "" | None -> default | Some value -> value

let uri =
  env "MONGO_AUTH_URI"
    "mongodb://root:secret@127.0.0.1:27018/admin?authMechanism=SCRAM-SHA-256"

let bad_uri =
  env "MONGO_AUTH_BAD_URI"
    "mongodb://root:wrong@127.0.0.1:27018/admin?authMechanism=SCRAM-SHA-256"

let db =
  Printf.sprintf "poster_auth_e2e_%d_%d" (Unix.getpid ()) (Random.bits ())

let collection = "auth_smoke"
let sha1_user = Printf.sprintf "poster_sha1_%d_%d" (Unix.getpid ()) (Random.bits ())
let sha1_password = "sha1-secret"

let doc fields =
  List.fold_right
    (fun (name, element) acc -> Bson.add_element name element acc)
    fields Bson.empty

let string name value = (name, Bson.create_string value)

let sha1_uri () =
  Printf.sprintf
    "mongodb://%s:%s@127.0.0.1:27018/admin?authMechanism=SCRAM-SHA-1"
    sha1_user sha1_password

let sha1_negotiated_uri () =
  Printf.sprintf "mongodb://%s:%s@127.0.0.1:27018/admin" sha1_user
    sha1_password

let assert_true label condition =
  if condition then Printf.printf "PASS %s\n%!" label
  else failwith ("FAIL " ^ label)

let parse_uri uri =
  match Mongo_uri.of_string uri with
  | Ok config -> config
  | Error err -> failwith (Mongo_error.to_string err)

let connect uri =
  match Mongo_connection.connect (parse_uri uri) with
  | Ok conn -> conn
  | Error err -> failwith (Mongo_error.to_string err)

let expect_ok label = function
  | Ok value ->
      assert_true label true;
      value
  | Error err -> failwith (Mongo_error.to_string err)

let expect_auth_failure () =
  match Mongo_connection.connect (parse_uri bad_uri) with
  | Ok conn ->
      Mongo_connection.close conn;
      failwith "bad password unexpectedly authenticated"
  | Error (Mongo_error.Authentication _) ->
      assert_true "wrong password returns authentication error" true
  | Error err -> failwith (Mongo_error.to_string err)

let create_sha1_user conn =
  let role =
    doc [ string "role" "readWriteAnyDatabase"; string "db" "admin" ]
  in
  expect_ok "create SCRAM-SHA-1 user"
    (Mongo_connection.run_command conn "admin"
       [
         ("createUser", Bson.create_string sha1_user);
         ("pwd", Bson.create_string sha1_password);
         ("roles", Bson.create_doc_element_list [ role ]);
         ("mechanisms", Bson.create_list [ Bson.create_string "SCRAM-SHA-1" ]);
       ])
  |> ignore

let drop_sha1_user conn =
  ignore
    (Mongo_connection.run_command conn "admin"
       [ ("dropUser", Bson.create_string sha1_user) ])

let expect_sha1_auth () =
  let conn = connect (sha1_uri ()) in
  Fun.protect
    ~finally:(fun () -> Mongo_connection.close conn)
    (fun () ->
      assert_true "SCRAM-SHA-1 authenticated connection" conn.authenticated;
      ignore
        (expect_ok "SCRAM-SHA-1 ping"
           (Mongo_connection.run_command conn "admin"
              [ ("ping", Bson.create_int32 1l) ])))

let expect_sha1_negotiated_auth () =
  let conn = connect (sha1_negotiated_uri ()) in
  Fun.protect
    ~finally:(fun () -> Mongo_connection.close conn)
    (fun () ->
      assert_true "SCRAM-SHA-1 negotiated authenticated connection"
        conn.authenticated;
      ignore
        (expect_ok "SCRAM-SHA-1 negotiated ping"
           (Mongo_connection.run_command conn "admin"
              [ ("ping", Bson.create_int32 1l) ])))

let () =
  Random.self_init ();
  expect_auth_failure ();
  let conn = connect uri in
  Fun.protect
    ~finally:(fun () ->
      ignore
        (Mongo_connection.run_command conn db
           [ ("dropDatabase", Bson.create_int32 1l) ]);
      drop_sha1_user conn;
      Mongo_connection.close conn)
    (fun () ->
      assert_true "authenticated connection" conn.authenticated;
      ignore
        (expect_ok "authenticated ping"
           (Mongo_connection.run_command conn "admin"
              [ ("ping", Bson.create_int32 1l) ]));
      expect_ok "authenticated insert"
        (Mongo_crud.insert_one conn ~db ~collection
           (doc [ string "kind" "auth"; string "status" "ok" ]))
      |> ignore;
      let found =
        expect_ok "authenticated find"
          (Mongo_crud.find_one conn ~db ~collection
             (doc [ string "kind" "auth" ]))
      in
      assert_true "authenticated find returns document" (Option.is_some found);
      create_sha1_user conn;
      expect_sha1_auth ();
      expect_sha1_negotiated_auth ())
