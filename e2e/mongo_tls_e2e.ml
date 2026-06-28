let env name default =
  match Sys.getenv_opt name with Some "" | None -> default | Some value -> value

let host = env "MONGO_TLS_HOST" "127.0.0.1"
let port = env "MONGO_TLS_PORT" "27019" |> int_of_string
let ca_file = env "MONGO_TLS_CA_FILE" ""

let db =
  Printf.sprintf "poster_tls_e2e_%d_%d" (Unix.getpid ()) (Random.bits ())

let collection = "tls_smoke"

let doc fields =
  List.fold_right
    (fun (name, element) acc -> Bson.add_element name element acc)
    fields Bson.empty

let string name value = (name, Bson.create_string value)

let assert_true label condition =
  if condition then Printf.printf "PASS %s\n%!" label
  else failwith ("FAIL " ^ label)

let tls_config ?(server_name = Some "localhost") () =
  if ca_file = "" then failwith "MONGO_TLS_CA_FILE is required";
  {
    (Mongo_config.default ~host ~port ~database:db ())
    with
    socket_timeout_ms = Some 10_000;
    tls =
      Mongo_config.Enabled
        {
          ca_file = Some ca_file;
          allow_invalid_certificates = false;
          server_name;
        };
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

let expect_hostname_failure () =
  match Mongo_connection.connect (tls_config ~server_name:(Some "wrong.localhost") ()) with
  | Ok conn ->
      Mongo_connection.close conn;
      failwith "TLS hostname verification unexpectedly succeeded"
  | Error (Mongo_error.Network _) ->
      assert_true "TLS hostname verification rejects wrong name" true
  | Error err -> failwith (Mongo_error.to_string err)

let () =
  Random.self_init ();
  expect_hostname_failure ();
  let conn = connect (tls_config ()) in
  Fun.protect
    ~finally:(fun () ->
      ignore
        (Mongo_connection.run_command conn db
           [ ("dropDatabase", Bson.create_int32 1l) ]);
      Mongo_connection.close conn)
    (fun () ->
      ignore
        (expect_ok "TLS ping"
           (Mongo_connection.run_command conn "admin"
              [ ("ping", Bson.create_int32 1l) ]));
      expect_ok "TLS insert"
        (Mongo_crud.insert_one conn ~db ~collection
           (doc [ string "kind" "tls"; string "status" "ok" ]))
      |> ignore;
      let found =
        expect_ok "TLS find"
          (Mongo_crud.find_one conn ~db ~collection
             (doc [ string "kind" "tls" ]))
      in
      assert_true "TLS find returns document" (Option.is_some found))
