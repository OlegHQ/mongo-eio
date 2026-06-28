let env name default =
  match Sys.getenv_opt name with Some "" | None -> default | Some value -> value

let host = env "POSTER_MONGO_HOST" "oracle-vm"
let port = env "POSTER_MONGO_PORT" "27017" |> int_of_string

let docs reply = MongoReply.get_document_list reply

let one_doc label reply =
  match docs reply with
  | [ doc ] -> doc
  | [] -> failwith ("FAIL " ^ label ^ ": no response document")
  | _ -> failwith ("FAIL " ^ label ^ ": too many response documents")

let assert_true label condition =
  if condition then Printf.printf "PASS %s\n%!" label
  else failwith ("FAIL " ^ label)

let has_field name doc = Bson.has_element name doc

let () =
  let admin = MongoAdmin.create host port in
  Fun.protect
    ~finally:(fun () -> MongoAdmin.destroy admin)
    (fun () ->
      let hello = MongoAdmin.hello admin |> one_doc "hello" in
      assert_true "admin hello command"
        (has_field "maxWireVersion" hello || has_field "isWritablePrimary" hello);

      let build_info = MongoAdmin.buildInfo admin |> one_doc "buildInfo" in
      assert_true "admin buildInfo command" (has_field "version" build_info);

      let list_databases =
        MongoAdmin.listDatabases admin |> one_doc "listDatabases"
      in
      assert_true "admin listDatabases command"
        (has_field "databases" list_databases);

      let list_commands = MongoAdmin.listCommands admin |> one_doc "listCommands" in
      assert_true "admin listCommands command"
        (has_field "commands" list_commands))
