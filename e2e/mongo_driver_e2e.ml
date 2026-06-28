let env name default =
  match Sys.getenv_opt name with Some "" | None -> default | Some value -> value

let host = env "POSTER_MONGO_HOST" "oracle-vm"
let port = env "POSTER_MONGO_PORT" "27017" |> int_of_string

let db =
  Printf.sprintf "poster_driver_e2e_%d_%d" (Unix.getpid ())
    (Random.bits ())

let collection = "capabilities"

let doc fields =
  List.fold_right
    (fun (name, element) acc -> Bson.add_element name element acc)
    fields Bson.empty

let string name value = (name, Bson.create_string value)
let int32 name value = (name, Bson.create_int32 (Int32.of_int value))
let bool name value = (name, Bson.create_boolean value)

let get_string name doc = Bson.get_string (Bson.get_element name doc)
let assert_true label condition =
  if condition then Printf.printf "PASS %s\n%!" label
  else failwith ("FAIL " ^ label)

let docs reply = MongoReply.get_document_list reply

let find_one mongo query =
  match docs (Mongo.find_q_one mongo query) with
  | [ item ] -> item
  | [] -> failwith "expected one document, got zero"
  | _ -> failwith "expected one document, got many"

let expect_duplicate mongo duplicate =
  try
    Mongo.insert mongo [ duplicate ];
    failwith "duplicate insert unexpectedly succeeded"
  with
  | Mongo.Mongo_failed message ->
      assert_true "unique index rejects duplicate"
        (String.length message > 0)

let () =
  Random.self_init ();
  let mongo = Mongo.create host port db collection in
  Fun.protect
    ~finally:(fun () ->
      try
        ignore (Mongo.drop_database mongo);
        Mongo.destroy mongo
      with _ -> ())
    (fun () ->
      ignore (Mongo.drop_collection mongo);
      Mongo.ensure_simple_index
        ~options:Mongo.[ Unique true; Name "unique_username" ]
        mongo "username";
      assert_true "createIndexes command"
        (Mongo.get_indexes mongo |> docs
        |> List.exists (fun index ->
               try get_string "name" index = "unique_username" with _ -> false));

      let alice =
        doc
          [
            string "id" "user_1";
            string "username" "alice";
            string "body" "first";
            int32 "score" 1;
            bool "active" true;
          ]
      in
      let bob =
        doc
          [
            string "id" "user_2";
            string "username" "bob";
            string "body" "second";
            int32 "score" 2;
            bool "active" true;
          ]
      in
      Mongo.insert mongo [ alice; bob ];
      assert_true "insert command" (Mongo.count mongo = 2);

      let all = Mongo.find mongo |> docs in
      assert_true "find command returns inserted documents" (List.length all = 2);

      let alice_query = doc [ string "username" "alice" ] in
      let alice_found = find_one mongo alice_query in
      assert_true "find_q_one command"
        (get_string "id" alice_found = "user_1");

      let projection = doc [ int32 "username" 1; int32 "_id" 0 ] in
      let projected =
        Mongo.find_q_s_one mongo alice_query projection |> docs |> List.hd
      in
      assert_true "projection command includes requested field"
        (get_string "username" projected = "alice");
      assert_true "projection command excludes id"
        (not (Bson.has_element "id" projected));

      let update_doc = doc [ ("$set", Bson.create_doc_element (doc [ string "body" "updated" ])) ] in
      Mongo.update_one mongo (alice_query, update_doc);
      let updated = find_one mongo alice_query in
      assert_true "update_one command" (get_string "body" updated = "updated");

      let update_all =
        doc [ ("$set", Bson.create_doc_element (doc [ bool "active" false ])) ]
      in
      Mongo.update_all mongo (doc [ bool "active" true ], update_all);
      assert_true "update_all command"
        (Mongo.count ~query:(doc [ bool "active" false ]) mongo = 2);

      expect_duplicate mongo
        (doc
           [
             string "id" "user_3";
             string "username" "alice";
             string "body" "duplicate";
           ]);

      Mongo.delete_one mongo (doc [ string "username" "bob" ]);
      assert_true "delete_one command" (Mongo.count mongo = 1);

      Mongo.delete_all mongo Bson.empty;
      assert_true "delete_all command" (Mongo.count mongo = 0);

      ignore (Mongo.drop_index mongo "unique_username");
      assert_true "dropIndexes command"
        (Mongo.get_indexes mongo |> docs
        |> List.for_all (fun index ->
               try get_string "name" index <> "unique_username" with _ -> true));

      ignore (Mongo.drop_collection mongo);
      assert_true "drop collection command" true)
