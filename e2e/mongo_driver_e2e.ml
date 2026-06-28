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
let get_int64 name doc = Bson.get_int64 (Bson.get_element name doc)
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

let raw_command mongo fields =
  Mongo_command.run_exn ~db:(Mongo.get_db_name mongo)
    ~request_id:(Int32.of_float (Unix.gettimeofday ()))
    (Mongo.get_file_descr mongo) fields

let cursor_doc reply = Bson.get_doc_element (Bson.get_element "cursor" reply)

let cursor_batch name cursor =
  Bson.get_list (Bson.get_element name cursor) |> List.map Bson.get_doc_element

let raw_find_with_batch mongo batch_size =
  raw_command mongo
    [
      ("find", Bson.create_string (Mongo.get_collection_name mongo));
      ("filter", Bson.create_doc_element Bson.empty);
      ("sort", Bson.create_doc_element (doc [ int32 "seq" 1 ]));
      ("batchSize", Bson.create_int32 (Int32.of_int batch_size));
    ]

let raw_kill_cursor mongo cursor =
  raw_command mongo
    [
      ("killCursors", Bson.create_string (Mongo.get_collection_name mongo));
      ("cursors", Bson.create_list [ Bson.create_int64 cursor ]);
    ]

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

      let cursor_docs =
        List.init 6 (fun i ->
            doc
              [
                string "id" (Printf.sprintf "cursor_%d" i);
                string "username" (Printf.sprintf "cursor_%d" i);
                int32 "seq" i;
              ])
      in
      Mongo.insert mongo cursor_docs;
      let first_cursor = raw_find_with_batch mongo 2 |> cursor_doc in
      let first_id = get_int64 "id" first_cursor in
      assert_true "find batch returns nonzero cursor" (first_id <> 0L);
      assert_true "first batch respects batchSize"
        (List.length (cursor_batch "firstBatch" first_cursor) = 2);
      let next_batch = Mongo.get_more_of_num mongo first_id 2 |> docs in
      assert_true "getMore returns next batch" (List.length next_batch = 2);

      let config =
        Mongo_config.default ~host ~port ~database:db ()
      in
      let conn =
        match Mongo_connection.connect config with
        | Ok conn -> conn
        | Error err -> failwith (Mongo_error.to_string err)
      in
      Fun.protect
        ~finally:(fun () -> Mongo_connection.close conn)
        (fun () ->
          let typed_cursor =
            match
              Mongo_cursor.find conn ~db ~collection ~sort:(doc [ int32 "seq" 1 ])
                ~batch_size:2 ()
            with
            | Ok cursor -> cursor
            | Error err -> failwith (Mongo_error.to_string err)
          in
          assert_true "typed cursor returns nonzero cursor"
            (Mongo_cursor.id typed_cursor <> 0L);
          assert_true "typed cursor first batch"
            (List.length (Mongo_cursor.batch typed_cursor) = 2);
          let typed_next =
            match Mongo_cursor.get_more ~batch_size:2 typed_cursor with
            | Ok batch -> batch
            | Error err -> failwith (Mongo_error.to_string err)
          in
          assert_true "typed cursor getMore returns next batch"
            (List.length typed_next = 2);
          ignore
            (match Mongo_cursor.kill typed_cursor with
            | Ok () -> ()
            | Error err -> failwith (Mongo_error.to_string err)));

      let modern_collection = "modern_crud" in
      let config = Mongo_config.default ~host ~port ~database:db () in
      let conn =
        match Mongo_connection.connect config with
        | Ok conn -> conn
        | Error err -> failwith (Mongo_error.to_string err)
      in
      Fun.protect
        ~finally:(fun () ->
          ignore
            (Mongo_connection.run_command conn db
               [ ("drop", Bson.create_string modern_collection) ]);
          Mongo_connection.close conn)
        (fun () ->
          let write_concern =
            Some
              {
                Mongo_command.w = Some (`Nodes 1);
                j = Some false;
                wtimeout_ms = Some 5_000;
              }
          in
          let insert_result =
            match
              Mongo_crud.insert_many
                ~options:{ Mongo_crud.ordered = false; write_concern }
                conn ~db ~collection:modern_collection
                [
                  doc [ string "kind" "modern"; int32 "score" 1 ];
                  doc [ string "kind" "modern"; int32 "score" 2 ];
                  doc [ string "kind" "other"; int32 "score" 3 ];
                ]
            with
            | Ok result -> result
            | Error err -> failwith (Mongo_error.to_string err)
          in
          assert_true "modern CRUD insert_many count"
            (insert_result.Mongo_crud.inserted_count = 3);

          let opts =
            {
              Mongo_crud.filter = doc [ string "kind" "modern" ];
              projection = Some (doc [ int32 "score" 1; int32 "_id" 0 ]);
              sort = Some (doc [ int32 "score" (-1) ]);
              skip = Some 0;
              limit = Some 2;
              batch_size = Some 2;
              read_concern = Some Mongo_command.Local;
            }
          in
          let modern_found =
            match Mongo_crud.find conn ~db ~collection:modern_collection opts with
            | Ok docs -> docs
            | Error err -> failwith (Mongo_error.to_string err)
          in
          assert_true "modern CRUD find options"
            (List.length modern_found = 2
            && Bson.has_element "score" (List.hd modern_found)
            && not (Bson.has_element "_id" (List.hd modern_found)));

          let update_result =
            match
              Mongo_crud.update_many ?write_concern conn ~db
                ~collection:modern_collection ~upsert:false
                (doc [ string "kind" "modern" ])
                (doc
                   [
                     ( "$set",
                       Bson.create_doc_element
                         (doc [ string "status" "updated" ]) );
                   ])
            with
            | Ok result -> result
            | Error err -> failwith (Mongo_error.to_string err)
          in
          assert_true "modern CRUD update_many count"
            (update_result.Mongo_crud.matched_count = 2);

          let counted =
            match
              Mongo_crud.count_documents conn ~db ~collection:modern_collection
                ~query:(doc [ string "status" "updated" ])
                ()
            with
            | Ok n -> n
            | Error err -> failwith (Mongo_error.to_string err)
          in
          assert_true "modern CRUD count_documents" (counted = 2);

          let estimated =
            match
              Mongo_crud.estimated_document_count conn ~db
                ~collection:modern_collection
            with
            | Ok n -> n
            | Error err -> failwith (Mongo_error.to_string err)
          in
          assert_true "modern CRUD estimated_document_count" (estimated = 3);

          let delete_result =
            match
              Mongo_crud.delete_many ?write_concern conn ~db
                ~collection:modern_collection (doc [ string "kind" "modern" ])
            with
            | Ok result -> result
            | Error err -> failwith (Mongo_error.to_string err)
          in
          assert_true "modern CRUD delete_many count"
            (delete_result.Mongo_crud.deleted_count = 2));

      let kill_cursor = raw_find_with_batch mongo 1 |> cursor_doc in
      let kill_id = get_int64 "id" kill_cursor in
      assert_true "second find returns nonzero cursor" (kill_id <> 0L);
      let killed = raw_kill_cursor mongo kill_id in
      let killed_ids =
        Bson.get_list (Bson.get_element "cursorsKilled" killed)
        |> List.map Bson.get_int64
      in
      assert_true "killCursors reports killed cursor"
        (List.exists (( = ) kill_id) killed_ids);
      Mongo.delete_all mongo Bson.empty;
      assert_true "delete cursor fixtures" (Mongo.count mongo = 0);

      ignore (Mongo.drop_index mongo "unique_username");
      assert_true "dropIndexes command"
        (Mongo.get_indexes mongo |> docs
        |> List.for_all (fun index ->
               try get_string "name" index <> "unique_username" with _ -> true));

      ignore (Mongo.drop_collection mongo);
      assert_true "drop collection command" true)
