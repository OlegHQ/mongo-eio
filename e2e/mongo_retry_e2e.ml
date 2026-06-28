let env name default =
  match Sys.getenv_opt name with Some "" | None -> default | Some value -> value

let host = env "MONGO_RETRY_HOST" "127.0.0.1"
let port = env "MONGO_RETRY_PORT" "27021" |> int_of_string
let replica_set = env "MONGO_RETRY_RS_NAME" "retryRs"

let db =
  Printf.sprintf "poster_retry_e2e_%d_%d" (Unix.getpid ()) (Random.bits ())

let collection = "retry_smoke"

let doc fields =
  List.fold_right
    (fun (name, element) acc -> Bson.add_element name element acc)
    fields Bson.empty

let string name value = (name, Bson.create_string value)

let assert_true label condition =
  if condition then Printf.printf "PASS %s\n%!" label
  else failwith ("FAIL " ^ label)

let expect_ok label = function
  | Ok value ->
      assert_true label true;
      value
  | Error err -> failwith (Mongo_error.to_string err)

let config ?(retry_reads = true) ?(retry_writes = true) () =
  {
    (Mongo_config.default ~host ~port ~database:db ())
    with
    retry_reads;
    retry_writes;
    replica_set = Some replica_set;
    socket_timeout_ms = Some 10_000;
    max_pool_size = 2;
  }

let configure_fail_command conn command =
  let data =
    doc
      [
        ("failCommands", Bson.create_list [ Bson.create_string command ]);
        ("closeConnection", Bson.create_boolean true);
      ]
  in
  expect_ok ("configure failCommand for " ^ command)
    (Mongo_connection.run_command conn "admin"
       [
         ("configureFailPoint", Bson.create_string "failCommand");
         ("mode", Bson.create_doc_element (doc [ ("times", Bson.create_int32 1l) ]));
         ("data", Bson.create_doc_element data);
       ])
  |> ignore

let disable_fail_command conn =
  ignore
    (Mongo_connection.run_command conn "admin"
       [
         ("configureFailPoint", Bson.create_string "failCommand");
         ("mode", Bson.create_string "off");
       ])

let retryable_find pool =
  Mongo_pool.run_command pool db
    [
      ("find", Bson.create_string collection);
      ("filter", Bson.create_doc_element (doc [ string "kind" "retry" ]));
    ]

let retryable_count pool =
  Mongo_pool.run_command pool db
    [
      ("count", Bson.create_string collection);
      ("query", Bson.create_doc_element (doc [ string "kind" "retry" ]));
    ]

let retryable_distinct pool =
  Mongo_pool.run_command pool db
    [
      ("distinct", Bson.create_string collection);
      ("key", Bson.create_string "status");
      ("query", Bson.create_doc_element (doc [ string "kind" "retry" ]));
    ]

let retryable_aggregate pool =
  Mongo_pool.run_command pool db
    [
      ("aggregate", Bson.create_string collection);
      ( "pipeline",
        Bson.create_doc_element_list
          [
            doc
              [
                ( "$match",
                  Bson.create_doc_element (doc [ string "kind" "retry" ]) );
              ];
          ] );
      ("cursor", Bson.create_doc_element Bson.empty);
    ]

let retryable_insert pool =
  Mongo_pool.run_command pool db
    [
      ("insert", Bson.create_string collection);
      ( "documents",
        Bson.create_doc_element_list
          [ doc [ string "kind" "retry-write"; string "status" "ok" ] ] );
      ("ordered", Bson.create_boolean true);
    ]

let retryable_update pool =
  let update_spec =
    doc
      [
        ("q", Bson.create_doc_element (doc [ string "kind" "retry-update" ]));
        ( "u",
          Bson.create_doc_element
            (doc
               [
                 ( "$set",
                   Bson.create_doc_element (doc [ string "status" "updated" ])
                 );
               ]) );
        ("multi", Bson.create_boolean false);
      ]
  in
  Mongo_pool.run_command pool db
    [
      ("update", Bson.create_string collection);
      ("updates", Bson.create_doc_element_list [ update_spec ]);
      ("ordered", Bson.create_boolean true);
    ]

let retryable_delete pool =
  let delete_spec =
    doc
      [
        ("q", Bson.create_doc_element (doc [ string "kind" "retry-delete" ]));
        ("limit", Bson.create_int32 1l);
      ]
  in
  Mongo_pool.run_command pool db
    [
      ("delete", Bson.create_string collection);
      ("deletes", Bson.create_doc_element_list [ delete_spec ]);
      ("ordered", Bson.create_boolean true);
    ]

let retryable_find_and_modify pool =
  Mongo_pool.run_command pool db
    [
      ("findAndModify", Bson.create_string collection);
      ("query", Bson.create_doc_element (doc [ string "kind" "retry-fam" ]));
      ( "update",
        Bson.create_doc_element
          (doc
             [
               ( "$set",
                 Bson.create_doc_element (doc [ string "status" "updated" ])
               );
             ]) );
      ("new", Bson.create_boolean true);
    ]

let nonretryable_find_fails () =
  let conn = expect_ok "connect no-retry client" (Mongo_connection.connect (config ~retry_reads:false ())) in
  Fun.protect
    ~finally:(fun () -> Mongo_connection.close conn)
    (fun () ->
      configure_fail_command conn "find";
      match
        Mongo_connection.run_command conn db
          [
            ("find", Bson.create_string collection);
            ("filter", Bson.create_doc_element (doc [ string "kind" "retry" ]));
          ]
      with
      | Error (Mongo_error.Network _) ->
          assert_true "non-pooled find sees failpoint network error" true
      | Error (Mongo_error.Timeout _) ->
          assert_true "non-pooled find sees failpoint network error" true
      | Error err -> failwith (Mongo_error.to_string err)
      | Ok _ -> failwith "find unexpectedly succeeded without retry")

let nonretryable_insert_fails () =
  let conn =
    expect_ok "connect no-retry write client"
      (Mongo_connection.connect (config ~retry_writes:false ()))
  in
  Fun.protect
    ~finally:(fun () -> Mongo_connection.close conn)
    (fun () ->
      configure_fail_command conn "insert";
      match
        Mongo_connection.run_command conn db
          [
            ("insert", Bson.create_string collection);
            ( "documents",
              Bson.create_doc_element_list
                [
                  doc
                    [
                      string "kind" "retry-write-no-retry";
                      string "status" "should-not-write";
                    ];
                ] );
            ("ordered", Bson.create_boolean true);
          ]
      with
      | Error (Mongo_error.Network _) ->
          assert_true "non-pooled insert sees failpoint network error" true
      | Error (Mongo_error.Timeout _) ->
          assert_true "non-pooled insert sees failpoint network error" true
      | Error err -> failwith (Mongo_error.to_string err)
      | Ok _ -> failwith "insert unexpectedly succeeded without retry")

let find_one conn kind =
  Mongo_crud.find_one conn ~db ~collection (doc [ string "kind" kind ])

let run_retryable_read setup_conn label command_name action verify =
  configure_fail_command setup_conn command_name;
  let retry_pool = Mongo_pool.create (config ()) in
  Fun.protect
    ~finally:(fun () -> Mongo_pool.close retry_pool)
    (fun () ->
      let response = expect_ok ("retryable pooled " ^ label) (action retry_pool) in
      verify response)

let () =
  Random.self_init ();
  let setup_conn = expect_ok "connect retry setup client" (Mongo_connection.connect (config ())) in
  Fun.protect
    ~finally:(fun () ->
      disable_fail_command setup_conn;
      ignore
        (Mongo_connection.run_command setup_conn db
           [ ("dropDatabase", Bson.create_int32 1l) ]);
      Mongo_connection.close setup_conn)
    (fun () ->
      expect_ok "retry fixture insert"
        (Mongo_crud.insert_one setup_conn ~db ~collection
           (doc [ string "kind" "retry"; string "status" "ok" ]))
      |> ignore;
      expect_ok "retry update fixture insert"
        (Mongo_crud.insert_one setup_conn ~db ~collection
           (doc [ string "kind" "retry-update"; string "status" "old" ]))
      |> ignore;
      expect_ok "retry delete fixture insert"
        (Mongo_crud.insert_one setup_conn ~db ~collection
           (doc [ string "kind" "retry-delete"; string "status" "old" ]))
      |> ignore;
      expect_ok "retry findAndModify fixture insert"
        (Mongo_crud.insert_one setup_conn ~db ~collection
           (doc [ string "kind" "retry-fam"; string "status" "old" ]))
      |> ignore;
      nonretryable_find_fails ();
      configure_fail_command setup_conn "find";
      let pool = Mongo_pool.create (config ()) in
      Fun.protect
        ~finally:(fun () -> Mongo_pool.close pool)
        (fun () ->
          let response = expect_ok "retryable pooled find" (retryable_find pool) in
          let docs = Mongo_command.cursor_batch response.body in
          assert_true "retryable pooled find returns document" (List.length docs = 1));
      run_retryable_read setup_conn "count" "count" retryable_count
        (fun response ->
          assert_true "retryable pooled count returns count"
            (Mongo_command.int_of_bson (Bson.get_element "n" response.body) = 1));
      run_retryable_read setup_conn "distinct" "distinct" retryable_distinct
        (fun response ->
          let values =
            Bson.get_list (Bson.get_element "values" response.body)
            |> List.map Bson.get_string
          in
          assert_true "retryable pooled distinct returns value"
            (List.exists (( = ) "ok") values));
      run_retryable_read setup_conn "aggregate" "aggregate" retryable_aggregate
        (fun response ->
          let docs = Mongo_command.cursor_batch response.body in
          assert_true "retryable pooled aggregate returns document"
            (List.length docs = 1));
      nonretryable_insert_fails ();
      configure_fail_command setup_conn "insert";
      let pool = Mongo_pool.create (config ()) in
      Fun.protect
        ~finally:(fun () -> Mongo_pool.close pool)
        (fun () ->
          ignore (expect_ok "retryable pooled insert" (retryable_insert pool));
          let found =
            expect_ok "retryable write find"
              (find_one setup_conn "retry-write")
          in
          assert_true "retryable write inserted document" (Option.is_some found));
      configure_fail_command setup_conn "update";
      let pool = Mongo_pool.create (config ()) in
      Fun.protect
        ~finally:(fun () -> Mongo_pool.close pool)
        (fun () ->
          ignore (expect_ok "retryable pooled update" (retryable_update pool));
          let updated = expect_ok "retryable update find" (find_one setup_conn "retry-update") in
          match updated with
          | Some doc ->
              assert_true "retryable update changed document"
                (Bson.get_string (Bson.get_element "status" doc) = "updated")
          | None -> failwith "missing retry-update document");
      configure_fail_command setup_conn "delete";
      let pool = Mongo_pool.create (config ()) in
      Fun.protect
        ~finally:(fun () -> Mongo_pool.close pool)
        (fun () ->
          ignore (expect_ok "retryable pooled delete" (retryable_delete pool));
          let deleted = expect_ok "retryable delete find" (find_one setup_conn "retry-delete") in
          assert_true "retryable delete removed document" (Option.is_none deleted));
      configure_fail_command setup_conn "findAndModify";
      let pool = Mongo_pool.create (config ()) in
      Fun.protect
        ~finally:(fun () -> Mongo_pool.close pool)
        (fun () ->
          let response =
            expect_ok "retryable pooled findAndModify"
              (retryable_find_and_modify pool)
          in
          let value = Bson.get_doc_element (Bson.get_element "value" response.body) in
          assert_true "retryable findAndModify changed document"
            (Bson.get_string (Bson.get_element "status" value) = "updated")))
