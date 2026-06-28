open Alcotest

let doc fields = Mongo_command.document fields

let test_retryable_write_shapes () =
  let insert_one =
    [
      ("insert", Bson.create_string "posts");
      ("documents", Bson.create_doc_element_list [ Bson.empty ]);
    ]
  in
  let insert_many =
    [
      ("insert", Bson.create_string "posts");
      ("documents", Bson.create_doc_element_list [ Bson.empty; Bson.empty ]);
    ]
  in
  let update_one =
    [
      ("update", Bson.create_string "posts");
      ( "updates",
        Bson.create_doc_element_list
          [
            doc
              [
                ("q", Bson.create_doc_element Bson.empty);
                ("u", Bson.create_doc_element Bson.empty);
                ("multi", Bson.create_boolean false);
              ];
          ] );
    ]
  in
  let update_many =
    [
      ("update", Bson.create_string "posts");
      ( "updates",
        Bson.create_doc_element_list
          [
            doc
              [
                ("q", Bson.create_doc_element Bson.empty);
                ("u", Bson.create_doc_element Bson.empty);
                ("multi", Bson.create_boolean true);
              ];
          ] );
    ]
  in
  let delete_one =
    [
      ("delete", Bson.create_string "posts");
      ( "deletes",
        Bson.create_doc_element_list
          [
            doc
              [
                ("q", Bson.create_doc_element Bson.empty);
                ("limit", Bson.create_int32 1l);
              ];
          ] );
    ]
  in
  let delete_many =
    [
      ("delete", Bson.create_string "posts");
      ( "deletes",
        Bson.create_doc_element_list
          [
            doc
              [
                ("q", Bson.create_doc_element Bson.empty);
                ("limit", Bson.create_int32 0l);
              ];
          ] );
    ]
  in
  check bool "insert one" true
    (Mongo_retry.is_retryable_write_command insert_one);
  check bool "insert many" false
    (Mongo_retry.is_retryable_write_command insert_many);
  check bool "update one" true
    (Mongo_retry.is_retryable_write_command update_one);
  check bool "update many" false
    (Mongo_retry.is_retryable_write_command update_many);
  check bool "delete one" true
    (Mongo_retry.is_retryable_write_command delete_one);
  check bool "delete many" false
    (Mongo_retry.is_retryable_write_command delete_many)

let test_implicit_session_filter () =
  check bool "count supports sessions" true
    (Mongo_retry.supports_implicit_session
       [ ("count", Bson.create_string "posts") ]);
  check bool "find awaits session-aware cursor API" false
    (Mongo_retry.supports_implicit_session
       [ ("find", Bson.create_string "posts") ]);
  check bool "getMore awaits session-aware cursor API" false
    (Mongo_retry.supports_implicit_session
       [ ("getMore", Bson.create_string "posts") ]);
  check bool "hello excludes sessions" false
    (Mongo_retry.supports_implicit_session
       [ ("hello", Bson.create_int32 1l) ]);
  check bool "saslStart excludes sessions" false
    (Mongo_retry.supports_implicit_session
       [ ("saslStart", Bson.create_int32 1l) ])

let test_retryable_error_classification () =
  let command_error code labels =
    Mongo_error.Command
      {
        code;
        code_name = None;
        message = "retry";
        labels;
        write_errors = [];
        write_concern_errors = [];
      }
  in
  check bool "read network" true
    (Mongo_retry.retryable_read_error (Mongo_error.Network "reset"));
  check bool "read code" true
    (Mongo_retry.retryable_read_error (command_error (Some 91) []));
  check bool "read non-retry code" false
    (Mongo_retry.retryable_read_error (command_error (Some 2) []));
  check bool "write label" true
    (Mongo_retry.retryable_write_error
       (command_error None [ "RetryableWriteError" ]));
  check bool "write network" true
    (Mongo_retry.retryable_write_error (Mongo_error.Timeout "socket"));
  check bool "execution timeout is not retryable read" false
    (Mongo_retry.retryable_read_error
       (Mongo_error.Execution_timeout "operation exceeded time limit"));
  check bool "execution timeout is not retryable write" false
    (Mongo_retry.retryable_write_error
       (Mongo_error.Execution_timeout "operation exceeded time limit"))

let () =
  run "mongo_retry"
    [
      ( "retry",
        [
          test_case "retryable write shapes" `Quick
            test_retryable_write_shapes;
          test_case "implicit session filter" `Quick
            test_implicit_session_filter;
          test_case "retryable error classification" `Quick
            test_retryable_error_classification;
        ] );
    ]
