open Alcotest

let duplicate_key_doc =
  Bson.add_element "ok" (Bson.create_double 0.0)
    (Bson.add_element "code" (Bson.create_int32 11000l)
       (Bson.add_element "errmsg" (Bson.create_string "duplicate key") Bson.empty))

let doc fields =
  List.fold_right
    (fun (name, element) acc -> Bson.add_element name element acc)
    fields Bson.empty

let string_field name value = (name, Bson.create_string value)
let int32 name value = (name, Bson.create_int32 (Int32.of_int value))

let test_duplicate_key () =
  match Mongo_error.of_command_reply duplicate_key_doc with
  | Error (Mongo_error.Command err) ->
      check int "code" 11000 (Option.value err.code ~default:0);
      check bool "duplicate" true (Mongo_error.is_duplicate_key (Mongo_error.Command err))
  | Ok _ -> fail "expected command error"
  | Error other -> fail (Mongo_error.to_string other)

let test_labels_and_code_helpers () =
  let reply =
    doc
      [
        ("ok", Bson.create_double 0.0);
        int32 "code" 91;
        string_field "codeName" "ShutdownInProgress";
        string_field "errmsg" "node is shutting down";
        ( "errorLabels",
          Bson.create_list
            [
              Bson.create_string "RetryableWriteError";
              Bson.create_string "NoWritesPerformed";
            ] );
      ]
  in
  match Mongo_error.of_command_reply reply with
  | Error err ->
      check (option int) "code helper" (Some 91) (Mongo_error.code err);
      check (option string) "codeName helper" (Some "ShutdownInProgress")
        (Mongo_error.code_name err);
      check bool "retryable write label" true
        (Mongo_error.has_label err "RetryableWriteError");
      check (list string) "labels"
        [ "RetryableWriteError"; "NoWritesPerformed" ]
        (Mongo_error.labels err);
      check bool "to_string includes labels" true
        (String.contains (Mongo_error.to_string err) '[')
  | Ok _ -> fail "expected command error"

let test_write_errors_and_write_concern_errors () =
  let write_error =
    doc
      [
        int32 "index" 0;
        int32 "code" 11000;
        string_field "codeName" "DuplicateKey";
        string_field "errmsg" "duplicate key";
      ]
  in
  let write_concern_error =
    doc
      [
        int32 "code" 64;
        string_field "codeName" "WriteConcernFailed";
        string_field "errmsg" "waiting for replication timed out";
      ]
  in
  let reply =
    doc
      [
        ("ok", Bson.create_double 1.0);
        ("writeErrors", Bson.create_list [ Bson.create_doc_element write_error ]);
        ( "writeConcernErrors",
          Bson.create_list [ Bson.create_doc_element write_concern_error ] );
        ( "errorLabels",
          Bson.create_list [ Bson.create_string "RetryableWriteError" ] );
      ]
  in
  match Mongo_error.of_command_reply reply with
  | Error (Mongo_error.Command err) ->
      check int "write error count" 1 (List.length err.write_errors);
      check int "write concern error count" 1
        (List.length err.write_concern_errors);
      check bool "duplicate key from writeErrors" true
        (Mongo_error.is_duplicate_key (Mongo_error.Command err));
      check bool "label preserved" true
        (Mongo_error.has_label (Mongo_error.Command err) "RetryableWriteError");
      check string "message prefers write error" "duplicate key" err.message
  | Ok _ -> fail "expected command error"
  | Error other -> fail (Mongo_error.to_string other)

let test_max_time_ms_expired_is_timeout () =
  let reply =
    doc
      [
        ("ok", Bson.create_double 0.0);
        int32 "code" 50;
        string_field "codeName" "MaxTimeMSExpired";
        string_field "errmsg" "operation exceeded time limit";
      ]
  in
  match Mongo_error.of_command_reply reply with
  | Error (Mongo_error.Execution_timeout message) ->
      check string "timeout message" "operation exceeded time limit" message
  | Ok _ -> fail "expected timeout"
  | Error other -> fail (Mongo_error.to_string other)

let test_to_string_redacts () =
  let original = "mongodb://user:secret@host/db" in
  let redacted = Mongo_error.redact_uri original in
  check bool "changed" true (not (String.equal original redacted));
  check bool "mask present" true (String.contains redacted '*')

let () =
  run "mongo_error"
    [
      ( "command errors",
        [
          test_case "duplicate key" `Quick test_duplicate_key;
          test_case "labels and code helpers" `Quick
            test_labels_and_code_helpers;
          test_case "write error details" `Quick
            test_write_errors_and_write_concern_errors;
          test_case "maxTimeMS expired is timeout" `Quick
            test_max_time_ms_expired_is_timeout;
          test_case "uri redaction" `Quick test_to_string_redacts;
        ] );
    ]
