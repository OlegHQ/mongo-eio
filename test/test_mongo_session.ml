open Alcotest

let byte s idx = Char.code s.[idx]

let test_session_id_is_uuid_v4 () =
  let session = Mongo_session.create () in
  check int "uuid length" 16 (String.length session.id);
  check int "uuid version" 4 ((byte session.id 6 land 0xf0) lsr 4);
  check int "uuid variant" 2 ((byte session.id 8 land 0xc0) lsr 6)

let test_lsid_uses_uuid_binary_subtype () =
  let session = Mongo_session.create () in
  let command =
    Mongo_command.enrich_command ~db:"admin"
      ~session:(Mongo_session.implicit_context session)
      [ ("ping", Bson.create_int32 1l) ]
  in
  let lsid = Bson.get_doc_element (Bson.get_element "lsid" command) in
  let id = Bson.get_uuid_binary (Bson.get_element "id" lsid) in
  check string "lsid id" session.id id

let test_transaction_context_fields () =
  let session = Mongo_session.create () in
  let command =
    Mongo_command.enrich_command ~db:"admin"
      ~session:(Mongo_session.transaction_context ~start:true session ~txn_number:7L)
      [ ("insert", Bson.create_string "posts") ]
  in
  check int64 "txn number" 7L
    (Bson.get_int64 (Bson.get_element "txnNumber" command));
  check bool "start transaction" true
    (Bson.get_boolean (Bson.get_element "startTransaction" command));
  check bool "autocommit" false
    (Bson.get_boolean (Bson.get_element "autocommit" command))

let () =
  run "mongo_session"
    [
      ( "session",
        [
          test_case "session id is UUID v4" `Quick
            test_session_id_is_uuid_v4;
          test_case "lsid uses UUID binary subtype" `Quick
            test_lsid_uses_uuid_binary_subtype;
          test_case "transaction context fields" `Quick
            test_transaction_context_fields;
        ] );
    ]
