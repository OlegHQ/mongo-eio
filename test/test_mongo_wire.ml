open Alcotest

let test_encode_op_msg () =
  let body = Bson.add_element "ping" (Bson.create_int32 1l) Bson.empty in
  let encoded = Mongo_wire.encode_op_msg 42l body in
  check bool "message length" true (String.length encoded >= 16);
  let header = MongoHeader.decode_header (String.sub encoded 0 16) in
  check bool "opcode is OP_MSG" true
    (MongoHeader.get_op header = MongoOperation.OP_MSG)

let test_read_timeout () =
  let left, right = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close left;
      Unix.close right)
    (fun () ->
      match Mongo_wire.read_message_result ~timeout_ms:25 left with
      | Error (Mongo_error.Timeout _) -> ()
      | Ok _ -> fail "expected read timeout"
      | Error err -> fail (Mongo_error.to_string err))

let () =
  run "mongo_wire"
    [
      ( "wire",
        [
          test_case "encode op_msg" `Quick test_encode_op_msg;
          test_case "read timeout" `Quick test_read_timeout;
        ] );
    ]
