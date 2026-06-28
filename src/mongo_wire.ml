open MongoUtils

type reply = {
  request_id : int32;
  response_to : int32;
  flags : int32;
  body : Bson.t;
}

let encode_op_msg request_id body_doc =
  let body_buf = Buffer.create 128 in
  encode_int32 body_buf 0l;
  Buffer.add_char body_buf '\x00';
  Buffer.add_string body_buf (Bson.encode body_doc);
  let header =
    MongoHeader.encode_header
      (MongoHeader.create_request_header (Buffer.length body_buf) request_id
         MongoOperation.OP_MSG)
  in
  header ^ Buffer.contents body_buf

let timeout_seconds = function
  | None -> -1.0
  | Some ms when ms <= 0 -> -1.0
  | Some ms -> float_of_int ms /. 1000.0

let wait_readable ?timeout_ms fd =
  match Unix.select [ fd ] [] [] (timeout_seconds timeout_ms) with
  | [], _, _ -> Error (Mongo_error.Timeout "socket read timed out")
  | _ -> Ok ()

let wait_writable ?timeout_ms fd =
  match Unix.select [] [ fd ] [] (timeout_seconds timeout_ms) with
  | _, [], _ -> Error (Mongo_error.Timeout "socket write timed out")
  | _ -> Ok ()

exception Io_timeout of string

let with_alarm_timeout ?timeout_ms message f =
  match timeout_ms with
  | None | Some 0 -> f ()
  | Some ms ->
      let seconds = max 1 ((ms + 999) / 1000) in
      let previous = Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise (Io_timeout message))) in
      let previous_alarm = Unix.alarm seconds in
      Fun.protect
        ~finally:(fun () ->
          ignore (Unix.alarm 0);
          if previous_alarm > seconds then ignore (Unix.alarm (previous_alarm - seconds));
          ignore (Sys.signal Sys.sigalrm previous))
        f

let read_exact_transport ?timeout_ms transport len =
  let bytes = Bytes.create len in
  let rec loop offset =
    if offset = len then Ok bytes
    else
      let fd = Mongo_transport.file_descr transport in
      let ready =
        if Mongo_transport.is_tls transport then Ok ()
        else wait_readable ?timeout_ms fd
      in
      match ready with
      | Error err -> Error err
      | Ok () -> (
          let read =
            try
              with_alarm_timeout ?timeout_ms "socket read timed out" (fun () ->
                  Mongo_transport.read transport bytes ~off:offset
                    ~len:(len - offset))
            with Io_timeout message -> Error (Mongo_error.Timeout message)
          in
          match read with
          | Ok n ->
            if n = 0 then Error (Mongo_error.Network "socket closed by peer")
            else loop (offset + n)
          | Error err -> Error err)
  in
  loop 0

let write_all_transport ?timeout_ms transport data =
  let len = String.length data in
  let rec loop offset =
    if offset = len then Ok ()
    else
      let fd = Mongo_transport.file_descr transport in
      let ready =
        if Mongo_transport.is_tls transport then Ok ()
        else wait_writable ?timeout_ms fd
      in
      match ready with
      | Error err -> Error err
      | Ok () -> (
          let write =
            try
              with_alarm_timeout ?timeout_ms "socket write timed out" (fun () ->
                  Mongo_transport.write transport data ~off:offset
                    ~len:(len - offset))
            with Io_timeout message -> Error (Mongo_error.Timeout message)
          in
          match write with
          | Ok n -> loop (offset + n)
          | Error err -> Error err)
  in
  loop 0

let write_all ?timeout_ms fd data =
  write_all_transport ?timeout_ms (Mongo_transport.plain fd) data

let read_message_transport ?timeout_ms transport =
  match read_exact_transport ?timeout_ms transport 4 with
  | Error err -> Error err
  | Ok len_bytes ->
  let len_str = Bytes.to_string len_bytes in
  let len32, _ = decode_int32 len_str 0 in
  let len = Int32.to_int len32 in
  if len < 16 then
    Error (Mongo_error.Protocol "message length too small")
  else
    match read_exact_transport ?timeout_ms transport (len - 4) with
    | Error err -> Error err
    | Ok rest -> Ok (len_str ^ Bytes.to_string rest)

let read_message_result ?timeout_ms file_descr =
  read_message_transport ?timeout_ms (Mongo_transport.plain file_descr)

let read_message ?timeout_ms file_descr =
  match read_message_result ?timeout_ms file_descr with
  | Ok message -> message
  | Error err -> Mongo_error.raise_exn err

let decode_reply ~expected_request_id message =
  if String.length message < 16 then
    Error (Mongo_error.Protocol "message too short for header")
  else
    let header = MongoHeader.decode_header (String.sub message 0 16) in
    match MongoHeader.get_op header with
    | MongoOperation.OP_MSG -> (
        let flags, section_index = decode_int32 message 16 in
        if section_index >= String.length message then
          Error (Mongo_error.Protocol "OP_MSG section index out of bounds")
        else if message.[section_index] <> '\x00' then
          Error (Mongo_error.Protocol "unsupported OP_MSG section kind")
        else (
          let doc_start = section_index + 1 in
          let body =
            Bson.decode
              (String.sub message doc_start (String.length message - doc_start))
          in
          let response_to = MongoHeader.get_response_to header in
          if response_to <> expected_request_id then
            Error
              (Mongo_error.Protocol
                 (Printf.sprintf "response_to %ld != request_id %ld" response_to
                    expected_request_id))
          else
            Ok
              {
                request_id = MongoHeader.get_request_id header;
                response_to;
                flags;
                body;
              }))
    | MongoOperation.OP_REPLY -> (
        match MongoReply.get_document_list (MongoReply.decode_reply message) with
        | doc :: _ ->
            Ok
              {
                request_id = MongoHeader.get_request_id header;
                response_to = MongoHeader.get_response_to header;
                flags = 0l;
                body = doc;
              }
        | [] -> Error (Mongo_error.Protocol "empty MongoDB OP_REPLY"))
    | _ -> Error (Mongo_error.Protocol "unexpected MongoDB reply opcode")

let response_body ~expected_request_id message =
  match decode_reply ~expected_request_id message with
  | Ok reply -> Ok reply.body
  | Error err -> Error err

let response_message_doc message =
  if String.length message < 16 then
    Mongo_error.raise_exn (Mongo_error.Protocol "message too short for header");
  let header = MongoHeader.decode_header (String.sub message 0 16) in
  match MongoHeader.get_op header with
  | MongoOperation.OP_MSG -> (
      let _flags, section_index = decode_int32 message 16 in
      if section_index >= String.length message || message.[section_index] <> '\x00'
      then Mongo_error.raise_exn (Mongo_error.Protocol "unsupported OP_MSG section kind");
      let doc_start = section_index + 1 in
      Bson.decode (String.sub message doc_start (String.length message - doc_start)))
  | MongoOperation.OP_REPLY -> (
      match MongoReply.get_document_list (MongoReply.decode_reply message) with
      | doc :: _ -> doc
      | [] -> Mongo_error.raise_exn (Mongo_error.Protocol "empty MongoDB OP_REPLY"))
  | _ -> Mongo_error.raise_exn (Mongo_error.Protocol "unexpected MongoDB reply opcode")
