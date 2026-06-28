type reply = {
  request_id : int32;
  response_to : int32;
  flags : int32;
  body : Bson.t;
}

val encode_op_msg : int32 -> Bson.t -> string
val write_all : ?timeout_ms:int -> Unix.file_descr -> string -> (unit, Mongo_error.t) result
val write_all_transport :
  ?timeout_ms:int -> Mongo_transport.t -> string -> (unit, Mongo_error.t) result
val read_message_result : ?timeout_ms:int -> Unix.file_descr -> (string, Mongo_error.t) result
val read_message_transport :
  ?timeout_ms:int -> Mongo_transport.t -> (string, Mongo_error.t) result
val read_message : ?timeout_ms:int -> Unix.file_descr -> string
val decode_reply : expected_request_id:int32 -> string -> (reply, Mongo_error.t) result
val response_body : expected_request_id:int32 -> string -> (Bson.t, Mongo_error.t) result
val response_message_doc : string -> Bson.t
