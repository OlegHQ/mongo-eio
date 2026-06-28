type write_error = {
  index : int option;
  code : int option;
  code_name : string option;
  message : string;
}

type command_error = {
  code : int option;
  code_name : string option;
  message : string;
  labels : string list;
  write_errors : write_error list;
  write_concern_errors : write_error list;
}

type t =
  | Network of string
  | Timeout of string
  | Execution_timeout of string
  | Protocol of string
  | Command of command_error
  | Authentication of string
  | Server_selection of string
  | Unsupported of string

exception Mongo_failed of string

val duplicate_key_code : int
val ok_value : Bson.t -> bool

val of_command_reply : Bson.t -> (Bson.t, t) result
val labels : t -> string list
val has_label : t -> string -> bool
val code : t -> int option
val code_name : t -> string option
val is_duplicate_key : t -> bool
val to_string : t -> string
val redact_uri : string -> string
val to_exn : t -> exn
val raise_exn : t -> 'a
val command_to_exn : Bson.t -> exn
