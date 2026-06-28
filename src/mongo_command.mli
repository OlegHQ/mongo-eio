type read_preference =
  | Primary
  | PrimaryPreferred
  | Secondary
  | SecondaryPreferred
  | Nearest

type write_concern = Mongo_config.write_concern = {
  w : [ `Majority | `Nodes of int | `Tag of string ] option;
  j : bool option;
  wtimeout_ms : int option;
}

type read_concern = Mongo_config.read_concern =
  | Local
  | Majority
  | Linearizable
  | Available
  | Snapshot
  | Custom of string

type response = {
  ok : bool;
  body : Bson.t;
  cluster_time : Bson.t option;
  operation_time : int64 option;
}

type command_event =
  | Command_started of {
      command_name : string;
      database_name : string;
      command : Bson.t;
      request_id : int32;
      connection_id : string option;
    }
  | Command_succeeded of {
      command_name : string;
      duration_ms : float;
      reply : Bson.t;
      request_id : int32;
      connection_id : string option;
    }
  | Command_failed of {
      command_name : string;
      duration_ms : float;
      failure : Mongo_error.t;
      request_id : int32;
      connection_id : string option;
    }

type command_event_handler = command_event -> unit

type session_context = {
  session_id : string option;
  txn_number : int64 option;
}

val document : (string * Bson.element) list -> Bson.t
val command_name : (string * Bson.element) list -> string option
val read_concern_doc : read_concern -> Bson.t
val write_concern_doc : write_concern -> Bson.t
val enrich_command :
  db:string ->
  ?session:session_context ->
  ?read_preference:Mongo_config.read_preference ->
  ?read_preference_tags:Mongo_config.read_preference_tag_set list ->
  ?max_staleness_seconds:int ->
  ?read_concern:read_concern ->
  ?write_concern:write_concern ->
  (string * Bson.element) list ->
  Bson.t
val parse_response : Bson.t -> response
val run :
  ?timeout_ms:int ->
  ?session:session_context ->
  ?read_preference:Mongo_config.read_preference ->
  ?command_event_handler:command_event_handler ->
  ?read_preference_tags:Mongo_config.read_preference_tag_set list ->
  ?max_staleness_seconds:int ->
  ?read_concern:read_concern ->
  ?write_concern:write_concern ->
  ?connection_id:string ->
  db:string ->
  request_id:int32 ->
  Unix.file_descr ->
  (string * Bson.element) list ->
  (response, Mongo_error.t) result
val run_transport :
  ?timeout_ms:int ->
  ?session:session_context ->
  ?read_preference:Mongo_config.read_preference ->
  ?command_event_handler:command_event_handler ->
  ?read_preference_tags:Mongo_config.read_preference_tag_set list ->
  ?max_staleness_seconds:int ->
  ?read_concern:read_concern ->
  ?write_concern:write_concern ->
  ?connection_id:string ->
  db:string ->
  request_id:int32 ->
  Mongo_transport.t ->
  (string * Bson.element) list ->
  (response, Mongo_error.t) result
val run_exn :
  ?timeout_ms:int ->
  ?session:session_context ->
  ?read_preference:Mongo_config.read_preference ->
  ?command_event_handler:command_event_handler ->
  ?read_preference_tags:Mongo_config.read_preference_tag_set list ->
  ?max_staleness_seconds:int ->
  ?read_concern:read_concern ->
  ?write_concern:write_concern ->
  ?connection_id:string ->
  db:string ->
  request_id:int32 ->
  Unix.file_descr ->
  (string * Bson.element) list ->
  Bson.t
val cursor_batch : Bson.t -> Bson.t list
val int_of_bson : Bson.element -> int
