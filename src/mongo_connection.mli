type server_info = {
  max_wire_version : int;
  min_wire_version : int;
  is_writable_primary : bool;
  secondary : bool;
  set_name : string option;
  hosts : (string * int) list;
  passives : (string * int) list;
  arbiters : (string * int) list;
  is_mongos : bool;
  logical_session_timeout_minutes : int option;
  sasl_supported_mechs : string list option;
  service_id : Bson.element option;
  tags : (string * string) list;
  last_write_date : float option;
  round_trip_time_ms : float option;
  round_trip_time_samples_ms : float list;
}

type t = {
  host : string;
  port : int;
  fd : Unix.file_descr;
  transport : Mongo_transport.t;
  config : Mongo_config.t;
  mutable server : server_info;
  authenticated : bool;
}

val connect : Mongo_config.t -> (t, Mongo_error.t) result
val parse_server_info : Bson.t -> server_info
val file_descr : t -> Unix.file_descr
val close : t -> unit
val server_info : t -> server_info

val run_command :
  ?session:Mongo_command.session_context ->
  ?read_concern:Mongo_command.read_concern ->
  ?write_concern:Mongo_command.write_concern ->
  ?command_event_handler:Mongo_command.command_event_handler ->
  t ->
  string ->
  (string * Bson.element) list ->
  (Mongo_command.response, Mongo_error.t) result

val run_command_exn :
  ?session:Mongo_command.session_context ->
  ?read_concern:Mongo_command.read_concern ->
  ?write_concern:Mongo_command.write_concern ->
  ?command_event_handler:Mongo_command.command_event_handler ->
  t ->
  string ->
  (string * Bson.element) list ->
  Bson.t

val heartbeat : t -> (Mongo_server_description.t, Mongo_error.t) result
val monitor_once : t -> (Mongo_server_description.t -> unit) -> unit

val start_monitor :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  ?period_ms:int ->
  t ->
  (Mongo_server_description.t -> unit) ->
  unit
