type tls =
  | Disabled
  | Enabled of {
      ca_file : string option;
      allow_invalid_certificates : bool;
      server_name : string option;
    }

type credentials = {
  username : string;
  password : string;
  auth_source : string option;
  auth_mechanism : [ `Scram_sha_256 | `Scram_sha_1 ] option;
}

type read_preference =
  | Primary
  | PrimaryPreferred
  | Secondary
  | SecondaryPreferred
  | Nearest

type read_preference_tag_set = (string * string) list

type write_concern = {
  w : [ `Majority | `Nodes of int | `Tag of string ] option;
  j : bool option;
  wtimeout_ms : int option;
}

type read_concern =
  | Local
  | Majority
  | Linearizable
  | Available
  | Snapshot
  | Custom of string

type t = {
  hosts : (string * int) list;
  database : string;
  credentials : credentials option;
  tls : tls;
  app_name : string option;
  replica_set : string option;
  direct_connection : bool;
  connect_timeout_ms : int;
  socket_timeout_ms : int option;
  timeout_ms : int option;
  server_selection_timeout_ms : int;
  local_threshold_ms : int;
  heartbeat_frequency_ms : int;
  max_pool_size : int;
  min_pool_size : int;
  wait_queue_timeout_ms : int;
  max_idle_time_ms : int option;
  retry_reads : bool;
  retry_writes : bool;
  read_preference : read_preference;
  read_preference_tags : read_preference_tag_set list;
  max_staleness_seconds : int option;
  read_concern : read_concern option;
  write_concern : write_concern option;
}

val default_port : int
val default : ?host:string -> ?port:int -> ?database:string -> unit -> t
val driver_name : string
val driver_version : string
val client_metadata : ?app_name:string -> unit -> Bson.t
val read_preference_to_bson : read_preference -> Bson.element
val read_preference_doc :
  ?tag_sets:read_preference_tag_set list ->
  ?max_staleness_seconds:int ->
  read_preference ->
  Bson.t
val min_supported_wire_version : int
val max_supported_wire_version : int
