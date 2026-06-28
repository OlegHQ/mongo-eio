(** Eio wrappers for the legacy blocking [Mongo] API.

    The existing [Mongo] API is preserved. These helpers isolate blocking socket
    work in Eio domains so callers do not stall the main Eio scheduler. *)

type client = Mongo.t

type config = {
  host : string;
  port : int;
  database : string;
  collection : string;
}

val default_port : int

val create : domain_mgr:_ Eio.Domain_manager.t -> config -> client
val destroy : domain_mgr:_ Eio.Domain_manager.t -> client -> unit

val with_client :
  domain_mgr:_ Eio.Domain_manager.t -> config -> (client -> 'a) -> 'a

val insert :
  domain_mgr:_ Eio.Domain_manager.t -> client -> Bson.t list -> unit

val delete_one :
  domain_mgr:_ Eio.Domain_manager.t -> client -> Bson.t -> unit

val delete_all :
  domain_mgr:_ Eio.Domain_manager.t -> client -> Bson.t -> unit

val ensure_simple_index :
  ?options:Mongo.index_option list ->
  domain_mgr:_ Eio.Domain_manager.t ->
  client ->
  string ->
  unit

val find :
  ?skip:int -> domain_mgr:_ Eio.Domain_manager.t -> client -> MongoReply.t

val find_one :
  ?skip:int -> domain_mgr:_ Eio.Domain_manager.t -> client -> MongoReply.t

val find_q :
  ?skip:int ->
  domain_mgr:_ Eio.Domain_manager.t ->
  client ->
  Bson.t ->
  MongoReply.t
