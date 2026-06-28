(** Eio wrappers for the legacy blocking [MongoAdmin] API. *)

type client = MongoAdmin.t

val create : domain_mgr:_ Eio.Domain_manager.t -> host:string -> port:int -> client
val create_local_default : domain_mgr:_ Eio.Domain_manager.t -> unit -> client
val destroy : domain_mgr:_ Eio.Domain_manager.t -> client -> unit
val list_databases : domain_mgr:_ Eio.Domain_manager.t -> client -> MongoReply.t
val build_info : domain_mgr:_ Eio.Domain_manager.t -> client -> MongoReply.t
val server_status : domain_mgr:_ Eio.Domain_manager.t -> client -> MongoReply.t

