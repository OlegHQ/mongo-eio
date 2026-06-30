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

(** Switch-scoped direct-style client backed by the modern pool/command layer.

    This is additive to the legacy collection-bound [client] API above. The
    current implementation still uses the verified synchronous transport under
    the pool boundary, but exposes typed results and switch-scoped cleanup for
    Eio callers. *)
type direct_client

val connect :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  config:Mongo_config.t ->
  (direct_client, Mongo_error.t) result

val close_direct : direct_client -> unit

val with_direct_client :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  config:Mongo_config.t ->
  (direct_client -> ('a, Mongo_error.t) result) ->
  ('a, Mongo_error.t) result

val direct_run_command :
  ?session:Mongo_command.session_context ->
  ?command_event_handler:Mongo_command.command_event_handler ->
  direct_client ->
  string ->
  (string * Bson.element) list ->
  (Mongo_command.response, Mongo_error.t) result

val direct_with_connection :
  direct_client ->
  (Mongo_connection.t -> ('a, Mongo_error.t) result) ->
  ('a, Mongo_error.t) result

val direct_find :
  direct_client ->
  db:string ->
  collection:string ->
  Mongo_crud.find_options ->
  (Bson.t list, Mongo_error.t) result

val direct_find_one :
  direct_client ->
  db:string ->
  collection:string ->
  Bson.t ->
  (Bson.t option, Mongo_error.t) result

val direct_insert_one :
  ?write_concern:Mongo_command.write_concern ->
  direct_client ->
  db:string ->
  collection:string ->
  Bson.t ->
  (Mongo_crud.write_result, Mongo_error.t) result

val direct_insert_many :
  ?options:Mongo_crud.insert_options ->
  direct_client ->
  db:string ->
  collection:string ->
  Bson.t list ->
  (Mongo_crud.write_result, Mongo_error.t) result

val direct_update_one :
  ?write_concern:Mongo_command.write_concern ->
  direct_client ->
  db:string ->
  collection:string ->
  upsert:bool ->
  Bson.t ->
  Bson.t ->
  (Mongo_crud.write_result, Mongo_error.t) result

val direct_update_many :
  ?write_concern:Mongo_command.write_concern ->
  direct_client ->
  db:string ->
  collection:string ->
  upsert:bool ->
  Bson.t ->
  Bson.t ->
  (Mongo_crud.write_result, Mongo_error.t) result

val direct_delete_one :
  ?write_concern:Mongo_command.write_concern ->
  direct_client ->
  db:string ->
  collection:string ->
  Bson.t ->
  (Mongo_crud.write_result, Mongo_error.t) result

val direct_delete_many :
  ?write_concern:Mongo_command.write_concern ->
  direct_client ->
  db:string ->
  collection:string ->
  Bson.t ->
  (Mongo_crud.write_result, Mongo_error.t) result

val direct_ensure_simple_index :
  direct_client ->
  db:string ->
  collection:string ->
  field:string ->
  Mongo_index.index_option list ->
  (unit, Mongo_error.t) result

val direct_ensure_index :
  direct_client ->
  db:string ->
  collection:string ->
  Bson.t ->
  Mongo_index.index_option list ->
  (unit, Mongo_error.t) result

val direct_count_documents :
  direct_client ->
  db:string ->
  collection:string ->
  ?query:Bson.t ->
  unit ->
  (int, Mongo_error.t) result

val direct_estimated_document_count :
  direct_client ->
  db:string ->
  collection:string ->
  (int, Mongo_error.t) result
