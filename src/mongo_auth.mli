val auth_source : Mongo_config.credentials -> Mongo_config.t -> string

val choose_mechanism :
  ?sasl_supported_mechs:string list ->
  Mongo_config.credentials ->
  Mongo_scram.mechanism

val authenticate :
  ?timeout_ms:int ->
  ?sasl_supported_mechs:string list ->
  Mongo_config.t ->
  Unix.file_descr ->
  Mongo_config.credentials ->
  (unit, Mongo_error.t) result
val authenticate_transport :
  ?timeout_ms:int ->
  ?sasl_supported_mechs:string list ->
  Mongo_config.t ->
  Mongo_transport.t ->
  Mongo_config.credentials ->
  (unit, Mongo_error.t) result

val authenticate_sha1 :
  ?timeout_ms:int ->
  Mongo_config.t -> Unix.file_descr -> Mongo_config.credentials -> (unit, Mongo_error.t) result
