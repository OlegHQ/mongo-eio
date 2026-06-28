type t

val id : t -> int64
val namespace : t -> string
val batch : t -> Bson.t list
val alive : t -> bool

val find :
  Mongo_connection.t ->
  db:string ->
  collection:string ->
  ?filter:Bson.t ->
  ?projection:Bson.t ->
  ?sort:Bson.t ->
  ?skip:int ->
  ?limit:int ->
  ?batch_size:int ->
  unit ->
  (t, Mongo_error.t) result

val get_more : ?batch_size:int -> t -> (Bson.t list, Mongo_error.t) result
val kill : t -> (unit, Mongo_error.t) result
