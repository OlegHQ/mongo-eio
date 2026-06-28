type t

val plain : Unix.file_descr -> t
val tls : Tls_unix.t -> t
val is_tls : t -> bool
val file_descr : t -> Unix.file_descr
val close : t -> unit
val read : t -> bytes -> off:int -> len:int -> (int, Mongo_error.t) result
val write : t -> string -> off:int -> len:int -> (int, Mongo_error.t) result
