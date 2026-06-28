type t =
  | Plain of Unix.file_descr
  | Tls of Tls_unix.t

let plain fd = Plain fd
let tls session = Tls session
let is_tls = function Plain _ -> false | Tls _ -> true

let file_descr = function
  | Plain fd -> fd
  | Tls session -> Tls_unix.file_descr session

let close = function
  | Plain fd -> (try Unix.close fd with Unix.Unix_error _ -> ())
  | Tls session -> (try Tls_unix.close session with _ -> ())

let tls_error = function
  | Tls_unix.Tls_alert _ -> Mongo_error.Network "TLS alert"
  | Tls_unix.Tls_failure _ -> Mongo_error.Network "TLS failure"
  | Tls_unix.Closed_by_peer -> Mongo_error.Network "TLS connection closed by peer"
  | End_of_file -> Mongo_error.Network "TLS connection closed by peer"
  | Unix.Unix_error (e, _, _) -> Mongo_error.Network (Unix.error_message e)
  | exn -> Mongo_error.Network (Printexc.to_string exn)

let read t bytes ~off ~len =
  try
    match t with
    | Plain fd -> Ok (Unix.read fd bytes off len)
    | Tls session -> Ok (Tls_unix.read session ~off ~len bytes)
  with exn -> Error (tls_error exn)

let write t data ~off ~len =
  try
    match t with
    | Plain fd ->
        let bytes = Bytes.unsafe_of_string data in
        let n = Unix.write fd bytes off len in
        if n = 0 then Error (Mongo_error.Network "socket write returned zero")
        else Ok n
    | Tls session ->
        Tls_unix.write session ~off ~len data;
        Ok len
  with exn -> Error (tls_error exn)
