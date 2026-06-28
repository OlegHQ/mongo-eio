type t = {
  id : string;
  mutable txn_number : int64;
}

let rng_initialized = lazy (Mirage_crypto_rng_unix.use_default ())

let set_uuid_v4_bits bytes =
  let version = Char.code (Bytes.get bytes 6) in
  Bytes.set bytes 6 (Char.chr ((version land 0x0f) lor 0x40));
  let variant = Char.code (Bytes.get bytes 8) in
  Bytes.set bytes 8 (Char.chr ((variant land 0x3f) lor 0x80))

let create () =
  Lazy.force rng_initialized;
  let id = Bytes.of_string (Mirage_crypto_rng.generate 16) in
  set_uuid_v4_bits id;
  let id = Bytes.to_string id in
  { id; txn_number = 0L }

let next_txn t =
  t.txn_number <- Int64.add t.txn_number 1L;
  t.txn_number

let command_context t =
  Mongo_command.{ session_id = Some t.id; txn_number = Some (next_txn t) }

let implicit_context t = Mongo_command.{ session_id = Some t.id; txn_number = None }
