open Mongo_config

let auth_source creds config =
  match creds.auth_source with
  | Some source -> source
  | None -> config.database

let choose_mechanism ?sasl_supported_mechs creds =
  match creds.auth_mechanism with
  | Some `Scram_sha_256 -> Mongo_scram.Scram_sha_256
  | Some `Scram_sha_1 -> Mongo_scram.Scram_sha_1
  | None -> (
      match sasl_supported_mechs with
      | Some mechanisms
        when List.exists (( = ) "SCRAM-SHA-256") mechanisms ->
          Mongo_scram.Scram_sha_256
      | Some _ | None -> Mongo_scram.Scram_sha_1)

let authenticate_transport ?timeout_ms ?sasl_supported_mechs config transport creds =
  let source = auth_source creds config in
  let mechanism = choose_mechanism ?sasl_supported_mechs creds in
  let timeout_ms =
    match timeout_ms with
    | Some _ -> timeout_ms
    | None -> config.socket_timeout_ms
  in
  match
    Mongo_scram.authenticate_transport ?timeout_ms mechanism
      ~password:creds.password ~username:creds.username ~auth_source:source
      transport
  with
  | Ok () -> Ok ()
  | Error err -> Error err

let authenticate ?timeout_ms ?sasl_supported_mechs config fd creds =
  authenticate_transport ?timeout_ms ?sasl_supported_mechs config
    (Mongo_transport.plain fd) creds

let authenticate_sha1 ?timeout_ms config fd creds =
  let timeout_ms =
    match timeout_ms with
    | Some _ -> timeout_ms
    | None -> config.socket_timeout_ms
  in
  Mongo_scram.authenticate ?timeout_ms Mongo_scram.Scram_sha_1 ~password:creds.password
    ~username:creds.username ~auth_source:(auth_source creds config) fd
