type server_info = {
  max_wire_version : int;
  min_wire_version : int;
  is_writable_primary : bool;
  secondary : bool;
  set_name : string option;
  hosts : (string * int) list;
  passives : (string * int) list;
  arbiters : (string * int) list;
  is_mongos : bool;
  logical_session_timeout_minutes : int option;
  sasl_supported_mechs : string list option;
  service_id : Bson.element option;
  tags : (string * string) list;
  last_write_date : float option;
  round_trip_time_ms : float option;
  round_trip_time_samples_ms : float list;
}

type t = {
  host : string;
  port : int;
  fd : Unix.file_descr;
  transport : Mongo_transport.t;
  config : Mongo_config.t;
  mutable server : server_info;
  authenticated : bool;
}

let rec take n = function
  | _ when n <= 0 -> []
  | [] -> []
  | x :: xs -> x :: take (n - 1) xs

let record_round_trip_time_ms ?(previous_samples = []) server rtt =
  let samples = take 10 (rtt :: previous_samples) in
  { server with round_trip_time_ms = Some rtt; round_trip_time_samples_ms = samples }

let minimum_round_trip_time_ms server =
  match server.round_trip_time_samples_ms with
  | _ :: _ :: _ as samples ->
      Some (List.fold_left min Float.infinity samples)
  | _ -> None

let close_noerr fd =
  try Unix.close fd with Unix.Unix_error _ -> ()

let timeout_seconds ms =
  if ms <= 0 then -1.0 else float_of_int ms /. 1000.0

let connect_nonblocking ~timeout_ms fd sockaddr =
  Unix.set_nonblock fd;
  let finish () =
    Unix.clear_nonblock fd;
    Ok fd
  in
  try
    Unix.connect fd sockaddr;
    finish ()
  with
  | Unix.Unix_error ((Unix.EINPROGRESS | Unix.EWOULDBLOCK), _, _) -> (
      match Unix.select [] [ fd ] [] (timeout_seconds timeout_ms) with
      | _, [], _ -> Error (Mongo_error.Timeout "TCP connect timed out")
      | _ -> (
          match Unix.getsockopt_error fd with
          | None -> finish ()
          | Some err -> Error (Mongo_error.Network (Unix.error_message err))))
  | Unix.Unix_error (e, _, _) -> Error (Mongo_error.Network (Unix.error_message e))

let connect_tcp ~timeout_ms host port =
  let service = string_of_int port in
  let addresses =
    Unix.getaddrinfo host service [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]
  in
  let rec connect_first = function
    | [] -> Error (Mongo_error.Network (Printf.sprintf "cannot connect to %s:%d" host port))
    | address :: rest ->
        let fd =
          Unix.socket address.Unix.ai_family address.Unix.ai_socktype
            address.Unix.ai_protocol
        in
        Unix.set_close_on_exec fd;
        (match connect_nonblocking ~timeout_ms fd address.Unix.ai_addr with
        | Ok fd -> Ok fd
        | Error err ->
            close_noerr fd;
            if rest = [] then Error err else connect_first rest)
  in
  connect_first addresses

let split_host_port value =
  if String.length value > 0 && value.[0] = '[' then
    match String.index_opt value ']' with
    | Some end_br ->
        let host = String.sub value 1 (end_br - 1) in
        let port =
          if end_br + 1 < String.length value && value.[end_br + 1] = ':' then
            int_of_string
              (String.sub value (end_br + 2)
                 (String.length value - end_br - 2))
          else Mongo_config.default_port
        in
        (host, port)
    | None -> (value, Mongo_config.default_port)
  else
    match String.rindex_opt value ':' with
    | Some idx -> (
        try
          ( String.sub value 0 idx,
            int_of_string
              (String.sub value (idx + 1) (String.length value - idx - 1)) )
        with _ -> (value, Mongo_config.default_port))
    | None -> (value, Mongo_config.default_port)

let endpoint_list doc name =
  try
    Bson.get_list (Bson.get_element name doc)
    |> List.filter_map (fun item ->
           try Some (split_host_port (Bson.get_string item))
           with _ -> (
             try
               let endpoint = Bson.get_doc_element item in
               let host = Bson.get_string (Bson.get_element "host" endpoint) in
               let port =
                 try
                   Int32.to_int
                     (Bson.get_int32 (Bson.get_element "port" endpoint))
                 with _ -> Mongo_config.default_port
               in
               Some (host, port)
             with _ -> None))
  with _ -> []

let string_list doc name =
  try
    Some
      (Bson.get_list (Bson.get_element name doc)
      |> List.filter_map (fun item ->
             try Some (Bson.get_string item) with _ -> None))
  with _ -> None

let string_doc doc name =
  try
    Bson.get_doc_element (Bson.get_element name doc)
    |> Bson.all_elements
    |> List.filter_map (fun (key, element) ->
           try Some (key, Bson.get_string element) with _ -> None)
  with _ -> []

let last_write_date doc =
  try
    Bson.get_doc_element (Bson.get_element "lastWrite" doc)
    |> Bson.get_element "lastWriteDate"
    |> Bson.get_utc |> Int64.to_float |> fun ms -> Some (ms /. 1000.0)
  with _ -> None

let parse_server_info doc =
  {
    max_wire_version =
      (try Int32.to_int (Bson.get_int32 (Bson.get_element "maxWireVersion" doc))
       with _ -> 0);
    min_wire_version =
      (try Int32.to_int (Bson.get_int32 (Bson.get_element "minWireVersion" doc))
       with _ -> 0);
    is_writable_primary =
      (try Bson.get_boolean (Bson.get_element "isWritablePrimary" doc)
       with _ -> (
         try not (Bson.get_boolean (Bson.get_element "secondary" doc))
         with _ -> true));
    secondary =
      (try Bson.get_boolean (Bson.get_element "secondary" doc) with _ -> false);
    set_name =
      (try Some (Bson.get_string (Bson.get_element "setName" doc))
       with _ -> None);
    hosts = endpoint_list doc "hosts";
    passives = endpoint_list doc "passives";
    arbiters = endpoint_list doc "arbiters";
    is_mongos =
      (try Bson.get_string (Bson.get_element "msg" doc) = "isdbgrid"
       with _ -> false);
    logical_session_timeout_minutes =
      (try
         Some
           (Int32.to_int
              (Bson.get_int32
                 (Bson.get_element "logicalSessionTimeoutMinutes" doc)))
       with _ -> None);
    sasl_supported_mechs = string_list doc "saslSupportedMechs";
    service_id = (try Some (Bson.get_element "serviceId" doc) with _ -> None);
    tags = string_doc doc "tags";
    last_write_date = last_write_date doc;
    round_trip_time_ms = None;
    round_trip_time_samples_ms = [];
  }

let server_description host port server =
  let server_type =
    if server.is_mongos then Mongo_server_description.Mongos
    else
      match (server.set_name, server.is_writable_primary, server.secondary) with
      | Some _, true, _ -> Mongo_server_description.RSPrimary
      | Some _, _, true -> Mongo_server_description.RSSecondary
      | Some _, _, _ -> Mongo_server_description.Unknown
      | None, _, _ -> Mongo_server_description.Standalone
  in
  {
    Mongo_server_description.address = (host, port);
    server_type;
    round_trip_time_ms = server.round_trip_time_ms;
    last_update = Unix.gettimeofday ();
    last_write_date = server.last_write_date;
    tags = server.tags;
    max_wire_version = server.max_wire_version;
    set_name = server.set_name;
    primary = None;
    error = None;
  }

let validate_wire_version server =
  if server.max_wire_version < Mongo_config.min_supported_wire_version then
    Error
      (Mongo_error.Unsupported
         (Printf.sprintf "server maxWireVersion %d below supported minimum"
            server.max_wire_version))
  else if server.min_wire_version > Mongo_config.max_supported_wire_version then
    Error
      (Mongo_error.Unsupported
         (Printf.sprintf "server minWireVersion %d above supported maximum"
            server.min_wire_version))
  else Ok ()

let validate_replica_set (config : Mongo_config.t) server =
  match config.replica_set with
  | None -> Ok ()
  | Some expected -> (
      match server.set_name with
      | Some actual when actual = expected -> Ok ()
      | Some actual ->
          Error
            (Mongo_error.Server_selection
               (Printf.sprintf "server belongs to replica set %s, expected %s"
                  actual expected))
      | None ->
          Error
            (Mongo_error.Server_selection
               (Printf.sprintf
                  "server is not a member of expected replica set %s" expected)))

type tls_peer =
  | Tls_host of [ `host ] Domain_name.t
  | Tls_ip of Ipaddr.t

let tls_peer host =
  match Ipaddr.of_string host with
  | Ok ip -> Ok (Tls_ip ip)
  | Error (`Msg _) -> (
      match Domain_name.of_string host with
      | Error (`Msg message) -> Error (Mongo_error.Network message)
      | Ok raw -> (
          match Domain_name.host raw with
          | Ok host -> Ok (Tls_host host)
          | Error (`Msg message) -> Error (Mongo_error.Network message)))

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      really_input_string channel (in_channel_length channel))

let tls_authenticator ca_file allow_invalid_certificates =
  if allow_invalid_certificates then
    match X509.Authenticator.of_string "none" with
    | Ok make -> Ok (make (fun () -> Some (Ptime_clock.now ())))
    | Error (`Msg message) -> Error (Mongo_error.Network message)
  else
    match ca_file with
    | None -> (
        match Ca_certs.authenticator () with
        | Ok auth -> Ok auth
        | Error (`Msg message) -> Error (Mongo_error.Network message))
    | Some path -> (
        try
          match X509.Certificate.decode_pem_multiple (read_file path) with
          | Error (`Msg message) -> Error (Mongo_error.Network message)
          | Ok anchors ->
              Ok
                (X509.Authenticator.chain_of_trust
                   ~time:(fun () -> Some (Ptime_clock.now ()))
                   anchors)
        with Sys_error message -> Error (Mongo_error.Network message))

let tls_transport ~host fd = function
  | Mongo_config.Disabled -> Ok (Mongo_transport.plain fd)
  | Enabled { ca_file; allow_invalid_certificates; server_name } -> (
      Mirage_crypto_rng_unix.use_default ();
      let peer_host = Option.value server_name ~default:host in
      match tls_peer peer_host with
      | Error err -> Error err
      | Ok peer -> (
          match tls_authenticator ca_file allow_invalid_certificates with
          | Error err -> Error err
          | Ok authenticator -> (
              let config =
                match peer with
                | Tls_host peer_name ->
                    Tls.Config.client ~authenticator ~peer_name ()
                | Tls_ip ip -> Tls.Config.client ~authenticator ~ip ()
              in
              match config with
              | Error (`Msg message) -> Error (Mongo_error.Network message)
              | Ok tls_config -> (
                  try
                    let session =
                      match peer with
                      | Tls_host peer_name ->
                          Tls_unix.client_of_fd tls_config ~host:peer_name fd
                      | Tls_ip _ -> Tls_unix.client_of_fd tls_config fd
                    in
                    Ok (Mongo_transport.tls session)
                  with exn ->
                    Error (Mongo_error.Network (Printexc.to_string exn))))))

let handshake ?timeout_ms (config : Mongo_config.t) transport =
  let request_id = Int32.of_float (Unix.gettimeofday ()) in
  let fields =
    [
      ("hello", Bson.create_int32 1l);
      ( "client",
        Bson.create_doc_element
          (Mongo_config.client_metadata ?app_name:config.app_name ()) );
    ]
  in
  let fields =
    match config.credentials with
    | Some creds when Option.is_none creds.auth_mechanism ->
        let user = Mongo_auth.auth_source creds config ^ "." ^ creds.username in
        fields @ [ ("saslSupportedMechs", Bson.create_string user) ]
    | Some _ | None -> fields
  in
  let started_at = Unix.gettimeofday () in
  match
    Mongo_command.run_transport ?timeout_ms ~db:"admin" ~request_id transport
      fields
  with
  | Error err -> Error err
  | Ok response ->
      let server =
        record_round_trip_time_ms (parse_server_info response.body)
          ((Unix.gettimeofday () -. started_at) *. 1000.0)
      in
      validate_wire_version server |> Result.map (fun () -> server)

let validate_server config server =
  match validate_wire_version server with
  | Error err -> Error err
  | Ok () -> validate_replica_set config server

let authenticated_connection ~timeout_ms ~host ~port ~fd ~transport ~config
    ~server =
  match config.Mongo_config.credentials with
  | None ->
      Ok
        {
          host;
          port;
          fd;
          transport;
          config;
          server;
          authenticated = false;
        }
  | Some creds -> (
      match
        Mongo_auth.authenticate_transport
          ?timeout_ms ?sasl_supported_mechs:server.sasl_supported_mechs config
          transport creds
      with
      | Ok () ->
          Ok
            {
              host;
              port;
              fd;
              transport;
              config;
              server;
              authenticated = true;
            }
      | Error err ->
          Mongo_transport.close transport;
          Error err)

let effective_timeout_ms ?legacy_timeout_ms (config : Mongo_config.t) =
  match (config.timeout_ms, legacy_timeout_ms) with
  | Some 0, legacy_timeout_ms -> legacy_timeout_ms
  | Some timeout_ms, None -> Some timeout_ms
  | Some timeout_ms, Some legacy_timeout_ms when legacy_timeout_ms <= 0 ->
      Some timeout_ms
  | Some timeout_ms, Some legacy_timeout_ms -> Some (min timeout_ms legacy_timeout_ms)
  | None, legacy_timeout_ms -> legacy_timeout_ms

let effective_required_timeout_ms ~legacy_timeout_ms config =
  Option.value
    (effective_timeout_ms ~legacy_timeout_ms config)
    ~default:legacy_timeout_ms

let connect_seed (config : Mongo_config.t) (host, port) =
  let connect_timeout_ms =
    effective_required_timeout_ms ~legacy_timeout_ms:config.connect_timeout_ms
      config
  in
  let socket_timeout_ms =
    effective_timeout_ms ?legacy_timeout_ms:config.socket_timeout_ms config
  in
  match connect_tcp ~timeout_ms:connect_timeout_ms host port with
  | Error err -> Error err
  | Ok fd -> (
      match tls_transport ~host fd config.tls with
      | Error err ->
          close_noerr fd;
          Error err
      | Ok transport -> (
          match handshake ?timeout_ms:socket_timeout_ms config transport with
          | Error err ->
              Mongo_transport.close transport;
              Error err
          | Ok server -> (
              match validate_server config server with
              | Error err ->
                  Mongo_transport.close transport;
                  Error err
              | Ok () ->
                  authenticated_connection ~host ~port ~fd ~transport ~config
                    ~timeout_ms:socket_timeout_ms ~server)))

let connect (config : Mongo_config.t) =
  match config.hosts with
  | [] -> Error (Mongo_error.Network "no hosts configured")
  | hosts ->
      let server_selection_timeout_ms =
        effective_required_timeout_ms
          ~legacy_timeout_ms:config.server_selection_timeout_ms config
      in
      let deadline =
        Unix.gettimeofday ()
        +. (float_of_int server_selection_timeout_ms /. 1000.0)
      in
      let add_endpoint endpoints endpoint =
        if List.mem endpoint endpoints then endpoints else endpoints @ [ endpoint ]
      in
      let add_endpoints endpoints discovered =
        List.fold_left add_endpoint endpoints discovered
      in
      let discovered_hosts server =
        server.hosts @ server.passives @ server.arbiters
      in
      let topology_from_descriptions descriptions =
        List.fold_left Mongo_topology.update_server Mongo_topology.empty
          descriptions
      in
      let selection_timeout descriptions =
        let topology = topology_from_descriptions descriptions in
        Mongo_error.Server_selection
          (Printf.sprintf "server selection timed out after %d ms (%s)"
             server_selection_timeout_ms
             (Mongo_topology.snapshot topology))
      in
      let sleep_for_selection () =
        let remaining = deadline -. Unix.gettimeofday () in
        if remaining > 0.0 then Unix.sleepf (min 0.5 remaining)
      in
      let rec try_hosts known descriptions last_error = function
        | [] ->
            if Unix.gettimeofday () >= deadline then
              Error
                (match last_error with
                | Some (Mongo_error.Server_selection _ as err) when descriptions = [] ->
                    err
                | Some (Mongo_error.Server_selection _) -> selection_timeout descriptions
                | Some err when descriptions = [] -> err
                | Some _ | None -> selection_timeout descriptions)
            else (
              sleep_for_selection ();
              try_hosts known descriptions last_error known)
        | (host, port) :: rest -> (
            match connect_seed config (host, port) with
            | Error (Mongo_error.Server_selection _ as err) -> Error err
            | Error err -> try_hosts known descriptions (Some err) rest
            | Ok conn ->
                let description =
                  server_description host port conn.server
                in
                let descriptions =
                  Mongo_topology.update_server
                    (topology_from_descriptions descriptions)
                    description
                  |> fun topology -> topology.servers
                in
                let known =
                  add_endpoints known (discovered_hosts conn.server)
                in
                let topology = topology_from_descriptions descriptions in
                (match
                   Mongo_server_select.select
                     ~tag_sets:config.read_preference_tags topology
                     ?max_staleness_seconds:config.max_staleness_seconds
                     ~local_threshold_ms:config.local_threshold_ms
                     config.read_preference
                 with
                | Ok selected when selected.address = description.address ->
                    Ok conn
                | Ok selected ->
                    Mongo_transport.close conn.transport;
                    let next =
                      if List.mem selected.address rest then rest
                      else selected.address :: rest
                    in
                    try_hosts known descriptions None next
                | Error err ->
                    Mongo_transport.close conn.transport;
                    try_hosts known descriptions (Some err) rest))
      in
      try_hosts hosts [] None hosts

let file_descr t = Mongo_transport.file_descr t.transport
let close t = Mongo_transport.close t.transport
let server_info t = t.server

let implicit_session_context (t : t) fields =
  match t.server.logical_session_timeout_minutes with
  | None -> None
  | Some _ when not (Mongo_retry.supports_implicit_session fields) -> None
  | Some _ ->
      let session = Mongo_session.create () in
      let retryable_write_server = t.server.set_name <> None || t.server.is_mongos in
      if
        retryable_write_server
        && t.config.retry_writes
        && Mongo_retry.is_retryable_write_command fields
      then Some (Mongo_session.command_context session)
      else Some (Mongo_session.implicit_context session)

let command_timeout_ms (config : Mongo_config.t) =
  effective_timeout_ms ?legacy_timeout_ms:config.socket_timeout_ms config

let command_supports_max_time_ms fields =
  match Mongo_command.command_name fields with
  | None -> false
  | Some name -> (
      match String.lowercase_ascii name with
      | "getmore" | "hello" | "ismaster" -> false
      | _ -> true)

let apply_timeout_ms server (config : Mongo_config.t) fields =
  match config.timeout_ms with
  | Some timeout_ms when timeout_ms > 0 && command_supports_max_time_ms fields -> (
      let min_rtt_ms =
        minimum_round_trip_time_ms server |> Option.value ~default:0.0
      in
      let max_time_ms = float_of_int timeout_ms -. min_rtt_ms in
      if max_time_ms <= 0.0 then
        Error
          (Mongo_error.Timeout
             "operation timeoutMS expired before server round trip")
      else
        Ok
          (fields
          @ [
              ( "maxTimeMS",
                Bson.create_int64
                  (Int64.of_int (int_of_float (ceil max_time_ms))) );
            ]))
  | _ -> Ok fields

let run_command ?session ?read_concern ?write_concern ?command_event_handler
    (t : t) db fields =
  let request_id = Int32.of_float (Unix.gettimeofday ()) in
  let read_concern =
    match read_concern with
    | Some _ -> read_concern
    | None -> t.config.read_concern
  in
  let write_concern =
    match write_concern with
    | Some _ -> write_concern
    | None -> t.config.write_concern
  in
  let session =
    match session with
    | Some _ -> session
    | None -> implicit_session_context t fields
  in
  match apply_timeout_ms t.server t.config fields with
  | Error err -> Error err
  | Ok fields ->
      let connection_id = Printf.sprintf "%s:%d" t.host t.port in
      Mongo_command.run_transport ?session
        ?timeout_ms:(command_timeout_ms t.config)
        ?read_preference:(Some t.config.read_preference)
        ?command_event_handler
        ~read_preference_tags:t.config.read_preference_tags
        ?max_staleness_seconds:t.config.max_staleness_seconds
        ?read_concern ?write_concern
        ~connection_id
        ~db ~request_id t.transport fields

let run_command_exn ?session ?read_concern ?write_concern ?command_event_handler
    t db fields =
  match
    run_command ?session ?read_concern ?write_concern ?command_event_handler t
      db fields
  with
  | Ok response -> response.body
  | Error err -> Mongo_error.raise_exn err

let heartbeat connection =
  let started_at = Unix.gettimeofday () in
  let monitor_connection =
    {
      connection with
      config = { connection.config with timeout_ms = None };
    }
  in
  match
    run_command monitor_connection "admin" [ ("hello", Bson.create_int32 1l) ]
  with
  | Ok response ->
      let rtt = (Unix.gettimeofday () -. started_at) *. 1000.0 in
      let server =
        record_round_trip_time_ms
          ~previous_samples:connection.server.round_trip_time_samples_ms
          (parse_server_info response.body) rtt
      in
      connection.server <- server;
      Ok
        (server_description connection.host connection.port server)
  | Error err -> Error err

let unknown_server_description connection err =
  {
    Mongo_server_description.address = (connection.host, connection.port);
    server_type = Mongo_server_description.Unknown;
    round_trip_time_ms = None;
    last_update = Unix.gettimeofday ();
    last_write_date = None;
    tags = [];
    max_wire_version = 0;
    set_name = None;
    primary = None;
    error = Some (Mongo_error.to_string err);
  }

let monitor_once connection update =
  match heartbeat connection with
  | Ok server -> update server
  | Error err -> update (unknown_server_description connection err)

let start_monitor ~sw ~clock ?period_ms connection update =
  let period_ms =
    Option.value period_ms ~default:connection.config.heartbeat_frequency_ms
  in
  let rec loop () =
    monitor_once connection update;
    Eio.Time.sleep clock (float_of_int period_ms /. 1000.);
    loop ()
  in
  Eio.Fiber.fork ~sw loop
