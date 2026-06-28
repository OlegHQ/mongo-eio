let percent_decode input =
  let len = String.length input in
  let buf = Buffer.create len in
  let hex_value c =
    match c with
    | '0' .. '9' -> Char.code c - Char.code '0'
    | 'a' .. 'f' -> 10 + Char.code c - Char.code 'a'
    | 'A' .. 'F' -> 10 + Char.code c - Char.code 'A'
    | _ -> invalid_arg "invalid percent encoding"
  in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else
      match input.[i] with
      | '%' when i + 2 < len ->
          let code =
            (hex_value input.[i + 1] lsl 4) lor hex_value input.[i + 2]
          in
          Buffer.add_char buf (Char.chr code);
          loop (i + 3)
      | '%' -> invalid_arg "invalid percent encoding"
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  in
  loop 0

let split_on_char c s =
  let len = String.length s in
  let rec aux start i acc =
    if i >= len then
      List.rev
        (if start = len then acc else String.sub s start (len - start) :: acc)
    else if s.[i] = c then
      aux (i + 1) (i + 1) (String.sub s start (i - start) :: acc)
    else aux start (i + 1) acc
  in
  aux 0 0 []

let parse_bool = function
  | "true" | "True" | "TRUE" -> true
  | "false" | "False" | "FALSE" -> false
  | other -> invalid_arg ("invalid boolean URI option: " ^ other)

let parse_int name value =
  try int_of_string value
  with _ ->
    invalid_arg (Printf.sprintf "invalid integer for %s: %s" name value)

let parse_nonnegative_int name value =
  let parsed = parse_int name value in
  if parsed < 0 then invalid_arg (Printf.sprintf "%s must be nonnegative" name);
  parsed

let parse_min_int name ~min value =
  let parsed = parse_int name value in
  if parsed < min then
    invalid_arg (Printf.sprintf "%s must be at least %d" name min);
  parsed

let parse_read_preference_tags value =
  if value = "" then []
  else
    split_on_char ',' value
    |> List.map (fun tag ->
        match String.index_opt tag ':' with
        | Some idx when idx > 0 && idx + 1 < String.length tag ->
            ( String.sub tag 0 idx,
              String.sub tag (idx + 1) (String.length tag - idx - 1) )
        | _ -> invalid_arg "invalid readPreferenceTags")

let parse_port value =
  let port = parse_int "port" value in
  if port < 1 || port > 65_535 then
    invalid_arg "port must be between 1 and 65535";
  port

let starts_or_ends_with c s =
  let len = String.length s in
  len > 0 && (s.[0] = c || s.[len - 1] = c)

let parse_hosts hosts_part =
  if hosts_part = "" then invalid_arg "URI must include at least one host";
  if starts_or_ends_with ',' hosts_part then
    invalid_arg "URI host list must not contain empty hosts";
  split_on_char ',' hosts_part
  |> List.map (fun host_port ->
      if host_port = "" then
        invalid_arg "URI host list must not contain empty hosts";
      match String.rindex_opt host_port ':' with
      | None ->
          if String.contains host_port '[' || String.contains host_port ']' then
            invalid_arg "malformed IPv6 host";
          (host_port, Mongo_config.default_port)
      | Some _idx when host_port.[0] = '[' -> (
          match String.index_opt host_port ']' with
          | Some end_br ->
              let host = String.sub host_port 1 (end_br - 1) in
              if host = "" then invalid_arg "IPv6 host must not be empty";
              let port =
                if end_br + 1 = String.length host_port then
                  Mongo_config.default_port
                else if host_port.[end_br + 1] = ':' then
                  parse_port
                    (String.sub host_port (end_br + 2)
                       (String.length host_port - end_br - 2))
                else invalid_arg "malformed IPv6 host"
              in
              (host, port)
          | None -> invalid_arg "malformed IPv6 host")
      | Some idx ->
          let host = String.sub host_port 0 idx in
          if host = "" then invalid_arg "URI host must not be empty";
          if String.contains host ':' then
            invalid_arg "IPv6 literals must be enclosed in brackets";
          ( host,
            parse_port
              (String.sub host_port (idx + 1)
                 (String.length host_port - idx - 1)) ))

let parse_auth_mechanism = function
  | "SCRAM-SHA-256" | "scram-sha-256" -> `Scram_sha_256
  | "SCRAM-SHA-1" | "scram-sha-1" -> `Scram_sha_1
  | other -> invalid_arg ("unsupported authMechanism: " ^ other)

let parse_read_concern_level = function
  | "local" -> Mongo_config.Local
  | "majority" -> Majority
  | "linearizable" -> Linearizable
  | "available" -> Available
  | "snapshot" -> Snapshot
  | level -> Custom level

let parse_write_concern_w value =
  if String.lowercase_ascii value = "majority" then `Majority
  else
    match int_of_string_opt value with
    | Some nodes when nodes < 0 -> invalid_arg "w must be nonnegative"
    | Some nodes -> `Nodes nodes
    | None -> `Tag value

let update_write_concern (config : Mongo_config.t) f =
  let existing =
    Option.value config.write_concern
      ~default:{ Mongo_config.w = None; j = None; wtimeout_ms = None }
  in
  { config with write_concern = Some (f existing) }

type parsed = { config : Mongo_config.t; warnings : string list }

let update_credentials (config : Mongo_config.t) f =
  { config with credentials = Option.map f config.credentials }

let enable_tls ?ca_file ?server_name ?allow_invalid_certificates config =
  let ( existing_ca_file,
        existing_allow_invalid_certificates,
        existing_server_name ) =
    match config.Mongo_config.tls with
    | Mongo_config.Disabled -> (None, false, None)
    | Mongo_config.Enabled tls ->
        (tls.ca_file, tls.allow_invalid_certificates, tls.server_name)
  in
  {
    config with
    tls =
      Mongo_config.Enabled
        {
          ca_file =
            (match ca_file with
            | None -> existing_ca_file
            | Some ca_file -> Some ca_file);
          allow_invalid_certificates =
            (match allow_invalid_certificates with
            | None -> existing_allow_invalid_certificates
            | Some allow_invalid_certificates -> allow_invalid_certificates);
          server_name =
            (match server_name with
            | None -> existing_server_name
            | Some server_name -> Some server_name);
        };
  }

let apply_option (config : Mongo_config.t) key value =
  match String.lowercase_ascii key with
  | "authsource" ->
      if value = "" then invalid_arg "authSource must not be empty";
      update_credentials config (fun (c : Mongo_config.credentials) ->
          { c with auth_source = Some value })
  | "authmechanism" ->
      let mechanism = parse_auth_mechanism value in
      update_credentials config (fun (c : Mongo_config.credentials) ->
          { c with auth_mechanism = Some mechanism })
  | "authmechanismproperties" ->
      if value = "" then config
      else invalid_arg "authMechanismProperties is not supported by this driver"
  | "replicaset" -> { config with replica_set = Some value }
  | "tls" | "ssl" ->
      {
        config with
        tls =
          (if parse_bool value then
             Mongo_config.Enabled
               {
                 ca_file = None;
                 allow_invalid_certificates = false;
                 server_name = None;
               }
           else Mongo_config.Disabled);
      }
  | "tlscafile" -> enable_tls ~ca_file:value config
  | "tlscertificatekeyfile" | "tlscertificatekeyfilepath"
  | "tlscertificatekeyfilepassword" ->
      invalid_arg (Printf.sprintf "%s is not supported by this driver" key)
  | "tlsinsecure" | "tlsallowinvalidcertificates" ->
      if parse_bool value then
        enable_tls ~allow_invalid_certificates:true config
      else enable_tls ~allow_invalid_certificates:false config
  | "tlsallowinvalidhostnames" | "tlsdisableocspendpointcheck"
  | "tlsdisablecertificaterevocationcheck" ->
      if parse_bool value then
        invalid_arg (Printf.sprintf "%s is not supported by this driver" key)
      else config
  | "directconnection" -> { config with direct_connection = parse_bool value }
  | "serverselectiontimeoutms" ->
      {
        config with
        server_selection_timeout_ms = parse_nonnegative_int key value;
      }
  | "connecttimeoutms" ->
      { config with connect_timeout_ms = parse_nonnegative_int key value }
  | "sockettimeoutms" ->
      { config with socket_timeout_ms = Some (parse_nonnegative_int key value) }
  | "timeoutms" ->
      { config with timeout_ms = Some (parse_nonnegative_int key value) }
  | "localthresholdms" ->
      { config with local_threshold_ms = parse_nonnegative_int key value }
  | "maxpoolsize" ->
      { config with max_pool_size = parse_nonnegative_int key value }
  | "minpoolsize" ->
      { config with min_pool_size = parse_nonnegative_int key value }
  | "maxidletimems" ->
      { config with max_idle_time_ms = Some (parse_nonnegative_int key value) }
  | "waitqueuetimeoutms" ->
      { config with wait_queue_timeout_ms = parse_nonnegative_int key value }
  | "heartbeatfrequencyms" ->
      { config with heartbeat_frequency_ms = parse_min_int key ~min:500 value }
  | "retryreads" -> { config with retry_reads = parse_bool value }
  | "retrywrites" -> { config with retry_writes = parse_bool value }
  | "appname" -> { config with app_name = Some value }
  | "readconcernlevel" ->
      { config with read_concern = Some (parse_read_concern_level value) }
  | "w" ->
      update_write_concern config (fun concern ->
          { concern with w = Some (parse_write_concern_w value) })
  | "journal" ->
      update_write_concern config (fun concern ->
          { concern with j = Some (parse_bool value) })
  | "wtimeoutms" ->
      update_write_concern config (fun concern ->
          { concern with wtimeout_ms = Some (parse_nonnegative_int key value) })
  | "readpreference" ->
      let read_preference =
        match String.lowercase_ascii value with
        | "primary" -> Mongo_config.Primary
        | "primarypreferred" -> Mongo_config.PrimaryPreferred
        | "secondary" -> Mongo_config.Secondary
        | "secondarypreferred" -> Mongo_config.SecondaryPreferred
        | "nearest" -> Mongo_config.Nearest
        | other -> invalid_arg ("unknown readPreference: " ^ other)
      in
      { config with read_preference }
  | "readpreferencetags" ->
      {
        config with
        read_preference_tags =
          config.read_preference_tags @ [ parse_read_preference_tags value ];
      }
  | "maxstalenessseconds" ->
      {
        config with
        max_staleness_seconds = Some (parse_nonnegative_int key value);
      }
  | "loadbalanced" ->
      if parse_bool value then
        invalid_arg "loadBalanced=true is not supported by this driver"
      else config
  | _ -> config

let parse_option_pair pair =
  match String.index_opt pair '=' with
  | Some idx ->
      let key = String.sub pair 0 idx |> percent_decode in
      let value =
        String.sub pair (idx + 1) (String.length pair - idx - 1)
        |> percent_decode
      in
      (String.lowercase_ascii key, value)
  | None -> (String.lowercase_ascii (percent_decode pair), "true")

let option_values key options =
  List.filter_map
    (fun (option_key, value) -> if option_key = key then Some value else None)
    options

let option_present key options = option_values key options <> []

let validate_duplicate_bool_aliases options =
  let values = option_values "tls" options @ option_values "ssl" options in
  let valid_values =
    List.filter_map
      (fun value ->
        try Some (parse_bool value) with Invalid_argument _ -> None)
      values
  in
  match List.sort_uniq Bool.compare valid_values with
  | [] | [ _ ] -> ()
  | _ -> invalid_arg "tls and ssl URI options must not conflict"

let validate_tls_conflicts options =
  let present key = option_present key options in
  if present "tlsinsecure" then (
    if present "tlsallowinvalidcertificates" then
      invalid_arg "tlsInsecure conflicts with tlsAllowInvalidCertificates";
    if present "tlsallowinvalidhostnames" then
      invalid_arg "tlsInsecure conflicts with tlsAllowInvalidHostnames";
    if present "tlsdisableocspendpointcheck" then
      invalid_arg "tlsInsecure conflicts with tlsDisableOCSPEndpointCheck";
    if present "tlsdisablecertificaterevocationcheck" then
      invalid_arg
        "tlsInsecure conflicts with tlsDisableCertificateRevocationCheck");
  if
    present "tlsallowinvalidcertificates"
    && present "tlsdisablecertificaterevocationcheck"
  then
    invalid_arg
      "tlsAllowInvalidCertificates conflicts with \
       tlsDisableCertificateRevocationCheck";
  if
    present "tlsdisableocspendpointcheck"
    && present "tlsdisablecertificaterevocationcheck"
  then
    invalid_arg
      "tlsDisableOCSPEndpointCheck conflicts with \
       tlsDisableCertificateRevocationCheck"

let validate_txt_options options =
  List.iter
    (fun (key, _value) ->
      match key with
      | "authsource" | "replicaset" | "loadbalanced" -> ()
      | _ ->
          invalid_arg
            (Printf.sprintf
               "TXT record option %s is not supported for mongodb+srv" key))
    options

let contains_option key options =
  List.exists (fun (option_key, _) -> option_key = key) options

let reject_non_srv_options options =
  if contains_option "srvservicename" options then
    invalid_arg "srvServiceName is only valid for mongodb+srv URIs";
  if contains_option "srvmaxhosts" options then
    invalid_arg "srvMaxHosts is only valid for mongodb+srv URIs"

let srv_option_defaults options =
  let service_name =
    match option_values "srvservicename" options with
    | [] -> "mongodb"
    | [ value ] ->
        if value = "" then invalid_arg "srvServiceName must not be empty";
        value
    | _ -> invalid_arg "srvServiceName must not appear more than once"
  in
  let max_hosts =
    match option_values "srvmaxhosts" options with
    | [] -> 0
    | [ value ] -> parse_nonnegative_int "srvMaxHosts" value
    | _ -> invalid_arg "srvMaxHosts must not appear more than once"
  in
  (service_name, max_hosts)

let options_to_query options =
  String.concat "&" (List.map (fun (key, value) -> key ^ "=" ^ value) options)

let warning_option = function
  | "tls" | "ssl" | "tlsinsecure" | "tlsallowinvalidcertificates"
  | "tlsallowinvalidhostnames" | "tlsdisableocspendpointcheck"
  | "tlsdisablecertificaterevocationcheck" | "directconnection" | "loadbalanced"
  | "serverselectiontimeoutms" | "connecttimeoutms" | "sockettimeoutms"
  | "timeoutms"
  | "localthresholdms" | "maxpoolsize" | "minpoolsize" | "maxidletimems" | "waitqueuetimeoutms"
  | "heartbeatfrequencyms" | "retryreads" | "retrywrites" | "readconcernlevel"
  | "w" | "journal" | "wtimeoutms" | "readpreference" | "readpreferencetags"
  | "maxstalenessseconds" ->
      true
  | _ -> false

let invalid_option_value_message message =
  String.starts_with ~prefix:"invalid boolean URI option:" message
  || String.starts_with ~prefix:"invalid integer for " message
  || String.ends_with ~suffix:" must be nonnegative" message
  || String.ends_with ~suffix:" must be positive" message
  || String.starts_with ~prefix:"heartbeatfrequencyms must be at least " message
  || String.starts_with ~prefix:"unknown readPreference:" message
  || message = "invalid readPreferenceTags"

let parse_options config query =
  if query = "" then { config; warnings = [] }
  else
    let options = split_on_char '&' query |> List.map parse_option_pair in
    validate_duplicate_bool_aliases options;
    validate_tls_conflicts options;
    let config, warnings =
      List.fold_left
        (fun (config, warnings) (key, value) ->
          try (apply_option config key value, warnings)
          with Invalid_argument message ->
            if warning_option key && invalid_option_value_message message then
              (config, Printf.sprintf "%s ignored: %s" key message :: warnings)
            else raise (Invalid_argument message))
        (config, []) options
    in
    { config; warnings = List.rev warnings }

let parse_auth userinfo (config : Mongo_config.t) =
  match String.index_opt userinfo ':' with
  | None ->
      let username = percent_decode userinfo in
      {
        config with
        credentials =
          Some
            {
              username;
              password = "";
              auth_source = None;
              auth_mechanism = None;
            };
      }
  | Some idx ->
      let username = percent_decode (String.sub userinfo 0 idx) in
      let password =
        percent_decode
          (String.sub userinfo (idx + 1) (String.length userinfo - idx - 1))
      in
      ({
         config with
         credentials =
           Some
             { username; password; auth_source = None; auth_mechanism = None };
       }
        : Mongo_config.t)

let finalize_credentials ~database_supplied (config : Mongo_config.t) =
  let auth_source = if database_supplied then config.database else "admin" in
  {
    config with
    credentials =
      Option.map
        (fun (c : Mongo_config.credentials) ->
          match c.auth_source with
          | Some _ -> c
          | None -> { c with auth_source = Some auth_source })
        config.credentials;
  }

let validate_common ~srv (config : Mongo_config.t) =
  if config.direct_connection && (srv || List.length config.hosts > 1) then
    invalid_arg
      "directConnection=true cannot be used with multiple seeds or SRV URIs";
  if config.max_pool_size > 0 && config.min_pool_size > config.max_pool_size
  then invalid_arg "minPoolSize must not exceed maxPoolSize";
  config

let parse_mongodb uri =
  let without_scheme =
    if String.starts_with ~prefix:"mongodb://" uri then
      String.sub uri 10 (String.length uri - 10)
    else invalid_arg "expected mongodb:// URI"
  in
  let path_start = String.index_opt without_scheme '/' in
  let authority =
    match path_start with
    | None -> without_scheme
    | Some idx -> String.sub without_scheme 0 idx
  in
  if String.contains authority '?' then
    invalid_arg "URI options require '/' before '?'";
  let path_and_query =
    match path_start with
    | None -> ""
    | Some idx ->
        String.sub without_scheme idx (String.length without_scheme - idx)
  in
  let query_start = String.index_opt path_and_query '?' in
  let path =
    match query_start with
    | None -> path_and_query
    | Some idx -> String.sub path_and_query 0 idx
  in
  let query =
    match query_start with
    | None -> ""
    | Some idx ->
        String.sub path_and_query (idx + 1)
          (String.length path_and_query - idx - 1)
  in
  let database =
    if path = "" || path = "/" then "test"
    else String.sub path 1 (String.length path - 1) |> percent_decode
  in
  let database_supplied = not (path = "" || path = "/") in
  let config = Mongo_config.default ~database () in
  let at = String.index_opt authority '@' in
  let config =
    match at with
    | None -> config
    | Some idx -> parse_auth (String.sub authority 0 idx) config
  in
  let hosts_part =
    match at with
    | None -> authority
    | Some idx ->
        String.sub authority (idx + 1) (String.length authority - idx - 1)
  in
  let config = { config with hosts = parse_hosts hosts_part } in
  let options =
    if query = "" then []
    else split_on_char '&' query |> List.map parse_option_pair
  in
  reject_non_srv_options options;
  let parsed = parse_options config query in
  {
    parsed with
    config =
      parsed.config
      |> finalize_credentials ~database_supplied
      |> validate_common ~srv:false;
  }

let parse_mongodb_srv uri =
  let host =
    if String.starts_with ~prefix:"mongodb+srv://" uri then
      String.sub uri 14 (String.length uri - 14)
    else invalid_arg "expected mongodb+srv:// URI"
  in
  let query_start = String.index_opt host '?' in
  let hostname =
    match query_start with None -> host | Some idx -> String.sub host 0 idx
  in
  let query =
    match query_start with
    | None -> ""
    | Some idx -> String.sub host (idx + 1) (String.length host - idx - 1)
  in
  let uri_options =
    if query = "" then []
    else split_on_char '&' query |> List.map parse_option_pair
  in
  let srv_service_name, srv_max_hosts = srv_option_defaults uri_options in
  let path_start = String.index_opt hostname '/' in
  let srv_host =
    match path_start with
    | None -> hostname
    | Some idx -> String.sub hostname 0 idx
  in
  let at = String.index_opt srv_host '@' in
  let userinfo, srv_host =
    match at with
    | None -> (None, srv_host)
    | Some idx ->
        ( Some (String.sub srv_host 0 idx),
          String.sub srv_host (idx + 1) (String.length srv_host - idx - 1) )
  in
  if String.contains srv_host ',' then
    invalid_arg "mongodb+srv URI must include exactly one host";
  if String.contains srv_host ':' then
    invalid_arg "mongodb+srv URI must not include a port";
  let database =
    match path_start with
    | None -> "test"
    | Some idx ->
        let path = String.sub hostname idx (String.length hostname - idx) in
        if path = "/" then "test"
        else String.sub path 1 (String.length path - 1) |> percent_decode
  in
  let database_supplied =
    match path_start with
    | None -> false
    | Some idx -> idx + 1 < String.length hostname
  in
  let config =
    Mongo_config.default ~database () |> fun cfg ->
    {
      cfg with
      tls =
        Mongo_config.Enabled
          {
            ca_file = None;
            allow_invalid_certificates = false;
            server_name = Some srv_host;
          };
    }
  in
  let config =
    match userinfo with
    | None -> config
    | Some userinfo -> parse_auth userinfo config
  in
  if srv_max_hosts > 0 && config.replica_set <> None then
    invalid_arg "srvMaxHosts cannot be combined with replicaSet";
  if srv_max_hosts > 0 && contains_option "replicaset" uri_options then
    invalid_arg "srvMaxHosts cannot be combined with replicaSet";
  if
    srv_max_hosts > 0
    && List.exists
         (fun (key, value) -> key = "loadbalanced" && parse_bool value)
         uri_options
  then invalid_arg "srvMaxHosts cannot be combined with loadBalanced=true";
  match
    Mongo_dns.resolve ~service:srv_service_name ~max_hosts:srv_max_hosts
      srv_host
  with
  | Ok seedlist ->
      let txt_query =
        match seedlist.txt_options with
        | None -> ""
        | Some value ->
            let txt_options =
              split_on_char '&' value |> List.map parse_option_pair
            in
            validate_txt_options txt_options;
            options_to_query txt_options
      in
      let query =
        if txt_query = "" then query
        else if query = "" then txt_query
        else txt_query ^ "&" ^ query
      in
      let parsed = parse_options { config with hosts = seedlist.hosts } query in
      if srv_max_hosts > 0 && parsed.config.replica_set <> None then
        invalid_arg "srvMaxHosts cannot be combined with replicaSet";
      {
        parsed with
        config =
          parsed.config
          |> finalize_credentials ~database_supplied
          |> validate_common ~srv:true;
      }
  | Error err -> raise (Mongo_error.to_exn err)

let of_string_with_warnings uri =
  try
    if String.starts_with ~prefix:"mongodb+srv://" uri then
      Ok (parse_mongodb_srv uri)
    else if String.starts_with ~prefix:"mongodb://" uri then
      Ok (parse_mongodb uri)
    else
      Error
        (Mongo_error.Unsupported
           "URI must start with mongodb:// or mongodb+srv://")
  with
  | Invalid_argument message -> Error (Protocol message)
  | Mongo_error.Mongo_failed message -> Error (Protocol message)

let of_string uri =
  match of_string_with_warnings uri with
  | Ok parsed -> Ok parsed.config
  | Error err -> Error err
