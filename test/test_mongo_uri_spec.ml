open Alcotest

type fixture_case = { file : string; description : string }

let fixture_dirs =
  let exe_dir =
    if Array.length Sys.argv = 0 then "." else Filename.dirname Sys.argv.(0)
  in
  [
    Filename.concat exe_dir "fixtures/imported/uri-options/tests";
    "test/fixtures/imported/uri-options/tests";
    "vendor/mongo-eio/test/fixtures/imported/uri-options/tests";
  ]

let fixture_path file =
  match
    List.find_map
      (fun dir ->
        let path = Filename.concat dir file in
        if Sys.file_exists path then Some path else None)
      fixture_dirs
  with
  | Some path -> path
  | None ->
      failwith
        (Printf.sprintf
           "missing URI fixture %s; run scripts/import-spec-fixtures.sh" file)

let bool_member name json =
  Yojson.Safe.Util.member name json |> Yojson.Safe.Util.to_bool

let string_member name json =
  Yojson.Safe.Util.member name json |> Yojson.Safe.Util.to_string

let option_doc json = Yojson.Safe.Util.member "options" json

let json_member name json =
  match Yojson.Safe.Util.member name json with
  | `Null -> None
  | value -> Some value

let int_option name options =
  match json_member name options with
  | Some (`Int value) -> Some value
  | Some (`Intlit value) -> Some (int_of_string value)
  | Some _ -> failwith ("expected integer option " ^ name)
  | None -> None

let bool_option name options =
  match json_member name options with
  | Some (`Bool value) -> Some value
  | Some _ -> failwith ("expected boolean option " ^ name)
  | None -> None

let string_option name options =
  match json_member name options with
  | Some (`String value) -> Some value
  | Some _ -> failwith ("expected string option " ^ name)
  | None -> None

let write_concern_w_option options =
  match json_member "w" options with
  | None -> None
  | Some (`Int value) -> Some (`Nodes value)
  | Some (`Intlit value) -> Some (`Nodes (int_of_string value))
  | Some (`String "majority") -> Some `Majority
  | Some (`String value) -> Some (`Tag value)
  | Some _ -> failwith "expected string or integer option w"

let read_preference_tags_option options =
  match json_member "readPreferenceTags" options with
  | None -> None
  | Some (`List tag_sets) ->
      Some
        (List.map
           (function
             | `Assoc fields ->
                 List.map
                   (function
                     | key, `String value -> (key, value)
                     | _ -> failwith "expected string readPreferenceTags value")
                   fields
             | _ -> failwith "expected readPreferenceTags object")
           tag_sets)
  | Some _ -> failwith "expected readPreferenceTags array"

let check_int_option name expected actual =
  match expected with
  | None -> ()
  | Some expected -> check int name expected actual

let check_string_option name expected actual =
  match expected with
  | None -> ()
  | Some expected -> check (option string) name (Some expected) actual

let check_bool_option name expected actual =
  match expected with
  | None -> ()
  | Some expected -> check bool name expected actual

let check_read_preference options actual =
  match string_option "readPreference" options with
  | None -> ()
  | Some value ->
      let expected =
        match String.lowercase_ascii value with
        | "primary" -> Mongo_config.Primary
        | "primarypreferred" -> PrimaryPreferred
        | "secondary" -> Secondary
        | "secondarypreferred" -> SecondaryPreferred
        | "nearest" -> Nearest
        | other -> failwith ("unsupported readPreference fixture: " ^ other)
      in
      check bool "readPreference" true (actual = expected)

let check_read_preference_options options (config : Mongo_config.t) =
  check_read_preference options config.read_preference;
  (match read_preference_tags_option options with
  | None -> ()
  | Some expected ->
      check
        (list (list (pair string string)))
        "readPreferenceTags" expected config.read_preference_tags);
  (match int_option "maxStalenessSeconds" options with
  | None -> ()
  | Some expected ->
      check (option int) "maxStalenessSeconds" (Some expected)
        config.max_staleness_seconds)

let check_concern_options options (config : Mongo_config.t) =
  (match string_option "readConcernLevel" options with
  | None -> ()
  | Some level ->
      let expected =
        match String.lowercase_ascii level with
        | "local" -> Mongo_config.Local
        | "majority" -> Majority
        | "linearizable" -> Linearizable
        | "available" -> Available
        | "snapshot" -> Snapshot
        | _ -> Custom level
      in
      check bool "readConcernLevel" true (config.read_concern = Some expected));
  (match write_concern_w_option options with
  | None -> ()
  | Some expected -> (
      match config.write_concern with
      | None -> fail "expected writeConcern for w"
      | Some concern -> check bool "w" true (concern.w = Some expected)));
  (match bool_option "journal" options with
  | None -> ()
  | Some expected -> (
      match config.write_concern with
      | None -> fail "expected writeConcern for journal"
      | Some concern -> check (option bool) "journal" (Some expected) concern.j));
  (match int_option "wTimeoutMS" options with
  | None -> ()
  | Some expected -> (
      match config.write_concern with
      | None -> fail "expected writeConcern for wTimeoutMS"
      | Some concern ->
          check (option int) "wTimeoutMS" (Some expected)
            concern.wtimeout_ms))

let check_auth_options options (config : Mongo_config.t) =
  match config.credentials with
  | None ->
      if
        string_option "authSource" options <> None
        || string_option "authMechanism" options <> None
      then fail "expected credentials for auth fixture"
  | Some creds -> (
      check_string_option "authSource"
        (string_option "authSource" options)
        creds.auth_source;
      match string_option "authMechanism" options with
      | None -> ()
      | Some "SCRAM-SHA-1" ->
          check bool "authMechanism" true
            (creds.auth_mechanism = Some `Scram_sha_1)
      | Some "SCRAM-SHA-256" ->
          check bool "authMechanism" true
            (creds.auth_mechanism = Some `Scram_sha_256)
      | Some other -> failwith ("unsupported authMechanism fixture: " ^ other))

let check_tls_options options (config : Mongo_config.t) =
  let tls_expected =
    match (bool_option "tls" options, bool_option "ssl" options) with
    | Some value, _ | _, Some value -> Some value
    | None, None -> None
  in
  match (tls_expected, config.tls) with
  | Some true, Mongo_config.Enabled tls ->
      check_string_option "tlsCAFile"
        (string_option "tlsCAFile" options)
        tls.ca_file
  | Some true, Disabled -> fail "expected TLS enabled"
  | Some false, Enabled _ -> fail "expected TLS disabled"
  | Some false, Disabled | None, _ -> ()

let check_supported_options options (config : Mongo_config.t) =
  check_string_option "appname"
    (string_option "appname" options)
    config.app_name;
  check_string_option "appName"
    (string_option "appName" options)
    config.app_name;
  check_string_option "replicaSet"
    (string_option "replicaSet" options)
    config.replica_set;
  check_int_option "connectTimeoutMS"
    (int_option "connectTimeoutMS" options)
    config.connect_timeout_ms;
  check_int_option "serverSelectionTimeoutMS"
    (int_option "serverSelectionTimeoutMS" options)
    config.server_selection_timeout_ms;
  check_int_option "localThresholdMS"
    (int_option "localThresholdMS" options)
    config.local_threshold_ms;
  check_int_option "heartbeatFrequencyMS"
    (int_option "heartbeatFrequencyMS" options)
    config.heartbeat_frequency_ms;
  (match int_option "socketTimeoutMS" options with
  | None -> ()
  | Some expected ->
      check (option int) "socketTimeoutMS" (Some expected)
        config.socket_timeout_ms);
  (match int_option "timeoutMS" options with
  | None -> ()
  | Some expected ->
      check (option int) "timeoutMS" (Some expected) config.timeout_ms);
  check_int_option "maxPoolSize"
    (int_option "maxPoolSize" options)
    config.max_pool_size;
  check_int_option "minPoolSize"
    (int_option "minPoolSize" options)
    config.min_pool_size;
  (match int_option "maxIdleTimeMS" options with
  | None -> ()
  | Some expected ->
      check (option int) "maxIdleTimeMS" (Some expected) config.max_idle_time_ms);
  check_bool_option "retryReads"
    (bool_option "retryReads" options)
    config.retry_reads;
  check_bool_option "retryWrites"
    (bool_option "retryWrites" options)
    config.retry_writes;
  check_bool_option "directConnection"
    (bool_option "directConnection" options)
    config.direct_connection;
  check_concern_options options config;
  check_read_preference_options options config;
  check_auth_options options config;
  check_tls_options options config

let find_case file description =
  let path = fixture_path file in
  let tests =
    Yojson.Safe.from_file path
    |> Yojson.Safe.Util.member "tests"
    |> Yojson.Safe.Util.to_list
  in
  match
    List.find_opt
      (fun test -> string_member "description" test = description)
      tests
  with
  | Some test -> test
  | None ->
      failwith (Printf.sprintf "missing fixture case %s: %s" file description)

let run_case { file; description } () =
  let test = find_case file description in
  let uri = string_member "uri" test in
  let valid = bool_member "valid" test in
  let warning = bool_member "warning" test in
  match (valid, Mongo_uri.of_string_with_warnings uri) with
  | true, Ok parsed -> (
      if warning then check bool "has warnings" true (parsed.warnings <> [])
      else check (list string) "warnings" [] parsed.warnings;
      match option_doc test with
      | `Null -> ()
      | options -> check_supported_options options parsed.config)
  | true, Error err ->
      fail
        (Printf.sprintf "expected valid URI %s, got %s" uri
           (Mongo_error.to_string err))
  | false, Error _ -> ()
  | false, Ok _ -> fail ("expected invalid URI: " ^ uri)

let supported_cases =
  [
    {
      file = "auth-options.json";
      description = "Valid auth options are parsed correctly (SCRAM-SHA-1)";
    };
    {
      file = "connection-options.json";
      description = "Valid connection and timeout options are parsed correctly";
    };
    {
      file = "connection-options.json";
      description = "Non-numeric connectTimeoutMS causes a warning";
    };
    {
      file = "connection-options.json";
      description = "Too low connectTimeoutMS causes a warning";
    };
    {
      file = "connection-options.json";
      description = "Non-numeric heartbeatFrequencyMS causes a warning";
    };
    {
      file = "connection-options.json";
      description = "Too low heartbeatFrequencyMS causes a warning";
    };
    {
      file = "connection-options.json";
      description = "Invalid retryWrites causes a warning";
    };
    {
      file = "connection-options.json";
      description = "Non-numeric serverSelectionTimeoutMS causes a warning";
    };
    {
      file = "connection-options.json";
      description = "Too low serverSelectionTimeoutMS causes a warning";
    };
    {
      file = "connection-options.json";
      description = "Non-numeric socketTimeoutMS causes a warning";
    };
    {
      file = "connection-options.json";
      description = "Too low socketTimeoutMS causes a warning";
    };
    { file = "connection-options.json"; description = "timeoutMS=0" };
    {
      file = "connection-options.json";
      description = "Non-numeric timeoutMS causes a warning";
    };
    {
      file = "connection-options.json";
      description = "Too low timeoutMS causes a warning";
    };
    { file = "connection-options.json"; description = "directConnection=true" };
    {
      file = "connection-options.json";
      description = "directConnection=true with multiple seeds";
    };
    { file = "connection-options.json"; description = "directConnection=false" };
    {
      file = "connection-options.json";
      description = "directConnection=false with multiple seeds";
    };
    {
      file = "connection-options.json";
      description = "Invalid directConnection value";
    };
    { file = "connection-options.json"; description = "loadBalanced=false" };
    {
      file = "connection-options.json";
      description = "Invalid loadBalanced value";
    };
    {
      file = "connection-pool-options.json";
      description = "Valid connection pool options are parsed correctly";
    };
    {
      file = "connection-pool-options.json";
      description = "Non-numeric maxIdleTimeMS causes a warning";
    };
    {
      file = "connection-pool-options.json";
      description = "Too low maxIdleTimeMS causes a warning";
    };
    {
      file = "connection-pool-options.json";
      description = "maxPoolSize=0 does not error";
    };
    {
      file = "connection-pool-options.json";
      description = "minPoolSize=0 does not error";
    };
    {
      file = "concern-options.json";
      description = "Valid read and write concern are parsed correctly";
    };
    {
      file = "concern-options.json";
      description =
        "Arbitrary string readConcernLevel does not cause a warning";
    };
    {
      file = "concern-options.json";
      description = "Arbitrary string w doesn't cause a warning";
    };
    {
      file = "concern-options.json";
      description = "Non-numeric wTimeoutMS causes a warning";
    };
    {
      file = "concern-options.json";
      description = "Too low wTimeoutMS causes a warning";
    };
    {
      file = "concern-options.json";
      description = "Invalid journal causes a warning";
    };
    {
      file = "read-preference-options.json";
      description = "Valid read preference options are parsed correctly";
    };
    {
      file = "read-preference-options.json";
      description = "Single readPreferenceTags is parsed as array of size one";
    };
    {
      file = "read-preference-options.json";
      description = "Read preference tags are case sensitive";
    };
    {
      file = "read-preference-options.json";
      description = "Invalid readPreferenceTags causes a warning";
    };
    {
      file = "read-preference-options.json";
      description = "Non-numeric maxStalenessSeconds causes a warning";
    };
    {
      file = "read-preference-options.json";
      description = "Too low maxStalenessSeconds causes a warning";
    };
    {
      file = "srv-options.json";
      description = "Non-SRV URI with custom srvServiceName";
    };
    { file = "srv-options.json"; description = "Non-SRV URI with srvMaxHosts" };
    {
      file = "tls-options.json";
      description = "tls=true and ssl=true doesn't warn";
    };
    {
      file = "tls-options.json";
      description = "tls=false and ssl=false doesn't warn";
    };
    {
      file = "tls-options.json";
      description = "Invalid tlsAllowInvalidCertificates causes a warning";
    };
    {
      file = "tls-options.json";
      description = "Invalid tlsAllowInvalidHostnames causes a warning";
    };
    {
      file = "tls-options.json";
      description = "Invalid tlsInsecure causes a warning";
    };
    {
      file = "tls-options.json";
      description = "tls=false and ssl=true raises error";
    };
    {
      file = "tls-options.json";
      description = "tls=true and ssl=false raises error";
    };
    {
      file = "tls-options.json";
      description =
        "tlsInsecure and tlsAllowInvalidCertificates both present (and true) \
         raises an error";
    };
    {
      file = "tls-options.json";
      description =
        "tlsAllowInvalidCertificates and tlsInsecure both present (and false) \
         raises an error";
    };
  ]

let () =
  run "mongo_uri_spec"
    [
      ( "official URI option fixtures",
        List.map
          (fun fixture ->
            test_case
              (Filename.remove_extension fixture.file
              ^ ": " ^ fixture.description)
              `Quick (run_case fixture))
          supported_cases );
    ]
