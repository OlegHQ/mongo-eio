open Alcotest

let test_parse_mongodb_uri () =
  match
    Mongo_uri.of_string
      ("mongodb://alice:pass%40word@host1:27017,host2/db?retryWrites=false&waitQueueTimeoutMS=250"
     ^ "&maxIdleTimeMS=0")
  with
  | Ok config -> (
      check string "database" "db" config.database;
      check bool "retry writes" false config.retry_writes;
      check int "wait queue timeout" 250 config.wait_queue_timeout_ms;
      check (option int) "max idle time" (Some 0) config.max_idle_time_ms;
      check int "hosts" 2 (List.length config.hosts);
      match config.credentials with
      | Some creds ->
          check string "username" "alice" creds.username;
          check string "password" "pass@word" creds.password;
          check (option string) "auth source" (Some "db") creds.auth_source
      | None -> fail "expected credentials")
  | Error err -> fail (Mongo_error.to_string err)

let test_auth_source_defaults_to_admin_without_uri_database () =
  match Mongo_uri.of_string "mongodb://alice:secret@host" with
  | Ok config -> (
      match config.credentials with
      | Some creds ->
          check (option string) "auth source" (Some "admin") creds.auth_source
      | None -> fail "expected credentials")
  | Error err -> fail (Mongo_error.to_string err)

let test_userinfo_without_colon_still_configures_auth () =
  match Mongo_uri.of_string "mongodb://alice@host/db" with
  | Ok config -> (
      match config.credentials with
      | Some creds ->
          check string "username" "alice" creds.username;
          check string "password" "" creds.password;
          check (option string) "auth source" (Some "db") creds.auth_source
      | None -> fail "expected credentials")
  | Error err -> fail (Mongo_error.to_string err)

let test_auth_mechanism () =
  match
    Mongo_uri.of_string
      "mongodb://alice:secret@host/db?authMechanism=SCRAM-SHA-1"
  with
  | Ok config -> (
      match config.credentials with
      | Some creds ->
          check bool "mechanism" true (creds.auth_mechanism = Some `Scram_sha_1)
      | None -> fail "expected credentials")
  | Error err -> fail (Mongo_error.to_string err)

let test_percent_decoding () =
  match
    Mongo_uri.of_string
      "mongodb://user%40name:p%40ss%2Fword@host/db%2Dname?authSource=auth%2Ddb&appName=poster%20test"
  with
  | Ok config -> (
      check string "database" "db-name" config.database;
      check (option string) "app name" (Some "poster test") config.app_name;
      match config.credentials with
      | Some creds ->
          check string "username" "user@name" creds.username;
          check string "password" "p@ss/word" creds.password;
          check (option string) "auth source" (Some "auth-db") creds.auth_source
      | None -> fail "expected credentials")
  | Error err -> fail (Mongo_error.to_string err)

let test_ipv6_hosts () =
  match Mongo_uri.of_string "mongodb://[::1]:27018,[2001:db8::1]/db" with
  | Ok config ->
      check
        (list (pair string int))
        "hosts"
        [ ("::1", 27018); ("2001:db8::1", 27017) ]
        config.hosts
  | Error err -> fail (Mongo_error.to_string err)

let expect_protocol_error uri =
  match Mongo_uri.of_string uri with
  | Ok _ -> fail ("expected URI error for " ^ uri)
  | Error (Mongo_error.Protocol _) -> ()
  | Error err -> fail (Mongo_error.to_string err)

let test_invalid_auth_source () =
  expect_protocol_error "mongodb://alice:secret@host/db?authSource="

let test_invalid_numeric_options () =
  expect_protocol_error "mongodb://host/db?maxPoolSize=2&minPoolSize=3"

let test_warning_options () =
  (match
     Mongo_uri.of_string_with_warnings
       "mongodb://host/db?connectTimeoutMS=-1&retryWrites=invalid&loadBalanced=1&tlsAllowInvalidHostnames=invalid&readPreferenceTags=invalid&maxStalenessSeconds=invalid&w=-1"
   with
  | Ok parsed ->
      check int "warnings" 7 (List.length parsed.warnings);
      check int "connect timeout default" 10_000
        parsed.config.connect_timeout_ms;
      check bool "retry writes default" true parsed.config.retry_writes
  | Error err -> fail (Mongo_error.to_string err));
  match Mongo_uri.of_string "mongodb://host/db?connectTimeoutMS=-1" with
  | Ok config -> check int "compat default" 10_000 config.connect_timeout_ms
  | Error err -> fail (Mongo_error.to_string err)

let test_invalid_hosts () =
  expect_protocol_error "mongodb:///db";
  expect_protocol_error "mongodb://host?retryWrites=false";
  expect_protocol_error "mongodb://host1,,host2/db";
  expect_protocol_error "mongodb://host1,/db";
  expect_protocol_error "mongodb://:27017/db";
  expect_protocol_error "mongodb://::1/db";
  expect_protocol_error "mongodb://[::1/db";
  expect_protocol_error "mongodb://[::1]extra/db"

let test_invalid_ports () =
  expect_protocol_error "mongodb://host:0/db";
  expect_protocol_error "mongodb://host:65536/db";
  expect_protocol_error "mongodb://host:-1/db";
  expect_protocol_error "mongodb://host:abc/db";
  expect_protocol_error "mongodb://[::1]:0/db";
  expect_protocol_error "mongodb://[::1]:65536/db"

let test_invalid_percent_encoding () =
  expect_protocol_error "mongodb://user%G0:secret@host/db";
  expect_protocol_error "mongodb://user%:secret@host/db";
  expect_protocol_error "mongodb://host/db?appName=poster%2"

let test_unsupported_auth_mechanism_properties () =
  expect_protocol_error
    "mongodb://alice:secret@host/db?authMechanismProperties=SERVICE_NAME:mongodb"

let test_tls_options () =
  expect_protocol_error "mongodb://host/db?tls=true&ssl=false";
  expect_protocol_error
    "mongodb://host/db?tlsInsecure=true&tlsAllowInvalidCertificates=true";
  expect_protocol_error "mongodb://host/db?tlsAllowInvalidHostnames=true";
  expect_protocol_error
    "mongodb://host/db?tlsCertificateKeyFile=/tmp/client.pem";
  expect_protocol_error "mongodb://host/db?tlsCertificateKeyFilePassword=secret";
  match Mongo_uri.of_string "mongodb://host/db?tlsCAFile=/tmp/ca.pem" with
  | Ok config -> (
      match config.tls with
      | Mongo_config.Enabled tls ->
          check (option string) "ca file" (Some "/tmp/ca.pem") tls.ca_file
      | Mongo_config.Disabled -> fail "expected TLS to be enabled")
  | Error err -> fail (Mongo_error.to_string err)

let test_load_balanced_unsupported () =
  expect_protocol_error "mongodb://host/db?loadBalanced=true"

let test_srv_options_rejected_on_mongodb () =
  expect_protocol_error "mongodb://host/db?srvServiceName=custom";
  expect_protocol_error "mongodb://host/db?srvMaxHosts=2"

let test_srv_option_helpers () =
  let options =
    [
      ("srvservicename", "custom"); ("srvmaxhosts", "2"); ("replicaset", "rs0");
    ]
  in
  let service, max_hosts = Mongo_uri.srv_option_defaults options in
  check string "service name" "custom" service;
  check int "max hosts" 2 max_hosts;
  Mongo_uri.validate_txt_options
    [ ("authsource", "admin"); ("replicaset", "rs0") ];
  match Mongo_uri.validate_txt_options [ ("retrywrites", "true") ] with
  | () -> fail "expected invalid TXT option"
  | exception Invalid_argument _ -> ()

let test_direct_connection_multiple_hosts () =
  expect_protocol_error "mongodb://host1,host2/db?directConnection=true"

let test_srv_validation () =
  expect_protocol_error "mongodb+srv://host:27017/db";
  expect_protocol_error "mongodb+srv://host1,host2/db";
  expect_protocol_error "mongodb+srv://host/db?directConnection=true"

let test_invalid_scheme () =
  match Mongo_uri.of_string "http://example.com" with
  | Ok _ -> fail "expected error"
  | Error (Mongo_error.Unsupported _) -> ()
  | Error err -> fail (Mongo_error.to_string err)

let () =
  run "mongo_uri"
    [
      ( "parse",
        [
          test_case "mongodb uri" `Quick test_parse_mongodb_uri;
          test_case "auth source defaults to admin" `Quick
            test_auth_source_defaults_to_admin_without_uri_database;
          test_case "userinfo without colon configures auth" `Quick
            test_userinfo_without_colon_still_configures_auth;
          test_case "auth mechanism" `Quick test_auth_mechanism;
          test_case "percent decoding" `Quick test_percent_decoding;
          test_case "ipv6 hosts" `Quick test_ipv6_hosts;
          test_case "invalid auth source" `Quick test_invalid_auth_source;
          test_case "invalid numeric options" `Quick
            test_invalid_numeric_options;
          test_case "warning options" `Quick test_warning_options;
          test_case "invalid hosts" `Quick test_invalid_hosts;
          test_case "invalid ports" `Quick test_invalid_ports;
          test_case "invalid percent encoding" `Quick
            test_invalid_percent_encoding;
          test_case "unsupported auth mechanism properties" `Quick
            test_unsupported_auth_mechanism_properties;
          test_case "tls options" `Quick test_tls_options;
          test_case "load balanced unsupported" `Quick
            test_load_balanced_unsupported;
          test_case "srv options rejected on mongodb" `Quick
            test_srv_options_rejected_on_mongodb;
          test_case "srv option helpers" `Quick test_srv_option_helpers;
          test_case "direct connection multiple hosts" `Quick
            test_direct_connection_multiple_hosts;
          test_case "srv validation" `Quick test_srv_validation;
          test_case "invalid scheme" `Quick test_invalid_scheme;
        ] );
    ]
