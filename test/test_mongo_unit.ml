open Alcotest

let test_default_config () =
  let config = Mongo_config.default () in
  check string "host" "127.0.0.1" (fst (List.hd config.hosts));
  check int "port" 27017 (snd (List.hd config.hosts));
  check (option int) "timeoutMS" None config.timeout_ms;
  check int "localThresholdMS" 15 config.local_threshold_ms;
  check int "heartbeatFrequencyMS" 10_000 config.heartbeat_frequency_ms

let credentials ?auth_mechanism () =
  { Mongo_config.username = "user"; password = "secret"; auth_source = None; auth_mechanism }

let test_auth_mechanism_selection () =
  check bool "explicit SHA-1 wins" true
    (Mongo_auth.choose_mechanism
       ?sasl_supported_mechs:(Some [ "SCRAM-SHA-256" ])
       (credentials ~auth_mechanism:`Scram_sha_1 ())
    = Mongo_scram.Scram_sha_1);
  check bool "negotiates SHA-256 when supported" true
    (Mongo_auth.choose_mechanism
       ?sasl_supported_mechs:(Some [ "SCRAM-SHA-1"; "SCRAM-SHA-256" ])
       (credentials ())
    = Mongo_scram.Scram_sha_256);
  check bool "falls back to SHA-1 from negotiated list" true
    (Mongo_auth.choose_mechanism
       ?sasl_supported_mechs:(Some [ "SCRAM-SHA-1" ])
       (credentials ())
    = Mongo_scram.Scram_sha_1);
  check bool "legacy default is SHA-1 when no negotiated list" true
    (Mongo_auth.choose_mechanism (credentials ()) = Mongo_scram.Scram_sha_1)

let () =
  run "mongo unit"
    [
      ("config", [ test_case "default config" `Quick test_default_config ]);
      ( "auth",
        [
          test_case "mechanism selection" `Quick test_auth_mechanism_selection;
        ] );
    ]
