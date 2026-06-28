open Alcotest

let dummy_server ?(logical_session_timeout_minutes = Some 30)
    ?(set_name = Some "rs0") ?(is_mongos = false) () :
    Mongo_connection.server_info =
  {
    max_wire_version = Mongo_config.max_supported_wire_version;
    min_wire_version = Mongo_config.min_supported_wire_version;
    is_writable_primary = true;
    secondary = false;
    set_name;
    hosts = [];
    passives = [];
    arbiters = [];
    is_mongos;
    logical_session_timeout_minutes;
    sasl_supported_mechs = None;
    service_id = None;
    tags = [];
    last_write_date = None;
    round_trip_time_ms = None;
    round_trip_time_samples_ms = [];
  }

let ok_doc =
  Bson.add_element "ok" (Bson.create_double 1.0) Bson.empty

let hello_doc ?(set_name = None) ?(hosts = []) ?(is_writable_primary = true)
    ?(secondary = false) () =
  let doc =
    Mongo_command.document
      [
        ("ok", Bson.create_double 1.0);
        ("maxWireVersion", Bson.create_int32 (Int32.of_int Mongo_config.max_supported_wire_version));
        ("minWireVersion", Bson.create_int32 (Int32.of_int Mongo_config.min_supported_wire_version));
        ("isWritablePrimary", Bson.create_boolean is_writable_primary);
        ("secondary", Bson.create_boolean secondary);
        ("logicalSessionTimeoutMinutes", Bson.create_int32 30l);
      ]
  in
  let doc =
    match set_name with
    | None -> doc
    | Some name -> Bson.add_element "setName" (Bson.create_string name) doc
  in
  if hosts = [] then doc
  else
    Bson.add_element "hosts"
      (Bson.create_list (List.map Bson.create_string hosts))
      doc

let encode_reply ~request_id ~response_to body_doc =
  let body_buf = Buffer.create 128 in
  MongoUtils.encode_int32 body_buf 0l;
  Buffer.add_char body_buf '\x00';
  Buffer.add_string body_buf (Bson.encode body_doc);
  let header =
    MongoHeader.encode_header
      (MongoHeader.create_header (Buffer.length body_buf) request_id
         response_to MongoOperation.OP_MSG)
  in
  header ^ Buffer.contents body_buf

let run_and_capture ?logical_session_timeout_minutes ?set_name ?is_mongos
    ?command_event_handler ?config_update ?server_update ?(reply_doc = ok_doc)
    ?(retry_writes = true) fields =
  let client_fd, server_fd =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let config =
    { (Mongo_config.default ~database:"test" ()) with retry_writes }
  in
  let config =
    match config_update with None -> config | Some f -> f config
  in
  let server =
    dummy_server ?logical_session_timeout_minutes ?set_name ?is_mongos ()
  in
  let server =
    match server_update with None -> server | Some f -> f server
  in
  let conn =
    {
      Mongo_connection.host = "fake";
      port = 0;
      fd = client_fd;
      transport = Mongo_transport.plain client_fd;
      config;
      server;
      authenticated = false;
    }
  in
  let result = ref None in
  let thread =
    Thread.create
      (fun () ->
        result :=
          Some
            (Mongo_connection.run_command ?command_event_handler conn "test"
               fields))
      ()
  in
  let request = Mongo_wire.read_message server_fd in
  let header = MongoHeader.decode_header (String.sub request 0 16) in
  let command = Mongo_wire.response_message_doc request in
  let reply =
    encode_reply ~request_id:42l
      ~response_to:(MongoHeader.get_request_id header)
      reply_doc
  in
  (match Mongo_wire.write_all server_fd reply with
  | Error err ->
      Unix.close server_fd;
      Mongo_connection.close conn;
      fail (Mongo_error.to_string err)
  | Ok () -> ());
  Thread.join thread;
  Unix.close server_fd;
  Mongo_connection.close conn;
  ( command,
    match !result with
    | Some result -> result
    | None -> fail "missing command result" )

let require_ok = function
  | Ok _ -> ()
  | Error err -> fail (Mongo_error.to_string err)

let has_field name doc = Bson.has_element name doc

let command_event_name = function
  | Mongo_command.Command_started event ->
      "started:" ^ event.command_name
  | Command_succeeded event -> "succeeded:" ^ event.command_name
  | Command_failed event -> "failed:" ^ event.command_name

let event_recorder () =
  let events = ref [] in
  let handler event = events := event :: !events in
  let events () = List.rev !events in
  (handler, events)

let test_run_command_adds_implicit_lsid () =
  let command, result =
    run_and_capture
      [
        ("count", Bson.create_string "posts");
        ("query", Bson.create_doc_element Bson.empty);
      ]
  in
  require_ok result;
  check bool "has lsid" true (has_field "lsid" command);
  check bool "no txnNumber for read" false (has_field "txnNumber" command)

let test_run_command_adds_txn_number_for_retryable_write () =
  let command, result =
    run_and_capture
      [
        ("insert", Bson.create_string "posts");
        ("documents", Bson.create_doc_element_list [ Bson.empty ]);
      ]
  in
  require_ok result;
  check bool "has lsid" true (has_field "lsid" command);
  check int64 "txnNumber" 1L
    (Bson.get_int64 (Bson.get_element "txnNumber" command))

let test_run_command_skips_txn_number_for_nonretryable_write () =
  let command, result =
    run_and_capture
      [
        ("insert", Bson.create_string "posts");
        ("documents", Bson.create_doc_element_list [ Bson.empty; Bson.empty ]);
      ]
  in
  require_ok result;
  check bool "has lsid" true (has_field "lsid" command);
  check bool "no txnNumber" false (has_field "txnNumber" command)

let test_run_command_skips_txn_number_on_standalone () =
  let command, result =
    run_and_capture ~set_name:None
      [
        ("insert", Bson.create_string "posts");
        ("documents", Bson.create_doc_element_list [ Bson.empty ]);
      ]
  in
  require_ok result;
  check bool "has lsid" true (has_field "lsid" command);
  check bool "no txnNumber" false (has_field "txnNumber" command)

let test_run_command_skips_implicit_sessions_without_server_support () =
  let command, result =
    run_and_capture ~logical_session_timeout_minutes:None
      [
        ("count", Bson.create_string "posts");
        ("query", Bson.create_doc_element Bson.empty);
      ]
  in
  require_ok result;
  check bool "no lsid" false (has_field "lsid" command)

let test_run_command_applies_config_concerns () =
  let command, result =
    run_and_capture
      ~config_update:(fun config ->
        {
          config with
          read_concern = Some (Mongo_config.Custom "available");
          write_concern =
            Some
              { Mongo_config.w = Some (`Nodes 2); j = Some true; wtimeout_ms = Some 25 };
        })
      [
        ("count", Bson.create_string "posts");
        ("query", Bson.create_doc_element Bson.empty);
      ]
  in
  require_ok result;
  let read_concern =
    Bson.get_doc_element (Bson.get_element "readConcern" command)
  in
  check string "readConcern level" "available"
    (Bson.get_string (Bson.get_element "level" read_concern));
  let write_concern =
    Bson.get_doc_element (Bson.get_element "writeConcern" command)
  in
  check int "writeConcern w" 2
    (Int32.to_int (Bson.get_int32 (Bson.get_element "w" write_concern)));
  check bool "writeConcern j" true
    (Bson.get_boolean (Bson.get_element "j" write_concern));
  check int "writeConcern wtimeout" 25
    (Int32.to_int (Bson.get_int32 (Bson.get_element "wtimeout" write_concern)))

let test_run_command_applies_timeout_ms_as_max_time_ms () =
  let command, result =
    run_and_capture
      ~config_update:(fun config -> { config with timeout_ms = Some 123 })
      [
        ("count", Bson.create_string "posts");
        ("query", Bson.create_doc_element Bson.empty);
      ]
  in
  require_ok result;
  check int64 "maxTimeMS" 123L
    (Bson.get_int64 (Bson.get_element "maxTimeMS" command))

let test_run_command_subtracts_min_rtt_from_max_time_ms () =
  let command, result =
    run_and_capture
      ~config_update:(fun config -> { config with timeout_ms = Some 123 })
      ~server_update:(fun server ->
        {
          server with
          round_trip_time_ms = Some 40.0;
          round_trip_time_samples_ms = [ 40.0 ];
        })
      [
        ("count", Bson.create_string "posts");
        ("query", Bson.create_doc_element Bson.empty);
      ]
  in
  require_ok result;
  check int64 "single RTT sample uses zero min RTT" 123L
    (Bson.get_int64 (Bson.get_element "maxTimeMS" command));
  let command, result =
    run_and_capture
      ~config_update:(fun config -> { config with timeout_ms = Some 123 })
      ~server_update:(fun server ->
        {
          server with
          round_trip_time_ms = Some 40.0;
          round_trip_time_samples_ms = [ 40.0; 20.0 ];
        })
      [
        ("count", Bson.create_string "posts");
        ("query", Bson.create_doc_element Bson.empty);
      ]
  in
  require_ok result;
  check int64 "two RTT samples subtract min RTT" 103L
    (Bson.get_int64 (Bson.get_element "maxTimeMS" command))

let test_run_command_times_out_when_min_rtt_exhausts_budget () =
  let client_fd, server_fd =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let server =
    {
      (dummy_server ()) with
      round_trip_time_ms = Some 60.0;
      round_trip_time_samples_ms = [ 60.0; 50.0 ];
    }
  in
  let conn =
    {
      Mongo_connection.host = "fake";
      port = 0;
      fd = client_fd;
      transport = Mongo_transport.plain client_fd;
      config = { (Mongo_config.default ~database:"test" ()) with timeout_ms = Some 50 };
      server;
      authenticated = false;
    }
  in
  Fun.protect
    ~finally:(fun () ->
      Unix.close server_fd;
      Mongo_connection.close conn)
    (fun () ->
      match
        Mongo_connection.run_command conn "test"
          [
            ("count", Bson.create_string "posts");
            ("query", Bson.create_doc_element Bson.empty);
          ]
      with
      | Error (Mongo_error.Timeout _) -> ()
      | Error err -> fail (Mongo_error.to_string err)
      | Ok _ -> fail "expected timeout before command send")

let test_run_command_timeout_ms_zero_is_infinite () =
  let command, result =
    run_and_capture
      ~config_update:(fun config -> { config with timeout_ms = Some 0 })
      [
        ("count", Bson.create_string "posts");
        ("query", Bson.create_doc_element Bson.empty);
      ]
  in
  require_ok result;
  check bool "no maxTimeMS" false (Bson.has_element "maxTimeMS" command)

let test_run_command_does_not_add_max_time_ms_to_get_more () =
  let command, result =
    run_and_capture
      ~config_update:(fun config -> { config with timeout_ms = Some 123 })
      [
        ("getMore", Bson.create_int64 42L);
        ("collection", Bson.create_string "posts");
      ]
  in
  require_ok result;
  check bool "no maxTimeMS" false (Bson.has_element "maxTimeMS" command)

let test_command_monitoring_success () =
  let handler, events = event_recorder () in
  let _command, result =
    run_and_capture ~command_event_handler:handler
      [
        ("count", Bson.create_string "posts");
        ("query", Bson.create_doc_element Bson.empty);
      ]
  in
  require_ok result;
  check (list string) "event order"
    [ "started:count"; "succeeded:count" ]
    (List.map command_event_name (events ()));
  match events () with
  | Command_started started :: Command_succeeded succeeded :: [] ->
      check string "database" "test" started.database_name;
      check (option string) "connection id" (Some "fake:0")
        started.connection_id;
      check bool "duration recorded" true (succeeded.duration_ms >= 0.0);
      check int32 "request ids match" started.request_id succeeded.request_id;
      check bool "started command includes db" true
        (Bson.has_element "$db" started.command)
  | _ -> fail "unexpected command monitoring events"

let test_command_monitoring_failure () =
  let handler, events = event_recorder () in
  let failure_doc =
    Mongo_command.document
      [
        ("ok", Bson.create_double 0.0);
        ("errmsg", Bson.create_string "no such command");
        ("code", Bson.create_int32 59l);
        ("codeName", Bson.create_string "CommandNotFound");
      ]
  in
  let _command, result =
    run_and_capture ~command_event_handler:handler ~reply_doc:failure_doc
      [ ("notACommand", Bson.create_int32 1l) ]
  in
  (match result with
  | Error (Mongo_error.Command _) -> ()
  | Ok _ -> fail "expected command failure"
  | Error err -> fail (Mongo_error.to_string err));
  check (list string) "failure event order"
    [ "started:notACommand"; "failed:notACommand" ]
    (List.map command_event_name (events ()));
  match events () with
  | Command_started started :: Command_failed failed :: [] ->
      check int32 "failure request ids match" started.request_id
        failed.request_id;
      check bool "failure duration recorded" true (failed.duration_ms >= 0.0);
      check (option int) "failure code" (Some 59) (Mongo_error.code failed.failure)
  | _ -> fail "unexpected command failure events"

let test_command_monitoring_redacts_sensitive_commands () =
  let handler, events = event_recorder () in
  let _command, result =
    run_and_capture ~logical_session_timeout_minutes:None
      ~command_event_handler:handler
      [
        ("saslStart", Bson.create_int32 1l);
        ("payload", Bson.create_string "secret");
      ]
  in
  require_ok result;
  match events () with
  | Command_started started :: Command_succeeded succeeded :: [] ->
      check bool "started payload redacted" false
        (Bson.has_element "payload" started.command);
      check string "started redacted command" "<redacted>"
        (Bson.get_string (Bson.get_element "saslStart" started.command));
      check bool "reply redacted" false
        (Bson.has_element "ok" succeeded.reply);
      check string "succeeded redacted reply" "<redacted>"
        (Bson.get_string (Bson.get_element "saslStart" succeeded.reply))
  | _ -> fail "unexpected sensitive command events"

let listen_loopback () =
  let server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt server Unix.SO_REUSEADDR true;
  Unix.bind server (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen server 1;
  let port =
    match Unix.getsockname server with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> failwith "expected inet socket"
  in
  (server, port)

let test_handshake_read_timeout () =
  let server, port = listen_loopback () in
  let child =
    Unix.fork ()
  in
  if child = 0 then (
    let client, _ = Unix.accept server in
    Unix.close server;
    Unix.sleep 2;
    Unix.close client;
    exit 0)
  else
    Fun.protect
      ~finally:(fun () ->
        Unix.close server;
        (try Unix.kill child Sys.sigkill with _ -> ());
        ignore (Unix.waitpid [] child))
      (fun () ->
        let config =
          {
            (Mongo_config.default ~host:"127.0.0.1" ~port ~database:"admin" ())
            with
            connect_timeout_ms = 1_000;
            socket_timeout_ms = Some 50;
            server_selection_timeout_ms = 100;
          }
        in
        match Mongo_connection.connect config with
        | Error (Mongo_error.Timeout _) -> ()
        | Ok conn ->
            Mongo_connection.close conn;
            fail "expected handshake timeout"
        | Error err -> fail (Mongo_error.to_string err))

let test_timeout_ms_limits_handshake_read () =
  let server, port = listen_loopback () in
  let child = Unix.fork () in
  if child = 0 then (
    let client, _ = Unix.accept server in
    Unix.close server;
    Unix.sleep 2;
    Unix.close client;
    exit 0)
  else
    Fun.protect
      ~finally:(fun () ->
        Unix.close server;
        (try Unix.kill child Sys.sigkill with _ -> ());
        ignore (Unix.waitpid [] child))
      (fun () ->
        let config =
          {
            (Mongo_config.default ~host:"127.0.0.1" ~port ~database:"admin" ())
            with
            connect_timeout_ms = 1_000;
            socket_timeout_ms = Some 1_000;
            timeout_ms = Some 50;
            server_selection_timeout_ms = 1_000;
          }
        in
        let started_at = Unix.gettimeofday () in
        match Mongo_connection.connect config with
        | Error (Mongo_error.Timeout _) ->
            let elapsed_ms = (Unix.gettimeofday () -. started_at) *. 1000.0 in
            check bool "handshake used timeoutMS" true (elapsed_ms < 500.0)
        | Ok conn ->
            Mongo_connection.close conn;
            fail "expected timeoutMS handshake timeout"
        | Error err -> fail (Mongo_error.to_string err))

let test_refused_port_returns_network_error () =
  let server, port = listen_loopback () in
  Unix.close server;
  let config =
    {
      (Mongo_config.default ~host:"127.0.0.1" ~port ~database:"admin" ())
      with
      connect_timeout_ms = 500;
      server_selection_timeout_ms = 50;
    }
  in
  match Mongo_connection.connect config with
  | Error (Mongo_error.Network _) -> ()
  | Error (Mongo_error.Timeout _) -> ()
  | Error err -> fail (Mongo_error.to_string err)
  | Ok conn ->
      Mongo_connection.close conn;
      fail "expected connection failure"

let serve_one_hello ?(delay_seconds = 0.0) server_socket body =
  let client, _ = Unix.accept server_socket in
  Fun.protect
    ~finally:(fun () -> Unix.close client)
    (fun () ->
      let request = Mongo_wire.read_message client in
      if delay_seconds > 0.0 then Unix.sleepf delay_seconds;
      let header = MongoHeader.decode_header (String.sub request 0 16) in
      match
        Mongo_wire.write_all client
          (encode_reply ~request_id:99l
             ~response_to:(MongoHeader.get_request_id header)
             body)
      with
      | Ok () -> ()
      | Error err -> fail (Mongo_error.to_string err))

let test_connect_records_handshake_rtt () =
  let server, port = listen_loopback () in
  let child = Unix.fork () in
  if child = 0 then (
    serve_one_hello ~delay_seconds:0.05 server (hello_doc ());
    Unix.close server;
    exit 0)
  else
    Fun.protect
      ~finally:(fun () ->
        Unix.close server;
        (try Unix.kill child Sys.sigkill with _ -> ());
        ignore (Unix.waitpid [] child))
      (fun () ->
        let config =
          {
            (Mongo_config.default ~host:"127.0.0.1" ~port ~database:"admin" ())
            with
            connect_timeout_ms = 1_000;
            socket_timeout_ms = Some 1_000;
            server_selection_timeout_ms = 1_000;
          }
        in
        match Mongo_connection.connect config with
        | Error err -> fail (Mongo_error.to_string err)
        | Ok conn ->
            let rtt =
              match (Mongo_connection.server_info conn).round_trip_time_ms with
              | Some rtt -> rtt
              | None -> fail "expected handshake RTT"
            in
            check int "handshake RTT sample count" 1
              (List.length
                 (Mongo_connection.server_info conn).round_trip_time_samples_ms);
            check bool "handshake RTT includes hello wait" true (rtt >= 25.0);
            Mongo_connection.close conn)

let test_heartbeat_records_rtt () =
  let client_fd, server_fd =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let conn =
    {
      Mongo_connection.host = "fake";
      port = 0;
      fd = client_fd;
      transport = Mongo_transport.plain client_fd;
      config = Mongo_config.default ~database:"admin" ();
      server = dummy_server ();
      authenticated = false;
    }
  in
  let result = ref None in
  let thread =
    Thread.create
      (fun () -> result := Some (Mongo_connection.heartbeat conn))
      ()
  in
  let request = Mongo_wire.read_message server_fd in
  Unix.sleepf 0.05;
  let header = MongoHeader.decode_header (String.sub request 0 16) in
  let command = Mongo_wire.response_message_doc request in
  check bool "heartbeat command" true (Bson.has_element "hello" command);
  (match
     Mongo_wire.write_all server_fd
       (encode_reply ~request_id:42l
          ~response_to:(MongoHeader.get_request_id header)
          (hello_doc ()))
   with
  | Ok () -> ()
  | Error err -> fail (Mongo_error.to_string err));
  Thread.join thread;
  Unix.close server_fd;
  Mongo_connection.close conn;
  match !result with
  | Some (Ok description) -> (
      check int "heartbeat RTT sample count" 1
        (List.length (Mongo_connection.server_info conn).round_trip_time_samples_ms);
      match description.Mongo_server_description.round_trip_time_ms with
      | Some rtt ->
          check bool "heartbeat RTT includes hello wait" true (rtt >= 25.0)
      | None -> fail "expected heartbeat RTT")
  | Some (Error err) -> fail (Mongo_error.to_string err)
  | None -> fail "missing heartbeat result"

let test_heartbeat_ignores_timeout_ms () =
  let client_fd, server_fd =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let conn =
    {
      Mongo_connection.host = "fake";
      port = 0;
      fd = client_fd;
      transport = Mongo_transport.plain client_fd;
      config =
        {
          (Mongo_config.default ~database:"admin" ()) with
          timeout_ms = Some 10;
          socket_timeout_ms = Some 200;
        };
      server = dummy_server ();
      authenticated = false;
    }
  in
  let result = ref None in
  let thread =
    Thread.create
      (fun () -> result := Some (Mongo_connection.heartbeat conn))
      ()
  in
  let request = Mongo_wire.read_message server_fd in
  Unix.sleepf 0.05;
  let header = MongoHeader.decode_header (String.sub request 0 16) in
  (match
     Mongo_wire.write_all server_fd
       (encode_reply ~request_id:42l
          ~response_to:(MongoHeader.get_request_id header)
          (hello_doc ()))
   with
  | Ok () -> ()
  | Error err -> fail (Mongo_error.to_string err));
  Thread.join thread;
  Unix.close server_fd;
  Mongo_connection.close conn;
  match !result with
  | Some (Ok _) -> ()
  | Some (Error err) -> fail (Mongo_error.to_string err)
  | None -> fail "missing heartbeat result"

let test_monitor_once_success_updates_description () =
  let client_fd, server_fd =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let conn =
    {
      Mongo_connection.host = "fake";
      port = 27017;
      fd = client_fd;
      transport = Mongo_transport.plain client_fd;
      config = Mongo_config.default ~database:"admin" ();
      server = dummy_server ();
      authenticated = false;
    }
  in
  let updated = ref None in
  let thread =
    Thread.create
      (fun () -> Mongo_connection.monitor_once conn (fun server -> updated := Some server))
      ()
  in
  let request = Mongo_wire.read_message server_fd in
  let header = MongoHeader.decode_header (String.sub request 0 16) in
  (match
     Mongo_wire.write_all server_fd
       (encode_reply ~request_id:42l
          ~response_to:(MongoHeader.get_request_id header)
          (hello_doc ~set_name:(Some "rs0") ()))
   with
  | Ok () -> ()
  | Error err -> fail (Mongo_error.to_string err));
  Thread.join thread;
  Unix.close server_fd;
  Mongo_connection.close conn;
  match !updated with
  | Some server ->
      check bool "monitor once primary" true
        (server.Mongo_server_description.server_type
        = Mongo_server_description.RSPrimary);
      check (option string) "no monitor error" None server.error
  | None -> fail "missing monitor update"

let test_monitor_once_failure_updates_unknown () =
  let client_fd, server_fd =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let conn =
    {
      Mongo_connection.host = "fake";
      port = 27017;
      fd = client_fd;
      transport = Mongo_transport.plain client_fd;
      config =
        {
          (Mongo_config.default ~database:"admin" ()) with
          socket_timeout_ms = Some 10;
        };
      server = dummy_server ();
      authenticated = false;
    }
  in
  let updated = ref None in
  Fun.protect
    ~finally:(fun () ->
      Unix.close server_fd;
      Mongo_connection.close conn)
    (fun () ->
      Mongo_connection.monitor_once conn (fun server -> updated := Some server);
      match !updated with
      | Some server ->
          check bool "monitor failure unknown" true
            (server.Mongo_server_description.server_type
            = Mongo_server_description.Unknown);
          check bool "monitor error set" true (server.error <> None)
      | None -> fail "missing monitor failure update")

let test_connect_tries_second_seed () =
  let closed, closed_port = listen_loopback () in
  Unix.close closed;
  let server, port = listen_loopback () in
  let child = Unix.fork () in
  if child = 0 then (
    serve_one_hello server (hello_doc ());
    Unix.close server;
    exit 0)
  else
    Fun.protect
      ~finally:(fun () ->
        Unix.close server;
        (try Unix.kill child Sys.sigkill with _ -> ());
        ignore (Unix.waitpid [] child))
      (fun () ->
        let config =
          {
            (Mongo_config.default ~host:"127.0.0.1" ~port:closed_port
               ~database:"admin" ())
            with
            hosts = [ ("127.0.0.1", closed_port); ("127.0.0.1", port) ];
            connect_timeout_ms = 500;
            socket_timeout_ms = Some 1_000;
          }
        in
        match Mongo_connection.connect config with
        | Ok conn ->
            check int "connected second seed" port conn.port;
            Mongo_connection.close conn
        | Error err -> fail (Mongo_error.to_string err))

let test_connect_discovers_primary_from_secondary_seed () =
  let secondary_server, secondary_port = listen_loopback () in
  let primary_server, primary_port = listen_loopback () in
  let secondary_child = Unix.fork () in
  if secondary_child = 0 then (
    serve_one_hello secondary_server
      (hello_doc ~set_name:(Some "rs0")
         ~hosts:[ Printf.sprintf "127.0.0.1:%d" primary_port ]
         ~is_writable_primary:false ~secondary:true ());
    Unix.close secondary_server;
    Unix.close primary_server;
    exit 0)
  else
    let primary_child = Unix.fork () in
    if primary_child = 0 then (
      serve_one_hello primary_server
        (hello_doc ~set_name:(Some "rs0") ());
      Unix.close secondary_server;
      Unix.close primary_server;
      exit 0)
    else
      Fun.protect
        ~finally:(fun () ->
          Unix.close secondary_server;
          Unix.close primary_server;
          (try Unix.kill secondary_child Sys.sigkill with _ -> ());
          (try Unix.kill primary_child Sys.sigkill with _ -> ());
          ignore (Unix.waitpid [] secondary_child);
          ignore (Unix.waitpid [] primary_child))
        (fun () ->
          let config =
            {
              (Mongo_config.default ~host:"127.0.0.1" ~port:secondary_port
                 ~database:"admin" ())
              with
              replica_set = Some "rs0";
              hosts = [ ("127.0.0.1", secondary_port) ];
              connect_timeout_ms = 500;
              socket_timeout_ms = Some 1_000;
              server_selection_timeout_ms = 1_000;
            }
          in
          match Mongo_connection.connect config with
          | Ok conn ->
              check int "connected discovered primary" primary_port conn.port;
              Mongo_connection.close conn
          | Error err -> fail (Mongo_error.to_string err))

let test_replica_set_name_mismatch_fails () =
  let server, port = listen_loopback () in
  let child = Unix.fork () in
  if child = 0 then (
    serve_one_hello server (hello_doc ~set_name:(Some "actual") ());
    Unix.close server;
    exit 0)
  else
    Fun.protect
      ~finally:(fun () ->
        Unix.close server;
        (try Unix.kill child Sys.sigkill with _ -> ());
        ignore (Unix.waitpid [] child))
      (fun () ->
        let config =
          {
            (Mongo_config.default ~host:"127.0.0.1" ~port ~database:"admin" ())
            with
            replica_set = Some "expected";
            socket_timeout_ms = Some 1_000;
            server_selection_timeout_ms = 50;
          }
        in
        match Mongo_connection.connect config with
        | Error (Mongo_error.Server_selection _) -> ()
        | Error err -> fail (Mongo_error.to_string err)
        | Ok conn ->
            Mongo_connection.close conn;
            fail "expected replica set mismatch")

let test_hello_hosts_parse_ports () =
  let server =
    Mongo_connection.parse_server_info
      (hello_doc ~set_name:(Some "rs0")
         ~hosts:[ "db1.example:27018"; "[::1]:27019"; "db2.example" ]
         ())
  in
  check (list (pair string int)) "hosts"
    [
      ("db1.example", 27018);
      ("::1", 27019);
      ("db2.example", Mongo_config.default_port);
    ]
    server.hosts

let test_hello_parses_tags_and_last_write () =
  let last_write =
    Mongo_command.document
      [ ("lastWriteDate", Bson.create_utc 1_700_000_123_000L) ]
  in
  let hello =
    hello_doc ~set_name:(Some "rs0") ()
    |> Bson.add_element "tags"
         (Bson.create_doc_element
            (Mongo_command.document
               [ ("dc", Bson.create_string "ny"); ("rack", Bson.create_string "1") ]))
    |> Bson.add_element "lastWrite" (Bson.create_doc_element last_write)
  in
  let server = Mongo_connection.parse_server_info hello in
  check (list (pair string string)) "tags"
    [ ("dc", "ny"); ("rack", "1") ]
    server.tags;
  check (option (float 0.001)) "lastWriteDate"
    (Some 1_700_000_123.0) server.last_write_date

let () =
  run "mongo_connection"
    [
      ( "sessions",
        [
          test_case "adds implicit lsid" `Quick
            test_run_command_adds_implicit_lsid;
          test_case "adds txnNumber for retryable write" `Quick
            test_run_command_adds_txn_number_for_retryable_write;
          test_case "skips txnNumber for nonretryable write" `Quick
            test_run_command_skips_txn_number_for_nonretryable_write;
          test_case "skips txnNumber on standalone" `Quick
            test_run_command_skips_txn_number_on_standalone;
          test_case "skips implicit sessions without server support" `Quick
            test_run_command_skips_implicit_sessions_without_server_support;
          test_case "applies config concerns" `Quick
            test_run_command_applies_config_concerns;
          test_case "applies timeoutMS maxTimeMS" `Quick
            test_run_command_applies_timeout_ms_as_max_time_ms;
          test_case "timeoutMS maxTimeMS subtracts min RTT" `Quick
            test_run_command_subtracts_min_rtt_from_max_time_ms;
          test_case "timeoutMS expires before min RTT" `Quick
            test_run_command_times_out_when_min_rtt_exhausts_budget;
          test_case "timeoutMS zero is infinite" `Quick
            test_run_command_timeout_ms_zero_is_infinite;
          test_case "skips getMore maxTimeMS" `Quick
            test_run_command_does_not_add_max_time_ms_to_get_more;
        ] );
      ( "monitoring",
        [
          test_case "command success events" `Quick
            test_command_monitoring_success;
          test_case "command failure events" `Quick
            test_command_monitoring_failure;
          test_case "sensitive command redaction" `Quick
            test_command_monitoring_redacts_sensitive_commands;
        ] );
      ( "timeouts",
        [
          test_case "handshake read timeout" `Quick test_handshake_read_timeout;
          test_case "timeoutMS limits handshake read" `Quick
            test_timeout_ms_limits_handshake_read;
          test_case "records handshake RTT" `Quick
            test_connect_records_handshake_rtt;
          test_case "records heartbeat RTT" `Quick
            test_heartbeat_records_rtt;
          test_case "heartbeat ignores timeoutMS" `Quick
            test_heartbeat_ignores_timeout_ms;
          test_case "monitor once success" `Quick
            test_monitor_once_success_updates_description;
          test_case "monitor once failure unknown" `Quick
            test_monitor_once_failure_updates_unknown;
          test_case "refused port" `Quick test_refused_port_returns_network_error;
          test_case "tries second seed" `Quick test_connect_tries_second_seed;
          test_case "discovers primary from secondary seed" `Quick
            test_connect_discovers_primary_from_secondary_seed;
          test_case "replica set mismatch" `Quick
            test_replica_set_name_mismatch_fails;
          test_case "hello hosts parse ports" `Quick
            test_hello_hosts_parse_ports;
          test_case "hello tags and lastWrite" `Quick
            test_hello_parses_tags_and_last_write;
        ] );
    ]
