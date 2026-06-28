open Alcotest

let dummy_server : Mongo_connection.server_info =
  {
    max_wire_version = Mongo_config.max_supported_wire_version;
    min_wire_version = Mongo_config.min_supported_wire_version;
    is_writable_primary = true;
    secondary = false;
    set_name = None;
    hosts = [];
    passives = [];
    arbiters = [];
    is_mongos = false;
    logical_session_timeout_minutes = None;
    sasl_supported_mechs = None;
    service_id = None;
    tags = [];
    last_write_date = None;
    round_trip_time_ms = None;
    round_trip_time_samples_ms = [];
  }

let retryable_server : Mongo_connection.server_info =
  {
    dummy_server with
    set_name = Some "rs0";
    logical_session_timeout_minutes = Some 30;
  }

let ok_doc =
  Bson.add_element "ok" (Bson.create_double 1.0) Bson.empty

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

let read_command fd =
  let request = Mongo_wire.read_message fd in
  let header = MongoHeader.decode_header (String.sub request 0 16) in
  (MongoHeader.get_request_id header, Mongo_wire.response_message_doc request)

let write_response fd ~response_to body =
  match Mongo_wire.write_all fd (encode_reply ~request_id:99l ~response_to body) with
  | Ok () -> ()
  | Error err -> fail (Mongo_error.to_string err)

let get_lsid_id command =
  let lsid = Bson.get_doc_element (Bson.get_element "lsid" command) in
  Bson.get_uuid_binary (Bson.get_element "id" lsid)

let base_config =
  {
    (Mongo_config.default ~host:"127.0.0.1" ~database:"pool_test" ())
    with
    max_pool_size = 2;
    wait_queue_timeout_ms = 20;
  }

let make_connector () =
  let created = ref 0 in
  let peers = ref [] in
  let connect config =
    incr created;
    let fd, peer = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    peers := peer :: !peers;
    Ok
      {
        Mongo_connection.host = "fake";
        port = 0;
        fd;
        transport = Mongo_transport.plain fd;
        config;
        server = dummy_server;
        authenticated = false;
      }
  in
  let cleanup () =
    List.iter
      (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ())
      !peers
  in
  (connect, created, cleanup)

let event_name = function
  | Mongo_pool.Pool_created -> "pool_created"
  | Pool_closed -> "pool_closed"
  | Pool_cleared -> "pool_cleared"
  | Connection_created -> "connection_created"
  | Connection_ready -> "connection_ready"
  | Connection_closed reason -> "connection_closed:" ^ reason
  | Checkout_started -> "checkout_started"
  | Checkout_succeeded -> "checkout_succeeded"
  | Checkout_failed reason -> "checkout_failed:" ^ reason
  | Checkin_started -> "checkin_started"
  | Checkin_succeeded -> "checkin_succeeded"

let event_recorder () =
  let events = ref [] in
  let handler event = events := event_name event :: !events in
  let names () = List.rev !events in
  (handler, names)

let test_respects_max_pool_size () =
  let connect, created, cleanup = make_connector () in
  Fun.protect ~finally:cleanup (fun () ->
      let pool = Mongo_pool.create ~connect base_config in
      let first =
        match Mongo_pool.checkout pool with
        | Ok conn -> conn
        | Error err -> fail (Mongo_error.to_string err)
      in
      let second =
        match Mongo_pool.checkout pool with
        | Ok conn -> conn
        | Error err -> fail (Mongo_error.to_string err)
      in
      (match Mongo_pool.checkout pool with
      | Error (Mongo_error.Timeout _) -> ()
      | Error err -> fail (Mongo_error.to_string err)
      | Ok conn ->
          Mongo_connection.close conn;
          fail "expected wait queue timeout");
      check int "created connections" 2 !created;
      check int "total while checked out" 2 (Mongo_pool.total_connections pool);
      Mongo_pool.checkin pool first;
      Mongo_pool.checkin pool second;
      check int "idle after checkin" 2 (Mongo_pool.idle_connections pool);
      Mongo_pool.close pool)

let test_reuses_idle_connection () =
  let connect, created, cleanup = make_connector () in
  Fun.protect ~finally:cleanup (fun () ->
      let pool = Mongo_pool.create ~connect base_config in
      let first =
        match Mongo_pool.checkout pool with
        | Ok conn -> conn
        | Error err -> fail (Mongo_error.to_string err)
      in
      let first_fd = Mongo_connection.file_descr first in
      Mongo_pool.checkin pool first;
      let second =
        match Mongo_pool.checkout pool with
        | Ok conn -> conn
        | Error err -> fail (Mongo_error.to_string err)
      in
      check int "created connections" 1 !created;
      check bool "same fd reused" true
        (Mongo_connection.file_descr second = first_fd);
      Mongo_pool.checkin pool second;
      Mongo_pool.close pool)

let test_pool_events_cover_lifecycle () =
  let connect, _created, cleanup = make_connector () in
  let handler, names = event_recorder () in
  Fun.protect ~finally:cleanup (fun () ->
      let config = { base_config with min_pool_size = 1 } in
      let pool = Mongo_pool.create ~event_handler:handler ~connect config in
      let conn =
        match Mongo_pool.checkout pool with
        | Ok conn -> conn
        | Error err -> fail (Mongo_error.to_string err)
      in
      Mongo_pool.checkin pool conn;
      Mongo_pool.clear pool;
      Mongo_pool.close pool;
      let names = names () in
      check bool "pool created event" true (List.mem "pool_created" names);
      check bool "connection created event" true
        (List.mem "connection_created" names);
      check bool "connection ready event" true (List.mem "connection_ready" names);
      check bool "checkout started event" true (List.mem "checkout_started" names);
      check bool "checkout succeeded event" true
        (List.mem "checkout_succeeded" names);
      check bool "checkin succeeded event" true
        (List.mem "checkin_succeeded" names);
      check bool "pool cleared event" true (List.mem "pool_cleared" names);
      check bool "pool closed event" true (List.mem "pool_closed" names))

let test_pool_events_report_checkout_failure () =
  let connect, _created, cleanup = make_connector () in
  let handler, names = event_recorder () in
  Fun.protect ~finally:cleanup (fun () ->
      let pool = Mongo_pool.create ~event_handler:handler ~connect base_config in
      let first =
        match Mongo_pool.checkout pool with
        | Ok conn -> conn
        | Error err -> fail (Mongo_error.to_string err)
      in
      let second =
        match Mongo_pool.checkout pool with
        | Ok conn -> conn
        | Error err -> fail (Mongo_error.to_string err)
      in
      (match Mongo_pool.checkout pool with
      | Error (Mongo_error.Timeout _) -> ()
      | Error err -> fail (Mongo_error.to_string err)
      | Ok conn ->
          Mongo_connection.close conn;
          fail "expected wait queue timeout");
      let names = names () in
      check bool "checkout failed event" true
        (List.mem "checkout_failed:pool wait queue timed out" names);
      Mongo_pool.checkin pool first;
      Mongo_pool.checkin pool second;
      Mongo_pool.close pool)

let test_timeout_ms_limits_checkout_wait () =
  let connect, _created, cleanup = make_connector () in
  Fun.protect ~finally:cleanup (fun () ->
      let config =
        { base_config with max_pool_size = 1; wait_queue_timeout_ms = 200; timeout_ms = Some 10 }
      in
      let pool = Mongo_pool.create ~connect config in
      let first =
        match Mongo_pool.checkout pool with
        | Ok conn -> conn
        | Error err -> fail (Mongo_error.to_string err)
      in
      let started_at = Unix.gettimeofday () in
      (match Mongo_pool.checkout pool with
      | Error (Mongo_error.Timeout _) -> ()
      | Error err -> fail (Mongo_error.to_string err)
      | Ok conn ->
          Mongo_connection.close conn;
          fail "expected timeoutMS checkout timeout");
      let elapsed_ms = (Unix.gettimeofday () -. started_at) *. 1000.0 in
      check bool "checkout used timeoutMS budget" true (elapsed_ms < 100.0);
      Mongo_pool.checkin pool first;
      Mongo_pool.close pool)

let test_timeout_ms_zero_uses_wait_queue_timeout () =
  let connect, _created, cleanup = make_connector () in
  Fun.protect ~finally:cleanup (fun () ->
      let config =
        { base_config with max_pool_size = 1; wait_queue_timeout_ms = 20; timeout_ms = Some 0 }
      in
      let pool = Mongo_pool.create ~connect config in
      let first =
        match Mongo_pool.checkout pool with
        | Ok conn -> conn
        | Error err -> fail (Mongo_error.to_string err)
      in
      let started_at = Unix.gettimeofday () in
      (match Mongo_pool.checkout pool with
      | Error (Mongo_error.Timeout _) -> ()
      | Error err -> fail (Mongo_error.to_string err)
      | Ok conn ->
          Mongo_connection.close conn;
          fail "expected wait queue timeout");
      let elapsed_ms = (Unix.gettimeofday () -. started_at) *. 1000.0 in
      check bool "checkout used wait queue timeout" true (elapsed_ms >= 10.0);
      Mongo_pool.checkin pool first;
      Mongo_pool.close pool)

let test_create_warms_min_pool_size () =
  let connect, created, cleanup = make_connector () in
  Fun.protect ~finally:cleanup (fun () ->
      let config = { base_config with min_pool_size = 2 } in
      let pool = Mongo_pool.create ~connect config in
      check int "created min pool" 2 !created;
      check int "total min pool" 2 (Mongo_pool.total_connections pool);
      check int "idle min pool" 2 (Mongo_pool.idle_connections pool);
      Mongo_pool.close pool)

let test_min_pool_size_is_capped_by_max_pool_size () =
  let connect, created, cleanup = make_connector () in
  Fun.protect ~finally:cleanup (fun () ->
      let config = { base_config with min_pool_size = 3; max_pool_size = 2 } in
      let pool = Mongo_pool.create ~connect config in
      check int "created capped min pool" 2 !created;
      check int "total capped min pool" 2 (Mongo_pool.total_connections pool);
      check int "idle capped min pool" 2 (Mongo_pool.idle_connections pool);
      Mongo_pool.close pool)

let test_clear_discards_checked_out_generation () =
  let connect, _created, cleanup = make_connector () in
  Fun.protect ~finally:cleanup (fun () ->
      let pool = Mongo_pool.create ~connect base_config in
      let conn =
        match Mongo_pool.checkout pool with
        | Ok conn -> conn
        | Error err -> fail (Mongo_error.to_string err)
      in
      Mongo_pool.clear pool;
      Mongo_pool.checkin pool conn;
      check int "total after stale checkin" 0 (Mongo_pool.total_connections pool);
      check int "idle after stale checkin" 0 (Mongo_pool.idle_connections pool);
      Mongo_pool.close pool)

let test_ensure_min_pool_size_after_clear () =
  let connect, created, cleanup = make_connector () in
  Fun.protect ~finally:cleanup (fun () ->
      let config = { base_config with min_pool_size = 2 } in
      let pool = Mongo_pool.create ~connect config in
      check int "created initial min pool" 2 !created;
      Mongo_pool.clear pool;
      check int "total after clear" 0 (Mongo_pool.total_connections pool);
      (match Mongo_pool.ensure_min_pool_size pool with
      | Ok () -> ()
      | Error err -> fail (Mongo_error.to_string err));
      check int "created replenished min pool" 4 !created;
      check int "total replenished min pool" 2 (Mongo_pool.total_connections pool);
      check int "idle replenished min pool" 2 (Mongo_pool.idle_connections pool);
      Mongo_pool.close pool)

let server_description ?(server_type = Mongo_server_description.RSPrimary)
    ?error () : Mongo_server_description.t =
  {
    address = ("fake", 0);
    server_type;
    round_trip_time_ms = None;
    last_update = Unix.gettimeofday ();
    last_write_date = None;
    tags = [];
    max_wire_version = Mongo_config.max_supported_wire_version;
    set_name = None;
    primary = None;
    error;
  }

let test_unknown_server_description_clears_pool () =
  let connect, _created, cleanup = make_connector () in
  let handler, names = event_recorder () in
  Fun.protect ~finally:cleanup (fun () ->
      let pool = Mongo_pool.create ~event_handler:handler ~connect base_config in
      let conn =
        match Mongo_pool.checkout pool with
        | Ok conn -> conn
        | Error err -> fail (Mongo_error.to_string err)
      in
      Mongo_pool.checkin pool conn;
      check int "idle before unknown" 1 (Mongo_pool.idle_connections pool);
      Mongo_pool.update_server_description pool
        (server_description ~server_type:Mongo_server_description.Unknown
           ~error:"monitor timeout" ());
      check int "idle after unknown" 0 (Mongo_pool.idle_connections pool);
      check int "total after unknown" 0 (Mongo_pool.total_connections pool);
      let names = names () in
      check bool "pool clear event" true (List.mem "pool_cleared" names);
      check bool "idle connection closed" true
        (List.mem "connection_closed:pool cleared" names);
      Mongo_pool.close pool)

let test_healthy_server_description_keeps_pool () =
  let connect, _created, cleanup = make_connector () in
  Fun.protect ~finally:cleanup (fun () ->
      let pool = Mongo_pool.create ~connect base_config in
      let conn =
        match Mongo_pool.checkout pool with
        | Ok conn -> conn
        | Error err -> fail (Mongo_error.to_string err)
      in
      Mongo_pool.checkin pool conn;
      Mongo_pool.update_server_description pool (server_description ());
      check int "idle after healthy update" 1 (Mongo_pool.idle_connections pool);
      check int "total after healthy update" 1 (Mongo_pool.total_connections pool);
      Mongo_pool.close pool)

let test_with_connection_checkin_on_error_result () =
  let connect, _created, cleanup = make_connector () in
  Fun.protect ~finally:cleanup (fun () ->
      let pool = Mongo_pool.create ~connect base_config in
      (match Mongo_pool.with_connection pool (fun _ -> Error (Mongo_error.Protocol "boom")) with
      | Error (Mongo_error.Protocol "boom") -> ()
      | Error err -> fail (Mongo_error.to_string err)
      | Ok _ -> fail "expected error");
      check int "idle after error result" 1 (Mongo_pool.idle_connections pool);
      Mongo_pool.close pool)

let test_network_error_clears_generation () =
  let connect, created, cleanup = make_connector () in
  Fun.protect ~finally:cleanup (fun () ->
      let pool = Mongo_pool.create ~connect base_config in
      let first =
        match Mongo_pool.checkout pool with
        | Ok conn -> conn
        | Error err -> fail (Mongo_error.to_string err)
      in
      Mongo_pool.checkin pool first;
      check int "idle before network error" 1 (Mongo_pool.idle_connections pool);
      (match
         Mongo_pool.with_connection pool (fun _ ->
             Error (Mongo_error.Network "socket reset"))
       with
      | Error (Mongo_error.Network "socket reset") -> ()
      | Error err -> fail (Mongo_error.to_string err)
      | Ok _ -> fail "expected network error");
      check int "idle after generation clear" 0 (Mongo_pool.idle_connections pool);
      check int "total after stale checkin" 0 (Mongo_pool.total_connections pool);
      let second =
        match Mongo_pool.checkout pool with
        | Ok conn -> conn
        | Error err -> fail (Mongo_error.to_string err)
      in
      check int "new connection after clear" 2 !created;
      Mongo_pool.checkin pool second;
      Mongo_pool.close pool)

let test_max_idle_time_prunes_idle_connection () =
  let connect, created, cleanup = make_connector () in
  Fun.protect ~finally:cleanup (fun () ->
      let config = { base_config with max_idle_time_ms = Some 1 } in
      let pool = Mongo_pool.create ~connect config in
      let first =
        match Mongo_pool.checkout pool with
        | Ok conn -> conn
        | Error err -> fail (Mongo_error.to_string err)
      in
      Mongo_pool.checkin pool first;
      check int "idle before prune" 1 (Mongo_pool.idle_connections pool);
      Unix.sleepf 0.01;
      let second =
        match Mongo_pool.checkout pool with
        | Ok conn -> conn
        | Error err -> fail (Mongo_error.to_string err)
      in
      check int "expired connection replaced" 2 !created;
      Mongo_pool.checkin pool second;
      Mongo_pool.close pool)

let test_max_idle_zero_is_unlimited () =
  let connect, created, cleanup = make_connector () in
  Fun.protect ~finally:cleanup (fun () ->
      let config = { base_config with max_idle_time_ms = Some 0 } in
      let pool = Mongo_pool.create ~connect config in
      let first =
        match Mongo_pool.checkout pool with
        | Ok conn -> conn
        | Error err -> fail (Mongo_error.to_string err)
      in
      let first_fd = Mongo_connection.file_descr first in
      Mongo_pool.checkin pool first;
      Unix.sleepf 0.01;
      let second =
        match Mongo_pool.checkout pool with
        | Ok conn -> conn
        | Error err -> fail (Mongo_error.to_string err)
      in
      check int "no replacement" 1 !created;
      check bool "same fd reused" true
        (Mongo_connection.file_descr second = first_fd);
      Mongo_pool.checkin pool second;
      Mongo_pool.close pool)

type scripted_reply =
  | Close_after_read of Bson.t option ref
  | Sleep_then_close_after_read of float * Bson.t option ref
  | Reply_ok of Bson.t option ref

let make_scripted_connector scripts =
  let created = ref 0 in
  let peers = ref [] in
  let threads = ref [] in
  let remaining = ref scripts in
  let connect config =
    incr created;
    let fd, peer = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    peers := peer :: !peers;
    let script =
      match !remaining with
      | next :: rest ->
          remaining := rest;
          next
      | [] -> fail "missing scripted connection behavior"
    in
    let thread =
      Thread.create
        (fun () ->
          match script with
          | Close_after_read command_ref ->
              let _request_id, command = read_command peer in
              command_ref := Some command;
              Unix.close peer
          | Sleep_then_close_after_read (seconds, command_ref) ->
              let _request_id, command = read_command peer in
              command_ref := Some command;
              Unix.sleepf seconds;
              Unix.close peer
          | Reply_ok command_ref ->
              let request_id, command = read_command peer in
              command_ref := Some command;
              write_response peer ~response_to:request_id ok_doc)
        ()
    in
    threads := thread :: !threads;
    Ok
      {
        Mongo_connection.host = "fake";
        port = 0;
        fd;
        transport = Mongo_transport.plain fd;
        config;
        server = retryable_server;
        authenticated = false;
      }
  in
  let cleanup () =
    List.iter
      (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ())
      !peers;
    List.iter Thread.join !threads
  in
  (connect, created, cleanup)

let test_retryable_write_retries_once_with_same_session () =
  let first_command = ref None in
  let second_command = ref None in
  let connect, created, cleanup =
    make_scripted_connector
      [ Close_after_read first_command; Reply_ok second_command ]
  in
  Fun.protect ~finally:cleanup (fun () ->
      let pool = Mongo_pool.create ~connect base_config in
      let fields =
        [
          ("insert", Bson.create_string "posts");
          ("documents", Bson.create_doc_element_list [ Bson.empty ]);
          ("ordered", Bson.create_boolean true);
        ]
      in
      (match Mongo_pool.run_command pool "test" fields with
      | Ok _ -> ()
      | Error err -> fail (Mongo_error.to_string err));
      check int "retry created second connection" 2 !created;
      let first =
        match !first_command with Some command -> command | None -> fail "missing first command"
      in
      let second =
        match !second_command with Some command -> command | None -> fail "missing second command"
      in
      check string "same lsid" (get_lsid_id first) (get_lsid_id second);
      check int64 "first txnNumber" 1L
        (Bson.get_int64 (Bson.get_element "txnNumber" first));
      check int64 "second txnNumber" 1L
        (Bson.get_int64 (Bson.get_element "txnNumber" second));
      Mongo_pool.close pool)

let test_retryable_write_reuses_remaining_timeout_ms () =
  let first_command = ref None in
  let second_command = ref None in
  let connect, _created, cleanup =
    make_scripted_connector
      [
        Sleep_then_close_after_read (0.05, first_command);
        Reply_ok second_command;
      ]
  in
  Fun.protect ~finally:cleanup (fun () ->
      let config = { base_config with timeout_ms = Some 200 } in
      let pool = Mongo_pool.create ~connect config in
      let fields =
        [
          ("insert", Bson.create_string "posts");
          ("documents", Bson.create_doc_element_list [ Bson.empty ]);
          ("ordered", Bson.create_boolean true);
        ]
      in
      (match Mongo_pool.run_command pool "test" fields with
      | Ok _ -> ()
      | Error err -> fail (Mongo_error.to_string err));
      let first =
        match !first_command with Some command -> command | None -> fail "missing first command"
      in
      let second =
        match !second_command with Some command -> command | None -> fail "missing second command"
      in
      check int64 "first maxTimeMS" 200L
        (Bson.get_int64 (Bson.get_element "maxTimeMS" first));
      let second_max_time_ms =
        Bson.get_int64 (Bson.get_element "maxTimeMS" second)
      in
      check bool "second maxTimeMS uses remaining budget" true
        (second_max_time_ms < 200L && second_max_time_ms > 0L);
      check string "same lsid" (get_lsid_id first) (get_lsid_id second);
      check int64 "same txnNumber" 1L
        (Bson.get_int64 (Bson.get_element "txnNumber" second));
      Mongo_pool.close pool)

let () =
  run "mongo_pool"
    [
      ( "pool",
        [
          test_case "respects max pool size" `Quick test_respects_max_pool_size;
          test_case "reuses idle connection" `Quick test_reuses_idle_connection;
          test_case "events cover lifecycle" `Quick
            test_pool_events_cover_lifecycle;
          test_case "events report checkout failure" `Quick
            test_pool_events_report_checkout_failure;
          test_case "timeoutMS limits checkout wait" `Quick
            test_timeout_ms_limits_checkout_wait;
          test_case "timeoutMS zero uses wait queue timeout" `Quick
            test_timeout_ms_zero_uses_wait_queue_timeout;
          test_case "create warms min pool size" `Quick
            test_create_warms_min_pool_size;
          test_case "min pool capped by max pool size" `Quick
            test_min_pool_size_is_capped_by_max_pool_size;
          test_case "clear discards checked out generation" `Quick
            test_clear_discards_checked_out_generation;
          test_case "ensure min pool size after clear" `Quick
            test_ensure_min_pool_size_after_clear;
          test_case "unknown server description clears pool" `Quick
            test_unknown_server_description_clears_pool;
          test_case "healthy server description keeps pool" `Quick
            test_healthy_server_description_keeps_pool;
          test_case "with_connection checkin on error result" `Quick
            test_with_connection_checkin_on_error_result;
          test_case "network error clears generation" `Quick
            test_network_error_clears_generation;
          test_case "max idle time prunes idle connection" `Quick
            test_max_idle_time_prunes_idle_connection;
          test_case "max idle zero is unlimited" `Quick
            test_max_idle_zero_is_unlimited;
          test_case "retryable write retries once with same session" `Quick
            test_retryable_write_retries_once_with_same_session;
          test_case "retryable write uses remaining timeoutMS" `Quick
            test_retryable_write_reuses_remaining_timeout_ms;
        ] );
    ]
