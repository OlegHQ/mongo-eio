open Alcotest

let doc fields = Mongo_command.document fields

let cursor_response ~id ~ns batch_name docs =
  let cursor =
    doc
      [
        ("id", Bson.create_int64 id);
        ("ns", Bson.create_string ns);
        (batch_name, Bson.create_doc_element_list docs);
      ]
  in
  doc
    [
      ("ok", Bson.create_double 1.0);
      ("cursor", Bson.create_doc_element cursor);
    ]

let dummy_server : Mongo_connection.server_info =
  {
    max_wire_version = Mongo_config.max_supported_wire_version;
    min_wire_version = Mongo_config.min_supported_wire_version;
    is_writable_primary = true;
    secondary = false;
    set_name = Some "rs0";
    hosts = [];
    passives = [];
    arbiters = [];
    is_mongos = false;
    logical_session_timeout_minutes = Some 30;
    sasl_supported_mechs = None;
    service_id = None;
    tags = [];
    last_write_date = None;
    round_trip_time_ms = None;
    round_trip_time_samples_ms = [];
  }

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

let with_fake_connection ?config_update f =
  let client_fd, server_fd =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let config =
    Mongo_config.default ~database:"test" ()
    |> fun config ->
    match config_update with None -> config | Some f -> f config
  in
  let conn =
    {
      Mongo_connection.host = "fake";
      port = 0;
      fd = client_fd;
      transport = Mongo_transport.plain client_fd;
      config;
      server = dummy_server;
      authenticated = false;
    }
  in
  Fun.protect
    ~finally:(fun () ->
      Unix.close server_fd;
      Mongo_connection.close conn)
    (fun () -> f conn server_fd)

let test_find_get_more_and_kill_reuse_session () =
  with_fake_connection (fun conn server_fd ->
      let find_result = ref None in
      let find_thread =
        Thread.create
          (fun () ->
            find_result :=
              Some
                (Mongo_cursor.find conn ~db:"test" ~collection:"posts"
                   ~batch_size:1 ()))
          ()
      in
      let request_id, find_command = read_command server_fd in
      let find_lsid = get_lsid_id find_command in
      check bool "find has lsid" true (Bson.has_element "lsid" find_command);
      write_response server_fd ~response_to:request_id
        (cursor_response ~id:123L ~ns:"test.posts" "firstBatch"
           [ doc [ ("seq", Bson.create_int32 1l) ] ]);
      Thread.join find_thread;
      let cursor =
        match !find_result with
        | Some (Ok cursor) -> cursor
        | Some (Error err) -> fail (Mongo_error.to_string err)
        | None -> fail "missing find result"
      in
      check int64 "cursor id" 123L (Mongo_cursor.id cursor);
      check int "first batch" 1 (List.length (Mongo_cursor.batch cursor));

      let get_more_result = ref None in
      let get_more_thread =
        Thread.create
          (fun () ->
            get_more_result := Some (Mongo_cursor.get_more ~batch_size:1 cursor))
          ()
      in
      let request_id, get_more_command = read_command server_fd in
      check string "getMore collection" "posts"
        (Bson.get_string (Bson.get_element "collection" get_more_command));
      check string "getMore lsid" find_lsid (get_lsid_id get_more_command);
      write_response server_fd ~response_to:request_id
        (cursor_response ~id:0L ~ns:"test.posts" "nextBatch"
           [ doc [ ("seq", Bson.create_int32 2l) ] ]);
      Thread.join get_more_thread;
      (match !get_more_result with
      | Some (Ok batch) -> check int "next batch" 1 (List.length batch)
      | Some (Error err) -> fail (Mongo_error.to_string err)
      | None -> fail "missing getMore result");
      check bool "cursor exhausted" false (Mongo_cursor.alive cursor);

      let killed = Mongo_cursor.kill cursor in
      match killed with
      | Ok () -> ()
      | Error err -> fail (Mongo_error.to_string err))

let test_cursor_get_more_uses_remaining_timeout_ms () =
  with_fake_connection
    ~config_update:(fun config -> { config with timeout_ms = Some 200 })
    (fun conn server_fd ->
      let find_result = ref None in
      let find_thread =
        Thread.create
          (fun () ->
            find_result :=
              Some
                (Mongo_cursor.find conn ~db:"test" ~collection:"posts"
                   ~batch_size:1 ()))
          ()
      in
      let request_id, find_command = read_command server_fd in
      check int64 "find maxTimeMS" 200L
        (Bson.get_int64 (Bson.get_element "maxTimeMS" find_command));
      write_response server_fd ~response_to:request_id
        (cursor_response ~id:123L ~ns:"test.posts" "firstBatch"
           [ doc [ ("seq", Bson.create_int32 1l) ] ]);
      Thread.join find_thread;
      let cursor =
        match !find_result with
        | Some (Ok cursor) -> cursor
        | Some (Error err) -> fail (Mongo_error.to_string err)
        | None -> fail "missing find result"
      in

      Unix.sleepf 0.05;
      let get_more_result = ref None in
      let get_more_thread =
        Thread.create
          (fun () ->
            get_more_result := Some (Mongo_cursor.get_more ~batch_size:1 cursor))
          ()
      in
      let request_id, get_more_command = read_command server_fd in
      check bool "getMore omits maxTimeMS" false
        (Bson.has_element "maxTimeMS" get_more_command);
      write_response server_fd ~response_to:request_id
        (cursor_response ~id:0L ~ns:"test.posts" "nextBatch"
           [ doc [ ("seq", Bson.create_int32 2l) ] ]);
      Thread.join get_more_thread;
      match !get_more_result with
      | Some (Ok batch) -> check int "next batch" 1 (List.length batch)
      | Some (Error err) -> fail (Mongo_error.to_string err)
      | None -> fail "missing getMore result")

let test_cursor_get_more_fails_after_timeout_ms_expires () =
  with_fake_connection
    ~config_update:(fun config -> { config with timeout_ms = Some 20 })
    (fun conn server_fd ->
      let find_result = ref None in
      let find_thread =
        Thread.create
          (fun () ->
            find_result :=
              Some
                (Mongo_cursor.find conn ~db:"test" ~collection:"posts"
                   ~batch_size:1 ()))
          ()
      in
      let request_id, _find_command = read_command server_fd in
      write_response server_fd ~response_to:request_id
        (cursor_response ~id:123L ~ns:"test.posts" "firstBatch"
           [ doc [ ("seq", Bson.create_int32 1l) ] ]);
      Thread.join find_thread;
      let cursor =
        match !find_result with
        | Some (Ok cursor) -> cursor
        | Some (Error err) -> fail (Mongo_error.to_string err)
        | None -> fail "missing find result"
      in
      Unix.sleepf 0.05;
      (match Mongo_cursor.get_more ~batch_size:1 cursor with
      | Error (Mongo_error.Timeout _) -> ()
      | Error err -> fail (Mongo_error.to_string err)
      | Ok _ -> fail "expected cursor timeout");

      let kill_result = ref None in
      let kill_thread =
        Thread.create
          (fun () -> kill_result := Some (Mongo_cursor.kill cursor))
          ()
      in
      let request_id, kill_command = read_command server_fd in
      check string "kill command" "posts"
        (Bson.get_string (Bson.get_element "killCursors" kill_command));
      check int64 "kill maxTimeMS refreshed" 20L
        (Bson.get_int64 (Bson.get_element "maxTimeMS" kill_command));
      write_response server_fd ~response_to:request_id
        (doc
           [
             ("ok", Bson.create_double 1.0);
             ("cursorsKilled", Bson.create_list [ Bson.create_int64 123L ]);
           ]);
      Thread.join kill_thread;
      match !kill_result with
      | Some (Ok ()) -> ()
      | Some (Error err) -> fail (Mongo_error.to_string err)
      | None -> fail "missing kill result")

let () =
  run "mongo_cursor"
    [
      ( "cursor",
        [
          test_case "find/getMore reuse session" `Quick
            test_find_get_more_and_kill_reuse_session;
          test_case "getMore uses remaining timeoutMS" `Quick
            test_cursor_get_more_uses_remaining_timeout_ms;
          test_case "getMore fails after timeoutMS expires" `Quick
            test_cursor_get_more_fails_after_timeout_ms_expires;
        ] );
    ]
