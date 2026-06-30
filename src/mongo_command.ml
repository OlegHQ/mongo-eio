type read_preference =
  | Primary
  | PrimaryPreferred
  | Secondary
  | SecondaryPreferred
  | Nearest

type write_concern = Mongo_config.write_concern = {
  w : [ `Majority | `Nodes of int | `Tag of string ] option;
  j : bool option;
  wtimeout_ms : int option;
}

type read_concern = Mongo_config.read_concern =
  | Local
  | Majority
  | Linearizable
  | Available
  | Snapshot
  | Custom of string

type response = {
  ok : bool;
  body : Bson.t;
  cluster_time : Bson.t option;
  operation_time : int64 option;
}

type command_event =
  | Command_started of {
      command_name : string;
      database_name : string;
      command : Bson.t;
      request_id : int32;
      connection_id : string option;
    }
  | Command_succeeded of {
      command_name : string;
      duration_ms : float;
      reply : Bson.t;
      request_id : int32;
      connection_id : string option;
    }
  | Command_failed of {
      command_name : string;
      duration_ms : float;
      failure : Mongo_error.t;
      request_id : int32;
      connection_id : string option;
    }

type command_event_handler = command_event -> unit

let document fields =
  List.fold_right
    (fun (name, element) doc -> Bson.add_element name element doc)
    fields Bson.empty

let command_name = function
  | (name, _) :: _ -> Some name
  | [] -> None

let sensitive_command name =
  match String.lowercase_ascii name with
  | "authenticate" | "saslstart" | "saslcontinue" | "getnonce"
  | "createuser" | "updateuser" | "copydbgetnonce" | "copydbsaslstart"
  | "copydb" ->
      true
  | _ -> false

let redacted_doc command_name =
  document [ (command_name, Bson.create_string "<redacted>") ]

let redacted_if_sensitive command_name doc =
  if sensitive_command command_name then redacted_doc command_name else doc

let emit handler event =
  match handler with
  | None -> ()
  | Some handler -> (
      try handler event with _ -> ())

let duration_ms started_at =
  (Unix.gettimeofday () -. started_at) *. 1000.0

let add_last name element doc =
  Bson.all_elements (Bson.remove_element name doc) @ [ (name, element) ]
  |> document

let operation_time doc =
  try
    Some (Bson.get_int64 (Bson.get_element "$clusterTime" doc))
  with _ -> (
    try Some (Bson.get_int64 (Bson.get_element "operationTime" doc))
    with _ -> None)

let cluster_time doc =
  try Some (Bson.get_doc_element (Bson.get_element "$clusterTime" doc))
  with _ -> None

let parse_response doc =
  {
    ok = Mongo_error.ok_value doc;
    body = doc;
    cluster_time = cluster_time doc;
    operation_time = operation_time doc;
  }

let append_db db command =
  add_last "$db" (Bson.create_string db) command

let append_read_preference ?(tag_sets = []) ?max_staleness_seconds pref command =
  add_last "$readPreference"
    (Bson.create_doc_element
       (Mongo_config.read_preference_doc ~tag_sets ?max_staleness_seconds pref))
    command

let write_concern_doc (concern : write_concern) =
  let fields =
    match concern.w with
    | None -> []
    | Some `Majority -> [ ("w", Bson.create_string "majority") ]
    | Some (`Nodes n) -> [ ("w", Bson.create_int32 (Int32.of_int n)) ]
    | Some (`Tag tag) -> [ ("w", Bson.create_string tag) ]
  in
  let fields =
    match concern.j with
    | None -> fields
    | Some j -> fields @ [ ("j", Bson.create_boolean j) ]
  in
  let fields =
    match concern.wtimeout_ms with
    | None -> fields
    | Some ms -> fields @ [ ("wtimeout", Bson.create_int32 (Int32.of_int ms)) ]
  in
  document fields

let read_concern_doc = function
  | Local -> document [ ("level", Bson.create_string "local") ]
  | Majority -> document [ ("level", Bson.create_string "majority") ]
  | Linearizable -> document [ ("level", Bson.create_string "linearizable") ]
  | Available -> document [ ("level", Bson.create_string "available") ]
  | Snapshot -> document [ ("level", Bson.create_string "snapshot") ]
  | Custom level -> document [ ("level", Bson.create_string level) ]

let append_read_concern concern command =
  add_last "readConcern"
    (Bson.create_doc_element (read_concern_doc concern))
    command

let append_write_concern concern command =
  add_last "writeConcern"
    (Bson.create_doc_element (write_concern_doc concern))
    command

let append_lsid session_id command =
  let uuid_element bytes =
    let add_int32_le buf n =
      Buffer.add_char buf (Char.chr (n land 0xff));
      Buffer.add_char buf (Char.chr ((n lsr 8) land 0xff));
      Buffer.add_char buf (Char.chr ((n lsr 16) land 0xff));
      Buffer.add_char buf (Char.chr ((n lsr 24) land 0xff))
    in
    let key = "id" in
    let len = 4 + 1 + String.length key + 1 + 4 + 1 + String.length bytes + 1 in
    let buf = Buffer.create len in
    add_int32_le buf len;
    Buffer.add_char buf '\x05';
    Buffer.add_string buf key;
    Buffer.add_char buf '\x00';
    add_int32_le buf (String.length bytes);
    Buffer.add_char buf '\x04';
    Buffer.add_string buf bytes;
    Buffer.add_char buf '\x00';
    Bson.get_element key (Bson.decode (Buffer.contents buf))
  in
  add_last "lsid"
    (Bson.create_doc_element
       (Bson.add_element "id" (uuid_element session_id) Bson.empty))
    command

let append_txn_number txn command =
  add_last "txnNumber" (Bson.create_int64 txn) command

type session_context = {
  session_id : string option;
  txn_number : int64 option;
  start_transaction : bool option;
  autocommit : bool option;
}

let enrich_command ~db ?session ?read_preference ?(read_preference_tags = [])
    ?max_staleness_seconds ?read_concern ?write_concern fields =
  let command = document fields in
  let command = append_db db command in
  let command =
    match read_preference with
    | None -> command
    | Some pref ->
        append_read_preference ~tag_sets:read_preference_tags
          ?max_staleness_seconds pref command
  in
  let command =
    match read_concern with
    | None -> command
    | Some concern -> append_read_concern concern command
  in
  let command =
    match write_concern with
    | None -> command
    | Some concern -> append_write_concern concern command
  in
  let command =
    match session with
    | None -> command
    | Some ctx -> (
      let command =
        match ctx.session_id with
        | None -> command
        | Some id -> append_lsid id command
      in
      match ctx.txn_number with
      | None -> command
      | Some txn -> append_txn_number txn command)
  in
  let command =
    match session with
    | Some { start_transaction = Some value; _ } ->
        add_last "startTransaction" (Bson.create_boolean value) command
    | Some { start_transaction = None; _ } | None -> command
  in
  match session with
  | Some { autocommit = Some value; _ } ->
      add_last "autocommit" (Bson.create_boolean value) command
  | Some { autocommit = None; _ } | None -> command

let run_transport ?timeout_ms ?session ?read_preference ?command_event_handler
    ?(read_preference_tags = []) ?max_staleness_seconds ?read_concern
    ?write_concern ?connection_id ~db ~request_id transport fields =
  let command =
    enrich_command ~db ?session ?read_preference ~read_preference_tags
      ?max_staleness_seconds ?read_concern ?write_concern fields
  in
  let command_name = Option.value (command_name fields) ~default:"<unknown>" in
  let started_at = Unix.gettimeofday () in
  emit command_event_handler
    (Command_started
       {
         command_name;
         database_name = db;
         command = redacted_if_sensitive command_name command;
         request_id;
         connection_id;
       });
  let fail failure =
    emit command_event_handler
      (Command_failed
         {
           command_name;
           duration_ms = duration_ms started_at;
           failure;
           request_id;
           connection_id;
         });
    Error failure
  in
  let request = Mongo_wire.encode_op_msg request_id command in
  match Mongo_wire.write_all_transport ?timeout_ms transport request with
  | Error err -> fail err
  | Ok () -> (
      match Mongo_wire.read_message_transport ?timeout_ms transport with
      | Error err -> fail err
      | Ok message -> (
          match Mongo_wire.response_body ~expected_request_id:request_id message with
          | Error err -> fail err
          | Ok body ->
              (if Mongo_error.ok_value body then
                emit command_event_handler
                  (Command_succeeded
                     {
                       command_name;
                       duration_ms = duration_ms started_at;
                       reply = redacted_if_sensitive command_name body;
                       request_id;
                       connection_id;
                     })
              else
                let failure =
                  match Mongo_error.of_command_reply body with
                  | Error err -> err
                  | Ok _ ->
                      Mongo_error.Protocol
                        "command reply had non-success ok value"
                in
                emit command_event_handler
                  (Command_failed
                     {
                       command_name;
                       duration_ms = duration_ms started_at;
                       failure;
                       request_id;
                       connection_id;
                     }));
              match Mongo_error.of_command_reply body with
              | Ok doc -> Ok (parse_response doc)
              | Error err -> Error err))

let run ?timeout_ms ?session ?read_preference ?command_event_handler
    ?(read_preference_tags = []) ?max_staleness_seconds ?read_concern
    ?write_concern ?connection_id ~db ~request_id file_descr fields =
  run_transport ?timeout_ms ?session ?read_preference ?command_event_handler
    ~read_preference_tags ?max_staleness_seconds ?read_concern ?write_concern
    ?connection_id ~db ~request_id (Mongo_transport.plain file_descr) fields

let run_exn ?timeout_ms ?session ?read_preference ?command_event_handler
    ?(read_preference_tags = []) ?max_staleness_seconds ?read_concern
    ?write_concern ?connection_id ~db ~request_id file_descr fields =
  match
    run ?timeout_ms ?session ?read_preference ?command_event_handler
      ~read_preference_tags
      ?max_staleness_seconds ?read_concern ?write_concern ~db ~request_id
      ?connection_id file_descr fields
  with
  | Ok response -> response.body
  | Error err -> Mongo_error.raise_exn err

let cursor_batch reply =
  let cursor = Bson.get_doc_element (Bson.get_element "cursor" reply) in
  let batch =
    try Bson.get_element "firstBatch" cursor
    with Not_found -> Bson.get_element "nextBatch" cursor
  in
  Bson.get_list batch |> List.map Bson.get_doc_element

let int_of_bson element =
  try Int32.to_int (Bson.get_int32 element)
  with _ -> (
    try Int64.to_int (Bson.get_int64 element)
    with _ -> int_of_float (Bson.get_double element))
