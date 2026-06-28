type t = {
  connection : Mongo_connection.t;
  db : string;
  mutable collection : string;
  mutable cursor_id : int64;
  mutable namespace : string;
  session : Mongo_command.session_context option;
  deadline : float option;
  timeout_ms : int option;
  mutable batch : Bson.t list;
}

let id t = t.cursor_id
let namespace t = t.namespace
let batch t = t.batch
let alive t = t.cursor_id <> 0L

let int64_of_element element =
  try Bson.get_int64 element
  with _ -> Int64.of_int32 (Bson.get_int32 element)

let cursor_doc response =
  Bson.get_doc_element (Bson.get_element "cursor" response)

let cursor_batch cursor =
  let batch =
    try Bson.get_element "firstBatch" cursor
    with Not_found -> Bson.get_element "nextBatch" cursor
  in
  Bson.get_list batch |> List.map Bson.get_doc_element

let collection_of_namespace ~fallback ns =
  match String.index_opt ns '.' with
  | None -> fallback
  | Some idx when idx + 1 < String.length ns ->
      String.sub ns (idx + 1) (String.length ns - idx - 1)
  | Some _ -> fallback

let operation_deadline (connection : Mongo_connection.t) =
  match connection.config.timeout_ms with
  | Some timeout_ms when timeout_ms > 0 ->
      Some (Unix.gettimeofday () +. (float_of_int timeout_ms /. 1000.0))
  | Some _ | None -> None

let operation_timeout_ms (connection : Mongo_connection.t) =
  connection.config.timeout_ms

let connection_with_remaining_timeout deadline (connection : Mongo_connection.t)
    =
  match deadline with
  | None -> Ok connection
  | Some deadline ->
      let remaining_ms =
        int_of_float (ceil ((deadline -. Unix.gettimeofday ()) *. 1000.0))
      in
      if remaining_ms <= 0 then
        Error (Mongo_error.Timeout "cursor timeoutMS expired")
      else
        Ok
          {
            connection with
            config = { connection.config with timeout_ms = Some remaining_ms };
          }

let parse_cursor ~connection ~db ~collection ~session ~deadline ~timeout_ms
    response =
  let cursor = cursor_doc response.Mongo_command.body in
  let cursor_id = int64_of_element (Bson.get_element "id" cursor) in
  let namespace =
    try Bson.get_string (Bson.get_element "ns" cursor)
    with _ -> db ^ "." ^ collection
  in
  let collection = collection_of_namespace ~fallback:collection namespace in
  {
    connection;
    db;
    collection;
    cursor_id;
    namespace;
    session;
    deadline;
    timeout_ms;
    batch = cursor_batch cursor;
  }

let session_for_connection connection =
  match
    (Mongo_connection.server_info connection)
      .Mongo_connection.logical_session_timeout_minutes
  with
  | None -> None
  | Some _ ->
      Some (Mongo_session.implicit_context (Mongo_session.create ()))

let find_fields collection ?(filter = Bson.empty) ?projection ?sort ?skip ?limit
    ?batch_size () =
  let fields =
    [
      ("find", Bson.create_string collection);
      ("filter", Bson.create_doc_element filter);
    ]
  in
  let add name value fields =
    match value with
    | None -> fields
    | Some v -> fields @ [ (name, v) ]
  in
  fields
  |> add "projection" (Option.map (fun p -> Bson.create_doc_element p) projection)
  |> add "sort" (Option.map (fun s -> Bson.create_doc_element s) sort)
  |> add "skip"
       (Option.map (fun n -> Bson.create_int32 (Int32.of_int n)) skip)
  |> add "limit"
       (Option.map (fun n -> Bson.create_int32 (Int32.of_int n)) limit)
  |> add "batchSize"
       (Option.map (fun n -> Bson.create_int32 (Int32.of_int n)) batch_size)

let find connection ~db ~collection ?filter ?projection ?sort ?skip ?limit
    ?batch_size () =
  let session = session_for_connection connection in
  let deadline = operation_deadline connection in
  let timeout_ms = operation_timeout_ms connection in
  let fields =
    find_fields collection ?filter ?projection ?sort ?skip ?limit ?batch_size ()
  in
  Mongo_connection.run_command ?session connection db
    fields
  |> Result.map
       (parse_cursor ~connection ~db ~collection ~session ~deadline ~timeout_ms)

let update_from_response t response =
  let cursor = cursor_doc response.Mongo_command.body in
  t.cursor_id <- int64_of_element (Bson.get_element "id" cursor);
  t.namespace <-
    (try Bson.get_string (Bson.get_element "ns" cursor)
     with _ -> t.namespace);
  t.collection <- collection_of_namespace ~fallback:t.collection t.namespace;
  t.batch <- cursor_batch cursor;
  t.batch

let get_more ?batch_size t =
  if t.cursor_id = 0L then Ok []
  else
    let fields =
      [
        ("getMore", Bson.create_int64 t.cursor_id);
        ("collection", Bson.create_string t.collection);
      ]
      @
      match batch_size with
      | None -> []
      | Some n -> [ ("batchSize", Bson.create_int32 (Int32.of_int n)) ]
    in
    match connection_with_remaining_timeout t.deadline t.connection with
    | Error err -> Error err
    | Ok connection ->
        Mongo_connection.run_command ?session:t.session connection t.db fields
        |> Result.map (update_from_response t)

let kill t =
  if t.cursor_id = 0L then Ok ()
  else
    let connection =
      {
        t.connection with
        config = { t.connection.config with timeout_ms = t.timeout_ms };
      }
    in
    Mongo_connection.run_command ?session:t.session connection t.db
      [
        ("killCursors", Bson.create_string t.collection);
        ("cursors", Bson.create_list [ Bson.create_int64 t.cursor_id ]);
      ]
    |> Result.map (fun _ ->
           t.cursor_id <- 0L;
           t.batch <- [])
