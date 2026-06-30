type find_options = {
  filter : Bson.t;
  projection : Bson.t option;
  sort : Bson.t option;
  skip : int option;
  limit : int option;
  batch_size : int option;
  read_concern : Mongo_command.read_concern option;
}

type insert_options = {
  ordered : bool;
  write_concern : Mongo_command.write_concern option;
}

type write_result = {
  acknowledged : bool;
  inserted_count : int;
  matched_count : int;
  modified_count : int option;
  deleted_count : int;
  upserted_ids : (int * Bson.element) list;
}

let default_find _collection filter =
  {
    filter;
    projection = None;
    sort = None;
    skip = None;
    limit = None;
    batch_size = None;
    read_concern = None;
  }

let default_insert = { ordered = true; write_concern = None }

let int_field name doc =
  try Mongo_command.int_of_bson (Bson.get_element name doc) with _ -> 0

let optional_int_field name doc =
  try Some (Mongo_command.int_of_bson (Bson.get_element name doc)) with _ -> None

let parse_upserted doc =
  try
    Bson.get_list (Bson.get_element "upserted" doc)
    |> List.filter_map (fun element ->
           try
             let item = Bson.get_doc_element element in
             Some
               ( int_field "index" item,
                 Bson.get_element "_id" item )
           with _ -> None)
  with _ -> []

let write_result ?(inserted_count = 0) ?(matched_count = 0)
    ?modified_count ?(deleted_count = 0) response =
  {
    acknowledged = response.Mongo_command.ok;
    inserted_count;
    matched_count;
    modified_count;
    deleted_count;
    upserted_ids = parse_upserted response.body;
  }

let find_fields collection opts =
  let fields =
    [
      ("find", Bson.create_string collection);
      ("filter", Bson.create_doc_element opts.filter);
    ]
  in
  let add name value fields =
    match value with
    | None -> fields
    | Some v -> fields @ [ (name, v) ]
  in
  fields
  |> add "projection"
       (Option.map (fun p -> Bson.create_doc_element p) opts.projection)
  |> add "sort" (Option.map (fun s -> Bson.create_doc_element s) opts.sort)
  |> add "skip"
       (Option.map (fun n -> Bson.create_int32 (Int32.of_int n)) opts.skip)
  |> add "limit"
       (Option.map (fun n -> Bson.create_int32 (Int32.of_int n)) opts.limit)
  |> add "batchSize"
       (Option.map (fun n -> Bson.create_int32 (Int32.of_int n)) opts.batch_size)

let find ?session conn ~db ~collection opts =
  Mongo_connection.run_command ?session ?read_concern:opts.read_concern conn db
    (find_fields collection opts)
  |> Result.map (fun (response : Mongo_command.response) ->
         Mongo_command.cursor_batch response.body)

let find_one ?session conn ~db ~collection filter =
  let opts = { (default_find collection filter) with limit = Some 1 } in
  find ?session conn ~db ~collection opts
  |> Result.map (function [] -> None | h :: _ -> Some h)

let insert ?session conn ~db ~collection ?write_concern ~ordered docs =
  Mongo_connection.run_command ?session ?write_concern conn db
    [
      ("insert", Bson.create_string collection);
      ("documents", Bson.create_doc_element_list docs);
      ("ordered", Bson.create_boolean ordered);
    ]
  |> Result.map (fun (response : Mongo_command.response) ->
         write_result ~inserted_count:(int_field "n" response.body) response)

let insert_one ?session ?write_concern conn ~db ~collection doc =
  insert ?session conn ~db ~collection ?write_concern ~ordered:true [ doc ]

let insert_many ?session ?(options = default_insert) conn ~db ~collection docs =
  insert ?session conn ~db ~collection ?write_concern:options.write_concern
    ~ordered:options.ordered docs

let update ?session conn ~db ~collection ?write_concern ~multi selector update_doc
    ~upsert =
  let update_spec =
    Mongo_command.document
      [
        ("q", Bson.create_doc_element selector);
        ("u", Bson.create_doc_element update_doc);
        ("upsert", Bson.create_boolean upsert);
        ("multi", Bson.create_boolean multi);
      ]
  in
  Mongo_connection.run_command ?session ?write_concern conn db
    [
      ("update", Bson.create_string collection);
      ("updates", Bson.create_doc_element_list [ update_spec ]);
      ("ordered", Bson.create_boolean true);
    ]
  |> Result.map (fun (response : Mongo_command.response) ->
         write_result ~matched_count:(int_field "n" response.body)
         ?modified_count:(optional_int_field "nModified" response.body)
         response)

let update_one ?session ?write_concern conn ~db ~collection ~upsert selector update_doc =
  update ?session conn ~db ~collection ?write_concern ~multi:false ~upsert selector
    update_doc

let update_many ?session ?write_concern conn ~db ~collection ~upsert selector update_doc =
  update ?session conn ~db ~collection ?write_concern ~multi:true ~upsert selector
    update_doc

let delete ?session conn ~db ~collection ?write_concern ~limit selector =
  let delete_spec =
    Mongo_command.document
      [
        ("q", Bson.create_doc_element selector);
        ("limit", Bson.create_int32 (Int32.of_int limit));
      ]
  in
  Mongo_connection.run_command ?session ?write_concern conn db
    [
      ("delete", Bson.create_string collection);
      ("deletes", Bson.create_doc_element_list [ delete_spec ]);
      ("ordered", Bson.create_boolean true);
    ]
  |> Result.map (fun (response : Mongo_command.response) ->
         write_result ~deleted_count:(int_field "n" response.body) response)

let delete_one ?session ?write_concern conn ~db ~collection selector =
  delete ?session conn ~db ~collection ?write_concern ~limit:1 selector

let delete_many ?session ?write_concern conn ~db ~collection selector =
  delete ?session conn ~db ~collection ?write_concern ~limit:0 selector

let count_documents ?session conn ~db ~collection ?query () =
  let query = Option.value query ~default:Bson.empty in
  Mongo_connection.run_command ?session conn db
    [
      ("count", Bson.create_string collection);
      ("query", Bson.create_doc_element query);
    ]
  |> Result.map (fun (response : Mongo_command.response) ->
         Mongo_command.int_of_bson (Bson.get_element "n" response.body))

let estimated_document_count ?session conn ~db ~collection =
  count_documents ?session conn ~db ~collection ()
