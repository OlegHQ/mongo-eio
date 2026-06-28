exception Mongo_failed = Mongo_error.Mongo_failed

type t = {
  db_name : string;
  collection_name : string;
  ip : string;
  port : int;
  connection : Mongo_connection.t;
}

let get_db_name m = m.db_name
let get_collection_name m = m.collection_name
let get_ip m = m.ip
let get_port m = m.port
let get_file_descr m = Mongo_connection.file_descr m.connection

let change_collection m c = { m with collection_name = c }

let wrap_result = function
  | Ok value -> value
  | Error err -> Mongo_error.raise_exn err

let create ip port db_name collection_name =
  let config = Mongo_config.default ~host:ip ~port ~database:db_name () in
  match Mongo_connection.connect config with
  | Ok connection ->
      { db_name; collection_name; ip; port; connection }
  | Error err -> Mongo_error.raise_exn err

let create_local_default db_name collection_name =
  create "127.0.0.1" 27017 db_name collection_name

let destroy m = Mongo_connection.close m.connection

let command m fields =
  Mongo_connection.run_command_exn m.connection m.db_name fields

let cursor_batch reply = Mongo_command.cursor_batch reply

let find_command ?(skip = 0) ?limit ?projection m query =
  let opts =
    {
      Mongo_crud.filter = query;
      projection;
      sort = None;
      skip = (if skip > 0 then Some skip else None);
      limit;
      batch_size = None;
      read_concern = None;
    }
  in
  wrap_result
    (Mongo_crud.find m.connection ~db:m.db_name ~collection:m.collection_name opts)
  |> MongoReply.create

let insert m doc_list =
  ignore
    (wrap_result
       (Mongo_crud.insert m.connection ~db:m.db_name ~collection:m.collection_name
          ~ordered:true doc_list))

let update_one ?(upsert = false) m (s, u) =
  ignore
    (wrap_result
       (Mongo_crud.update_one m.connection ~db:m.db_name
          ~collection:m.collection_name ~upsert s u))

let update_all ?(upsert = false) m (s, u) =
  ignore
    (wrap_result
       (Mongo_crud.update_many m.connection ~db:m.db_name
          ~collection:m.collection_name ~upsert s u))

let delete_one m s =
  ignore
    (wrap_result
       (Mongo_crud.delete_one m.connection ~db:m.db_name
          ~collection:m.collection_name s))

let delete_all m s =
  ignore
    (wrap_result
       (Mongo_crud.delete_many m.connection ~db:m.db_name
          ~collection:m.collection_name s))

let find ?(skip = 0) m = find_command ~skip m Bson.empty
let find_one ?(skip = 0) m = find_command ~skip ~limit:1 m Bson.empty
let find_of_num ?(skip = 0) m num = find_command ~skip ~limit:num m Bson.empty
let find_q ?(skip = 0) m q = find_command ~skip m q
let find_q_one ?(skip = 0) m q = find_command ~skip ~limit:1 m q
let find_q_of_num ?(skip = 0) m q num = find_command ~skip ~limit:num m q
let find_q_s ?(skip = 0) m q s = find_command ~skip ~projection:s m q

let find_q_s_one ?(skip = 0) m q s =
  find_command ~skip ~limit:1 ~projection:s m q

let find_q_s_of_num ?(skip = 0) m q s num =
  find_command ~skip ~limit:num ~projection:s m q

let count ?skip ?limit ?(query = Bson.empty) m =
  let fields =
    [
      ("count", Bson.create_string m.collection_name);
      ("query", Bson.create_doc_element query);
    ]
  in
  let fields =
    match skip with
    | Some n -> fields @ [ ("skip", Bson.create_int32 (Int32.of_int n)) ]
    | None -> fields
  in
  let fields =
    match limit with
    | Some n -> fields @ [ ("limit", Bson.create_int32 (Int32.of_int n)) ]
    | None -> fields
  in
  command m fields |> fun reply ->
  Mongo_command.int_of_bson (Bson.get_element "n" reply)

let get_more_of_num m c num =
  let fields =
    [
      ("getMore", Bson.create_int64 c);
      ("collection", Bson.create_string m.collection_name);
    ]
  in
  let fields =
    if num > 0 then
      fields @ [ ("batchSize", Bson.create_int32 (Int32.of_int num)) ]
    else fields
  in
  command m fields |> cursor_batch |> MongoReply.create

let get_more m c = get_more_of_num m c 0

let kill_cursors m c_list =
  ignore
    (command m
       [
         ("killCursors", Bson.create_string m.collection_name);
         ("cursors", Bson.create_list (List.map Bson.create_int64 c_list));
       ])

let drop_database m =
  command m [ ("dropDatabase", Bson.create_int32 1l) ] |> fun doc ->
  MongoReply.create [ doc ]

let drop_collection m =
  command m [ ("drop", Bson.create_string m.collection_name) ] |> fun doc ->
  MongoReply.create [ doc ]

type index_option =
  | Background of bool
  | Unique of bool
  | Name of string
  | DropDups of bool
  | Sparse of bool
  | ExpireAfterSeconds of int
  | V of int
  | Weight of Bson.t
  | Default_language of string
  | Language_override of string

let to_index_option = function
  | Background b -> Mongo_index.Background b
  | Unique b -> Mongo_index.Unique b
  | Name s -> Mongo_index.Name s
  | DropDups b -> Mongo_index.DropDups b
  | Sparse b -> Mongo_index.Sparse b
  | ExpireAfterSeconds i -> Mongo_index.ExpireAfterSeconds i
  | V i -> Mongo_index.V i
  | Weight b -> Mongo_index.Weight b
  | Default_language s -> Mongo_index.Default_language s
  | Language_override s -> Mongo_index.Language_override s

let get_indexes m =
  wrap_result
    (Mongo_index.list_indexes m.connection ~db:m.db_name ~collection:m.collection_name)
  |> fun docs -> MongoReply.create docs

let ensure_index m key_bson (options : index_option list) =
  wrap_result
    (Mongo_index.ensure_index m.connection ~db:m.db_name ~collection:m.collection_name
       key_bson (List.map to_index_option options))

let ensure_simple_index ?(options = []) m field =
  wrap_result
    (Mongo_index.ensure_simple_index m.connection ~db:m.db_name
       ~collection:m.collection_name field (List.map to_index_option options))

let ensure_multi_simple_index ?(options = []) m fields =
  let key_bson =
    List.fold_left
      (fun acc f -> Bson.add_element f (Bson.create_int32 1l) acc)
      Bson.empty fields
  in
  let key_bson =
    Bson.add_element "key" (Bson.create_doc_element key_bson) Bson.empty
  in
  ensure_index m key_bson options

let drop_index m index_name =
  wrap_result
    (Mongo_index.drop_index m.connection ~db:m.db_name ~collection:m.collection_name
       index_name)
  |> fun response -> MongoReply.create [ response.body ]

let drop_all_index m = drop_index m "*"
