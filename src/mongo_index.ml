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

let default_name key_bson =
  let doc = Bson.get_element "key" key_bson in
  List.fold_left
    (fun s (k, e) ->
      let i = Bson.get_int32 e in
      if s = "" then Printf.sprintf "%s_%ld" k i
      else Printf.sprintf "%s_%s_%ld" s k i)
    "" (Bson.all_elements (Bson.get_doc_element doc))

let apply_option acc = function
  | Background b -> Bson.add_element "background" (Bson.create_boolean b) acc
  | Unique b -> Bson.add_element "unique" (Bson.create_boolean b) acc
  | Name s -> Bson.add_element "name" (Bson.create_string s) acc
  | DropDups b -> Bson.add_element "dropDups" (Bson.create_boolean b) acc
  | Sparse b -> Bson.add_element "sparse" (Bson.create_boolean b) acc
  | ExpireAfterSeconds i ->
      Bson.add_element "expireAfterSeconds" (Bson.create_int32 (Int32.of_int i))
        acc
  | V i ->
      if i <> 0 && i <> 1 then
        Mongo_error.raise_exn (Unsupported "Version number for index must be 0 or 1");
      Bson.add_element "v" (Bson.create_int32 (Int32.of_int i)) acc
  | Weight bson -> Bson.add_element "weights" (Bson.create_doc_element bson) acc
  | Default_language s ->
      Bson.add_element "default_language" (Bson.create_string s) acc
  | Language_override s ->
      Bson.add_element "language_override" (Bson.create_string s) acc

let ensure_index conn ~db ~collection key_bson options =
  let has_name = ref false in
  let has_version = ref false in
  let main_bson =
    List.fold_left
      (fun acc o ->
        match o with
        | Name _ ->
            has_name := true;
            apply_option acc o
        | V _ ->
            has_version := true;
            apply_option acc o
        | other -> apply_option acc other)
      key_bson options
  in
  let main_bson =
    if not !has_name then
      Bson.add_element "name" (Bson.create_string (default_name key_bson)) main_bson
    else main_bson
  in
  let main_bson =
    if not !has_version then Bson.add_element "v" (Bson.create_int32 1l) main_bson
    else main_bson
  in
  Mongo_connection.run_command conn db
    [
      ("createIndexes", Bson.create_string collection);
      ("indexes", Bson.create_doc_element_list [ main_bson ]);
    ]
  |> Result.map (fun _ -> ())

let ensure_simple_index conn ~db ~collection field options =
  let key_bson =
    Bson.add_element "key"
      (Bson.create_doc_element
         (Bson.add_element field (Bson.create_int32 1l) Bson.empty))
      Bson.empty
  in
  ensure_index conn ~db ~collection key_bson options

let list_indexes conn ~db ~collection =
  Mongo_connection.run_command conn db
    [ ("listIndexes", Bson.create_string collection) ]
  |> Result.map (fun (response : Mongo_command.response) ->
         Mongo_command.cursor_batch response.body)

let drop_index conn ~db ~collection index_name =
  Mongo_connection.run_command conn db
    [
      ("dropIndexes", Bson.create_string collection);
      ("index", Bson.create_string index_name);
    ]

let drop_all_indexes conn ~db ~collection = drop_index conn ~db ~collection "*"
