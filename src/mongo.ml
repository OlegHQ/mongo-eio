open MongoUtils;;

exception Mongo_failed of string;;

type t =
    {
      db_name: string;
      collection_name: string;
      ip: string;
      port: int;
      file_descr: Unix.file_descr
    };;

let get_db_name m = m.db_name;;
let get_collection_name m = m.collection_name;;
let get_ip m = m.ip;;
let get_port m = m.port;;
let get_file_descr m = m.file_descr;;
let change_collection m c =
  { m with
      collection_name = c ;
  }

let wrap_unix f arg =
  try (f arg) with
    | Unix.Unix_error (e, _, _) -> raise (Mongo_failed (Unix.error_message e));;

let close_noerr file_descr =
  try Unix.close file_descr with Unix.Unix_error _ -> ();;

let connect_to (host,port) =
    let service = string_of_int port in
    let addresses = Unix.getaddrinfo host service [Unix.AI_SOCKTYPE Unix.SOCK_STREAM] in
    let rec connect_first = function
      | [] -> raise (Unix.Unix_error (Unix.EHOSTUNREACH, "connect", host))
      | address :: rest ->
          let c_descr =
            Unix.socket address.Unix.ai_family address.Unix.ai_socktype
              address.Unix.ai_protocol
          in
          try
            Unix.connect c_descr address.Unix.ai_addr;
            c_descr
          with Unix.Unix_error _ as error ->
            close_noerr c_descr;
            if rest = [] then raise error else connect_first rest
    in
    connect_first addresses;;

let create ip port db_name collection_name =
  {
    db_name = db_name;
    collection_name = collection_name;
    ip = ip;
    port = port;
    file_descr = wrap_unix connect_to (ip,port)
  };;

let create_local_default db_name collection_name =
  create "127.0.0.1" 27017 db_name collection_name;;

let destroy m = wrap_unix Unix.close m.file_descr;;


let get_request_id = cur_timestamp;;

let send_only (m, str) = MongoSend.send_no_reply m.file_descr str;;

let document fields =
  List.fold_right
    (fun (name, element) doc -> Bson.add_element name element doc)
    fields Bson.empty;;

let command_doc m fields =
  document (fields @ [("$db", Bson.create_string m.db_name)]);;

let read_message file_descr =
  let in_ch = Unix.in_channel_of_descr file_descr in
  let len_bytes = Bytes.create 4 in
  really_input in_ch len_bytes 0 4;
  let len_str = Bytes.to_string len_bytes in
  let (len32, _) = decode_int32 len_str 0 in
  let len = Int32.to_int len32 in
  let rest = Bytes.create (len - 4) in
  really_input in_ch rest 0 (len - 4);
  len_str ^ Bytes.to_string rest;;

let create_op_msg request_id body_doc =
  let body_buf = Buffer.create 128 in
  encode_int32 body_buf 0l;
  Buffer.add_char body_buf '\x00';
  Buffer.add_string body_buf (Bson.encode body_doc);
  let header =
    MongoHeader.encode_header
      (MongoHeader.create_request_header (Buffer.length body_buf) request_id
         MongoOperation.OP_MSG)
  in
  header ^ Buffer.contents body_buf;;

let response_message_doc message =
  let header = MongoHeader.decode_header (String.sub message 0 (4 * 4)) in
  match MongoHeader.get_op header with
  | MongoOperation.OP_MSG ->
      let (_flags, section_index) = decode_int32 message (4 * 4) in
      if message.[section_index] <> '\x00' then
        raise (Mongo_failed "unsupported OP_MSG section kind");
      let doc_start = section_index + 1 in
      Bson.decode (String.sub message doc_start (String.length message - doc_start))
  | MongoOperation.OP_REPLY -> (
      match MongoReply.get_document_list (MongoReply.decode_reply message) with
      | doc :: _ -> doc
      | [] -> raise (Mongo_failed "empty MongoDB reply"))
  | _ -> raise (Mongo_failed "unexpected MongoDB reply opcode");;

let ok_value doc =
  try Bson.get_double (Bson.get_element "ok" doc) = 1.0 with
  | _ -> (
      try Bson.get_int32 (Bson.get_element "ok" doc) = 1l with
      | _ -> (
          try Bson.get_int64 (Bson.get_element "ok" doc) = 1L with
          | _ -> false));;

let command_error_message doc =
  let field name =
    try Some (Bson.get_string (Bson.get_element name doc)) with _ -> None
  in
  match (field "errmsg", field "$err") with
  | Some message, _ | None, Some message -> message
  | None, None -> "MongoDB command failed";;

let write_error_message doc =
  let first_error name =
    try
      match Bson.get_list (Bson.get_element name doc) with
      | [] -> None
      | error :: _ ->
          let error_doc = Bson.get_doc_element error in
          Some (Bson.get_string (Bson.get_element "errmsg" error_doc))
    with _ -> None
  in
  match (first_error "writeErrors", first_error "writeConcernErrors") with
  | Some message, _ | None, Some message -> Some message
  | None, None -> None;;

let command m fields =
  let request = create_op_msg (get_request_id ()) (command_doc m fields) in
  send_only (m, request);
  let reply = response_message_doc (read_message m.file_descr) in
  if not (ok_value reply) then raise (Mongo_failed (command_error_message reply));
  match write_error_message reply with
  | Some message -> raise (Mongo_failed message)
  | None -> reply;;

let cursor_batch reply =
  let cursor = Bson.get_doc_element (Bson.get_element "cursor" reply) in
  let batch =
    try Bson.get_element "firstBatch" cursor with
    | Not_found -> Bson.get_element "nextBatch" cursor
  in
  Bson.get_list batch |> List.map Bson.get_doc_element;;

let int_of_bson element =
  try Int32.to_int (Bson.get_int32 element) with
  | _ -> (
      try Int64.to_int (Bson.get_int64 element) with
      | _ -> int_of_float (Bson.get_double element));;

let find_command ?(skip=0) ?limit ?projection m query =
  let fields =
    [
      ("find", Bson.create_string m.collection_name);
      ("filter", Bson.create_doc_element query);
    ]
  in
  let fields =
    if skip > 0 then fields @ [("skip", Bson.create_int32 (Int32.of_int skip))]
    else fields
  in
  let fields =
    match limit with
    | Some limit when limit > 0 ->
        fields @ [("limit", Bson.create_int32 (Int32.of_int limit))]
    | _ -> fields
  in
  let fields =
    match projection with
    | Some projection when not (Bson.is_empty projection) ->
        fields @ [("projection", Bson.create_doc_element projection)]
    | _ -> fields
  in
  command m fields |> cursor_batch |> MongoReply.create;;

let insert_command m doc_list =
  ignore
    (command m
       [
         ("insert", Bson.create_string m.collection_name);
         ("documents", Bson.create_doc_element_list doc_list);
         ("ordered", Bson.create_boolean true);
       ]);;

let update_command m selector update_doc ~upsert ~multi =
  let update_spec =
    document
      [
        ("q", Bson.create_doc_element selector);
        ("u", Bson.create_doc_element update_doc);
        ("upsert", Bson.create_boolean upsert);
        ("multi", Bson.create_boolean multi);
      ]
  in
  ignore
    (command m
       [
         ("update", Bson.create_string m.collection_name);
         ("updates", Bson.create_doc_element_list [update_spec]);
         ("ordered", Bson.create_boolean true);
       ]);;

let delete_command m selector ~limit =
  let delete_spec =
    document
      [
        ("q", Bson.create_doc_element selector);
        ("limit", Bson.create_int32 (Int32.of_int limit));
      ]
  in
  ignore
    (command m
       [
         ("delete", Bson.create_string m.collection_name);
         ("deletes", Bson.create_doc_element_list [delete_spec]);
         ("ordered", Bson.create_boolean true);
       ]);;

let insert m doc_list = wrap_unix (fun m -> insert_command m doc_list) m;;

let update_one ?(upsert=false) m (s,u) = wrap_unix (fun m -> update_command m s u ~upsert ~multi:false) m;;
let update_all ?(upsert=false) m (s,u) = wrap_unix (fun m -> update_command m s u ~upsert ~multi:true) m;;

let delete_one m s = wrap_unix (fun m -> delete_command m s ~limit:1) m;;
let delete_all m s = wrap_unix (fun m -> delete_command m s ~limit:0) m;;

let find ?(skip=0) m = wrap_unix (fun m -> find_command ~skip m Bson.empty) m;;
let find_one ?(skip=0) m = wrap_unix (fun m -> find_command ~skip ~limit:1 m Bson.empty) m;;
let find_of_num ?(skip=0) m num = wrap_unix (fun m -> find_command ~skip ~limit:num m Bson.empty) m;;
let find_q ?(skip=0) m q = wrap_unix (fun m -> find_command ~skip m q) m;;
let find_q_one ?(skip=0) m q = wrap_unix (fun m -> find_command ~skip ~limit:1 m q) m;;
let find_q_of_num ?(skip=0) m q num = wrap_unix (fun m -> find_command ~skip ~limit:num m q) m;;
let find_q_s ?(skip=0) m q s = wrap_unix (fun m -> find_command ~skip ~projection:s m q) m;;
let find_q_s_one ?(skip=0) m q s = wrap_unix (fun m -> find_command ~skip ~limit:1 ~projection:s m q) m;;
let find_q_s_of_num ?(skip=0) m q s num = wrap_unix (fun m -> find_command ~skip ~limit:num ~projection:s m q) m;;

let count ?skip ?limit ?(query=Bson.empty) m =
  let fields =
    [
      ("count", Bson.create_string m.collection_name);
      ("query", Bson.create_doc_element query);
    ]
  in
  let fields =
    match skip with
    | Some n -> fields @ [("skip", Bson.create_int32 (Int32.of_int n))]
    | None -> fields
  in
  let fields =
    match limit with
    | Some n -> fields @ [("limit", Bson.create_int32 (Int32.of_int n))]
    | None -> fields
  in
  let reply = command m fields in
  int_of_bson (Bson.get_element "n" reply)


let get_more_of_num m c num =
  wrap_unix
    (fun m ->
      let fields =
        [
          ("getMore", Bson.create_int64 c);
          ("collection", Bson.create_string m.collection_name);
        ]
      in
      let fields =
        if num > 0 then
          fields @ [("batchSize", Bson.create_int32 (Int32.of_int num))]
        else fields
      in
      command m fields |> cursor_batch |> MongoReply.create)
    m;;
let get_more m c = get_more_of_num m c 0;;

let kill_cursors m c_list =
  wrap_unix
    (fun m ->
      ignore
        (command m
           [
             ("killCursors", Bson.create_string m.collection_name);
             ("cursors", Bson.create_list (List.map Bson.create_int64 c_list));
           ]))
    m;;

let drop_database m =
  command m [("dropDatabase", Bson.create_int32 1l)] |> fun doc ->
  MongoReply.create [doc]

let drop_collection m =
  command m [("drop", Bson.create_string m.collection_name)] |> fun doc ->
  MongoReply.create [doc]


(** INDEX **)
let get_indexes m =
  command m [("listIndexes", Bson.create_string m.collection_name)]
  |> cursor_batch |> MongoReply.create

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

let ensure_index m key_bson options =
  let default_name () =
    let doc = Bson.get_element "key" key_bson in

    List.fold_left (
      fun s (k,e) ->
        let i = Bson.get_int32 e in
        if s = "" then
          Printf.sprintf "%s_%ld" k i
        else
          Printf.sprintf "%s_%s_%ld" s k i
    ) "" (Bson.all_elements (Bson.get_doc_element doc))
  in

  let has_name = ref false in
  let has_version = ref false in
  (* check all options *)

  let main_bson =
    List.fold_left (
      fun acc o ->
        match o with
          | Background b ->
            Bson.add_element "background" (Bson.create_boolean b) acc
          | Unique b ->
            Bson.add_element "unique" (Bson.create_boolean b) acc
          | Name s ->
            has_name := true;
            Bson.add_element "name" (Bson.create_string s) acc
          | DropDups b ->
            Bson.add_element "dropDups" (Bson.create_boolean b) acc
          | Sparse b ->
            Bson.add_element "sparse" (Bson.create_boolean b) acc
          | ExpireAfterSeconds i ->
            Bson.add_element "expireAfterSeconds" (Bson.create_int32 (Int32.of_int i)) acc
          | V i ->
            if i <> 0 && i <> 1 then raise (Mongo_failed "Version number for index must be 0 or 1");
            has_version := true;
            Bson.add_element "v" (Bson.create_int32 (Int32.of_int i)) acc
          | Weight bson ->
            Bson.add_element "weights" (Bson.create_doc_element bson) acc
          | Default_language s ->
            Bson.add_element "default_language" (Bson.create_string s) acc
          | Language_override s ->
            Bson.add_element "language_override" (Bson.create_string s) acc
    ) key_bson options
  in

  (* check if then name has been set, create a default name otherwise *)
  let main_bson =
    if !has_name = false then begin
      Bson.add_element "name" (Bson.create_string (default_name ())) main_bson
    end else main_bson
  in

  (* check if the version has been set, set 1 otherwise *)
  let main_bson =
    if !has_version = false then
      Bson.add_element "v" (Bson.create_int32 1l) main_bson
    else main_bson
  in

  ignore
    (command m
       [
         ("createIndexes", Bson.create_string m.collection_name);
         ("indexes", Bson.create_doc_element_list [main_bson]);
       ]);;


let ensure_simple_index ?(options=[]) m field =
  let key_bson = Bson.add_element "key" (Bson.create_doc_element (Bson.add_element field (Bson.create_int32 1l) Bson.empty)) Bson.empty in
  ensure_index m key_bson options

let ensure_multi_simple_index ?(options=[]) m fields =
  let key_bson =
    List.fold_left (
      fun acc f ->
        Bson.add_element f (Bson.create_int32 1l) acc
    ) Bson.empty fields
  in

  let key_bson = Bson.add_element "key" (Bson.create_doc_element key_bson) Bson.empty in
  ensure_index m key_bson options

let drop_index m index_name =
  command m
    [
      ("dropIndexes", Bson.create_string m.collection_name);
      ("index", Bson.create_string index_name);
    ]
  |> fun doc -> MongoReply.create [doc]

let drop_all_index m =
  drop_index m "*"
