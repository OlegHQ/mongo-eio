type write_error = {
  index : int option;
  code : int option;
  code_name : string option;
  message : string;
}

type command_error = {
  code : int option;
  code_name : string option;
  message : string;
  labels : string list;
  write_errors : write_error list;
  write_concern_errors : write_error list;
}

type t =
  | Network of string
  | Timeout of string
  | Execution_timeout of string
  | Protocol of string
  | Command of command_error
  | Authentication of string
  | Server_selection of string
  | Unsupported of string

exception Mongo_failed of string

let duplicate_key_code = 11000
let max_time_ms_expired_code = 50

let string_field doc name =
  try Some (Bson.get_string (Bson.get_element name doc)) with _ -> None

let parse_write_error doc =
  {
    index =
      (try Some (Int32.to_int (Bson.get_int32 (Bson.get_element "index" doc)))
       with _ -> None);
    code =
      (try Some (Int32.to_int (Bson.get_int32 (Bson.get_element "code" doc)))
       with _ -> None);
    code_name = string_field doc "codeName";
    message =
      (match string_field doc "errmsg" with
      | Some message -> message
      | None -> (
          match string_field doc "errMessage" with
          | Some message -> message
          | None -> "write error"));
  }

let parse_write_errors doc name =
  try
    Bson.get_list (Bson.get_element name doc)
    |> List.map (fun element ->
           parse_write_error (Bson.get_doc_element element))
  with _ -> []

let parse_labels doc =
  try
    Bson.get_list (Bson.get_element "errorLabels" doc)
    |> List.map Bson.get_string
  with _ -> []

let ok_value doc =
  try Bson.get_double (Bson.get_element "ok" doc) = 1.0
  with _ -> (
    try Bson.get_int32 (Bson.get_element "ok" doc) = 1l
    with _ -> (
      try Bson.get_int64 (Bson.get_element "ok" doc) = 1L
      with _ -> false))

let command_message doc =
  match (string_field doc "errmsg", string_field doc "$err") with
  | Some message, _ | None, Some message -> message
  | None, None -> "MongoDB command failed"

let of_command_reply doc =
  let write_errors = parse_write_errors doc "writeErrors" in
  let write_concern_errors = parse_write_errors doc "writeConcernErrors" in
  if ok_value doc && write_errors = [] && write_concern_errors = [] then Ok doc
  else
    let top_code =
      try Some (Int32.to_int (Bson.get_int32 (Bson.get_element "code" doc)))
      with _ -> (
        try Some (Int64.to_int (Bson.get_int64 (Bson.get_element "code" doc)))
        with _ -> None)
    in
    let message =
      match (write_errors, write_concern_errors) with
      | err :: _, _ -> err.message
      | [], err :: _ -> err.message
      | [], [] -> command_message doc
    in
    if top_code = Some max_time_ms_expired_code && write_errors = []
       && write_concern_errors = []
    then Error (Execution_timeout message)
    else
      Error
        (Command
           {
             code = top_code;
             code_name = string_field doc "codeName";
             message;
             labels = parse_labels doc;
             write_errors;
             write_concern_errors;
           })

let is_duplicate_key = function
  | Command { code = Some c; _ } when c = duplicate_key_code -> true
  | Command { write_errors = err :: _; _ } -> (
      match err.code with
      | Some c when c = duplicate_key_code -> true
      | _ -> false)
  | _ -> false

let labels = function Command { labels; _ } -> labels | _ -> []

let has_label err label = List.mem label (labels err)

let code = function Command { code; _ } -> code | _ -> None

let code_name = function Command { code_name; _ } -> code_name | _ -> None

let redact_uri uri =
  match String.index_opt uri '@' with
  | None -> uri
  | Some at ->
      let start =
        match String.rindex_from_opt uri at ':' with
        | Some colon -> colon + 1
        | None -> 0
      in
      String.sub uri 0 start ^ "***@" ^ String.sub uri (at + 1) (String.length uri - at - 1)

let to_string = function
  | Network message -> "network error: " ^ message
  | Timeout message -> "timeout: " ^ message
  | Execution_timeout message -> "timeout: " ^ message
  | Protocol message -> "protocol error: " ^ message
  | Authentication message -> "authentication error: " ^ message
  | Server_selection message -> "server selection error: " ^ message
  | Unsupported message -> "unsupported: " ^ message
  | Command err ->
      let code =
        match err.code with
        | Some c -> Printf.sprintf " (code %d)" c
        | None -> ""
      in
      let code_name =
        match err.code_name with
        | Some name -> Printf.sprintf " [%s]" name
        | None -> ""
      in
      let labels =
        match err.labels with
        | [] -> ""
        | labels -> " labels=[" ^ String.concat "," labels ^ "]"
      in
      "command error" ^ code ^ code_name ^ ": " ^ err.message ^ labels

let to_exn = function
  | Network message | Timeout message | Execution_timeout message
  | Protocol message | Authentication message | Server_selection message
  | Unsupported message ->
      Mongo_failed message
  | Command err -> Mongo_failed err.message

let raise_exn error = raise (to_exn error)

let command_to_exn doc =
  match of_command_reply doc with
  | Ok _ -> invalid_arg "command_to_exn called on success document"
  | Error err -> to_exn err
