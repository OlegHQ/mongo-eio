let retryable_read_commands = [ "find"; "aggregate"; "count"; "distinct" ]

let retryable_write_commands =
  [ "insert"; "update"; "delete"; "findAndModify" ]

let command_code = function
  | Mongo_error.Command { code; _ } -> code
  | _ -> None

let retryable_read_codes =
  [
    262;
    11600;
    11602;
    10107;
    13435;
    13436;
    189;
    134;
    91;
    7;
    6;
    89;
    9001;
    133;
    150;
    234;
    13388;
    13430;
    11601;
    91;
  ]

let retryable_network_error = function
  | Mongo_error.Network _ | Timeout _ -> true
  | _ -> false

let retryable_read_error err =
  retryable_network_error err
  ||
  match command_code err with
  | Some code -> List.mem code retryable_read_codes
  | None -> Mongo_error.has_label err "RetryableReadError"

let retryable_write_error err =
  retryable_network_error err || Mongo_error.has_label err "RetryableWriteError"

let lowercase cmd = String.lowercase_ascii cmd

let is_retryable_read cmd = List.mem (lowercase cmd) retryable_read_commands

let is_retryable_write cmd =
  List.exists
    (fun retryable -> String.lowercase_ascii retryable = lowercase cmd)
    retryable_write_commands

let list_length element =
  try Some (List.length (Bson.get_list element)) with _ -> None

let bool_field_default default name doc =
  try Bson.get_boolean (Bson.get_element name doc) with _ -> default

let int_field_default default name doc =
  try Some (Int32.to_int (Bson.get_int32 (Bson.get_element name doc)))
  with _ -> (
    try Some (Int64.to_int (Bson.get_int64 (Bson.get_element name doc)))
    with _ -> default)

let single_insert fields =
  match List.assoc_opt "documents" fields with
  | Some documents -> list_length documents = Some 1
  | None -> false

let retryable_update_spec element =
  try
    let spec = Bson.get_doc_element element in
    not (bool_field_default false "multi" spec)
  with _ -> false

let single_retryable_update fields =
  match List.assoc_opt "updates" fields with
  | Some updates -> (
      try
        match Bson.get_list updates with
        | [ spec ] -> retryable_update_spec spec
        | _ -> false
      with _ -> false)
  | None -> false

let retryable_delete_spec element =
  try
    let spec = Bson.get_doc_element element in
    int_field_default None "limit" spec = Some 1
  with _ -> false

let single_retryable_delete fields =
  match List.assoc_opt "deletes" fields with
  | Some deletes -> (
      try
        match Bson.get_list deletes with
        | [ spec ] -> retryable_delete_spec spec
        | _ -> false
      with _ -> false)
  | None -> false

let is_retryable_write_command fields =
  match Mongo_command.command_name fields with
  | Some cmd when lowercase cmd = "insert" -> single_insert fields
  | Some cmd when lowercase cmd = "update" -> single_retryable_update fields
  | Some cmd when lowercase cmd = "delete" -> single_retryable_delete fields
  | Some cmd when lowercase cmd = "findandmodify" -> true
  | _ -> false

let supports_implicit_session fields =
  match Mongo_command.command_name fields with
  | None -> false
  | Some cmd -> (
      match lowercase cmd with
      | "hello" | "ismaster" | "saslstart" | "saslcontinue" | "authenticate"
      | "getnonce" | "find" | "aggregate" | "getmore" | "killcursors" ->
          false
      | _ -> true)

let run_read config _cmd f =
  if not config.Mongo_config.retry_reads then f ()
  else
    match f () with
    | (Ok _ | Error _) as result -> (
        match result with
        | Error err when retryable_read_error err -> f ()
        | other -> other)

let run_write config _cmd f =
  if not config.Mongo_config.retry_writes then f ()
  else
    match f () with
    | (Ok _ | Error _) as result -> (
        match result with
        | Error err when retryable_write_error err -> f ()
        | other -> other)
