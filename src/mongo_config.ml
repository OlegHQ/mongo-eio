type tls =
  | Disabled
  | Enabled of {
      ca_file : string option;
      allow_invalid_certificates : bool;
      server_name : string option;
    }

type credentials = {
  username : string;
  password : string;
  auth_source : string option;
  auth_mechanism : [ `Scram_sha_256 | `Scram_sha_1 ] option;
}

type read_preference =
  | Primary
  | PrimaryPreferred
  | Secondary
  | SecondaryPreferred
  | Nearest

type read_preference_tag_set = (string * string) list

type write_concern = {
  w : [ `Majority | `Nodes of int | `Tag of string ] option;
  j : bool option;
  wtimeout_ms : int option;
}

type read_concern =
  | Local
  | Majority
  | Linearizable
  | Available
  | Snapshot
  | Custom of string

type t = {
  hosts : (string * int) list;
  database : string;
  credentials : credentials option;
  tls : tls;
  app_name : string option;
  replica_set : string option;
  direct_connection : bool;
  connect_timeout_ms : int;
  socket_timeout_ms : int option;
  timeout_ms : int option;
  server_selection_timeout_ms : int;
  local_threshold_ms : int;
  heartbeat_frequency_ms : int;
  max_pool_size : int;
  min_pool_size : int;
  wait_queue_timeout_ms : int;
  max_idle_time_ms : int option;
  retry_reads : bool;
  retry_writes : bool;
  read_preference : read_preference;
  read_preference_tags : read_preference_tag_set list;
  max_staleness_seconds : int option;
  read_concern : read_concern option;
  write_concern : write_concern option;
}

let default_port = 27017

let default ?(host = "127.0.0.1") ?(port = default_port) ?(database = "test") () =
  {
    hosts = [ (host, port) ];
    database;
    credentials = None;
    tls = Disabled;
    app_name = None;
    replica_set = None;
    direct_connection = false;
    connect_timeout_ms = 10_000;
    socket_timeout_ms = None;
    timeout_ms = None;
    server_selection_timeout_ms = 30_000;
    local_threshold_ms = 15;
    heartbeat_frequency_ms = 10_000;
    max_pool_size = 100;
    min_pool_size = 0;
    wait_queue_timeout_ms = 10_000;
    max_idle_time_ms = None;
    retry_reads = true;
    retry_writes = true;
    read_preference = Primary;
    read_preference_tags = [];
    max_staleness_seconds = None;
    read_concern = None;
    write_concern = None;
  }

let driver_name = "mongo-eio"
let driver_version = "0.2.0"

let client_metadata ?app_name () =
  let platform = Printf.sprintf "OCaml/%s" Sys.ocaml_version in
  let driver =
    Bson.add_element "name" (Bson.create_string driver_name)
      (Bson.add_element "version" (Bson.create_string driver_version) Bson.empty)
  in
  let os =
    Bson.add_element "type" (Bson.create_string Sys.os_type) Bson.empty
  in
  let doc =
    Bson.add_element "driver" (Bson.create_doc_element driver)
      (Bson.add_element "os" (Bson.create_doc_element os)
         (Bson.add_element "platform" (Bson.create_string platform) Bson.empty))
  in
  match app_name with
  | None -> doc
  | Some name ->
      Bson.add_element "application"
        (Bson.create_doc_element
           (Bson.add_element "name" (Bson.create_string name) Bson.empty))
        doc

let read_preference_to_bson = function
  | Primary -> Bson.create_string "primary"
  | PrimaryPreferred -> Bson.create_string "primaryPreferred"
  | Secondary -> Bson.create_string "secondary"
  | SecondaryPreferred -> Bson.create_string "secondaryPreferred"
  | Nearest -> Bson.create_string "nearest"

let document fields =
  List.fold_right
    (fun (name, element) doc -> Bson.add_element name element doc)
    fields Bson.empty

let read_preference_doc ?(tag_sets = []) ?max_staleness_seconds mode =
  let doc =
    document [ ("mode", read_preference_to_bson mode) ]
  in
  let doc =
    match tag_sets with
    | [] -> doc
    | tag_sets ->
        Bson.add_element "tags"
          (Bson.create_doc_element_list
             (List.map
                (fun tag_set ->
                  document
                    (List.map
                       (fun (key, value) -> (key, Bson.create_string value))
                       tag_set))
                tag_sets))
          doc
  in
  match max_staleness_seconds with
  | None -> doc
  | Some seconds ->
      Bson.add_element "maxStalenessSeconds"
        (Bson.create_int32 (Int32.of_int seconds))
        doc

let min_supported_wire_version = 0
let max_supported_wire_version = 21
