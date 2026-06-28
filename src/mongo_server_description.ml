type server_type =
  | Standalone
  | Mongos
  | RSGhost
  | RSPrimary
  | RSSecondary
  | RSArbiter
  | Unknown

type t = {
  address : string * int;
  server_type : server_type;
  round_trip_time_ms : float option;
  last_update : float;
  last_write_date : float option;
  tags : (string * string) list;
  max_wire_version : int;
  set_name : string option;
  primary : (string * int) option;
  error : string option;
}

let from_hello address hello =
  let is_mongos =
    try Bson.get_string (Bson.get_element "msg" hello) = "isdbgrid"
    with _ -> false
  in
  let is_replica_set_ghost =
    try Bson.get_boolean (Bson.get_element "isreplicaset" hello)
    with _ -> false
  in
  let secondary =
    try Bson.get_boolean (Bson.get_element "secondary" hello) with _ -> false
  in
  let arbiter_only =
    try Bson.get_boolean (Bson.get_element "arbiterOnly" hello) with _ -> false
  in
  let is_primary =
    try Bson.get_boolean (Bson.get_element "isWritablePrimary" hello)
    with _ -> (
      try not secondary with _ -> true)
  in
  let server_type =
    if is_mongos then Mongos
    else if is_replica_set_ghost then RSGhost
    else if arbiter_only then RSArbiter
    else if is_primary then RSPrimary
    else if secondary then RSSecondary
    else if
      try ignore (Bson.get_element "setName" hello); true with _ -> false
    then Unknown
    else Standalone
  in
  let tags =
    try
      Bson.get_doc_element (Bson.get_element "tags" hello)
      |> Bson.all_elements
      |> List.filter_map (fun (key, element) ->
             try Some (key, Bson.get_string element) with _ -> None)
    with _ -> []
  in
  let last_write_date =
    try
      Bson.get_doc_element (Bson.get_element "lastWrite" hello)
      |> Bson.get_element "lastWriteDate"
      |> Bson.get_utc |> Int64.to_float |> fun ms -> Some (ms /. 1000.0)
    with _ -> None
  in
  {
    address;
    server_type;
    round_trip_time_ms = None;
    last_update = Unix.gettimeofday ();
    last_write_date;
    tags;
    max_wire_version =
      (try Int32.to_int (Bson.get_int32 (Bson.get_element "maxWireVersion" hello))
       with _ -> 0);
    set_name =
      (try Some (Bson.get_string (Bson.get_element "setName" hello))
       with _ -> None);
    primary = None;
    error = None;
  }

let is_writable server =
  match server.server_type with
  | RSPrimary | Standalone | Mongos -> true
  | _ -> false

let is_readable server =
  match server.server_type with
  | RSPrimary | RSSecondary | Standalone | Mongos -> true
  | _ -> false

let is_data_bearing server =
  match server.server_type with
  | RSPrimary | RSSecondary | Standalone | Mongos -> true
  | _ -> false

let to_string t =
  let base =
    Printf.sprintf "%s:%d (%s)"
    (fst t.address)
    (snd t.address)
    (match t.server_type with
    | Standalone -> "standalone"
    | Mongos -> "mongos"
    | RSGhost -> "rs-ghost"
    | RSPrimary -> "primary"
    | RSSecondary -> "secondary"
    | RSArbiter -> "arbiter"
    | _ -> "unknown")
  in
  match t.error with
  | None -> base
  | Some error -> base ^ " error=" ^ error
