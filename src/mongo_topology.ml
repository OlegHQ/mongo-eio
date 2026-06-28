type topology_type =
  | Single
  | ReplicaSetNoPrimary
  | ReplicaSetWithPrimary
  | Sharded
  | Unknown

type t = {
  topology_type : topology_type;
  set_name : string option;
  servers : Mongo_server_description.t list;
  stale : bool;
}

let empty =
  { topology_type = Unknown; set_name = None; servers = []; stale = false }

let from_server server =
  let topology_type =
    match server.Mongo_server_description.server_type with
    | Mongo_server_description.Standalone -> Single
    | Mongo_server_description.Mongos -> Sharded
    | Mongo_server_description.RSPrimary -> ReplicaSetWithPrimary
    | Mongo_server_description.RSSecondary | Mongo_server_description.RSArbiter ->
        ReplicaSetNoPrimary
    | _ -> Unknown
  in
  {
    topology_type;
    set_name = server.set_name;
    servers = [ server ];
    stale = false;
  }

let update_server topology (server : Mongo_server_description.t) =
  let replaced = ref false in
  let servers =
    List.map
      (fun (existing : Mongo_server_description.t) ->
        if existing.address = server.address then (
          replaced := true;
          server)
        else existing)
      topology.servers
  in
  let servers = if !replaced then servers else server :: servers in
  let has_primary =
    List.exists
      (fun s ->
        s.Mongo_server_description.server_type = Mongo_server_description.RSPrimary
        || s.server_type = Mongo_server_description.Mongos)
      servers
  in
  let topology_type =
    match topology.topology_type with
    | Unknown -> (from_server server).topology_type
    | Single -> Single
    | Sharded -> Sharded
    | _ ->
        if has_primary then ReplicaSetWithPrimary else ReplicaSetNoPrimary
  in
  { topology with servers; topology_type }

let snapshot topology =
  Printf.sprintf "topology=%s servers=[%s]"
    (match topology.topology_type with
    | Single -> "Single"
    | ReplicaSetNoPrimary -> "ReplicaSetNoPrimary"
    | ReplicaSetWithPrimary -> "ReplicaSetWithPrimary"
    | Sharded -> "Sharded"
    | Unknown -> "Unknown")
    (String.concat "; "
       (List.map Mongo_server_description.to_string topology.servers))
