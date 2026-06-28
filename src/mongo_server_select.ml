let default_local_threshold_ms = 15

let filter_latency_window ~local_threshold_ms servers =
  let rtts =
    List.filter_map
      (fun (server : Mongo_server_description.t) -> server.round_trip_time_ms)
      servers
  in
  let local_threshold_ms = float_of_int local_threshold_ms in
  match rtts with
  | [] -> servers
  | first :: rest ->
      let fastest = List.fold_left min first rest in
      List.filter
        (fun (server : Mongo_server_description.t) ->
          match server.round_trip_time_ms with
          | None -> true
          | Some rtt -> rtt <= fastest +. local_threshold_ms)
        servers

let primaries servers =
  List.filter
    (fun (server : Mongo_server_description.t) ->
      server.server_type = Mongo_server_description.RSPrimary)
    servers

let secondaries servers =
  List.filter
    (fun (server : Mongo_server_description.t) ->
      server.server_type = Mongo_server_description.RSSecondary)
    servers

let data_bearing servers =
  List.filter Mongo_server_description.is_data_bearing servers

let sharded_servers servers =
  List.filter
    (fun (server : Mongo_server_description.t) ->
      server.server_type = Mongo_server_description.Mongos)
    servers

let first_nonempty preferred fallback =
  match preferred with [] -> fallback | servers -> servers

let heartbeat_frequency_seconds = 10.0

let max_last_write_date servers =
  servers
  |> List.filter_map (fun (server : Mongo_server_description.t) ->
         match server.server_type with
         | Mongo_server_description.RSSecondary -> server.last_write_date
         | _ -> None)
  |> function
  | [] -> None
  | first :: rest -> Some (List.fold_left max first rest)

let secondary_staleness_with_primary primary secondary =
  match
    ( primary.Mongo_server_description.last_write_date,
      secondary.Mongo_server_description.last_write_date )
  with
  | Some primary_last_write, Some secondary_last_write ->
      Some
        ((secondary.last_update -. secondary_last_write)
        -. (primary.last_update -. primary_last_write)
        +. heartbeat_frequency_seconds)
  | _ -> None

let secondary_staleness_without_primary max_secondary_last_write secondary =
  match secondary.Mongo_server_description.last_write_date with
  | None -> None
  | Some secondary_last_write ->
      Some
        (max_secondary_last_write -. secondary_last_write
        +. heartbeat_frequency_seconds)

let filter_max_staleness ?max_staleness_seconds ?primary all_secondaries servers
    =
  match max_staleness_seconds with
  | None -> servers
  | Some max_staleness_seconds ->
      let max_staleness = float_of_int max_staleness_seconds in
      let max_secondary_last_write = max_last_write_date all_secondaries in
      List.filter
        (fun (server : Mongo_server_description.t) ->
          match server.server_type with
          | Mongo_server_description.RSSecondary ->
              let staleness =
                match primary with
                | Some primary ->
                    secondary_staleness_with_primary primary server
                | None -> (
                    match max_secondary_last_write with
                    | None -> None
                    | Some last_write ->
                        secondary_staleness_without_primary last_write server)
              in
              (match staleness with
              | Some staleness -> staleness <= max_staleness
              | None -> false)
          | _ -> true)
        servers

let matches_tag_set tag_set (server : Mongo_server_description.t) =
  List.for_all
    (fun (key, value) -> List.assoc_opt key server.tags = Some value)
    tag_set

let filter_tag_sets tag_sets servers =
  match tag_sets with
  | [] -> servers
  | tag_sets ->
      List.find_map
        (fun tag_set ->
          let matching = List.filter (matches_tag_set tag_set) servers in
          match matching with [] -> None | _ -> Some matching)
        tag_sets
      |> Option.value ~default:[]

let suitable ?(local_threshold_ms = default_local_threshold_ms) ?(tag_sets = [])
    ?max_staleness_seconds
    (topo : Mongo_topology.t) read_preference =
  let selected =
    match topo.Mongo_topology.topology_type with
    | Mongo_topology.Single -> data_bearing topo.servers
    | Mongo_topology.Sharded -> sharded_servers topo.servers
    | Mongo_topology.ReplicaSetWithPrimary
    | Mongo_topology.ReplicaSetNoPrimary ->
        let primary = primaries topo.servers in
        let secondary = secondaries topo.servers in
        let primary_option =
          match primary with server :: _ -> Some server | [] -> None
        in
        let filter_staleness =
          filter_max_staleness ?max_staleness_seconds
            ?primary:primary_option secondary
        in
        (match read_preference with
        | Mongo_config.Primary -> primary
        | Mongo_config.PrimaryPreferred ->
            first_nonempty primary
              (secondary |> filter_staleness |> filter_tag_sets tag_sets)
        | Mongo_config.Secondary ->
            secondary |> filter_staleness |> filter_tag_sets tag_sets
        | Mongo_config.SecondaryPreferred ->
            first_nonempty
              (secondary |> filter_staleness |> filter_tag_sets tag_sets)
              primary
        | Mongo_config.Nearest ->
            primary @ secondary |> filter_staleness |> filter_tag_sets tag_sets)
    | Mongo_topology.Unknown -> []
  in
  filter_latency_window ~local_threshold_ms selected

let select ?local_threshold_ms ?tag_sets ?max_staleness_seconds
    (topo : Mongo_topology.t) read_preference =
  match
    suitable ?local_threshold_ms ?tag_sets ?max_staleness_seconds topo
      read_preference
  with
  | server :: _ -> Ok server
  | [] ->
      Error
        (Mongo_error.Server_selection
           (Printf.sprintf "no suitable server (%s)"
              (Mongo_topology.snapshot topo)))

let select_writable topo = select topo Mongo_config.Primary

let select_with_timeout ?(min_heartbeat_frequency_ms = 500) ~now ~sleep
    ~timeout_ms ~topology ?local_threshold_ms ?tag_sets ?max_staleness_seconds
    read_preference =
  let started_at = now () in
  let deadline = started_at +. (float_of_int timeout_ms /. 1000.) in
  let rec loop last_topology =
    match
      select ?local_threshold_ms ?tag_sets ?max_staleness_seconds last_topology
        read_preference
    with
    | Ok _ as selected -> selected
    | Error _ ->
        let remaining = deadline -. now () in
        if remaining <= 0. then
          Error
            (Mongo_error.Server_selection
               (Printf.sprintf
                  "server selection timed out after %d ms (%s)" timeout_ms
                  (Mongo_topology.snapshot last_topology)))
        else (
          sleep
            (min remaining
               (float_of_int min_heartbeat_frequency_ms /. 1000.));
          loop (topology ()))
  in
  loop (topology ())

let select_writable_with_timeout ?min_heartbeat_frequency_ms ~now ~sleep
    ~timeout_ms ~topology () =
  select_with_timeout ?min_heartbeat_frequency_ms ~now ~sleep ~timeout_ms
    ~topology Mongo_config.Primary
