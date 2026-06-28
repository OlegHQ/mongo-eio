open Alcotest

let server ?(rtt = None) ?(last_update = 0.0) ?last_write_date ?(tags = [])
    address server_type =
  {
    Mongo_server_description.address;
    server_type;
    round_trip_time_ms = rtt;
    last_update;
    last_write_date;
    tags;
    max_wire_version = Mongo_config.max_supported_wire_version;
    set_name = Some "rs0";
    primary = None;
    error = None;
  }

let topology servers topology_type =
  {
    Mongo_topology.topology_type;
    set_name = Some "rs0";
    servers;
    stale = false;
  }

let check_selected label expected selected =
  check int label expected (List.length selected)

let contains_substring haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop index =
    if needle_len = 0 then true
    else if index + needle_len > haystack_len then false
    else if String.sub haystack index needle_len = needle then true
    else loop (index + 1)
  in
  loop 0

let test_unknown_update_replaces_primary_and_reports_error () =
  let primary =
    server ("primary", 27017) Mongo_server_description.RSPrimary
  in
  let unknown =
    {
      primary with
      server_type = Mongo_server_description.Unknown;
      error = Some "timeout: socket read timed out";
    }
  in
  let topo =
    Mongo_topology.update_server
      (topology [ primary ] Mongo_topology.ReplicaSetWithPrimary)
      unknown
  in
  check bool "primary removed from selection" true
    (Mongo_server_select.suitable topo Mongo_config.Primary = []);
  check bool "topology no primary" true
    (topo.Mongo_topology.topology_type = Mongo_topology.ReplicaSetNoPrimary);
  check bool "snapshot includes monitor error" true
    (contains_substring (Mongo_topology.snapshot topo)
       "timeout: socket read timed out")

let test_replica_set_read_preferences () =
  let primary =
    server ("primary", 27017) Mongo_server_description.RSPrimary
  in
  let secondary =
    server ("secondary", 27017) Mongo_server_description.RSSecondary
  in
  let topo =
    topology [ primary; secondary ] Mongo_topology.ReplicaSetWithPrimary
  in
  check_selected "primary read" 1
    (Mongo_server_select.suitable topo Mongo_config.Primary);
  check string "primary host" "primary"
    (fst
       (List.hd
          (Mongo_server_select.suitable topo Mongo_config.Primary))
         .Mongo_server_description.address);
  check_selected "secondary read" 1
    (Mongo_server_select.suitable topo Mongo_config.Secondary);
  check string "secondary host" "secondary"
    (fst
       (List.hd
          (Mongo_server_select.suitable topo Mongo_config.Secondary))
         .Mongo_server_description.address);
  check_selected "nearest includes both" 2
    (Mongo_server_select.suitable topo Mongo_config.Nearest)

let test_preferred_fallbacks () =
  let primary =
    server ("primary", 27017) Mongo_server_description.RSPrimary
  in
  let secondary =
    server ("secondary", 27017) Mongo_server_description.RSSecondary
  in
  let no_primary =
    topology [ secondary ] Mongo_topology.ReplicaSetNoPrimary
  in
  let no_secondary =
    topology [ primary ] Mongo_topology.ReplicaSetWithPrimary
  in
  check string "primary preferred fallback" "secondary"
    (fst
       (List.hd
          (Mongo_server_select.suitable no_primary
             Mongo_config.PrimaryPreferred))
         .Mongo_server_description.address);
  check string "secondary preferred fallback" "primary"
    (fst
       (List.hd
          (Mongo_server_select.suitable no_secondary
             Mongo_config.SecondaryPreferred))
         .Mongo_server_description.address)

let test_single_topology_ignores_read_preference () =
  let secondary =
    server ("direct-secondary", 27017) Mongo_server_description.RSSecondary
  in
  let topo = topology [ secondary ] Mongo_topology.Single in
  check_selected "single secondary is selectable" 1
    (Mongo_server_select.suitable topo Mongo_config.Primary)

let test_latency_window () =
  let fast =
    server ~rtt:(Some 5.0) ("fast", 27017)
      Mongo_server_description.RSSecondary
  in
  let slow =
    server ~rtt:(Some 30.5) ("slow", 27017)
      Mongo_server_description.RSSecondary
  in
  let topo =
    topology [ fast; slow ] Mongo_topology.ReplicaSetNoPrimary
  in
  let selected = Mongo_server_select.suitable topo Mongo_config.Secondary in
  check_selected "latency window" 1 selected;
  check string "fast host" "fast"
    (fst (List.hd selected).Mongo_server_description.address)

let test_configured_latency_window () =
  let fast =
    server ~rtt:(Some 5.0) ("fast", 27017)
      Mongo_server_description.RSSecondary
  in
  let slow =
    server ~rtt:(Some 30.5) ("slow", 27017)
      Mongo_server_description.RSSecondary
  in
  let topo =
    topology [ fast; slow ] Mongo_topology.ReplicaSetNoPrimary
  in
  check_selected "configured latency window" 2
    (Mongo_server_select.suitable ~local_threshold_ms:30 topo
       Mongo_config.Secondary)

let test_tag_sets_use_first_matching_set () =
  let east =
    server ~tags:[ ("dc", "ny"); ("rack", "1") ] ("east", 27017)
      Mongo_server_description.RSSecondary
  in
  let west =
    server ~tags:[ ("dc", "sf") ] ("west", 27017)
      Mongo_server_description.RSSecondary
  in
  let topo =
    topology [ east; west ] Mongo_topology.ReplicaSetNoPrimary
  in
  let selected =
    Mongo_server_select.suitable
      ~tag_sets:[ [ ("dc", "la") ]; [ ("dc", "ny") ] ]
      topo Mongo_config.Secondary
  in
  check_selected "tagged secondary" 1 selected;
  check string "east host" "east"
    (fst (List.hd selected).Mongo_server_description.address)

let test_empty_tag_set_matches_all () =
  let east =
    server ~tags:[ ("dc", "ny") ] ("east", 27017)
      Mongo_server_description.RSSecondary
  in
  let west =
    server ~tags:[ ("dc", "sf") ] ("west", 27017)
      Mongo_server_description.RSSecondary
  in
  let topo =
    topology [ east; west ] Mongo_topology.ReplicaSetNoPrimary
  in
  check_selected "empty tag set" 2
    (Mongo_server_select.suitable ~tag_sets:[ [] ] topo
       Mongo_config.Secondary)

let test_max_staleness_with_primary () =
  let primary =
    server ~last_update:110.0 ~last_write_date:100.0 ("primary", 27017)
      Mongo_server_description.RSPrimary
  in
  let fresh =
    server ~last_update:110.0 ~last_write_date:95.0 ("fresh", 27017)
      Mongo_server_description.RSSecondary
  in
  let stale =
    server ~last_update:110.0 ~last_write_date:70.0 ("stale", 27017)
      Mongo_server_description.RSSecondary
  in
  let topo =
    topology [ primary; fresh; stale ] Mongo_topology.ReplicaSetWithPrimary
  in
  let selected =
    Mongo_server_select.suitable ~max_staleness_seconds:20 topo
      Mongo_config.Secondary
  in
  check_selected "fresh secondary" 1 selected;
  check string "fresh host" "fresh"
    (fst (List.hd selected).Mongo_server_description.address)

let test_max_staleness_without_primary () =
  let fresh =
    server ~last_update:110.0 ~last_write_date:100.0 ("fresh", 27017)
      Mongo_server_description.RSSecondary
  in
  let stale =
    server ~last_update:110.0 ~last_write_date:50.0 ("stale", 27017)
      Mongo_server_description.RSSecondary
  in
  let topo =
    topology [ fresh; stale ] Mongo_topology.ReplicaSetNoPrimary
  in
  let selected =
    Mongo_server_select.suitable ~max_staleness_seconds:20 topo
      Mongo_config.Secondary
  in
  check_selected "fresh secondary without primary" 1 selected;
  check string "fresh host without primary" "fresh"
    (fst (List.hd selected).Mongo_server_description.address)

let test_select_with_timeout_waits_for_updated_topology () =
  let primary =
    server ("primary", 27017) Mongo_server_description.RSPrimary
  in
  let unknown =
    topology [] Mongo_topology.Unknown
  in
  let ready =
    topology [ primary ] Mongo_topology.ReplicaSetWithPrimary
  in
  let current_time = ref 0.0 in
  let polls = ref 0 in
  let topology () =
    incr polls;
    if !polls < 2 then unknown else ready
  in
  let sleep seconds = current_time := !current_time +. seconds in
  match
    Mongo_server_select.select_with_timeout ~min_heartbeat_frequency_ms:10
      ~now:(fun () -> !current_time) ~sleep ~timeout_ms:100 ~topology
      Mongo_config.Primary
  with
  | Ok selected ->
      check string "selected host" "primary"
        (fst selected.Mongo_server_description.address);
      check int "polled twice" 2 !polls
  | Error err -> fail (Mongo_error.to_string err)

let test_select_with_timeout_reports_snapshot () =
  let secondary =
    server ("secondary", 27017) Mongo_server_description.RSSecondary
  in
  let topo =
    topology [ secondary ] Mongo_topology.ReplicaSetNoPrimary
  in
  let current_time = ref 0.0 in
  let sleeps = ref 0 in
  let sleep seconds =
    incr sleeps;
    current_time := !current_time +. seconds
  in
  match
    Mongo_server_select.select_with_timeout ~min_heartbeat_frequency_ms:10
      ~now:(fun () -> !current_time) ~sleep ~timeout_ms:25
      ~topology:(fun () -> topo) Mongo_config.Primary
  with
  | Ok _ -> fail "expected server selection timeout"
  | Error (Mongo_error.Server_selection message) ->
      check bool "mentions timeout" true
        (contains_substring message "timed out after 25 ms");
      check bool "includes topology snapshot" true
        (contains_substring message "ReplicaSetNoPrimary");
      check bool "includes server address" true
        (contains_substring message "secondary:27017");
      check bool "slept at least once" true (!sleeps > 0)
  | Error err -> fail (Mongo_error.to_string err)

let () =
  run "mongo_server_select"
    [
      ( "selection",
        [
          test_case "replica set read preferences" `Quick
            test_replica_set_read_preferences;
          test_case "unknown update replaces primary" `Quick
            test_unknown_update_replaces_primary_and_reports_error;
          test_case "preferred fallbacks" `Quick test_preferred_fallbacks;
          test_case "single topology" `Quick
            test_single_topology_ignores_read_preference;
          test_case "latency window" `Quick test_latency_window;
          test_case "configured latency window" `Quick
            test_configured_latency_window;
          test_case "tag sets use first matching set" `Quick
            test_tag_sets_use_first_matching_set;
          test_case "empty tag set matches all" `Quick
            test_empty_tag_set_matches_all;
          test_case "max staleness with primary" `Quick
            test_max_staleness_with_primary;
          test_case "max staleness without primary" `Quick
            test_max_staleness_without_primary;
          test_case "selection timeout waits for topology update" `Quick
            test_select_with_timeout_waits_for_updated_topology;
          test_case "selection timeout reports snapshot" `Quick
            test_select_with_timeout_reports_snapshot;
        ] );
    ]
