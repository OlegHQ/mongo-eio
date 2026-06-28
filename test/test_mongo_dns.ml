open Alcotest

let host name = Domain_name.of_string_exn name |> Domain_name.host_exn

let srv target port =
  {
    Dns.Srv.priority = 0;
    weight = 0;
    port;
    target = host target;
  }

let srv_records entries =
  List.fold_left
    (fun set entry -> Dns.Rr_map.Srv_set.add entry set)
    Dns.Rr_map.Srv_set.empty entries

let txt_records entries =
  List.fold_left
    (fun set entry -> Dns.Rr_map.Txt_set.add entry set)
    Dns.Rr_map.Txt_set.empty entries

let source name = Domain_name.of_string_exn name

let expect_protocol_error = function
  | Ok _ -> fail "expected protocol error"
  | Error (Mongo_error.Protocol _) -> ()
  | Error err -> fail (Mongo_error.to_string err)

let test_srv_parent_validation () =
  let response =
    (30l, srv_records [ srv "mongo1.example.com" 27018 ])
  in
  (match
     Mongo_dns.parse_srv_response ~source:(source "cluster.example.com")
       ~max_hosts:0 response
   with
  | Ok [ host, port ] ->
      check string "host" "mongo1.example.com" host;
      check int "port" 27018 port
  | Ok _ -> fail "expected one host"
  | Error err -> fail (Mongo_error.to_string err));
  expect_protocol_error
    (Mongo_dns.parse_srv_response ~source:(source "cluster.example.com")
       ~max_hosts:0
       (30l, srv_records [ srv "mongo1.other.com" 27017 ]))

let test_short_source_requires_extra_label () =
  expect_protocol_error
    (Mongo_dns.parse_srv_response ~source:(source "example.com") ~max_hosts:0
       (30l, srv_records [ srv "example.com" 27017 ]));
  match
    Mongo_dns.parse_srv_response ~source:(source "example.com") ~max_hosts:0
      (30l, srv_records [ srv "mongo1.example.com" 27017 ])
  with
  | Ok [ _ ] -> ()
  | Ok _ -> fail "expected one host"
  | Error err -> fail (Mongo_error.to_string err)

let test_srv_max_hosts () =
  match
    Mongo_dns.parse_srv_response ~source:(source "cluster.example.com")
      ~max_hosts:2
      ( 30l,
        srv_records
          [
            srv "mongo1.example.com" 27017;
            srv "mongo2.example.com" 27017;
            srv "mongo3.example.com" 27017;
          ] )
  with
  | Ok hosts -> check int "limited hosts" 2 (List.length hosts)
  | Error err -> fail (Mongo_error.to_string err)

let test_txt_records () =
  check (option string) "no txt" None
    (Result.get_ok (Mongo_dns.parse_txt_response None));
  check (option string) "one txt" (Some "authSource=admin")
    (Result.get_ok
       (Mongo_dns.parse_txt_response
          (Some (30l, txt_records [ "authSource=admin" ]))));
  expect_protocol_error
    (Mongo_dns.parse_txt_response
       (Some
          ( 30l,
            txt_records [ "authSource=admin"; "replicaSet=rs0" ] )))

let () =
  run "mongo_dns"
    [
      ( "srv",
        [
          test_case "parent validation" `Quick test_srv_parent_validation;
          test_case "short source extra label" `Quick
            test_short_source_requires_extra_label;
          test_case "srvMaxHosts limit" `Quick test_srv_max_hosts;
          test_case "txt records" `Quick test_txt_records;
        ] );
    ]
