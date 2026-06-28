type seedlist = {
  hosts : (string * int) list;
  txt_options : string option;
}

let error message = Error (Mongo_error.Protocol message)

let domain_name host =
  match Domain_name.of_string host with
  | Ok name -> Ok name
  | Error (`Msg message) -> error ("invalid SRV hostname: " ^ message)

let host_name host =
  match domain_name host with
  | Error _ as err -> err
  | Ok name -> (
      match Domain_name.host name with
      | Ok host -> Ok host
      | Error (`Msg message) -> error ("invalid SRV hostname: " ^ message))

let service_name service host =
  let name = Printf.sprintf "_%s._tcp.%s" service host in
  match Domain_name.of_string name with
  | Ok name -> (
      match Domain_name.service name with
      | Ok service -> Ok service
      | Error (`Msg message) -> error ("invalid SRV service name: " ^ message))
  | Error (`Msg message) -> error ("invalid SRV service name: " ^ message)

let label_count name = Domain_name.count_labels name

let parent_domain name =
  if label_count name >= 3 then Domain_name.drop_label name else Ok name

let validate_target ~source target =
  let target_raw = Domain_name.raw target in
  match parent_domain source with
  | Error (`Msg message) -> error message
  | Ok parent ->
      if not (Domain_name.is_subdomain ~subdomain:target_raw ~domain:parent)
      then
        error
          (Printf.sprintf
             "SRV target %s does not share parent domain with %s"
             (Domain_name.to_string target)
             (Domain_name.to_string source))
      else if
        label_count source < 3 && label_count target_raw <= label_count source
      then
        error
          (Printf.sprintf
             "SRV target %s must have more domain labels than %s"
             (Domain_name.to_string target)
             (Domain_name.to_string source))
      else Ok ()

let shuffle hosts =
  let array = Array.of_list hosts in
  for i = Array.length array - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = array.(i) in
    array.(i) <- array.(j);
    array.(j) <- tmp
  done;
  Array.to_list array

let limit_hosts ~max_hosts hosts =
  if max_hosts = 0 || max_hosts >= List.length hosts then hosts
  else
    shuffle hosts
    |> List.filteri (fun i _ -> i < max_hosts)

let srv_hosts ~source records =
  Dns.Rr_map.Srv_set.to_list records
  |> List.fold_left
       (fun acc srv ->
         match acc with
         | Error _ as err -> err
         | Ok hosts -> (
             match validate_target ~source srv.Dns.Srv.target with
             | Error _ as err -> err
             | Ok () ->
                 Ok
                   (( Domain_name.to_string srv.target,
                      srv.port )
                   :: hosts)))
       (Ok [])
  |> Result.map List.rev

let parse_srv_response ~source ~max_hosts = function
  | _, records -> (
      match srv_hosts ~source records with
      | Error _ as err -> err
      | Ok [] -> error "SRV lookup returned no hosts"
      | Ok hosts -> Ok (limit_hosts ~max_hosts hosts))

let parse_txt_response = function
  | None -> Ok None
  | Some (_, records) -> (
      match Dns.Rr_map.Txt_set.to_list records with
      | [] -> Ok None
      | [ value ] -> Ok (Some value)
      | _ -> error "multiple TXT records are not supported for mongodb+srv")

let dns_error name = function
  | `Msg message ->
      Mongo_error.Protocol
        (Printf.sprintf "DNS lookup failed for %s: %s" name message)
  | `No_data _ -> Mongo_error.Protocol ("DNS lookup returned no data for " ^ name)
  | `No_domain _ -> Mongo_error.Protocol ("DNS name does not exist: " ^ name)

let resolve_txt resolver source =
  match Dns_client_unix.get_resource_record resolver Dns.Rr_map.Txt source with
  | Ok txt -> Ok (Some txt)
  | Error (`No_data _) | Error (`No_domain _) -> Ok None
  | Error err -> Error (dns_error (Domain_name.to_string source) err)

let resolve ?(service = "mongodb") ?(max_hosts = 0) hostname =
  match (host_name hostname, service_name service hostname) with
  | Error err, _ | _, Error err -> Error err
  | Ok source, Ok query ->
      Random.self_init ();
      let resolver = Dns_client_unix.create () in
      let srv_result =
        Dns_client_unix.get_resource_record resolver Dns.Rr_map.Srv query
      in
      let txt_result = resolve_txt resolver source in
      (match (srv_result, txt_result) with
      | Error err, _ -> Error (dns_error (Domain_name.to_string query) err)
      | Ok _, Error err -> Error err
      | Ok srv, Ok txt -> (
          match parse_srv_response ~source:(Domain_name.raw source) ~max_hosts srv with
          | Error _ as err -> err
          | Ok hosts -> (
              match parse_txt_response txt with
              | Error _ as err -> err
              | Ok txt_options -> Ok { hosts; txt_options })))

let resolve_srv hostname =
  match resolve hostname with
  | Ok result -> Ok result.hosts
  | Error err -> Error err
