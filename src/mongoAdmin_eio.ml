type client = MongoAdmin.t

let run_blocking ~domain_mgr f = Eio.Domain_manager.run domain_mgr f

let create ~domain_mgr ~host ~port =
  run_blocking ~domain_mgr (fun () -> MongoAdmin.create host port)

let create_local_default ~domain_mgr () =
  run_blocking ~domain_mgr MongoAdmin.create_local_default

let destroy ~domain_mgr client =
  run_blocking ~domain_mgr (fun () -> MongoAdmin.destroy client)

let list_databases ~domain_mgr client =
  run_blocking ~domain_mgr (fun () -> MongoAdmin.listDatabases client)

let build_info ~domain_mgr client =
  run_blocking ~domain_mgr (fun () -> MongoAdmin.buildInfo client)

let server_status ~domain_mgr client =
  run_blocking ~domain_mgr (fun () -> MongoAdmin.serverStatus client)

