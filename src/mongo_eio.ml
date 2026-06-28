type client = Mongo.t

type config = {
  host : string;
  port : int;
  database : string;
  collection : string;
}

let default_port = 27017

let run_blocking ~domain_mgr f = Eio.Domain_manager.run domain_mgr f

let create ~domain_mgr { host; port; database; collection } =
  run_blocking ~domain_mgr (fun () -> Mongo.create host port database collection)

let destroy ~domain_mgr client =
  run_blocking ~domain_mgr (fun () -> Mongo.destroy client)

let with_client ~domain_mgr config f =
  let client = create ~domain_mgr config in
  Fun.protect ~finally:(fun () -> destroy ~domain_mgr client) (fun () -> f client)

let insert ~domain_mgr client docs =
  run_blocking ~domain_mgr (fun () -> Mongo.insert client docs)

let find ?skip ~domain_mgr client =
  run_blocking ~domain_mgr (fun () -> Mongo.find ?skip client)

let find_one ?skip ~domain_mgr client =
  run_blocking ~domain_mgr (fun () -> Mongo.find_one ?skip client)

let find_q ?skip ~domain_mgr client query =
  run_blocking ~domain_mgr (fun () -> Mongo.find_q ?skip client query)

