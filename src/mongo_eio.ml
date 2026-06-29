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

let delete_one ~domain_mgr client query =
  run_blocking ~domain_mgr (fun () -> Mongo.delete_one client query)

let delete_all ~domain_mgr client query =
  run_blocking ~domain_mgr (fun () -> Mongo.delete_all client query)

let ensure_simple_index ?options ~domain_mgr client field =
  run_blocking ~domain_mgr (fun () -> Mongo.ensure_simple_index ?options client field)

let find ?skip ~domain_mgr client =
  run_blocking ~domain_mgr (fun () -> Mongo.find ?skip client)

let find_one ?skip ~domain_mgr client =
  run_blocking ~domain_mgr (fun () -> Mongo.find_one ?skip client)

let find_q ?skip ~domain_mgr client query =
  run_blocking ~domain_mgr (fun () -> Mongo.find_q ?skip client query)

type direct_client = {
  pool : Mongo_pool.t;
  mutable closed : bool;
}

let close_direct client =
  if not client.closed then (
    client.closed <- true;
    Mongo_pool.close client.pool)

let connect ~sw ~net:_ ~clock:_ ~config =
  let client = { pool = Mongo_pool.create config; closed = false } in
  Eio.Switch.on_release sw (fun () -> close_direct client);
  Ok client

let with_direct_client ~sw ~net ~clock ~config f =
  match connect ~sw ~net ~clock ~config with
  | Error _ as err -> err
  | Ok client ->
      Fun.protect ~finally:(fun () -> close_direct client) (fun () -> f client)

let direct_run_command ?session ?command_event_handler client db fields =
  Mongo_pool.run_command ?session ?command_event_handler client.pool db fields

let direct_with_connection client f = Mongo_pool.with_connection client.pool f

let direct_find client ~db ~collection opts =
  direct_with_connection client (fun conn ->
      Mongo_crud.find conn ~db ~collection opts)

let direct_find_one client ~db ~collection filter =
  direct_with_connection client (fun conn ->
      Mongo_crud.find_one conn ~db ~collection filter)

let direct_insert_one ?write_concern client ~db ~collection doc =
  direct_with_connection client (fun conn ->
      Mongo_crud.insert_one ?write_concern conn ~db ~collection doc)

let direct_insert_many ?options client ~db ~collection docs =
  direct_with_connection client (fun conn ->
      Mongo_crud.insert_many ?options conn ~db ~collection docs)

let direct_update_one ?write_concern client ~db ~collection ~upsert selector
    update_doc =
  direct_with_connection client (fun conn ->
      Mongo_crud.update_one ?write_concern conn ~db ~collection ~upsert selector
        update_doc)

let direct_update_many ?write_concern client ~db ~collection ~upsert selector
    update_doc =
  direct_with_connection client (fun conn ->
      Mongo_crud.update_many ?write_concern conn ~db ~collection ~upsert selector
        update_doc)

let direct_delete_one ?write_concern client ~db ~collection selector =
  direct_with_connection client (fun conn ->
      Mongo_crud.delete_one ?write_concern conn ~db ~collection selector)

let direct_delete_many ?write_concern client ~db ~collection selector =
  direct_with_connection client (fun conn ->
      Mongo_crud.delete_many ?write_concern conn ~db ~collection selector)

let direct_ensure_simple_index client ~db ~collection ~field options =
  direct_with_connection client (fun conn ->
      Mongo_index.ensure_simple_index conn ~db ~collection field options)

let direct_count_documents client ~db ~collection ?query () =
  direct_with_connection client (fun conn ->
      Mongo_crud.count_documents conn ~db ~collection ?query ())

let direct_estimated_document_count client ~db ~collection =
  direct_with_connection client (fun conn ->
      Mongo_crud.estimated_document_count conn ~db ~collection)
