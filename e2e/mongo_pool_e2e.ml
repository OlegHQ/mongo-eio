let env name default =
  match Sys.getenv_opt name with Some "" | None -> default | Some value -> value

let host = env "POSTER_MONGO_HOST" "oracle-vm"
let port = env "POSTER_MONGO_PORT" "27017" |> int_of_string
let max_pool_size = env "MONGO_POOL_E2E_MAX_POOL_SIZE" "4" |> int_of_string
let min_pool_size = env "MONGO_POOL_E2E_MIN_POOL_SIZE" "2" |> int_of_string
let workers = env "MONGO_POOL_E2E_WORKERS" "100" |> int_of_string

let db =
  Printf.sprintf "poster_pool_e2e_%d_%d" (Unix.getpid ()) (Random.bits ())

let collection = "pool_stress"

let doc fields =
  List.fold_right
    (fun (name, element) acc -> Bson.add_element name element acc)
    fields Bson.empty

let string name value = (name, Bson.create_string value)
let int32 name value = (name, Bson.create_int32 (Int32.of_int value))
let bool name value = (name, Bson.create_boolean value)

let assert_true label condition =
  if condition then Printf.printf "PASS %s\n%!" label
  else failwith ("FAIL " ^ label)

let ( let* ) result f = match result with Ok value -> f value | Error err -> Error err

let worker_flow pool i =
  let id = Printf.sprintf "worker_%03d" i in
  Mongo_pool.with_connection pool (fun conn ->
      let initial =
        doc [ string "id" id; int32 "worker" i; string "state" "new" ]
      in
      let filter = doc [ string "id" id ] in
      let update =
        doc
          [
            ( "$set",
              Bson.create_doc_element
                (doc [ string "state" "updated"; bool "done" true ]) );
          ]
      in
      let* insert_result = Mongo_crud.insert_one conn ~db ~collection initial in
      let* () =
        if insert_result.inserted_count = 1 then Ok ()
        else Error (Mongo_error.Protocol ("insert count mismatch " ^ id))
      in
      let* found = Mongo_crud.find_one conn ~db ~collection filter in
      let* () =
        match found with
        | Some _ -> Ok ()
        | None -> Error (Mongo_error.Protocol ("missing inserted doc " ^ id))
      in
      let* update_result =
        Mongo_crud.update_one conn ~db ~collection ~upsert:false filter update
      in
      let* () =
        if update_result.matched_count = 1 then Ok ()
        else Error (Mongo_error.Protocol ("update count mismatch " ^ id))
      in
      let* updated = Mongo_crud.find_one conn ~db ~collection filter in
      let* () =
        match updated with
        | Some doc when Bson.get_boolean (Bson.get_element "done" doc) -> Ok ()
        | Some _ -> Error (Mongo_error.Protocol ("doc not updated " ^ id))
        | None -> Error (Mongo_error.Protocol ("missing updated doc " ^ id))
      in
      let* delete_result = Mongo_crud.delete_one conn ~db ~collection filter in
      if delete_result.deleted_count = 1 then Ok ()
      else Error (Mongo_error.Protocol ("delete count mismatch " ^ id)))

let record_error mutex errors message =
  Mutex.lock mutex;
  errors := message :: !errors;
  Mutex.unlock mutex

let () =
  Random.self_init ();
  let config =
    {
      (Mongo_config.default ~host ~port ~database:db ())
      with
      max_pool_size;
      min_pool_size;
      wait_queue_timeout_ms = 30_000;
      socket_timeout_ms = Some 10_000;
    }
  in
  let pool = Mongo_pool.create config in
  let errors = ref [] in
  let error_mutex = Mutex.create () in
  Fun.protect
    ~finally:(fun () ->
      let cleanup =
        Mongo_pool.with_connection pool (fun conn ->
            Mongo_connection.run_command conn db
              [ ("dropDatabase", Bson.create_int32 1l) ]
            |> Result.map (fun _ -> ()))
      in
      (match cleanup with Ok () | Error _ -> ());
      Mongo_pool.close pool)
    (fun () ->
      assert_true "pool warmed minPoolSize"
        (Mongo_pool.idle_connections pool >= min min_pool_size max_pool_size);
      let threads =
        List.init workers (fun i ->
            Thread.create
              (fun () ->
                match worker_flow pool i with
                | Ok () -> ()
                | Error err ->
                    record_error error_mutex errors (Mongo_error.to_string err))
              ())
      in
      List.iter Thread.join threads;
      (match !errors with
      | [] -> assert_true "pool concurrent CRUD workers" true
      | messages -> failwith ("FAIL pool worker errors: " ^ String.concat "; " messages));
      assert_true "pool peak respects maxPoolSize"
        (Mongo_pool.peak_connections pool <= max_pool_size);
      assert_true "pool returned connections are idle"
        (Mongo_pool.total_connections pool = Mongo_pool.idle_connections pool);
      assert_true "pool reused connections under concurrency"
        (Mongo_pool.peak_connections pool <= max_pool_size && Mongo_pool.peak_connections pool > 1))
