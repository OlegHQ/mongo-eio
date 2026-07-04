type pooled = {
  connection : Mongo_connection.t;
  generation : int;
  checked_in_at : float;
}

type connector = Mongo_config.t -> (Mongo_connection.t, Mongo_error.t) result

type event =
  | Pool_created
  | Pool_closed
  | Pool_cleared
  | Connection_created
  | Connection_ready
  | Connection_closed of string
  | Checkout_started
  | Checkout_succeeded
  | Checkout_failed of string
  | Checkin_started
  | Checkin_succeeded

type t = {
  config : Mongo_config.t;
  connect : connector;
  event_handler : (event -> unit) option;
  mutable generation : int;
  mutable idle : pooled list;
  mutable total : int;
  mutable peak_total : int;
  mutable closed : bool;
  checked_out : (Unix.file_descr, int) Hashtbl.t;
  mutex : Mutex.t;
}

let emit pool event =
  match pool.event_handler with
  | None -> ()
  | Some handler -> (
      try handler event with _ -> ())

let with_lock mutex f =
  Mutex.lock mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock mutex) f

let max_reached pool =
  pool.config.max_pool_size > 0 && pool.total >= pool.config.max_pool_size

let idle_expired pool now pooled =
  match pool.config.max_idle_time_ms with
  | None | Some 0 -> false
  | Some ms ->
      let idle_ms = (now -. pooled.checked_in_at) *. 1000.0 in
      idle_ms > float_of_int ms

let prune_idle_locked pool =
  let now = Unix.gettimeofday () in
  let expired, fresh =
    List.partition (fun pooled -> idle_expired pool now pooled) pool.idle
  in
  pool.idle <- fresh;
  pool.total <- max 0 (pool.total - List.length expired);
  expired

let prune_idle pool =
  let expired = with_lock pool.mutex (fun () -> prune_idle_locked pool) in
  List.iter
    (fun pooled ->
      Mongo_connection.close pooled.connection;
      emit pool (Connection_closed "maxIdleTimeMS"))
    expired

let wait_queue_deadline pool =
  let timeout_ms =
    match (pool.config.timeout_ms, pool.config.wait_queue_timeout_ms) with
    | Some 0, wait_queue_timeout_ms -> wait_queue_timeout_ms
    | Some timeout_ms, wait_queue_timeout_ms when wait_queue_timeout_ms <= 0 ->
        timeout_ms
    | Some timeout_ms, wait_queue_timeout_ms -> min timeout_ms wait_queue_timeout_ms
    | None, wait_queue_timeout_ms -> wait_queue_timeout_ms
  in
  if timeout_ms <= 0 then None
  else Some (Unix.gettimeofday () +. (float_of_int timeout_ms /. 1000.0))

let wait_for_slot deadline =
  match deadline with
  | None ->
      Unix.sleepf 0.005;
      true
  | Some deadline ->
      let remaining = deadline -. Unix.gettimeofday () in
      if remaining <= 0.0 then false
      else (
        Unix.sleepf (min 0.005 remaining);
        true)

let reserve_new pool =
  let expired, reservation =
    with_lock pool.mutex (fun () ->
        let expired = prune_idle_locked pool in
        let reservation =
          if pool.closed then Error (Mongo_error.Network "connection pool is closed")
          else
            match pool.idle with
            | pooled :: idle ->
                pool.idle <- idle;
                Hashtbl.replace pool.checked_out
                  (Mongo_connection.file_descr pooled.connection)
                  pooled.generation;
                Ok (`Existing pooled.connection)
            | [] when not (max_reached pool) ->
                pool.total <- pool.total + 1;
                pool.peak_total <- max pool.peak_total pool.total;
                Ok (`Reserved pool.generation)
            | [] -> Ok `Wait
        in
        (expired, reservation))
  in
  List.iter
    (fun pooled ->
      Mongo_connection.close pooled.connection;
      emit pool (Connection_closed "maxIdleTimeMS"))
    expired;
  reservation

let finalize_new pool generation connection =
  with_lock pool.mutex (fun () ->
      if pool.closed || generation <> pool.generation then `Discard
      else (
        Hashtbl.replace pool.checked_out
          (Mongo_connection.file_descr connection)
          generation;
        `Use))

let unreserve pool =
  with_lock pool.mutex (fun () -> pool.total <- max 0 (pool.total - 1))

let min_pool_target config =
  if config.Mongo_config.min_pool_size <= 0 then 0
  else if config.max_pool_size > 0 then
    min config.min_pool_size config.max_pool_size
  else config.min_pool_size

let reserve_warmup pool target =
  let expired, reservation =
    with_lock pool.mutex (fun () ->
        let expired = prune_idle_locked pool in
        let reservation =
          if pool.closed then Error (Mongo_error.Network "connection pool is closed")
          else if pool.total >= target then Ok `Done
          else if max_reached pool then Ok `Done
          else (
            pool.total <- pool.total + 1;
            pool.peak_total <- max pool.peak_total pool.total;
            Ok (`Reserved pool.generation))
        in
        (expired, reservation))
  in
  List.iter
    (fun pooled ->
      Mongo_connection.close pooled.connection;
      emit pool (Connection_closed "maxIdleTimeMS"))
    expired;
  reservation

let finalize_warmup pool generation connection =
  with_lock pool.mutex (fun () ->
      if pool.closed || generation <> pool.generation then `Discard
      else (
        pool.idle <-
          { connection; generation; checked_in_at = Unix.gettimeofday () }
          :: pool.idle;
        `Use))

let rec ensure_min_pool_size pool =
  let target = min_pool_target pool.config in
  match reserve_warmup pool target with
  | Error err -> Error err
  | Ok `Done -> Ok ()
  | Ok (`Reserved generation) -> (
      emit pool Connection_created;
      match pool.connect pool.config with
      | Error err ->
          unreserve pool;
          emit pool (Connection_closed (Mongo_error.to_string err));
          Error err
      | Ok connection -> (
          match finalize_warmup pool generation connection with
          | `Use ->
              emit pool Connection_ready;
              ensure_min_pool_size pool
          | `Discard ->
              Mongo_connection.close connection;
              unreserve pool;
              emit pool (Connection_closed "stale generation");
              ensure_min_pool_size pool))

let create ?event_handler ?(connect = Mongo_connection.connect) config =
  let pool =
    {
      config;
      connect;
      event_handler;
      generation = 0;
      idle = [];
      total = 0;
      peak_total = 0;
      closed = false;
      checked_out = Hashtbl.create 16;
      mutex = Mutex.create ();
    }
  in
  emit pool Pool_created;
  ignore (ensure_min_pool_size pool);
  pool

let rec checkout_loop pool deadline =
  match reserve_new pool with
  | Error err ->
      emit pool (Checkout_failed (Mongo_error.to_string err));
      Error err
  | Ok (`Existing conn) ->
      emit pool Checkout_succeeded;
      Ok conn
  | Ok (`Reserved generation) -> (
      emit pool Connection_created;
      match pool.connect pool.config with
      | Ok connection -> (
          match finalize_new pool generation connection with
          | `Use ->
              emit pool Checkout_succeeded;
              Ok connection
          | `Discard ->
              Mongo_connection.close connection;
              unreserve pool;
              emit pool (Connection_closed "stale generation");
              checkout_loop pool deadline)
      | Error err ->
          unreserve pool;
          emit pool (Connection_closed (Mongo_error.to_string err));
          emit pool (Checkout_failed (Mongo_error.to_string err));
          Error err)
  | Ok `Wait ->
      if wait_for_slot deadline then checkout_loop pool deadline
      else (
        emit pool (Checkout_failed "pool wait queue timed out");
        Error (Mongo_error.Timeout "pool wait queue timed out"))

let checkout pool =
  emit pool Checkout_started;
  checkout_loop pool (wait_queue_deadline pool)

let close_and_forget pool conn =
  Mongo_connection.close conn;
  unreserve pool;
  emit pool (Connection_closed "closed")

let checkin pool conn =
  emit pool Checkin_started;
  let fd = Mongo_connection.file_descr conn in
  let should_close =
    with_lock pool.mutex (fun () ->
        let generation =
          match Hashtbl.find_opt pool.checked_out fd with
          | None -> pool.generation
          | Some generation ->
              Hashtbl.remove pool.checked_out fd;
              generation
        in
        if pool.closed || generation <> pool.generation then true
        else if List.length pool.idle >= pool.config.max_pool_size && pool.config.max_pool_size > 0 then true
        else (
          pool.idle <-
            { connection = conn; generation; checked_in_at = Unix.gettimeofday () }
            :: pool.idle;
          false))
  in
  if should_close then close_and_forget pool conn else emit pool Checkin_succeeded

let clear pool =
  let idle =
    with_lock pool.mutex (fun () ->
        let idle = pool.idle in
        pool.idle <- [];
        pool.total <- pool.total - List.length idle;
        pool.generation <- pool.generation + 1;
        idle)
  in
  emit pool Pool_cleared;
  List.iter
    (fun pooled ->
      Mongo_connection.close pooled.connection;
      emit pool (Connection_closed "pool cleared"))
    idle

let update_server_description pool
    (server : Mongo_server_description.t) =
  match (server.server_type, server.error) with
  | Unknown, Some _ -> clear pool
  | _ -> ()

let clears_pool = function
  | Mongo_error.Network _ | Timeout _ -> true
  | _ -> false

type retry_kind =
  | Not_retryable
  | Retry_read
  | Retry_write

let retryable_write_server (server : Mongo_connection.server_info) =
  server.max_wire_version >= 6
  && server.logical_session_timeout_minutes <> None
  && (server.set_name <> None || server.is_mongos)

let retryable_read_server (server : Mongo_connection.server_info) =
  server.max_wire_version >= 6

let command_retry_kind config (conn : Mongo_connection.t) fields =
  match Mongo_command.command_name fields with
  | None -> Not_retryable
  | Some command
    when config.Mongo_config.retry_reads
         && Mongo_retry.is_retryable_read command
         && retryable_read_server conn.server ->
      Retry_read
  | Some _
    when config.Mongo_config.retry_writes
         && Mongo_retry.is_retryable_write_command fields
         && retryable_write_server conn.server ->
      Retry_write
  | Some _ -> Not_retryable

let retry_session = function
  | Not_retryable -> None
  | Retry_read ->
      Some (Mongo_session.implicit_context (Mongo_session.create ()))
  | Retry_write ->
      Some (Mongo_session.command_context (Mongo_session.create ()))

let retryable_error kind err =
  match kind with
  | Not_retryable -> false
  | Retry_read -> Mongo_retry.retryable_read_error err
  | Retry_write -> Mongo_retry.retryable_write_error err

let operation_deadline config =
  match config.Mongo_config.timeout_ms with
  | Some timeout_ms when timeout_ms > 0 ->
      Some (Unix.gettimeofday () +. (float_of_int timeout_ms /. 1000.0))
  | Some _ | None -> None

let connection_with_remaining_timeout deadline (conn : Mongo_connection.t) =
  match deadline with
  | None -> Ok conn
  | Some deadline ->
      let remaining_ms =
        int_of_float (ceil ((deadline -. Unix.gettimeofday ()) *. 1000.0))
      in
      if remaining_ms <= 0 then
        Error (Mongo_error.Timeout "operation timeoutMS expired")
      else
        Ok
          {
            conn with
            config = { conn.Mongo_connection.config with timeout_ms = Some remaining_ms };
          }

let with_connection pool f =
  match checkout pool with
  | Error err -> Error err
  | Ok conn ->
      Fun.protect
        ~finally:(fun () -> checkin pool conn)
        (fun () ->
          match f conn with
          | Error err as result ->
              if clears_pool err then clear pool;
              result
          | Ok _ as result -> result)

let run_command ?session ?command_event_handler pool db fields =
  let first_kind = ref Not_retryable in
  let retry_session_ref = ref session in
  let deadline = operation_deadline pool.config in
  let first =
    with_connection pool (fun conn ->
        match connection_with_remaining_timeout deadline conn with
        | Error err -> Error err
        | Ok conn ->
        let kind =
          match session with
          | Some _ -> Not_retryable
          | None -> command_retry_kind pool.config conn fields
        in
        first_kind := kind;
        retry_session_ref := (
          match session with
          | Some _ -> session
          | None -> retry_session kind);
        Mongo_connection.run_command ?session:!retry_session_ref
          ?command_event_handler conn db fields)
  in
  match first with
  | Ok _ -> first
  | Error err when retryable_error !first_kind err ->
      with_connection pool (fun conn ->
          match connection_with_remaining_timeout deadline conn with
          | Error err -> Error err
          | Ok conn ->
          let kind = command_retry_kind pool.config conn fields in
          if kind = !first_kind then
            Mongo_connection.run_command ?session:!retry_session_ref
              ?command_event_handler conn db fields
          else Error err)
  | Error _ -> first

let close pool =
  let idle =
    with_lock pool.mutex (fun () ->
        pool.closed <- true;
        let idle = pool.idle in
        pool.idle <- [];
        pool.total <- pool.total - List.length idle;
        idle)
  in
  List.iter
    (fun pooled ->
      Mongo_connection.close pooled.connection;
      emit pool (Connection_closed "pool closed"))
    idle;
  emit pool Pool_closed

let total_connections pool = with_lock pool.mutex (fun () -> pool.total)
let idle_connections pool = with_lock pool.mutex (fun () -> List.length pool.idle)
let peak_connections pool = with_lock pool.mutex (fun () -> pool.peak_total)
let prune_idle_connections pool = prune_idle pool
