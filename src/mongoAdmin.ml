open MongoUtils;;

exception MongoAdmin_failed of string;;

type t = Mongo.t;;

type cmd = { name: string};;

let admin_db_name = "admin";;
let admin_collection_name = "$cmd";;

let get_db_name = Mongo.get_db_name;;
let get_collection_name = Mongo.get_collection_name;;
let get_ip = Mongo.get_ip;;
let get_port = Mongo.get_port;;
let get_file_descr = Mongo. get_file_descr;;

let wrap_mongo f arg =
  try f arg with
  | Mongo.Mongo_failed message -> raise (MongoAdmin_failed message)
  | Mongo_error.Mongo_failed message -> raise (MongoAdmin_failed message)
  | Unix.Unix_error (e, _, _) -> raise (MongoAdmin_failed (Unix.error_message e));;

let create ip port  = Mongo.create ip port admin_db_name admin_collection_name;;
let create_local_default () = create "127.0.0.1" 27017;;

let destroy a = Mongo.destroy a;;

let get_request_id = cur_timestamp;;

let create_cmd name = { name };;

let send_cmd (a, cmd) =
  Mongo_command.run_exn ~db:admin_db_name ~request_id:(get_request_id ())
    (Mongo.get_file_descr a)
    [ (cmd.name, Bson.create_int32 1l) ]
  |> fun doc -> MongoReply.create [ doc ];;

let hello a = wrap_mongo send_cmd (a, create_cmd "hello");;
let listDatabases a = wrap_mongo send_cmd (a, create_cmd "listDatabases");;
let buildInfo a = wrap_mongo send_cmd (a, create_cmd "buildInfo");;
let collStats a = wrap_mongo send_cmd (a, create_cmd "collStats");;
let connPoolStats a = wrap_mongo send_cmd (a, create_cmd "connPoolStats");;
let cursorInfo a = wrap_mongo send_cmd (a, create_cmd "cursorInfo");;
let getCmdLineOpts a = wrap_mongo send_cmd (a, create_cmd "getCmdLineOpts");;
let hostInfo a = wrap_mongo send_cmd (a, create_cmd "hostInfo");;
let listCommands a = wrap_mongo send_cmd (a, create_cmd "listCommands");;
let serverStatus a = wrap_mongo send_cmd (a, create_cmd "serverStatus");;
