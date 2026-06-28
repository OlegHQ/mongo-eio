type mechanism = Scram_sha_256 | Scram_sha_1

let mechanism_name = function
  | Scram_sha_256 -> "SCRAM-SHA-256"
  | Scram_sha_1 -> "SCRAM-SHA-1"

let to_hex s =
  String.fold_left (fun acc c -> acc ^ Printf.sprintf "%02x" (Char.code c)) "" s

let mongo_sha1_password ~username ~password =
  Digest.to_hex (Digest.string (username ^ ":mongo:" ^ password))

let hash_sha256 data =
  Digestif.SHA256.(to_raw_string (digest_string data))

let hash_sha1 data =
  Digestif.SHA1.(to_raw_string (digest_string data))

let hmac_sha256 ~key data =
  let block_size = 64 in
  let normalize k =
    if String.length k > block_size then hash_sha256 k else k
  in
  let key = normalize key in
  let key = key ^ String.make (block_size - String.length key) '\x00' in
  let inner =
    String.init block_size (fun i -> Char.chr (Char.code key.[i] lxor 0x36))
    ^ data
  in
  let outer =
    String.init block_size (fun i -> Char.chr (Char.code key.[i] lxor 0x5c))
    ^ hash_sha256 inner
  in
  hash_sha256 outer

let hmac_sha1 ~key data =
  let block_size = 64 in
  let normalize k = if String.length k > block_size then hash_sha1 k else k in
  let key = normalize key in
  let key = key ^ String.make (block_size - String.length key) '\x00' in
  let inner =
    String.init block_size (fun i -> Char.chr (Char.code key.[i] lxor 0x36))
    ^ data
  in
  let outer =
    String.init block_size (fun i -> Char.chr (Char.code key.[i] lxor 0x5c))
    ^ hash_sha1 inner
  in
  hash_sha1 outer

let xor_strings a b =
  String.init (String.length a) (fun i ->
      Char.chr (Char.code a.[i] lxor Char.code b.[i]))

let pbkdf2 mechanism ~password ~salt ~iterations ~length =
  let hmac key data =
    match mechanism with
    | Scram_sha_256 -> hmac_sha256 ~key data
    | Scram_sha_1 -> hmac_sha1 ~key data
  in
  let rec loop block result =
    if Bytes.length result >= length then Bytes.sub result 0 length
    else
      let block = block + 1 in
      let salt_block =
        salt
        ^ String.init 4 (fun i ->
              Char.chr ((block lsr ((3 - i) * 8)) land 0xFF))
      in
      let u1 = hmac password salt_block in
      let rec iter count previous acc =
        if count >= iterations then acc
        else
          let next = hmac password previous in
          iter (count + 1) next (xor_strings acc next)
      in
      let block_key = iter 1 u1 u1 in
      let result = Bytes.of_string (Bytes.to_string result ^ block_key) in
      loop block result
  in
  loop 0 (Bytes.create 0)

let salted_password mechanism ~password ~salt ~iterations =
  let length = match mechanism with Scram_sha_256 -> 32 | Scram_sha_1 -> 20 in
  pbkdf2 mechanism ~password ~salt ~iterations ~length

let client_key mechanism salted =
  match mechanism with
  | Scram_sha_256 -> hmac_sha256 ~key:salted "Client Key"
  | Scram_sha_1 -> hmac_sha1 ~key:salted "Client Key"

let server_key mechanism salted =
  match mechanism with
  | Scram_sha_256 -> hmac_sha256 ~key:salted "Server Key"
  | Scram_sha_1 -> hmac_sha1 ~key:salted "Server Key"

let stored_key mechanism client_key =
  match mechanism with
  | Scram_sha_256 -> hash_sha256 client_key
  | Scram_sha_1 -> hash_sha1 client_key

module Base64 = struct
  let encode s =
    let table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
    let len = String.length s in
    let rec chunk i acc =
      if i >= len then acc
      else
        let b0 = Char.code s.[i] in
        let b1 = if i + 1 < len then Char.code s.[i + 1] else 0 in
        let b2 = if i + 2 < len then Char.code s.[i + 2] else 0 in
        let emit idx = String.make 1 table.[idx] in
        let acc = acc ^ emit (b0 lsr 2) in
        let acc = acc ^ emit (((b0 land 0x3) lsl 4) lor (b1 lsr 4)) in
        let acc =
          if i + 1 < len then acc ^ emit (((b1 land 0xF) lsl 2) lor (b2 lsr 6))
          else acc ^ "="
        in
        let acc = if i + 2 < len then acc ^ emit (b2 land 0x3F) else acc ^ "=" in
        chunk (i + 3) acc
    in
    chunk 0 ""

  let decode input =
    let pad =
      match String.length input mod 4 with
      | 2 -> input ^ "=="
      | 3 -> input ^ "="
      | _ -> input
    in
    let table = Array.make 256 (-1) in
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    |> String.iteri (fun i c -> table.(Char.code c) <- i);
    let decode_char c =
      let v = table.(Char.code c) in
      if v < 0 then invalid_arg "invalid base64";
      v
    in
    let rec go i acc =
      if i >= String.length pad then acc
      else if pad.[i] = '=' then acc
      else
        let c0 = decode_char pad.[i] in
        let c1 = decode_char pad.[i + 1] in
        let c2 =
          if i + 2 < String.length pad && pad.[i + 2] <> '=' then
            decode_char pad.[i + 2]
          else 0
        in
        let c3 =
          if i + 3 < String.length pad && pad.[i + 3] <> '=' then
            decode_char pad.[i + 3]
          else 0
        in
        let b0 = (c0 lsl 2) lor (c1 lsr 4) in
        let acc = acc ^ String.make 1 (Char.chr b0) in
        let acc =
          if pad.[i + 2] <> '=' then
            acc
            ^ String.make 1 (Char.chr (((c1 land 0xF) lsl 4) lor (c2 lsr 2)))
          else acc
        in
        let acc =
          if pad.[i + 3] <> '=' then
            acc ^ String.make 1 (Char.chr (((c2 land 0x3) lsl 6) lor c3))
          else acc
        in
        go (i + 4) acc
    in
    go 0 ""
end

let client_proof mechanism client_key auth_message =
  let stored = stored_key mechanism client_key in
  let client_signature =
    match mechanism with
    | Scram_sha_256 -> hmac_sha256 ~key:stored auth_message
    | Scram_sha_1 -> hmac_sha1 ~key:stored auth_message
  in
  Base64.encode (xor_strings client_key client_signature)

let server_signature mechanism server_key auth_message =
  let sig_ =
    match mechanism with
    | Scram_sha_256 -> hmac_sha256 ~key:server_key auth_message
    | Scram_sha_1 -> hmac_sha1 ~key:server_key auth_message
  in
  Base64.encode sig_

let parse_server_first payload =
  let parts = String.split_on_char ',' payload in
  let find prefix =
    try
      let part = List.find (String.starts_with ~prefix) parts in
      Some (String.sub part (String.length prefix) (String.length part - String.length prefix))
    with _ -> None
  in
  match (find "r=", find "s=", find "i=") with
  | Some nonce, Some salt_b64, Some iter_s -> (
      let salt = Base64.decode salt_b64 in
      Ok (nonce, salt, int_of_string iter_s))
  | _ -> Error "invalid server-first message"

let generate_nonce () =
  Random.self_init ();
  Bytes.init 24 (fun _ -> Char.chr (Random.bits () land 0xFF)) |> Bytes.to_string
  |> Base64.encode

let client_first_bare username nonce = Printf.sprintf "n=%s,r=%s" username nonce

let client_first_message username nonce = Printf.sprintf "n,,n=%s,r=%s" username nonce

let client_final_message channel_binding nonce proof =
  Printf.sprintf "c=%s,r=%s,p=%s" channel_binding nonce proof

let client_final_without_proof channel_binding nonce =
  Printf.sprintf "c=%s,r=%s" channel_binding nonce

let auth_message ~client_first_bare ~server_first ~client_final_without_proof =
  client_first_bare ^ "," ^ server_first ^ "," ^ client_final_without_proof

let parse_server_final payload =
  let parts = String.split_on_char ',' payload in
  let find prefix =
    try
      let part = List.find (String.starts_with ~prefix) parts in
      Some
        (String.sub part (String.length prefix)
           (String.length part - String.length prefix))
    with _ -> None
  in
  find "v="

let payload_element payload =
  let add_int32_le buf n =
    Buffer.add_char buf (Char.chr (n land 0xff));
    Buffer.add_char buf (Char.chr ((n lsr 8) land 0xff));
    Buffer.add_char buf (Char.chr ((n lsr 16) land 0xff));
    Buffer.add_char buf (Char.chr ((n lsr 24) land 0xff))
  in
  let key = "payload" in
  let len = 4 + 1 + String.length key + 1 + 4 + 1 + String.length payload + 1 in
  let buf = Buffer.create len in
  add_int32_le buf len;
  Buffer.add_char buf '\x05';
  Buffer.add_string buf key;
  Buffer.add_char buf '\x00';
  add_int32_le buf (String.length payload);
  Buffer.add_char buf '\x00';
  Buffer.add_string buf payload;
  Buffer.add_char buf '\x00';
  Bson.get_element key (Bson.decode (Buffer.contents buf))

let payload_string element =
  try Bson.get_generic_binary element with _ -> Bson.get_user_binary element

let options_doc =
  Bson.add_element "skipEmptyExchange" (Bson.create_boolean true) Bson.empty

let authenticate_transport ?timeout_ms mechanism ~password ~username ~auth_source
    transport =
  try
    let nonce = generate_nonce () in
    let client_first = client_first_bare username nonce in
    let request_id = Int32.of_float (Unix.gettimeofday ()) in
    let sasl_start =
      match
        Mongo_command.run_transport ?timeout_ms ~db:auth_source ~request_id
          transport
        [
          ("saslStart", Bson.create_int32 1l);
          ("mechanism", Bson.create_string (mechanism_name mechanism));
          ("payload", payload_element (client_first_message username nonce));
          ("options", Bson.create_doc_element options_doc);
        ]
      with
      | Ok response -> response.body
      | Error err -> Mongo_error.raise_exn err
    in
    let conversation_id =
      Int32.to_int
        (Bson.get_int32 (Bson.get_element "conversationId" sasl_start))
    in
    let server_first = payload_string (Bson.get_element "payload" sasl_start) in
    match parse_server_first server_first with
    | Error message -> Error (Mongo_error.Authentication message)
    | Ok (server_nonce, salt, iterations) ->
        if not (String.starts_with ~prefix:nonce server_nonce) then
          Error (Mongo_error.Authentication "server nonce mismatch")
        else if iterations < 4096 then
          Error
            (Mongo_error.Authentication
               "SCRAM iteration count below required minimum")
        else
          let password =
            match mechanism with
            | Scram_sha_1 -> mongo_sha1_password ~username ~password
            | Scram_sha_256 -> password
          in
          let salted =
            salted_password mechanism ~password ~salt ~iterations
            |> Bytes.to_string
          in
          let ckey = client_key mechanism salted in
          let skey = server_key mechanism salted in
          let client_final_without_proof =
            client_final_without_proof "biws" server_nonce
          in
          let auth_msg =
            auth_message ~client_first_bare:client_first ~server_first
              ~client_final_without_proof
          in
          let proof = client_proof mechanism ckey auth_msg in
          let client_final = client_final_message "biws" server_nonce proof in
          let request_id2 = Int32.of_float (Unix.gettimeofday ()) in
          let sasl_continue =
            match
              Mongo_command.run_transport ?timeout_ms ~db:auth_source
                ~request_id:request_id2 transport
              [
                ("saslContinue", Bson.create_int32 1l);
                ( "conversationId",
                  Bson.create_int32 (Int32.of_int conversation_id) );
                ("payload", payload_element client_final);
              ]
            with
            | Ok response -> response.body
            | Error err -> Mongo_error.raise_exn err
          in
          let done_payload =
            payload_string (Bson.get_element "payload" sasl_continue)
          in
          match parse_server_final done_payload with
          | None when done_payload = "" -> Ok ()
          | Some sig_
            when String.equal sig_ (server_signature mechanism skey auth_msg) ->
              Ok ()
          | _ -> Error (Mongo_error.Authentication "server signature mismatch")
  with
  | Mongo_error.Mongo_failed message -> Error (Mongo_error.Authentication message)
  | exn -> Error (Mongo_error.Authentication (Printexc.to_string exn))

let authenticate ?timeout_ms mechanism ~password ~username ~auth_source fd =
  authenticate_transport ?timeout_ms mechanism ~password ~username ~auth_source
    (Mongo_transport.plain fd)
