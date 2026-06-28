open Alcotest

let sha1_server_first =
  "r=fyko+d2lbbFgONRv9qkxdawLHo+Vgk7qvUOKUwuWLIWg4l/9SraGMHEE,s=rQ9ZY3MntBeuP3E1TDVC4w==,i=10000"

let sha256_server_first =
  "r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096"

let compute mechanism ~username ~password ~client_first_bare ~server_first =
  let server_nonce, salt, iterations =
    match Mongo_scram.parse_server_first server_first with
    | Ok parsed -> parsed
    | Error message -> fail message
  in
  let password =
    match mechanism with
    | Mongo_scram.Scram_sha_1 ->
        Mongo_scram.mongo_sha1_password ~username ~password
    | Mongo_scram.Scram_sha_256 -> password
  in
  let salted =
    Mongo_scram.salted_password mechanism ~password ~salt ~iterations
    |> Bytes.to_string
  in
  let ckey = Mongo_scram.client_key mechanism salted in
  let skey = Mongo_scram.server_key mechanism salted in
  let without_proof =
    Mongo_scram.client_final_without_proof "biws" server_nonce
  in
  let auth_message =
    Mongo_scram.auth_message ~client_first_bare ~server_first
      ~client_final_without_proof:without_proof
  in
  ( Mongo_scram.client_proof mechanism ckey auth_message,
    Mongo_scram.server_signature mechanism skey auth_message )

let test_sha1_vector () =
  let proof, sig_ =
    compute Mongo_scram.Scram_sha_1 ~username:"user" ~password:"pencil"
      ~client_first_bare:"n=user,r=fyko+d2lbbFgONRv9qkxdawL"
      ~server_first:sha1_server_first
  in
  check string "client proof" "MC2T8BvbmWRckDw8oWl5IVghwCY=" proof;
  check string "server signature" "UMWeI25JD1yNYZRMpZ4VHvhZ9e0=" sig_

let test_sha256_vector () =
  let proof, sig_ =
    compute Mongo_scram.Scram_sha_256 ~username:"user" ~password:"pencil"
      ~client_first_bare:"n=user,r=rOprNGfwEbeRWgbNEkqO"
      ~server_first:sha256_server_first
  in
  check string "client proof" "dHzbZapWIk4jUhN+Ute9ytag9zjfMHgsqmmiz7AndVQ=" proof;
  check string "server signature" "6rriTRBi23WpRR/wtup+mMhUZUn/dB5nLTJRsjl95G4=" sig_

let () =
  run "mongo_scram"
    [
      ( "spec vectors",
        [
          test_case "SCRAM-SHA-1" `Quick test_sha1_vector;
          test_case "SCRAM-SHA-256" `Quick test_sha256_vector;
        ] );
    ]
