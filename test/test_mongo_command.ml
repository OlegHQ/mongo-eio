open Alcotest

let success_doc =
  Bson.add_element "ok" (Bson.create_double 1.0)
    (Bson.add_element "n" (Bson.create_int32 3l) Bson.empty)

let test_parse_success () =
  let response = Mongo_command.parse_response success_doc in
  check bool "ok" true response.ok;
  check int "count" 3 (Mongo_command.int_of_bson (Bson.get_element "n" response.body))

let test_enrich_preserves_command_first () =
  let command =
    Mongo_command.enrich_command ~db:"poster"
      [
        ("find", Bson.create_string "posts");
        ("filter", Bson.create_doc_element Bson.empty);
      ]
  in
  match Bson.all_elements command with
  | (name, _) :: _ -> check string "first field" "find" name
  | [] -> fail "empty command"

let test_enrich_read_preference_options () =
  let command =
    Mongo_command.enrich_command ~db:"poster"
      ~read_preference:Mongo_config.Secondary
      ~read_preference_tags:[ [ ("dc", "ny"); ("rack", "1") ]; [ ("dc", "sf") ] ]
      ~max_staleness_seconds:120
      [
        ("find", Bson.create_string "posts");
        ("filter", Bson.create_doc_element Bson.empty);
      ]
  in
  let read_preference =
    Bson.get_doc_element (Bson.get_element "$readPreference" command)
  in
  check string "mode" "secondary"
    (Bson.get_string (Bson.get_element "mode" read_preference));
  check int "maxStalenessSeconds" 120
    (Int32.to_int
       (Bson.get_int32 (Bson.get_element "maxStalenessSeconds" read_preference)));
  let tags =
    Bson.get_list (Bson.get_element "tags" read_preference)
    |> List.map Bson.get_doc_element
  in
  check int "tag set count" 2 (List.length tags);
  check string "first tag dc" "ny"
    (Bson.get_string (Bson.get_element "dc" (List.nth tags 0)));
  check string "first tag rack" "1"
    (Bson.get_string (Bson.get_element "rack" (List.nth tags 0)));
  check string "second tag dc" "sf"
    (Bson.get_string (Bson.get_element "dc" (List.nth tags 1)))

let test_enrich_read_and_write_concern () =
  let command =
    Mongo_command.enrich_command ~db:"poster"
      ~read_concern:Mongo_command.Majority
      ~write_concern:
        {
          Mongo_command.w = Some `Majority;
          j = Some true;
          wtimeout_ms = Some 5_000;
        }
      [
        ("find", Bson.create_string "posts");
        ("filter", Bson.create_doc_element Bson.empty);
      ]
  in
  (match Bson.all_elements command with
  | (name, _) :: _ -> check string "first field" "find" name
  | [] -> fail "empty command");
  let read_concern =
    Bson.get_doc_element (Bson.get_element "readConcern" command)
  in
  check string "read concern level" "majority"
    (Bson.get_string (Bson.get_element "level" read_concern));
  let write_concern =
    Bson.get_doc_element (Bson.get_element "writeConcern" command)
  in
  check string "write concern w" "majority"
    (Bson.get_string (Bson.get_element "w" write_concern));
  check bool "write concern j" true
    (Bson.get_boolean (Bson.get_element "j" write_concern));
  check int "write concern timeout" 5_000
    (Int32.to_int (Bson.get_int32 (Bson.get_element "wtimeout" write_concern)))

let () =
  run "mongo_command"
    [
      ( "response",
        [
          test_case "parse success" `Quick test_parse_success;
          test_case "preserve command order" `Quick
            test_enrich_preserves_command_first;
          test_case "read preference options" `Quick
            test_enrich_read_preference_options;
          test_case "read and write concern" `Quick
            test_enrich_read_and_write_concern;
        ] );
    ]
