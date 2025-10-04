open Alcotest
open Tw.Cursor.Handler

let check parts =
  let expected = String.concat "-" parts in
  match of_class expected with
  | Ok result ->
      Alcotest.check string "cursor class name" expected (to_class result)
  | Error (`Msg msg) -> fail msg

let of_string_valid () =
  check [ "cursor"; "auto" ];
  check [ "cursor"; "default" ];
  check [ "cursor"; "pointer" ];
  check [ "cursor"; "wait" ];
  check [ "cursor"; "move" ];
  check [ "cursor"; "not"; "allowed" ];
  check [ "cursor"; "text" ];
  check [ "cursor"; "crosshair" ];
  check [ "cursor"; "help" ];
  check [ "cursor"; "grab" ];
  check [ "cursor"; "grabbing" ]

let of_string_invalid () =
  let fail_maybe input =
    let class_name = String.concat "-" input in
    match of_class class_name with
    | Ok _ -> fail ("Expected error for: " ^ class_name)
    | Error _ -> ()
  in

  fail_maybe [ "cursor" ];
  fail_maybe [ "cursor"; "invalid" ];
  fail_maybe []

let all_utilities () =
  let open Tw in
  [
    cursor_auto;
    cursor_default;
    cursor_pointer;
    cursor_wait;
    cursor_move;
    cursor_not_allowed;
  ]

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"cursor suborder matches Tailwind" shuffled

let tests =
  [
    test_case "cursor of_string - valid values" `Quick of_string_valid;
    test_case "cursor of_string - invalid values" `Quick of_string_invalid;
    test_case "cursor suborder matches Tailwind" `Slow suborder_matches_tailwind;
  ]

let suite = ("cursor", tests)
