open Alcotest

let check = Test_helpers.check_handler_roundtrip (module Tw.Layout.Handler)

let of_string_valid () =
  check "block";
  check "inline";
  check "inline-block";
  check "hidden"

let of_string_invalid () =
  let fail_maybe input =
    let class_name = String.concat "-" input in
    match Tw.Layout.Handler.of_class class_name with
    | Ok _ -> fail ("Expected error for: " ^ class_name)
    | Error _ -> ()
  in

  fail_maybe [ "invalid" ];
  fail_maybe [ "inline"; "invalid" ];
  fail_maybe []

let all_utilities () =
  let open Tw in
  [ block; inline; inline_block; hidden ]

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"display suborder matches Tailwind" shuffled

let tests =
  [
    test_case "display of_string - valid values" `Quick of_string_valid;
    test_case "display of_string - invalid values" `Quick of_string_invalid;
    test_case "display suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
  ]

let suite = ("display", tests)
