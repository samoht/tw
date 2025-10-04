open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Gap.Handler)

let of_string_valid () =
  (* Gap all *)
  check "gap-0";
  check "gap-1";
  check "gap-4";
  check "gap-px";
  check "gap-0.5";
  check "gap-1.5";

  (* Gap x *)
  check "gap-x-0";
  check "gap-x-2";
  check "gap-x-4";
  check "gap-x-px";

  (* Gap y *)
  check "gap-y-0";
  check "gap-y-2";
  check "gap-y-6";
  check "gap-y-px";

  (* Space utilities *)
  check "space-x-2";
  check "space-y-4";
  check "-space-x-1";
  check "-space-y-2"

let of_string_invalid () =
  let fail_maybe = Test_helpers.check_invalid_parts (module Tw.Gap.Handler) in

  fail_maybe [ "gap" ];
  fail_maybe [ "gap"; "invalid" ];
  fail_maybe [ "gap"; "x" ];
  fail_maybe [ "gap"; "y" ];
  fail_maybe [ "space" ];
  fail_maybe [ "space"; "x" ];
  fail_maybe []

let all_utilities () =
  let open Tw in
  List.concat_map
    (fun n -> [ gap n; gap_x n; gap_y n ])
    Test_helpers.spacing_values

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches ~test_name:"gap suborder matches Tailwind"
    shuffled

let tests =
  [
    test_case "gap of_string - valid values" `Quick of_string_valid;
    test_case "gap of_string - invalid values" `Quick of_string_invalid;
    test_case "gap suborder matches Tailwind" `Slow suborder_matches_tailwind;
  ]

let suite = ("gap", tests)
