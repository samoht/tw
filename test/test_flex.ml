open Alcotest
open Test_helpers

let check_display = check_handler_roundtrip (module Tw.Flex.Handler)
let check_layout = check_handler_roundtrip (module Tw.Flex_layout.Handler)
let check_props = check_handler_roundtrip (module Tw.Flex_props.Handler)

let of_string_valid () =
  (* Display *)
  check_display "flex";
  check_display "inline-flex";

  (* Direction - now in Flex_layout *)
  check_layout "flex-row";
  check_layout "flex-row-reverse";
  check_layout "flex-col";
  check_layout "flex-col-reverse";

  (* Wrap - now in Flex_layout *)
  check_layout "flex-wrap";
  check_layout "flex-wrap-reverse";
  check_layout "flex-nowrap";

  (* Flex shortcuts *)
  check_props "flex-1";
  check_props "flex-auto";
  check_props "flex-initial";
  check_props "flex-none";

  (* Grow/Shrink - Tailwind v4 uses shorter names *)
  check_props "grow";
  check_props "grow-0";
  check_props "shrink";
  check_props "shrink-0";

  (* Basis *)
  check_props "basis-0";
  check_props "basis-1";
  check_props "basis-auto";
  check_props "basis-full";

  (* Order *)
  check_props "order-1";
  check_props "order-2";
  check_props "order-3";
  check_props "order-4";
  check_props "order-5";
  check_props "order-6";
  check_props "order-first";
  check_props "order-last";
  check_props "order-none"

let of_string_invalid () =
  let fail_display =
    Test_helpers.check_invalid_parts (module Tw.Flex.Handler)
  in
  let fail_props =
    Test_helpers.check_invalid_parts (module Tw.Flex_props.Handler)
  in

  fail_display [ "flex"; "invalid" ];
  fail_display [ "flex"; "col" ];
  (* Now in flex_props *)
  fail_props [ "flex"; "invalid" ];
  fail_props [ "basis" ];
  fail_props [ "order" ];
  fail_props [ "order"; "0" ];
  fail_props []

let all_utilities () =
  let open Tw in
  [ flex; inline_flex ]

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"flex suborder matches Tailwind" shuffled

let tests =
  [
    test_case "flex of_string - valid values" `Quick of_string_valid;
    test_case "flex of_string - invalid values" `Quick of_string_invalid;
    test_case "flex suborder matches Tailwind" `Quick suborder_matches_tailwind;
  ]

let suite = ("flex", tests)
