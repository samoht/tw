open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Flex.Handler)

let of_string_valid () =
  (* Display *)
  check "flex";
  check "inline-flex";

  (* Direction *)
  check "flex-row";
  check "flex-row-reverse";
  check "flex-col";
  check "flex-col-reverse";

  (* Wrap *)
  check "flex-wrap";
  check "flex-wrap-reverse";
  check "flex-nowrap";

  (* Flex shortcuts *)
  check "flex-1";
  check "flex-auto";
  check "flex-initial";
  check "flex-none";

  (* Grow/Shrink *)
  check "flex-grow";
  check "flex-grow-0";
  check "flex-shrink";
  check "flex-shrink-0";

  (* Basis *)
  check "basis-0";
  check "basis-1";
  check "basis-auto";
  check "basis-full";

  (* Order *)
  check "order-1";
  check "order-2";
  check "order-3";
  check "order-4";
  check "order-5";
  check "order-6";
  check "order-first";
  check "order-last";
  check "order-none"

let of_string_invalid () =
  let fail_maybe = Test_helpers.check_invalid_parts (module Tw.Flex.Handler) in

  fail_maybe [ "flex"; "invalid" ];
  fail_maybe [ "basis" ];
  fail_maybe [ "order" ];
  fail_maybe [ "order"; "0" ];
  fail_maybe [ "order"; "7" ];
  fail_maybe []

let all_utilities () =
  let open Tw in
  [
    flex_row;
    flex_row_reverse;
    flex_col;
    flex_col_reverse;
    flex_wrap;
    flex_wrap_reverse;
    flex_nowrap;
  ]

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
