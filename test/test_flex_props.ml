open Alcotest

let check = Test_helpers.check_handler_roundtrip (module Tw.Flex_props.Handler)

let of_string_valid () =
  (* Note: Direction and Wrap utilities are now in Flex_layout module *)

  (* Flex shortcuts *)
  check "flex-1";
  check "flex-auto";
  check "flex-initial";
  check "flex-none";

  (* Grow/Shrink - Tailwind v4 uses shorter names *)
  check "grow";
  check "grow-0";
  check "shrink";
  check "shrink-0";

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
  let fail_maybe input =
    let class_name = String.concat "-" input in
    match Tw.Flex_props.Handler.of_class class_name with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "flex"; "invalid" ];
  fail_maybe [ "basis" ];
  (* Missing value *)
  fail_maybe [ "order" ];
  (* Missing value *)
  fail_maybe [ "order"; "0" ];
  (* Invalid - must be >= 1 *)
  fail_maybe []

let all_utilities () =
  let open Tw in
  [
    (* Note: Direction and Wrap utilities are now in Flex_layout module *)
    (* Flex shortcuts *)
    flex_1;
    flex_auto;
    flex_initial;
    flex_none;
    (* Grow *)
    flex_grow;
    flex_grow_0;
    (* Shrink *)
    flex_shrink;
    flex_shrink_0;
    (* Basis *)
    basis_0;
    basis_1;
    basis_auto;
    basis_full;
    (* Order *)
    order 1;
    order 3;
    order 6;
    order_first;
    order_last;
    order_none;
  ]

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"flex_props suborder matches Tailwind" shuffled

let tests =
  [
    test_case "flex_props of_string - valid values" `Quick of_string_valid;
    test_case "flex_props of_string - invalid values" `Quick of_string_invalid;
    test_case "flex_props suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
  ]

let suite = ("flex_props", tests)
