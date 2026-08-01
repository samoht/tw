open Alcotest
open Test_helpers

let check_display = check_handler_roundtrip (module Tw.Private.Flex.Handler)

let check_layout =
  check_handler_roundtrip (module Tw.Private.Flex_layout.Handler)

let check_props = check_handler_roundtrip (module Tw.Private.Flex_props.Handler)

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
  check_props "grow-3";
  check_props "grow-7";
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
    Test_helpers.check_invalid_parts (module Tw.Private.Flex.Handler)
  in
  let fail_props =
    Test_helpers.check_invalid_parts (module Tw.Private.Flex_props.Handler)
  in

  fail_display [ "flex"; "invalid" ];
  fail_display [ "flex"; "col" ];
  (* Now in flex_props *)
  fail_props [ "flex"; "invalid" ];
  fail_props [ "basis" ];
  fail_props [ "order" ];
  fail_props []

let suborder_matches_tailwind () =
  let open Tw in
  let shuffled = Test_helpers.shuffle [ flex; inline_flex ] in

  Test_helpers.check_ordering_matches
    ~test_name:"flex suborder matches Tailwind" shuffled

(* flex-1, flex-auto, flex-initial and flex-none all write the flex shorthand
   and grow/shrink/basis write the longhands it expands to, so what an element
   ends up flexing by is decided by the order the two sheets emit them in. The
   set is Tw.Private.Flex's own surface, re-exports included. *)
let rendering_matches_tailwind () =
  let classes =
    [
      "flex";
      "inline-flex";
      "flex-row";
      "flex-row-reverse";
      "flex-col";
      "flex-wrap";
      "flex-nowrap";
      "flex-1";
      "flex-auto";
      "flex-initial";
      "flex-none";
      "grow";
      "grow-0";
      "shrink";
      "shrink-0";
      "basis-0";
      "basis-1";
      "basis-1/2";
      "basis-3/4";
      "basis-[10rem]";
      "basis-full";
      "basis-auto";
      "order-1";
      "order-first";
      "order-last";
      "order-none";
    ]
  in
  Test_helpers.check_rendering_matches ~test_name:"flex renders like Tailwind"
    (List.map (fun c -> Result.get_ok (Tw.of_string c)) classes)

let tests =
  [
    test_case "flex of_string - valid values" `Quick of_string_valid;
    test_case "flex of_string - invalid values" `Quick of_string_invalid;
    test_case "flex suborder matches Tailwind" `Quick suborder_matches_tailwind;
    test_case "flex renders like Tailwind" `Slow rendering_matches_tailwind;
  ]

let suite = ("flex", tests)
