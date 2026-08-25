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

  (* Deprecated v3 spellings keep their own class name (not folded into the
     shorter grow/shrink), so the emitted selector matches the source class *)
  check "flex-grow";
  check "flex-grow-0";
  check "flex-shrink";
  check "flex-shrink-0";

  (* Basis *)
  check "basis-0";
  check "basis-1";
  (* v4 accepts any bare integer (spacing scale), not just 0/1 *)
  check "basis-3";
  check "basis-7";
  check "basis-auto";
  check "basis-full";
  (* container scale, including the digit-led names is_named_spacing rejects *)
  check "basis-xs";
  check "basis-xl";
  check "basis-2xs";
  check "basis-3xs";
  check "basis-2xl";
  check "basis-7xl";

  (* Order *)
  check "order-0";
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
    match Tw.Flex_props.Handler.of_class Tw.Scheme.default class_name with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "flex"; "invalid" ];
  fail_maybe [ "basis" ];
  (* not a container-scale name *)
  fail_maybe [ "basis"; "4xs" ];
  (* Missing value *)
  fail_maybe [ "order" ];
  fail_maybe []

let suborder_matches_tailwind () =
  let open Tw in
  let utilities =
    [
      flex_1;
      flex_auto;
      flex_initial;
      flex_none;
      flex_grow;
      flex_grow_0;
      flex_shrink;
      flex_shrink_0;
      basis_0;
      basis_1;
      basis_auto;
      basis_full;
      order 1;
      order 3;
      order 6;
      order_first;
      order_last;
      order_none;
    ]
  in
  let shuffled = Test_helpers.shuffle utilities in

  Test_helpers.check_ordering_matches
    ~test_name:"flex_props suborder matches Tailwind" shuffled

(* [basis] lists --flex-basis, then --spacing, then --container, so a theme that
   sets both --spacing-sm and --container-sm reads the spacing one. *)
let test_basis_named_prefers_spacing () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("spacing-sm", "8px"); ("container-sm", "256px") ]
  in
  let css =
    match Tw.of_string ~theme "basis-sm" with
    | Ok u -> Tw.to_css ~theme ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "basis-sm: %s" m
  in
  Alcotest.(check bool)
    "basis-sm reads --spacing-sm" true
    (Astring.String.is_infix ~affix:"var(--spacing-sm)" css);
  Alcotest.(check bool)
    "basis-sm declares --spacing-sm" true
    (Astring.String.is_infix ~affix:"--spacing-sm: 8px" css)

let tests =
  [
    test_case "basis-* prefers --spacing-*" `Quick
      test_basis_named_prefers_spacing;
    test_case "flex_props of_string - valid values" `Quick of_string_valid;
    test_case "flex_props of_string - invalid values" `Quick of_string_invalid;
    test_case "flex_props suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
  ]

let suite = ("flex_props", tests)
