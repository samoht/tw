module Css = Cascade.Css
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
  check "-space-y-2";
  (* px-valued space, both signs *)
  check "space-x-px";
  check "space-y-px";
  check "-space-x-px";
  check "-space-y-px"

let of_string_invalid () =
  let fail_maybe = Test_helpers.check_invalid_parts (module Tw.Gap.Handler) in

  fail_maybe [ "gap" ];
  fail_maybe [ "gap"; "invalid" ];
  fail_maybe [ "gap"; "x" ];
  fail_maybe [ "gap"; "y" ];
  fail_maybe [ "space" ];
  fail_maybe [ "space"; "x" ];
  fail_maybe []

let suborder_matches_tailwind () =
  let open Tw in
  let utilities =
    List.concat_map
      (fun n -> [ gap n; gap_x n; gap_y n ])
      Test_helpers.spacing_values
  in
  let shuffled = Test_helpers.shuffle utilities in

  Test_helpers.check_ordering_matches ~test_name:"gap suborder matches Tailwind"
    shuffled

let require_parse class_name =
  match Tw.of_string class_name with
  | Ok u -> u
  | Error (`Msg m) -> Alcotest.failf "%s: %s" class_name m

(* gap and space interleave in Tailwind's property-registration order: .gap-4
   comes before :where(.space-x-2 > :not(:last-child)). *)
let mixed_gap_space_order_matches_tailwind () =
  [ "gap-4"; "space-x-2" ] |> List.map require_parse
  |> Test_helpers.check_ordering_matches
       ~test_name:"mixed gap/space order matches Tailwind"

(* gap-x (column-gap) sorts before gap-y (row-gap) regardless of arbitrary vs
   standard values. *)
let mixed_gap_axis_arbitrary_order_matches_tailwind () =
  [ "gap-4"; "gap-x-[4px]"; "gap-y-1.5" ]
  |> List.map require_parse
  |> Test_helpers.check_ordering_matches
       ~test_name:"mixed gap axis arbitrary order matches Tailwind"

(* space-x-px / -space-x-px use a literal +/-1px gap (no --spacing multiple),
   wrapped in the reverse calc like the numeric variants. *)
let test_space_px_values () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.check bool "space-x-px uses 1px gap" true
    (Astring.String.is_infix ~affix:"calc(1px * var(--tw-space-x-reverse))"
       (css "space-x-px"));
  Alcotest.check bool "-space-x-px uses -1px gap" true
    (Astring.String.is_infix ~affix:"calc(-1px * var(--tw-space-x-reverse))"
       (css "-space-x-px"));
  Alcotest.check bool "space-x-px sets no --spacing" false
    (Astring.String.is_infix ~affix:"--spacing" (css "space-x-px"))

(** Test that CSS values use the correct spacing multiplier. gap-64 should
    generate calc(var(--spacing)*64), not calc(var(--spacing)*16) *)
let test_css_values () =
  let open Tw in
  let css_for cls = Tw.to_css [ cls ] |> Tw.Css.pp ~minify:true in
  (* gap-64 => calc(var(--spacing)*64) *)
  Alcotest.check bool "gap-64 uses spacing*64" true
    (Astring.String.is_infix ~affix:"*64)" (css_for (gap 64)));
  (* gap-4 => calc(var(--spacing)*4) *)
  Alcotest.check bool "gap-4 uses spacing*4" true
    (Astring.String.is_infix ~affix:"*4)" (css_for (gap 4)));
  (* gap-x-10 => calc(var(--spacing)*10) *)
  Alcotest.check bool "gap-x-10 uses spacing*10" true
    (Astring.String.is_infix ~affix:"*10)" (css_for (gap_x 10)))

let tests =
  [
    test_case "gap of_string - valid values" `Quick of_string_valid;
    test_case "gap of_string - invalid values" `Quick of_string_invalid;
    test_case "gap suborder matches Tailwind" `Quick suborder_matches_tailwind;
    test_case "mixed gap/space order matches Tailwind" `Quick
      mixed_gap_space_order_matches_tailwind;
    test_case "mixed gap axis arbitrary order matches Tailwind" `Quick
      mixed_gap_axis_arbitrary_order_matches_tailwind;
    test_case "space-px CSS values" `Quick test_space_px_values;
    test_case "gap CSS values" `Quick test_css_values;
  ]

let suite = ("gap", tests)
