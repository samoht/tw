open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Padding.Handler)

let of_string_valid () =
  check "p-0";
  check "p-1";
  check "p-4";
  check "p-px";
  check "p-0.5";
  check "p-1.5";

  check "px-4";
  check "px-0";
  check "px-8";
  check "py-2";
  check "py-6";

  check "pt-2";
  check "pr-4";
  check "pb-6";
  check "pl-8"

let of_string_invalid () =
  let fail_maybe input =
    let class_name = String.concat "-" input in
    check_invalid_input (module Tw.Padding.Handler) class_name
  in

  fail_maybe [ "p" ];
  (* Missing value *)
  fail_maybe [ "p"; "!!!" ];
  (* Invalid value - non-alphanumeric *)
  fail_maybe [ "p"; "-1" ];
  (* Negative not allowed for padding *)
  fail_maybe [ "px"; "auto" ];
  (* Auto not valid for px *)
  fail_maybe [ "py"; "auto" ]
(* Auto not valid for py *)

let all_utilities () =
  let open Tw in
  List.concat_map
    (fun n -> [ p n; px n; py n; pt n; pb n; pl n; pr n ])
    Test_helpers.spacing_values

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"padding suborder matches Tailwind" shuffled

(** Test that CSS values use the correct spacing multiplier. p-64 should
    generate calc(var(--spacing)*64), not calc(var(--spacing)*16) *)
let test_css_values () =
  let open Tw in
  let css_for cls = Tw.to_css [ cls ] |> Tw.Css.pp ~minify:true in
  (* p-64 => calc(var(--spacing)*64) *)
  Alcotest.check bool "p-64 uses spacing*64" true
    (Astring.String.is_infix ~affix:"*64)" (css_for (p 64)));
  (* p-4 => calc(var(--spacing)*4) *)
  Alcotest.check bool "p-4 uses spacing*4" true
    (Astring.String.is_infix ~affix:"*4)" (css_for (p 4)));
  (* px-10 => calc(var(--spacing)*10) *)
  Alcotest.check bool "px-10 uses spacing*10" true
    (Astring.String.is_infix ~affix:"*10)" (css_for (px 10)))

let tests =
  [
    test_case "padding of_string - valid values" `Quick of_string_valid;
    test_case "padding of_string - invalid values" `Quick of_string_invalid;
    test_case "padding suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "padding CSS values" `Quick test_css_values;
  ]

let suite = ("padding", tests)
