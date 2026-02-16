open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Margin.Handler)

let of_string_valid () =
  check "m-0";
  check "m-1";
  check "m-4";
  check "m-px";
  check "m-0.5";
  check "m-1.5";
  check "m-auto";
  check "-m-1";
  check "-m-4";

  check "mx-auto";
  check "mx-0";
  check "mx-4";
  check "-mx-2";
  check "-mx-4";
  check "my-8";
  check "my-2";
  check "-my-2";
  check "-my-8";

  check "mt-auto";
  check "mt-0";
  check "mt-2";
  check "mr-0";
  check "mr-4";
  check "mb-6";
  check "ml-8";
  check "-mt-2";
  check "-mr-4";
  check "-mb-6";
  check "-ml-8"

let of_string_invalid () =
  let fail_maybe input =
    let class_name = String.concat "-" input in
    check_invalid_input (module Tw.Margin.Handler) class_name
  in

  fail_maybe [ "m" ]
(* Missing value - note: m-<name> is valid in Tailwind v4 as named spacing
   var *)

let all_utilities () =
  let open Tw in
  List.concat_map
    (fun n -> [ m n; mx n; my n; mt n; mb n; ml n; mr n ])
    Test_helpers.spacing_values

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"margin suborder matches Tailwind" shuffled

(** Test that CSS values use the correct spacing multiplier. m-64 should
    generate calc(var(--spacing)*64), not calc(var(--spacing)*16) *)
let test_css_values () =
  let open Tw in
  let css_for cls = Tw.to_css [ cls ] |> Tw.Css.pp ~minify:true in
  (* m-64 => calc(var(--spacing)*64) *)
  Alcotest.check bool "m-64 uses spacing*64" true
    (Astring.String.is_infix ~affix:"*64)" (css_for (m 64)));
  (* m-4 => calc(var(--spacing)*4) *)
  Alcotest.check bool "m-4 uses spacing*4" true
    (Astring.String.is_infix ~affix:"*4)" (css_for (m 4)));
  (* mx-10 => calc(var(--spacing)*10) *)
  Alcotest.check bool "mx-10 uses spacing*10" true
    (Astring.String.is_infix ~affix:"*10)" (css_for (mx 10)));
  (* -m-8 => calc(var(--spacing)*-8) *)
  Alcotest.check bool "-m-8 uses spacing*-8" true
    (Astring.String.is_infix ~affix:"*-8)" (css_for (m (-8))))

let tests =
  [
    test_case "margin of_string - valid values" `Quick of_string_valid;
    test_case "margin of_string - invalid values" `Quick of_string_invalid;
    test_case "margin suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "margin CSS values" `Quick test_css_values;
  ]

let suite = ("margin", tests)
