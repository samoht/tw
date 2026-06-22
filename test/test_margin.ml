module Css = Cascade.Css
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

  fail_maybe [ "m" ];
  (* Named spacing is valid only when the theme defines --spacing-<name>; stray
     source tokens like my-form / mt-big must not parse as utilities. *)
  fail_maybe [ "my"; "form" ];
  fail_maybe [ "mt"; "big" ];
  fail_maybe [ "mx"; "foo" ]

(* my-<name> parses only when the theme defines the spacing token, matching
   Tailwind; without it the class is rejected (see [of_string_invalid]). *)
let named_spacing_requires_theme_token () =
  let themed =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("spacing-form", "1rem") ]
  in
  (match Tw.Margin.Handler.of_class themed "my-form" with
  | Ok _ -> ()
  | Error (`Msg m) -> Alcotest.failf "my-form with theme rejected: %s" m);
  match Tw.Margin.Handler.of_class Tw.Scheme.default "my-form" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "my-form without theme token should be rejected"

let suborder_matches_tailwind () =
  let open Tw in
  let utilities =
    List.concat_map
      (fun n -> [ m n; mx n; my n; mt n; mb n; ml n; mr n ])
      Test_helpers.spacing_values
  in
  let shuffled = Test_helpers.shuffle utilities in

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
    test_case "named spacing requires theme token" `Quick
      named_spacing_requires_theme_token;
    test_case "margin suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "margin CSS values" `Quick test_css_values;
  ]

let suite = ("margin", tests)
