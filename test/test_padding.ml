module Css = Cascade.Css
open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Private.Padding.Handler)

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
    check_invalid_input (module Tw.Private.Padding.Handler) class_name
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

let suborder_matches_tailwind () =
  let open Tw in
  let utilities =
    List.concat_map
      (fun n -> [ p n; px n; py n; pt n; pb n; pl n; pr n ])
      Test_helpers.spacing_values
  in
  let shuffled = Test_helpers.shuffle utilities in

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

(* Arbitrary paddings accept the full length grammar (percent, calc), not just
   px/rem, and round-trip verbatim. *)
let test_arbitrary_length_grammar () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "p-[calc(var(--spacing-6)-1px)] spaces the operator" true
    (Astring.String.is_infix ~affix:"padding:calc(var(--spacing-6) - 1px)"
       (css "p-[calc(var(--spacing-6)-1px)]"));
  Alcotest.(check bool)
    "pl-[calc(100%-21.5rem)] spaces the operator" true
    (Astring.String.is_infix ~affix:"padding-left:calc(100% - 21.5rem)"
       (css "pl-[calc(100%-21.5rem)]"));
  Alcotest.(check bool)
    "px-[50%] keeps the percent" true
    (Astring.String.is_infix ~affix:"padding-inline:50%" (css "px-[50%]"));
  let check c =
    match Tw.Private.Padding.Handler.of_class Tw.Theme.default c with
    | Ok u ->
        Alcotest.check string "roundtrip" c
          (Tw.Private.Padding.Handler.to_class u)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" c m
  in
  check "px-[50%]";
  check "p-[calc(var(--spacing-6)-1px)]"

(* Tailwind's [--spacing(N)] shorthand can appear inside an arbitrary value; it
   is not CSS, so the whole utility used to drop out. Expanding it also has to
   pull [--spacing] into the theme layer, which only colour tokens reached. *)
let test_arbitrary_spacing_fn () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let s = css "py-[calc(--spacing(2)+1px)]" in
  Alcotest.(check bool)
    "expands to the spacing scale" true
    (Astring.String.is_infix
       ~affix:"padding-block:calc(calc(var(--spacing)*2) + 1px)" s);
  Alcotest.(check bool)
    "declares --spacing" true
    (Astring.String.is_infix ~affix:"--spacing:.25rem" s)

let tests =
  [
    test_case "arbitrary --spacing()" `Quick test_arbitrary_spacing_fn;
    test_case "padding of_string - valid values" `Quick of_string_valid;
    test_case "padding of_string - invalid values" `Quick of_string_invalid;
    test_case "padding suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "padding CSS values" `Quick test_css_values;
    test_case "arbitrary length grammar" `Quick test_arbitrary_length_grammar;
  ]

let suite = ("padding", tests)
