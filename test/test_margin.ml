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

(* Tailwind orders margins by side first, then sign (negative before positive
   within a side), then value: e.g. m-0, -mt-1, mt-2, -ml-1, ml-2. tw used to
   sort all negatives ahead of all positives, which both diverged from Tailwind
   and reversed the cascade for conflicting rules (.m-0 vs .-ml-4). *)
let negative_suborder_matches_tailwind () =
  let mk s =
    match Tw.of_string s with
    | Ok u -> u
    | Error (`Msg m) -> failwith (s ^ ": " ^ m)
  in
  let utilities =
    List.map mk
      [
        "m-0";
        "m-2";
        "-m-1";
        "-m-4";
        "mt-2";
        "-mt-1";
        "ml-2";
        "-ml-1";
        "-mr-1";
        "mb-4";
      ]
  in
  Test_helpers.check_ordering_matches
    ~test_name:"negative margin suborder matches Tailwind"
    (Test_helpers.shuffle utilities)

let margin_side_bands_match_tailwind () =
  Test_helpers.check_class_order ~test_name:"margin side property bands"
    [
      "ml-2";
      "mbs-8";
      "mbe-2";
      "mt-2";
      "me-2";
      "ms-2";
      "my-2";
      "mx-2";
      "m-2";
      "mr-2";
      "mb-2";
      "mbs-6";
    ]

(* m, mx and ml all write margin-left, so which one an element ends up with is
   decided by the order the two sheets emit them in. *)
let rendering_matches_tailwind () =
  let classes =
    [
      "m-0";
      "m-2";
      "-m-1";
      "mx-2";
      "mx-auto";
      "my-4";
      "-mx-1";
      "mt-2";
      "-mt-1";
      "mr-2";
      "mb-4";
      "ml-2";
      "-ml-1";
      "ms-2";
      "me-4";
      (* State variants, so the browser is the oracle for them too. The harness
         forces :hover and :focus through CDP; without that these rules sit in
         both sheets, are matched by neither, and the two agree for the wrong
         reason. *)
      "hover:mt-8";
      "focus:mb-8";
      "active:ml-8";
      "disabled:mr-8";
    ]
  in
  Test_helpers.check_rendering_matches ~test_name:"margins render like Tailwind"
    (List.map (fun c -> Result.get_ok (Tw.of_string c)) classes)

(** Test that CSS values use the correct spacing multiplier. m-64 should
    generate calc(var(--spacing)*64), not calc(var(--spacing)*16) *)
let test_css_values () =
  let open Tw in
  let css_for cls = Tw.to_css [ cls ] |> Tw.Css.to_string ~minify:true in
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

(* Arbitrary margins accept the full length grammar (percent, container-query
   units, calc), not just px/rem, and round-trip verbatim. *)
let test_arbitrary_length_grammar () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "ml-[50%] emits margin-left:50%" true
    (Astring.String.is_infix ~affix:"margin-left:50%" (css "ml-[50%]"));
  Alcotest.(check bool)
    "mb-[-5cqw] keeps the cqw unit" true
    (Astring.String.is_infix ~affix:"margin-bottom:-5cqw" (css "mb-[-5cqw]"));
  Alcotest.(check bool)
    "ml-[calc(5%-2px)] spaces the calc operator" true
    (Astring.String.is_infix ~affix:"margin-left:calc(5% - 2px)"
       (css "ml-[calc(5%-2px)]"));
  (* class names round-trip verbatim *)
  let check c =
    match Tw.Margin.Handler.of_class Tw.Scheme.default c with
    | Ok u -> Alcotest.check string "roundtrip" c (Tw.Margin.Handler.to_class u)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" c m
  in
  check "ml-[50%]";
  check "mb-[-5cqw]"

(* An arbitrary value is read by the whole decoder, not by its last stage alone:
   [_] is a space, [--spacing(n)] expands to the spacing product, and only then
   are the math operators re-spaced. Tailwind emits all of these. *)
let test_arbitrary_value_decoder_stages () =
  check_declarations "mx-[calc(1px_+_1px)]" [ "margin-inline:calc(1px + 1px)" ];
  check_declarations "mt-[calc(1px_+_1px)]" [ "margin-top:calc(1px + 1px)" ];
  check_declarations "m-[--spacing(4)]" [ "margin:calc(var(--spacing)*4)" ]

(* The [']-suffixed sibling of each int constructor takes a half-step float (and
   still supports negative values); the int base keeps emitting what it always
   did. *)
let typed_prime () =
  let open Tw in
  check_typed_class "m-0.5" (m' 0.5);
  check_typed_class "-mt-0.5" (mt' (-0.5));
  check_typed_class "mx-1.5" (mx' 1.5);
  check_typed_class "my-0.5" (my' 0.5);
  check_typed_class "mr-0.5" (mr' 0.5);
  check_typed_class "mb-0.5" (mb' 0.5);
  check_typed_class "ml-0.5" (ml' 0.5);
  check_typed_class "m-4" (m 4);
  check_typed_class "-mt-4" (mt (-4))

(* A data-type hint chooses the longhand and says nothing about the value.
   Margin writes one longhand per side, so every hint reaches it and the length
   reader sees only what follows the hint, which stays in the class name. *)
let test_data_type_hint_before_the_length_reader () =
  check_declarations "m-[length:4px]" [ "margin:4px" ];
  check_declarations "mt-[foo:4px]" [ "margin-top:4px" ];
  check_declarations "m-[length:var(--x)]" [ "margin:var(--x)" ];
  List.iter check [ "m-[length:4px]"; "mt-[foo:4px]"; "m-[length:var(--x)]" ];
  let reject c = check_invalid_input (module Tw.Margin.Handler) c in
  reject "m-[:4px]";
  reject "m-[length:]"

let tests =
  [
    test_case "data-type hint before the length reader" `Quick
      test_data_type_hint_before_the_length_reader;
    test_case "margin of_string - valid values" `Quick of_string_valid;
    test_case "typed constructors: half-step" `Quick typed_prime;
    test_case "margin of_string - invalid values" `Quick of_string_invalid;
    test_case "named spacing requires theme token" `Quick
      named_spacing_requires_theme_token;
    test_case "margin suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "negative margin suborder matches Tailwind" `Quick
      negative_suborder_matches_tailwind;
    test_case "margin side bands match Tailwind" `Quick
      margin_side_bands_match_tailwind;
    test_case "margin CSS values" `Quick test_css_values;
    test_case "arbitrary length grammar" `Quick test_arbitrary_length_grammar;
    test_case "arbitrary value decoder stages" `Quick
      test_arbitrary_value_decoder_stages;
    test_case "margins render like Tailwind" `Slow rendering_matches_tailwind;
  ]

let suite = ("margin", tests)
