open Alcotest
open Tw.Private.Arbitrary.Handler

let check input =
  match of_class Tw.Theme.default input with
  | Ok result ->
      Alcotest.check string "arbitrary class name" input (to_class result)
  | Error (`Msg msg) -> fail msg

let of_string_valid () =
  check "[color:red]/50";
  check "[background-color:blue]/[50%]";
  check "[border-color:#ff0000]/25";
  (* var-valued colours and custom properties with /opacity round-trip *)
  check "[color:var(--my-color)]/50";
  check "[--x:#ff0000]/50";
  check "[--gradient-bg:var(--color-black)]/15";
  (* Plain custom-property declarations and non-colour standard properties parse
     via the cascade declaration parser. *)
  check "[--foo:bar]";
  check "[mask-type:luminance]";
  check "[display:flex]";
  check "[color:red]"

let of_string_invalid () =
  let fail_maybe input =
    match of_class Tw.Theme.default input with
    | Ok _ -> fail ("Expected error for: " ^ input)
    | Error _ -> ()
  in
  fail_maybe "";
  fail_maybe "color:red";
  fail_maybe "[invalid]";
  fail_maybe "[]"

let css cls =
  match Tw.of_string cls with
  | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
  | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m

(* An arbitrary [property:value] normalises omitted whitespace around calc
   operators, like the utility form: [margin:calc(100%-10px)] emits margin:
   calc(100% - 10px). *)
let test_property_calc_operators () =
  Alcotest.(check bool)
    "[margin:calc(100%-10px)] spaces the operator" true
    (Astring.String.is_infix ~affix:"margin: calc(100% - 10px)"
       (css "[margin:calc(100%-10px)]"));
  Alcotest.(check bool)
    "[width:calc(var(--a)-var(--b))] spaces the operator" true
    (Astring.String.is_infix ~affix:"width: calc(var(--a) - var(--b))"
       (css "[width:calc(var(--a)-var(--b))]"))

(* theme() dot-notation inside an arbitrary value resolves statically:
   theme(colors.red.500) to the red-500 oklch (with /alpha appended for the
   opacity form), and the class name round-trips via its alias. *)
let test_theme_dot_notation () =
  let g =
    "bg-[image:linear-gradient(to_right,theme(colors.red.500)_75%,theme(colors.red.500/25%))]"
  in
  Alcotest.(check bool)
    "theme(colors.red.500) resolves to the red-500 oklch" true
    (Astring.String.is_infix ~affix:"oklch(63.7%" (css g));
  Alcotest.(check bool)
    "theme(colors.red.500/25%) appends the alpha" true
    (Astring.String.is_infix ~affix:"25.331 / 25%" (css g));
  Alcotest.(check string)
    "theme() class round-trips" g
    (Tw.to_string (Result.get_ok (Tw.of_string g)))

(* A var-valued colour with /opacity used to raise invalid_arg; it now emits an
   oklab color-mix under @supports, with a fallback, type-safely. *)
let test_var_color_opacity () =
  let out = css "[color:var(--my-color)]/50" in
  Alcotest.(check bool)
    "oklab color-mix on the var" true
    (Astring.String.is_infix
       ~affix:"color-mix(in oklab, var(--my-color) 50%, transparent)" out)

(* A custom property with /opacity sets the property to a color-mix via the
   typed [Css.var] form (no token stream). *)
let test_custom_prop_opacity () =
  let out = css "[--x:#ff0000]/50" in
  Alcotest.(check bool)
    "custom property gets a color-mix value" true
    (Astring.String.is_infix
       ~affix:"--x: color-mix(in oklab, #ff0000 50%, transparent)" out)

(* The previously-crashing inputs must never raise: they either render or are
   rejected, but [to_css] must complete. *)
let test_no_crash () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok u -> ignore (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string)
      | Error _ -> ())
    [
      "[--gradient-bg:var(--color-black)]/15";
      "[color:var(--color-red-500)]/40";
      "[--foo:bar]";
      "[mask-type:luminance]";
    ]

(* Tailwind's [--spacing(N)] shorthand reads the spacing scale, so it has to be
   expanded here too: the value used to reach the sheet verbatim. *)
let test_property_spacing_fn () =
  Alcotest.(check bool)
    "[--gap:--spacing(10)] reads the spacing scale" true
    (Astring.String.is_infix ~affix:"--gap: calc(var(--spacing) * 10)"
       (css "[--gap:--spacing(10)]"))

(* Tailwind's [--alpha(C/P)] is the [/opacity] form written as a function, and a
   reference to a palette token renders from the palette, so the fallback is a
   colour rather than the bare reference. *)
let test_alpha_fn () =
  let out = css "[--checkered-bg:--alpha(var(--color-gray-950)/10%)]" in
  Alcotest.(check bool)
    "fallback resolves the palette colour" true
    (Astring.String.is_infix ~affix:"--checkered-bg: #0307121a" out);
  Alcotest.(check bool)
    "@supports keeps the token reference" true
    (Astring.String.is_infix
       ~affix:"color-mix(in oklab, var(--color-gray-950) 10%, transparent)" out);
  Alcotest.(check string)
    "the --alpha() spelling round-trips"
    "[--checkered-bg:--alpha(var(--color-gray-950)/10%)]"
    (Tw.to_string
       (Result.get_ok
          (Tw.of_string "[--checkered-bg:--alpha(var(--color-gray-950)/10%)]")))

(* A [#...] value is only a colour when it is a hex spelling. A malformed one
   reaches the raising hex constructor from inside [of_class], so the exception
   escapes the parser itself. *)
let test_invalid_hex_value () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "[color:#zz]";
  rejected "[color:#]";
  rejected "[color:#12345]";
  Alcotest.(check bool)
    "[color:#ff0000] still emits the colour" true
    (Astring.String.is_infix ~affix:"color: #ff0000" (css "[color:#ff0000]"))

let tests =
  [
    test_case "invalid hex value" `Quick test_invalid_hex_value;
    test_case "arbitrary of_string - valid values" `Quick of_string_valid;
    test_case "arbitrary of_string - invalid values" `Quick of_string_invalid;
    test_case "property value calc operators" `Quick
      test_property_calc_operators;
    test_case "property value --spacing()" `Quick test_property_spacing_fn;
    test_case "property value --alpha()" `Quick test_alpha_fn;
    test_case "theme() dot-notation" `Quick test_theme_dot_notation;
    test_case "var-valued colour with opacity" `Quick test_var_color_opacity;
    test_case "custom property with opacity" `Quick test_custom_prop_opacity;
    test_case "deferred and var inputs never crash" `Quick test_no_crash;
  ]

let suite = ("arbitrary", tests)
