open Alcotest
open Tw.Arbitrary.Handler

let check input =
  match of_class Tw.Scheme.default input with
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
    match of_class Tw.Scheme.default input with
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

let tests =
  [
    test_case "arbitrary of_string - valid values" `Quick of_string_valid;
    test_case "arbitrary of_string - invalid values" `Quick of_string_invalid;
    test_case "var-valued colour with opacity" `Quick test_var_color_opacity;
    test_case "custom property with opacity" `Quick test_custom_prop_opacity;
    test_case "deferred and var inputs never crash" `Quick test_no_crash;
  ]

let suite = ("arbitrary", tests)
