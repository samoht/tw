open Alcotest
open Tw.Spacing

let test_pp_spacing_suffix () =
  let check suffix expected =
    check string "spacing suffix" expected (pp_spacing_suffix suffix)
  in
  check `Px "px";
  check `Full "full";
  check (`Rem 0.25) "1";
  check (`Rem 0.5) "2";
  check (`Rem 1.0) "4";
  check (`Rem 2.0) "8";
  check (`Rem 0.125) "0.5";
  check (`Rem 0.375) "1.5"

let test_pp_margin_suffix () =
  let check suffix expected =
    check string "margin suffix" expected (pp_margin_suffix suffix)
  in
  check `Auto "auto";
  check `Px "px";
  check `Full "full";
  check (`Rem 1.0) "4"

let test_int_constructor () =
  check
    (module struct
      type t = [ `Px | `Full | `Rem of float | `Named of string ]

      let equal a b =
        match (a, b) with
        | `Px, `Px | `Full, `Full -> true
        | `Rem a, `Rem b -> Float.equal a b
        | `Named a, `Named b -> String.equal a b
        | _ -> false

      let pp fmt = function
        | `Px -> Fmt.string fmt "Px"
        | `Full -> Fmt.string fmt "Full"
        | `Rem f -> Fmt.pf fmt "Rem %f" f
        | `Named s -> Fmt.pf fmt "Named %s" s
    end)
    "int spacing" (`Rem 1.0) (int 4)

(* A named spacing renders as [var(--spacing-<name>)], so the utility has to
   declare the token as well as reference it: padding and gap used to emit the
   reference alone, which renders as nothing. Tailwind emits the binding for
   every one of these classes given the same @theme. *)
let test_named_spacing_declares_token () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("spacing-form", "1rem") ]
  in
  List.iter
    (fun cls ->
      match Tw.of_string ~theme cls with
      | Error (`Msg m) -> Alcotest.failf "%s rejected: %s" cls m
      | Ok u ->
          let css = Tw.to_css ~theme ~base:false [ u ] in
          if not (Test_helpers.has_var_in_layer "--spacing-form" "theme" css)
          then
            Alcotest.failf "%s references --spacing-form without declaring it"
              cls)
    [ "m-form"; "-m-form"; "p-form"; "px-form"; "gap-form"; "gap-x-form" ]

(* Tailwind reads [--spacing: initial] as "remove the multiplier", so a bare
   step has nothing to compute from and stops being a utility across every
   family that offers the scale. A step the theme binds outright survives, and
   the removed token itself never reaches the theme layer. *)
let test_removed_spacing_multiplier () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("spacing", "initial"); ("spacing-4", "1rem") ]
  in
  List.iter
    (fun cls ->
      match Tw.of_string ~theme cls with
      | Ok _ -> Alcotest.failf "%s is a utility with no --spacing to read" cls
      | Error _ -> ())
    [
      "px-1";
      "-m-2";
      "gap-3";
      "space-x-1";
      "w-1";
      "top-1";
      "indent-1";
      "scroll-m-1";
      "border-spacing-1";
    ];
  match Tw.of_string ~theme "px-4" with
  | Error (`Msg m) -> Alcotest.failf "px-4 rejected: %s" m
  | Ok u ->
      let css = Tw.to_css ~theme ~base:false [ u ] in
      Alcotest.(check bool)
        "the step the theme binds is declared" true
        (Test_helpers.has_var_in_layer "--spacing-4" "theme" css);
      Alcotest.(check bool)
        "the removed multiplier is not declared" false
        (Test_helpers.has_var_in_layer "--spacing" "theme" css)

(* [--spacing-*: initial] resets the whole namespace, the multiplier included,
   and only the steps the block goes on to declare survive it. *)
let test_reset_spacing_namespace () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("spacing-*", "initial"); ("spacing-4", "1rem") ]
  in
  (match Tw.of_string ~theme "px-1" with
  | Ok _ -> Alcotest.fail "px-1 is a utility after the namespace reset"
  | Error _ -> ());
  match Tw.of_string ~theme "px-4" with
  | Error (`Msg m) -> Alcotest.failf "px-4 rejected: %s" m
  | Ok u ->
      let css = Tw.to_css ~theme ~base:false [ u ] in
      Alcotest.(check bool)
        "the step declared after the reset survives" true
        (Test_helpers.has_var_in_layer "--spacing-4" "theme" css)

let tests =
  [
    test_case "pp_spacing_suffix" `Quick test_pp_spacing_suffix;
    test_case "pp_margin_suffix" `Quick test_pp_margin_suffix;
    test_case "int constructor" `Quick test_int_constructor;
    test_case "named spacing declares its token" `Quick
      test_named_spacing_declares_token;
    test_case "--spacing: initial removes the multiplier" `Quick
      test_removed_spacing_multiplier;
    test_case "--spacing-*: initial resets the namespace" `Quick
      test_reset_spacing_namespace;
  ]

let suite = ("spacing", tests)
