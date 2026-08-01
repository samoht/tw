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

let tests =
  [
    test_case "pp_spacing_suffix" `Quick test_pp_spacing_suffix;
    test_case "pp_margin_suffix" `Quick test_pp_margin_suffix;
    test_case "int constructor" `Quick test_int_constructor;
    test_case "named spacing declares its token" `Quick
      test_named_spacing_declares_token;
  ]

let suite = ("spacing", tests)
