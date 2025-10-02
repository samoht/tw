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
      type t = [ `Px | `Full | `Rem of float ]

      let equal a b =
        match (a, b) with
        | `Px, `Px | `Full, `Full -> true
        | `Rem a, `Rem b -> Float.equal a b
        | _ -> false

      let pp fmt = function
        | `Px -> Fmt.string fmt "Px"
        | `Full -> Fmt.string fmt "Full"
        | `Rem f -> Fmt.pf fmt "Rem %f" f
    end)
    "int spacing" (`Rem 1.0) (int 4)

let tests =
  [
    test_case "pp_spacing_suffix" `Quick test_pp_spacing_suffix;
    test_case "pp_margin_suffix" `Quick test_pp_margin_suffix;
    test_case "int constructor" `Quick test_int_constructor;
  ]

let suite = ("spacing", tests)
