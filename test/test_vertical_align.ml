open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Vertical_align.Handler)

let test_vertical_align () =
  check "align-baseline";
  check "align-top";
  check "align-middle";
  check "align-bottom";
  check "align-text-top";
  check "align-text-bottom";
  check "align-sub";
  check "align-super"

(* A bracket that opens with a var() and goes on past it is no custom property
   reference: Tailwind writes it as it stands. *)
let test_var_prefixed_bracket () =
  List.iter Test_helpers.check_class_matches_tailwind
    [ "align-[var(--a)_1px]"; "align-[var(--a),foo]" ]

let tests =
  [
    Alcotest.test_case "var-prefixed bracket" `Quick test_var_prefixed_bracket;
    test_case "vertical align" `Quick test_vertical_align;
  ]

let suite = ("vertical_align", tests)
