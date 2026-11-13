open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Font_variant_numeric.Handler)

let test_numeric_variants () =
  check "normal-nums";
  check "ordinal";
  check "slashed-zero";
  check "lining-nums";
  check "oldstyle-nums";
  check "proportional-nums";
  check "tabular-nums";
  check "diagonal-fractions";
  check "stacked-fractions"

let tests = [ test_case "numeric variants" `Quick test_numeric_variants ]
let suite = ("font_variant_numeric", tests)
