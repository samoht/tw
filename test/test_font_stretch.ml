open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Font_stretch.Handler)

let test_font_stretch () =
  check "font-stretch-ultra-condensed";
  check "font-stretch-extra-condensed";
  check "font-stretch-condensed";
  check "font-stretch-semi-condensed";
  check "font-stretch-normal";
  check "font-stretch-semi-expanded";
  check "font-stretch-expanded";
  check "font-stretch-extra-expanded";
  check "font-stretch-ultra-expanded";
  check "font-stretch-150%"

let tests = [ test_case "font stretch" `Quick test_font_stretch ]
let suite = ("font_stretch", tests)
