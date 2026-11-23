open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Font_smoothing.Handler)

let test_antialiasing_order () =
  Test_helpers.check_class_order ~test_name:"antialiasing order"
    [
      "caret-red-500";
      "placeholder-gray-400";
      "subpixel-antialiased";
      "antialiased";
    ]

let test_variant_antialiasing_order () =
  Test_helpers.check_class_order ~test_name:"variant antialiasing order"
    [
      "hover:placeholder-gray-400";
      "hover:subpixel-antialiased";
      "hover:antialiased";
    ]

let test_font_smoothing () =
  check "antialiased";
  check "subpixel-antialiased"

let tests =
  [
    test_case "font smoothing" `Quick test_font_smoothing;
    test_case "antialiasing order" `Slow test_antialiasing_order;
    test_case "variant antialiasing order" `Slow test_variant_antialiasing_order;
  ]

let suite = ("font_smoothing", tests)
