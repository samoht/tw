let check = Test_helpers.check_handler_roundtrip (module Tw.Text_shadow.Handler)

let test_roundtrip () =
  check "text-shadow-none";
  check "text-shadow-2xs";
  check "text-shadow-xs";
  check "text-shadow-sm";
  check "text-shadow";
  check "text-shadow-lg"

let test_invalid () =
  Test_helpers.check_invalid_input
    (module Tw.Text_shadow.Handler)
    "text-shadow-foo"

let tests =
  Alcotest.
    [
      test_case "roundtrip" `Quick test_roundtrip;
      test_case "invalid" `Quick test_invalid;
    ]

let suite = ("text_shadow", tests)
