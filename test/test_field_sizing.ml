let check =
  Test_helpers.check_handler_roundtrip (module Tw.Field_sizing.Handler)

let test_roundtrip () =
  check "field-sizing-content";
  check "field-sizing-fixed"

let test_invalid () =
  Test_helpers.check_invalid_input
    (module Tw.Field_sizing.Handler)
    "field-sizing";
  Test_helpers.check_invalid_input
    (module Tw.Field_sizing.Handler)
    "field-sizing-auto"

let tests =
  Alcotest.
    [
      test_case "roundtrip" `Quick test_roundtrip;
      test_case "invalid" `Quick test_invalid;
    ]

let suite = ("field_sizing", tests)
