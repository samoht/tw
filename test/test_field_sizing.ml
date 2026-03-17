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
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid

let suite = ("field_sizing", tests)
