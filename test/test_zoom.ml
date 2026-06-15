let check = Test_helpers.check_handler_roundtrip (module Tw.Zoom.Handler)

let test_roundtrip () =
  check "zoom-50";
  check "zoom-100";
  check "zoom-[var(--zoom)]"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Zoom.Handler) "zoom";
  Test_helpers.check_invalid_input (module Tw.Zoom.Handler) "zoom-1.5";
  Test_helpers.check_invalid_input (module Tw.Zoom.Handler) "zoom-unknown"

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid

let suite = ("zoom", tests)
