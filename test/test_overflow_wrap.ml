let check =
  Test_helpers.check_handler_roundtrip (module Tw.Overflow_wrap.Handler)

let test_roundtrip () =
  check "wrap-normal";
  check "wrap-break-word";
  check "wrap-anywhere"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Overflow_wrap.Handler) "wrap";
  Test_helpers.check_invalid_input (module Tw.Overflow_wrap.Handler) "wrap-foo"

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid

let suite = ("overflow_wrap", tests)
