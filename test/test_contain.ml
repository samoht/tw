let check = Test_helpers.check_handler_roundtrip (module Tw.Contain.Handler)

let test_roundtrip () =
  check "contain-none";
  check "contain-strict";
  check "contain-content";
  check "contain-size";
  check "contain-inline-size";
  check "contain-layout";
  check "contain-paint";
  check "contain-style"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Contain.Handler) "contain";
  Test_helpers.check_invalid_input (module Tw.Contain.Handler) "contain-foo"

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid

let suite = ("contain", tests)
