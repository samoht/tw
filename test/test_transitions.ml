let check = Test_helpers.check_handler_roundtrip (module Tw.Transitions.Handler)

let test_roundtrip () =
  check "transition-none";
  check "transition-all";
  check "transition-colors";
  check "transition-opacity";
  check "transition-shadow";
  check "transition-transform";
  check "transition";
  check "duration-150";
  check "duration-300";
  check "delay-150";
  check "delay-300";
  check "ease-linear";
  check "ease-in";
  check "ease-out";
  check "ease-in-out"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Transitions.Handler) "duration";
  Test_helpers.check_invalid_input (module Tw.Transitions.Handler) "delay";
  Test_helpers.check_invalid_input (module Tw.Transitions.Handler) "ease"

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid

let suite = ("transitions", tests)
