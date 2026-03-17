let check = Test_helpers.check_handler_roundtrip (module Tw.Touch.Handler)

let test_roundtrip () =
  check "touch-auto";
  check "touch-none";
  check "touch-manipulation";
  check "touch-pan-x";
  check "touch-pan-y";
  check "touch-pan-left";
  check "touch-pan-right";
  check "touch-pan-up";
  check "touch-pan-down";
  check "touch-pinch-zoom"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Touch.Handler) "touch";
  Test_helpers.check_invalid_input (module Tw.Touch.Handler) "touch-foo"

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid

let suite = ("touch", tests)
