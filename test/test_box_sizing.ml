let check = Test_helpers.check_handler_roundtrip (module Tw.Box_sizing.Handler)

let test_roundtrip () =
  check "box-border";
  check "box-content"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Box_sizing.Handler) "box-padding";
  Test_helpers.check_invalid_input (module Tw.Box_sizing.Handler) "box"

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid

let suite = ("box_sizing", tests)
