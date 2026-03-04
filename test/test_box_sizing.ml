let check = Test_helpers.check_handler_roundtrip (module Tw.Box_sizing.Handler)

let test_roundtrip () =
  check "box-border";
  check "box-content"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Box_sizing.Handler) "box-padding";
  Test_helpers.check_invalid_input (module Tw.Box_sizing.Handler) "box"

let tests =
  Alcotest.
    [
      test_case "roundtrip" `Quick test_roundtrip;
      test_case "invalid" `Quick test_invalid;
    ]

let suite = ("box_sizing", tests)
