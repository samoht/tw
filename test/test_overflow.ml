let check = Test_helpers.check_handler_roundtrip (module Tw.Overflow.Handler)

let test_roundtrip () =
  check "overflow-auto";
  check "overflow-hidden";
  check "overflow-clip";
  check "overflow-visible";
  check "overflow-scroll";
  check "overflow-x-auto";
  check "overflow-x-clip";
  check "overflow-x-hidden";
  check "overflow-x-visible";
  check "overflow-x-scroll";
  check "overflow-y-auto";
  check "overflow-y-clip";
  check "overflow-y-hidden";
  check "overflow-y-visible";
  check "overflow-y-scroll"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Overflow.Handler) "overflow";
  Test_helpers.check_invalid_input (module Tw.Overflow.Handler) "overflow-foo"

let tests =
  Alcotest.
    [
      test_case "roundtrip" `Quick test_roundtrip;
      test_case "invalid" `Quick test_invalid;
    ]

let suite = ("overflow", tests)
