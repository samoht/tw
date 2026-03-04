let check = Test_helpers.check_handler_roundtrip (module Tw.Flex_layout.Handler)

let test_roundtrip () =
  check "flex-row";
  check "flex-row-reverse";
  check "flex-col";
  check "flex-col-reverse";
  check "flex-wrap";
  check "flex-wrap-reverse";
  check "flex-nowrap"

let test_invalid () =
  Test_helpers.check_invalid_input
    (module Tw.Flex_layout.Handler)
    "flex-diagonal";
  Test_helpers.check_invalid_input (module Tw.Flex_layout.Handler) "flex-foo"

let tests =
  Alcotest.
    [
      test_case "roundtrip" `Quick test_roundtrip;
      test_case "invalid" `Quick test_invalid;
    ]

let suite = ("flex_layout", tests)
