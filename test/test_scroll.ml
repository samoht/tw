let check = Test_helpers.check_handler_roundtrip (module Tw.Scroll.Handler)

let test_roundtrip () =
  check "scroll-m-0";
  check "scroll-m-4";
  check "scroll-mx-2";
  check "scroll-my-4";
  check "scroll-mt-8";
  check "scroll-mr-2";
  check "scroll-mb-4";
  check "scroll-ml-6";
  check "scroll-p-0";
  check "scroll-p-4";
  check "scroll-px-2";
  check "scroll-py-4";
  check "scroll-pt-8";
  check "scroll-pr-2";
  check "scroll-pb-4";
  check "scroll-pl-6"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Scroll.Handler) "scroll";
  Test_helpers.check_invalid_input (module Tw.Scroll.Handler) "scroll-foo"

let tests =
  Alcotest.
    [
      test_case "roundtrip" `Quick test_roundtrip;
      test_case "invalid" `Quick test_invalid;
    ]

let suite = ("scroll", tests)
