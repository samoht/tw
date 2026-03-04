let check =
  Test_helpers.check_handler_roundtrip (module Tw.Mask_gradient.Handler)

let test_roundtrip () =
  check "mask-t-from-0%";
  check "mask-t-to-100%";
  check "mask-b-from-50%";
  check "mask-b-to-50%";
  check "mask-l-from-0%";
  check "mask-r-to-100%";
  check "mask-x-from-0%";
  check "mask-y-to-100%";
  check "mask-radial"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Mask_gradient.Handler) "mask-foo"

let tests =
  Alcotest.
    [
      test_case "roundtrip" `Quick test_roundtrip;
      test_case "invalid" `Quick test_invalid;
    ]

let suite = ("mask_gradient", tests)
