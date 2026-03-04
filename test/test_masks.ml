let check = Test_helpers.check_handler_roundtrip (module Tw.Masks.Handler)

let test_roundtrip () =
  check "mask-none";
  check "mask-add";
  check "mask-exclude";
  check "mask-intersect";
  check "mask-subtract";
  check "mask-alpha";
  check "mask-luminance";
  check "mask-match";
  check "mask-type-alpha";
  check "mask-type-luminance";
  check "mask-auto";
  check "mask-clip-border";
  check "mask-clip-padding";
  check "mask-clip-content";
  check "mask-clip-fill";
  check "mask-clip-stroke";
  check "mask-clip-view";
  check "mask-no-clip";
  check "mask-origin-border";
  check "mask-origin-padding";
  check "mask-origin-content";
  check "mask-origin-fill";
  check "mask-origin-stroke";
  check "mask-origin-view"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Masks.Handler) "mask-foo";
  Test_helpers.check_invalid_input (module Tw.Masks.Handler) "mask"

let tests =
  Alcotest.
    [
      test_case "roundtrip" `Quick test_roundtrip;
      test_case "invalid" `Quick test_invalid;
    ]

let suite = ("masks", tests)
