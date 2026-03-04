let check = Test_helpers.check_handler_roundtrip (module Tw.Columns.Handler)

let test_roundtrip () =
  check "columns-auto";
  check "columns-1";
  check "columns-2";
  check "columns-3";
  check "columns-12";
  check "columns-3xs";
  check "columns-2xs";
  check "columns-xs";
  check "columns-sm";
  check "columns-md";
  check "columns-lg";
  check "columns-xl";
  check "columns-2xl";
  check "columns-3xl";
  check "columns-4xl";
  check "columns-5xl";
  check "columns-6xl";
  check "columns-7xl"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Columns.Handler) "columns";
  Test_helpers.check_invalid_input (module Tw.Columns.Handler) "columns-abc"

let tests =
  Alcotest.
    [
      test_case "roundtrip" `Quick test_roundtrip;
      test_case "invalid" `Quick test_invalid;
    ]

let suite = ("columns", tests)
