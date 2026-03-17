let check = Test_helpers.check_handler_roundtrip (module Tw.Overscroll.Handler)

let test_roundtrip () =
  check "overscroll-auto";
  check "overscroll-contain";
  check "overscroll-none";
  check "overscroll-x-auto";
  check "overscroll-x-contain";
  check "overscroll-x-none";
  check "overscroll-y-auto";
  check "overscroll-y-contain";
  check "overscroll-y-none"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Overscroll.Handler) "overscroll";
  Test_helpers.check_invalid_input
    (module Tw.Overscroll.Handler)
    "overscroll-foo"

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid

let suite = ("overscroll", tests)
