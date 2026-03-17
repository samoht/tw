let check = Test_helpers.check_handler_roundtrip (module Tw.Divide.Handler)

let test_roundtrip () =
  check "divide-x";
  check "divide-y";
  check "divide-x-2";
  check "divide-y-4";
  check "divide-x-reverse";
  check "divide-y-reverse";
  check "divide-solid";
  check "divide-dashed";
  check "divide-dotted";
  check "divide-double";
  check "divide-none";
  check "divide-transparent";
  check "divide-current";
  check "divide-inherit"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Divide.Handler) "divide";
  Test_helpers.check_invalid_input (module Tw.Divide.Handler) "divide-foo"

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid

let suite = ("divide", tests)
