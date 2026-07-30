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

(* Every divide utility the parser accepts also has a typed constructor, and the
   two agree on the class name (issue #5). *)
let test_typed () =
  let open Tw in
  Test_helpers.check_typed_class "divide-x-2" (divide_x 2);
  Test_helpers.check_typed_class "divide-y-4" (divide_y 4);
  Test_helpers.check_typed_class "divide-x-reverse" divide_x_reverse;
  Test_helpers.check_typed_class "divide-y-reverse" divide_y_reverse;
  Test_helpers.check_typed_class "divide-blue-500" (divide_color blue);
  Test_helpers.check_typed_class "divide-gray-300"
    (divide_color ~shade:300 gray);
  Test_helpers.check_typed_class "divide-transparent" divide_transparent;
  Test_helpers.check_typed_class "divide-current" divide_current;
  Test_helpers.check_typed_class "divide-inherit" divide_inherit;
  Test_helpers.check_typed_class "divide-dashed" (divide_style Dashed)

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [ Alcotest.test_case "typed constructors" `Quick test_typed ]

let suite = ("divide", tests)
