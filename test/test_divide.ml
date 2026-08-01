let check =
  Test_helpers.check_handler_roundtrip (module Tw.Private.Divide.Handler)

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
  Test_helpers.check_invalid_input (module Tw.Private.Divide.Handler) "divide";
  Test_helpers.check_invalid_input
    (module Tw.Private.Divide.Handler)
    "divide-foo"

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

(* The widths lead the family and divide-x-reverse trails it, which is
   Tailwind's own order; the styles used to sort first, putting divide-dashed
   ahead of divide-y-4. *)
let divide_classes =
  [
    "divide-x";
    "divide-x-2";
    "divide-y";
    "divide-y-4";
    "divide-y-reverse";
    "divide-x-reverse";
    "divide-solid";
    "divide-dashed";
    "divide-current";
    "divide-gray-200";
    "border-2";
    "border-gray-500";
    "border-dashed";
  ]

let divide_utilities () =
  List.map (fun c -> Result.get_ok (Tw.of_string c)) divide_classes

let order_matches_tailwind () =
  Test_helpers.check_ordering_matches ~test_name:"divide order matches Tailwind"
    (Test_helpers.shuffle (divide_utilities ()))

(* divide-* and border-* write the same border properties, so what an element is
   actually bordered with is a rendering question, not only an ordering one. *)
let rendering_matches_tailwind () =
  Test_helpers.check_rendering_matches ~test_name:"divide renders like Tailwind"
    (divide_utilities ())

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "typed constructors" `Quick test_typed;
      Alcotest.test_case "order matches Tailwind" `Slow order_matches_tailwind;
      Alcotest.test_case "renders like Tailwind" `Slow
        rendering_matches_tailwind;
    ]

let suite = ("divide", tests)
