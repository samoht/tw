open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Text_overflow.Handler)

let test_text_overflow_order () =
  Test_helpers.check_class_order ~test_name:"text-overflow order"
    [ "text-ellipsis"; "overflow-ellipsis"; "text-clip" ]

let test_text_overflow () =
  check "text-ellipsis";
  check "overflow-ellipsis";
  check "text-clip"

let tests =
  [
    test_case "text overflow" `Quick test_text_overflow;
    test_case "text overflow order" `Slow test_text_overflow_order;
  ]

let suite = ("text_overflow", tests)
