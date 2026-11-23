open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Text_transform.Handler)

let test_text_transform () =
  check "uppercase";
  check "lowercase";
  check "capitalize";
  check "normal-case"

let tests = [ test_case "text transform" `Quick test_text_transform ]
let suite = ("text_transform", tests)
