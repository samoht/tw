open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Vertical_align.Handler)

let test_vertical_align () =
  check "align-baseline";
  check "align-top";
  check "align-middle";
  check "align-bottom";
  check "align-text-top";
  check "align-text-bottom";
  check "align-sub";
  check "align-super"

let tests = [ test_case "vertical align" `Quick test_vertical_align ]
let suite = ("vertical_align", tests)
