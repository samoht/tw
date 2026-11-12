open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Line_breaking.Handler)

let test_word_overflow_wrap () =
  check "break-normal";
  check "break-words";
  check "break-all";
  check "break-keep";
  check "overflow-wrap-normal";
  check "overflow-wrap-anywhere";
  check "overflow-wrap-break-word"

let test_hyphens () =
  check "hyphens-none";
  check "hyphens-manual";
  check "hyphens-auto"

let test_text_wrap () =
  check "text-wrap";
  check "text-nowrap";
  check "text-balance";
  check "text-pretty"

let tests =
  [
    test_case "text wrap" `Quick test_text_wrap;
    test_case "word/overflow wrap" `Quick test_word_overflow_wrap;
    test_case "hyphens" `Quick test_hyphens;
  ]

let suite = ("line_breaking", tests)
