open Alcotest
module Df = Tw_tools.Diff_format

let test_format_diff_basic () =
  let diff = Df.format_diff ~original:"abcdef" ~actual:"abcXdef" in
  (* Should include - and + lines and a caret marker *)
  check bool "has dash line" true (String.exists (fun c -> c = '-') diff);
  check bool "has plus line" true (String.exists (fun c -> c = '+') diff);
  check bool "has caret" true (String.contains diff '^')

let test_eprintf_diff_no_raise () =
  (* Should not raise *)
  Df.eprintf_diff ~original:"foo" ~actual:"bar"

let tests =
  [
    test_case "format basic" `Quick test_format_diff_basic;
    test_case "eprintf no raise" `Quick test_eprintf_diff_no_raise;
  ]

let suite = ("diff_format", tests)
