open Alcotest
module Sd = Tw_tools.String_diff

let test_equal () =
  let result = Sd.diff ~expected:"hello" "hello" in
  match result with
  | None -> ()
  | Some _ -> fail "Expected None for identical strings"

let test_simple_diff () =
  let result = Sd.diff ~expected:"abc" "abd" in
  match result with
  | Some diff ->
      check int "position" 2 diff.position;
      check int "line_expected" 0 diff.line_expected;
      check int "column_expected" 2 diff.column_expected;
      check int "line_actual" 0 diff.line_actual;
      check int "column_actual" 2 diff.column_actual
  | None -> fail "Expected Some diff for different strings"

let test_multiline_diff () =
  let expected = "line1\nline2\nline3" in
  let actual = "line1\nline2\nline4" in
  let result = Sd.diff ~expected actual in
  match result with
  | Some diff ->
      check int "line_expected" 2 diff.line_expected;
      check int "line_actual" 2 diff.line_actual;
      check int "column_expected" 4 diff.column_expected;
      check int "column_actual" 4 diff.column_actual;
      let exp_line, act_line = diff.diff_lines in
      check string "expected line" "line3" exp_line;
      check string "actual line" "line4" act_line
  | None -> fail "Expected Some diff for different strings"

let test_context () =
  let expected = "a\nb\nc\nd\ne" in
  let actual = "a\nb\nx\nd\ne" in
  let result = Sd.diff ~context_size:1 ~expected actual in
  match result with
  | Some diff -> (
      (* Should have 1 line of context before *)
      check int "context before length" 1 (List.length diff.context_before);
      (* Should have 1 line of context after *)
      check int "context after length" 1 (List.length diff.context_after);
      (match diff.context_before with
      | [ (a, b) ] ->
          check string "before exp" "b" a;
          check string "before act" "b" b
      | _ -> fail "Wrong context before");
      match diff.context_after with
      | [ (a, b) ] ->
          check string "after exp" "d" a;
          check string "after act" "d" b
      | _ -> fail "Wrong context after")
  | None -> fail "Expected Some diff for different strings"

let test_first_diff_pos () =
  check (option int) "identical strings" None (Sd.first_diff_pos "abc" "abc");
  check (option int) "diff at start" (Some 0) (Sd.first_diff_pos "abc" "xbc");
  check (option int) "diff at end" (Some 2) (Sd.first_diff_pos "abc" "abx");
  check (option int) "diff in middle" (Some 1) (Sd.first_diff_pos "abc" "axc");
  check (option int) "different lengths" (Some 3)
    (Sd.first_diff_pos "abc" "abcd")

let test_truncate_middle () =
  let s = "abcdefghijklmnopqrstuvwxyz" in
  check string "short enough" s (Sd.truncate_middle 30 s);
  let truncated = Sd.truncate_middle 10 s in
  check bool "has ellipsis" true (String.contains truncated '.');
  check bool "max length respected" true (String.length truncated <= 10)

let test_pp () =
  let expected = "hello world" in
  let actual = "hello warld" in
  match Sd.diff ~expected actual with
  | Some diff ->
      (* Check that the diff structure is properly populated *)
      check int "position" 7 diff.position;
      check int "line_expected" 0 diff.line_expected;
      check int "column_expected" 7 diff.column_expected;
      let exp_line, act_line = diff.diff_lines in
      check string "expected line" "hello world" exp_line;
      check string "actual line" "hello warld" act_line;
      (* Basic test that pp function works without crashing *)
      let buf = Buffer.create 256 in
      let fmt = Format.formatter_of_buffer buf in
      Sd.pp ~expected_label:"Expected" ~actual_label:"Actual" fmt diff;
      Format.pp_print_flush fmt ();
      let _output = Buffer.contents buf in
      ()
  | None -> fail "Expected diff"

let tests =
  [
    test_case "equal strings" `Quick test_equal;
    test_case "simple diff" `Quick test_simple_diff;
    test_case "multiline diff" `Quick test_multiline_diff;
    test_case "context lines" `Quick test_context;
    test_case "first_diff_pos" `Quick test_first_diff_pos;
    test_case "truncate_middle" `Quick test_truncate_middle;
    test_case "pretty printing" `Quick test_pp;
  ]

let suite = ("string_diff", tests)
