open Alcotest
module Df = Tw_tools.Diff_format

let test_equal () =
  let result = Df.diff ~expected:"hello" "hello" in
  match result with
  | `Equal -> ()
  | _ -> fail "Expected `Equal for identical strings"

let test_short_diff () =
  let result = Df.diff ~expected:"abc" "abd" in
  match result with
  | `Diff_short (exp, act) ->
      check string "expected" "abc" exp;
      check string "actual" "abd" act
  | _ -> fail "Expected `Diff_short for short strings"

let test_medium_diff () =
  let config = { Df.default_config with short_threshold = 5 } in
  let result = Df.diff ~config ~expected:"hello world" "hello warld" in
  match result with
  | `Diff_medium (exp, act, pos) ->
      check string "expected" "hello world" exp;
      check string "actual" "hello warld" act;
      check int "diff position" 7 pos
  | _ -> fail "Expected `Diff_medium for medium strings"

let test_long_diff () =
  let long1 = String.make 100 'a' ^ "difference" ^ String.make 100 'b' in
  let long2 = String.make 100 'a' ^ "different!" ^ String.make 100 'b' in
  let result = Df.diff ~expected:long1 long2 in
  match result with
  | `Diff_long (exp_window, act_window, _pos) ->
      (* Windows should contain ellipsis *)
      check bool "expected has ellipsis" true (String.contains exp_window '.');
      check bool "actual has ellipsis" true (String.contains act_window '.')
  | _ -> fail "Expected `Diff_long for long strings"

let test_first_diff_pos () =
  check (option int) "identical strings" None (Df.first_diff_pos "abc" "abc");
  check (option int) "diff at start" (Some 0) (Df.first_diff_pos "abc" "xbc");
  check (option int) "diff at end" (Some 2) (Df.first_diff_pos "abc" "abx");
  check (option int) "diff in middle" (Some 1) (Df.first_diff_pos "abc" "axc");
  check (option int) "different lengths" (Some 3)
    (Df.first_diff_pos "abc" "abcd")

let test_truncate_middle () =
  let s = "abcdefghijklmnopqrstuvwxyz" in
  check string "short enough" s (Df.truncate_middle 30 s);
  let truncated = Df.truncate_middle 10 s in
  check bool "has ellipsis" true (String.contains truncated '.');
  check bool "max length respected" true (String.length truncated <= 10)

let test_custom_config () =
  let config =
    { Df.default_config with max_width = 20; short_threshold = 10 }
  in
  let s1 = "short" in
  let s2 = "shirt" in
  let result = Df.diff ~config ~expected:s1 s2 in
  match result with
  | `Diff_short _ -> ()
  | _ -> fail "Config should treat as short diff"

let tests =
  [
    test_case "equal strings" `Quick test_equal;
    test_case "short diff" `Quick test_short_diff;
    test_case "medium diff" `Quick test_medium_diff;
    test_case "long diff" `Quick test_long_diff;
    test_case "first_diff_pos" `Quick test_first_diff_pos;
    test_case "truncate_middle" `Quick test_truncate_middle;
    test_case "custom config" `Quick test_custom_config;
  ]

let suite = ("diff_format", tests)
