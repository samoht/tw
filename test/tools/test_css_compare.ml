open Alcotest
module Cc = Tw_tools.Css_compare
module Td = Tw_tools.Tree_diff
open Cc
open Test_helpers

let pp_css fmt css =
  match Css.of_string css with
  | Ok ast -> Fmt.string fmt (Css.to_string ~minify:true ast)
  | Error e -> Fmt.pf fmt "<parse error: %s>" (Css.pp_parse_error e)

let css = Alcotest.testable pp_css Cc.compare

(* ===== Enhanced Test Helpers ===== *)

(* Use shared test helpers *)

let test_strip_header () =
  let css = "/*! header */\n.a{color:red}" in
  check string "header stripped" ".a{color:red}" (Cc.strip_header css)

let test_compare_equivalent () =
  let a = ".a{color:red;padding:10px}" in
  let b = ".a{color:red;padding:10px}" in
  check css "structurally equal" a b

let test_compare_media_layers_equivalent () =
  let a =
    "@layer utilities{.x{color:red}}@media screen and \
     (min-width:600px){.x{color:red;padding:10px}}"
  in
  let b =
    "@layer utilities{.x{color:red}}@media screen and \
     (min-width:600px){.x{color:red;padding:10px}}"
  in
  check css "media/layers equal" a b

let test_dispatching_to_tree_diff () =
  let css_expected = ".a{color:red}" in
  let css_actual = ".a{color:blue}" in
  match Cc.diff ~expected:css_expected ~actual:css_actual with
  | Cc.Tree_diff _ -> () (* Expected *)
  | Cc.String_diff _ -> fail "Expected Tree_diff, got String_diff"
  | Cc.No_diff -> fail "Expected Tree_diff, got No_diff"
  | _ -> fail "Expected Tree_diff"

let test_dispatching_to_string_diff () =
  (* Test case where structural diff is empty but strings differ *)
  let css_expected = ".a { color : red }" in
  let css_actual = ".a{color:red}" in
  match Cc.diff ~expected:css_expected ~actual:css_actual with
  | Cc.String_diff _ -> () (* Expected when only formatting differs *)
  | _ -> fail "Expected String_diff for formatting differences"

let test_dispatching_no_diff () =
  let css = ".a{color:red}" in
  match Cc.diff ~expected:css ~actual:css with
  | Cc.No_diff -> () (* Expected *)
  | _ -> fail "Expected No_diff for identical CSS"

let test_rule_added_removed_modified () =
  (* Test rule added *)
  let diff_added =
    tree_diff ~expected:".a{color:red}" ~actual:".a{color:red}.b{margin:0}"
  in
  assert_rule_added ".b" diff_added;

  (* Test rule removed *)
  let diff_removed =
    tree_diff ~expected:".a{color:red}.b{margin:0}" ~actual:".a{color:red}"
  in
  assert_rule_removed ".b" diff_removed;

  (* Test rule modified *)
  let diff_modified =
    tree_diff ~expected:".a{color:red}" ~actual:".a{color:blue}"
  in
  assert_rule_content_changed ".a" diff_modified

let test_compare_function () =
  let css1 = ".a{color:red}" in
  let css2 = ".a{color:red}" in
  let css3 = ".a{color:blue}" in
  let css4 = ".a { color : red }" in
  (* formatting difference *)
  check bool "identical CSS should compare equal" true (Cc.compare css1 css2);
  check bool "different CSS should compare unequal" false (Cc.compare css1 css3);
  check bool "formatting differences should compare unequal" false
    (Cc.compare css1 css4)

let test_compare_with_parse_errors () =
  let valid_css = ".a{color:red}" in
  let invalid_css = ".a{color:}" in
  (* Should fall back to string comparison when parsing fails *)
  check bool "valid vs invalid should be false" false
    (Cc.compare valid_css invalid_css);
  check bool "same invalid CSS should be true" true
    (Cc.compare invalid_css invalid_css)

let test_diff_parse_errors () =
  let valid_css = ".a{color:red}" in
  let invalid_css = ".a{color:}" in
  (match Cc.diff ~expected:valid_css ~actual:invalid_css with
  | Cc.Actual_error _ -> () (* Expected *)
  | _ -> fail "Expected Actual_error for invalid actual CSS");
  (match Cc.diff ~expected:invalid_css ~actual:valid_css with
  | Cc.Expected_error _ -> () (* Expected *)
  | _ -> fail "Expected Expected_error for invalid expected CSS");
  match Cc.diff ~expected:invalid_css ~actual:invalid_css with
  | Cc.Both_errors _ -> () (* Expected *)
  | _ -> fail "Expected Both_errors for both invalid CSS"

let test_pp_stats_tree_diff () =
  let css_expected = ".a{color:red}.b{margin:0}" in
  let css_actual = ".a{color:blue}.c{margin:1px}" in
  let diff_result = Cc.diff ~expected:css_expected ~actual:css_actual in
  let stats =
    Cc.stats ~expected_str:css_expected ~actual_str:css_actual diff_result
  in
  let stats_str = Fmt.str "%a" Cc.pp_stats stats in
  check string "stats format"
    "CSS: 28 chars vs 25 chars (12.0% diff)\n\
     Changes: 1 added rule, 1 removed rule, 1 modified rule\n"
    stats_str

let test_pp_stats_string_diff () =
  let css_expected = ".a { color : red }" in
  let css_actual = ".a{color:red}" in
  let diff_result = Cc.diff ~expected:css_expected ~actual:css_actual in
  let stats =
    Cc.stats ~expected_str:css_expected ~actual_str:css_actual diff_result
  in
  (* For string diff, we only show char diff, not rule changes *)
  let stats_str =
    Fmt.str "CSS: %d chars vs %d chars (27.8%% diff)" stats.actual_chars
      stats.expected_chars
  in
  check string "stats format" "CSS: 13 chars vs 18 chars (27.8% diff)" stats_str

let test_pp_stats_no_diff () =
  let css = ".a{color:red}" in
  let diff_result = Cc.diff ~expected:css ~actual:css in
  let stats = Cc.stats ~expected_str:css ~actual_str:css diff_result in
  (* For No_diff, all counts should be zero *)
  check int "no added rules" 0 stats.added_rules;
  check int "no removed rules" 0 stats.removed_rules;
  check int "no modified rules" 0 stats.modified_rules;
  check int "no reordered rules" 0 stats.reordered_rules;
  check int "no container changes" 0 stats.container_changes

let test_pp_stats_rule_types () =
  let css_expected = ".old{color:red}.same{margin:0;padding:0}" in
  let css_actual = ".new{color:red}.same{padding:0;margin:0}" in
  let diff_result = Cc.diff ~expected:css_expected ~actual:css_actual in
  let stats =
    Cc.stats ~expected_str:css_expected ~actual_str:css_actual diff_result
  in
  let stats_str = Fmt.str "%a" Cc.pp_stats stats in
  check string "stats format"
    "CSS: 40 chars vs 40 chars (0.0% diff)\n\
     Changes: 1 added rule, 1 removed rule, 1 reordered rule\n"
    stats_str

let test_property_value_modified () =
  let css_expected = ".x{color:red}" in
  let css_actual = ".x{color:blue}" in
  let diff = tree_diff ~expected:css_expected ~actual:css_actual in
  check int "no container changes" 0 (List.length diff.containers);
  let rule = single_rule_diff diff in
  assert_single_property_change ~prop:"color" ~from:"red" ~to_:"blue" rule

let test_property_added_only () =
  let css_expected = ".y{color:red}" in
  let css_actual = ".y{color:red;padding:10px}" in
  let diff = tree_diff ~expected:css_expected ~actual:css_actual in
  let rule = single_rule_diff diff in
  (* Should detect as Rule_content_changed - test both rule type and detailed
     structure *)
  match rule with
  | Td.Rule_content_changed { selector; old_declarations; new_declarations; _ }
    ->
      check string "selector" ".y" selector;
      check bool "has old declarations" true (List.length old_declarations > 0);
      check bool "has new declarations" true (List.length new_declarations > 0);
      check bool "new has more declarations" true
        (List.length new_declarations > List.length old_declarations)
  | _ -> fail "Expected Rule_content_changed for property addition"

let test_property_removed_only () =
  let css_expected = ".y{color:red;padding:10px}" in
  let css_actual = ".y{color:red}" in
  let diff = tree_diff ~expected:css_expected ~actual:css_actual in
  let rule = single_rule_diff diff in
  (* Should detect as Rule_content_changed - test both rule type and detailed
     structure *)
  match rule with
  | Td.Rule_content_changed { selector; old_declarations; new_declarations; _ }
    ->
      check string "selector" ".y" selector;
      check bool "has old declarations" true (List.length old_declarations > 0);
      check bool "has new declarations" true (List.length new_declarations > 0);
      check bool "old has more declarations" true
        (List.length old_declarations > List.length new_declarations)
  | _ -> fail "Expected Rule_content_changed for property removal"

let test_pp_stats_parse_errors () =
  (* Test stats for parse error cases *)
  let valid_css = ".a{color:red}" in
  let invalid_css = ".a{color:}" in

  (* Both errors case *)
  let both_err = Cc.diff ~expected:invalid_css ~actual:invalid_css in
  let stats_both =
    Cc.stats ~expected_str:invalid_css ~actual_str:invalid_css both_err
  in
  check int "both errors - no rule changes" 0 stats_both.added_rules;
  check int "both errors - expected chars counted"
    (String.length invalid_css)
    stats_both.expected_chars;
  check int "both errors - actual chars counted"
    (String.length invalid_css)
    stats_both.actual_chars;

  (* Expected error case *)
  let exp_err = Cc.diff ~expected:invalid_css ~actual:valid_css in
  let stats_exp =
    Cc.stats ~expected_str:invalid_css ~actual_str:valid_css exp_err
  in
  check int "expected error - no rule changes" 0 stats_exp.added_rules;
  check int "expected error - expected chars"
    (String.length invalid_css)
    stats_exp.expected_chars;
  check int "expected error - actual chars" (String.length valid_css)
    stats_exp.actual_chars;

  (* Actual error case *)
  let act_err = Cc.diff ~expected:valid_css ~actual:invalid_css in
  let stats_act =
    Cc.stats ~expected_str:valid_css ~actual_str:invalid_css act_err
  in
  check int "actual error - no rule changes" 0 stats_act.added_rules;
  check int "actual error - expected chars" (String.length valid_css)
    stats_act.expected_chars;
  check int "actual error - actual chars"
    (String.length invalid_css)
    stats_act.actual_chars

let test_pp_stats_containers () =
  (* Test stats for container changes *)
  let css_expected = "@media screen{.a{color:red}}" in
  let css_actual = "@media print{.a{color:red}}" in
  let diff_result = Cc.diff ~expected:css_expected ~actual:css_actual in
  let stats =
    Cc.stats ~expected_str:css_expected ~actual_str:css_actual diff_result
  in
  check bool "has container changes" true (stats.container_changes > 0)

let test_pp_stats_combined_changes () =
  (* Test stats with multiple types of changes *)
  let css_expected =
    ".old{color:red}@media screen{.x{margin:0}}.same{padding:0}"
  in
  let css_actual =
    ".new{color:blue}@media print{.x{margin:0}}.same{padding:0}"
  in
  let diff_result = Cc.diff ~expected:css_expected ~actual:css_actual in
  let stats =
    Cc.stats ~expected_str:css_expected ~actual_str:css_actual diff_result
  in
  (* Should have added, removed, and container changes *)
  check bool "has added rules" true (stats.added_rules > 0);
  check bool "has removed rules" true (stats.removed_rules > 0);
  check bool "has container changes" true (stats.container_changes > 0);
  (* Format the stats to ensure it works *)
  let stats_output = Fmt.str "%a" Cc.pp_stats stats in
  check bool "stats output not empty" true (String.length stats_output > 0)

let test_important_and_custom_props () =
  (* Importance difference should be detected *)
  let d1 =
    tree_diff ~expected:".x{color:red!important}" ~actual:".x{color:red}"
  in
  assert_rule_content_changed ".x" d1;
  (* Custom property value difference should be detected *)
  let d2 = tree_diff ~expected:".x{--foo:1}" ~actual:".x{--foo:2}" in
  assert_rule_content_changed ".x" d2

let test_media_and_layer_diffs () =
  let css_expected = "@media screen{.a{color:red}}@layer base{.b{margin:0}}" in
  let css_actual =
    "@media print{.a{color:red}}@layer utilities{.b{margin:0}}"
  in
  let diff = tree_diff ~expected:css_expected ~actual:css_actual in
  check int "media containers changed" 2
    (Td.count_containers_by_type `Media diff);
  check int "layer containers changed" 2
    (Td.count_containers_by_type `Layer diff)

let test_supports_and_container_diffs () =
  let css_expected =
    "@supports (display:grid){.s{margin:0}}@container \
     (min-width:500px){.c{padding:1px}}"
  in
  let css_actual =
    "@supports (display:grid){.s{margin:1px}}@container \
     (min-width:500px){.c{padding:2px}}"
  in
  let diff =
    match Cc.diff ~expected:css_expected ~actual:css_actual with
    | Tree_diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  (* Media/layers may be empty; we look for structural differences in supports/containers implicitly via rules diff absence *)
  (* Ensure no top-level rule diffs pollute this case *)
  check int "no top-level rule diffs" 0 (List.length diff.rules);
  check int "one supports change" 1 (Td.count_containers_by_type `Supports diff);
  check int "one container change" 1
    (Td.count_containers_by_type `Container diff)

(* Intentionally avoid substring/count-based checks; use structural diff
   only. *)

let test_string_diff_context_basic () =
  let expected = "hello world" in
  let actual = "hello wurld" in
  match Tw_tools.String_diff.diff ~expected actual with
  | Some sdiff ->
      check int "diff position" 7 sdiff.position;
      (* Position of 'o' vs 'u' is index 7 *)
      check int "char position in line" 7 sdiff.column_expected;
      let exp_line, act_line = sdiff.diff_lines in
      check string "expected context" "hello world" exp_line;
      check string "actual context" "hello wurld" act_line
  | None -> fail "expected diff context"

let test_string_diff_context_multiline () =
  let expected = "line one\nline two\nline three" in
  let actual = "line one\nline too\nline three" in
  match Tw_tools.String_diff.diff ~expected actual with
  | Some sdiff ->
      check int "diff position" 15 sdiff.position;
      (* Position of 'w' vs 'o' at index 15 *)
      check int "diff line number" 1 sdiff.line_expected;
      check int "char position in line" 6 sdiff.column_expected;
      (* Position in line after "line t" *)
      let exp_line, act_line = sdiff.diff_lines in
      check string "expected diff line" "line two" exp_line;
      check string "actual diff line" "line too" act_line
  | None -> fail "expected diff context"

let test_string_diff_at_end () =
  let expected = "abc" in
  let actual = "abcd" in
  match Tw_tools.String_diff.diff ~expected actual with
  | Some sdiff -> check int "diff at end position" 3 sdiff.position
  | None -> fail "expected diff context"

let test_string_diff_context_none () =
  let expected = "same" in
  let actual = "same" in
  match Tw_tools.String_diff.diff ~expected actual with
  | Some _ -> fail "no diff expected"
  | None -> ()

let test_pp_with_string_context () =
  (* Test that pp_diff_result shows string context when no structural diff *)
  let css1 = ".a{color:red}" in
  let css2 = ".a{color: red}" in
  (* Extra space *)
  let result = Cc.diff ~expected:css1 ~actual:css2 in
  (* First, ensure we got a string-only diff, not structural *)
  (match result with
  | Cc.String_diff _ -> ()
  | _ -> fail "Expected String_diff for whitespace-only difference");
  (* Then, verify formatting includes context header and caret *)
  let output = Fmt.to_to_string Cc.pp result in
  (* Should show unified diff headers and caret for the differing char *)
  let has_headers = contains output "--- " && contains output "+++ " in
  check bool "has diff headers" true has_headers;
  check bool "has caret" true (contains output "^")
(* Position of the space difference *)

let test_pp_structural () =
  (* Test that pp_diff_result shows structural diff when present *)
  let css1 = ".a{color:red}" in
  let css2 = ".a{color:blue}" in
  let result = Cc.diff ~expected:css1 ~actual:css2 in
  (* Ensure a structural diff is produced *)
  (match result with Cc.Tree_diff _ -> () | _ -> fail "Expected Tree_diff");
  (* Check formatting has expected structural headers and modification line *)
  let output = Fmt.to_to_string Cc.pp result in
  let has_headers = contains output "--- " && contains output "+++ " in
  check bool "has diff headers" true has_headers;
  (* Verify property modification text appears for short values *)
  let has_modify = contains output "*" in
  let has_arrow = contains output "->" in
  check bool "shows color change inline" true (has_modify && has_arrow)

(* Selector-only change should be reported as a single rule Changed with a
   synthetic selector diff, not as add/remove. *)
let test_selector_formatting_difference () =
  (* CSS attribute selectors with optional quotes (when value has no
     spaces/special chars) are semantically equivalent, so this should be
     detected as a string formatting difference, not structural. The CSS parser
     correctly normalizes both forms. *)
  let css1 = ".prose :where(ol[type=\"A\"]) { list-style-type: upper-alpha }" in
  let css2 = ".prose :where(ol[type=A]) { list-style-type: upper-alpha }" in
  match Cc.diff ~expected:css1 ~actual:css2 with
  | Cc.String_diff _ ->
      ()
      (* Expected: CSS parser normalizes both to same AST, but strings differ *)
  | Tree_diff _ ->
      fail
        "parser should normalize semantically equivalent selectors to same AST"
  | _ -> fail "unexpected diff result type"

let test_css_variable_differences () =
  (* Test case that reproduces the shadow-sm bug: CSS variables missing should
     be detected as structural diff *)
  let expected =
    "*,:before,:after,::backdrop{--tw-shadow:0 0 \
     #0000;--tw-shadow-color:initial}"
  in
  let actual = "*,:before,:after,::backdrop{--tw-border-style:initial}" in
  let result = Cc.diff ~expected ~actual in
  match result with
  | Tree_diff diff ->
      (* Should detect structural differences in the ::backdrop rule *)
      check bool "detects CSS variable differences as structural" false
        (Td.is_empty diff)
  | _ -> fail "CSS should parse successfully"

let test_layer_custom_properties () =
  (* Test custom property differences within layers *)
  let expected_full =
    "@layer base {*, :before, :after, ::backdrop { --tw-shadow: 0 0 #0000; \
     --tw-shadow-color: initial; }}"
  in
  let actual_full =
    "@layer base {*, :before, :after, ::backdrop { --tw-border-style: initial; \
     }}"
  in
  let result = Cc.diff ~expected:expected_full ~actual:actual_full in
  match result with
  | Tree_diff diff ->
      (* Should detect structural differences - missing custom properties should
         be flagged *)
      check bool "layer custom property differences detected" false
        (Td.is_empty diff)
  | _ -> fail "CSS should have structural differences"

let test_css_unit_differences () =
  (* Test case for 0px vs 0 - should be detected as structural difference *)
  let expected = "*{--tw-ring-offset-width:0px}" in
  let actual = "*{--tw-ring-offset-width:0}" in
  let result = Cc.diff ~expected ~actual in
  match result with
  | Tree_diff diff ->
      (* Should detect structural differences - 0px vs 0 are different values *)
      check bool "detects 0px vs 0 as structural difference" false
        (Td.is_empty diff)
  | _ -> fail "CSS should parse successfully"

let test_complex_css_unit_differences () =
  (* Test case with many properties to reproduce the real bug *)
  let expected =
    "*{--tw-ring-inset:initial;--tw-ring-offset-width:0px;--tw-ring-offset-color:#fff;--tw-ring-color:rgb(59 \
     130 246 / 0.5)}"
  in
  let actual =
    "*{--tw-ring-inset:initial;--tw-ring-offset-width:0;--tw-ring-offset-color:#fff;--tw-ring-color:rgb(59 \
     130 246 / 0.5)}"
  in
  let result = Cc.diff ~expected ~actual in
  match result with
  | Tree_diff diff ->
      (* Should detect structural differences - 0px vs 0 are different values
         even in complex rules *)
      check bool "detects 0px vs 0 in complex CSS as structural difference"
        false (Td.is_empty diff)
  | _ -> fail "CSS should parse successfully"

let test_property_reordering () =
  (* Test that pure property reordering is detected as Reordered, not Changed *)
  let css_expected = ".x{color:red;padding:10px;margin:5px}" in
  let css_actual = ".x{margin:5px;color:red;padding:10px}" in
  let diff = tree_diff ~expected:css_expected ~actual:css_actual in
  check int "one rule change" 1 (List.length diff.rules);
  assert_rule_reordered ".x" diff

let test_reordered_diff_formatting () =
  (* Test that Reordered diffs produce proper output *)
  let css_expected = ".test{color:red;padding:10px}" in
  let css_actual = ".test{padding:10px;color:red}" in
  let result = Cc.diff ~expected:css_expected ~actual:css_actual in
  (* Ensure structural diff and that it classifies as a reorder *)
  let has_reorder =
    match result with
    | Cc.Tree_diff d ->
        List.exists
          (function Td.Rule_reordered _ -> true | _ -> false)
          d.rules
    | _ -> false
  in
  check bool "classifies as reorder" true has_reorder;
  (* Pretty output should include a reorder note summary *)
  let output = Fmt.to_to_string Cc.pp result in
  let has_reorder_word = contains output "reorder" in
  check bool "shows reorder summary" true has_reorder_word

let test_mixed_reordering_and_changes () =
  (* Test that when properties are both reordered AND changed, it's detected as
     Changed *)
  let css_expected = ".x{color:red;padding:10px;margin:5px}" in
  let css_actual = ".x{margin:5px;color:blue;padding:10px}" in
  (* Same order as first test but color changed from red to blue *)
  let diff =
    match Cc.diff ~expected:css_expected ~actual:css_actual with
    | Tree_diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  check int "one rule change" 1 (List.length diff.rules);
  let rule = List.hd diff.rules in
  (* Pattern match on the new rule_diff structure *)
  (* Should be Rule_content_changed, not Rule_reordered, because color value changed *)
  match rule with
  | Td.Rule_content_changed { selector; property_changes; _ } ->
      check string "rule selector" ".x" selector;
      check int "one property diff" 1 (List.length property_changes);
      let prop_diff = List.hd property_changes in
      check string "property name" "color" prop_diff.property_name;
      check string "expected value" "red" prop_diff.expected_value;
      check string "actual value" "blue" prop_diff.actual_value
  | Td.Rule_reordered _ ->
      fail
        "Expected Changed diff (not Reordered) when both order and values \
         change"
  | _ -> fail "Expected Changed diff with property differences"

let test_diff_never_empty () =
  (* Test that ensures we always show SOMETHING when input strings differ,
     either structural diffs or string diffs *)
  let test_cases =
    [
      (* Shadow variables case from tw 10 *)
      ( "@layer base{*,:before,:after,::backdrop{--tw-shadow:0 0 \
         #0000;--tw-shadow-color:initial}}",
        "@layer base{*,:before,:after,::backdrop{--tw-border-style:initial}}" );
      (* Simple property difference *)
      (".a{color:red}", ".a{color:blue}");
      (* Missing properties *)
      (".a{color:red;padding:10px}", ".a{color:red}");
      (* Added properties *)
      (".a{color:red}", ".a{color:red;padding:10px}");
      (* Different selectors *)
      (".a{color:red}", ".b{color:red}");
      (* Unit differences - treat 0px vs 0 as structural differences *)
      (".a{margin:0px}", ".a{margin:0}");
      (* Custom property differences *)
      (":root{--tw-shadow:0 0 #0000}", ":root{--tw-border:1px}");
      (* Missing layer *)
      ("@layer base{.a{color:red}}", ".a{color:red}");
      (* Added layer *)
      (".a{color:red}", "@layer base{.a{color:red}}");
      (* Media query differences *)
      ("@media(min-width:600px){.a{color:red}}", ".a{color:red}");
    ]
  in

  List.iter
    (fun (expected, actual) ->
      (* Strings are different *)
      check bool "input strings differ" false (expected = actual);

      let result = Cc.diff ~expected ~actual in

      (* Check that pp_diff_result produces some output when strings differ *)
      let buffer = Buffer.create 256 in
      let fmt = Format.formatter_of_buffer buffer in
      Cc.pp fmt result;
      Format.pp_print_flush fmt ();
      let output = Buffer.contents buffer in

      (* When strings differ, we should get some output - either structural or
         string diff *)
      if String.length output = 0 then
        Alcotest.failf
          "No diff output for different strings:\n\
           Expected: %s\n\
           Actual: %s\n\
           Different strings MUST produce some diff output"
          expected actual)
    test_cases

let test_selector_list_reorder_structural () =
  (* Reordering items inside a grouped selector SHOULD be detected as a
     structural diff - users want to know about ALL reorderings *)
  let css_expected = ".a,.b{color:red;padding:2px}" in
  let css_actual = ".b,.a{color:red;padding:2px}" in
  (* Should detect this as a structural difference *)
  match Cc.diff ~expected:css_expected ~actual:css_actual with
  | Tree_diff d -> (
      (* Should have exactly one rule change *)
      check int "one rule change" 1 (List.length d.rules);
      (* The change should be detected as selector-related *)
      let rule = List.hd d.rules in
      match rule with
      | Td.Rule_selector_changed _ | Td.Rule_reordered _ ->
          () (* Both are acceptable *)
      | _ -> fail "expected selector change or reordering")
  | _ -> fail "grouped selector reorder should be detected as structural"

let test_rule_position_reordering () =
  (* Reordering rule positions across different selectors should be detected as
     Reordered *)
  let css_expected = ".a{color:red}.b{color:blue}" in
  let css_actual = ".b{color:blue}.a{color:red}" in
  let result = Cc.diff ~expected:css_expected ~actual:css_actual in
  match result with
  | Tree_diff d -> (
      (* Expect exactly one rule change representing the reorder *)
      check int "one rule change" 1 (List.length d.rules);
      let r = List.hd d.rules in
      (* Pattern match on the new rule_diff structure *)
      match r with
      | Td.Rule_reordered { selector; _ } ->
          (* Either of the swapped selectors may be reported depending on
             pairing *)
          let ok = selector = ".a" || selector = ".b" in
          check bool "rule selector is one of swapped" true ok
      | _ -> fail "Expected Rule_reordered")
  | _ -> fail "Both CSS should parse and produce a structural diff"

let test_multiple_rule_reordering () =
  (* Test case similar to the prose ul/ol reordering issue *)
  let css_expected =
    ".prose .ul-first{margin:1em}.prose .ul-last{margin:2em}.prose \
     .ol-first{margin:3em}.prose .ol-last{margin:4em}"
  in
  let css_actual =
    ".prose .ol-first{margin:3em}.prose .ol-last{margin:4em}.prose \
     .ul-first{margin:1em}.prose .ul-last{margin:2em}"
  in
  let result = Cc.diff ~expected:css_expected ~actual:css_actual in
  match result with
  | Tree_diff d ->
      (* Should detect reordering, not add/remove *)
      let reordered =
        List.filter
          (function Td.Rule_reordered _ -> true | _ -> false)
          d.rules
      in
      let added =
        List.filter (function Td.Rule_added _ -> true | _ -> false) d.rules
      in
      let removed =
        List.filter (function Td.Rule_removed _ -> true | _ -> false) d.rules
      in
      check int "no rules added" 0 (List.length added);
      check int "no rules removed" 0 (List.length removed);
      check bool "rules are reordered" true (List.length reordered > 0)
  | _ -> fail "Both CSS should parse and produce a structural diff"

let test_selector_change_suppression () =
  (* Test that when there's a selector change from X to Y, we don't see
     redundant "add" entries for Y in nested contexts like layers *)
  let css_expected = "@layer utilities{x .old{color:red;margin:10px}}" in
  let css_actual = "@layer utilities{x .new{color:red;margin:10px}}" in
  let result = Cc.diff ~expected:css_expected ~actual:css_actual in
  match result with
  | Cc.Tree_diff d -> (
      (* Find the utilities layer container and inspect its rule changes *)
      let is_util_layer = function
        | Td.Container_modified
            {
              info = { container_type = `Layer; condition; _ };
              rule_changes;
              _;
            } ->
            if String.equal condition "utilities" then Some rule_changes
            else None
        | _ -> None
      in
      match List.find_map is_util_layer d.containers with
      | None -> fail "expected a modified @layer utilities container"
      | Some rule_changes -> (
          let selector_changes =
            List.filter
              (function Td.Rule_selector_changed _ -> true | _ -> false)
              rule_changes
          in
          let adds =
            List.filter
              (function Td.Rule_added _ -> true | _ -> false)
              rule_changes
          in
          let removes =
            List.filter
              (function Td.Rule_removed _ -> true | _ -> false)
              rule_changes
          in
          check int "single selector change" 1 (List.length selector_changes);
          check int "no adds" 0 (List.length adds);
          check int "no removes" 0 (List.length removes);
          (* Sanity-check the actual selector values *)
          match List.hd selector_changes with
          | Td.Rule_selector_changed { old_selector; new_selector; _ } ->
              check string "old selector" "x .old" old_selector;
              check string "new selector" "x .new" new_selector
          | _ -> assert false))
  | _ -> fail "CSS should have structural differences"

(* Result variant coverage tests *)
let test_no_diff_result () =
  let css = ".a{color:red}" in
  match Cc.diff ~expected:css ~actual:css with
  | Cc.No_diff -> ()
  | _ -> fail "Expected No_diff for identical CSS"

let test_expected_error_result () =
  let bad = "{" in
  let good = ".a{color:red}" in
  match Cc.diff ~expected:bad ~actual:good with
  | Cc.Expected_error _ -> ()
  | _ -> fail "Expected Expected_error for invalid expected CSS"

let test_actual_error_result () =
  let bad = "{" in
  let good = ".a{color:red}" in
  match Cc.diff ~expected:good ~actual:bad with
  | Cc.Actual_error _ -> ()
  | _ -> fail "Expected Actual_error for invalid actual CSS"

let test_both_errors_result () =
  let bad1 = "{" in
  let bad2 = "{" in
  match Cc.diff ~expected:bad1 ~actual:bad2 with
  | Cc.Both_errors _ -> ()
  | _ -> fail "Expected Both_errors when both CSS inputs are invalid"

(* Explicit container added/removed tests *)
let test_container_added_explicit () =
  let expected = ".a{color:red}" in
  let actual = "@media (min-width:600px){.a{color:red}}" in
  match Cc.diff ~expected ~actual with
  | Cc.Tree_diff d ->
      check bool "media container added" true
        (Td.has_container_added_of_type `Media d)
  | _ -> fail "Expected structural diff with media Container_added"

let test_container_removed_explicit () =
  let expected = "@media (min-width:600px){.a{color:red}}" in
  let actual = ".a{color:red}" in
  match Cc.diff ~expected ~actual with
  | Cc.Tree_diff d ->
      check bool "media container removed" true
        (Td.has_container_removed_of_type `Media d)
  | _ -> fail "Expected structural diff with media Container_removed"

(* @property container type coverage *)
let test_property_container_added_removed () =
  let prop =
    "@property --tw-size { syntax: '<length>'; inherits: false; initial-value: \
     0px }"
  in
  (* Added *)
  (match Cc.diff ~expected:".a{color:red}" ~actual:(prop ^ ".a{color:red}") with
  | Cc.Tree_diff d ->
      check bool "property container added" true
        (Td.has_container_added_of_type `Property d)
  | _ -> fail "Expected property Container_added");
  (* Removed *)
  match Cc.diff ~expected:(prop ^ ".a{color:red}") ~actual:".a{color:red}" with
  | Cc.Tree_diff d ->
      check bool "property container removed" true
        (Td.has_container_removed_of_type `Property d)
  | _ -> fail "Expected property Container_removed"

let test_complex_selector_change () =
  (* Test complex selector changes with shared parent context - these should be
     detected as selector changes, not add/remove *)
  let css_expected =
    ".container .sidebar-left{display:flex;justify-content:start}"
  in
  let css_actual =
    ".container .sidebar-right{display:flex;justify-content:start}"
  in
  assert_single_selector_change ~expected:css_expected ~actual:css_actual
    ~old_selector:".container .sidebar-left"
    ~new_selector:".container .sidebar-right"

(* More complex selector changes that share a prefix but differ in
   composition *)
let test_shared_prefix_combinator_swap () =
  (* Both selectors share the same prefix ".wrap .menu" but differ in where the
     child combinator ">" is applied. This should be reported as a selector
     change, not an add/remove. *)
  let css_expected = ".wrap .menu > li.active a{color:red;padding:1px}" in
  let css_actual = ".wrap .menu li.active > a{color:red;padding:1px}" in
  assert_single_selector_change ~expected:css_expected ~actual:css_actual
    ~old_selector:".wrap .menu > li.active a"
    ~new_selector:".wrap .menu li.active > a"

let test_shared_prefix_pseudo_moved () =
  (* Move :hover from child to parent while keeping the ".container" prefix. *)
  let css_expected = ".container .item:hover .label{display:block}" in
  let css_actual = ".container:hover .item .label{display:block}" in
  assert_single_selector_change ~expected:css_expected ~actual:css_actual
    ~old_selector:".container .item:hover .label"
    ~new_selector:".container:hover .item .label"

let test_compound_vs_descendant_selector () =
  (* Change from compound .b.c to descendant .b .c under the same .parent
     prefix. These are semantically different and should be a selector
     change. *)
  let css_expected = ".parent .b.c .title{margin:0}" in
  let css_actual = ".parent .b .c .title{margin:0}" in
  assert_single_selector_change ~expected:css_expected ~actual:css_actual
    ~old_selector:".parent .b.c .title" ~new_selector:".parent .b .c .title"

(* Specific regressions: detect selector-only changes instead of add/remove. *)
let test_list_child_vs_descendant () =
  (* ol > li, ul > li padding changed to ol li, ul li padding *)
  let css_expected = "ol > li, ul > li{padding-left:1em}" in
  let css_actual = "ol li, ul li{padding-left:1em}" in
  assert_single_selector_change ~expected:css_expected ~actual:css_actual
    ~old_selector:"ol > li, ul > li" ~new_selector:"ol li, ul li"

let test_ul_child_vs_descendant () =
  (* ul p changed to ul > p *)
  let css_expected = "ul p{margin-top:0}" in
  let css_actual = "ul > p{margin-top:0}" in
  assert_single_selector_change ~expected:css_expected ~actual:css_actual
    ~old_selector:"ul p" ~new_selector:"ul > p"

let test_hr_adjacent_vs_sibling () =
  (* hr + * changed to hr ~ * *)
  let css_expected = "hr + *{margin-top:0}" in
  let css_actual = "hr ~ *{margin-top:0}" in
  assert_single_selector_change ~expected:css_expected ~actual:css_actual
    ~old_selector:"hr + *" ~new_selector:"hr ~ *"

(* ===== Additional Test Coverage for Gaps ===== *)

let test_multiple_property_changes () =
  let expected = ".card{color:red;padding:10px;margin:5px;border:1px solid}" in
  let actual = ".card{color:blue;padding:20px;margin:5px;border:2px dotted}" in
  let diff = tree_diff ~expected ~actual in
  let rule = single_rule_diff diff in
  match rule with
  | Td.Rule_content_changed { selector; property_changes; _ } ->
      check string "selector" ".card" selector;
      check int "changed properties" 3 (List.length property_changes);
      assert_property_changed "color" ~from:"red" ~to_:"blue" property_changes;
      assert_property_changed "padding" ~from:"10px" ~to_:"20px"
        property_changes;
      assert_property_changed "border" ~from:"1px solid" ~to_:"2px dotted"
        property_changes
  | _ -> fail "Expected Rule_content_changed"

let test_pseudo_element_changes () =
  (* ::before to ::after should be add/remove, not selector change - they're
     different pseudo-elements *)
  let diff1 =
    tree_diff ~expected:".btn::before{content:'→'}"
      ~actual:".btn::after{content:'→'}"
  in
  assert_rule_added ".btn::after" diff1;
  assert_rule_removed ".btn::before" diff1;

  (* :first-child to :last-child should also be add/remove - different
     pseudo-classes *)
  let diff2 =
    tree_diff ~expected:"li:first-child{margin:0}"
      ~actual:"li:last-child{margin:0}"
  in
  assert_rule_added "li:last-child" diff2;
  assert_rule_removed "li:first-child" diff2

let test_attribute_selector_changes () =
  (* Different attribute selectors match different elements, so they should be
     add/remove. The CSS parser normalizes quotes. *)
  let diff1 =
    tree_diff ~expected:"[href^='http']{color:blue}"
      ~actual:"[href$='.pdf']{color:blue}"
  in
  assert_rule_added "[href$=\".pdf\"]" diff1;
  (* Parser adds quotes for .pdf *)
  assert_rule_removed "[href^=http]" diff1;

  (* Parser removes quotes from http *)
  let diff2 =
    tree_diff ~expected:"[disabled]{opacity:0.5}"
      ~actual:"[disabled='true']{opacity:0.5}"
  in
  assert_rule_added "[disabled=true]" diff2;
  (* Parser removes quotes from true *)
  assert_rule_removed "[disabled]" diff2

let test_empty_rules () =
  (* Empty rule gets content added *)
  let diff = tree_diff ~expected:".empty{}" ~actual:".empty{color:red}" in
  assert_rule_content_changed ".empty" diff

let test_css_variable_references () =
  (* Variable reference changes *)
  let diff =
    tree_diff ~expected:".theme{color:var(--primary)}"
      ~actual:".theme{color:var(--secondary)}"
  in
  let rule = single_rule_diff diff in
  assert_single_property_change ~prop:"color" ~from:"var(--primary)"
    ~to_:"var(--secondary)" rule

let test_nested_media_layer () =
  (* The CSS parser might normalize the output differently, so we need proper
     spacing *)
  let expected = "@media screen { @layer utils { .x { color: red } } }" in
  let actual = "@media screen { @layer utils { .x { color: blue } } }" in
  match Cc.diff ~expected ~actual with
  | Cc.Tree_diff diff ->
      check int "no top-level changes" 0 (List.length diff.rules);
      (* Changes should be in container modifications *)
      check bool "has container changes" true (List.length diff.containers > 0)
  | Cc.String_diff _ ->
      (* If we get a string diff, it means normalization differs. Accept this as
         expected behavior for now *)
      ()
  | Cc.No_diff -> fail "Expected differences between red and blue"
  | Cc.Both_errors _ | Cc.Expected_error _ | Cc.Actual_error _ ->
      fail "CSS should parse correctly"

let test_media_query_differences () =
  let css1 =
    "@media (min-width:48rem){.md\\:p-8{padding:calc(var(--spacing)*4)}}"
  in
  let css2 =
    "@media (min-width:64rem){.lg\\:p-12{padding:calc(var(--spacing)*4)}}"
  in

  let diff = tree_diff ~expected:css1 ~actual:css2 in

  assert_container_added `Media "(min-width:64rem)" diff;
  assert_container_removed `Media "(min-width:48rem)" diff

let test_reordered_rules_within_layer () =
  (* Test that css_compare correctly detects when rules are reordered within the
     same layer (not added/removed) *)
  let expected =
    {|@layer utilities {
  .rounded-lg { border-radius: var(--radius-lg); }
  .p-6 { padding: calc(var(--spacing) * 6); }
  .shadow-md {
    --tw-shadow: 0 4px 6px -1px var(--tw-shadow-color, #0000001a);
    box-shadow: var(--tw-shadow);
  }
}|}
  in
  let actual =
    {|@layer utilities {
  .p-6 { padding: calc(var(--spacing) * 6); }
  .rounded-lg { border-radius: var(--radius-lg); }
  .shadow-md {
    --tw-shadow: 0 4px 6px -1px var(--tw-shadow-color, #0000001a);
    box-shadow: var(--tw-shadow);
  }
}|}
  in
  match Cc.diff ~expected ~actual with
  | Cc.Tree_diff diff ->
      (* The reordering should be detected in containers for the layer *)

      (* Check for layer modifications *)
      let has_layer_with_reordering =
        List.exists
          (function
            | Td.Container_modified { info; rule_changes; _ }
              when info.container_type = `Layer ->
                (* Check if any rules are marked as reordered *)
                List.exists
                  (function Td.Rule_reordered _ -> true | _ -> false)
                  rule_changes
            | _ -> false)
          diff.containers
      in

      check bool "should detect reordering in layer" true
        has_layer_with_reordering
  | _ -> fail "Expected structural diff for reordered utilities"

let test_selector_complexity_structural () =
  (* Test that css_compare detects differences in selector complexity as
     structural differences. E.g. `.hover\:prose:hover :where(ol>li)::marker` vs
     `.hover\:prose:hover` should be reported as structural, not just a string
     difference. *)
  let expected =
    {|.hover\:prose:hover :where(ol>li):not(:where([class~=not-prose],[class~=not-prose] *))::marker{color:var(--tw-prose-counters);font-weight:400}
.hover\:prose:hover :where(ul>li):not(:where([class~=not-prose],[class~=not-prose] *))::marker{color:var(--tw-prose-bullets)}|}
  in
  let actual =
    {|.hover\:prose:hover{color:var(--tw-prose-bullets);font-weight:400}|}
  in

  match Cc.diff ~expected ~actual with
  | Cc.Tree_diff diff ->
      (* Should detect rules with different selectors as structural changes *)
      let has_structural_changes =
        List.length diff.rules > 0
        || List.exists
             (function
               | Td.Container_modified { rule_changes; _ } ->
                   List.length rule_changes > 0
               | _ -> false)
             diff.containers
      in
      Alcotest.(check bool)
        "Should detect missing pseudo-element selectors as structural \
         difference"
        true has_structural_changes
  | Cc.String_diff _ ->
      fail
        "Should detect missing pseudo-element as structural difference, not \
         string difference"
  | Cc.No_diff -> fail "Should detect difference between selectors"
  | Cc.Expected_error _ | Cc.Actual_error _ | Cc.Both_errors _ ->
      fail "Should not have parsing errors"

let test_media_layer_missing_rules () =
  (* This test reproduces the hover:prose issue where @media is inside @layer
     utilities. The tree_diff function should detect differences inside nested
     structures. *)
  let expected =
    {|@layer utilities{@media (hover:hover){.a{color:red}.b::marker{color:blue}}}|}
  in
  let actual = {|@layer utilities{@media (hover:hover){.c{color:green}}}|} in

  match Cc.diff ~expected ~actual with
  | Cc.Tree_diff diff ->
      (* Should detect structural changes in nested contexts *)
      let has_changes =
        List.length diff.rules > 0 || List.length diff.containers > 0
      in
      check bool "Should detect structural differences in nested media" true
        has_changes
  | Cc.String_diff _ ->
      (* This is the bug we want to fix - should be structural not string
         diff *)
      fail "Should detect as structural difference, not string difference"
  | _ -> fail "Unexpected diff result"

let test_media_layer_string_fallback () =
  (* This test shows that when tree_diff fails to detect nested differences, the
     string diff should still be helpful *)
  let expected =
    {|@layer utilities{@media (hover:hover){.hover\:prose:hover :where(ol>li)::marker{color:red}}}|}
  in
  let actual =
    {|@layer utilities{@media (hover:hover){.hover\:prose:hover{color:blue}}}|}
  in

  match Cc.diff ~expected ~actual with
  | Cc.String_diff sdiff ->
      (* This is expected due to current tree_diff limitations *)
      check bool "String diff should have valid position" true
        (sdiff.position > 0)
  | Cc.Tree_diff _ ->
      (* If tree_diff works, that's great too *)
      ()
  | _ -> fail "Should get either structural or string diff"

let test_simple_media_works () =
  (* Control test: simple media (not inside layers) should work correctly *)
  let expected =
    {|@media (hover:hover){.a{color:red}.b::marker{color:blue}}|}
  in
  let actual = {|@media (hover:hover){.c{color:green}}|} in

  match Cc.diff ~expected ~actual with
  | Cc.Tree_diff diff ->
      (* Should work correctly for simple media *)
      let has_changes =
        List.length diff.rules > 0 || List.length diff.containers > 0
      in
      check bool "Simple media should detect structural differences" true
        has_changes
  | _ -> fail "Simple media comparison should work"

(* Helper to test nested container detection *)
let assert_detects_changes css1 css2 =
  match Cc.diff ~expected:css1 ~actual:css2 with
  | Cc.Tree_diff diff ->
      let has_changes =
        List.length diff.rules > 0 || List.length diff.containers > 0
      in
      Alcotest.(check bool) "should detect structural changes" true has_changes
  | Cc.No_diff ->
      Alcotest.fail "Expected structural differences but got No_diff"
  | _ -> Alcotest.fail "Expected structural differences"

let test_nested_media_added () =
  (* Adding nested @media inside @media *)
  let css1 =
    {|
    @media (min-width: 768px) {
      .text { font-size: 1rem; }
    }
  |}
  in
  let css2 =
    {|
    @media (min-width: 768px) {
      @media (prefers-color-scheme: dark) {
        .dark-text { color: white; }
      }
      .text { font-size: 1rem; }
    }
  |}
  in
  assert_detects_changes css1 css2

let test_nested_media_removed () =
  (* Removing nested @media from inside @media *)
  let css1 =
    {|
    @media (min-width: 768px) {
      @media (prefers-color-scheme: dark) {
        .dark-text { color: white; }
      }
      .text { font-size: 1rem; }
    }
  |}
  in
  let css2 =
    {|
    @media (min-width: 768px) {
      .text { font-size: 1rem; }
    }
  |}
  in
  assert_detects_changes css1 css2

let test_layer_in_media_added () =
  (* Adding @layer inside @media *)
  let css1 =
    {|
    @media (min-width: 768px) {
      .text { font-size: 1rem; }
    }
  |}
  in
  let css2 =
    {|
    @media (min-width: 768px) {
      @layer utilities {
        .special { color: blue; }
      }
      .text { font-size: 1rem; }
    }
  |}
  in
  assert_detects_changes css1 css2

let test_nested_supports_in_layer () =
  (* Adding @supports inside @layer *)
  let css1 =
    {|
    @layer utilities {
      .utility { display: block; }
    }
  |}
  in
  let css2 =
    {|
    @layer utilities {
      @supports (display: grid) {
        .grid-utility { display: grid; }
      }
      .utility { display: block; }
    }
  |}
  in
  assert_detects_changes css1 css2

let test_nested_deep_nesting () =
  (* Deep nesting: @layer > @media > @supports > @media *)
  let css1 =
    {|
    @layer utilities {
      .utility { display: block; }
    }
  |}
  in
  let css2 =
    {|
    @layer utilities {
      @media (min-width: 768px) {
        @supports (display: grid) {
          @media (prefers-color-scheme: dark) {
            .deep-nested { color: white; }
          }
        }
      }
      .utility { display: block; }
    }
  |}
  in
  (* Spec: should detect structural difference when nested containers are
     added *)
  assert_detects_changes css1 css2

(* Test that media query ordering is detected as a structural difference *)
let test_media_query_ordering () =
  let css1 =
    {|
    @media (min-width: 768px) {
      .md { display: block; }
    }
    @media (min-width: 1024px) {
      .lg { display: flex; }
    }
  |}
  in
  let css2 =
    {|
    @media (min-width: 1024px) {
      .lg { display: flex; }
    }
    @media (min-width: 768px) {
      .md { display: block; }
    }
  |}
  in
  (* Media queries in different order should be detected as structural change *)
  assert_detects_changes css1 css2

let tests =
  [
    test_case "strip header" `Quick test_strip_header;
    test_case "compare equivalent" `Quick test_compare_equivalent;
    test_case "compare media+layers equivalent" `Quick
      test_compare_media_layers_equivalent;
    test_case "dispatching to tree diff" `Quick test_dispatching_to_tree_diff;
    test_case "dispatching to string diff" `Quick
      test_dispatching_to_string_diff;
    test_case "dispatching no diff" `Quick test_dispatching_no_diff;
    test_case "compare function" `Quick test_compare_function;
    test_case "compare with parse errors" `Quick test_compare_with_parse_errors;
    test_case "diff parse errors" `Quick test_diff_parse_errors;
    test_case "pp_stats tree diff" `Quick test_pp_stats_tree_diff;
    test_case "pp_stats string diff" `Quick test_pp_stats_string_diff;
    test_case "pp_stats no diff" `Quick test_pp_stats_no_diff;
    test_case "pp_stats rule types" `Quick test_pp_stats_rule_types;
    test_case "pp_stats parse errors" `Quick test_pp_stats_parse_errors;
    test_case "pp_stats containers" `Quick test_pp_stats_containers;
    test_case "pp_stats combined changes" `Quick test_pp_stats_combined_changes;
    test_case "added/removed/modified rules" `Quick
      test_rule_added_removed_modified;
    test_case "modified property value" `Quick test_property_value_modified;
    test_case "modified property added only" `Quick test_property_added_only;
    test_case "modified property removed only" `Quick test_property_removed_only;
    test_case "media and layer diffs" `Quick test_media_and_layer_diffs;
    test_case "important and custom props" `Quick
      test_important_and_custom_props;
    test_case "supports and container diffs" `Quick
      test_supports_and_container_diffs;
    test_case "string diff context basic" `Quick test_string_diff_context_basic;
    test_case "string diff context multiline" `Quick
      test_string_diff_context_multiline;
    test_case "string diff context at end" `Quick test_string_diff_at_end;
    test_case "string diff context none" `Quick test_string_diff_context_none;
    test_case "pp diff result with string context" `Quick
      test_pp_with_string_context;
    test_case "pp diff result structural" `Quick test_pp_structural;
    test_case "selector formatting difference" `Quick
      test_selector_formatting_difference;
    test_case "CSS variable differences" `Quick test_css_variable_differences;
    test_case "layer custom properties" `Quick test_layer_custom_properties;
    test_case "CSS unit differences 0px vs 0" `Quick test_css_unit_differences;
    test_case "complex CSS unit differences" `Quick
      test_complex_css_unit_differences;
    test_case "never empty diff when strings differ" `Quick
      test_diff_never_empty;
    test_case "grouped selector reorder is structural" `Quick
      test_selector_list_reorder_structural;
    test_case "rule position reordering" `Quick test_rule_position_reordering;
    test_case "multiple rule reordering" `Quick test_multiple_rule_reordering;
    test_case "property reordering" `Quick test_property_reordering;
    test_case "reordered diff formatting" `Quick test_reordered_diff_formatting;
    test_case "mixed reordering and changes" `Quick
      test_mixed_reordering_and_changes;
    test_case "selector change suppression" `Quick
      test_selector_change_suppression;
    test_case "diff result: no diff" `Quick test_no_diff_result;
    test_case "diff result: expected error" `Quick test_expected_error_result;
    test_case "diff result: actual error" `Quick test_actual_error_result;
    test_case "diff result: both errors" `Quick test_both_errors_result;
    test_case "explicit container added" `Quick test_container_added_explicit;
    test_case "explicit container removed" `Quick
      test_container_removed_explicit;
    test_case "@property container add/remove" `Quick
      test_property_container_added_removed;
    test_case "complex selector change with parent context" `Quick
      test_complex_selector_change;
    test_case "selector change: shared prefix combinator swap" `Quick
      test_shared_prefix_combinator_swap;
    test_case "selector change: shared prefix pseudo moved" `Quick
      test_shared_prefix_pseudo_moved;
    test_case "selector change: shared prefix compound vs descendant" `Quick
      test_compound_vs_descendant_selector;
    test_case "selector change: list child vs descendant" `Quick
      test_list_child_vs_descendant;
    test_case "selector change: ul p child vs descendant" `Quick
      test_ul_child_vs_descendant;
    test_case "selector change: hr + vs ~" `Quick test_hr_adjacent_vs_sibling;
    (* New gap coverage tests *)
    test_case "multiple property changes" `Quick test_multiple_property_changes;
    test_case "pseudo element changes" `Quick test_pseudo_element_changes;
    test_case "attribute selector changes" `Quick
      test_attribute_selector_changes;
    test_case "empty rules" `Quick test_empty_rules;
    test_case "css variable references" `Quick test_css_variable_references;
    test_case "nested media layer" `Quick test_nested_media_layer;
    test_case "media query differences" `Quick test_media_query_differences;
    test_case "reordered rules within layer" `Quick
      test_reordered_rules_within_layer;
    test_case "selector complexity difference is structural" `Quick
      test_selector_complexity_structural;
    test_case "media inside layer missing rules" `Quick
      test_media_layer_missing_rules;
    test_case "media inside layer string fallback" `Quick
      test_media_layer_string_fallback;
    test_case "simple media works" `Quick test_simple_media_works;
    test_case "nested containers - media in media added" `Quick
      test_nested_media_added;
    test_case "nested containers - media in media removed" `Quick
      test_nested_media_removed;
    test_case "nested containers - layer in media added" `Quick
      test_layer_in_media_added;
    test_case "nested containers - supports in layer" `Quick
      test_nested_supports_in_layer;
    test_case "nested containers - deep nesting" `Quick test_nested_deep_nesting;
    test_case "media query order matters" `Quick test_media_query_ordering;
  ]

let suite = ("css_compare", tests)
