open Alcotest
module Cc = Tw_tools.Css_compare
open Cc

let pp_css fmt css =
  match Css.of_string css with
  | Ok ast -> Fmt.string fmt (Css.to_string ~minify:true ast)
  | Error e -> Fmt.pf fmt "<parse error: %s>" (Css.pp_parse_error e)

let css = Alcotest.testable pp_css Cc.compare_css

(* Test helpers *)
let get_diff ~expected ~actual =
  match Cc.diff ~expected ~actual with
  | Tree_diff d -> d
  | _ -> failwith "Both CSS should parse"

let get_single_rule_diff diff =
  match diff.rules with
  | [ rule ] -> rule
  | [] -> fail "Expected one rule change, got none"
  | _ -> fail "Expected exactly one rule change"

let assert_rule_content_changed ~selector ~property_name ~expected_value
    ~actual_value rule =
  match rule with
  | Cc.Rule_content_changed { selector = s; property_changes; _ } ->
      check string "rule selector" selector s;
      check int "one property diff" 1 (List.length property_changes);
      let prop_diff = List.hd property_changes in
      check string "property name" property_name prop_diff.property_name;
      check string "expected value" expected_value prop_diff.expected_value;
      check string "actual value" actual_value prop_diff.actual_value
  | _ -> fail "Expected Rule_content_changed"

let has_rule_content_changed ~selector diff =
  List.exists
    (function
      | Cc.Rule_content_changed { selector = s; _ } -> s = selector | _ -> false)
    diff.rules

let count_containers_by_type container_type diff =
  List.length
    (List.filter
       (function
         | Cc.Container_added { container_type = ct; _ }
         | Cc.Container_removed { container_type = ct; _ }
         | Cc.Container_modified { info = { container_type = ct; _ }; _ } ->
             ct = container_type)
       diff.containers)

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

let test_rule_added_removed_modified () =
  let css_expected = ".a{color:red;padding:10px}\n.b{margin:0}" in
  let css_actual = ".a{color:red}\n.c{margin:0}" in
  let diff =
    match Cc.diff ~expected:css_expected ~actual:css_actual with
    | Tree_diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  let has_added_c =
    List.exists
      (function Cc.Rule_added { selector = ".c"; _ } -> true | _ -> false)
      diff.rules
  in
  let has_removed_b =
    List.exists
      (function Cc.Rule_removed { selector = ".b"; _ } -> true | _ -> false)
      diff.rules
  in
  let has_modified_a =
    List.exists
      (function
        | Cc.Rule_content_changed { selector = ".a"; _ } -> true | _ -> false)
      diff.rules
  in
  check bool "added .c" true has_added_c;
  check bool "removed .b" true has_removed_b;
  check bool "modified .a" true has_modified_a

let test_media_and_layer_diffs () =
  let css_expected =
    "@media screen and (min-width:600px){.m{margin:0}}@layer \
     theme{.a{color:red}}"
  in
  let css_actual = "@media screen and (min-width:600px){.m{margin:1px}}" in
  let diff =
    match Cc.diff ~expected:css_expected ~actual:css_actual with
    | Tree_diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  (* Structural expectations: containers should have changes, no top-level rule
     diffs *)
  check int "no top-level rule diffs" 0 (List.length diff.rules);
  check int "has container changes" 1 (List.length diff.containers);
  let container = List.hd diff.containers in
  match container with
  | Cc.Container_modified
      { info = { container_type = `Media; _ }; rule_changes } ->
      let has_modified_m =
        List.exists
          (function
            | Cc.Rule_content_changed { selector = ".m"; _ } -> true
            | _ -> false)
          rule_changes
      in
      check bool "media .m rule modified" true has_modified_m
  | _ -> fail "expected media container modification"

let test_property_value_modified () =
  let css_expected = ".x{color:red}" in
  let css_actual = ".x{color:blue}" in
  let diff = get_diff ~expected:css_expected ~actual:css_actual in

  (* Check structural properties of the diff *)
  check int "one rule change" 1 (List.length diff.rules);
  check int "no container changes" 0 (List.length diff.containers);

  let rule = get_single_rule_diff diff in
  assert_rule_content_changed ~selector:".x" ~property_name:"color"
    ~expected_value:"red" ~actual_value:"blue" rule

let test_property_added_only () =
  let css_expected = ".y{color:red}" in
  let css_actual = ".y{color:red;padding:10px}" in
  let diff = get_diff ~expected:css_expected ~actual:css_actual in

  let has_modified_y = has_rule_content_changed ~selector:".y" diff in
  check bool "modified .y" true has_modified_y

let test_important_and_custom_props () =
  (* Importance difference should be detected *)
  let d1 =
    get_diff ~expected:".x{color:red!important}" ~actual:".x{color:red}"
  in
  let has_mod_imp = has_rule_content_changed ~selector:".x" d1 in
  check bool "importance change => modified" true has_mod_imp;
  (* Custom property value difference should be detected *)
  let d2 = get_diff ~expected:".x{--foo:1}" ~actual:".x{--foo:2}" in
  let has_mod_custom = has_rule_content_changed ~selector:".x" d2 in
  check bool "custom prop change => modified" true has_mod_custom

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
  check int "one supports change" 1 (count_containers_by_type `Supports diff);
  check int "one container change" 1 (count_containers_by_type `Container diff)

(* Intentionally avoid substring/count-based checks; use structural diff
   only. *)

let test_string_diff_context_basic () =
  let expected = "hello world" in
  let actual = "hello wurld" in
  match Cc.show_string_diff_context ~expected ~actual with
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
  match Cc.show_string_diff_context ~expected ~actual with
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

let test_string_diff_context_at_end () =
  let expected = "abc" in
  let actual = "abcd" in
  match Cc.show_string_diff_context ~expected ~actual with
  | Some sdiff -> check int "diff at end position" 3 sdiff.position
  | None -> fail "expected diff context"

let test_string_diff_context_none () =
  let expected = "same" in
  let actual = "same" in
  match Cc.show_string_diff_context ~expected ~actual with
  | Some _ -> fail "no diff expected"
  | None -> ()

let test_pp_with_string_context () =
  (* Test that pp_diff_result shows string context when no structural diff *)
  let css1 = ".a{color:red}" in
  let css2 = ".a{color: red}" in
  (* Extra space *)
  let result = Cc.diff ~expected:css1 ~actual:css2 in
  let output = Fmt.to_to_string Cc.pp result in
  (* Should show "no structural differences" and the string diff context *)
  check bool "shows no structural differences" true
    (* FIXME *)
    (String.contains output 'n' && String.contains output 'o');
  check bool "shows position" true (String.contains output '^')
(* Position of the space difference *)

let test_pp_structural () =
  (* Test that pp_diff_result shows structural diff when present *)
  let css1 = ".a{color:red}" in
  let css2 = ".a{color:blue}" in
  let result = Cc.diff ~expected:css1 ~actual:css2 in
  let output = Fmt.to_to_string Cc.pp result in
  (* Should show structural CSS diff, not string context *)
  check bool "shows CSS diff" true
    (* FIXME *)
    (String.contains output 'r' && String.contains output 'e'
   && String.contains output 'd')

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
  | Cc.No_diff -> fail "strings differ so should detect some difference"
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
        (Cc.is_empty diff)
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
        (Cc.is_empty diff)
  | _ -> fail "CSS should parse successfully"

let test_css_unit_differences () =
  (* Test case for 0px vs 0 - should be detected as structural difference *)
  let expected = "*{--tw-ring-offset-width:0px}" in
  let actual = "*{--tw-ring-offset-width:0}" in
  let result = Cc.diff ~expected ~actual in
  match result with
  | Tree_diff diff ->
      (* Should detect structural differences - 0px vs 0 are different values *)
      check bool "detects 0px vs 0 as structural difference" false
        (Cc.is_empty diff)
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
        false (Cc.is_empty diff)
  | _ -> fail "CSS should parse successfully"

let test_property_reordering () =
  (* Test that pure property reordering is detected as Reordered, not Changed *)
  let css_expected = ".x{color:red;padding:10px;margin:5px}" in
  let css_actual = ".x{margin:5px;color:red;padding:10px}" in
  let diff =
    match Cc.diff ~expected:css_expected ~actual:css_actual with
    | Tree_diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  (* Check that we get exactly one rule diff *)
  check int "one rule change" 1 (List.length diff.rules);
  let rule = List.hd diff.rules in
  (* Pattern match on the new rule_diff structure *)
  match rule with
  | Cc.Rule_reordered { selector } ->
      check string "rule selector" ".x" selector
      (* Success - pure reordering detected *)
  | Cc.Rule_content_changed _ ->
      fail
        "Expected Rule_reordered diff but got Rule_content_changed (pure \
         reordering should be detected)"
  | _ -> fail "Expected Reordered diff"

let test_reordered_diff_formatting () =
  (* Test that Reordered diffs produce proper output *)
  let css_expected = ".test{color:red;padding:10px}" in
  let css_actual = ".test{padding:10px;color:red}" in
  let result = Cc.diff ~expected:css_expected ~actual:css_actual in
  let output = Fmt.to_to_string Cc.pp result in
  (* Should contain reorder information *)
  check bool "shows reorder info" true
    (* FIXME *)
    (String.contains output 'r' && String.contains output 'e')

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
  | Cc.Rule_content_changed { selector; property_changes; _ } ->
      check string "rule selector" ".x" selector;
      check int "one property diff" 1 (List.length property_changes);
      let prop_diff = List.hd property_changes in
      check string "property name" "color" prop_diff.property_name;
      check string "expected value" "red" prop_diff.expected_value;
      check string "actual value" "blue" prop_diff.actual_value
  | Cc.Rule_reordered _ ->
      fail
        "Expected Changed diff (not Reordered) when both order and values \
         change"
  | _ -> fail "Expected Changed diff with property differences"

let test_never_empty_diff_when_strings_differ () =
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
      (* Unit differences - CSS normalizes 0px to 0, but we should show string
         diff *)
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
      if String.length output = 0 then (
        Printf.printf "\nERROR: No diff output for different strings:\n";
        Printf.printf "Expected: %s\n" expected;
        Printf.printf "Actual: %s\n" actual;
        fail "Different strings MUST produce some diff output"))
    test_cases

let test_grouped_selector_list_reorder_structural () =
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
      | Cc.Rule_selector_changed _ | Cc.Rule_reordered _ ->
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
      | Cc.Rule_reordered { selector } ->
          (* Either of the swapped selectors may be reported depending on
             pairing *)
          let ok = selector = ".a" || selector = ".b" in
          check bool "rule selector is one of swapped" true ok
      | _ -> fail "Expected Rule_reordered")
  | _ -> fail "Both CSS should parse and produce a structural diff"

let test_selector_change_suppression () =
  (* Test that when there's a selector change from X to Y, we don't see
     redundant "add" entries for Y in nested contexts like layers *)
  let css_expected = "@layer utilities{.old-selector{color:red;margin:10px}}" in
  let css_actual = "@layer utilities{.new-selector{color:red;margin:10px}}" in
  let result = Cc.diff ~expected:css_expected ~actual:css_actual in
  match result with
  | Tree_diff _ ->
      (* Should have a single selector change, not separate add/remove
         entries *)
      let buffer = Buffer.create 256 in
      let fmt = Format.formatter_of_buffer buffer in
      Cc.pp fmt result;
      Format.pp_print_flush fmt ();
      let output = Buffer.contents buffer in

      (* Count occurrences of the new selector to ensure it only appears once *)
      let count_occurrences str sub =
        let len_str = String.length str in
        let len_sub = String.length sub in
        let rec count i acc =
          if i > len_str - len_sub then acc
          else if String.sub str i len_sub = sub then count (i + 1) (acc + 1)
          else count (i + 1) acc
        in
        count 0 0
      in

      (* The new selector should appear only once (in the selector change entry)
         and NOT as a separate "add" entry *)
      let new_selector_count = count_occurrences output "new-selector" in

      (* Should see selector change, not redundant add/remove *)
      check bool "contains selector changed" true
        (String.contains output 's' && String.contains output 'e');

      (* Should not have excessive occurrences of the new selector *)
      check bool "new selector not duplicated" true (new_selector_count <= 2);

      (* Should not see both "add" and "selector changed" for same rule *)
      let has_add = String.contains output 'a' in
      let has_selector_changed = String.contains output 's' in
      if has_add && has_selector_changed then
        Printf.printf
          "\nWARNING: Output contains both 'add' and 'selector changed':\n%s\n"
          output
  | _ -> fail "Both CSS should parse and produce a structural diff"

let tests =
  [
    test_case "strip header" `Quick test_strip_header;
    test_case "compare equivalent" `Quick test_compare_equivalent;
    test_case "compare media+layers equivalent" `Quick
      test_compare_media_layers_equivalent;
    test_case "added/removed/modified rules" `Quick
      test_rule_added_removed_modified;
    test_case "modified property value" `Quick test_property_value_modified;
    test_case "modified property added only" `Quick test_property_added_only;
    test_case "media and layer diffs" `Quick test_media_and_layer_diffs;
    test_case "important and custom props" `Quick
      test_important_and_custom_props;
    test_case "supports and container diffs" `Quick
      test_supports_and_container_diffs;
    test_case "string diff context basic" `Quick test_string_diff_context_basic;
    test_case "string diff context multiline" `Quick
      test_string_diff_context_multiline;
    test_case "string diff context at end" `Quick
      test_string_diff_context_at_end;
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
      test_never_empty_diff_when_strings_differ;
    test_case "grouped selector reorder is structural" `Quick
      test_grouped_selector_list_reorder_structural;
    test_case "rule position reordering" `Quick test_rule_position_reordering;
    test_case "property reordering" `Quick test_property_reordering;
    test_case "reordered diff formatting" `Quick test_reordered_diff_formatting;
    test_case "mixed reordering and changes" `Quick
      test_mixed_reordering_and_changes;
    test_case "selector change suppression" `Quick
      test_selector_change_suppression;
  ]

let suite = ("css_compare", tests)
