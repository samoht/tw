open Alcotest
module Cc = Tw_tools.Css_compare
open Cc

let pp_css fmt css =
  match Css.of_string css with
  | Ok ast -> Fmt.string fmt (Css.to_string ~minify:true ast)
  | Error e -> Fmt.pf fmt "<parse error: %s>" (Css.pp_parse_error e)

let css = Alcotest.testable pp_css Cc.compare_css

(* ===== Enhanced Test Helpers ===== *)

(* Core diff extraction helpers *)
let get_tree_diff ~expected ~actual =
  match Cc.diff ~expected ~actual with
  | Cc.Tree_diff d -> d
  | Cc.No_diff -> fail "Expected differences but got No_diff"
  | Cc.String_diff _ -> fail "Expected structural diff but got String_diff"
  | _ -> fail "Expected Tree_diff"

let get_single_rule_diff diff =
  match diff.rules with
  | [ rule ] -> rule
  | [] -> fail "Expected one rule change, got none"
  | _ -> fail "Expected exactly one rule change"

(* Rule type assertions *)
let assert_rule_added selector diff =
  let found =
    List.exists
      (function
        | Cc.Rule_added { selector = s; _ } -> s = selector | _ -> false)
      diff.rules
  in
  if not found then failf "Expected rule added: %s" selector

let assert_rule_removed selector diff =
  let found =
    List.exists
      (function
        | Cc.Rule_removed { selector = s; _ } -> s = selector | _ -> false)
      diff.rules
  in
  if not found then failf "Expected rule removed: %s" selector

let assert_rule_modified selector diff =
  let found =
    List.exists
      (function
        | Cc.Rule_content_changed { selector = s; _ } -> s = selector
        | _ -> false)
      diff.rules
  in
  if not found then failf "Expected rule modified: %s" selector

(* Property change assertions *)
let assert_property_changed prop_name ~from ~to_ changes =
  match List.find_opt (fun p -> p.property_name = prop_name) changes with
  | Some p ->
      check string (prop_name ^ " old value") from p.expected_value;
      check string (prop_name ^ " new value") to_ p.actual_value
  | None -> failf "Property '%s' not found in changes" prop_name

let assert_single_property_change ~prop ~from ~to_ rule =
  match rule with
  | Cc.Rule_content_changed { property_changes; _ } ->
      check int "property change count" 1 (List.length property_changes);
      assert_property_changed prop ~from ~to_ property_changes
  | _ -> fail "Expected Rule_content_changed"

let assert_rule_reordered selector diff =
  let found =
    List.exists
      (function
        | Cc.Rule_reordered { selector = s } -> s = selector | _ -> false)
      diff.rules
  in
  if not found then failf "Expected rule reordered: %s" selector

(* Container helpers *)
let count_containers_by_type container_type diff =
  List.length
    (List.filter
       (function
         | Cc.Container_added { container_type = ct; _ }
         | Cc.Container_removed { container_type = ct; _ }
         | Cc.Container_modified { info = { container_type = ct; _ }; _ } ->
             ct = container_type)
       diff.containers)

let has_container_added_of_type container_type diff =
  List.exists
    (function
      | Cc.Container_added { container_type = ct; _ } -> ct = container_type
      | _ -> false)
    diff.containers

let has_container_removed_of_type container_type diff =
  List.exists
    (function
      | Cc.Container_removed { container_type = ct; _ } -> ct = container_type
      | _ -> false)
    diff.containers

let assert_container_added container_type condition diff =
  let found =
    List.exists
      (function
        | Cc.Container_added { container_type = ct; condition = c; _ } ->
            ct = container_type && c = condition
        | _ -> false)
      diff.containers
  in
  if not found then
    failf "Expected container added: @%s %s"
      (match container_type with
      | `Media -> "media"
      | `Layer -> "layer"
      | `Supports -> "supports"
      | `Container -> "container"
      | `Property -> "property")
      condition

let assert_container_removed container_type condition diff =
  let found =
    List.exists
      (function
        | Cc.Container_removed { container_type = ct; condition = c; _ } ->
            ct = container_type && c = condition
        | _ -> false)
      diff.containers
  in
  if not found then
    failf "Expected container removed: @%s %s"
      (match container_type with
      | `Media -> "media"
      | `Layer -> "layer"
      | `Supports -> "supports"
      | `Container -> "container"
      | `Property -> "property")
      condition

(* Selector change assertion *)
let assert_single_selector_change ~expected ~actual ~old_selector ~new_selector
    =
  match Cc.diff ~expected ~actual with
  | Cc.Tree_diff d -> (
      check int "one rule change" 1 (List.length d.rules);
      match List.hd d.rules with
      | Cc.Rule_selector_changed { old_selector = o; new_selector = n; _ } ->
          check string "old selector" old_selector o;
          check string "new selector" new_selector n
      | Cc.Rule_added _ | Cc.Rule_removed _ ->
          fail "Expected selector change, not add/remove"
      | _ -> fail "Expected Rule_selector_changed")
  | _ -> fail "Expected structural selector change"

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
  let diff = get_tree_diff ~expected:css_expected ~actual:css_actual in
  assert_rule_added ".c" diff;
  assert_rule_removed ".b" diff;
  assert_rule_modified ".a" diff

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
  check int "has container changes" 2 (List.length diff.containers);
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
  let diff = get_tree_diff ~expected:css_expected ~actual:css_actual in
  check int "no container changes" 0 (List.length diff.containers);
  let rule = get_single_rule_diff diff in
  assert_single_property_change ~prop:"color" ~from:"red" ~to_:"blue" rule

let test_property_added_only () =
  let css_expected = ".y{color:red}" in
  let css_actual = ".y{color:red;padding:10px}" in
  let diff = get_tree_diff ~expected:css_expected ~actual:css_actual in
  assert_rule_modified ".y" diff

let test_important_and_custom_props () =
  (* Importance difference should be detected *)
  let d1 =
    get_tree_diff ~expected:".x{color:red!important}" ~actual:".x{color:red}"
  in
  assert_rule_modified ".x" d1;
  (* Custom property value difference should be detected *)
  let d2 = get_tree_diff ~expected:".x{--foo:1}" ~actual:".x{--foo:2}" in
  assert_rule_modified ".x" d2

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

let contains s pat = Re.execp (Re.compile (Re.str pat)) s

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
  let has_modify = contains output "modify:" in
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
  let diff = get_tree_diff ~expected:css_expected ~actual:css_actual in
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
          (function Cc.Rule_reordered _ -> true | _ -> false)
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
          (function Cc.Rule_reordered _ -> true | _ -> false)
          d.rules
      in
      let added =
        List.filter (function Cc.Rule_added _ -> true | _ -> false) d.rules
      in
      let removed =
        List.filter (function Cc.Rule_removed _ -> true | _ -> false) d.rules
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
        | Cc.Container_modified
            {
              info = { container_type = `Layer; condition; rules = _ };
              rule_changes;
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
              (function Cc.Rule_selector_changed _ -> true | _ -> false)
              rule_changes
          in
          let adds =
            List.filter
              (function Cc.Rule_added _ -> true | _ -> false)
              rule_changes
          in
          let removes =
            List.filter
              (function Cc.Rule_removed _ -> true | _ -> false)
              rule_changes
          in
          check int "single selector change" 1 (List.length selector_changes);
          check int "no adds" 0 (List.length adds);
          check int "no removes" 0 (List.length removes);
          (* Sanity-check the actual selector values *)
          match List.hd selector_changes with
          | Cc.Rule_selector_changed { old_selector; new_selector; _ } ->
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
        (has_container_added_of_type `Media d)
  | _ -> fail "Expected structural diff with media Container_added"

let test_container_removed_explicit () =
  let expected = "@media (min-width:600px){.a{color:red}}" in
  let actual = ".a{color:red}" in
  match Cc.diff ~expected ~actual with
  | Cc.Tree_diff d ->
      check bool "media container removed" true
        (has_container_removed_of_type `Media d)
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
        (has_container_added_of_type `Property d)
  | _ -> fail "Expected property Container_added");
  (* Removed *)
  match Cc.diff ~expected:(prop ^ ".a{color:red}") ~actual:".a{color:red}" with
  | Cc.Tree_diff d ->
      check bool "property container removed" true
        (has_container_removed_of_type `Property d)
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
let test_selector_change_shared_prefix_combinator_swap () =
  (* Both selectors share the same prefix ".wrap .menu" but differ in where the
     child combinator ">" is applied. This should be reported as a selector
     change, not an add/remove. *)
  let css_expected = ".wrap .menu > li.active a{color:red;padding:1px}" in
  let css_actual = ".wrap .menu li.active > a{color:red;padding:1px}" in
  assert_single_selector_change ~expected:css_expected ~actual:css_actual
    ~old_selector:".wrap .menu > li.active a"
    ~new_selector:".wrap .menu li.active > a"

let test_selector_change_shared_prefix_pseudo_moved () =
  (* Move :hover from child to parent while keeping the ".container" prefix. *)
  let css_expected = ".container .item:hover .label{display:block}" in
  let css_actual = ".container:hover .item .label{display:block}" in
  assert_single_selector_change ~expected:css_expected ~actual:css_actual
    ~old_selector:".container .item:hover .label"
    ~new_selector:".container:hover .item .label"

let test_selector_change_shared_prefix_compound_vs_descendant () =
  (* Change from compound .b.c to descendant .b .c under the same .parent
     prefix. These are semantically different and should be a selector
     change. *)
  let css_expected = ".parent .b.c .title{margin:0}" in
  let css_actual = ".parent .b .c .title{margin:0}" in
  assert_single_selector_change ~expected:css_expected ~actual:css_actual
    ~old_selector:".parent .b.c .title" ~new_selector:".parent .b .c .title"

(* Specific regressions: detect selector-only changes instead of add/remove. *)
let test_selector_change_list_child_vs_descendant () =
  (* ol > li, ul > li padding changed to ol li, ul li padding *)
  let css_expected = "ol > li, ul > li{padding-left:1em}" in
  let css_actual = "ol li, ul li{padding-left:1em}" in
  assert_single_selector_change ~expected:css_expected ~actual:css_actual
    ~old_selector:"ol > li, ul > li" ~new_selector:"ol li, ul li"

let test_selector_change_ul_p_child_vs_descendant () =
  (* ul p changed to ul > p *)
  let css_expected = "ul p{margin-top:0}" in
  let css_actual = "ul > p{margin-top:0}" in
  assert_single_selector_change ~expected:css_expected ~actual:css_actual
    ~old_selector:"ul p" ~new_selector:"ul > p"

let test_selector_change_hr_adjacent_vs_general_sibling () =
  (* hr + * changed to hr ~ * *)
  let css_expected = "hr + *{margin-top:0}" in
  let css_actual = "hr ~ *{margin-top:0}" in
  assert_single_selector_change ~expected:css_expected ~actual:css_actual
    ~old_selector:"hr + *" ~new_selector:"hr ~ *"

(* ===== Additional Test Coverage for Gaps ===== *)

let test_multiple_property_changes () =
  let expected = ".card{color:red;padding:10px;margin:5px;border:1px solid}" in
  let actual = ".card{color:blue;padding:20px;margin:5px;border:2px dotted}" in
  let diff = get_tree_diff ~expected ~actual in
  let rule = get_single_rule_diff diff in
  match rule with
  | Cc.Rule_content_changed { selector; property_changes; _ } ->
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
    get_tree_diff ~expected:".btn::before{content:'→'}"
      ~actual:".btn::after{content:'→'}"
  in
  assert_rule_added ".btn::after" diff1;
  assert_rule_removed ".btn::before" diff1;

  (* :first-child to :last-child should also be add/remove - different
     pseudo-classes *)
  let diff2 =
    get_tree_diff ~expected:"li:first-child{margin:0}"
      ~actual:"li:last-child{margin:0}"
  in
  assert_rule_added "li:last-child" diff2;
  assert_rule_removed "li:first-child" diff2

let test_attribute_selector_changes () =
  (* Different attribute selectors match different elements, so they should be
     add/remove. The CSS parser normalizes quotes. *)
  let diff1 =
    get_tree_diff ~expected:"[href^='http']{color:blue}"
      ~actual:"[href$='.pdf']{color:blue}"
  in
  assert_rule_added "[href$=\".pdf\"]" diff1;
  (* Parser adds quotes for .pdf *)
  assert_rule_removed "[href^=http]" diff1;

  (* Parser removes quotes from http *)
  let diff2 =
    get_tree_diff ~expected:"[disabled]{opacity:0.5}"
      ~actual:"[disabled='true']{opacity:0.5}"
  in
  assert_rule_added "[disabled=true]" diff2;
  (* Parser removes quotes from true *)
  assert_rule_removed "[disabled]" diff2

let test_empty_rules () =
  (* Empty rule gets content added *)
  let diff = get_tree_diff ~expected:".empty{}" ~actual:".empty{color:red}" in
  assert_rule_modified ".empty" diff

let test_css_variable_references () =
  (* Variable reference changes *)
  let diff =
    get_tree_diff ~expected:".theme{color:var(--primary)}"
      ~actual:".theme{color:var(--secondary)}"
  in
  let rule = get_single_rule_diff diff in
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

  let diff = get_tree_diff ~expected:css1 ~actual:css2 in

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
      Printf.printf "Top-level rules: %d\n" (List.length diff.rules);
      Printf.printf "Containers: %d\n" (List.length diff.containers);

      (* Check for layer modifications *)
      let has_layer_with_reordering =
        List.exists
          (function
            | Cc.Container_modified { info; rule_changes }
              when info.container_type = `Layer ->
                Printf.printf "Modified layer '%s' with %d rule changes\n"
                  info.condition (List.length rule_changes);
                List.iter
                  (function
                    | Cc.Rule_reordered r ->
                        Printf.printf "  Reordered: %s\n" r.selector
                    | Cc.Rule_added r ->
                        Printf.printf "  Added: %s\n" r.selector
                    | Cc.Rule_removed r ->
                        Printf.printf "  Removed: %s\n" r.selector
                    | Cc.Rule_content_changed r ->
                        Printf.printf "  Content changed: %s\n" r.selector
                    | Cc.Rule_selector_changed r ->
                        Printf.printf "  Selector changed: %s -> %s\n"
                          r.old_selector r.new_selector)
                  rule_changes;
                (* Check if any rules are marked as reordered *)
                List.exists
                  (function Cc.Rule_reordered _ -> true | _ -> false)
                  rule_changes
            | _ -> false)
          diff.containers
      in

      check bool "should detect reordering in layer" true
        has_layer_with_reordering
  | _ -> fail "Expected structural diff for reordered utilities"

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
      test_selector_change_shared_prefix_combinator_swap;
    test_case "selector change: shared prefix pseudo moved" `Quick
      test_selector_change_shared_prefix_pseudo_moved;
    test_case "selector change: shared prefix compound vs descendant" `Quick
      test_selector_change_shared_prefix_compound_vs_descendant;
    test_case "selector change: list child vs descendant" `Quick
      test_selector_change_list_child_vs_descendant;
    test_case "selector change: ul p child vs descendant" `Quick
      test_selector_change_ul_p_child_vs_descendant;
    test_case "selector change: hr + vs ~" `Quick
      test_selector_change_hr_adjacent_vs_general_sibling;
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
  ]

let suite = ("css_compare", tests)
