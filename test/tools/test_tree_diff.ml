(** Tests for Tree_diff module *)

open Alcotest
module Td = Tw_tools.Tree_diff
open Test_helpers

(* ===== Testable Types ===== *)

let tree_diff = Alcotest.testable Td.pp ( = )

(* ===== Test Data ===== *)

let sample_declarations = [ Css.color (Css.hex "ff0000") ]

let sample_property_change =
  Td.{ property_name = "color"; expected_value = "red"; actual_value = "blue" }

let sample_rule_added =
  Td.Rule_added { selector = ".new-class"; declarations = sample_declarations }

let sample_rule_removed =
  Td.Rule_removed
    { selector = ".old-class"; declarations = sample_declarations }

let sample_rule_content_changed =
  Td.Rule_content_changed
    {
      selector = ".changed";
      old_declarations = [ Css.color (Css.hex "ff0000") ];
      new_declarations = [ Css.color (Css.hex "0000ff") ];
      property_changes = [ sample_property_change ];
      added_properties = [];
      removed_properties = [];
    }

let sample_rule_selector_changed =
  Td.Rule_selector_changed
    {
      old_selector = ".old-name";
      new_selector = ".new-name";
      declarations = sample_declarations;
    }

let sample_rule_reordered =
  Td.Rule_reordered
    {
      selector = ".reordered";
      expected_pos = 0;
      actual_pos = 1;
      swapped_with = Some ".other";
    }

let sample_container_added =
  Td.Container_added
    { container_type = `Media; condition = "(min-width: 768px)"; rules = [] }

let sample_container_removed =
  Td.Container_removed
    { container_type = `Layer; condition = "utilities"; rules = [] }

let sample_container_modified =
  Td.Container_modified
    {
      info =
        {
          container_type = `Supports;
          condition = "(display: grid)";
          rules = [];
        };
      rule_changes = [ sample_rule_content_changed ];
      container_changes = [];
    }

let css_tree_diff ~expected ~actual =
  match (Css.of_string expected, Css.of_string actual) with
  | Ok ast1, Ok ast2 -> Td.diff ~expected:ast1 ~actual:ast2
  | _ -> fail "CSS should parse successfully"

let has_rule_changes (diff : Td.t) = List.length diff.rules > 0
let has_container_changes (diff : Td.t) = List.length diff.containers > 0

let has_content_changes (diff : Td.t) =
  List.exists
    (function Td.Rule_content_changed _ -> true | _ -> false)
    diff.rules

let has_selector_changes (diff : Td.t) =
  List.exists
    (function Td.Rule_selector_changed _ -> true | _ -> false)
    diff.rules

let test_is_empty () =
  let empty_diff = Td.{ rules = []; containers = [] } in
  check bool "empty diff should be empty" true (Td.is_empty empty_diff);

  let non_empty_rules = Td.{ rules = [ sample_rule_added ]; containers = [] } in
  check bool "diff with rules should not be empty" false
    (Td.is_empty non_empty_rules);

  let non_empty_containers =
    Td.{ rules = []; containers = [ sample_container_added ] }
  in
  check bool "diff with containers should not be empty" false
    (Td.is_empty non_empty_containers)

(* ===== Rule Diff Tests ===== *)

let test_rule_added_structure () =
  match sample_rule_added with
  | Td.Rule_added { selector; declarations } ->
      check string "selector should match" ".new-class" selector;
      check int "should have declarations" 1 (List.length declarations)
  | _ -> fail "Expected Rule_added"

let test_rule_removed_structure () =
  match sample_rule_removed with
  | Td.Rule_removed { selector; declarations } ->
      check string "selector should match" ".old-class" selector;
      check int "should have declarations" 1 (List.length declarations)
  | _ -> fail "Expected Rule_removed"

let test_rule_content_changed_structure () =
  match sample_rule_content_changed with
  | Td.Rule_content_changed
      {
        selector;
        property_changes;
        old_declarations;
        new_declarations;
        added_properties;
        removed_properties;
      } ->
      check string "selector should match" ".changed" selector;
      check int "should have property changes" 1 (List.length property_changes);
      check int "should have old declarations" 1 (List.length old_declarations);
      check int "should have new declarations" 1 (List.length new_declarations);
      check int "should have no added properties" 0
        (List.length added_properties);
      check int "should have no removed properties" 0
        (List.length removed_properties);
      let change = List.hd property_changes in
      check string "property name" "color" change.property_name;
      check string "expected value" "red" change.expected_value;
      check string "actual value" "blue" change.actual_value
  | _ -> fail "Expected Rule_content_changed"

let test_rule_selector_changed_structure () =
  match sample_rule_selector_changed with
  | Td.Rule_selector_changed { old_selector; new_selector; declarations } ->
      check string "old selector" ".old-name" old_selector;
      check string "new selector" ".new-name" new_selector;
      check int "should have declarations" 1 (List.length declarations)
  | _ -> fail "Expected Rule_selector_changed"

let test_rule_reordered_structure () =
  match sample_rule_reordered with
  | Td.Rule_reordered { selector; _ } ->
      check string "selector should match" ".reordered" selector
  | _ -> fail "Expected Rule_reordered"

(* ===== Container Diff Tests ===== *)

let test_container_added_structure () =
  match sample_container_added with
  | Td.Container_added { container_type; condition; rules } ->
      check bool "should be Media type" true (container_type = `Media);
      check string "condition should match" "(min-width: 768px)" condition;
      check int "should have empty rules" 0 (List.length rules)
  | _ -> fail "Expected Container_added"

let test_container_removed_structure () =
  match sample_container_removed with
  | Td.Container_removed { container_type; condition; rules } ->
      check bool "should be Layer type" true (container_type = `Layer);
      check string "condition should match" "utilities" condition;
      check int "should have empty rules" 0 (List.length rules)
  | _ -> fail "Expected Container_removed"

let test_container_modified_structure () =
  match sample_container_modified with
  | Td.Container_modified { info; rule_changes; container_changes = _ } ->
      check bool "should be Supports type" true (info.container_type = `Supports);
      check string "condition should match" "(display: grid)" info.condition;
      check int "should have rule changes" 1 (List.length rule_changes)
  | _ -> fail "Expected Container_modified"

(* ===== Structural Diff Tests ===== *)

let test_empty_diff () =
  let empty_diff = Td.{ rules = []; containers = [] } in
  check tree_diff "empty diff structure" empty_diff empty_diff

let test_diff_with_rules () =
  let diff =
    Td.{ rules = [ sample_rule_added; sample_rule_removed ]; containers = [] }
  in
  check int "should have two rules" 2 (List.length diff.rules);
  check int "should have no containers" 0 (List.length diff.containers)

let test_diff_with_containers () =
  let diff =
    Td.
      {
        rules = [];
        containers = [ sample_container_added; sample_container_removed ];
      }
  in
  check int "should have no rules" 0 (List.length diff.rules);
  check int "should have two containers" 2 (List.length diff.containers)

let test_diff_with_both () =
  let diff =
    Td.
      {
        rules = [ sample_rule_added ];
        containers = [ sample_container_modified ];
      }
  in
  check int "should have one rule" 1 (List.length diff.rules);
  check int "should have one container" 1 (List.length diff.containers);
  check bool "should not be empty" false (Td.is_empty diff)

(* ===== Property Change Tests ===== *)

let test_property_change_structure () =
  let prop_change =
    Td.
      {
        property_name = "margin";
        expected_value = "0px";
        actual_value = "10px";
      }
  in
  check string "property name" "margin" prop_change.property_name;
  check string "expected value" "0px" prop_change.expected_value;
  check string "actual value" "10px" prop_change.actual_value

(* ===== Container Type Tests ===== *)

let test_container_types () =
  let media_container =
    Td.Container_added
      { container_type = `Media; condition = "(width > 800px)"; rules = [] }
  in
  let layer_container =
    Td.Container_added
      { container_type = `Layer; condition = "base"; rules = [] }
  in
  let supports_container =
    Td.Container_added
      { container_type = `Supports; condition = "(display: flex)"; rules = [] }
  in
  let container_container =
    Td.Container_added
      {
        container_type = `Container;
        condition = "myContainer (width > 400px)";
        rules = [];
      }
  in
  let property_container =
    Td.Container_added
      { container_type = `Property; condition = "--my-color"; rules = [] }
  in

  (* Test pattern matching on container types *)
  let get_type = function
    | Td.Container_added { container_type; _ } -> container_type
    | Td.Container_removed { container_type; _ } -> container_type
    | Td.Container_modified { info = { container_type; _ }; _ } ->
        container_type
  in

  check bool "media container type" true (get_type media_container = `Media);
  check bool "layer container type" true (get_type layer_container = `Layer);
  check bool "supports container type" true
    (get_type supports_container = `Supports);
  check bool "container container type" true
    (get_type container_container = `Container);
  check bool "property container type" true
    (get_type property_container = `Property)

(* ===== Nested Structure Tests ===== *)

let test_nested_rule_changes () =
  let nested_diff =
    Td.Container_modified
      {
        info = { container_type = `Layer; condition = "components"; rules = [] };
        rule_changes =
          [
            sample_rule_added;
            sample_rule_content_changed;
            sample_rule_reordered;
          ];
        container_changes = [];
      }
  in

  match nested_diff with
  | Td.Container_modified { rule_changes; _ } ->
      check int "should have three rule changes" 3 (List.length rule_changes)
  | _ -> fail "Expected Container_modified"

let test_diff_identical_css () =
  let css = ".a{color:red}" in
  let diff = css_tree_diff ~expected:css ~actual:css in
  check (list string) "should have no rule changes" []
    (List.map
       (function
         | Td.Rule_added { selector; _ }
         | Td.Rule_removed { selector; _ }
         | Td.Rule_content_changed { selector; _ }
         | Td.Rule_selector_changed { new_selector = selector; _ }
         | Td.Rule_reordered { selector; _ } ->
             selector)
       diff.rules);
  check (list string) "should have no container changes" []
    (List.map
       (function
         | Td.Container_added { condition; _ }
         | Td.Container_removed { condition; _ }
         | Td.Container_modified { info = { condition; _ }; _ } ->
             condition)
       diff.containers)

let test_diff_rule_added () =
  let css_expected = ".a{color:red}" in
  let css_actual = ".a{color:red}.b{margin:0}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  let rule = single_rule_diff diff in
  match rule with
  | Td.Rule_added { selector; declarations } ->
      check string "selector" ".b" selector;
      check bool "has declarations" true (List.length declarations > 0)
  | _ -> fail "Expected Rule_added"

let test_diff_rule_removed () =
  let css_expected = ".a{color:red}.b{margin:0}" in
  let css_actual = ".a{color:red}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  let rule = single_rule_diff diff in
  match rule with
  | Td.Rule_removed { selector; declarations } ->
      check string "selector" ".b" selector;
      check bool "has declarations" true (List.length declarations > 0)
  | _ -> fail "Expected Rule_removed"

let test_diff_rule_content_changed () =
  let css_expected = ".a{color:red}" in
  let css_actual = ".a{color:blue}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check int "should have one rule change" 1 (List.length diff.rules);
  assert_rule_content_changed ".a" diff;
  match List.hd diff.rules with
  | Td.Rule_content_changed { property_changes; _ } ->
      check int "should have one property change" 1
        (List.length property_changes);
      let prop_change = List.hd property_changes in
      check string "property name" "color" prop_change.property_name;
      check string "expected value" "red" prop_change.expected_value;
      check string "actual value" "blue" prop_change.actual_value
  | _ -> fail "Expected Rule_content_changed"

let test_diff_rule_reordered () =
  let css_expected = ".a{color:red;margin:0}" in
  let css_actual = ".a{margin:0;color:red}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  let rule = single_rule_diff diff in
  (* Property reordering is detected as Rule_reordered *)
  match rule with
  | Td.Rule_reordered { selector; _ } -> check string "selector" ".a" selector
  | _ -> fail "Expected Rule_reordered for property reordering"

let test_diff_media_query () =
  let css_expected = "@media screen{.a{color:red}}" in
  let css_actual = "@media screen{.a{color:blue}}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check int "should have no top-level rule changes" 0 (List.length diff.rules);
  check int "should have container changes" 1 (List.length diff.containers);
  assert_container_modified `Media "screen" diff

let test_diff_nested_media () =
  let css_expected = "@media screen{@media print{.a{color:red}}}" in
  let css_actual = "@media screen{@media print{.a{color:blue}}}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check int "should have container changes" 1 (List.length diff.containers);
  check int "should have no top-level rule changes" 0 (List.length diff.rules)

let test_diff_multiple_changes () =
  let css_expected = ".a{color:red}.b{margin:0}" in
  let css_actual = ".a{color:blue}.c{margin:1px}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check int "should have exactly 3 rule changes" 3 (List.length diff.rules);
  let content_changes =
    List.filter
      (function Td.Rule_content_changed _ -> true | _ -> false)
      diff.rules
  in
  let add_removes =
    List.filter
      (function Td.Rule_added _ | Td.Rule_removed _ -> true | _ -> false)
      diff.rules
  in
  check int "should have content changes" 1 (List.length content_changes);
  check int "should have add/remove changes" 2 (List.length add_removes)

let test_diff_keyframes () =
  let css_expected = "@keyframes slide{0%{left:0}100%{left:100px}}" in
  let css_actual = "@keyframes slide{0%{left:0}100%{left:200px}}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  (* Test that [@keyframes] differences are detected somehow (may be rules or
     containers) *)
  check bool "should detect keyframes changes" true (not (Td.is_empty diff))

let test_selector_change_shared_parent () =
  let css_expected = ".container .sidebar-left{display:flex}" in
  let css_actual = ".container .sidebar-right{display:flex}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check bool "should not be empty" false (Td.is_empty diff);
  check bool "should detect selector change" true (has_selector_changes diff)

let test_pseudo_element_changes () =
  let css_expected = "ul li::before{content:'•';margin-right:5px}" in
  let css_actual = "ul li::after{content:'•';margin-right:5px}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check bool "should not be empty" false (Td.is_empty diff);
  check bool "should detect changes" true
    (has_rule_changes diff || has_container_changes diff)

let test_attribute_selector_changes () =
  let css_expected = "input[type=\"text\"]{border:1px solid #ccc}" in
  let css_actual = "input[type=\"password\"]{border:1px solid #ccc}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check bool "should not be empty" false (Td.is_empty diff);
  check bool "should detect attribute changes" true (has_rule_changes diff)

let test_important_property_changes () =
  let css_expected = ".urgent{color:red}" in
  let css_actual = ".urgent{color:red !important}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check bool "should not be empty" false (Td.is_empty diff);
  check bool "should detect !important change" true (has_content_changes diff)

let test_empty_rules () =
  let css_expected = ".empty{}" in
  let css_actual = ".empty{color:red}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check bool "should not be empty" false (Td.is_empty diff);
  check bool "should detect property added to empty rule" true
    (has_content_changes diff)

let test_css_variable_references () =
  let css_expected = ".var-test{color:var(--primary-color)}" in
  let css_actual = ".var-test{color:var(--secondary-color)}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check bool "should not be empty" false (Td.is_empty diff);
  check bool "should detect variable reference change" true
    (has_content_changes diff)

let test_nested_media_in_media () =
  let css_expected = "@media screen{@media (min-width:768px){.a{color:red}}}" in
  let css_actual = "@media screen{@media (min-width:768px){.a{color:blue}}}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check bool "should not be empty" false (Td.is_empty diff);
  check int "should have no top-level rule changes" 0 (List.length diff.rules);
  check int "should have container changes" 1 (List.length diff.containers)

let test_nested_layer_in_media () =
  let css_expected = "@media screen{@layer base{.a{margin:0}}}" in
  let css_actual = "@media screen{@layer utilities{.b{padding:0}}}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check bool "should not be empty" false (Td.is_empty diff);
  (* With nested containers, we now have one media container with nested layer
     changes *)
  check int "should have one top-level container" 1
    (List.length diff.containers)

let test_nested_supports_in_layer () =
  let css_expected =
    "@layer base{@supports (display:grid){.grid{display:grid}}}"
  in
  let css_actual =
    "@layer base{@supports (display:grid){.grid{display:flex}}}"
  in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check bool "should not be empty" false (Td.is_empty diff);
  check int "should have container changes" 1 (List.length diff.containers)

let test_deep_nesting_changes () =
  let css_expected =
    "@media screen{@layer base{@supports (display:grid){.deep{color:red}}}}"
  in
  let css_actual =
    "@media screen{@layer base{@supports (display:grid){.deep{color:blue}}}}"
  in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check bool "should not be empty" false (Td.is_empty diff);
  check int "should have container changes for deep nesting" 1
    (List.length diff.containers)

let test_mixed_nested_changes () =
  let css_expected =
    "@media screen{.top{color:red}@layer base{.nested{margin:0}}}"
  in
  let css_actual =
    "@media screen{.top{color:blue}@layer base{.nested{margin:1px}}}"
  in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check bool "should not be empty" false (Td.is_empty diff);
  (* With proper nesting, we have one media container with nested layer
     inside *)
  check int "should have one top-level container" 1
    (List.length diff.containers)

let test_property_modification_detection () =
  let css_expected =
    "@property --my-color{syntax:\"<color>\";inherits:false;initial-value:red}"
  in
  let css_actual =
    "@property --my-color{syntax:\"<color>\";inherits:true;initial-value:red}"
  in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check bool "should detect property modification" false (Td.is_empty diff);
  assert_container_modified `Property "--my-color" diff

let test_property_removal_only () =
  let css_expected =
    "@property \
     --a{syntax:\"<color>\";inherits:false;initial-value:red}@property \
     --b{syntax:\"<length>\";inherits:true;initial-value:0}"
  in
  let css_actual =
    "@property --a{syntax:\"<color>\";inherits:false;initial-value:red}"
  in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check bool "should detect property removal" false (Td.is_empty diff);
  assert_container_removed `Property "--b" diff

let test_keyframes_frame_add_remove () =
  let css_expected = "@keyframes slide{0%{left:0}100%{left:100px}}" in
  let css_actual =
    "@keyframes slide{0%{left:0}50%{left:50px}100%{left:100px}}"
  in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check bool "should detect keyframe changes" false (Td.is_empty diff);
  (* Verify it's detected as a Container_modified with keyframes type *)
  check int "should have container changes" 1 (List.length diff.containers);
  match List.hd diff.containers with
  | Td.Container_modified
      { info = { container_type = `Layer; condition; _ }; _ } ->
      check bool "should have @keyframes prefix" true
        (String.starts_with ~prefix:"@keyframes" condition)
  | _ -> fail "Expected Container_modified for keyframes"

let test_keyframes_rename () =
  let css_expected = "@keyframes slideIn{from{left:0}to{left:100px}}" in
  let css_actual = "@keyframes slideOut{from{left:0}to{left:100px}}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check bool "should detect keyframe rename" false (Td.is_empty diff);
  (* Should detect as removed + added containers *)
  check int "should have 2 container changes" 2 (List.length diff.containers);
  let has_removed =
    List.exists
      (function
        | Td.Container_removed { condition; _ } ->
            String.starts_with ~prefix:"@keyframes slideIn" condition
        | _ -> false)
      diff.containers
  in
  let has_added =
    List.exists
      (function
        | Td.Container_added { condition; _ } ->
            String.starts_with ~prefix:"@keyframes slideOut" condition
        | _ -> false)
      diff.containers
  in
  check bool "should have removed slideIn" true has_removed;
  check bool "should have added slideOut" true has_added

let test_font_face_changes () =
  let css_expected = "@font-face{font-family:MyFont;src:url(font.woff2)}" in
  let css_actual = "@font-face{font-family:MyFont;src:url(font-v2.woff2)}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check bool "should detect font-face changes" false (Td.is_empty diff);
  (* Verify it's detected as a Container_modified with font-face type *)
  check int "should have container changes" 1 (List.length diff.containers);
  match List.hd diff.containers with
  | Td.Container_modified
      { info = { container_type = `Layer; condition = "@font-face"; _ }; _ } ->
      () (* Expected *)
  | _ -> fail "Expected Container_modified for @font-face"

let test_vendor_prefixes () =
  let css_expected =
    ".box{-webkit-transform:rotate(45deg);transform:rotate(45deg)}"
  in
  let css_actual = ".box{transform:rotate(45deg)}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check bool "should detect vendor prefix removal" false (Td.is_empty diff);
  assert_rule_content_changed ".box" diff

let test_selector_list_reordering () =
  let css_expected = ".a,.b,.c>.d{color:red}" in
  let css_actual = ".b,.c>.d,.a{color:red}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  (* Selector list order differences should be detected *)
  check bool "should detect selector list reordering" false (Td.is_empty diff)

let test_escaped_selectors () =
  let css_expected = ".foo\\:bar{background:red}" in
  let css_actual = ".foo\\:baz{background:red}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  check bool "should detect escaped selector changes" false (Td.is_empty diff)

let test_large_diff_structure () =
  let many_rules =
    List.init 5 (fun i ->
        Td.Rule_added
          {
            selector = Fmt.str ".rule-%d" i;
            declarations = sample_declarations;
          })
  in
  let many_containers =
    List.init 3 (fun i ->
        Td.Container_added
          {
            container_type = `Media;
            condition = Fmt.str "(min-width: %dpx)" ((i * 200) + 400);
            rules = [];
          })
  in
  let large_diff = Td.{ rules = many_rules; containers = many_containers } in

  check bool "large diff should not be empty" false (Td.is_empty large_diff);
  check int "should have five rules" 5 (List.length large_diff.rules);
  check int "should have three containers" 3 (List.length large_diff.containers)

(* Tests for added_properties and removed_properties fields *)

let test_property_added_field () =
  (* Test that added_properties field is populated *)
  let css_expected = ".test{color:red}" in
  let css_actual = ".test{color:red;margin:10px}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  let rule = single_rule_diff diff in
  match rule with
  | Td.Rule_content_changed
      { added_properties; removed_properties; property_changes; _ } ->
      check int "should have one added property" 1
        (List.length added_properties);
      check int "should have no removed properties" 0
        (List.length removed_properties);
      check int "should have no property changes" 0
        (List.length property_changes);
      check string "added property name" "margin" (List.hd added_properties)
  | _ -> fail "Expected Rule_content_changed"

let test_property_removed_field () =
  (* Test that removed_properties field is populated *)
  let css_expected = ".test{color:red;margin:10px}" in
  let css_actual = ".test{color:red}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  let rule = single_rule_diff diff in
  match rule with
  | Td.Rule_content_changed
      { added_properties; removed_properties; property_changes; _ } ->
      check int "should have no added properties" 0
        (List.length added_properties);
      check int "should have one removed property" 1
        (List.length removed_properties);
      check int "should have no property changes" 0
        (List.length property_changes);
      check string "removed property name" "margin" (List.hd removed_properties)
  | _ -> fail "Expected Rule_content_changed"

let test_property_added_and_removed () =
  (* Test that both added and removed properties work together *)
  let css_expected = ".test{color:red;margin:10px}" in
  let css_actual = ".test{color:red;padding:5px}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  let rule = single_rule_diff diff in
  match rule with
  | Td.Rule_content_changed
      { added_properties; removed_properties; property_changes; _ } ->
      check int "should have one added property" 1
        (List.length added_properties);
      check int "should have one removed property" 1
        (List.length removed_properties);
      check int "should have no property changes" 0
        (List.length property_changes);
      check string "added property name" "padding" (List.hd added_properties);
      check string "removed property name" "margin" (List.hd removed_properties)
  | _ -> fail "Expected Rule_content_changed"

let test_property_swap_display () =
  (* Test that property swap is displayed consistently with rule swap *)
  let css_expected = ".test{color:red;margin:0}" in
  let css_actual = ".test{margin:0;color:red}" in
  let diff = css_tree_diff ~expected:css_expected ~actual:css_actual in
  let rule = single_rule_diff diff in
  (* Should be Rule_reordered since properties are same, just reordered *)
  match rule with
  | Td.Rule_reordered { selector; _ } ->
      check string "selector" ".test" selector
  | _ -> fail "Expected Rule_reordered for property swap"

(* ===== Test Suite ===== *)

let suite =
  [
    (* Core functionality *)
    test_case "is_empty" `Quick test_is_empty;
    test_case "empty_diff" `Quick test_empty_diff;
    test_case "diff_with_rules" `Quick test_diff_with_rules;
    test_case "diff_with_containers" `Quick test_diff_with_containers;
    test_case "diff_with_both" `Quick test_diff_with_both;
    (* Direct Tree_diff.diff tests *)
    test_case "diff_identical_css" `Quick test_diff_identical_css;
    test_case "diff_rule_added" `Quick test_diff_rule_added;
    test_case "diff_rule_removed" `Quick test_diff_rule_removed;
    test_case "diff_rule_content_changed" `Quick test_diff_rule_content_changed;
    test_case "diff_rule_reordered" `Quick test_diff_rule_reordered;
    test_case "diff_media_query" `Quick test_diff_media_query;
    test_case "diff_nested_media" `Quick test_diff_nested_media;
    test_case "diff_multiple_changes" `Quick test_diff_multiple_changes;
    test_case "diff_keyframes" `Quick test_diff_keyframes;
    (* Rule type structure *)
    test_case "rule_added_structure" `Quick test_rule_added_structure;
    test_case "rule_removed_structure" `Quick test_rule_removed_structure;
    test_case "rule_content_changed_structure" `Quick
      test_rule_content_changed_structure;
    test_case "rule_selector_changed_structure" `Quick
      test_rule_selector_changed_structure;
    test_case "rule_reordered_structure" `Quick test_rule_reordered_structure;
    (* Container type structure *)
    test_case "container_added_structure" `Quick test_container_added_structure;
    test_case "container_removed_structure" `Quick
      test_container_removed_structure;
    test_case "container_modified_structure" `Quick
      test_container_modified_structure;
    test_case "container_types" `Quick test_container_types;
    (* Property changes *)
    test_case "property_change_structure" `Quick test_property_change_structure;
    test_case "property_modification_detection" `Quick
      test_property_modification_detection;
    test_case "property_removal_only" `Quick test_property_removal_only;
    (* Keyframes tests *)
    test_case "keyframes_frame_add_remove" `Quick
      test_keyframes_frame_add_remove;
    test_case "keyframes_rename" `Quick test_keyframes_rename;
    (* Font-face tests *)
    test_case "font_face_changes" `Quick test_font_face_changes;
    (* Vendor prefix tests *)
    test_case "vendor_prefixes" `Quick test_vendor_prefixes;
    (* Selector tests *)
    test_case "selector_list_duplicates_and_whitespace" `Quick
      test_selector_list_reordering;
    test_case "escaped_selectors" `Quick test_escaped_selectors;
    (* Nested structures *)
    test_case "nested_rule_changes" `Quick test_nested_rule_changes;
    (* Edge cases *)
    test_case "selector_change_shared_parent" `Quick
      test_selector_change_shared_parent;
    test_case "pseudo_element_changes" `Quick test_pseudo_element_changes;
    test_case "attribute_selector_changes" `Quick
      test_attribute_selector_changes;
    test_case "important_property_changes" `Quick
      test_important_property_changes;
    test_case "empty_rules" `Quick test_empty_rules;
    test_case "css_variable_references" `Quick test_css_variable_references;
    (* Nested structure tests *)
    test_case "nested_media_in_media" `Quick test_nested_media_in_media;
    test_case "nested_layer_in_media" `Quick test_nested_layer_in_media;
    test_case "nested_supports_in_layer" `Quick test_nested_supports_in_layer;
    test_case "deep_nesting_changes" `Quick test_deep_nesting_changes;
    test_case "mixed_nested_changes" `Quick test_mixed_nested_changes;
    (* Complex cases *)
    test_case "large_diff_structure" `Quick test_large_diff_structure;
    (* Property addition/removal field tests *)
    test_case "property_added_field" `Quick test_property_added_field;
    test_case "property_removed_field" `Quick test_property_removed_field;
    test_case "property_added_and_removed" `Quick
      test_property_added_and_removed;
    test_case "property_swap_display" `Quick test_property_swap_display;
  ]
