(** Shared test utilities for tools tests *)

open Alcotest
module Td = Tw_tools.Tree_diff
module Cc = Tw_tools.Css_compare

(** Testable types *)
let rule_diff = testable Td.pp_rule_diff_simple ( = )

(** Get tree diff or fail if not a tree diff *)
let tree_diff ~expected ~actual =
  match Cc.diff ~expected ~actual |> Cc.as_tree_diff with
  | Some d -> d
  | None -> fail "Expected Tree_diff but got different result type"

(** Get single rule diff or fail if not exactly one *)
let single_rule_diff (diff : Td.t) =
  match Td.single_rule_diff diff with
  | Some rule -> rule
  | None -> fail "Expected exactly one rule change"

(** Rule type assertions *)
let assert_rule_added selector (diff : Td.t) =
  let found =
    List.exists
      (function
        | Td.Rule_added { selector = s; _ } -> s = selector | _ -> false)
      diff.rules
  in
  if not found then Alcotest.failf "Expected rule added: %s" selector

let assert_rule_removed selector (diff : Td.t) =
  let found =
    List.exists
      (function
        | Td.Rule_removed { selector = s; _ } -> s = selector | _ -> false)
      diff.rules
  in
  if not found then Alcotest.failf "Expected rule removed: %s" selector

let assert_rule_content_changed selector (diff : Td.t) =
  let found =
    List.exists
      (function
        | Td.Rule_content_changed { selector = s; _ } -> s = selector
        | _ -> false)
      diff.rules
  in
  if not found then Alcotest.failf "Expected rule content changed: %s" selector

let assert_rule_reordered selector (diff : Td.t) =
  let found =
    List.exists
      (function
        | Td.Rule_reordered { selector = s; _ } -> s = selector | _ -> false)
      diff.rules
  in
  if not found then Alcotest.failf "Expected rule reordered: %s" selector

(** Property change assertions *)
let assert_property_changed prop_name ~from ~to_ changes =
  match List.find_opt (fun p -> p.Td.property_name = prop_name) changes with
  | Some p ->
      check string (prop_name ^ " old value") from p.expected_value;
      check string (prop_name ^ " new value") to_ p.actual_value
  | None -> Alcotest.failf "Property '%s' not found in changes" prop_name

let assert_single_property_change ~prop ~from ~to_ rule =
  match rule with
  | Td.Rule_content_changed { property_changes; _ } ->
      check int "property change count" 1 (List.length property_changes);
      assert_property_changed prop ~from ~to_ property_changes
  | _ -> fail "Expected Rule_content_changed"

let assert_container_added container_type condition (diff : Td.t) =
  let found =
    List.exists
      (function
        | Td.Container_added { container_type = ct; condition = c; _ } ->
            ct = container_type && c = condition
        | _ -> false)
      diff.containers
  in
  if not found then
    Alcotest.failf "Expected container added: @%s %s"
      (match container_type with
      | `Media -> "media"
      | `Layer -> "layer"
      | `Supports -> "supports"
      | `Container -> "container"
      | `Property -> "property")
      condition

let assert_container_removed container_type condition (diff : Td.t) =
  let found =
    List.exists
      (function
        | Td.Container_removed { container_type = ct; condition = c; _ } ->
            ct = container_type && c = condition
        | _ -> false)
      diff.containers
  in
  if not found then
    Alcotest.failf "Expected container removed: @%s %s"
      (match container_type with
      | `Media -> "media"
      | `Layer -> "layer"
      | `Supports -> "supports"
      | `Container -> "container"
      | `Property -> "property")
      condition

let assert_container_modified container_type condition (diff : Td.t) =
  let found =
    List.exists
      (function
        | Td.Container_modified
            { info = { container_type = ct; condition = c; _ }; _ } ->
            ct = container_type && c = condition
        | _ -> false)
      diff.containers
  in
  if not found then
    Alcotest.failf "Expected container modified: @%s %s"
      (match container_type with
      | `Media -> "media"
      | `Layer -> "layer"
      | `Supports -> "supports"
      | `Container -> "container"
      | `Property -> "property")
      condition

(** Selector change assertion *)
let assert_single_selector_change ~expected ~actual ~old_selector ~new_selector
    =
  match Cc.diff ~expected ~actual with
  | Cc.Tree_diff d -> (
      check int "one rule change" 1 (List.length d.rules);
      match List.hd d.rules with
      | Td.Rule_selector_changed { old_selector = o; new_selector = n; _ } ->
          check string "old selector" old_selector o;
          check string "new selector" new_selector n
      | Td.Rule_added _ | Td.Rule_removed _ ->
          fail "Expected selector change, not add/remove"
      | _ -> fail "Expected Rule_selector_changed")
  | _ -> fail "Expected Tree_diff for selector change"

(** String utilities *)
let contains s pat = Re.execp (Re.compile (Re.str pat)) s
