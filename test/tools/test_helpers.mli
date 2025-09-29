(** Shared test utilities for tools tests *)

module Td = Tw_tools.Tree_diff
module Cc = Tw_tools.Css_compare

(** {1 Testable types} *)

val rule_diff : Td.rule_diff Alcotest.testable
(** Testable for rule diffs with nice formatting *)

val tree_diff : expected:string -> actual:string -> Td.t
(** Get tree diff or fail if not a tree diff *)

val single_rule_diff : Td.t -> Td.rule_diff
(** Get single rule diff or fail if not exactly one *)

(** {1 Rule assertions} *)

val assert_rule_added : string -> Td.t -> unit
val assert_rule_removed : string -> Td.t -> unit

val assert_rule_content_changed : string -> Td.t -> unit
(** Assert that a rule with the given selector has content changes (properties,
    values, or declaration order) *)

val assert_rule_reordered : string -> Td.t -> unit

(** {1 Property change assertions} *)

val assert_property_changed :
  string -> from:string -> to_:string -> Td.declaration list -> unit

val assert_single_property_change :
  prop:string -> from:string -> to_:string -> Td.rule_diff -> unit

(** {1 Container helpers} *)

val assert_container_added :
  [ `Container | `Layer | `Media | `Property | `Supports ] ->
  string ->
  Td.t ->
  unit

val assert_container_removed :
  [ `Container | `Layer | `Media | `Property | `Supports ] ->
  string ->
  Td.t ->
  unit

val assert_container_modified :
  [ `Container | `Layer | `Media | `Property | `Supports ] ->
  string ->
  Td.t ->
  unit

(** {1 Selector change assertions} *)

val assert_single_selector_change :
  expected:string ->
  actual:string ->
  old_selector:string ->
  new_selector:string ->
  unit

(** {1 String utilities} *)

val contains : string -> string -> bool
(** Check if string contains pattern using regex *)
