(** Shared test utilities for tools tests *)

module Td = Tw_tools.Tree_diff
module Cc = Tw_tools.Css_compare

(** {1 Testable types} *)

val rule_diff : Td.rule_diff Alcotest.testable
(** [rule_diff] is a testable for rule diffs with nice formatting. *)

val tree_diff : expected:string -> actual:string -> Td.t
(** [tree_diff ~expected ~actual] extracts tree diff or fails if not a tree
    diff. *)

val single_rule_diff : Td.t -> Td.rule_diff
(** [single_rule_diff diff] extracts single rule diff or fails if not exactly
    one. *)

(** {1 Rule assertions} *)

val assert_rule_added : string -> Td.t -> unit
(** [assert_rule_added selector diff] asserts that a rule with the given
    selector was added. *)

val assert_rule_removed : string -> Td.t -> unit
(** [assert_rule_removed selector diff] asserts that a rule with the given
    selector was removed. *)

val assert_rule_content_changed : string -> Td.t -> unit
(** [assert_rule_content_changed selector diff] asserts that a rule with the
    given selector has content changes. *)

val assert_rule_reordered : string -> Td.t -> unit
(** [assert_rule_reordered selector diff] asserts that a rule with the given
    selector was reordered. *)

(** {1 Property change assertions} *)

val assert_property_changed :
  string -> from:string -> to_:string -> Td.declaration list -> unit
(** [assert_property_changed prop ~from ~to_ decls] asserts that a property
    changed from one value to another. *)

val assert_single_property_change :
  prop:string -> from:string -> to_:string -> Td.rule_diff -> unit
(** [assert_single_property_change ~prop ~from ~to_ rule_diff] asserts a single
    property change in a rule. *)

(** {1 Container helpers} *)

val assert_container_added :
  [ `Container | `Layer | `Media | `Property | `Supports ] ->
  string ->
  Td.t ->
  unit
(** [assert_container_added kind name diff] asserts that a container of the
    given kind was added. *)

val assert_container_removed :
  [ `Container | `Layer | `Media | `Property | `Supports ] ->
  string ->
  Td.t ->
  unit
(** [assert_container_removed kind name diff] asserts that a container of the
    given kind was removed. *)

val assert_container_modified :
  [ `Container | `Layer | `Media | `Property | `Supports ] ->
  string ->
  Td.t ->
  unit
(** [assert_container_modified kind name diff] asserts that a container of the
    given kind was modified. *)

(** {1 Selector change assertions} *)

val assert_single_selector_change :
  expected:string ->
  actual:string ->
  old_selector:string ->
  new_selector:string ->
  unit
(** [assert_single_selector_change ~expected ~actual ~old_selector
     ~new_selector] asserts a selector change occurred. *)

(** {1 String utilities} *)

val contains : string -> string -> bool
(** [contains pattern str] checks if [str] contains [pattern] using regex. *)
