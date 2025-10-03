(** Test helper functions for CSS comparison and minimization *)

val extract_utilities_layer_rules : Css.t -> Css.statement list
(** [extract_utilities_layer_rules css] extracts rules from the utilities layer
*)

val extract_rule_selectors : Css.statement list -> string list
(** [extract_rule_selectors stmts] extracts selector strings from CSS rules *)

val check_ordering_fails : Tw.t list -> bool
(** [check_ordering_fails utilities] checks if utilities produce different
    ordering than Tailwind CSS *)

val delta_debug : ('a list -> bool) -> 'a list -> 'a list
(** [delta_debug check_fails lst] uses delta debugging (ddmin algorithm) to
    minimize a failing test case *)

val find_minimal_pair : ('a list -> bool) -> 'a list -> 'a list option
(** [find_minimal_pair check_fails lst] finds the first pair of elements that
    causes the check to fail *)

val minimize_failing_case : ('a list -> bool) -> 'a list -> 'a list option
(** [minimize_failing_case check_fails initial] minimizes a failing test case to
    the smallest possible set using delta debugging. Returns [None] if the
    initial case doesn't fail. *)

val check_ordering_matches : test_name:string -> Tw.t list -> unit
(** [check_ordering_matches ~test_name utilities] compares the ordering of
    utilities between our implementation and Tailwind CSS, failing the test if
    they differ *)

(** {1 CSS Test Helpers} *)

val has_layer : string -> Css.t -> bool
(** [has_layer name css] checks if a layer with the given name exists in the
    stylesheet *)

val vars_in_layer : string -> Css.t -> string list
(** [vars_in_layer layer_name css] gets all custom property names from a layer
*)

val has_var_in_layer : string -> string -> Css.t -> bool
(** [has_var_in_layer var_name layer_name css] checks if a variable name exists
    in a layer *)

val selectors_in_layer : string -> Css.t -> string list
(** [selectors_in_layer layer_name css] gets all selectors from a layer *)

val has_selector_in_layer : string -> string -> Css.t -> bool
(** [has_selector_in_layer selector layer_name css] checks if a selector exists
    in a layer *)

val media_conditions : Css.t -> string list
(** [media_conditions css] gets all media query conditions from stylesheet,
    recursively *)

val has_media_condition : string -> Css.t -> bool
(** [has_media_condition condition css] checks if a specific media condition
    exists *)

val inline_has_property : string -> string -> bool
(** [inline_has_property prop_name inline_style] checks if inline style contains
    a specific property *)

val has_var_in_declarations : ?inline:bool -> Css.declaration list -> bool
(** [has_var_in_declarations ~inline decls] checks if declarations contain any
    var() references *)

(** {1 Generic Test Patterns} *)

module type Handler = sig
  type t

  val of_string : string list -> (t, [ `Msg of string ]) result
  val to_style : t -> Tw.Style.t
end

module type Parser = sig
  type t

  val of_string : string list -> (t, [ `Msg of string ]) result
end

val check_handler_roundtrip : (module Handler) -> string list -> unit
(** [check_handler_roundtrip (module H) parts] tests that parsing and
    pretty-printing round-trip correctly. Useful for testing Handler modules. *)

val check_invalid_input : (module Parser) -> string list -> unit
(** [check_invalid_input (module H) input] tests that parsing fails for invalid
    input as expected *)
