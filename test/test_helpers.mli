(** Test helper functions for CSS comparison and minimization *)

val extract_utilities_layer_rules : Css.t -> Css.statement list
(** [extract_utilities_layer_rules css] extracts rules from the utilities layer
*)

val extract_rule_selectors : Css.statement list -> string list
(** [extract_rule_selectors stmts] extracts selector strings from CSS rules *)

val check_ordering_fails : ?forms:bool -> Tw.t list -> bool
(** [check_ordering_fails ~forms utilities] checks if utilities produce
    different ordering than Tailwind CSS *)

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

val check_ordering_matches :
  ?forms:bool -> test_name:string -> Tw.t list -> unit
(** [check_ordering_matches ~test_name utilities] compares the ordering of
    utilities between our implementation and Tailwind CSS, failing the test if
    they differ *)

(** {1 CSS Test Helpers} *)

val selector_testable : Css.Selector.t Alcotest.testable
(** [selector_testable] Alcotest testable for CSS selectors, using structural
    equality and pretty-printing via [Css.Selector.to_string]. *)

val sort_selectors : Css.Selector.t list -> Css.Selector.t list
(** [sort_selectors sels] returns selectors sorted by their string
    representation. Useful for order-insensitive comparisons in tests. *)

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

val selectors_in_media : condition:string -> Css.t -> string list
(** [selectors_in_media ~condition css] returns the selector strings contained
    within the media query that matches [condition]. Returns [[]] if not found
    or if the block has no rules. *)

val has_selector_in_media : condition:string -> selector:string -> Css.t -> bool
(** [has_selector_in_media ~condition ~selector css] checks whether [selector]
    appears inside the media query identified by [condition]. *)

val count_selector_in_media :
  condition:string -> selector:string -> Css.t -> int
(** [count_selector_in_media ~condition ~selector css] counts how many times
    [selector] appears inside the media query identified by [condition]. *)

val selectors_in_media_sel : condition:string -> Css.t -> Css.Selector.t list
(** [selectors_in_media_sel ~condition css] returns the raw selector ASTs inside
    the matching media query. *)

val has_selector_in_media_sel :
  condition:string -> selector:Css.Selector.t -> Css.t -> bool
(** [has_selector_in_media_sel ~condition ~selector css] checks for a selector
    using structural equality on the selector AST. *)

val count_selector_in_media_sel :
  condition:string -> selector:Css.Selector.t -> Css.t -> int
(** [count_selector_in_media_sel ~condition ~selector css] counts selectors by
    structural equality on the selector AST. *)

val inline_has_property : string -> string -> bool
(** [inline_has_property prop_name inline_style] checks if inline style contains
    a specific property *)

val has_var_in_declarations : ?inline:bool -> Css.declaration list -> bool
(** [has_var_in_declarations ~inline decls] checks if declarations contain any
    var() references *)

(** {1 Utility Generators} *)

val spacing_values : int list
(** Common spacing values used in Tailwind utilities *)

val test_rng : Random.State.t
(** Global RNG for randomized tests. Initialized with a random seed printed to
    stderr. Set [TEST_SEED] env var to replay a specific seed. *)

val shuffle : 'a list -> 'a list
(** [shuffle lst] returns a shuffled copy of the list using Fisher-Yates
    algorithm with {!test_rng}. *)

(** {1 Generic Test Patterns} *)

module type Handler = sig
  type t

  val of_class : string -> (t, [ `Msg of string ]) result
  val to_class : t -> string
  val to_style : t -> Tw.Style.t
end

val check_handler_roundtrip : (module Handler) -> string -> unit
(** [check_handler_roundtrip (module H) class_name] tests that parsing with
    [of_class] and converting back with [to_class] round-trip correctly. Useful
    for testing Handler modules. *)

val check_invalid_input : (module Handler) -> string -> unit
(** [check_invalid_input (module H) input] tests that parsing fails for invalid
    input as expected *)

val check_parts : (module Handler) -> string list -> unit
(** [check_parts (module H) parts] concatenates parts with "-" and tests
    roundtrip *)

val check_invalid_parts : (module Handler) -> string list -> unit
(** [check_invalid_parts (module H) parts] concatenates parts with "-" and tests
    that parsing fails *)
