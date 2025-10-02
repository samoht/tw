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
