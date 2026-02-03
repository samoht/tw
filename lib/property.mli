(** Helpers for building the {i \@layer properties} block. *)

val split : Css.statement list -> Css.statement list * Css.statement list
(** [split stmts] partitions into ({i \@property} rules, other statements). *)

val dedup : Css.statement list -> Css.statement list
(** [dedup props] deduplicates {i \@property} rules by name, preserving first.
*)

val initial_values : Css.statement list -> (string * string) list
(** Extract (name, initial-value) pairs from {i \@property} rules, in order. *)

val sort_by_order :
  (string -> int) -> (string * string) list -> (string * string) list
(** Sort (name, initial) pairs using a provided name->order function. *)
