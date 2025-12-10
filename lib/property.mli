(** Helpers for building the @layer properties block. *)

val split : Css.statement list -> Css.statement list * Css.statement list
(** [split stmts] partitions into (@property rules, other statements). *)

val dedup : Css.statement list -> Css.statement list
(** [dedup props] deduplicates @property rules by name, preserving first. *)

val initial_values : Css.statement list -> (string * string) list
(** Extract (name, initial-value) pairs from @property rules, in order. *)

val sort_by_order :
  (string -> int) -> (string * string) list -> (string * string) list
(** Sort (name, initial) pairs using a provided name->order function. *)
