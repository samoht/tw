(** Helpers for building the {i \@layer properties} block. *)

open Cascade

val split : Css.statement list -> Css.statement list * Css.statement list
(** [split stmts] partitions into ({i \@property} rules, other statements). *)

val dedup : Css.statement list -> Css.statement list
(** [dedup props] deduplicates {i \@property} rules by name, keeping the first
    of each name. CSS Properties and Values API 1 sec. 2 gives the last
    registration for a name, which is what cascade's canonicaliser keeps;
    nothing rides on the difference here because tw's duplicates all come from
    one {!Var} and are identical. *)

val initial_values : Css.statement list -> (string * Css.declaration) list
(** [initial_values stmts] extracts (name, initial-value declaration) pairs from
    {i \@property} rules, in order. *)

val sort_by_order :
  (string -> int) ->
  (string * Css.declaration) list ->
  (string * Css.declaration) list
(** [sort_by_order f pairs] sorts (name, initial) pairs using a provided
    name->order function. *)
