(** Identity and merging over cascade statements.

    Every [\@apply] and every declared utility brings back the statements its
    utilities hoist, so the same [\@property] or rule arrives many times. Two
    statements are the same when cascade says they are equal, not when they
    print the same text. *)

val seen :
  (int, Cascade.Css.statement) Hashtbl.t -> Cascade.Css.statement -> bool
(** [seen table stmt] is [true] when [table] already holds [stmt], and records
    [stmt] in [table] when it does not. *)

val dedup : Cascade.Css.statement list -> Cascade.Css.statement list
(** [dedup stmts] is [stmts] with every repeat of an earlier statement left out.
*)

val merge_same_selector :
  Cascade.Css.statement list -> Cascade.Css.statement list
(** [merge_same_selector stmts] joins each run of adjacent plain rules over an
    equal selector into one rule holding their declarations in order. A rule
    with nested rules is left as it is. *)
