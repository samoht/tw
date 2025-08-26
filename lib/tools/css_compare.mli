(** CSS comparison utilities for testing *)

val strip_header : string -> string
(** [strip_header css] removes a leading header comment if present. *)

val compare_css : string -> string -> bool
(** [compare_css a b] returns [true] when [a] and [b] are structurally
    equivalent CSS (ignoring property order within a rule). *)

val format_diff : string -> string -> string
(** [format_diff ours theirs] produces a human-readable, single-difference
    summary between two CSS strings. *)
