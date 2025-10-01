(** Table-related utilities *)

open Style

(** {1 Utility Types} *)

type utility

val to_style : utility -> t
(** [to_style u] converts a structured table utility to a style. For internal
    use by the Tw module. *)

val suborder : utility -> int
(** [suborder u] returns the ordering value for table utility [u]. Used for
    deterministic CSS output ordering. *)

(** {1 Table Utilities} *)

val border_collapse : t
(** [border_collapse] collapses table borders. *)

val border_separate : t
(** [border_separate] separates table borders. *)

val border_spacing : int -> t
(** [border_spacing n] sets uniform table border-spacing. *)

val table_auto : t
(** [table_auto] uses automatic table layout. *)

val table_fixed : t
(** [table_fixed] uses fixed table layout. *)

val of_string : string list -> (utility, [ `Msg of string ]) result
(** [of_string parts] parses table utilities from string [parts]. *)
