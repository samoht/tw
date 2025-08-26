(** Table-related utilities *)

open Core

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

val of_string : string list -> (t, [ `Msg of string ]) result
(** Parse table utilities from tokens. *)
