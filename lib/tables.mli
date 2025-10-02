(** Table-related utilities

    https://tailwindcss.com/docs/border-collapse
    https://tailwindcss.com/docs/border-spacing
    https://tailwindcss.com/docs/table-layout *)

open Utility

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
