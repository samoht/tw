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

val border_spacing : float -> t
(** [border_spacing n] sets uniform table border-spacing using CSS variables.
    [n] is the spacing multiplier (e.g., 4.0 for border-spacing-4, 0.5 for
    border-spacing-0.5). *)

val border_spacing_x : float -> t
(** [border_spacing_x n] sets horizontal table border-spacing. [n] is the
    spacing multiplier (e.g., 4.0 for border-spacing-x-4). *)

val border_spacing_y : float -> t
(** [border_spacing_y n] sets vertical table border-spacing. [n] is the spacing
    multiplier (e.g., 4.0 for border-spacing-y-4). *)

val table_auto : t
(** [table_auto] uses automatic table layout. *)

val table_fixed : t
(** [table_fixed] uses fixed table layout. *)

val caption_top : t
(** [caption_top] positions a table caption above the table. *)

val caption_bottom : t
(** [caption_bottom] positions a table caption below the table. *)

module Handler : Utility.Handler
