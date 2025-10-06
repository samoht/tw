(** Grid item placement utilities (col-{i n}, row-{i n}).

    These utilities control how grid items are placed within a grid container.

    @see <https://tailwindcss.com/docs/grid-column> Tailwind CSS Grid Column
    @see <https://tailwindcss.com/docs/grid-row> Tailwind CSS Grid Row *)

open Utility

(** {1 Column Placement} *)

val col_auto : t
(** [col_auto] sets grid-column to auto. *)

val col_span : int -> t
(** [col_span n] sets grid-column to span n columns (1-12). *)

val col_span_full : t
(** [col_span_full] sets grid-column to span from first to last line. *)

val col_start : int -> t
(** [col_start n] sets grid-column-start to n. *)

val col_start_auto : t
(** [col_start_auto] sets grid-column-start to auto. *)

val col_end : int -> t
(** [col_end n] sets grid-column-end to n. *)

val col_end_auto : t
(** [col_end_auto] sets grid-column-end to auto. *)

(** {1 Row Placement} *)

val row_auto : t
(** [row_auto] sets grid-row to auto. *)

val row_span : int -> t
(** [row_span n] sets grid-row to span n rows (1-12). *)

val row_span_full : t
(** [row_span_full] sets grid-row to span from first to last line. *)

val row_start : int -> t
(** [row_start n] sets grid-row-start to n. *)

val row_start_auto : t
(** [row_start_auto] sets grid-row-start to auto. *)

val row_end : int -> t
(** [row_end n] sets grid-row-end to n. *)

val row_end_auto : t
(** [row_end_auto] sets grid-row-end to auto. *)

module Handler : Utility.Handler
