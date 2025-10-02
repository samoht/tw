(** Grid utilities

    @see <https://tailwindcss.com/docs/grid-template-columns>
      Tailwind CSS Grid documentation
    @see <https://tailwindcss.com/docs/grid-column> Grid Column
    @see <https://tailwindcss.com/docs/grid-row> Grid Row *)

open Utility

(** {1 Display} *)

val grid : t
(** [grid] creates a grid container. *)

val inline_grid : t
(** [inline_grid] creates an inline grid container. *)

(** {1 Column} *)

val col_auto : t
(** [col_auto] sets grid-column to auto. *)

val col_span : int -> t
(** [col_span n] sets grid-column to span n columns. *)

val col_span_full : t
(** [col_span_full] sets grid-column to span all columns. *)

val col_start : int -> t
(** [col_start n] sets grid-column-start to n. *)

val col_start_auto : t
(** [col_start_auto] sets grid-column-start to auto. *)

val col_end : int -> t
(** [col_end n] sets grid-column-end to n. *)

val col_end_auto : t
(** [col_end_auto] sets grid-column-end to auto. *)

(** {1 Row} *)

val row_auto : t
(** [row_auto] sets grid-row to auto. *)

val row_span : int -> t
(** [row_span n] sets grid-row to span n rows. *)

val row_span_full : t
(** [row_span_full] sets grid-row to span all rows. *)

val row_start : int -> t
(** [row_start n] sets grid-row-start to n. *)

val row_start_auto : t
(** [row_start_auto] sets grid-row-start to auto. *)

val row_end : int -> t
(** [row_end n] sets grid-row-end to n. *)

val row_end_auto : t
(** [row_end_auto] sets grid-row-end to auto. *)

module Handler : sig
  type t

  val of_string : string list -> (t, [ `Msg of string ]) result
  val suborder : t -> int
  val to_style : t -> Style.t
  val order : t -> int * int
end
