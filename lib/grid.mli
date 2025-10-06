(** Grid display utilities (grid, inline-grid).

    For grid item placement utilities (col-{i n}, row-{i n}), see {!Grid_item}.

    @see <https://tailwindcss.com/docs/display>
      Tailwind CSS Display documentation *)

open Utility

(** {1 Display} *)

val grid : t
(** [grid] creates a grid container. *)

val inline_grid : t
(** [inline_grid] creates an inline grid container. *)

module Handler : Utility.Handler
