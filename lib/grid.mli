(** Grid display utilities (grid, inline-grid).

    For grid item placement utilities (col-{i n}, row-{i n}), see {!Grid_item}.
    See {{:https://tailwindcss.com/docs/display}Display} in the Tailwind
    documentation. *)

open Utility

(** {1 Display} *)

val grid : t
(** [grid] sets [display: grid]. *)

val inline_grid : t
(** [inline_grid] sets [display: inline-grid]. *)

module Handler : Utility.Handler
