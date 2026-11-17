(** Line clamp utilities.

    See {{:https://tailwindcss.com/docs/line-clamp}line-clamp} in the Tailwind
    documentation. *)

open Utility

val line_clamp : int -> t
(** [line_clamp n] clamps text to [n] lines (0 disables). *)

module Handler : Utility.Handler
