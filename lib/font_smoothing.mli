(** Font smoothing utilities.

    See {{:https://tailwindcss.com/docs/font-smoothing}font-smoothing} in the
    Tailwind documentation. *)

open Utility

val antialiased : t
(** [antialiased] enables font smoothing (antialiasing). *)

val subpixel_antialiased : t
(** [subpixel_antialiased] disables font smoothing (uses subpixel rendering). *)

module Handler : Utility.Handler
