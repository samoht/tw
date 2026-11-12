(** Font smoothing utilities.

    https://tailwindcss.com/docs/font-smoothing *)

open Utility

val antialiased : t
(** [antialiased] enables font smoothing (antialiasing). *)

val subpixel_antialiased : t
(** [subpixel_antialiased] disables font smoothing (uses subpixel rendering). *)

module Handler : Utility.Handler
