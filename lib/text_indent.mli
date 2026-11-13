(** Text indent utilities.

    https://tailwindcss.com/docs/text-indent *)

open Utility

val indent : int -> t
(** [indent n] sets text-indent to [n] times the spacing scale (n * 0.25rem). *)

val indent' : float -> t
(** [indent' n] is {!indent} for a half-step scale value (e.g. [indent' 0.5] for
    [indent-0.5]). *)

module Handler : Utility.Handler
