(** Divide utilities for creating gaps between child elements

    @see <https://tailwindcss.com/docs/divide-width>
      Tailwind CSS Divide Width documentation *)

open Utility

val set_scheme : Scheme.t -> unit
(** [set_scheme scheme] sets the current theme scheme for divide width defaults.
*)

(** {1 Divide Reverse Utilities} *)

val divide_x_reverse : t
(** [divide_x_reverse] reverses the direction of horizontal divide borders
    (useful for RTL layouts). *)

val divide_y_reverse : t
(** [divide_y_reverse] reverses the direction of vertical divide borders. *)

module Handler : Utility.Handler
