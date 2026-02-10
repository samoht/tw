(** Touch-action utilities

    @see <https://tailwindcss.com/docs/touch-action>
      Tailwind CSS Touch Action documentation *)

open Utility

(** {1 Touch Action Utilities} *)

val touch_auto : t
(** [touch_auto] sets touch-action to auto (browser handles all panning and
    zooming). *)

val touch_none : t
(** [touch_none] disables touch gestures. *)

val touch_manipulation : t
(** [touch_manipulation] enables panning and pinch zoom but disables other
    gestures. *)

val touch_pan_x : t
(** [touch_pan_x] enables horizontal panning. *)

val touch_pan_y : t
(** [touch_pan_y] enables vertical panning. *)

val touch_pan_left : t
(** [touch_pan_left] enables left panning. *)

val touch_pan_right : t
(** [touch_pan_right] enables right panning. *)

val touch_pan_up : t
(** [touch_pan_up] enables upward panning. *)

val touch_pan_down : t
(** [touch_pan_down] enables downward panning. *)

val touch_pinch_zoom : t
(** [touch_pinch_zoom] enables pinch-to-zoom gesture. *)

module Handler : Utility.Handler
