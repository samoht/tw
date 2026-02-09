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
