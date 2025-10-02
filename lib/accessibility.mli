(** Accessibility utilities

    https://tailwindcss.com/docs/forced-color-adjust *)

open Utility

(** {1 Accessibility Utilities} *)

val forced_color_adjust_auto : t
(** [forced_color_adjust_auto] allows the browser to apply forced color
    adjustments. *)

val forced_color_adjust_none : t
(** [forced_color_adjust_none] prevents the browser from applying forced color
    adjustments. *)
