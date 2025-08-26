(** Accessibility utilities *)

open Core

val forced_color_adjust_auto : t
(** [forced_color_adjust_auto] allows the browser to apply forced color
    adjustments. *)

val forced_color_adjust_none : t
(** [forced_color_adjust_none] prevents the browser from applying forced color
    adjustments. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses an accessibility utility from string parts. *)
