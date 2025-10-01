(** Accessibility utilities *)

open Style

(** {1 Utility Types} *)

type utility

val of_string : string list -> (utility, [ `Msg of string ]) result
(** [of_string parts] parses an accessibility utility from string parts. Returns
    an internal structured representation. *)

(** {1 Internal Conversion Functions} *)

val to_style : utility -> t
(** [to_style u] converts a structured accessibility utility to a style. For
    internal use by the Tw module. *)

val suborder : utility -> int
(** [suborder u] returns the ordering value for accessibility utility [u]. Used
    for deterministic CSS output ordering. *)

(** {1 Accessibility Utilities} *)

val forced_color_adjust_auto : t
(** [forced_color_adjust_auto] allows the browser to apply forced color
    adjustments. *)

val forced_color_adjust_none : t
(** [forced_color_adjust_none] prevents the browser from applying forced color
    adjustments. *)
