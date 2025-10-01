(** Gap and space-between utilities *)

open Style

(** {1 Helper Functions} *)

val to_length : Css.length Css.var -> spacing -> Css.length
(** [to_length spacing_ref s] converts spacing to CSS length using the provided
    spacing variable reference. *)

(** {1 Gap Utilities} *)

val gap : int -> t
(** [gap n] sets gap to [n] × 0.25rem. *)

val gap_x : int -> t
(** [gap_x n] sets column-gap to [n] × 0.25rem. *)

val gap_y : int -> t
(** [gap_y n] sets row-gap to [n] × 0.25rem. *)

val gap' : spacing -> t
(** [gap' s] sets gap using typed spacing [s]. *)

val gap_x' : spacing -> t
(** [gap_x' s] sets column-gap using typed spacing [s]. *)

val gap_y' : spacing -> t
(** [gap_y' s] sets row-gap using typed spacing [s]. *)

(** {1 Special Gap Values} *)

val gap_px : t
(** [gap_px] sets gap to 1px. *)

val gap_full : t
(** [gap_full] sets gap to 100%. *)

val gap_x_px : t
(** [gap_x_px] sets column-gap to 1px. *)

val gap_x_full : t
(** [gap_x_full] sets column-gap to 100%. *)

val gap_y_px : t
(** [gap_y_px] sets row-gap to 1px. *)

val gap_y_full : t
(** [gap_y_full] sets row-gap to 100%. *)

(** {1 Space Between Utilities} *)

val space_x : int -> t
(** [space_x n] creates horizontal space between child elements. *)

val space_y : int -> t
(** [space_y n] creates vertical space between child elements. *)

(** {1 Utility Types} *)

type utility =
  | Gap of [ `All | `X | `Y ] * spacing
  | Space of bool (* negative *) * [ `X | `Y ] * spacing

val to_style : utility -> t
(** [to_style u] converts a structured gap utility to a style. For internal use
    by the Tw module. *)

val suborder : utility -> int
(** [suborder u] returns the ordering value for gap utility [u]. Used for
    deterministic CSS output ordering. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (utility, [ `Msg of string ]) result
(** [of_string parts] parses a gap utility from string parts. Returns an
    internal structured representation. *)
