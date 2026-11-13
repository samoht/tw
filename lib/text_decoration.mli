(** Text decoration utilities.

    https://tailwindcss.com/docs/text-decoration-line
    https://tailwindcss.com/docs/text-decoration-color
    https://tailwindcss.com/docs/text-decoration-style
    https://tailwindcss.com/docs/text-decoration-thickness
    https://tailwindcss.com/docs/text-underline-offset *)

open Utility

(** {1 Text Decoration Line and Style} *)

val underline : t
(** [underline] underlines text. *)

val overline : t
(** [overline] applies an overline decoration. *)

val line_through : t
(** [line_through] applies a line-through decoration. *)

val no_underline : t
(** [no_underline] removes text decoration. *)

val decoration_solid : t
(** [decoration_solid] sets text-decoration-style to solid. *)

val decoration_double : t
(** [decoration_double] sets text-decoration-style to double. *)

val decoration_dotted : t
(** [decoration_dotted] sets text-decoration-style to dotted. *)

val decoration_dashed : t
(** [decoration_dashed] sets text-decoration-style to dashed. *)

val decoration_wavy : t
(** [decoration_wavy] sets text-decoration-style to wavy. *)

(** {1 Text Decoration Color & Thickness} *)

val decoration_color : ?shade:int -> Color.color -> t
(** [decoration_color ?shade color] sets text-decoration color. *)

val decoration_thickness : int -> t
(** [decoration_thickness n] sets text-decoration thickness to [n]px. *)

val decoration_from_font : t
(** [decoration_from_font] uses the font's default decoration thickness. *)

(** {1 Text Underline Offset} *)

val underline_offset : int -> t
(** [underline_offset n] sets underline offset to [n]px. *)

val underline_offset_auto : t
(** [underline_offset_auto] sets underline offset to auto. *)

val underline_offset_0 : t
(** [underline_offset_0] sets underline offset to 0. *)

val underline_offset_1 : t
(** [underline_offset_1] sets underline offset to 1px. *)

val underline_offset_2 : t
(** [underline_offset_2] sets underline offset to 2px. *)

val underline_offset_4 : t
(** [underline_offset_4] sets underline offset to 4px. *)

val underline_offset_8 : t
(** [underline_offset_8] sets underline offset to 8px. *)

module Handler : Utility.Handler
