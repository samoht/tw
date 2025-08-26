(** Typography utilities for text and font styling *)

open Core

(** {1 Font Size Utilities} *)

val text_xs : t
(** Sets font-size to extra-small. *)

val text_sm : t
(** Sets font-size to small. *)

val text_base : t
(** Sets font-size to base. *)

val text_lg : t
(** Sets font-size to large. *)

val text_xl : t
(** Sets font-size to extra-large. *)

val text_2xl : t
(** Sets font-size to 2× extra-large. *)

val text_3xl : t
(** Sets font-size to 3× extra-large. *)

val text_4xl : t
(** Sets font-size to 4× extra-large. *)

val text_5xl : t
(** Sets font-size to 5× extra-large. *)

val text_6xl : t
(** Sets font-size to 6× extra-large. *)

val text_7xl : t
(** Sets font-size to 7× extra-large. *)

val text_8xl : t
(** Sets font-size to 8× extra-large. *)

val text_9xl : t
(** Sets font-size to 9× extra-large. *)

val font_thin : t
(** Sets font-weight to 100. *)

val font_light : t
(** Sets font-weight to 300. *)

val font_normal : t
(** Sets font-weight to 400. *)

val font_medium : t
(** Sets font-weight to 500. *)

val font_semibold : t
(** Sets font-weight to 600. *)

val font_bold : t
(** Sets font-weight to 700. *)

val font_extrabold : t
(** Sets font-weight to 800. *)

val font_black : t
(** Sets font-weight to 900. *)

val font_sans : t
(** Sets font-family to the sans stack. *)

val font_serif : t
(** Sets font-family to the serif stack. *)

val font_mono : t
(** Sets font-family to the monospace stack. *)

val italic : t
(** Applies italic style. *)

val not_italic : t
(** Disables italic style. *)

val text_left : t
(** Aligns text left. *)

val text_center : t
(** Aligns text center. *)

val text_right : t
(** Aligns text right. *)

val text_justify : t
(** Justifies text. *)

val underline : t
(** Underlines text. *)

val line_through : t
(** Applies a line-through decoration. *)

val no_underline : t
(** Removes text decoration. *)

val underline_solid : t
(** Uses solid underline style. *)

val underline_double : t
(** Uses double underline style. *)

val underline_dotted : t
(** Uses dotted underline style. *)

val underline_dashed : t
(** Uses dashed underline style. *)

val underline_wavy : t
(** Uses wavy underline style. *)

val leading_none : t
(** Sets line-height to none. *)

val leading_tight : t
(** Sets tight line-height. *)

val leading_snug : t
(** Sets snug line-height. *)

val leading_normal : t
(** Sets normal line-height. *)

val leading_relaxed : t
(** Sets relaxed line-height. *)

val leading_loose : t
(** Sets loose line-height. *)

val leading : int -> t
(** [leading n] sets custom line-height based on the spacing scale. *)

(** {1 Whitespace Utilities} *)

val whitespace_normal : t
(** Preserves normal whitespace collapsing and wrapping. *)

val whitespace_nowrap : t
(** Prevents wrapping. *)

val whitespace_pre : t
(** Preserves whitespace and line breaks. *)

val whitespace_pre_line : t
(** Preserves whitespace, but collapses newlines to spaces. *)

val whitespace_pre_wrap : t
(** Preserves whitespace and wraps when needed. *)

(** {1 Letter Spacing Utilities} *)

val tracking_tighter : t
(** Decreases letter spacing. *)

val tracking_tight : t
(** Slightly decreases letter spacing. *)

val tracking_normal : t
(** Normal letter spacing. *)

val tracking_wide : t
(** Slightly increases letter spacing. *)

val tracking_wider : t
(** Increases letter spacing. *)

val tracking_widest : t
(** Maximally increases letter spacing. *)

val uppercase : t
(** Transforms text to uppercase. *)

val lowercase : t
(** Transforms text to lowercase. *)

val capitalize : t
(** Capitalizes words. *)

val normal_case : t
(** Resets text transform. *)

(** {1 Text Underline Offset} *)

val underline_offset_auto : t
(** Sets underline offset to auto. *)

val underline_offset_0 : t
(** Sets underline offset to 0. *)

val underline_offset_1 : t
(** Sets underline offset to 1px. *)

val underline_offset_2 : t
(** Sets underline offset to 2px. *)

val underline_offset_4 : t
(** Sets underline offset to 4px. *)

val underline_offset_8 : t
(** Sets underline offset to 8px. *)

val antialiased : t
(** Enables font-smoothing (antialiasing). *)

(** {1 Rendering} *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a typography utility from string parts. *)
