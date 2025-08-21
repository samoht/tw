(** Typography utilities for text and font styling *)

open Core

(** {1 Font Size Utilities} *)

val text_xs : t
val text_sm : t
val text_base : t
val text_lg : t
val text_xl : t
val text_2xl : t
val text_3xl : t
val text_4xl : t
val text_5xl : t
val text_6xl : t
val text_7xl : t
val text_8xl : t
val text_9xl : t
val font_thin : t
val font_light : t
val font_normal : t
val font_medium : t
val font_semibold : t
val font_bold : t
val font_extrabold : t
val font_black : t
val font_sans : t
val font_serif : t
val font_mono : t
val italic : t
val not_italic : t
val text_left : t
val text_center : t
val text_right : t
val text_justify : t
val underline : t
val line_through : t
val no_underline : t
val leading_none : t
val leading_tight : t
val leading_snug : t
val leading_normal : t
val leading_relaxed : t
val leading_loose : t
val leading : int -> t

(** {1 Letter Spacing Utilities} *)

val tracking_tighter : t
val tracking_tight : t
val tracking_normal : t
val tracking_wide : t
val tracking_wider : t
val tracking_widest : t
val uppercase : t
val lowercase : t
val capitalize : t
val normal_case : t
