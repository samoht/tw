(** Typography utilities for text and font styling *)

open Core

(** {1 Font Size Utilities} *)

val text_xs : t
(** [text_xs] extra small text size. *)

val text_sm : t
(** [text_sm] small text size. *)

val text_base : t
(** [text_base] base text size. *)

val text_lg : t
(** [text_lg] large text size. *)

val text_xl : t
(** [text_xl] extra large text size. *)

val text_2xl : t
(** [text_2xl] 2x large text size. *)

val text_3xl : t
(** [text_3xl] 3x large text size. *)

val text_4xl : t
(** [text_4xl] 4x large text size. *)

val text_5xl : t
(** [text_5xl] 5x large text size. *)

val text_6xl : t
(** [text_6xl] 6x large text size. *)

val text_7xl : t
(** [text_7xl] 7x large text size. *)

val text_8xl : t
(** [text_8xl] 8x large text size. *)

val text_9xl : t
(** [text_9xl] 9x large text size. *)

(** {1 Font Weight Utilities} *)

val font_thin : t
(** [font_thin] thin font weight (100). *)

val font_light : t
(** [font_light] light font weight (300). *)

val font_normal : t
(** [font_normal] normal font weight (400). *)

val font_medium : t
(** [font_medium] medium font weight (500). *)

val font_semibold : t
(** [font_semibold] semibold font weight (600). *)

val font_bold : t
(** [font_bold] bold font weight (700). *)

val font_extrabold : t
(** [font_extrabold] extrabold font weight (800). *)

val font_black : t
(** [font_black] black font weight (900). *)

(** {1 Font Family Utilities} *)

val font_sans : t
(** [font_sans] sans-serif font family. *)

val font_serif : t
(** [font_serif] serif font family. *)

val font_mono : t
(** [font_mono] monospace font family. *)

(** {1 Font Style Utilities} *)

val italic : t
(** [italic] italic font style. *)

val not_italic : t
(** [not_italic] normal font style. *)

(** {1 Text Alignment Utilities} *)

val text_left : t
(** [text_left] left text alignment. *)

val text_center : t
(** [text_center] center text alignment. *)

val text_right : t
(** [text_right] right text alignment. *)

val text_justify : t
(** [text_justify] justified text alignment. *)

(** {1 Text Decoration Utilities} *)

val underline : t
(** [underline] underline text decoration. *)

val line_through : t
(** [line_through] line-through text decoration. *)

val no_underline : t
(** [no_underline] no text decoration. *)

(** {1 Line Height Utilities} *)

val leading_none : t
(** [leading_none] line height of 1. *)

val leading_tight : t
(** [leading_tight] line height of 1.25. *)

val leading_snug : t
(** [leading_snug] line height of 1.375. *)

val leading_normal : t
(** [leading_normal] line height of 1.5. *)

val leading_relaxed : t
(** [leading_relaxed] line height of 1.625. *)

val leading_loose : t
(** [leading_loose] line height of 2. *)

val leading : int -> t
(** [leading n] line height of n * 0.25rem. *)

(** {1 Letter Spacing Utilities} *)

val tracking_tighter : t
(** [tracking_tighter] letter spacing of -0.05em. *)

val tracking_tight : t
(** [tracking_tight] letter spacing of -0.025em. *)

val tracking_normal : t
(** [tracking_normal] normal letter spacing. *)

val tracking_wide : t
(** [tracking_wide] letter spacing of 0.025em. *)

val tracking_wider : t
(** [tracking_wider] letter spacing of 0.05em. *)

val tracking_widest : t
(** [tracking_widest] letter spacing of 0.1em. *)

(** {1 Text Transform Utilities} *)

val uppercase : t
(** [uppercase] uppercase text transform. *)

val lowercase : t
(** [lowercase] lowercase text transform. *)

val capitalize : t
(** [capitalize] capitalize text transform. *)

val normal_case : t
(** [normal_case] no text transform. *)
