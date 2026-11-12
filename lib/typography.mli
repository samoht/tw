(** Typography utilities for font family, size, weight and style, text alignment
    and line height.

    https://tailwindcss.com/docs/font-size
    https://tailwindcss.com/docs/font-weight
    https://tailwindcss.com/docs/font-family
    https://tailwindcss.com/docs/font-style
    https://tailwindcss.com/docs/text-align
    https://tailwindcss.com/docs/line-height *)

open Cascade
open Utility

(** {1 Font Size Utilities} *)

val text_xs : t
(** [text_xs] sets font-size to extra-small. *)

val text_sm : t
(** [text_sm] sets font-size to small. *)

val text_base : t
(** [text_base] sets font-size to base. *)

val text_lg : t
(** [text_lg] sets font-size to large. *)

val text_xl : t
(** [text_xl] sets font-size to extra-large. *)

val text_2xl : t
(** [text_2xl] sets font-size to 2× extra-large. *)

val text_3xl : t
(** [text_3xl] sets font-size to 3× extra-large. *)

val text_4xl : t
(** [text_4xl] sets font-size to 4× extra-large. *)

val text_5xl : t
(** [text_5xl] sets font-size to 5× extra-large. *)

val text_6xl : t
(** [text_6xl] sets font-size to 6× extra-large. *)

val text_7xl : t
(** [text_7xl] sets font-size to 7× extra-large. *)

val text_8xl : t
(** [text_8xl] sets font-size to 8× extra-large. *)

val text_9xl : t
(** [text_9xl] sets font-size to 9× extra-large. *)

val font_thin : t
(** [font_thin] sets font-weight to 100. *)

val font_extralight : t
(** [font_extralight] sets font-weight to 200. *)

val font_light : t
(** [font_light] sets font-weight to 300. *)

val font_normal : t
(** [font_normal] sets font-weight to 400. *)

val font_medium : t
(** [font_medium] sets font-weight to 500. *)

val font_semibold : t
(** [font_semibold] sets font-weight to 600. *)

val font_bold : t
(** [font_bold] sets font-weight to 700. *)

val font_extrabold : t
(** [font_extrabold] sets font-weight to 800. *)

val font_black : t
(** [font_black] sets font-weight to 900. *)

val font_sans : t
(** [font_sans] sets font-family to the sans stack. *)

val font_serif : t
(** [font_serif] sets font-family to the serif stack. *)

val font_mono : t
(** [font_mono] sets font-family to the monospace stack. *)

val italic : t
(** [italic] applies italic style. *)

val not_italic : t
(** [not_italic] disables italic style. *)

val text_left : t
(** [text_left] aligns text left. *)

val text_center : t
(** [text_center] aligns text center. *)

val text_right : t
(** [text_right] aligns text right. *)

val text_justify : t
(** [text_justify] justifies text. *)

val text_start : t
(** [text_start] aligns text to the start (left in LTR, right in RTL). *)

val text_end : t
(** [text_end] aligns text to the end (right in LTR, left in RTL). *)

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

val leading_none : t
(** [leading_none] sets line-height to none. *)

val leading_tight : t
(** [leading_tight] sets tight line-height. *)

val leading_snug : t
(** [leading_snug] sets snug line-height. *)

val leading_normal : t
(** [leading_normal] sets normal line-height. *)

val leading_relaxed : t
(** [leading_relaxed] sets relaxed line-height. *)

val leading_loose : t
(** [leading_loose] sets loose line-height. *)

val leading : int -> t
(** [leading n] sets custom line-height based on the spacing scale. *)

val leading' : float -> t
(** [leading' n] is {!leading} for a half-step scale value (e.g. [leading' 1.5]
    for [leading-1.5]). *)

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

val default_font_family_var : Css.font_family Var.theme
(** [default_font_family_var] is the CSS variable for the default font family,
    referencing --font-sans. *)

val default_mono_font_family_var : Css.font_family Var.theme
(** [default_mono_font_family_var] is the CSS variable for the default monospace
    font family, referencing --font-mono. *)

val font_sans_var : Css.font_family Var.theme
(** [font_sans_var] is the CSS variable for the sans-serif font family. *)

val font_serif_var : Css.font_family Var.theme
(** [font_serif_var] is the CSS variable for the serif font family. *)

val font_mono_var : Css.font_family Var.theme
(** [font_mono_var] is the CSS variable for the monospace font family. *)

val default_sans_stack : Css.font_family
(** [default_sans_stack] is Tailwind's default [--font-sans] value: the platform
    system-font stack. *)

val default_mono_stack : Css.font_family
(** [default_mono_stack] is Tailwind's default [--font-mono] value. *)

val default_font_declarations : Css.declaration list
(** [default_font_declarations] are the default font variable declarations for
    the theme layer. *)

val default_font_family_declarations : Css.declaration list
(** [default_font_family_declarations] are the default font-family variable
    declarations for the theme layer. *)

(** {1 Internal types} *)

val content_var : Css.content Var.property_default
(** [content_var] is the CSS variable for content, used by before/after
    pseudo-elements. Initial value is empty string. *)

module Typography_early : Utility.Handler
(** Typography handler - text sizes, font weights, font families, font style,
    text alignment and leading. Most sort at priority 24; font style joins the
    late text families at priority 26. *)

module Typography_late : Utility.Handler
(** Late typography handler - text decoration line, colour, style and thickness,
    and the underline offset. *)
