(** Typography utilities for text and font styling

    https://tailwindcss.com/docs/font-size
    https://tailwindcss.com/docs/font-weight
    https://tailwindcss.com/docs/font-family
    https://tailwindcss.com/docs/font-style
    https://tailwindcss.com/docs/text-align
    https://tailwindcss.com/docs/text-decoration
    https://tailwindcss.com/docs/line-height
    https://tailwindcss.com/docs/letter-spacing
    https://tailwindcss.com/docs/text-transform
    https://tailwindcss.com/docs/vertical-align
    https://tailwindcss.com/docs/list-style-type
    https://tailwindcss.com/docs/line-clamp
    https://tailwindcss.com/docs/text-overflow
    https://tailwindcss.com/docs/word-break *)

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

(** {1 Whitespace Utilities} *)

val whitespace_normal : t
(** [whitespace_normal] preserves normal whitespace collapsing and wrapping. *)

val whitespace_nowrap : t
(** [whitespace_nowrap] prevents wrapping. *)

val whitespace_pre : t
(** [whitespace_pre] preserves whitespace and line breaks. *)

val whitespace_pre_line : t
(** [whitespace_pre_line] preserves whitespace but collapses newlines to spaces.
*)

val whitespace_pre_wrap : t
(** [whitespace_pre_wrap] preserves whitespace and wraps when needed. *)

(** {1 Letter Spacing Utilities} *)

val tracking_tighter : t
(** [tracking_tighter] decreases letter spacing. *)

val tracking_tight : t
(** [tracking_tight] slightly decreases letter spacing. *)

val tracking_normal : t
(** [tracking_normal] uses normal letter spacing. *)

val tracking_wide : t
(** [tracking_wide] slightly increases letter spacing. *)

val tracking_wider : t
(** [tracking_wider] increases letter spacing. *)

val tracking_widest : t
(** [tracking_widest] maximally increases letter spacing. *)

val uppercase : t
(** [uppercase] transforms text to uppercase. *)

val lowercase : t
(** [lowercase] transforms text to lowercase. *)

val capitalize : t
(** [capitalize] capitalizes words. *)

val normal_case : t
(** [normal_case] resets text transform. *)

(** {1 Text Underline Offset} *)

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

val antialiased : t
(** [antialiased] enables font smoothing (antialiasing). *)

(** {1 Rendering} *)

(** {1 Vertical Align} *)

val align_baseline : t
(** [align_baseline] sets vertical-align to baseline. *)

val align_top : t
(** [align_top] sets vertical-align to top. *)

val align_middle : t
(** [align_middle] sets vertical-align to middle. *)

val align_bottom : t
(** [align_bottom] sets vertical-align to bottom. *)

val align_text_top : t
(** [align_text_top] sets vertical-align to text-top. *)

val align_text_bottom : t
(** [align_text_bottom] sets vertical-align to text-bottom. *)

val align_sub : t
(** [align_sub] sets vertical-align to sub. *)

val align_super : t
(** [align_super] sets vertical-align to super. *)

(** {1 List Style} *)

val list_none : t
(** [list_none] removes bullets/numbers from lists. *)

val list_disc : t
(** [list_disc] uses disc bullets. *)

val list_decimal : t
(** [list_decimal] uses decimal numbering. *)

val list_inside : t
(** [list_inside] places list markers inside content box. *)

val list_outside : t
(** [list_outside] places list markers outside content box. *)

val list_image_none : t
(** [list_image_none] removes list-style-image. *)

val list_image_url : string -> t
(** [list_image_url url] sets list-style-image to the given URL. *)

(** {1 Text Indent} *)

val indent : int -> t
(** [indent n] sets text-indent to [n] times the spacing scale (n * 0.25rem). *)

(** {1 Line Clamp} *)

val line_clamp : int -> t
(** [line_clamp n] clamps text to [n] lines (0 disables). *)

(** {1 Content} *)

val content_none : t
(** [content_none] sets content to none. Useful with pseudo-elements when
    combined with variants. *)

val content : string -> t
(** [content s] sets CSS content to the literal text [s]. Quotes are added
    automatically for you and the class name uses the Tailwind v4 arbitrary
    value syntax: [content-["s"]]. For example, [content "→"] generates the
    class [content-["→"]] and sets [content: "→"]. Use {!content_none} for
    [content: none]. *)

(** {1 Text Overflow} *)

val text_ellipsis : t
(** [text_ellipsis] sets text-overflow to ellipsis. *)

val text_clip : t
(** [text_clip] sets text-overflow to clip. *)

(** {1 Text Wrap} *)

val text_wrap : t
(** [text_wrap] enables automatic text wrapping. *)

val text_nowrap : t
(** [text_nowrap] disables text wrapping. *)

val text_balance : t
(** [text_balance] balances text across lines. *)

val text_pretty : t
(** [text_pretty] optimizes wrapping for readability. *)

(** {1 Word/Overflow Wrap} *)

val break_normal : t
(** [break_normal] uses normal word-breaking and overflow-wrap. *)

val break_words : t
(** [break_words] breaks words as needed (overflow-wrap: break-word). *)

val break_all : t
(** [break_all] breaks within words to prevent overflow. *)

val break_keep : t
(** [break_keep] keeps words intact (CJK keep-all). *)

val overflow_wrap_normal : t
(** [overflow_wrap_normal] uses the browser default overflow wrap behavior. *)

val overflow_wrap_anywhere : t
(** [overflow_wrap_anywhere] allows breaks anywhere to prevent overflow. *)

val overflow_wrap_break_word : t
(** [overflow_wrap_break_word] breaks long words to prevent overflow. *)

(** {1 Hyphens} *)

val hyphens_none : t
(** [hyphens_none] disables automatic hyphenation. *)

val hyphens_manual : t
(** [hyphens_manual] enables manual hyphenation via soft hyphens. *)

val hyphens_auto : t
(** [hyphens_auto] enables automatic hyphenation. *)

(** {1 Font Stretch} *)

val font_stretch_normal : t
(** [font_stretch_normal] uses the normal font stretch. *)

val font_stretch_condensed : t
(** [font_stretch_condensed] uses a condensed font stretch. *)

val font_stretch_expanded : t
(** [font_stretch_expanded] uses an expanded font stretch. *)

val font_stretch_percent : int -> t
(** [font_stretch_percent n] sets font stretch to [n]%. *)

(** {1 Numeric Variants} *)

val normal_nums : t
(** [normal_nums] resets to normal numeric glyphs. *)

val ordinal : t
(** [ordinal] enables ordinal markers (e.g., 1st, 2nd). *)

val slashed_zero : t
(** [slashed_zero] uses a slashed zero glyph. *)

val lining_nums : t
(** [lining_nums] uses lining numeral forms. *)

val oldstyle_nums : t
(** [oldstyle_nums] uses old‑style (text) figures. *)

val proportional_nums : t
(** [proportional_nums] uses proportional-width numbers. *)

val tabular_nums : t
(** [tabular_nums] uses tabular-width numbers. *)

val diagonal_fractions : t
(** [diagonal_fractions] uses diagonal fraction glyphs. *)

val stacked_fractions : t
(** [stacked_fractions] uses stacked fraction glyphs. *)

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

module Handler : Utility.Handler
