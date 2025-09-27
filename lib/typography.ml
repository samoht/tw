(** Typography utilities for text and font styling

    What's included:
    - Font size scale text-xs through text-9xl with matching line-height
      variables.
    - Font weight, font family, font style, text alignment, text decoration,
      line-height presets and `leading-N`, letter-spacing presets, case.

    What's not:
    - Some advanced text decoration options (thickness, underline offset) and
      typography nuances not present in the typed `Css` API. Extend with `style`
      and raw `Css.property` when needed.

    Parsing contract (`of_string`):
    - Accepts tokens for the above utilities, e.g. ["text"; "xl"],
      ["font"; "semibold"], ["leading"; n], ["tracking"; "wider"],
      ["underline"], ["uppercase"]. Unknown tokens yield `Error (`Msg "Not a
      typography utility")`. *)

open Core
open Css

(* Text size variables with line heights *)
let text_xs_var = Var.theme Css.Length "text-xs" ~order:(6, 0)

let text_xs_lh_var =
  Var.theme Css.Line_height "text-xs--line-height" ~order:(6, 1)

let text_sm_var = Var.theme Css.Length "text-sm" ~order:(6, 2)

let text_sm_lh_var =
  Var.theme Css.Line_height "text-sm--line-height" ~order:(6, 3)

let text_base_var = Var.theme Css.Length "text-base" ~order:(6, 4)

let text_base_lh_var =
  Var.theme Css.Line_height "text-base--line-height" ~order:(6, 5)

let text_lg_var = Var.theme Css.Length "text-lg" ~order:(6, 6)

let text_lg_lh_var =
  Var.theme Css.Line_height "text-lg--line-height" ~order:(6, 7)

let text_xl_var = Var.theme Css.Length "text-xl" ~order:(6, 8)

let text_xl_lh_var =
  Var.theme Css.Line_height "text-xl--line-height" ~order:(6, 9)

let text_2xl_var = Var.theme Css.Length "text-2xl" ~order:(6, 10)

let text_2xl_lh_var =
  Var.theme Css.Line_height "text-2xl--line-height" ~order:(6, 11)

let text_3xl_var = Var.theme Css.Length "text-3xl" ~order:(6, 12)

let text_3xl_lh_var =
  Var.theme Css.Line_height "text-3xl--line-height" ~order:(6, 13)

let text_4xl_var = Var.theme Css.Length "text-4xl" ~order:(6, 14)

let text_4xl_lh_var =
  Var.theme Css.Line_height "text-4xl--line-height" ~order:(6, 15)

let text_5xl_var = Var.theme Css.Length "text-5xl" ~order:(6, 16)

let text_5xl_lh_var =
  Var.theme Css.Line_height "text-5xl--line-height" ~order:(6, 17)

let text_6xl_var = Var.theme Css.Length "text-6xl" ~order:(6, 18)

let text_6xl_lh_var =
  Var.theme Css.Line_height "text-6xl--line-height" ~order:(6, 19)

let text_7xl_var = Var.theme Css.Length "text-7xl" ~order:(6, 20)

let text_7xl_lh_var =
  Var.theme Css.Line_height "text-7xl--line-height" ~order:(6, 21)

let text_8xl_var = Var.theme Css.Length "text-8xl" ~order:(6, 22)

let text_8xl_lh_var =
  Var.theme Css.Line_height "text-8xl--line-height" ~order:(6, 23)

let text_9xl_var = Var.theme Css.Length "text-9xl" ~order:(6, 24)

let text_9xl_lh_var =
  Var.theme Css.Line_height "text-9xl--line-height" ~order:(6, 25)

(* Font weight variables *)
let font_weight_thin_var =
  Var.theme Css.Font_weight "font-weight-thin" ~order:(6, 30)

let font_weight_extralight_var =
  Var.theme Css.Font_weight "font-weight-extralight" ~order:(6, 31)

let font_weight_light_var =
  Var.theme Css.Font_weight "font-weight-light" ~order:(6, 32)

let font_weight_normal_var =
  Var.theme Css.Font_weight "font-weight-normal" ~order:(6, 33)

let font_weight_medium_var =
  Var.theme Css.Font_weight "font-weight-medium" ~order:(6, 34)

let font_weight_semibold_var =
  Var.theme Css.Font_weight "font-weight-semibold" ~order:(6, 35)

let font_weight_bold_var =
  Var.theme Css.Font_weight "font-weight-bold" ~order:(6, 36)

let font_weight_extrabold_var =
  Var.theme Css.Font_weight "font-weight-extrabold" ~order:(6, 37)

let font_weight_black_var =
  Var.theme Css.Font_weight "font-weight-black" ~order:(6, 38)

(* Utility variable for font weight: Channel pattern with @property for
   animations *)
let font_weight_var =
  Var.channel ~needs_property:true Css.Font_weight "tw-font-weight"

(* Leading variable for line-height utilities *)
let leading_var = Var.channel Css.Line_height "tw-leading"

(* Font variant numeric variables for composed value *)
let ordinal_var =
  Var.channel ~needs_property:true Css.Font_variant_numeric_token "tw-ordinal"

let slashed_var =
  Var.channel ~needs_property:true Css.Font_variant_numeric_token
    "tw-slashed-zero"

let figure_var =
  Var.channel ~needs_property:true Css.Font_variant_numeric_token
    "tw-numeric-figure"

let spacing_var =
  Var.channel ~needs_property:true Css.Font_variant_numeric_token
    "tw-numeric-spacing"

let fraction_var =
  Var.channel ~needs_property:true Css.Font_variant_numeric_token
    "tw-numeric-fraction"

(* Helper to get line height calc value *)
let calc_line_height lh_rem size_rem =
  Calc (Expr (Num lh_rem, Div, Num size_rem))

(* Theme record for line height variables *)
type line_height_theme = { leading : Css.declaration * Css.line_height Css.var }

(* Default line height theme with empty fallback for Tailwind's var(--name,)
   pattern *)
let default_line_height_theme : line_height_theme =
  let leading_decl, leading_ref =
    Var.binding leading_var ~fallback:Css.Empty (Num 1.5)
  in
  { leading = (leading_decl, leading_ref) }

module Parse = Parse

(** {1 Font Size Utilities} *)

(* Text utilities use theme record for line height variable reference *)
let text_size_utility name (size_var : Css.length Var.theme)
    (lh_var : Css.line_height Var.theme) size_rem lh_value =
  let size_decl, size_ref = Var.binding size_var (Rem size_rem) in
  let lh_decl, lh_ref = Var.binding lh_var lh_value in
  (* Use shared theme record - no declaration, just reference *)
  let theme = default_line_height_theme in
  let leading_ref = snd theme.leading in
  let leading_with_fallback = Css.with_fallback leading_ref (Css.Var lh_ref) in
  style name
    [
      size_decl;
      lh_decl;
      font_size (Css.Var size_ref);
      line_height (Var leading_with_fallback);
    ]

let text_xs =
  text_size_utility "text-xs" text_xs_var text_xs_lh_var 0.75
    (calc_line_height 1.0 0.75)

let text_sm =
  text_size_utility "text-sm" text_sm_var text_sm_lh_var 0.875
    (calc_line_height 1.25 0.875)

let text_base =
  text_size_utility "text-base" text_base_var text_base_lh_var 1.0
    (calc_line_height 1.5 1.0)

let text_lg =
  text_size_utility "text-lg" text_lg_var text_lg_lh_var 1.125
    (calc_line_height 1.75 1.125)

let text_xl =
  text_size_utility "text-xl" text_xl_var text_xl_lh_var 1.25
    (calc_line_height 1.75 1.25)

let text_2xl =
  text_size_utility "text-2xl" text_2xl_var text_2xl_lh_var 1.5
    (calc_line_height 2.0 1.5)

let text_3xl =
  text_size_utility "text-3xl" text_3xl_var text_3xl_lh_var 1.875
    (calc_line_height 2.25 1.875)

let text_4xl =
  text_size_utility "text-4xl" text_4xl_var text_4xl_lh_var 2.25
    (calc_line_height 2.5 2.25)

let text_5xl =
  text_size_utility "text-5xl" text_5xl_var text_5xl_lh_var 3.0 (Num 1.0)

let text_6xl =
  text_size_utility "text-6xl" text_6xl_var text_6xl_lh_var 3.75 (Num 1.0)

let text_7xl =
  text_size_utility "text-7xl" text_7xl_var text_7xl_lh_var 4.5 (Num 1.0)

let text_8xl =
  text_size_utility "text-8xl" text_8xl_var text_8xl_lh_var 6.0 (Num 1.0)

let text_9xl =
  text_size_utility "text-9xl" text_9xl_var text_9xl_lh_var 8.0 (Num 1.0)

(* Font weight utilities set --tw-font-weight for animation but use theme var
   directly *)
let font_weight_utility name weight_var weight_value =
  let weight_theme_decl, weight_theme_ref =
    Var.binding weight_var weight_value
  in
  let weight_util_decl, _ =
    Var.binding font_weight_var (Css.Var weight_theme_ref)
  in
  (* Get @property rule for font-weight channel (needed for animations) *)
  let property_rules =
    match Var.property_rule font_weight_var with
    | None -> Css.empty
    | Some rule -> rule
  in
  style ("font-" ^ name) ~property_rules
    [
      weight_theme_decl;
      weight_util_decl;
      font_weight (Css.Var weight_theme_ref);
    ]

(* Font weight utilities *)
let font_thin = font_weight_utility "thin" font_weight_thin_var (Weight 100)

let font_extralight =
  font_weight_utility "extralight" font_weight_extralight_var (Weight 200)

let font_light = font_weight_utility "light" font_weight_light_var (Weight 300)

let font_normal =
  font_weight_utility "normal" font_weight_normal_var (Weight 400)

let font_medium =
  font_weight_utility "medium" font_weight_medium_var (Weight 500)

let font_semibold =
  font_weight_utility "semibold" font_weight_semibold_var (Weight 600)

let font_bold = font_weight_utility "bold" font_weight_bold_var (Weight 700)

let font_extrabold =
  font_weight_utility "extrabold" font_weight_extrabold_var (Weight 800)

let font_black = font_weight_utility "black" font_weight_black_var (Weight 900)

(** {1 Font Family Utilities} *)

(* Font family theme variables *)
let font_sans_var = Var.theme Css.Font_family "font-sans" ~order:(1, 0)
let font_serif_var = Var.theme Css.Font_family "font-serif" ~order:(1, 1)
let font_mono_var = Var.theme Css.Font_family "font-mono" ~order:(1, 2)

(* Default font family variables that reference the base font variables *)
let default_font_family_var =
  Var.theme Css.Font_family "default-font-family" ~order:(9, 0)

let default_mono_font_family_var =
  Var.theme Css.Font_family "default-mono-font-family" ~order:(9, 1)

(* Base font family variables for theme layer *)
let default_font_declarations =
  let sans_decl, _ =
    Var.binding font_sans_var
      (List
         [
           Ui_sans_serif;
           System_ui;
           Sans_serif;
           Apple_color_emoji;
           Segoe_ui_emoji;
           Segoe_ui_symbol;
           Noto_color_emoji;
         ])
  in
  let mono_decl, _ =
    Var.binding font_mono_var
      (List
         [
           Ui_monospace;
           SFMono_regular;
           Menlo;
           Monaco;
           Consolas;
           Liberation_mono;
           Courier_new;
           Monospace;
         ])
  in
  [ sans_decl; mono_decl ]

(* Default font family variables that reference the base font variables *)
let default_font_family_declarations =
  let sans_decl, sans_ref =
    Var.binding font_sans_var
      (List
         [
           Ui_sans_serif;
           System_ui;
           Sans_serif;
           Apple_color_emoji;
           Segoe_ui_emoji;
           Segoe_ui_symbol;
           Noto_color_emoji;
         ])
  in
  let mono_decl, mono_ref =
    Var.binding font_mono_var
      (List
         [
           Ui_monospace;
           SFMono_regular;
           Menlo;
           Monaco;
           Consolas;
           Liberation_mono;
           Courier_new;
           Monospace;
         ])
  in
  let default_font_decl, _ =
    Var.binding default_font_family_var (Css.Var sans_ref)
  in
  let default_mono_decl, _ =
    Var.binding default_mono_font_family_var (Css.Var mono_ref)
  in
  [ sans_decl; mono_decl; default_font_decl; default_mono_decl ]

(* Font family utilities use the font variables directly *)
let font_sans =
  let sans_decl, sans_ref =
    Var.binding font_sans_var
      (List
         [
           Ui_sans_serif;
           System_ui;
           Sans_serif;
           Apple_color_emoji;
           Segoe_ui_emoji;
           Segoe_ui_symbol;
           Noto_color_emoji;
         ])
  in
  style "font-sans" [ sans_decl; font_family (Css.Var sans_ref) ]

let font_serif =
  let serif_decl, serif_ref =
    Var.binding font_serif_var
      (List [ Ui_serif; Georgia; Cambria; Times_new_roman; Times; Serif ])
  in
  style "font-serif" [ serif_decl; font_family (Css.Var serif_ref) ]

let font_mono =
  let mono_decl, mono_ref =
    Var.binding font_mono_var
      (List
         [
           Ui_monospace;
           SFMono_regular;
           Menlo;
           Monaco;
           Consolas;
           Liberation_mono;
           Courier_new;
           Monospace;
         ])
  in
  style "font-mono" [ mono_decl; font_family (Css.Var mono_ref) ]

(** {1 Font Style Utilities} *)

let italic = style "italic" [ font_style Italic ]
let not_italic = style "not-italic" [ font_style Normal ]

(** {1 Text Alignment Utilities} *)

let text_left = style "text-left" [ text_align Left ]
let text_center = style "text-center" [ text_align Center ]
let text_right = style "text-right" [ text_align Right ]
let text_justify = style "text-justify" [ text_align Justify ]

(** {1 Text Decoration Utilities} *)

let underline =
  style "underline"
    [
      text_decoration
        (Shorthand
           {
             lines = [ Underline ];
             style = None;
             color = None;
             thickness = None;
           });
    ]

let overline =
  style "overline"
    [
      text_decoration
        (Shorthand
           {
             lines = [ Overline ];
             style = None;
             color = None;
             thickness = None;
           });
    ]

let line_through =
  style "line-through"
    [
      text_decoration
        (Shorthand
           {
             lines = [ Line_through ];
             style = None;
             color = None;
             thickness = None;
           });
    ]

let no_underline = style "no-underline" [ text_decoration None ]

(** {1 Text Decoration Style} *)

let decoration_solid = style "decoration-solid" [ text_decoration_style Solid ]

let decoration_double =
  style "decoration-double" [ text_decoration_style Double ]

let decoration_dotted =
  style "decoration-dotted" [ text_decoration_style Dotted ]

let decoration_dashed =
  style "decoration-dashed" [ text_decoration_style Dashed ]

let decoration_wavy = style "decoration-wavy" [ text_decoration_style Wavy ]

(** {1 Text Decoration Color & Thickness} *)

let decoration_color ?(shade = 500) (color : Color.color) =
  let class_name =
    if Color.is_base_color color then
      String.concat "" [ "decoration-"; Color.pp color ]
    else
      String.concat ""
        [ "decoration-"; Color.pp color; "-"; string_of_int shade ]
  in
  if Color.is_custom_color color then
    let css_color = Color.to_css color shade in
    style class_name
      [
        webkit_text_decoration_color css_color; text_decoration_color css_color;
      ]
  else
    let color_var = Color.get_color_var color shade in
    let default_color =
      Color.to_css color (if Color.is_base_color color then 500 else shade)
    in
    let color_decl, color_ref = Var.binding color_var default_color in
    style class_name
      [
        color_decl;
        webkit_text_decoration_color (Css.Var color_ref);
        text_decoration_color (Css.Var color_ref);
      ]

let decoration_thickness n =
  let class_name = "decoration-" ^ string_of_int n in
  style class_name [ text_decoration_thickness (Px (float_of_int n)) ]

let decoration_from_font =
  style "decoration-from-font" [ text_decoration_thickness From_font ]

(** {1 Line Height Utilities} *)

let leading_none =
  let leading_decl, leading_ref = Var.binding leading_var (Num 1.0) in
  style "leading-none" [ leading_decl; line_height (Css.Var leading_ref) ]

let leading_tight =
  let leading_decl, leading_ref = Var.binding leading_var (Num 1.25) in
  style "leading-tight" [ leading_decl; line_height (Css.Var leading_ref) ]

let leading_snug =
  let leading_decl, leading_ref = Var.binding leading_var (Num 1.375) in
  style "leading-snug" [ leading_decl; line_height (Css.Var leading_ref) ]

let leading_normal =
  let leading_decl, leading_ref = Var.binding leading_var (Num 1.5) in
  style "leading-normal" [ leading_decl; line_height (Css.Var leading_ref) ]

let leading_relaxed =
  let leading_decl, leading_ref = Var.binding leading_var (Num 1.625) in
  style "leading-relaxed" [ leading_decl; line_height (Css.Var leading_ref) ]

let leading_loose =
  let leading_decl, leading_ref = Var.binding leading_var (Num 2.0) in
  style "leading-loose" [ leading_decl; line_height (Css.Var leading_ref) ]

let leading n =
  let class_name = "leading-" ^ string_of_int n in
  let lh_value : line_height = Rem (float_of_int n *. 0.25) in
  let leading_decl, leading_ref = Var.binding leading_var lh_value in
  style class_name [ leading_decl; line_height (Css.Var leading_ref) ]

(* Additional whitespace utilities *)
let whitespace_normal = style "whitespace-normal" [ white_space Normal ]
let whitespace_nowrap = style "whitespace-nowrap" [ white_space Nowrap ]
let whitespace_pre = style "whitespace-pre" [ white_space Pre ]
let whitespace_pre_line = style "whitespace-pre-line" [ white_space Pre_line ]
let whitespace_pre_wrap = style "whitespace-pre-wrap" [ white_space Pre_wrap ]

(** {1 Letter Spacing Utilities} *)

let tracking_tighter = style "tracking-tighter" [ letter_spacing (Em (-0.05)) ]
let tracking_tight = style "tracking-tight" [ letter_spacing (Em (-0.025)) ]
let tracking_normal = style "tracking-normal" [ letter_spacing Zero ]
let tracking_wide = style "tracking-wide" [ letter_spacing (Em 0.025) ]
let tracking_wider = style "tracking-wider" [ letter_spacing (Em 0.05) ]
let tracking_widest = style "tracking-widest" [ letter_spacing (Em 0.1) ]

(** {1 Text Transform Utilities} *)

let uppercase = style "uppercase" [ text_transform Uppercase ]
let lowercase = style "lowercase" [ text_transform Lowercase ]
let capitalize = style "capitalize" [ text_transform Capitalize ]
let normal_case = style "normal-case" [ text_transform None ]

(** {1 Underline Offset} *)

let underline_offset_auto =
  style "underline-offset-auto" [ text_underline_offset "auto" ]

let underline_offset_0 =
  style "underline-offset-0" [ text_underline_offset "0" ]

let underline_offset_1 =
  style "underline-offset-1" [ text_underline_offset "1px" ]

let underline_offset_2 =
  style "underline-offset-2" [ text_underline_offset "2px" ]

let underline_offset_4 =
  style "underline-offset-4" [ text_underline_offset "4px" ]

let underline_offset_8 =
  style "underline-offset-8" [ text_underline_offset "8px" ]

(** {1 Rendering} *)

let antialiased =
  style "antialiased"
    [ webkit_font_smoothing Antialiased; moz_osx_font_smoothing Grayscale ]

(** {1 Vertical Align} *)

let align_baseline = style "align-baseline" [ vertical_align Baseline ]
let align_top = style "align-top" [ vertical_align Top ]
let align_middle = style "align-middle" [ vertical_align Middle ]
let align_bottom = style "align-bottom" [ vertical_align Bottom ]
let align_text_top = style "align-text-top" [ vertical_align Text_top ]
let align_text_bottom = style "align-text-bottom" [ vertical_align Text_bottom ]
let align_sub = style "align-sub" [ vertical_align Sub ]
let align_super = style "align-super" [ vertical_align Super ]

(** {1 List Style} *)

let list_none = style "list-none" [ list_style_type None ]
let list_disc = style "list-disc" [ list_style_type Disc ]
let list_decimal = style "list-decimal" [ list_style_type Decimal ]
let list_inside = style "list-inside" [ list_style_position Inside ]
let list_outside = style "list-outside" [ list_style_position Outside ]
let list_image_none = style "list-image-none" [ list_style_image None ]

let list_image_url url =
  style
    (String.concat "" [ "list-image-url-"; url ])
    [ list_style_image (Url url) ]

(** {1 Text Indent} *)

let indent n =
  let class_name = "indent-" ^ string_of_int n in
  let spacing_decl, spacing_ref = Var.binding Theme.spacing_var (Rem 0.25) in
  style class_name
    [
      spacing_decl;
      text_indent
        (Calc Calc.(length (Css.Var spacing_ref) * float (float_of_int n)));
    ]

(** {1 Line Clamp} *)

let line_clamp n =
  if n = 0 then
    style "line-clamp-0"
      [ webkit_line_clamp 0; overflow Visible; display Block ]
  else
    let class_name = "line-clamp-" ^ string_of_int n in
    style class_name
      [
        webkit_line_clamp n;
        webkit_box_orient Vertical;
        display Webkit_box;
        overflow Hidden;
      ]

(** {1 Content} *)

(* Content variable *)
let content_var =
  Var.property_default Content ~initial:(String "") ~universal:true "tw-content"

let content_none =
  let content_decl, content_ref = Var.binding content_var None in
  let property_rules = Var.property_rules content_var in
  style "content-none" ~property_rules
    [ content (Css.Var content_ref); content_decl; content None ]

let escape_css_string s =
  (* Escape backslashes and quotes inside CSS string literal *)
  let buf = Buffer.create (String.length s + 8) in
  String.iter
    (function
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let content s =
  (* The content value is the string itself, not double-quoted *)
  (* The class name needs to escape the quotes properly for CSS *)
  let class_name =
    String.concat "" [ "content-[\""; escape_css_string s; "\"]" ]
  in
  let content_decl, content_ref = Var.binding content_var (String s) in
  let property_rules = Var.property_rules content_var in
  style class_name ~property_rules
    [
      content (Css.Var content_ref); content_decl; content (Css.Var content_ref);
    ]

(** {1 Text Overflow} *)

let text_ellipsis = style "text-ellipsis" [ text_overflow Ellipsis ]
let text_clip = style "text-clip" [ text_overflow Clip ]

(** {1 Text Wrap} *)

let text_wrap = style "text-wrap" [ Css.text_wrap Wrap ]
let text_nowrap = style "text-nowrap" [ Css.text_wrap No_wrap ]
let text_balance = style "text-balance" [ Css.text_wrap Balance ]
let text_pretty = style "text-pretty" [ Css.text_wrap Pretty ]

(** {1 Word/Overflow Wrap} *)

let break_normal =
  style "break-normal" [ word_break Normal; overflow_wrap Normal ]

let break_words = style "break-words" [ overflow_wrap Break_word ]
let break_all = style "break-all" [ word_break Break_all ]
let break_keep = style "break-keep" [ word_break Keep_all ]
let overflow_wrap_normal = style "overflow-wrap-normal" [ overflow_wrap Normal ]

let overflow_wrap_anywhere =
  style "overflow-wrap-anywhere" [ overflow_wrap Anywhere ]

let overflow_wrap_break_word =
  style "overflow-wrap-break-word" [ overflow_wrap Break_word ]

(** {1 Hyphens} *)

let hyphens_none = style "hyphens-none" [ webkit_hyphens None; hyphens None ]

let hyphens_manual =
  style "hyphens-manual" [ webkit_hyphens Manual; hyphens Manual ]

let hyphens_auto = style "hyphens-auto" [ webkit_hyphens Auto; hyphens Auto ]

(** {1 Font Stretch} *)

let font_stretch_normal =
  style "font-stretch-normal" [ font_stretch (Pct 100.) ]

let font_stretch_condensed =
  style "font-stretch-condensed" [ font_stretch (Pct 75.) ]

let font_stretch_expanded =
  style "font-stretch-expanded" [ font_stretch (Pct 125.) ]

let font_stretch_percent n =
  style
    ("font-stretch-" ^ string_of_int n)
    [ font_stretch (Pct (float_of_int n)) ]

(** {1 Numeric Variants} *)

let normal_nums =
  style "normal-nums" [ font_variant_numeric (Tokens [ Normal ]) ]

(* Default theme for font variant numeric variables *)
type font_variant_theme = {
  ordinal : Css.declaration * Css.font_variant_numeric_token Css.var;
  slashed : Css.declaration * Css.font_variant_numeric_token Css.var;
  figure : Css.declaration * Css.font_variant_numeric_token Css.var;
  spacing : Css.declaration * Css.font_variant_numeric_token Css.var;
  fraction : Css.declaration * Css.font_variant_numeric_token Css.var;
}

(* Default theme with empty fallbacks for Tailwind's var(--name,) pattern *)
(* Created once and shared across all utilities for performance *)
let default_font_variant_theme : font_variant_theme =
  let ordinal_decl, ordinal_ref =
    Var.binding ordinal_var ~fallback:Css.Empty Normal
  in
  let slashed_decl, slashed_ref =
    Var.binding slashed_var ~fallback:Css.Empty Normal
  in
  let figure_decl, figure_ref =
    Var.binding figure_var ~fallback:Css.Empty Normal
  in
  let spacing_decl, spacing_ref =
    Var.binding spacing_var ~fallback:Css.Empty Normal
  in
  let fraction_decl, fraction_ref =
    Var.binding fraction_var ~fallback:Css.Empty Normal
  in
  {
    ordinal = (ordinal_decl, ordinal_ref);
    slashed = (slashed_decl, slashed_ref);
    figure = (figure_decl, figure_ref);
    spacing = (spacing_decl, spacing_ref);
    fraction = (fraction_decl, fraction_ref);
  }

let font_variant_numeric_utility var_to_set value class_name =
  (* Use the shared default theme *)
  let theme = default_font_variant_theme in

  (* Override the specific variable being set - with EMPTY fallback to match
     Tailwind *)
  let updated_theme =
    match var_to_set with
    | `Ordinal ->
        let new_decl, new_ref =
          Var.binding ordinal_var ~fallback:Css.Empty value
        in
        { theme with ordinal = (new_decl, new_ref) }
    | `Slashed ->
        let new_decl, new_ref =
          Var.binding slashed_var ~fallback:Css.Empty value
        in
        { theme with slashed = (new_decl, new_ref) }
    | `Figure ->
        let new_decl, new_ref =
          Var.binding figure_var ~fallback:Css.Empty value
        in
        { theme with figure = (new_decl, new_ref) }
    | `Spacing ->
        let new_decl, new_ref =
          Var.binding spacing_var ~fallback:Css.Empty value
        in
        { theme with spacing = (new_decl, new_ref) }
    | `Fraction ->
        let new_decl, new_ref =
          Var.binding fraction_var ~fallback:Css.Empty value
        in
        { theme with fraction = (new_decl, new_ref) }
  in

  (* Extract only the active declaration (the one being set) *)
  let active_decl =
    match var_to_set with
    | `Ordinal -> fst updated_theme.ordinal
    | `Slashed -> fst updated_theme.slashed
    | `Figure -> fst updated_theme.figure
    | `Spacing -> fst updated_theme.spacing
    | `Fraction -> fst updated_theme.fraction
  in

  (* Compose the font-variant-numeric value using all var references *)
  let composed_value =
    Composed
      {
        ordinal = Some (Css.Var (snd updated_theme.ordinal));
        slashed_zero = Some (Css.Var (snd updated_theme.slashed));
        numeric_figure = Some (Css.Var (snd updated_theme.figure));
        numeric_spacing = Some (Css.Var (snd updated_theme.spacing));
        numeric_fraction = Some (Css.Var (snd updated_theme.fraction));
      }
  in

  (* Get property rules for animation support *)
  let property_rules =
    List.filter_map
      (fun x -> x)
      [
        Var.property_rule ordinal_var;
        Var.property_rule slashed_var;
        Var.property_rule figure_var;
        Var.property_rule spacing_var;
        Var.property_rule fraction_var;
      ]
  in

  style class_name
    ~property_rules:(Css.concat property_rules)
    [ active_decl; font_variant_numeric composed_value ]

let ordinal = font_variant_numeric_utility `Ordinal Ordinal "ordinal"

let slashed_zero =
  font_variant_numeric_utility `Slashed Slashed_zero "slashed-zero"

let lining_nums = font_variant_numeric_utility `Figure Lining_nums "lining-nums"

let oldstyle_nums =
  font_variant_numeric_utility `Figure Oldstyle_nums "oldstyle-nums"

let proportional_nums =
  font_variant_numeric_utility `Spacing Proportional_nums "proportional-nums"

let tabular_nums =
  font_variant_numeric_utility `Spacing Tabular_nums "tabular-nums"

let diagonal_fractions =
  font_variant_numeric_utility `Fraction Diagonal_fractions "diagonal-fractions"

let stacked_fractions =
  font_variant_numeric_utility `Fraction Stacked_fractions "stacked-fractions"

(** {1 Parsing Functions} *)

let ( >|= ) = Parse.( >|= )

let of_string = function
  | [ "text"; "xs" ] -> Ok text_xs
  | [ "text"; "sm" ] -> Ok text_sm
  | [ "text"; "base" ] -> Ok text_base
  | [ "text"; "lg" ] -> Ok text_lg
  | [ "text"; "xl" ] -> Ok text_xl
  | [ "text"; "2xl" ] -> Ok text_2xl
  | [ "text"; "3xl" ] -> Ok text_3xl
  | [ "text"; "4xl" ] -> Ok text_4xl
  | [ "text"; "5xl" ] -> Ok text_5xl
  | [ "text"; "6xl" ] -> Ok text_6xl
  | [ "text"; "7xl" ] -> Ok text_7xl
  | [ "text"; "8xl" ] -> Ok text_8xl
  | [ "text"; "9xl" ] -> Ok text_9xl
  | [ "font"; "thin" ] -> Ok font_thin
  | [ "font"; "extralight" ] -> Ok font_extralight
  | [ "font"; "light" ] -> Ok font_light
  | [ "font"; "normal" ] -> Ok font_normal
  | [ "font"; "medium" ] -> Ok font_medium
  | [ "font"; "semibold" ] -> Ok font_semibold
  | [ "font"; "bold" ] -> Ok font_bold
  | [ "font"; "extrabold" ] -> Ok font_extrabold
  | [ "font"; "black" ] -> Ok font_black
  | [ "font"; "sans" ] -> Ok font_sans
  | [ "font"; "serif" ] -> Ok font_serif
  | [ "font"; "mono" ] -> Ok font_mono
  | [ "italic" ] -> Ok italic
  | [ "not"; "italic" ] -> Ok not_italic
  | [ "text"; "left" ] -> Ok text_left
  | [ "text"; "center" ] -> Ok text_center
  | [ "text"; "right" ] -> Ok text_right
  | [ "text"; "justify" ] -> Ok text_justify
  | [ "underline" ] -> Ok underline
  | [ "overline" ] -> Ok overline
  | [ "line"; "through" ] -> Ok line_through
  | [ "no"; "underline" ] -> Ok no_underline
  | [ "leading"; "none" ] -> Ok leading_none
  | [ "leading"; "tight" ] -> Ok leading_tight
  | [ "leading"; "snug" ] -> Ok leading_snug
  | [ "leading"; "normal" ] -> Ok leading_normal
  | [ "leading"; "relaxed" ] -> Ok leading_relaxed
  | [ "leading"; "loose" ] -> Ok leading_loose
  | [ "leading"; n ] -> (
      match Parse.int_bounded ~name:"leading" ~min:3 ~max:10 n with
      | Ok i -> Ok (leading i)
      | Error e -> Error e)
  | [ "tracking"; "tighter" ] -> Ok tracking_tighter
  | [ "tracking"; "tight" ] -> Ok tracking_tight
  | [ "tracking"; "normal" ] -> Ok tracking_normal
  | [ "tracking"; "wide" ] -> Ok tracking_wide
  | [ "tracking"; "wider" ] -> Ok tracking_wider
  | [ "tracking"; "widest" ] -> Ok tracking_widest
  | [ "decoration"; "from"; "font" ] -> Ok decoration_from_font
  | [ "decoration"; "solid" ] -> Ok decoration_solid
  | [ "decoration"; "double" ] -> Ok decoration_double
  | [ "decoration"; "dotted" ] -> Ok decoration_dotted
  | [ "decoration"; "dashed" ] -> Ok decoration_dashed
  | [ "decoration"; "wavy" ] -> Ok decoration_wavy
  | [ "decoration"; color; shade ] -> (
      match (Color.of_string color, Parse.int_any shade) with
      | Ok c, Ok s -> Ok (decoration_color ~shade:s c)
      | _ -> Error (`Msg "Not a typography utility"))
  | [ "decoration"; n ] -> (
      match Parse.int_pos ~name:"decoration thickness" n with
      | Ok thickness -> Ok (decoration_thickness thickness)
      | Error _ -> (
          match Color.of_string n with
          | Ok c -> Ok (decoration_color c)
          | Error _ -> Error (`Msg "Not a typography utility")))
  | [ "uppercase" ] -> Ok uppercase
  | [ "lowercase" ] -> Ok lowercase
  | [ "capitalize" ] -> Ok capitalize
  | [ "normal"; "case" ] -> Ok normal_case
  | [ "align"; "baseline" ] -> Ok align_baseline
  | [ "align"; "top" ] -> Ok align_top
  | [ "align"; "middle" ] -> Ok align_middle
  | [ "align"; "bottom" ] -> Ok align_bottom
  | [ "align"; "text"; "top" ] -> Ok align_text_top
  | [ "align"; "text"; "bottom" ] -> Ok align_text_bottom
  | [ "align"; "sub" ] -> Ok align_sub
  | [ "align"; "super" ] -> Ok align_super
  | [ "list"; "none" ] -> Ok list_none
  | [ "list"; "disc" ] -> Ok list_disc
  | [ "list"; "decimal" ] -> Ok list_decimal
  | [ "list"; "inside" ] -> Ok list_inside
  | [ "list"; "outside" ] -> Ok list_outside
  | [ "list"; "image"; "none" ] -> Ok list_image_none
  | [ "text"; "ellipsis" ] -> Ok text_ellipsis
  | [ "text"; "clip" ] -> Ok text_clip
  | [ "text"; "wrap" ] -> Ok text_wrap
  | [ "text"; "nowrap" ] -> Ok text_nowrap
  | [ "text"; "balance" ] -> Ok text_balance
  | [ "text"; "pretty" ] -> Ok text_pretty
  | [ "break"; "normal" ] -> Ok break_normal
  | [ "break"; "words" ] -> Ok break_words
  | [ "break"; "all" ] -> Ok break_all
  | [ "break"; "keep" ] -> Ok break_keep
  | [ "overflow"; "wrap"; "normal" ] -> Ok overflow_wrap_normal
  | [ "overflow"; "wrap"; "anywhere" ] -> Ok overflow_wrap_anywhere
  | [ "overflow"; "wrap"; "break"; "word" ] -> Ok overflow_wrap_break_word
  | [ "hyphens"; "none" ] -> Ok hyphens_none
  | [ "hyphens"; "manual" ] -> Ok hyphens_manual
  | [ "hyphens"; "auto" ] -> Ok hyphens_auto
  | [ "font"; "stretch"; "normal" ] -> Ok font_stretch_normal
  | [ "font"; "stretch"; "condensed" ] -> Ok font_stretch_condensed
  | [ "font"; "stretch"; "expanded" ] -> Ok font_stretch_expanded
  | [ "font"; "stretch"; n ] ->
      Parse.int_pos ~name:"font-stretch" n >|= font_stretch_percent
  | [ "normal"; "nums" ] -> Ok normal_nums
  | [ "ordinal" ] -> Ok ordinal
  | [ "slashed"; "zero" ] -> Ok slashed_zero
  | [ "lining"; "nums" ] -> Ok lining_nums
  | [ "oldstyle"; "nums" ] -> Ok oldstyle_nums
  | [ "proportional"; "nums" ] -> Ok proportional_nums
  | [ "tabular"; "nums" ] -> Ok tabular_nums
  | [ "diagonal"; "fractions" ] -> Ok diagonal_fractions
  | [ "stacked"; "fractions" ] -> Ok stacked_fractions
  | [ "indent"; n ] -> Parse.int_pos ~name:"indent" n >|= indent
  | [ "line"; "clamp"; n ] -> Parse.int_pos ~name:"line-clamp" n >|= line_clamp
  | [ "content"; "none" ] -> Ok content_none
  | _ -> Error (`Msg "Not a typography utility")

(** {1 Ordering Support} *)

type typography_info =
  | Text_size of
      string (* xs, sm, base, lg, xl, 2xl, 3xl, 4xl, 5xl, 6xl, 7xl, 8xl, 9xl *)
  | Text_color of string * int (* color name and shade *)
  | Font_weight of int (* 100-900 *)
  | Font_family of string (* sans, serif, mono *)
  | Font_style of string (* italic, not-italic *)
  | Text_align of string (* left, center, right, justify *)
  | Text_decoration of
      string (* underline, overline, line-through, no-underline *)
  | Leading of
      string
      * int option (* none, tight, snug, normal, relaxed, loose, or numeric *)
  | Tracking of string (* tighter, tight, normal, wide, wider, widest *)
  | Text_transform of string (* uppercase, lowercase, capitalize, normal-case *)
  | Unknown

let text_size_order = function
  | "xs" -> 1
  | "sm" -> 2
  | "base" -> 3
  | "lg" -> 4
  | "xl" -> 5
  | "2xl" -> 6
  | "3xl" -> 7
  | "4xl" -> 8
  | "5xl" -> 9
  | "6xl" -> 10
  | "7xl" -> 11
  | "8xl" -> 12
  | "9xl" -> 13
  | _ -> 100

let font_weight_order = function
  | "thin" -> 100
  | "extralight" -> 200
  | "light" -> 300
  | "normal" -> 400
  | "medium" -> 500
  | "semibold" -> 600
  | "bold" -> 700
  | "extrabold" -> 800
  | "black" -> 900
  | _ -> 0

let leading_order = function
  | "none" -> 1
  | "tight" -> 2
  | "snug" -> 3
  | "normal" -> 4
  | "relaxed" -> 5
  | "loose" -> 6
  | _ -> 0

let tracking_order = function
  | "tighter" -> 1
  | "tight" -> 2
  | "normal" -> 3
  | "wide" -> 4
  | "wider" -> 5
  | "widest" -> 6
  | _ -> 0

let parse_class class_name =
  let parts = String.split_on_char '-' class_name in
  match parts with
  | [ "text"; size ]
    when List.mem size
           [
             "xs";
             "sm";
             "base";
             "lg";
             "xl";
             "2xl";
             "3xl";
             "4xl";
             "5xl";
             "6xl";
             "7xl";
             "8xl";
             "9xl";
           ] ->
      Text_size size
  | [ "text"; align ]
    when List.mem align [ "left"; "center"; "right"; "justify" ] ->
      Text_align align
  | [ "text"; color; shade ] -> (
      (* Handle text-blue-500 style color utilities *)
      match int_of_string_opt shade with
      | Some s -> Text_color (color, s)
      | None -> Unknown)
  | [ "text"; color ] ->
      (* Handle text-black, text-white, etc. - but not text alignment *)
      if List.mem color [ "left"; "center"; "right"; "justify" ] then Unknown
        (* Already handled above *)
      else Text_color (color, 0)
  | [ "font"; weight ]
    when List.mem weight
           [
             "thin";
             "extralight";
             "light";
             "normal";
             "medium";
             "semibold";
             "bold";
             "extrabold";
             "black";
           ] ->
      Font_weight (font_weight_order weight)
  | [ "font"; family ] when List.mem family [ "sans"; "serif"; "mono" ] ->
      Font_family family
  | [ "italic" ] -> Font_style "italic"
  | [ "not"; "italic" ] -> Font_style "not-italic"
  | [ "underline" ] -> Text_decoration "underline"
  | [ "overline" ] -> Text_decoration "overline"
  | [ "line"; "through" ] -> Text_decoration "line-through"
  | [ "no"; "underline" ] -> Text_decoration "no-underline"
  | [ "leading"; preset ]
    when List.mem preset
           [ "none"; "tight"; "snug"; "normal"; "relaxed"; "loose" ] ->
      Leading (preset, None)
  | [ "leading"; n ] -> (
      match int_of_string_opt n with
      | Some i when i >= 3 && i <= 10 -> Leading ("numeric", Some i)
      | _ -> Unknown)
  | [ "tracking"; spacing ]
    when List.mem spacing
           [ "tighter"; "tight"; "normal"; "wide"; "wider"; "widest" ] ->
      Tracking spacing
  | [ "uppercase" ] -> Text_transform "uppercase"
  | [ "lowercase" ] -> Text_transform "lowercase"
  | [ "capitalize" ] -> Text_transform "capitalize"
  | [ "normal"; "case" ] -> Text_transform "normal-case"
  | _ -> Unknown

let suborder class_name : int =
  match parse_class class_name with
  | Text_size size -> 1000 + text_size_order size
  | Text_color (color, shade) -> (
      (* Use Color module's ordering for consistency *)
      try
        let _, color_order = Color.utilities_order color in
        1500 + (color_order * 1000) + shade
      with _ -> 1500 + shade)
  | Font_weight weight -> 2000 + weight
  | Font_family family -> (
      3000
      + match family with "sans" -> 1 | "serif" -> 2 | "mono" -> 3 | _ -> 0)
  | Font_style style -> (
      4000 + match style with "italic" -> 1 | "not-italic" -> 2 | _ -> 0)
  | Text_align align -> (
      5000
      +
      match align with
      | "left" -> 1
      | "center" -> 2
      | "right" -> 3
      | "justify" -> 4
      | _ -> 0)
  | Text_decoration deco -> (
      6000
      +
      match deco with
      | "underline" -> 1
      | "overline" -> 2
      | "line-through" -> 3
      | "no-underline" -> 4
      | _ -> 0)
  | Leading (preset, num) -> (
      7000 + match num with Some n -> 100 + n | None -> leading_order preset)
  | Tracking spacing -> 8000 + tracking_order spacing
  | Text_transform transform -> (
      9000
      +
      match transform with
      | "uppercase" -> 1
      | "lowercase" -> 2
      | "capitalize" -> 3
      | "normal-case" -> 4
      | _ -> 0)
  | Unknown -> 10000
