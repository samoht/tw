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

(* Helper to create text size variables with line height *)
let var_text_size var_t lh_var_t size_rem lh_multiplier =
  let size_def, size_var = Var.theme var_t (Rem size_rem) in
  (* Line height as calc(multiplier / size) to match Tailwind v4 *)
  let lh_val =
    Calc (Calc.div (Calc.float lh_multiplier) (Calc.float size_rem))
  in
  let lh_def, lh_var = Var.theme lh_var_t lh_val in
  (size_def, size_var, lh_def, lh_var)

(* Each text size has its own line height variable that's used directly *)

(* Text size variables defined at top level with Tailwind v4 line height
   multipliers *)
let text_xs_size_def, text_xs_var, text_xs_lh_def, text_xs_lh_var =
  var_text_size Var.Text_xs Var.Text_xs_line_height 0.75 1.0

let text_sm_size_def, text_sm_var, text_sm_lh_def, text_sm_lh_var =
  var_text_size Var.Text_sm Var.Text_sm_line_height 0.875 1.25

let text_base_size_def, text_base_var, text_base_lh_def, text_base_lh_var =
  var_text_size Var.Text_base Var.Text_base_line_height 1.0 1.5

let text_lg_size_def, text_lg_var, text_lg_lh_def, text_lg_lh_var =
  var_text_size Var.Text_lg Var.Text_lg_line_height 1.125 1.75

let text_xl_size_def, text_xl_var, text_xl_lh_def, text_xl_lh_var =
  var_text_size Var.Text_xl Var.Text_xl_line_height 1.25 1.75

let text_2xl_size_def, text_2xl_var, text_2xl_lh_def, text_2xl_lh_var =
  var_text_size Var.Text_2xl Var.Text_2xl_line_height 1.5 2.0

let text_3xl_size_def, text_3xl_var, text_3xl_lh_def, text_3xl_lh_var =
  var_text_size Var.Text_3xl Var.Text_3xl_line_height 1.875 2.25

let text_4xl_size_def, text_4xl_var, text_4xl_lh_def, text_4xl_lh_var =
  var_text_size Var.Text_4xl Var.Text_4xl_line_height 2.25 2.5

let text_5xl_size_def, text_5xl_var, text_5xl_lh_def, text_5xl_lh_var =
  var_text_size Var.Text_5xl Var.Text_5xl_line_height 3.0 1.0

let text_6xl_size_def, text_6xl_var, text_6xl_lh_def, text_6xl_lh_var =
  var_text_size Var.Text_6xl Var.Text_6xl_line_height 3.75 1.0

let text_7xl_size_def, text_7xl_var, text_7xl_lh_def, text_7xl_lh_var =
  var_text_size Var.Text_7xl Var.Text_7xl_line_height 4.5 1.0

let text_8xl_size_def, text_8xl_var, text_8xl_lh_def, text_8xl_lh_var =
  var_text_size Var.Text_8xl Var.Text_8xl_line_height 6.0 1.0

let text_9xl_size_def, text_9xl_var, text_9xl_lh_def, text_9xl_lh_var =
  var_text_size Var.Text_9xl Var.Text_9xl_line_height 8.0 1.0

let font_weight_thin_def, font_weight_thin_var =
  Var.theme Var.Font_weight_thin (Weight 100)

let font_weight_extralight_def, font_weight_extralight_var =
  Var.theme Var.Font_weight_extralight (Weight 200)

let font_weight_light_def, font_weight_light_var =
  Var.theme Var.Font_weight_light (Weight 300)

let font_weight_normal_def, font_weight_normal_var =
  Var.theme Var.Font_weight_normal (Weight 400)

let font_weight_medium_def, font_weight_medium_var =
  Var.theme Var.Font_weight_medium (Weight 500)

let font_weight_semibold_def, font_weight_semibold_var =
  Var.theme Var.Font_weight_semibold (Weight 600)

let font_weight_bold_def, font_weight_bold_var =
  Var.theme Var.Font_weight_bold (Weight 700)

let font_weight_extrabold_def, font_weight_extrabold_var =
  Var.theme Var.Font_weight_extrabold (Weight 800)

let font_weight_black_def, font_weight_black_var =
  Var.theme Var.Font_weight_black (Weight 900)

module Parse = Parse

(** {1 Font Size Utilities} *)

let text_xs =
  let _leading_def, leading_var =
    Var.utility Var.Leading ~fallback:(Var text_xs_lh_var) Zero
  in
  style "text-xs"
    [
      text_xs_size_def;
      text_xs_lh_def;
      font_size (Var text_xs_var);
      line_height (Var leading_var);
    ]

let text_sm =
  let _leading_def, leading_var =
    Var.utility Var.Leading ~fallback:(Var text_sm_lh_var) Zero
  in
  style "text-sm"
    [
      text_sm_size_def;
      text_sm_lh_def;
      font_size (Var text_sm_var);
      line_height (Var leading_var);
    ]

let text_base =
  let _leading_def, leading_var =
    Var.utility Var.Leading ~fallback:(Var text_base_lh_var) Zero
  in
  style "text-base"
    [
      text_base_size_def;
      text_base_lh_def;
      font_size (Var text_base_var);
      line_height (Var leading_var);
    ]

let text_lg =
  let _leading_def, leading_var =
    Var.utility Var.Leading ~fallback:(Var text_lg_lh_var) Zero
  in
  style "text-lg"
    [
      text_lg_size_def;
      text_lg_lh_def;
      font_size (Var text_lg_var);
      line_height (Var leading_var);
    ]

let text_xl =
  let _leading_def, leading_var =
    Var.utility Var.Leading ~fallback:(Var text_xl_lh_var) Zero
  in
  style "text-xl"
    [
      text_xl_size_def;
      text_xl_lh_def;
      font_size (Var text_xl_var);
      line_height (Var leading_var);
    ]

let text_2xl =
  let _leading_def, leading_var =
    Var.utility Var.Leading ~fallback:(Var text_2xl_lh_var) Zero
  in
  style "text-2xl"
    [
      text_2xl_size_def;
      text_2xl_lh_def;
      font_size (Var text_2xl_var);
      line_height (Var leading_var);
    ]

let text_3xl =
  let _leading_def, leading_var =
    Var.utility Var.Leading ~fallback:(Var text_3xl_lh_var) Zero
  in
  style "text-3xl"
    [
      text_3xl_size_def;
      text_3xl_lh_def;
      font_size (Var text_3xl_var);
      line_height (Var leading_var);
    ]

let text_4xl =
  let _leading_def, leading_var =
    Var.utility Var.Leading ~fallback:(Var text_4xl_lh_var) Zero
  in
  style "text-4xl"
    [
      text_4xl_size_def;
      text_4xl_lh_def;
      font_size (Var text_4xl_var);
      line_height (Var leading_var);
    ]

let text_5xl =
  let _leading_def, leading_var =
    Var.utility Var.Leading ~fallback:(Var text_5xl_lh_var) Zero
  in
  style "text-5xl"
    [
      text_5xl_size_def;
      text_5xl_lh_def;
      font_size (Var text_5xl_var);
      line_height (Var leading_var);
    ]

let text_6xl =
  let _leading_def, leading_var =
    Var.utility Var.Leading ~fallback:(Var text_6xl_lh_var) Zero
  in
  style "text-6xl"
    [
      text_6xl_size_def;
      text_6xl_lh_def;
      font_size (Var text_6xl_var);
      line_height (Var leading_var);
    ]

let text_7xl =
  let _leading_def, leading_var =
    Var.utility Var.Leading ~fallback:(Var text_7xl_lh_var) Zero
  in
  style "text-7xl"
    [
      text_7xl_size_def;
      text_7xl_lh_def;
      font_size (Var text_7xl_var);
      line_height (Var leading_var);
    ]

let text_8xl =
  let _leading_def, leading_var =
    Var.utility Var.Leading ~fallback:(Var text_8xl_lh_var) Zero
  in
  style "text-8xl"
    [
      text_8xl_size_def;
      text_8xl_lh_def;
      font_size (Var text_8xl_var);
      line_height (Var leading_var);
    ]

let text_9xl =
  let _leading_def, leading_var =
    Var.utility Var.Leading ~fallback:(Var text_9xl_lh_var) Zero
  in
  style "text-9xl"
    [
      text_9xl_size_def;
      text_9xl_lh_def;
      font_size (Var text_9xl_var);
      line_height (Var leading_var);
    ]

(* Create the @property rule for --tw-font-weight *)
let property_rules =
  [
    Var.property Var.Font_weight ~syntax:"*" ~inherits:false ~initial:"initial";
  ]

(* Font weight utilities using variables *)
let font_thin =
  let fw_def, _fw_var =
    Var.utility Var.Font_weight (Var font_weight_thin_var)
  in
  style "font-thin" ~property_rules
    [ font_weight_thin_def; fw_def; font_weight (Var font_weight_thin_var) ]

let font_extralight =
  let fw_def, _fw_var =
    Var.utility Var.Font_weight (Var font_weight_extralight_var)
  in
  style "font-extralight" ~property_rules
    [
      font_weight_extralight_def;
      fw_def;
      font_weight (Var font_weight_extralight_var);
    ]

let font_light =
  let fw_def, _fw_var =
    Var.utility Var.Font_weight (Var font_weight_light_var)
  in
  style "font-light" ~property_rules
    [ font_weight_light_def; fw_def; font_weight (Var font_weight_light_var) ]

let font_normal =
  let fw_def, _fw_var =
    Var.utility Var.Font_weight (Var font_weight_normal_var)
  in
  style "font-normal" ~property_rules
    [ font_weight_normal_def; fw_def; font_weight (Var font_weight_normal_var) ]

let font_medium =
  let fw_def, _fw_var =
    Var.utility Var.Font_weight (Var font_weight_medium_var)
  in
  style "font-medium" ~property_rules
    [ font_weight_medium_def; fw_def; font_weight (Var font_weight_medium_var) ]

let font_semibold =
  let fw_def, _fw_var =
    Var.utility Var.Font_weight (Var font_weight_semibold_var)
  in
  style "font-semibold" ~property_rules
    [
      font_weight_semibold_def;
      fw_def;
      font_weight (Var font_weight_semibold_var);
    ]

let font_bold =
  let fw_def, _fw_var =
    Var.utility Var.Font_weight (Var font_weight_bold_var)
  in
  style "font-bold" ~property_rules
    [ font_weight_bold_def; fw_def; font_weight (Var font_weight_bold_var) ]

let font_extrabold =
  let fw_def, _fw_var =
    Var.utility Var.Font_weight (Var font_weight_extrabold_var)
  in
  style "font-extrabold" ~property_rules
    [
      font_weight_extrabold_def;
      fw_def;
      font_weight (Var font_weight_extrabold_var);
    ]

let font_black =
  let fw_def, _fw_var =
    Var.utility Var.Font_weight (Var font_weight_black_var)
  in
  style "font-black" ~property_rules
    [ font_weight_black_def; fw_def; font_weight (Var font_weight_black_var) ]

(** {1 Font Family Utilities} *)

(* Define font variables for the theme layer - these are exported for use in
   theme generation *)
let font_sans_def, font_sans_var =
  Var.theme Var.Font_sans
    [
      Ui_sans_serif;
      System_ui;
      Sans_serif;
      Apple_color_emoji;
      Segoe_ui_emoji;
      Segoe_ui_symbol;
      Noto_color_emoji;
    ]

let font_serif_def, font_serif_var =
  Var.theme Var.Font_serif
    [ Ui_serif; Georgia; Cambria; Times_new_roman; Times; Serif ]

let font_mono_def, font_mono_var =
  Var.theme Var.Font_mono
    [
      Ui_monospace;
      SFMono_regular;
      Menlo;
      Monaco;
      Consolas;
      Liberation_mono;
      Courier_new;
      Monospace;
    ]

(* Export font declarations for theme layer *)
let default_font_declarations = [ font_sans_def; font_serif_def; font_mono_def ]

let default_font_family_def, default_font_family_var =
  let fallback =
    [
      Ui_sans_serif;
      System_ui;
      Sans_serif;
      Apple_color_emoji;
      Segoe_ui_emoji;
      Segoe_ui_symbol;
      Noto_color_emoji;
    ]
  in
  Var.theme Var.Default_font_family [ Var font_sans_var ] ~fallback

let default_mono_font_family_def, default_mono_font_family_var =
  let fallback =
    [
      Ui_monospace;
      SFMono_regular;
      Menlo;
      Monaco;
      Consolas;
      Liberation_mono;
      Courier_new;
      Monospace;
    ]
  in
  Var.theme Var.Default_mono_font_family [ Var font_mono_var ] ~fallback

let default_font_family_declarations =
  [ default_font_family_def; default_mono_font_family_def ]

(* Font family utilities use the font variables directly *)
let font_sans =
  style "font-sans" [ font_sans_def; font_family [ Var font_sans_var ] ]

let font_serif =
  style "font-serif" [ font_serif_def; font_family [ Var font_serif_var ] ]

let font_mono =
  style "font-mono" [ font_mono_def; font_family [ Var font_mono_var ] ]

(** {1 Font Style Utilities} *)

let italic = style "italic" [ font_style Italic ]
let not_italic = style "not-italic" [ font_style Normal ]

(** {1 Text Alignment Utilities} *)

let text_left = style "text-left" [ text_align Left ]
let text_center = style "text-center" [ text_align Center ]
let text_right = style "text-right" [ text_align Right ]
let text_justify = style "text-justify" [ text_align Justify ]

(** {1 Text Decoration Utilities} *)

let underline = style "underline" [ text_decoration Underline ]
let overline = style "overline" [ text_decoration Overline ]
let line_through = style "line-through" [ text_decoration Line_through ]
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
    if Color.is_base_color color then Pp.str [ "decoration-"; Color.pp color ]
    else Pp.str [ "decoration-"; Color.pp color; "-"; string_of_int shade ]
  in
  if Color.is_custom_color color then
    let css_color = Color.to_css color shade in
    style class_name
      [
        webkit_text_decoration_color css_color; text_decoration_color css_color;
      ]
  else
    let default_color =
      Color.to_css color (if Color.is_base_color color then 500 else shade)
    in
    let var_def, css_var =
      Var.theme
        (Var.Color
           ( Color.pp color,
             if Color.is_base_color color then None else Some shade ))
        default_color
    in
    style class_name
      [
        var_def;
        webkit_text_decoration_color (Var css_var);
        text_decoration_color (Var css_var);
      ]

let decoration_thickness n =
  let class_name = "decoration-" ^ string_of_int n in
  style class_name [ text_decoration_thickness (string_of_int n ^ "px") ]

let decoration_from_font =
  style "decoration-from-font" [ text_decoration_thickness "from-font" ]

(** {1 Line Height Utilities} *)

let leading_none = style "leading-none" [ line_height (Num 1.0) ]
let leading_tight = style "leading-tight" [ line_height (Num 1.25) ]
let leading_snug = style "leading-snug" [ line_height (Num 1.375) ]
let leading_normal = style "leading-normal" [ line_height (Num 1.5) ]
let leading_relaxed = style "leading-relaxed" [ line_height (Num 1.625) ]
let leading_loose = style "leading-loose" [ line_height (Num 2.0) ]

let leading n =
  let class_name = "leading-" ^ string_of_int n in
  style class_name [ line_height (Rem (float_of_int n *. 0.25)) ]

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
let list_image_none = style "list-image-none" [ list_style_image None_img ]

let list_image_url url =
  style (Pp.str [ "list-image-url-"; url ]) [ list_style_image (Url url) ]

(** {1 Text Indent} *)

let indent n =
  let class_name = "indent-" ^ string_of_int n in
  let spacing_def, spacing_var = Var.theme Var.Spacing (Rem 0.25) in
  style class_name
    [
      spacing_def;
      text_indent
        (Calc Calc.(length (Var spacing_var) * float (float_of_int n)));
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

(* Property rules for content variable *)
let content_property_rules =
  [ Var.property Var.Content ~syntax:"*" ~inherits:false ~initial:"" ]

let content_none =
  let content_def, content_var = Var.utility Var.Content ~fallback:None None in
  style "content-none" ~property_rules:content_property_rules
    [ content_def; content (Var content_var) ]

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
  (* Auto-quote the text and use Tailwind arbitrary value class name *)
  let quoted = "\"" ^ escape_css_string s ^ "\"" in
  let class_name = Pp.str [ "content-["; quoted; "]" ] in
  let content_def, content_var =
    Var.utility Var.Content ~fallback:(String quoted) (String quoted)
  in
  style class_name ~property_rules:content_property_rules
    [ content_def; content (Var content_var) ]

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
  style "normal-nums"
    [ font_variant_numeric (font_variant_numeric_tokens [ Normal ]) ]

(* Helper to create font-variant-numeric utilities *)
(* Each utility needs to set its own variable and use all 5 in font-variant-numeric *)
let font_variant_numeric_utility class_name var_type value =
  let def, _ = Var.utility var_type value ~fallback:Empty in
  (* Get all 5 variables for the composed value *)
  let _, ordinal_var =
    Var.utility Var.Font_variant_ordinal Empty ~fallback:Empty
  in
  let _, slashed_var =
    Var.utility Var.Font_variant_slashed_zero Empty ~fallback:Empty
  in
  let _, figure_var =
    Var.utility Var.Font_variant_numeric_figure Empty ~fallback:Empty
  in
  let _, spacing_var =
    Var.utility Var.Font_variant_numeric_spacing Empty ~fallback:Empty
  in
  let _, fraction_var =
    Var.utility Var.Font_variant_numeric_fraction Empty ~fallback:Empty
  in

  (* All utilities need @property registration for these variables *)
  let property_rules =
    [
      Var.property Var.Font_variant_ordinal ~syntax:"*" ~inherits:false;
      Var.property Var.Font_variant_slashed_zero ~syntax:"*" ~inherits:false;
      Var.property Var.Font_variant_numeric_figure ~syntax:"*" ~inherits:false;
      Var.property Var.Font_variant_numeric_spacing ~syntax:"*" ~inherits:false;
      Var.property Var.Font_variant_numeric_fraction ~syntax:"*" ~inherits:false;
    ]
  in

  (* Build the font-variant-numeric value using composed with all 5 variables *)
  let composed_value =
    font_variant_numeric_composed ~ordinal:(Var ordinal_var)
      ~slashed_zero:(Var slashed_var) ~numeric_figure:(Var figure_var)
      ~numeric_spacing:(Var spacing_var) ~numeric_fraction:(Var fraction_var) ()
  in

  style class_name ~property_rules [ def; font_variant_numeric composed_value ]

let ordinal =
  font_variant_numeric_utility "ordinal" Var.Font_variant_ordinal Ordinal

let slashed_zero =
  font_variant_numeric_utility "slashed-zero" Var.Font_variant_slashed_zero
    Slashed_zero

let lining_nums =
  font_variant_numeric_utility "lining-nums" Var.Font_variant_numeric_figure
    Lining_nums

let oldstyle_nums =
  font_variant_numeric_utility "oldstyle-nums" Var.Font_variant_numeric_figure
    Oldstyle_nums

let proportional_nums =
  font_variant_numeric_utility "proportional-nums"
    Var.Font_variant_numeric_spacing Proportional_nums

let tabular_nums =
  font_variant_numeric_utility "tabular-nums" Var.Font_variant_numeric_spacing
    Tabular_nums

let diagonal_fractions =
  font_variant_numeric_utility "diagonal-fractions"
    Var.Font_variant_numeric_fraction Diagonal_fractions

let stacked_fractions =
  font_variant_numeric_utility "stacked-fractions"
    Var.Font_variant_numeric_fraction Stacked_fractions

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
