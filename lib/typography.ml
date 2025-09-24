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
let text_xs_var = Var.create Css.Length "text-xs" ~layer:Theme ~order:100

let text_xs_lh_var =
  Var.create Css.Line_height "text-xs--line-height" ~layer:Theme ~order:101

let text_sm_var = Var.create Css.Length "text-sm" ~layer:Theme ~order:102

let text_sm_lh_var =
  Var.create Css.Line_height "text-sm--line-height" ~layer:Theme ~order:103

let text_base_var = Var.create Css.Length "text-base" ~layer:Theme ~order:104

let text_base_lh_var =
  Var.create Css.Line_height "text-base--line-height" ~layer:Theme ~order:105

let text_lg_var = Var.create Css.Length "text-lg" ~layer:Theme ~order:106

let text_lg_lh_var =
  Var.create Css.Line_height "text-lg--line-height" ~layer:Theme ~order:107

let text_xl_var = Var.create Css.Length "text-xl" ~layer:Theme ~order:108

let text_xl_lh_var =
  Var.create Css.Line_height "text-xl--line-height" ~layer:Theme ~order:109

let text_2xl_var = Var.create Css.Length "text-2xl" ~layer:Theme ~order:110

let text_2xl_lh_var =
  Var.create Css.Line_height "text-2xl--line-height" ~layer:Theme ~order:111

let text_3xl_var = Var.create Css.Length "text-3xl" ~layer:Theme ~order:112

let text_3xl_lh_var =
  Var.create Css.Line_height "text-3xl--line-height" ~layer:Theme ~order:113

let text_4xl_var = Var.create Css.Length "text-4xl" ~layer:Theme ~order:114

let text_4xl_lh_var =
  Var.create Css.Line_height "text-4xl--line-height" ~layer:Theme ~order:115

let text_5xl_var = Var.create Css.Length "text-5xl" ~layer:Theme ~order:116

let text_5xl_lh_var =
  Var.create Css.Line_height "text-5xl--line-height" ~layer:Theme ~order:117

let text_6xl_var = Var.create Css.Length "text-6xl" ~layer:Theme ~order:118

let text_6xl_lh_var =
  Var.create Css.Line_height "text-6xl--line-height" ~layer:Theme ~order:119

let text_7xl_var = Var.create Css.Length "text-7xl" ~layer:Theme ~order:120

let text_7xl_lh_var =
  Var.create Css.Line_height "text-7xl--line-height" ~layer:Theme ~order:121

let text_8xl_var = Var.create Css.Length "text-8xl" ~layer:Theme ~order:122

let text_8xl_lh_var =
  Var.create Css.Line_height "text-8xl--line-height" ~layer:Theme ~order:123

let text_9xl_var = Var.create Css.Length "text-9xl" ~layer:Theme ~order:124

let text_9xl_lh_var =
  Var.create Css.Line_height "text-9xl--line-height" ~layer:Theme ~order:125

(* Font weight variables *)
let font_weight_thin_var =
  Var.create Css.Font_weight "font-weight-thin" ~layer:Theme ~order:200

let font_weight_extralight_var =
  Var.create Css.Font_weight "font-weight-extralight" ~layer:Theme ~order:201

let font_weight_light_var =
  Var.create Css.Font_weight "font-weight-light" ~layer:Theme ~order:202

let font_weight_normal_var =
  Var.create Css.Font_weight "font-weight-normal" ~layer:Theme ~order:203

let font_weight_medium_var =
  Var.create Css.Font_weight "font-weight-medium" ~layer:Theme ~order:204

let font_weight_semibold_var =
  Var.create Css.Font_weight "font-weight-semibold" ~layer:Theme ~order:205

let font_weight_bold_var =
  Var.create Css.Font_weight "font-weight-bold" ~layer:Theme ~order:206

let font_weight_extrabold_var =
  Var.create Css.Font_weight "font-weight-extrabold" ~layer:Theme ~order:207

let font_weight_black_var =
  Var.create Css.Font_weight "font-weight-black" ~layer:Theme ~order:208

(* Utility layer variable for font weight tracking *)
let font_weight_var = Var.create Css.Font_weight "tw-font-weight" ~layer:Utility

(* Leading variable for line-height utilities *)
let leading_var = Var.create Css.Line_height "tw-leading" ~layer:Utility

(* Font variant numeric variables for composed value *)
let ordinal_var =
  Var.create Css.Font_variant_numeric_token "tw-font-variant-ordinal"
    ~layer:Utility

let slashed_var =
  Var.create Css.Font_variant_numeric_token "tw-font-variant-slashed-zero"
    ~layer:Utility

let figure_var =
  Var.create Css.Font_variant_numeric_token "tw-font-variant-numeric-figure"
    ~layer:Utility

let spacing_var =
  Var.create Css.Font_variant_numeric_token "tw-font-variant-numeric-spacing"
    ~layer:Utility

let fraction_var =
  Var.create Css.Font_variant_numeric_token "tw-font-variant-numeric-fraction"
    ~layer:Utility

(* Helper to get line height calc value *)
let calc_line_height lh_rem size_rem =
  Calc (Expr (Num lh_rem, Div, Num size_rem))

module Parse = Parse

(** {1 Font Size Utilities} *)

let text_xs =
  let text_xs_decl, text_xs_ref = Var.binding text_xs_var (Rem 0.75) in
  let text_xs_lh_decl, text_xs_lh_ref =
    Var.binding text_xs_lh_var (calc_line_height 1.0 0.75)
  in
  let leading_decl, leading_ref =
    Var.binding leading_var ~fallback:(Css.Var text_xs_lh_ref) (Num 1.5)
  in
  style "text-xs"
    [
      leading_decl;
      text_xs_decl;
      text_xs_lh_decl;
      font_size (Css.Var text_xs_ref);
      line_height (Var leading_ref);
    ]

let text_sm =
  let text_sm_decl, text_sm_ref = Var.binding text_sm_var (Rem 0.875) in
  let text_sm_lh_decl, text_sm_lh_ref =
    Var.binding text_sm_lh_var (calc_line_height 1.25 0.875)
  in
  let leading_decl, leading_ref =
    Var.binding leading_var ~fallback:(Css.Var text_sm_lh_ref) (Num 1.5)
  in
  style "text-sm"
    [
      leading_decl;
      text_sm_decl;
      text_sm_lh_decl;
      font_size (Css.Var text_sm_ref);
      line_height (Var leading_ref);
    ]

let text_base =
  let text_base_decl, text_base_ref = Var.binding text_base_var (Rem 1.0) in
  let text_base_lh_decl, text_base_lh_ref =
    Var.binding text_base_lh_var (calc_line_height 1.5 1.0)
  in
  let leading_decl, leading_ref =
    Var.binding leading_var ~fallback:(Css.Var text_base_lh_ref) (Num 1.5)
  in
  style "text-base"
    [
      leading_decl;
      text_base_decl;
      text_base_lh_decl;
      font_size (Css.Var text_base_ref);
      line_height (Var leading_ref);
    ]

let text_lg =
  let text_lg_decl, text_lg_ref = Var.binding text_lg_var (Rem 1.125) in
  let text_lg_lh_decl, text_lg_lh_ref =
    Var.binding text_lg_lh_var (calc_line_height 1.75 1.125)
  in
  let leading_decl, leading_ref =
    Var.binding leading_var ~fallback:(Css.Var text_lg_lh_ref) (Num 1.5)
  in
  style "text-lg"
    [
      leading_decl;
      text_lg_decl;
      text_lg_lh_decl;
      font_size (Css.Var text_lg_ref);
      line_height (Var leading_ref);
    ]

let text_xl =
  let text_xl_decl, text_xl_ref = Var.binding text_xl_var (Rem 1.25) in
  let text_xl_lh_decl, text_xl_lh_ref =
    Var.binding text_xl_lh_var (calc_line_height 1.75 1.25)
  in
  let leading_decl, leading_ref =
    Var.binding leading_var ~fallback:(Css.Var text_xl_lh_ref) (Num 1.5)
  in
  style "text-xl"
    [
      leading_decl;
      text_xl_decl;
      text_xl_lh_decl;
      font_size (Css.Var text_xl_ref);
      line_height (Var leading_ref);
    ]

let text_2xl =
  let text_2xl_decl, text_2xl_ref = Var.binding text_2xl_var (Rem 1.5) in
  let text_2xl_lh_decl, text_2xl_lh_ref =
    Var.binding text_2xl_lh_var (calc_line_height 2.0 1.5)
  in
  let leading_decl, leading_ref =
    Var.binding leading_var ~fallback:(Css.Var text_2xl_lh_ref) (Num 1.5)
  in
  style "text-2xl"
    [
      leading_decl;
      text_2xl_decl;
      text_2xl_lh_decl;
      font_size (Css.Var text_2xl_ref);
      line_height (Var leading_ref);
    ]

let text_3xl =
  let text_3xl_decl, text_3xl_ref = Var.binding text_3xl_var (Rem 1.875) in
  let text_3xl_lh_decl, text_3xl_lh_ref =
    Var.binding text_3xl_lh_var (calc_line_height 2.25 1.875)
  in
  let leading_decl, leading_ref =
    Var.binding leading_var ~fallback:(Css.Var text_3xl_lh_ref) (Num 1.5)
  in
  style "text-3xl"
    [
      leading_decl;
      text_3xl_decl;
      text_3xl_lh_decl;
      font_size (Css.Var text_3xl_ref);
      line_height (Var leading_ref);
    ]

let text_4xl =
  let text_4xl_decl, text_4xl_ref = Var.binding text_4xl_var (Rem 2.25) in
  let text_4xl_lh_decl, text_4xl_lh_ref =
    Var.binding text_4xl_lh_var (calc_line_height 2.5 2.25)
  in
  let leading_decl, leading_ref =
    Var.binding leading_var ~fallback:(Css.Var text_4xl_lh_ref) (Num 1.5)
  in
  style "text-4xl"
    [
      leading_decl;
      text_4xl_decl;
      text_4xl_lh_decl;
      font_size (Css.Var text_4xl_ref);
      line_height (Var leading_ref);
    ]

let text_5xl =
  let text_5xl_decl, text_5xl_ref = Var.binding text_5xl_var (Rem 3.0) in
  let text_5xl_lh_decl, text_5xl_lh_ref =
    Var.binding text_5xl_lh_var (Num 1.0)
  in
  let leading_decl, leading_ref =
    Var.binding leading_var ~fallback:(Css.Var text_5xl_lh_ref) (Num 1.5)
  in
  style "text-5xl"
    [
      leading_decl;
      text_5xl_decl;
      text_5xl_lh_decl;
      font_size (Css.Var text_5xl_ref);
      line_height (Var leading_ref);
    ]

let text_6xl =
  let text_6xl_decl, text_6xl_ref = Var.binding text_6xl_var (Rem 3.75) in
  let text_6xl_lh_decl, text_6xl_lh_ref =
    Var.binding text_6xl_lh_var (Num 1.0)
  in
  let leading_decl, leading_ref =
    Var.binding leading_var ~fallback:(Css.Var text_6xl_lh_ref) (Num 1.5)
  in
  style "text-6xl"
    [
      leading_decl;
      text_6xl_decl;
      text_6xl_lh_decl;
      font_size (Css.Var text_6xl_ref);
      line_height (Var leading_ref);
    ]

let text_7xl =
  let text_7xl_decl, text_7xl_ref = Var.binding text_7xl_var (Rem 4.5) in
  let text_7xl_lh_decl, text_7xl_lh_ref =
    Var.binding text_7xl_lh_var (Num 1.0)
  in
  let leading_decl, leading_ref =
    Var.binding leading_var ~fallback:(Css.Var text_7xl_lh_ref) (Num 1.5)
  in
  style "text-7xl"
    [
      leading_decl;
      text_7xl_decl;
      text_7xl_lh_decl;
      font_size (Css.Var text_7xl_ref);
      line_height (Var leading_ref);
    ]

let text_8xl =
  let text_8xl_decl, text_8xl_ref = Var.binding text_8xl_var (Rem 6.0) in
  let text_8xl_lh_decl, text_8xl_lh_ref =
    Var.binding text_8xl_lh_var (Num 1.0)
  in
  let leading_decl, leading_ref =
    Var.binding leading_var ~fallback:(Css.Var text_8xl_lh_ref) (Num 1.5)
  in
  style "text-8xl"
    [
      leading_decl;
      text_8xl_decl;
      text_8xl_lh_decl;
      font_size (Css.Var text_8xl_ref);
      line_height (Var leading_ref);
    ]

let text_9xl =
  let text_9xl_decl, text_9xl_ref = Var.binding text_9xl_var (Rem 8.0) in
  let text_9xl_lh_decl, text_9xl_lh_ref =
    Var.binding text_9xl_lh_var (Num 1.0)
  in
  let leading_decl, leading_ref =
    Var.binding leading_var ~fallback:(Css.Var text_9xl_lh_ref) (Num 1.5)
  in
  style "text-9xl"
    [
      leading_decl;
      text_9xl_decl;
      text_9xl_lh_decl;
      font_size (Css.Var text_9xl_ref);
      line_height (Var leading_ref);
    ]

(* Font weight utilities using the new API *)
let font_thin =
  let thin_decl, thin_ref = Var.binding font_weight_thin_var (Weight 100) in
  let weight_decl, _ = Var.binding font_weight_var (Css.Var thin_ref) in
  style "font-thin" [ thin_decl; weight_decl; font_weight (Css.Var thin_ref) ]

let font_extralight =
  let extralight_decl, extralight_ref =
    Var.binding font_weight_extralight_var (Weight 200)
  in
  let weight_decl, _ = Var.binding font_weight_var (Css.Var extralight_ref) in
  style "font-extralight"
    [ extralight_decl; weight_decl; font_weight (Css.Var extralight_ref) ]

let font_light =
  let light_decl, light_ref = Var.binding font_weight_light_var (Weight 300) in
  let weight_decl, _ = Var.binding font_weight_var (Css.Var light_ref) in
  style "font-light"
    [ light_decl; weight_decl; font_weight (Css.Var light_ref) ]

let font_normal =
  let normal_decl, normal_ref =
    Var.binding font_weight_normal_var (Weight 400)
  in
  let weight_decl, _ = Var.binding font_weight_var (Css.Var normal_ref) in
  style "font-normal"
    [ normal_decl; weight_decl; font_weight (Css.Var normal_ref) ]

let font_medium =
  let medium_decl, medium_ref =
    Var.binding font_weight_medium_var (Weight 500)
  in
  let weight_decl, _ = Var.binding font_weight_var (Css.Var medium_ref) in
  style "font-medium"
    [ medium_decl; weight_decl; font_weight (Css.Var medium_ref) ]

let font_semibold =
  let semibold_decl, semibold_ref =
    Var.binding font_weight_semibold_var (Weight 600)
  in
  let weight_decl, _ = Var.binding font_weight_var (Css.Var semibold_ref) in
  style "font-semibold"
    [ semibold_decl; weight_decl; font_weight (Css.Var semibold_ref) ]

let font_bold =
  let bold_decl, bold_ref = Var.binding font_weight_bold_var (Weight 700) in
  let weight_decl, _ = Var.binding font_weight_var (Css.Var bold_ref) in
  style "font-bold" [ bold_decl; weight_decl; font_weight (Css.Var bold_ref) ]

let font_extrabold =
  let extrabold_decl, extrabold_ref =
    Var.binding font_weight_extrabold_var (Weight 800)
  in
  let weight_decl, _ = Var.binding font_weight_var (Css.Var extrabold_ref) in
  style "font-extrabold"
    [ extrabold_decl; weight_decl; font_weight (Css.Var extrabold_ref) ]

let font_black =
  let black_decl, black_ref = Var.binding font_weight_black_var (Weight 900) in
  let weight_decl, _ = Var.binding font_weight_var (Css.Var black_ref) in
  style "font-black"
    [ black_decl; weight_decl; font_weight (Css.Var black_ref) ]

(** {1 Font Family Utilities} *)

(* Font family theme variables *)
let font_sans_var = Var.create Css.Font_family "font-sans" ~layer:Theme ~order:0

let font_serif_var =
  Var.create Css.Font_family "font-serif" ~layer:Theme ~order:1

let font_mono_var = Var.create Css.Font_family "font-mono" ~layer:Theme ~order:2

(* Default font family variables that reference the base font variables *)
let default_font_family_var =
  Var.create Css.Font_family "default-font-family" ~layer:Theme ~order:5

let default_mono_font_family_var =
  Var.create Css.Font_family "default-mono-font-family" ~layer:Theme ~order:6

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
    let default_color =
      Color.to_css color (if Color.is_base_color color then 500 else shade)
    in
    let color_var =
      let name =
        let base = Color.pp color in
        if Color.is_base_color color then "color-" ^ base
        else "color-" ^ base ^ "-" ^ string_of_int shade
      in
      Var.create Css.Color name ~layer:Theme ~order:3
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
  let spacing_var = Var.create Css.Length "spacing" ~layer:Theme ~order:4 in
  let spacing_decl, spacing_ref = Var.binding spacing_var (Rem 0.25) in
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

let content_var = Var.create Content "tw-content" ~layer:Utility

let content_none =
  let content_decl, content_ref = Var.binding content_var None in
  style "content-none" [ content_decl; content (Css.Var content_ref) ]

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
  let class_name = String.concat "" [ "content-["; quoted; "]" ] in
  let content_var = Var.create Content "tw-content" ~layer:Utility in
  let content_decl, content_ref = Var.binding content_var (String quoted) in
  style class_name [ content_decl; content (Css.Var content_ref) ]

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

(* Helper to create font-variant-numeric utilities *)
(* Each utility sets one specific variable and uses all 5 in composed value *)
type font_variant_vars = {
  ordinal : Css.font_variant_numeric_token Css.var;
  slashed : Css.font_variant_numeric_token Css.var;
  figure : Css.font_variant_numeric_token Css.var;
  spacing : Css.font_variant_numeric_token Css.var;
  fraction : Css.font_variant_numeric_token Css.var;
}

let optional_binding :
    bool -> 'a Var.t -> 'a -> 'a Css.var -> Css.declaration option * 'a Css.var
    =
 fun condition var_ref var_value fallback_var_ref ->
  if condition then
    let decl, var_ref = Var.binding var_ref var_value in
    (Some decl, var_ref)
  else (None, fallback_var_ref)

let font_numeric :
    ?base:Css.declaration list ->
    [ `Ordinal | `Slashed | `Figure | `Spacing | `Fraction ] ->
    Css.font_variant_numeric_token ->
    string ->
    font_variant_vars ->
    Core.t =
 fun ?(base = []) var_to_set value class_name vars ->
  let ordinal_decl_opt, ordinal_v =
    optional_binding (var_to_set = `Ordinal) ordinal_var value vars.ordinal
  in
  let slashed_decl_opt, slashed_v =
    optional_binding (var_to_set = `Slashed) slashed_var value vars.slashed
  in
  let figure_decl_opt, figure_v =
    optional_binding (var_to_set = `Figure) figure_var value vars.figure
  in
  let spacing_decl_opt, spacing_v =
    optional_binding (var_to_set = `Spacing) spacing_var value vars.spacing
  in
  let fraction_decl_opt, fraction_v =
    optional_binding (var_to_set = `Fraction) fraction_var value vars.fraction
  in
  let composed_value =
    Composed
      {
        ordinal = Some (Css.Var ordinal_v);
        slashed_zero = Some (Css.Var slashed_v);
        numeric_figure = Some (Css.Var figure_v);
        numeric_spacing = Some (Css.Var spacing_v);
        numeric_fraction = Some (Css.Var fraction_v);
      }
  in
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
  let decls =
    List.filter_map
      (fun x -> x)
      [
        ordinal_decl_opt;
        slashed_decl_opt;
        figure_decl_opt;
        spacing_decl_opt;
        fraction_decl_opt;
      ]
  in
  style class_name
    ~property_rules:(Css.concat property_rules)
    (base @ decls @ [ font_variant_numeric composed_value ])

let font_variant_numeric_utility var_to_set value class_name =
  let ordinal_decl, ordinal_ref = Var.binding ordinal_var Normal in
  let slashed_decl, slashed_ref = Var.binding slashed_var Normal in
  let figure_decl, figure_ref = Var.binding figure_var Normal in
  let spacing_decl, spacing_ref = Var.binding spacing_var Normal in
  let fraction_decl, fraction_ref = Var.binding fraction_var Normal in
  let base =
    [ ordinal_decl; slashed_decl; figure_decl; spacing_decl; fraction_decl ]
  in
  let vars =
    {
      ordinal = ordinal_ref;
      slashed = slashed_ref;
      figure = figure_ref;
      spacing = spacing_ref;
      fraction = fraction_ref;
    }
  in
  font_numeric ~base var_to_set value class_name vars

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
