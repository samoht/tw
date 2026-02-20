(** Typography utilities for text and font styling. *)

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

(* Theme variables for named tracking values *)
let tracking_tighter_var = Var.theme Css.Length "tracking-tighter" ~order:(6, 39)
let tracking_tight_var = Var.theme Css.Length "tracking-tight" ~order:(6, 40)
let tracking_normal_var = Var.theme Css.Length "tracking-normal" ~order:(6, 41)
let tracking_wide_var = Var.theme Css.Length "tracking-wide" ~order:(6, 42)
let tracking_wider_var = Var.theme Css.Length "tracking-wider" ~order:(6, 43)
let tracking_widest_var = Var.theme Css.Length "tracking-widest" ~order:(6, 44)

(* Theme variables for named leading values *)
let leading_none_var = Var.theme Css.Line_height "leading-none" ~order:(6, 47)
let leading_tight_var = Var.theme Css.Line_height "leading-tight" ~order:(6, 48)
let leading_snug_var = Var.theme Css.Line_height "leading-snug" ~order:(6, 49)

let leading_normal_var =
  Var.theme Css.Line_height "leading-normal" ~order:(6, 50)

let leading_relaxed_var =
  Var.theme Css.Line_height "leading-relaxed" ~order:(6, 51)

let leading_loose_var = Var.theme Css.Line_height "leading-loose" ~order:(6, 52)

let leading_var =
  (* Leading appears in @layer properties after transforms (position 11) *)
  Var.channel ~needs_property:true ~property_order:11 ~family:`Leading
    Css.Line_height "tw-leading"

let font_weight_var =
  (* Font-weight appears in @layer properties after leading (position 12) *)
  Var.channel ~needs_property:true ~property_order:12 ~family:`Font_weight
    Css.Font_weight "tw-font-weight"

let tracking_var =
  (* Tracking doesn't appear in @layer properties in standard Tailwind - only
     referenced *)
  Var.channel ~needs_property:true ~property_order:101 ~family:`Tracking
    Css.Length "tw-tracking"

let ordinal_var =
  Var.channel ~needs_property:true ~property_order:102
    Css.Font_variant_numeric_token "tw-ordinal"

let slashed_var =
  Var.channel ~needs_property:true ~property_order:103
    Css.Font_variant_numeric_token "tw-slashed-zero"

let figure_var =
  Var.channel ~needs_property:true ~property_order:104
    Css.Font_variant_numeric_token "tw-numeric-figure"

let spacing_var =
  Var.channel ~needs_property:true ~property_order:105
    Css.Font_variant_numeric_token "tw-numeric-spacing"

let fraction_var =
  Var.channel ~needs_property:true ~property_order:106
    Css.Font_variant_numeric_token "tw-numeric-fraction"

(* Helper to get line height calc value *)
let calc_line_height lh_rem size_rem =
  Css.Calc (Css.Expr (Css.Num lh_rem, Css.Div, Css.Num size_rem))

(* Theme record for line height variables *)
type line_height_theme = { leading : Css.declaration * Css.line_height Css.var }

(* Default line height theme with empty fallback for Tailwind's var(--name,)
   pattern *)
let default_line_height_theme : line_height_theme =
  let leading_decl, leading_ref =
    Var.binding leading_var ~fallback:Css.Empty (Num 1.5)
  in
  { leading = (leading_decl, leading_ref) }

(* Content variable *)
let content_var =
  Var.property_default Content ~initial:(String "") ~universal:true
    ~property_order:100 "tw-content"

(* Default theme for font variant numeric variables *)
type font_variant_theme = {
  ordinal : Css.declaration * Css.font_variant_numeric_token Css.var;
  slashed : Css.declaration * Css.font_variant_numeric_token Css.var;
  figure : Css.declaration * Css.font_variant_numeric_token Css.var;
  spacing : Css.declaration * Css.font_variant_numeric_token Css.var;
  fraction : Css.declaration * Css.font_variant_numeric_token Css.var;
}

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

(** Early typography handler - comes before color utilities (priority 22) *)
module Typography_early = struct
  open Style
  open Css

  type t =
    | (* Text sizes *)
      Text_xs
    | Text_sm
    | Text_base
    | Text_lg
    | Text_xl
    | Text_2xl
    | Text_3xl
    | Text_4xl
    | Text_5xl
    | Text_6xl
    | Text_7xl
    | Text_8xl
    | Text_9xl
    | (* Font weights *)
      Font_thin
    | Font_extralight
    | Font_light
    | Font_normal
    | Font_medium
    | Font_semibold
    | Font_bold
    | Font_extrabold
    | Font_black
    | (* Font families *)
      Font_sans
    | Font_serif
    | Font_mono
    | (* Font styles *)
      Italic
    | Not_italic
    | (* Text alignment *)
      Text_left
    | Text_center
    | Text_right
    | Text_justify
    | Text_start
    | Text_end
    | (* Leading *)
      Leading_none
    | Leading_tight
    | Leading_snug
    | Leading_normal
    | Leading_relaxed
    | Leading_loose
    | Leading of int

  type Utility.base += Self of t

  let name = "typography_early"
  let priority = 22
  let ( >|= ) = Parse.( >|= )
  let err_not_utility = Error (`Msg "Not an early typography utility")

  let of_class class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "text"; "xs" ] -> Ok Text_xs
    | [ "text"; "sm" ] -> Ok Text_sm
    | [ "text"; "base" ] -> Ok Text_base
    | [ "text"; "lg" ] -> Ok Text_lg
    | [ "text"; "xl" ] -> Ok Text_xl
    | [ "text"; "2xl" ] -> Ok Text_2xl
    | [ "text"; "3xl" ] -> Ok Text_3xl
    | [ "text"; "4xl" ] -> Ok Text_4xl
    | [ "text"; "5xl" ] -> Ok Text_5xl
    | [ "text"; "6xl" ] -> Ok Text_6xl
    | [ "text"; "7xl" ] -> Ok Text_7xl
    | [ "text"; "8xl" ] -> Ok Text_8xl
    | [ "text"; "9xl" ] -> Ok Text_9xl
    | [ "font"; "thin" ] -> Ok Font_thin
    | [ "font"; "extralight" ] -> Ok Font_extralight
    | [ "font"; "light" ] -> Ok Font_light
    | [ "font"; "normal" ] -> Ok Font_normal
    | [ "font"; "medium" ] -> Ok Font_medium
    | [ "font"; "semibold" ] -> Ok Font_semibold
    | [ "font"; "bold" ] -> Ok Font_bold
    | [ "font"; "extrabold" ] -> Ok Font_extrabold
    | [ "font"; "black" ] -> Ok Font_black
    | [ "font"; "sans" ] -> Ok Font_sans
    | [ "font"; "serif" ] -> Ok Font_serif
    | [ "font"; "mono" ] -> Ok Font_mono
    | [ "italic" ] -> Ok Italic
    | [ "not"; "italic" ] -> Ok Not_italic
    | [ "text"; "left" ] -> Ok Text_left
    | [ "text"; "center" ] -> Ok Text_center
    | [ "text"; "right" ] -> Ok Text_right
    | [ "text"; "justify" ] -> Ok Text_justify
    | [ "text"; "start" ] -> Ok Text_start
    | [ "text"; "end" ] -> Ok Text_end
    | [ "leading"; "none" ] -> Ok Leading_none
    | [ "leading"; "tight" ] -> Ok Leading_tight
    | [ "leading"; "snug" ] -> Ok Leading_snug
    | [ "leading"; "normal" ] -> Ok Leading_normal
    | [ "leading"; "relaxed" ] -> Ok Leading_relaxed
    | [ "leading"; "loose" ] -> Ok Leading_loose
    | [ "leading"; n ] ->
        Parse.int_bounded ~name:"leading" ~min:3 ~max:10 n >|= fun i ->
        Leading i
    | _ -> err_not_utility

  let to_class = function
    | Text_xs -> "text-xs"
    | Text_sm -> "text-sm"
    | Text_base -> "text-base"
    | Text_lg -> "text-lg"
    | Text_xl -> "text-xl"
    | Text_2xl -> "text-2xl"
    | Text_3xl -> "text-3xl"
    | Text_4xl -> "text-4xl"
    | Text_5xl -> "text-5xl"
    | Text_6xl -> "text-6xl"
    | Text_7xl -> "text-7xl"
    | Text_8xl -> "text-8xl"
    | Text_9xl -> "text-9xl"
    | Font_thin -> "font-thin"
    | Font_extralight -> "font-extralight"
    | Font_light -> "font-light"
    | Font_normal -> "font-normal"
    | Font_medium -> "font-medium"
    | Font_semibold -> "font-semibold"
    | Font_bold -> "font-bold"
    | Font_extrabold -> "font-extrabold"
    | Font_black -> "font-black"
    | Font_sans -> "font-sans"
    | Font_serif -> "font-serif"
    | Font_mono -> "font-mono"
    | Italic -> "italic"
    | Not_italic -> "not-italic"
    | Text_left -> "text-left"
    | Text_center -> "text-center"
    | Text_right -> "text-right"
    | Text_justify -> "text-justify"
    | Text_start -> "text-start"
    | Text_end -> "text-end"
    | Leading_none -> "leading-none"
    | Leading_tight -> "leading-tight"
    | Leading_snug -> "leading-snug"
    | Leading_normal -> "leading-normal"
    | Leading_relaxed -> "leading-relaxed"
    | Leading_loose -> "leading-loose"
    | Leading n -> "leading-" ^ string_of_int n

  (** {1 Ordering Support} *)

  let suborder = function
    (* Text align comes first - alphabetical order *)
    | Text_center -> 1001
    | Text_end -> 1002
    | Text_justify -> 1003
    | Text_left -> 1004
    | Text_right -> 1005
    | Text_start -> 1006
    (* Font family - comes between text-align and text-size *)
    | Font_sans -> 1501
    | Font_serif -> 1502
    | Font_mono -> 1503
    (* Font sizes come second - alphabetical order *)
    | Text_2xl -> 2001
    | Text_3xl -> 2002
    | Text_4xl -> 2003
    | Text_5xl -> 2004
    | Text_6xl -> 2005
    | Text_7xl -> 2006
    | Text_8xl -> 2007
    | Text_9xl -> 2008
    | Text_base -> 2009
    | Text_lg -> 2010
    | Text_sm -> 2011
    | Text_xl -> 2012
    | Text_xs -> 2013
    (* Leading comes third — Tailwind uses alphabetical order: loose, none,
       normal, relaxed, snug, tight *)
    | Leading_loose -> 3001
    | Leading_none -> 3002
    | Leading_normal -> 3003
    | Leading_relaxed -> 3004
    | Leading_snug -> 3005
    | Leading_tight -> 3006
    | Leading n -> 3100 + n
    (* Font weight comes fourth - alphabetical order *)
    | Font_black -> 4100
    | Font_bold -> 4200
    | Font_extrabold -> 4300
    | Font_extralight -> 4400
    | Font_light -> 4500
    | Font_medium -> 4600
    | Font_normal -> 4700
    | Font_semibold -> 4800
    | Font_thin -> 4900
    (* Italic *)
    | Italic -> 7001
    | Not_italic -> 7002

  (* Text utilities use theme record for line height variable reference *)
  let text_size_utility (size_var : Css.length Var.theme)
      (lh_var : Css.line_height Var.theme) size_rem lh_value =
    let size_decl, size_ref = Var.binding size_var (Rem size_rem) in
    let lh_decl, lh_ref = Var.binding lh_var lh_value in
    (* Use shared theme record - no declaration, just reference *)
    let theme = default_line_height_theme in
    let leading_ref = snd theme.leading in
    let leading_with_fallback =
      Css.with_fallback leading_ref (Css.Var lh_ref)
    in
    style
      [
        size_decl;
        lh_decl;
        font_size (Css.Var size_ref);
        line_height (Var leading_with_fallback);
      ]

  let text_xs =
    text_size_utility text_xs_var text_xs_lh_var 0.75
      (calc_line_height 1.0 0.75)

  let text_sm =
    text_size_utility text_sm_var text_sm_lh_var 0.875
      (calc_line_height 1.25 0.875)

  let text_base =
    text_size_utility text_base_var text_base_lh_var 1.0
      (calc_line_height 1.5 1.0)

  let text_lg =
    text_size_utility text_lg_var text_lg_lh_var 1.125
      (calc_line_height 1.75 1.125)

  let text_xl =
    text_size_utility text_xl_var text_xl_lh_var 1.25
      (calc_line_height 1.75 1.25)

  let text_2xl =
    text_size_utility text_2xl_var text_2xl_lh_var 1.5
      (calc_line_height 2.0 1.5)

  let text_3xl =
    text_size_utility text_3xl_var text_3xl_lh_var 1.875
      (calc_line_height 2.25 1.875)

  let text_4xl =
    text_size_utility text_4xl_var text_4xl_lh_var 2.25
      (calc_line_height 2.5 2.25)

  let text_5xl = text_size_utility text_5xl_var text_5xl_lh_var 3.0 (Num 1.0)
  let text_6xl = text_size_utility text_6xl_var text_6xl_lh_var 3.75 (Num 1.0)
  let text_7xl = text_size_utility text_7xl_var text_7xl_lh_var 4.5 (Num 1.0)
  let text_8xl = text_size_utility text_8xl_var text_8xl_lh_var 6.0 (Num 1.0)
  let text_9xl = text_size_utility text_9xl_var text_9xl_lh_var 8.0 (Num 1.0)

  (* Font weight utilities set --tw-font-weight for animation but use theme var
     directly *)
  let font_weight_utility weight_var weight_value =
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
    style ~property_rules
      [
        weight_theme_decl;
        weight_util_decl;
        font_weight (Css.Var weight_theme_ref);
      ]

  let font_thin = font_weight_utility font_weight_thin_var (Weight 100)

  let font_extralight =
    font_weight_utility font_weight_extralight_var (Weight 200)

  let font_light = font_weight_utility font_weight_light_var (Weight 300)
  let font_normal = font_weight_utility font_weight_normal_var (Weight 400)
  let font_medium = font_weight_utility font_weight_medium_var (Weight 500)
  let font_semibold = font_weight_utility font_weight_semibold_var (Weight 600)
  let font_bold = font_weight_utility font_weight_bold_var (Weight 700)

  let font_extrabold =
    font_weight_utility font_weight_extrabold_var (Weight 800)

  let font_black = font_weight_utility font_weight_black_var (Weight 900)

  let font_serif =
    let serif_decl, serif_ref =
      Var.binding font_serif_var
        (List [ Ui_serif; Georgia; Cambria; Times_new_roman; Times; Serif ])
    in
    style [ serif_decl; font_family (Css.Var serif_ref) ]

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
    style [ mono_decl; font_family (Css.Var mono_ref) ]

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
    style [ sans_decl; font_family (Css.Var sans_ref) ]

  let italic = style [ font_style Italic ]
  let not_italic = style [ font_style Normal ]
  let text_left = style [ text_align Left ]
  let text_center = style [ text_align Center ]
  let text_right = style [ text_align Right ]
  let text_justify = style [ text_align Justify ]
  let text_start = style [ text_align Start ]
  let text_end = style [ text_align End ]

  let leading_with_theme_var theme_var default_value =
    let theme_decl, theme_ref = Var.binding theme_var default_value in
    let channel_decl, _ = Var.binding leading_var (Css.Var theme_ref) in
    let property_rules =
      Var.property_rule leading_var |> Option.to_list |> Css.concat
    in
    style ~property_rules
      [ theme_decl; channel_decl; line_height (Css.Var theme_ref) ]

  let leading_none = leading_with_theme_var leading_none_var (Num 1.0)
  let leading_tight = leading_with_theme_var leading_tight_var (Num 1.25)
  let leading_snug = leading_with_theme_var leading_snug_var (Num 1.375)
  let leading_normal = leading_with_theme_var leading_normal_var (Num 1.5)
  let leading_relaxed = leading_with_theme_var leading_relaxed_var (Num 1.625)
  let leading_loose = leading_with_theme_var leading_loose_var (Num 2.0)

  let leading n =
    let lh_value : line_height = Rem (float_of_int n *. 0.25) in
    let theme_var =
      Var.theme Css.Line_height ("leading-" ^ string_of_int n) ~order:(6, 46)
    in
    leading_with_theme_var theme_var lh_value

  let to_style = function
    | Text_xs -> text_xs
    | Text_sm -> text_sm
    | Text_base -> text_base
    | Text_lg -> text_lg
    | Text_xl -> text_xl
    | Text_2xl -> text_2xl
    | Text_3xl -> text_3xl
    | Text_4xl -> text_4xl
    | Text_5xl -> text_5xl
    | Text_6xl -> text_6xl
    | Text_7xl -> text_7xl
    | Text_8xl -> text_8xl
    | Text_9xl -> text_9xl
    | Font_thin -> font_thin
    | Font_extralight -> font_extralight
    | Font_light -> font_light
    | Font_normal -> font_normal
    | Font_medium -> font_medium
    | Font_semibold -> font_semibold
    | Font_bold -> font_bold
    | Font_extrabold -> font_extrabold
    | Font_black -> font_black
    | Font_sans -> font_sans
    | Font_serif -> font_serif
    | Font_mono -> font_mono
    | Italic -> italic
    | Not_italic -> not_italic
    | Text_left -> text_left
    | Text_center -> text_center
    | Text_right -> text_right
    | Text_justify -> text_justify
    | Text_start -> text_start
    | Text_end -> text_end
    | Leading_none -> leading_none
    | Leading_tight -> leading_tight
    | Leading_snug -> leading_snug
    | Leading_normal -> leading_normal
    | Leading_relaxed -> leading_relaxed
    | Leading_loose -> leading_loose
    | Leading n -> leading n
end

(** Late typography handler - comes after color utilities (priority 24) *)
module Typography_late = struct
  open Style
  open Css

  type t =
    | (* Decoration color *)
      Decoration_color of Color.color * int option
    | Decoration_color_opacity of Color.color * int * Color.opacity_modifier
    | (* Text decoration lines *)
      Underline
    | Overline
    | Line_through
    | No_underline
    | (* Decoration styles *)
      Decoration_solid
    | Decoration_double
    | Decoration_dotted
    | Decoration_dashed
    | Decoration_wavy
    | Decoration_thickness of int
    | Decoration_from_font
    | (* Tracking *)
      Tracking_tighter
    | Tracking_tight
    | Tracking_normal
    | Tracking_wide
    | Tracking_wider
    | Tracking_widest
    | (* Text transform *)
      Uppercase
    | Lowercase
    | Capitalize
    | Normal_case
    | (* Whitespace *)
      Whitespace_normal
    | Whitespace_nowrap
    | Whitespace_pre
    | Whitespace_pre_line
    | Whitespace_pre_wrap
    | Whitespace_break_spaces
    | (* Vertical align *)
      Align_baseline
    | Align_top
    | Align_middle
    | Align_bottom
    | Align_text_top
    | Align_text_bottom
    | Align_sub
    | Align_super
    | (* List utilities *)
      List_none
    | List_disc
    | List_decimal
    | List_inside
    | List_outside
    | List_image_none
    | List_image_url of string
    | (* Underline offset *)
      Underline_offset_auto
    | Underline_offset_0
    | Underline_offset_1
    | Underline_offset_2
    | Underline_offset_4
    | Underline_offset_8
    | Underline_offset_px of float
    | Underline_offset_neg_px of float
    | (* Antialiased *)
      Antialiased
    | Subpixel_antialiased
    | (* Text overflow *)
      Text_ellipsis
    | Text_clip
    | Truncate
    | (* Text wrap *)
      Text_wrap
    | Text_nowrap
    | Text_balance
    | Text_pretty
    | (* Break utilities *)
      Break_normal
    | Break_words
    | Break_all
    | Break_keep
    | (* Overflow wrap *)
      Overflow_wrap_normal
    | Overflow_wrap_anywhere
    | Overflow_wrap_break_word
    | (* Hyphens *)
      Hyphens_none
    | Hyphens_manual
    | Hyphens_auto
    | (* Font stretch *)
      Font_stretch_ultra_condensed
    | Font_stretch_extra_condensed
    | Font_stretch_condensed
    | Font_stretch_semi_condensed
    | Font_stretch_normal
    | Font_stretch_semi_expanded
    | Font_stretch_expanded
    | Font_stretch_extra_expanded
    | Font_stretch_ultra_expanded
    | Font_stretch_percent of int
    | (* Numeric variants *)
      Normal_nums
    | Ordinal
    | Slashed_zero
    | Lining_nums
    | Oldstyle_nums
    | Proportional_nums
    | Tabular_nums
    | Diagonal_fractions
    | Stacked_fractions
    | (* Indent and line clamp *)
      Indent of int
    | Line_clamp of int
    | Line_clamp_none
    | (* Content *)
      Content_none
    | Content of string
    | Content_named of string

  type Utility.base += Self of t

  let name = "typography_late"
  let priority = 24
  let ( >|= ) = Parse.( >|= )
  let err_not_utility = Error (`Msg "Not a late typography utility")

  let of_class class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "underline" ] -> Ok Underline
    | [ "overline" ] -> Ok Overline
    | [ "line"; "through" ] -> Ok Line_through
    | [ "no"; "underline" ] -> Ok No_underline
    | [ "decoration"; "solid" ] -> Ok Decoration_solid
    | [ "decoration"; "double" ] -> Ok Decoration_double
    | [ "decoration"; "dotted" ] -> Ok Decoration_dotted
    | [ "decoration"; "dashed" ] -> Ok Decoration_dashed
    | [ "decoration"; "wavy" ] -> Ok Decoration_wavy
    | [ "decoration"; "from"; "font" ] -> Ok Decoration_from_font
    | [ "decoration"; n ] -> (
        (* Try parsing as number first (decoration thickness) *)
        match
          Parse.int_bounded ~name:"decoration-thickness" ~min:0 ~max:8 n
        with
        | Ok i -> Ok (Decoration_thickness i)
        | Error _ -> (
            (* If not a number, try parsing as color *)
            match Color.of_string n with
            | Ok c -> Ok (Decoration_color (c, None))
            | Error _ -> err_not_utility))
    | [ "decoration"; color; shade ] -> (
        (* Check for opacity modifier in shade (e.g., "500/50" or
           "500/[0.5]") *)
        let shade_str, opacity = Color.parse_opacity_modifier shade in
        match (Color.of_string color, Parse.int_any shade_str) with
        | Ok c, Ok s -> (
            match opacity with
            | Color.No_opacity -> Ok (Decoration_color (c, Some s))
            | _ -> Ok (Decoration_color_opacity (c, s, opacity)))
        | _ -> err_not_utility)
    | [ "tracking"; "tighter" ] -> Ok Tracking_tighter
    | [ "tracking"; "tight" ] -> Ok Tracking_tight
    | [ "tracking"; "normal" ] -> Ok Tracking_normal
    | [ "tracking"; "wide" ] -> Ok Tracking_wide
    | [ "tracking"; "wider" ] -> Ok Tracking_wider
    | [ "tracking"; "widest" ] -> Ok Tracking_widest
    | [ "uppercase" ] -> Ok Uppercase
    | [ "lowercase" ] -> Ok Lowercase
    | [ "capitalize" ] -> Ok Capitalize
    | [ "normal"; "case" ] -> Ok Normal_case
    | [ "whitespace"; "normal" ] -> Ok Whitespace_normal
    | [ "whitespace"; "nowrap" ] -> Ok Whitespace_nowrap
    | [ "whitespace"; "pre" ] -> Ok Whitespace_pre
    | [ "whitespace"; "pre"; "line" ] -> Ok Whitespace_pre_line
    | [ "whitespace"; "pre"; "wrap" ] -> Ok Whitespace_pre_wrap
    | [ "whitespace"; "break"; "spaces" ] -> Ok Whitespace_break_spaces
    | [ "align"; "baseline" ] -> Ok Align_baseline
    | [ "align"; "top" ] -> Ok Align_top
    | [ "align"; "middle" ] -> Ok Align_middle
    | [ "align"; "bottom" ] -> Ok Align_bottom
    | [ "align"; "text"; "top" ] -> Ok Align_text_top
    | [ "align"; "text"; "bottom" ] -> Ok Align_text_bottom
    | [ "align"; "sub" ] -> Ok Align_sub
    | [ "align"; "super" ] -> Ok Align_super
    | [ "list"; "none" ] -> Ok List_none
    | [ "list"; "disc" ] -> Ok List_disc
    | [ "list"; "decimal" ] -> Ok List_decimal
    | [ "list"; "inside" ] -> Ok List_inside
    | [ "list"; "outside" ] -> Ok List_outside
    | [ "list"; "image"; "none" ] -> Ok List_image_none
    | "list" :: "image" :: rest -> Ok (List_image_url (String.concat "-" rest))
    | [ "underline"; "offset"; "auto" ] -> Ok Underline_offset_auto
    | [ "underline"; "offset"; "0" ] -> Ok Underline_offset_0
    | [ "underline"; "offset"; "1" ] -> Ok Underline_offset_1
    | [ "underline"; "offset"; "2" ] -> Ok Underline_offset_2
    | [ "underline"; "offset"; "4" ] -> Ok Underline_offset_4
    | [ "underline"; "offset"; "8" ] -> Ok Underline_offset_8
    | [ "underline"; "offset"; n ] -> (
        match float_of_string_opt n with
        | Some px -> Ok (Underline_offset_px px)
        | None -> err_not_utility)
    | [ ""; "underline"; "offset"; n ] -> (
        match float_of_string_opt n with
        | Some px -> Ok (Underline_offset_neg_px px)
        | None -> err_not_utility)
    | [ "antialiased" ] -> Ok Antialiased
    | [ "subpixel"; "antialiased" ] -> Ok Subpixel_antialiased
    | [ "text"; "ellipsis" ] -> Ok Text_ellipsis
    | [ "text"; "clip" ] -> Ok Text_clip
    | [ "truncate" ] -> Ok Truncate
    | [ "text"; "wrap" ] -> Ok Text_wrap
    | [ "text"; "nowrap" ] -> Ok Text_nowrap
    | [ "text"; "balance" ] -> Ok Text_balance
    | [ "text"; "pretty" ] -> Ok Text_pretty
    | [ "break"; "normal" ] -> Ok Break_normal
    | [ "break"; "words" ] -> Ok Break_words
    | [ "break"; "all" ] -> Ok Break_all
    | [ "break"; "keep" ] -> Ok Break_keep
    | [ "overflow"; "wrap"; "normal" ] -> Ok Overflow_wrap_normal
    | [ "overflow"; "wrap"; "anywhere" ] -> Ok Overflow_wrap_anywhere
    | [ "overflow"; "wrap"; "break"; "word" ] -> Ok Overflow_wrap_break_word
    | [ "hyphens"; "none" ] -> Ok Hyphens_none
    | [ "hyphens"; "manual" ] -> Ok Hyphens_manual
    | [ "hyphens"; "auto" ] -> Ok Hyphens_auto
    | [ "font"; "stretch"; "ultra"; "condensed" ] ->
        Ok Font_stretch_ultra_condensed
    | [ "font"; "stretch"; "extra"; "condensed" ] ->
        Ok Font_stretch_extra_condensed
    | [ "font"; "stretch"; "condensed" ] -> Ok Font_stretch_condensed
    | [ "font"; "stretch"; "semi"; "condensed" ] ->
        Ok Font_stretch_semi_condensed
    | [ "font"; "stretch"; "normal" ] -> Ok Font_stretch_normal
    | [ "font"; "stretch"; "semi"; "expanded" ] -> Ok Font_stretch_semi_expanded
    | [ "font"; "stretch"; "expanded" ] -> Ok Font_stretch_expanded
    | [ "font"; "stretch"; "extra"; "expanded" ] ->
        Ok Font_stretch_extra_expanded
    | [ "font"; "stretch"; "ultra"; "expanded" ] ->
        Ok Font_stretch_ultra_expanded
    | [ "font"; "stretch"; n ] ->
        (* Handle both "50" and "50%" formats *)
        let n =
          if String.ends_with ~suffix:"%" n then
            String.sub n 0 (String.length n - 1)
          else n
        in
        Parse.int_bounded ~name:"font-stretch" ~min:50 ~max:200 n >|= fun i ->
        Font_stretch_percent i
    | [ "normal"; "nums" ] -> Ok Normal_nums
    | [ "ordinal" ] -> Ok Ordinal
    | [ "slashed"; "zero" ] -> Ok Slashed_zero
    | [ "lining"; "nums" ] -> Ok Lining_nums
    | [ "oldstyle"; "nums" ] -> Ok Oldstyle_nums
    | [ "proportional"; "nums" ] -> Ok Proportional_nums
    | [ "tabular"; "nums" ] -> Ok Tabular_nums
    | [ "diagonal"; "fractions" ] -> Ok Diagonal_fractions
    | [ "stacked"; "fractions" ] -> Ok Stacked_fractions
    | [ "indent"; n ] -> Parse.int_any n >|= fun i -> Indent i
    | [ "line"; "clamp"; "none" ] -> Ok Line_clamp_none
    | [ "line"; "clamp"; n ] ->
        Parse.int_bounded ~name:"line-clamp" ~min:0 ~max:999 n >|= fun i ->
        Line_clamp i
    | [ "content"; "none" ] -> Ok Content_none
    | "content" :: rest ->
        let joined = String.concat "-" rest in
        (* Parse arbitrary value syntax: content-["value"] -> value *)
        if
          String.length joined >= 4
          && String.get joined 0 = '['
          && String.get joined 1 = '"'
          && String.get joined (String.length joined - 1) = ']'
          && String.get joined (String.length joined - 2) = '"'
        then
          let value = String.sub joined 2 (String.length joined - 4) in
          Ok (Content value)
        else if Spacing.is_named_spacing joined then Ok (Content_named joined)
        else if Parse.is_valid_theme_name joined then Ok (Content joined)
        else err_not_utility
    | _ -> err_not_utility

  let to_class = function
    | Decoration_color (color, None) -> "decoration-" ^ Color.pp color
    | Decoration_color (color, Some shade) ->
        "decoration-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | Decoration_color_opacity (color, shade, opacity) ->
        "decoration-" ^ Color.pp color ^ "-" ^ string_of_int shade ^ "/"
        ^ Color.pp_opacity opacity
    | Underline -> "underline"
    | Overline -> "overline"
    | Line_through -> "line-through"
    | No_underline -> "no-underline"
    | Decoration_solid -> "decoration-solid"
    | Decoration_double -> "decoration-double"
    | Decoration_dotted -> "decoration-dotted"
    | Decoration_dashed -> "decoration-dashed"
    | Decoration_wavy -> "decoration-wavy"
    | Decoration_thickness n -> "decoration-" ^ string_of_int n
    | Decoration_from_font -> "decoration-from-font"
    | Tracking_tighter -> "tracking-tighter"
    | Tracking_tight -> "tracking-tight"
    | Tracking_normal -> "tracking-normal"
    | Tracking_wide -> "tracking-wide"
    | Tracking_wider -> "tracking-wider"
    | Tracking_widest -> "tracking-widest"
    | Uppercase -> "uppercase"
    | Lowercase -> "lowercase"
    | Capitalize -> "capitalize"
    | Normal_case -> "normal-case"
    | Whitespace_normal -> "whitespace-normal"
    | Whitespace_nowrap -> "whitespace-nowrap"
    | Whitespace_pre -> "whitespace-pre"
    | Whitespace_pre_line -> "whitespace-pre-line"
    | Whitespace_pre_wrap -> "whitespace-pre-wrap"
    | Whitespace_break_spaces -> "whitespace-break-spaces"
    | Align_baseline -> "align-baseline"
    | Align_top -> "align-top"
    | Align_middle -> "align-middle"
    | Align_bottom -> "align-bottom"
    | Align_text_top -> "align-text-top"
    | Align_text_bottom -> "align-text-bottom"
    | Align_sub -> "align-sub"
    | Align_super -> "align-super"
    | List_none -> "list-none"
    | List_disc -> "list-disc"
    | List_decimal -> "list-decimal"
    | List_inside -> "list-inside"
    | List_outside -> "list-outside"
    | List_image_none -> "list-image-none"
    | List_image_url url -> "list-image-" ^ url
    | Underline_offset_auto -> "underline-offset-auto"
    | Underline_offset_0 -> "underline-offset-0"
    | Underline_offset_1 -> "underline-offset-1"
    | Underline_offset_2 -> "underline-offset-2"
    | Underline_offset_4 -> "underline-offset-4"
    | Underline_offset_8 -> "underline-offset-8"
    | Underline_offset_px px ->
        let s = string_of_float px in
        let s =
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        "underline-offset-" ^ s
    | Underline_offset_neg_px px ->
        let s = string_of_float px in
        let s =
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        "-underline-offset-" ^ s
    | Antialiased -> "antialiased"
    | Subpixel_antialiased -> "subpixel-antialiased"
    | Text_ellipsis -> "text-ellipsis"
    | Text_clip -> "text-clip"
    | Truncate -> "truncate"
    | Text_wrap -> "text-wrap"
    | Text_nowrap -> "text-nowrap"
    | Text_balance -> "text-balance"
    | Text_pretty -> "text-pretty"
    | Break_normal -> "break-normal"
    | Break_words -> "break-words"
    | Break_all -> "break-all"
    | Break_keep -> "break-keep"
    | Overflow_wrap_normal -> "overflow-wrap-normal"
    | Overflow_wrap_anywhere -> "overflow-wrap-anywhere"
    | Overflow_wrap_break_word -> "overflow-wrap-break-word"
    | Hyphens_none -> "hyphens-none"
    | Hyphens_manual -> "hyphens-manual"
    | Hyphens_auto -> "hyphens-auto"
    | Font_stretch_ultra_condensed -> "font-stretch-ultra-condensed"
    | Font_stretch_extra_condensed -> "font-stretch-extra-condensed"
    | Font_stretch_condensed -> "font-stretch-condensed"
    | Font_stretch_semi_condensed -> "font-stretch-semi-condensed"
    | Font_stretch_normal -> "font-stretch-normal"
    | Font_stretch_semi_expanded -> "font-stretch-semi-expanded"
    | Font_stretch_expanded -> "font-stretch-expanded"
    | Font_stretch_extra_expanded -> "font-stretch-extra-expanded"
    | Font_stretch_ultra_expanded -> "font-stretch-ultra-expanded"
    | Font_stretch_percent n -> "font-stretch-" ^ string_of_int n ^ "%"
    | Normal_nums -> "normal-nums"
    | Ordinal -> "ordinal"
    | Slashed_zero -> "slashed-zero"
    | Lining_nums -> "lining-nums"
    | Oldstyle_nums -> "oldstyle-nums"
    | Proportional_nums -> "proportional-nums"
    | Tabular_nums -> "tabular-nums"
    | Diagonal_fractions -> "diagonal-fractions"
    | Stacked_fractions -> "stacked-fractions"
    | Indent n -> "indent-" ^ string_of_int n
    | Line_clamp n -> "line-clamp-" ^ string_of_int n
    | Line_clamp_none -> "line-clamp-none"
    | Content_none -> "content-none"
    | Content s -> "content-[\"" ^ s ^ "\"]"
    | Content_named s -> "content-" ^ s

  (** {1 Ordering Support} *)

  let suborder = function
    (* Decoration color - comes first in late typography *)
    | Decoration_color (color, shade_opt) -> (
        let shade = match shade_opt with Some s -> s | None -> 500 in
        try
          let _, color_order = Color.utilities_order (Color.pp color) in
          5000 + (color_order * 1000) + shade
        with Not_found | Failure _ -> 5000 + shade)
    | Decoration_color_opacity (color, shade, _) -> (
        try
          let _, color_order = Color.utilities_order (Color.pp color) in
          5000 + (color_order * 1000) + shade
        with Not_found | Failure _ -> 5000 + shade)
    (* Text decoration lines - suborder >= 8000 (alphabetical) *)
    | Line_through -> 8000
    | No_underline -> 8001
    | Overline -> 8002
    | Underline -> 8003
    (* Decoration styles *)
    | Decoration_solid -> 8100
    | Decoration_double -> 8101
    | Decoration_dotted -> 8102
    | Decoration_dashed -> 8103
    | Decoration_wavy -> 8104
    | Decoration_thickness n -> 8200 + n
    | Decoration_from_font -> 8210
    (* Tracking *)
    | Tracking_tighter -> 8300
    | Tracking_tight -> 8301
    | Tracking_normal -> 8302
    | Tracking_wide -> 8303
    | Tracking_wider -> 8304
    | Tracking_widest -> 8305
    (* Text transform - alphabetical order *)
    | Capitalize -> 8400
    | Lowercase -> 8401
    | Normal_case -> 8402
    | Uppercase -> 8403
    (* Whitespace - alphabetical order *)
    | Whitespace_break_spaces -> 8500
    | Whitespace_normal -> 8501
    | Whitespace_nowrap -> 8502
    | Whitespace_pre -> 8503
    | Whitespace_pre_line -> 8504
    | Whitespace_pre_wrap -> 8505
    (* Vertical align - alphabetical order *)
    | Align_baseline -> 8600
    | Align_bottom -> 8601
    | Align_middle -> 8602
    | Align_sub -> 8603
    | Align_super -> 8604
    | Align_text_bottom -> 8605
    | Align_text_top -> 8606
    | Align_top -> 8607
    (* List utilities - alphabetical order *)
    | List_decimal -> 8700
    | List_disc -> 8701
    | List_image_none -> 8702
    | List_inside -> 8703
    | List_none -> 8704
    | List_outside -> 8705
    | List_image_url _ -> 8706
    (* Underline offset *)
    | Underline_offset_auto -> 8800
    | Underline_offset_0 -> 8801
    | Underline_offset_1 -> 8802
    | Underline_offset_2 -> 8803
    | Underline_offset_4 -> 8804
    | Underline_offset_8 -> 8805
    | Underline_offset_px px -> 8806 + int_of_float px
    | Underline_offset_neg_px px -> 8900 + int_of_float px
    (* Antialiased *)
    | Antialiased -> 8900
    | Subpixel_antialiased -> 8901
    (* Text overflow - alphabetical order: text-clip, text-ellipsis, truncate *)
    | Text_clip -> 9000
    | Text_ellipsis -> 9001
    | Truncate -> 9002
    (* Text wrap - alphabetical order *)
    | Text_balance -> 9100
    | Text_nowrap -> 9101
    | Text_pretty -> 9102
    | Text_wrap -> 9103
    (* Break utilities *)
    | Break_normal -> 9200
    | Break_words -> 9201
    | Break_all -> 9202
    | Break_keep -> 9203
    (* Overflow wrap *)
    | Overflow_wrap_normal -> 9300
    | Overflow_wrap_anywhere -> 9301
    | Overflow_wrap_break_word -> 9302
    (* Hyphens - alphabetical order *)
    | Hyphens_auto -> 9400
    | Hyphens_manual -> 9401
    | Hyphens_none -> 9402
    (* Font stretch - percentages first (sorted by value), then keywords *)
    | Font_stretch_percent n -> 9500 + n
    | Font_stretch_condensed -> 9700
    | Font_stretch_expanded -> 9701
    | Font_stretch_extra_condensed -> 9702
    | Font_stretch_extra_expanded -> 9703
    | Font_stretch_normal -> 9704
    | Font_stretch_semi_condensed -> 9705
    | Font_stretch_semi_expanded -> 9706
    | Font_stretch_ultra_condensed -> 9707
    | Font_stretch_ultra_expanded -> 9708
    (* Numeric variants - alphabetical order with normal-nums last *)
    | Diagonal_fractions -> 9700
    | Lining_nums -> 9701
    | Oldstyle_nums -> 9702
    | Ordinal -> 9703
    | Proportional_nums -> 9704
    | Slashed_zero -> 9705
    | Stacked_fractions -> 9706
    | Tabular_nums -> 9707
    | Normal_nums -> 9708
    (* Indent and line clamp *)
    | Indent n -> 9800 + n
    | Line_clamp n -> 9900 + n
    | Line_clamp_none -> 9999
    (* Content *)
    | Content_none -> 10000
    | Content _ -> 10001
    | Content_named _ -> 10002

  (* Shared utility implementations *)
  let underline = style [ text_decoration_line Underline ]
  let overline = style [ text_decoration_line Overline ]
  let line_through = style [ text_decoration_line Line_through ]
  let no_underline = style [ text_decoration_line None ]
  let decoration_solid = style [ text_decoration_style Solid ]
  let decoration_double = style [ text_decoration_style Double ]
  let decoration_dotted = style [ text_decoration_style Dotted ]
  let decoration_dashed = style [ text_decoration_style Dashed ]
  let decoration_wavy = style [ text_decoration_style Wavy ]

  let decoration_thickness n =
    style [ text_decoration_thickness (Px (float_of_int n)) ]

  let decoration_from_font = style [ text_decoration_thickness From_font ]

  let decoration_color ?(shade = 500) (color : Color.color) =
    if Color.is_custom_color color then
      let css_color = Color.to_css color shade in
      style
        [
          webkit_text_decoration_color css_color;
          text_decoration_color css_color;
        ]
    else
      let color_var = Color.get_color_var color shade in
      let default_color =
        Color.to_css color (if Color.is_base_color color then 500 else shade)
      in
      let color_decl, color_ref = Var.binding color_var default_color in
      style
        [
          color_decl;
          webkit_text_decoration_color (Css.Var color_ref);
          text_decoration_color (Css.Var color_ref);
        ]

  let decoration_color_with_opacity (color : Color.color) shade opacity =
    (* Tailwind outputs only unprefixed text-decoration-color in the fallback *)
    match Color.get_hex_alpha_color color shade opacity with
    | Some hex_alpha -> style [ text_decoration_color (Css.hex hex_alpha) ]
    | None ->
        (* Fallback for non-scheme colors *)
        let css_color = Color.to_css color shade in
        style [ text_decoration_color css_color ]

  let whitespace_normal = style [ white_space Normal ]
  let whitespace_nowrap = style [ white_space Nowrap ]
  let whitespace_pre = style [ white_space Pre ]
  let whitespace_pre_line = style [ white_space Pre_line ]
  let whitespace_pre_wrap = style [ white_space Pre_wrap ]
  let whitespace_break_spaces = style [ white_space Break_spaces ]

  let tracking_tighter =
    let theme_decl, theme_ref = Var.binding tracking_tighter_var (Em (-0.05)) in
    let channel_decl, _ = Var.binding tracking_var (Css.Var theme_ref) in
    let property_rules =
      Var.property_rule tracking_var |> Option.to_list |> Css.concat
    in
    style ~property_rules
      [ theme_decl; channel_decl; letter_spacing (Css.Var theme_ref) ]

  let tracking_tight =
    (* Theme var: --tracking-tight: -0.025em *)
    let theme_decl, theme_ref = Var.binding tracking_tight_var (Em (-0.025)) in
    (* Channel var: --tw-tracking: var(--tracking-tight) *)
    let channel_decl, _ = Var.binding tracking_var (Css.Var theme_ref) in
    (* Property: letter-spacing: var(--tracking-tight) *)
    let property_rules =
      Var.property_rule tracking_var |> Option.to_list |> Css.concat
    in
    style ~property_rules
      [ theme_decl; channel_decl; letter_spacing (Css.Var theme_ref) ]

  let tracking_normal =
    let theme_decl, theme_ref = Var.binding tracking_normal_var Zero in
    let channel_decl, _ = Var.binding tracking_var (Css.Var theme_ref) in
    let property_rules =
      Var.property_rule tracking_var |> Option.to_list |> Css.concat
    in
    style ~property_rules
      [ theme_decl; channel_decl; letter_spacing (Css.Var theme_ref) ]

  let tracking_wide =
    let theme_decl, theme_ref = Var.binding tracking_wide_var (Em 0.025) in
    let channel_decl, _ = Var.binding tracking_var (Css.Var theme_ref) in
    let property_rules =
      Var.property_rule tracking_var |> Option.to_list |> Css.concat
    in
    style ~property_rules
      [ theme_decl; channel_decl; letter_spacing (Css.Var theme_ref) ]

  let tracking_wider =
    let theme_decl, theme_ref = Var.binding tracking_wider_var (Em 0.05) in
    let channel_decl, _ = Var.binding tracking_var (Css.Var theme_ref) in
    let property_rules =
      Var.property_rule tracking_var |> Option.to_list |> Css.concat
    in
    style ~property_rules
      [ theme_decl; channel_decl; letter_spacing (Css.Var theme_ref) ]

  let tracking_widest =
    let theme_decl, theme_ref = Var.binding tracking_widest_var (Em 0.1) in
    let channel_decl, _ = Var.binding tracking_var (Css.Var theme_ref) in
    let property_rules =
      Var.property_rule tracking_var |> Option.to_list |> Css.concat
    in
    style ~property_rules
      [ theme_decl; channel_decl; letter_spacing (Css.Var theme_ref) ]

  let uppercase = style [ text_transform Uppercase ]
  let lowercase = style [ text_transform Lowercase ]
  let capitalize = style [ text_transform Capitalize ]
  let normal_case = style [ text_transform None ]

  let underline_offset_auto =
    style
      [
        text_underline_offset
          (Var
             (Var.theme_ref "text-underline-offset-auto"
                ~default:(Auto : Css.length)
                ~default_css:"auto"));
      ]

  let underline_offset_0 = style [ text_underline_offset Zero ]
  let underline_offset_1 = style [ text_underline_offset (Px 1.) ]
  let underline_offset_2 = style [ text_underline_offset (Px 2.) ]
  let underline_offset_4 = style [ text_underline_offset (Px 4.) ]
  let underline_offset_8 = style [ text_underline_offset (Px 8.) ]

  let antialiased =
    style
      [ webkit_font_smoothing Antialiased; moz_osx_font_smoothing Grayscale ]

  let subpixel_antialiased =
    style [ webkit_font_smoothing Auto; moz_osx_font_smoothing Auto ]

  let list_image_url url = style [ list_style_image (Url url) ]

  let indent n =
    let spacing_decl, spacing_ref = Var.binding Theme.spacing_var (Rem 0.25) in
    style
      [
        spacing_decl;
        text_indent
          (Calc Calc.(length (Css.Var spacing_ref) * float (float_of_int n)));
      ]

  let line_clamp n =
    style
      [
        webkit_line_clamp (Lines n);
        webkit_box_orient Vertical;
        display Webkit_box;
        overflow Hidden;
      ]

  let line_clamp_none_style =
    (* Tailwind v4: uses theme var and keeps webkit-box display *)
    style
      [
        webkit_line_clamp
          (Var
             (Var.theme_ref "line-clamp-none" ~default:Unset
                ~default_css:"unset"));
        webkit_box_orient Vertical;
        display Webkit_box;
        overflow Hidden;
      ]

  let content_none =
    let content_decl, content_ref = Var.binding content_var None in
    let property_rules = Var.property_rules content_var in
    style ~property_rules
      [ content (Css.Var content_ref); content_decl; content None ]

  let content s =
    (* The content value is the string itself, not double-quoted *)
    (* The class name needs to escape the quotes properly for CSS *)
    let content_decl, content_ref = Var.binding content_var (String s) in
    let property_rules = Var.property_rules content_var in
    style ~property_rules
      [
        content (Css.Var content_ref);
        content_decl;
        content (Css.Var content_ref);
      ]

  let content_named name =
    let theme_ref : Css.content Css.var =
      Css.var_ref ~layer:"theme" ("content-" ^ name)
    in
    let content_decl, content_ref = Var.binding content_var (Var theme_ref) in
    let property_rules = Var.property_rules content_var in
    let c : Css.content = Css.Var content_ref in
    style ~property_rules [ content_decl; Css.content c ]

  let align_baseline = style [ vertical_align Baseline ]
  let align_top = style [ vertical_align Top ]
  let align_middle = style [ vertical_align Middle ]
  let align_bottom = style [ vertical_align Bottom ]
  let align_text_top = style [ vertical_align Text_top ]
  let align_text_bottom = style [ vertical_align Text_bottom ]
  let align_sub = style [ vertical_align Sub ]
  let align_super = style [ vertical_align Super ]

  let list_none =
    style
      [
        list_style_type
          (Var
             (Var.theme_ref "list-style-type-none"
                ~default:(None : Css.list_style_type)
                ~default_css:"none"));
      ]

  let list_disc = style [ list_style_type Disc ]
  let list_decimal = style [ list_style_type Decimal ]
  let list_inside = style [ list_style_position Inside ]
  let list_outside = style [ list_style_position Outside ]

  let list_image_none =
    style
      [
        list_style_image
          (Var
             (Var.theme_ref "list-style-image-none"
                ~default:(None : Css.list_style_image)
                ~default_css:"none"));
      ]

  let text_ellipsis = style [ text_overflow Ellipsis ]
  let text_clip = style [ text_overflow Clip ]

  let truncate =
    style [ text_overflow Ellipsis; white_space Nowrap; overflow Hidden ]

  let text_wrap = style [ Css.text_wrap Wrap ]
  let text_nowrap = style [ Css.text_wrap No_wrap ]
  let text_balance = style [ Css.text_wrap Balance ]
  let text_pretty = style [ Css.text_wrap Pretty ]
  let break_normal = style [ overflow_wrap Normal; word_break Normal ]
  let break_words = style [ overflow_wrap Break_word ]
  let break_all = style [ word_break Break_all ]
  let break_keep = style [ word_break Keep_all ]
  let overflow_wrap_normal = style [ overflow_wrap Normal ]
  let overflow_wrap_anywhere = style [ overflow_wrap Anywhere ]
  let overflow_wrap_break_word = style [ overflow_wrap Break_word ]
  let hyphens_none = style [ webkit_hyphens None; hyphens None ]
  let hyphens_manual = style [ webkit_hyphens Manual; hyphens Manual ]
  let hyphens_auto = style [ webkit_hyphens Auto; hyphens Auto ]
  let font_stretch_ultra_condensed = style [ font_stretch Ultra_condensed ]
  let font_stretch_extra_condensed = style [ font_stretch Extra_condensed ]
  let font_stretch_condensed = style [ font_stretch Condensed ]
  let font_stretch_semi_condensed = style [ font_stretch Semi_condensed ]
  let font_stretch_normal = style [ font_stretch Normal ]
  let font_stretch_semi_expanded = style [ font_stretch Semi_expanded ]
  let font_stretch_expanded = style [ font_stretch Expanded ]
  let font_stretch_extra_expanded = style [ font_stretch Extra_expanded ]
  let font_stretch_ultra_expanded = style [ font_stretch Ultra_expanded ]
  let font_stretch_percent n = style [ font_stretch (Pct (float_of_int n)) ]
  let normal_nums = style [ font_variant_numeric (Tokens [ Normal ]) ]

  let font_variant_numeric_utility var_to_set value =
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

    style
      ~property_rules:(Css.concat property_rules)
      [ active_decl; font_variant_numeric composed_value ]

  let ordinal = font_variant_numeric_utility `Ordinal Ordinal
  let slashed_zero = font_variant_numeric_utility `Slashed Slashed_zero
  let lining_nums = font_variant_numeric_utility `Figure Lining_nums
  let oldstyle_nums = font_variant_numeric_utility `Figure Oldstyle_nums

  let proportional_nums =
    font_variant_numeric_utility `Spacing Proportional_nums

  let tabular_nums = font_variant_numeric_utility `Spacing Tabular_nums

  let diagonal_fractions =
    font_variant_numeric_utility `Fraction Diagonal_fractions

  let stacked_fractions =
    font_variant_numeric_utility `Fraction Stacked_fractions

  let to_style = function
    | Decoration_color (color, None) -> decoration_color color
    | Decoration_color (color, Some shade) -> decoration_color ~shade color
    | Decoration_color_opacity (color, shade, opacity) ->
        decoration_color_with_opacity color shade opacity
    | Underline -> underline
    | Overline -> overline
    | Line_through -> line_through
    | No_underline -> no_underline
    | Decoration_solid -> decoration_solid
    | Decoration_double -> decoration_double
    | Decoration_dotted -> decoration_dotted
    | Decoration_dashed -> decoration_dashed
    | Decoration_wavy -> decoration_wavy
    | Decoration_thickness n -> decoration_thickness n
    | Decoration_from_font -> decoration_from_font
    | Tracking_tighter -> tracking_tighter
    | Tracking_tight -> tracking_tight
    | Tracking_normal -> tracking_normal
    | Tracking_wide -> tracking_wide
    | Tracking_wider -> tracking_wider
    | Tracking_widest -> tracking_widest
    | Uppercase -> uppercase
    | Lowercase -> lowercase
    | Capitalize -> capitalize
    | Normal_case -> normal_case
    | Whitespace_normal -> whitespace_normal
    | Whitespace_nowrap -> whitespace_nowrap
    | Whitespace_pre -> whitespace_pre
    | Whitespace_pre_line -> whitespace_pre_line
    | Whitespace_pre_wrap -> whitespace_pre_wrap
    | Whitespace_break_spaces -> whitespace_break_spaces
    | Align_baseline -> align_baseline
    | Align_top -> align_top
    | Align_middle -> align_middle
    | Align_bottom -> align_bottom
    | Align_text_top -> align_text_top
    | Align_text_bottom -> align_text_bottom
    | Align_sub -> align_sub
    | Align_super -> align_super
    | List_none -> list_none
    | List_disc -> list_disc
    | List_decimal -> list_decimal
    | List_inside -> list_inside
    | List_outside -> list_outside
    | List_image_none -> list_image_none
    | List_image_url url -> list_image_url url
    | Underline_offset_auto -> underline_offset_auto
    | Underline_offset_0 -> underline_offset_0
    | Underline_offset_1 -> underline_offset_1
    | Underline_offset_2 -> underline_offset_2
    | Underline_offset_4 -> underline_offset_4
    | Underline_offset_8 -> underline_offset_8
    | Underline_offset_px px -> style [ text_underline_offset (Px px) ]
    | Underline_offset_neg_px px ->
        style
          [
            text_underline_offset
              (Calc (Calc.mul (Calc.length (Px px)) (Calc.float (-1.))));
          ]
    | Antialiased -> antialiased
    | Subpixel_antialiased -> subpixel_antialiased
    | Text_ellipsis -> text_ellipsis
    | Text_clip -> text_clip
    | Truncate -> truncate
    | Text_wrap -> text_wrap
    | Text_nowrap -> text_nowrap
    | Text_balance -> text_balance
    | Text_pretty -> text_pretty
    | Break_normal -> break_normal
    | Break_words -> break_words
    | Break_all -> break_all
    | Break_keep -> break_keep
    | Overflow_wrap_normal -> overflow_wrap_normal
    | Overflow_wrap_anywhere -> overflow_wrap_anywhere
    | Overflow_wrap_break_word -> overflow_wrap_break_word
    | Hyphens_none -> hyphens_none
    | Hyphens_manual -> hyphens_manual
    | Hyphens_auto -> hyphens_auto
    | Font_stretch_ultra_condensed -> font_stretch_ultra_condensed
    | Font_stretch_extra_condensed -> font_stretch_extra_condensed
    | Font_stretch_condensed -> font_stretch_condensed
    | Font_stretch_semi_condensed -> font_stretch_semi_condensed
    | Font_stretch_normal -> font_stretch_normal
    | Font_stretch_semi_expanded -> font_stretch_semi_expanded
    | Font_stretch_expanded -> font_stretch_expanded
    | Font_stretch_extra_expanded -> font_stretch_extra_expanded
    | Font_stretch_ultra_expanded -> font_stretch_ultra_expanded
    | Font_stretch_percent n -> font_stretch_percent n
    | Normal_nums -> normal_nums
    | Ordinal -> ordinal
    | Slashed_zero -> slashed_zero
    | Lining_nums -> lining_nums
    | Oldstyle_nums -> oldstyle_nums
    | Proportional_nums -> proportional_nums
    | Tabular_nums -> tabular_nums
    | Diagonal_fractions -> diagonal_fractions
    | Stacked_fractions -> stacked_fractions
    | Indent n -> indent n
    | Line_clamp n -> line_clamp n
    | Line_clamp_none -> line_clamp_none_style
    | Content_none -> content_none
    | Content s -> content s
    | Content_named name -> content_named name
end

(* Register both handlers *)
let () = Utility.register (module Typography_early)
let () = Utility.register (module Typography_late)

(* Public API - using appropriate handler for each utility *)

(* Early typography utilities - priority 22 *)
let utility_early x = Utility.base (Typography_early.Self x)
let text_xs = utility_early Typography_early.Text_xs
let text_sm = utility_early Typography_early.Text_sm
let text_base = utility_early Typography_early.Text_base
let text_lg = utility_early Typography_early.Text_lg
let text_xl = utility_early Typography_early.Text_xl
let text_2xl = utility_early Typography_early.Text_2xl
let text_3xl = utility_early Typography_early.Text_3xl
let text_4xl = utility_early Typography_early.Text_4xl
let text_5xl = utility_early Typography_early.Text_5xl
let text_6xl = utility_early Typography_early.Text_6xl
let text_7xl = utility_early Typography_early.Text_7xl
let text_8xl = utility_early Typography_early.Text_8xl
let text_9xl = utility_early Typography_early.Text_9xl
let font_medium = utility_early Typography_early.Font_medium
let font_thin = utility_early Typography_early.Font_thin
let font_extralight = utility_early Typography_early.Font_extralight
let font_light = utility_early Typography_early.Font_light
let font_normal = utility_early Typography_early.Font_normal
let font_semibold = utility_early Typography_early.Font_semibold
let font_bold = utility_early Typography_early.Font_bold
let font_extrabold = utility_early Typography_early.Font_extrabold
let font_black = utility_early Typography_early.Font_black
let font_mono = utility_early Typography_early.Font_mono
let font_sans = utility_early Typography_early.Font_sans
let font_serif = utility_early Typography_early.Font_serif
let italic = utility_early Typography_early.Italic
let not_italic = utility_early Typography_early.Not_italic
let text_left = utility_early Typography_early.Text_left
let text_center = utility_early Typography_early.Text_center
let text_right = utility_early Typography_early.Text_right
let text_justify = utility_early Typography_early.Text_justify
let text_start = utility_early Typography_early.Text_start
let text_end = utility_early Typography_early.Text_end
let leading_none = utility_early Typography_early.Leading_none
let leading_tight = utility_early Typography_early.Leading_tight
let leading_snug = utility_early Typography_early.Leading_snug
let leading_normal = utility_early Typography_early.Leading_normal
let leading_relaxed = utility_early Typography_early.Leading_relaxed
let leading_loose = utility_early Typography_early.Leading_loose
let leading n = utility_early (Typography_early.Leading n)

(* Late typography utilities - priority 24 *)
let utility_late x = Utility.base (Typography_late.Self x)

let decoration_color ?(shade = 500) color =
  utility_late (Typography_late.Decoration_color (color, Some shade))

let underline = utility_late Typography_late.Underline
let overline = utility_late Typography_late.Overline
let line_through = utility_late Typography_late.Line_through
let no_underline = utility_late Typography_late.No_underline
let decoration_solid = utility_late Typography_late.Decoration_solid
let decoration_double = utility_late Typography_late.Decoration_double
let decoration_dotted = utility_late Typography_late.Decoration_dotted
let decoration_dashed = utility_late Typography_late.Decoration_dashed
let decoration_wavy = utility_late Typography_late.Decoration_wavy

let decoration_thickness n =
  utility_late (Typography_late.Decoration_thickness n)

let decoration_from_font = utility_late Typography_late.Decoration_from_font
let whitespace_normal = utility_late Typography_late.Whitespace_normal
let whitespace_nowrap = utility_late Typography_late.Whitespace_nowrap
let whitespace_pre = utility_late Typography_late.Whitespace_pre
let whitespace_pre_line = utility_late Typography_late.Whitespace_pre_line
let whitespace_pre_wrap = utility_late Typography_late.Whitespace_pre_wrap

let whitespace_break_spaces =
  utility_late Typography_late.Whitespace_break_spaces

let tracking_tighter = utility_late Typography_late.Tracking_tighter
let tracking_tight = utility_late Typography_late.Tracking_tight
let tracking_normal = utility_late Typography_late.Tracking_normal
let tracking_wide = utility_late Typography_late.Tracking_wide
let tracking_wider = utility_late Typography_late.Tracking_wider
let tracking_widest = utility_late Typography_late.Tracking_widest
let uppercase = utility_late Typography_late.Uppercase
let lowercase = utility_late Typography_late.Lowercase
let capitalize = utility_late Typography_late.Capitalize
let normal_case = utility_late Typography_late.Normal_case
let underline_offset_auto = utility_late Typography_late.Underline_offset_auto
let underline_offset_0 = utility_late Typography_late.Underline_offset_0
let underline_offset_1 = utility_late Typography_late.Underline_offset_1
let underline_offset_2 = utility_late Typography_late.Underline_offset_2
let underline_offset_4 = utility_late Typography_late.Underline_offset_4
let underline_offset_8 = utility_late Typography_late.Underline_offset_8
let antialiased = utility_late Typography_late.Antialiased
let subpixel_antialiased = utility_late Typography_late.Subpixel_antialiased
let align_baseline = utility_late Typography_late.Align_baseline
let align_top = utility_late Typography_late.Align_top
let align_middle = utility_late Typography_late.Align_middle
let align_bottom = utility_late Typography_late.Align_bottom
let align_text_top = utility_late Typography_late.Align_text_top
let align_text_bottom = utility_late Typography_late.Align_text_bottom
let align_sub = utility_late Typography_late.Align_sub
let align_super = utility_late Typography_late.Align_super
let list_none = utility_late Typography_late.List_none
let list_disc = utility_late Typography_late.List_disc
let list_decimal = utility_late Typography_late.List_decimal
let list_inside = utility_late Typography_late.List_inside
let list_outside = utility_late Typography_late.List_outside
let list_image_none = utility_late Typography_late.List_image_none
let list_image_url url = utility_late (Typography_late.List_image_url url)
let indent n = utility_late (Typography_late.Indent n)
let line_clamp n = utility_late (Typography_late.Line_clamp n)
let content_none = utility_late Typography_late.Content_none
let content s = utility_late (Typography_late.Content s)
let text_ellipsis = utility_late Typography_late.Text_ellipsis
let text_clip = utility_late Typography_late.Text_clip
let text_wrap = utility_late Typography_late.Text_wrap
let text_nowrap = utility_late Typography_late.Text_nowrap
let text_balance = utility_late Typography_late.Text_balance
let text_pretty = utility_late Typography_late.Text_pretty
let break_normal = utility_late Typography_late.Break_normal
let break_words = utility_late Typography_late.Break_words
let break_all = utility_late Typography_late.Break_all
let break_keep = utility_late Typography_late.Break_keep
let overflow_wrap_normal = utility_late Typography_late.Overflow_wrap_normal
let overflow_wrap_anywhere = utility_late Typography_late.Overflow_wrap_anywhere

let overflow_wrap_break_word =
  utility_late Typography_late.Overflow_wrap_break_word

let hyphens_none = utility_late Typography_late.Hyphens_none
let hyphens_manual = utility_late Typography_late.Hyphens_manual
let hyphens_auto = utility_late Typography_late.Hyphens_auto

let font_stretch_ultra_condensed =
  utility_late Typography_late.Font_stretch_ultra_condensed

let font_stretch_extra_condensed =
  utility_late Typography_late.Font_stretch_extra_condensed

let font_stretch_condensed = utility_late Typography_late.Font_stretch_condensed

let font_stretch_semi_condensed =
  utility_late Typography_late.Font_stretch_semi_condensed

let font_stretch_normal = utility_late Typography_late.Font_stretch_normal

let font_stretch_semi_expanded =
  utility_late Typography_late.Font_stretch_semi_expanded

let font_stretch_expanded = utility_late Typography_late.Font_stretch_expanded

let font_stretch_extra_expanded =
  utility_late Typography_late.Font_stretch_extra_expanded

let font_stretch_ultra_expanded =
  utility_late Typography_late.Font_stretch_ultra_expanded

let font_stretch_percent n =
  utility_late (Typography_late.Font_stretch_percent n)

(** {1 Numeric Variants} *)

let normal_nums = utility_late Typography_late.Normal_nums
let ordinal = utility_late Typography_late.Ordinal
let slashed_zero = utility_late Typography_late.Slashed_zero
let lining_nums = utility_late Typography_late.Lining_nums
let oldstyle_nums = utility_late Typography_late.Oldstyle_nums
let proportional_nums = utility_late Typography_late.Proportional_nums
let tabular_nums = utility_late Typography_late.Tabular_nums
let diagonal_fractions = utility_late Typography_late.Diagonal_fractions
let stacked_fractions = utility_late Typography_late.Stacked_fractions
