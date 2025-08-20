(** Typography utilities for text and font styling *)

open Core
open Css

(** {1 Font Size Utilities} *)

let text_xs =
  style "text-xs"
    [
      font_size (Var (var "text-xs"));
      line_height
        (Var (var ~fallback:(Var (var "text-xs--line-height")) "tw-leading"));
    ]

let text_sm =
  style "text-sm"
    [
      font_size (Var (var "text-sm"));
      line_height
        (Var (var ~fallback:(Var (var "text-sm--line-height")) "tw-leading"));
    ]

let text_base =
  style "text-base"
    [
      font_size (Var (var "text-base"));
      line_height
        (Var (var ~fallback:(Var (var "text-base--line-height")) "tw-leading"));
    ]

let text_lg =
  style "text-lg"
    [
      font_size (Var (var "text-lg"));
      line_height
        (Var (var ~fallback:(Var (var "text-lg--line-height")) "tw-leading"));
    ]

let text_xl =
  style "text-xl"
    [
      font_size (Var (var "text-xl"));
      line_height
        (Var (var ~fallback:(Var (var "text-xl--line-height")) "tw-leading"));
    ]

let text_2xl =
  style "text-2xl"
    [
      font_size (Var (var "text-2xl"));
      line_height
        (Var (var ~fallback:(Var (var "text-2xl--line-height")) "tw-leading"));
    ]

let text_3xl =
  style "text-3xl"
    [
      font_size (Var (var "text-3xl"));
      line_height
        (Var (var ~fallback:(Var (var "text-3xl--line-height")) "tw-leading"));
    ]

let text_4xl =
  style "text-4xl"
    [
      font_size (Var (var "text-4xl"));
      line_height
        (Var (var ~fallback:(Var (var "text-4xl--line-height")) "tw-leading"));
    ]

let text_5xl =
  style "text-5xl"
    [
      font_size (Var (var "text-5xl"));
      line_height
        (Var (var ~fallback:(Var (var "text-5xl--line-height")) "tw-leading"));
    ]

let text_6xl =
  style "text-6xl"
    [
      font_size (Var (var "text-6xl"));
      line_height
        (Var (var ~fallback:(Var (var "text-6xl--line-height")) "tw-leading"));
    ]

let text_7xl =
  style "text-7xl"
    [
      font_size (Var (var "text-7xl"));
      line_height
        (Var (var ~fallback:(Var (var "text-7xl--line-height")) "tw-leading"));
    ]

let text_8xl =
  style "text-8xl"
    [
      font_size (Var (var "text-8xl"));
      line_height
        (Var (var ~fallback:(Var (var "text-8xl--line-height")) "tw-leading"));
    ]

let text_9xl =
  style "text-9xl"
    [
      font_size (Var (var "text-9xl"));
      line_height
        (Var (var ~fallback:(Var (var "text-9xl--line-height")) "tw-leading"));
    ]

(** {1 Font Weight Utilities} *)

let font_thin =
  style_with_vars "font-thin"
    [
      custom_property "--tw-font-weight" "var(--font-weight-thin)";
      font_weight (Var (var "font-weight-thin"));
    ]
    []

let font_light =
  style_with_vars "font-light"
    [
      custom_property "--tw-font-weight" "var(--font-weight-light)";
      font_weight (Var (var "font-weight-light"));
    ]
    []

let font_normal =
  style_with_vars "font-normal"
    [
      custom_property "--tw-font-weight" "var(--font-weight-normal)";
      font_weight (Var (var "font-weight-normal"));
    ]
    []

let font_medium =
  style_with_vars "font-medium"
    [
      custom_property "--tw-font-weight" "var(--font-weight-medium)";
      font_weight (Var (var "font-weight-medium"));
    ]
    []

let font_semibold =
  style_with_vars "font-semibold"
    [
      custom_property "--tw-font-weight" "var(--font-weight-semibold)";
      font_weight (Var (var "font-weight-semibold"));
    ]
    []

let font_bold =
  style_with_vars "font-bold"
    [
      custom_property "--tw-font-weight" "var(--font-weight-bold)";
      font_weight (Var (var "font-weight-bold"));
    ]
    []

let font_extrabold =
  style_with_vars "font-extrabold"
    [
      custom_property "--tw-font-weight" "var(--font-weight-extrabold)";
      font_weight (Var (var "font-weight-extrabold"));
    ]
    []

let font_black =
  style_with_vars "font-black"
    [
      custom_property "--tw-font-weight" "var(--font-weight-black)";
      font_weight (Var (var "font-weight-black"));
    ]
    []

(** {1 Font Family Utilities} *)

let font_sans =
  style "font-sans"
    [ font_family [ Var { name = "font-sans"; fallback = None } ] ]

let font_serif =
  style "font-serif"
    [ font_family [ Var { name = "font-serif"; fallback = None } ] ]

let font_mono =
  style "font-mono"
    [ font_family [ Var { name = "font-mono"; fallback = None } ] ]

(** {1 Font Style Utilities} *)

let italic = style "italic" [ font_style Italic ]
let not_italic = style "not-italic" [ font_style Font_normal ]

(** {1 Text Alignment Utilities} *)

let text_left = style "text-left" [ text_align Left ]
let text_center = style "text-center" [ text_align Center ]
let text_right = style "text-right" [ text_align Right ]
let text_justify = style "text-justify" [ text_align Justify ]

(** {1 Text Decoration Utilities} *)

let underline = style "underline" [ text_decoration Underline ]
let line_through = style "line-through" [ text_decoration Line_through ]
let no_underline = style "no-underline" [ text_decoration None ]

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
