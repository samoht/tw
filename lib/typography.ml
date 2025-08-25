(** Typography utilities for text and font styling

    What's included:
    - Font size scale `text-{xs..9xl}` with matching line-height variables.
    - Font weight, font family, font style, text alignment, text decoration,
      line-height presets and `leading-N`, letter-spacing presets, case.

    What's not:
    - Some advanced text decoration options (thickness, underline offset) and
      typography nuances not present in the typed `Css` API. Extend with
      `style` and raw `Css.property` when needed.

    Parsing contract (`of_string`):
    - Accepts tokens for the above utilities, e.g. ["text"; "xl"],
      ["font"; "semibold"], ["leading"; n], ["tracking"; "wider"],
      ["underline"], ["uppercase"]. Unknown tokens yield
      `Error (`Msg "Not a typography utility")`. *)

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

(** {1 Parsing Functions} *)

let int_of_string_positive name s =
  match int_of_string_opt s with
  | None -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))
  | Some n when n > 0 -> Ok n
  | Some _ -> Error (`Msg (name ^ " must be positive: " ^ s))

let ( >|= ) r f = Result.map f r

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
  | [ "line"; "through" ] -> Ok line_through
  | [ "no"; "underline" ] -> Ok no_underline
  | [ "leading"; "none" ] -> Ok leading_none
  | [ "leading"; "tight" ] -> Ok leading_tight
  | [ "leading"; "snug" ] -> Ok leading_snug
  | [ "leading"; "normal" ] -> Ok leading_normal
  | [ "leading"; "relaxed" ] -> Ok leading_relaxed
  | [ "leading"; "loose" ] -> Ok leading_loose
  | [ "leading"; n ] -> int_of_string_positive "leading" n >|= leading
  | [ "tracking"; "tighter" ] -> Ok tracking_tighter
  | [ "tracking"; "tight" ] -> Ok tracking_tight
  | [ "tracking"; "normal" ] -> Ok tracking_normal
  | [ "tracking"; "wide" ] -> Ok tracking_wide
  | [ "tracking"; "wider" ] -> Ok tracking_wider
  | [ "tracking"; "widest" ] -> Ok tracking_widest
  | [ "uppercase" ] -> Ok uppercase
  | [ "lowercase" ] -> Ok lowercase
  | [ "capitalize" ] -> Ok capitalize
  | [ "normal"; "case" ] -> Ok normal_case
  | _ -> Error (`Msg "Not a typography utility")
