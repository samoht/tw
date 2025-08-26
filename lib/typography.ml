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
module Parse = Parse

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

(** Helper to create font weight utilities with proper variable dependencies *)
let font_weight name weight_value =
  let var_name =
    "--font-weight-" ^ String.sub name 5 (String.length name - 5)
  in
  (* Remove "font-" prefix *)
  let var_name_without_prefix =
    String.sub var_name 2 (String.length var_name - 2)
  in
  (* Remove "--" *)
  style name
    [
      custom_property ~deps:[ var_name ] "--tw-font-weight"
        ("var(" ^ var_name ^ ")");
      Css.font_weight
        (Var (var ~default:(Weight weight_value) var_name_without_prefix));
    ]

let font_thin = font_weight "font-thin" 100
let font_extralight = font_weight "font-extralight" 200
let font_light = font_weight "font-light" 300
let font_normal = font_weight "font-normal" 400
let font_medium = font_weight "font-medium" 500
let font_semibold = font_weight "font-semibold" 600
let font_bold = font_weight "font-bold" 700
let font_extrabold = font_weight "font-extrabold" 800
let font_black = font_weight "font-black" 900

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
let overline = style "overline" [ text_decoration Overline ]
let line_through = style "line-through" [ text_decoration Line_through ]
let no_underline = style "no-underline" [ text_decoration None ]
let underline_solid = style "underline-solid" [ text_decoration_style Solid ]
let underline_double = style "underline-double" [ text_decoration_style Double ]
let underline_dotted = style "underline-dotted" [ text_decoration_style Dotted ]
let underline_dashed = style "underline-dashed" [ text_decoration_style Dashed ]
let underline_wavy = style "underline-wavy" [ text_decoration_style Wavy ]

(** {1 Text Decoration Color & Thickness} *)

let decoration_color ?(shade = 500) (color : Color.color) =
  let class_name =
    if Color.is_base_color color then Pp.str [ "decoration-"; Color.pp color ]
    else Pp.str [ "decoration-"; Color.pp color; "-"; string_of_int shade ]
  in
  style class_name [ text_decoration_color (Color.to_css color shade) ]

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

let list_none = style "list-none" [ list_style "none" ]
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
  style class_name
    [
      text_indent
        (Calc (Expr (Calc.var "spacing", Mult, Calc.float (float_of_int n))));
    ]

(** {1 Line Clamp} *)

let line_clamp n =
  if n = 0 then
    style "line-clamp-0"
      [ webkit_line_clamp 0; overflow Visible; display Block ]
  else
    let class_name = "line-clamp-" ^ string_of_int n in
    style class_name [ webkit_line_clamp n; overflow Hidden; display Block ]

(** {1 Content} *)

let content_none = style "content-none" [ content "none" ]

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
  style class_name [ content quoted ]

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
  style "break-normal" [ word_break Normal; overflow_wrap Normal_wrap ]

let break_words = style "break-words" [ overflow_wrap Break_word_wrap ]
let break_all = style "break-all" [ word_break Break_all ]
let break_keep = style "break-keep" [ word_break Keep_all ]

let overflow_wrap_normal =
  style "overflow-wrap-normal" [ overflow_wrap Normal_wrap ]

let overflow_wrap_anywhere =
  style "overflow-wrap-anywhere" [ overflow_wrap Anywhere ]

let overflow_wrap_break_word =
  style "overflow-wrap-break-word" [ overflow_wrap Break_word_wrap ]

(** {1 Hyphens} *)

let hyphens_none = style "hyphens-none" [ hyphens None_h ]
let hyphens_manual = style "hyphens-manual" [ hyphens Manual ]
let hyphens_auto = style "hyphens-auto" [ hyphens Auto ]

(** {1 Font Stretch} *)

let font_stretch_normal = style "font-stretch-normal" [ font_stretch Normal ]

let font_stretch_condensed =
  style "font-stretch-condensed" [ font_stretch Condensed ]

let font_stretch_expanded =
  style "font-stretch-expanded" [ font_stretch Expanded ]

let font_stretch_percent n =
  style
    ("font-stretch-" ^ string_of_int n)
    [ font_stretch (Percentage (float_of_int n)) ]

(** {1 Numeric Variants} *)

let normal_nums =
  style "normal-nums" [ font_variant_numeric [ Normal_numeric ] ]

let ordinal = style "ordinal" [ font_variant_numeric [ Ordinal ] ]

let slashed_zero =
  style "slashed-zero" [ font_variant_numeric [ Slashed_zero ] ]

let lining_nums = style "lining-nums" [ font_variant_numeric [ Lining_nums ] ]

let oldstyle_nums =
  style "oldstyle-nums" [ font_variant_numeric [ Oldstyle_nums ] ]

let proportional_nums =
  style "proportional-nums" [ font_variant_numeric [ Proportional_nums ] ]

let tabular_nums =
  style "tabular-nums" [ font_variant_numeric [ Tabular_nums ] ]

let diagonal_fractions =
  style "diagonal-fractions" [ font_variant_numeric [ Diagonal_fractions ] ]

let stacked_fractions =
  style "stacked-fractions" [ font_variant_numeric [ Stacked_fractions ] ]

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
