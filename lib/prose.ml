(** Prose typography utilities

    What's included:
    - Pre-styled typography rules for long-form content with sensible defaults
      (headings, paragraphs, lists, blockquotes, code, tables) and theme
      variables (`--tw-prose-*`).

    What's not:
    - Arbitrary prose variants beyond the included base and size/color themes.
      Extend by composing your own rules or adding variants to this module.

    Notes:
    - Use `Prose.to_css_rules` to emit rules for a variant list.
    - Theme via CSS custom properties exposed in [css_variables]. *)

open Css

(** Helper: define a color custom property by name (without leading --) *)
let color_prop (var : Css.color Var.t) r g b =
  let def, _ = Var.utility var (Rgb { r; g; b }) in
  def

(* Prose variant type *)
type variant = [ `Base | `Sm | `Lg | `Xl | `Xl2 | `Gray | `Slate ]

(* Create variables for reuse *)

(** Default prose CSS variables for theming *)
let prose_body_def, prose_body_var =
  Var.utility Var.Prose_body (Rgb { r = 55; g = 65; b = 81 })

let prose_headings_def, prose_headings_var =
  Var.utility Var.Prose_headings (Rgb { r = 17; g = 24; b = 39 })

let _prose_code_def, prose_code_var =
  Var.utility Var.Prose_code (Rgb { r = 17; g = 24; b = 39 })

let _prose_pre_code_def, prose_pre_code_var =
  Var.utility Var.Prose_pre_code (Rgb { r = 229; g = 231; b = 235 })

let _prose_pre_bg_def, prose_pre_bg_var =
  Var.utility Var.Prose_pre_bg (Rgb { r = 31; g = 41; b = 55 })

let _prose_th_borders_def, prose_th_borders_var =
  Var.utility Var.Prose_th_borders (Rgb { r = 209; g = 213; b = 219 })

let _prose_td_borders_def, prose_td_borders_var =
  Var.utility Var.Prose_td_borders (Rgb { r = 229; g = 231; b = 235 })

let _prose_links_def, prose_links_var =
  Var.utility Var.Prose_links (Rgb { r = 17; g = 24; b = 39 })

let _prose_quotes_def, prose_quotes_var =
  Var.utility Var.Prose_quotes (Rgb { r = 107; g = 114; b = 128 })

let _prose_quote_borders_def, prose_quote_borders_var =
  Var.utility Var.Prose_quote_borders (Rgb { r = 229; g = 231; b = 235 })

let _prose_hr_def, prose_hr_var =
  Var.utility Var.Prose_hr (Rgb { r = 229; g = 231; b = 235 })

let _prose_bold_def, prose_bold_var =
  Var.utility Var.Prose_bold (Rgb { r = 17; g = 24; b = 39 })

let css_variables =
  [
    (* Base variables with handles *)
    prose_body_def;
    prose_headings_def;
    _prose_code_def;
    _prose_pre_code_def;
    _prose_pre_bg_def;
    _prose_th_borders_def;
    _prose_td_borders_def;
    _prose_links_def;
    _prose_quotes_def;
    _prose_quote_borders_def;
    _prose_hr_def;
    _prose_bold_def;
    (* Additional variables used in prose without explicit handles *)
    color_prop Var.Prose_lead 75 85 99;
    color_prop Var.Prose_counters 107 114 128;
    color_prop Var.Prose_bullets 209 213 219;
    color_prop Var.Prose_captions 107 114 128;
  ]

(** Convert prose variant to CSS class name *)
let to_class = function
  | `Base -> "prose"
  | `Sm -> "prose-sm"
  | `Lg -> "prose-lg"
  | `Xl -> "prose-xl"
  | `Xl2 -> "prose-2xl"
  | `Gray -> "prose-gray"
  | `Slate -> "prose-slate"

(** Generate CSS rules for a prose variant *)
(* Helper functions for generating CSS rules *)

(* Heading styles for base prose *)
let base_heading_rules selector =
  [
    rule ~selector:(selector " h1")
      [
        color (Var prose_headings_var);
        font_weight (Weight 800);
        font_size (Em 2.25);
        margin_top Zero;
        margin_bottom (Em 0.8888889);
        line_height (Num 1.1111111);
      ];
    rule ~selector:(selector " h2")
      [
        color (Var prose_headings_var);
        font_weight (Weight 700);
        font_size (Em 1.5);
        margin_top (Em 2.0);
        margin_bottom (Em 1.0);
        line_height (Num 1.3333333);
      ];
    rule ~selector:(selector " h3")
      [
        color (Var prose_headings_var);
        font_weight (Weight 600);
        font_size (Em 1.25);
        margin_top (Em 1.6);
        margin_bottom (Em 0.6);
        line_height (Num 1.6);
      ];
    rule ~selector:(selector " h4")
      [
        color (Var prose_headings_var);
        font_weight (Weight 600);
        margin_top (Em 1.5);
        margin_bottom (Em 0.5);
        line_height (Num 1.5);
      ];
  ]

(* List and paragraph styles *)
let base_text_rules selector =
  [
    rule ~selector:(selector " p")
      [ margin_top (Em 1.25); margin_bottom (Em 1.25) ];
    rule ~selector:(selector " ol")
      [
        list_style_type Decimal;
        margin_top (Em 1.25);
        margin_bottom (Em 1.25);
        padding_left (Em 1.625);
      ];
    rule ~selector:(selector " ul")
      [
        list_style_type Disc;
        margin_top (Em 1.25);
        margin_bottom (Em 1.25);
        padding_left (Em 1.625);
      ];
    rule ~selector:(selector " li")
      [ margin_top (Em 0.5); margin_bottom (Em 0.5) ];
  ]

(* Code and pre styles *)
let base_code_rules selector =
  [
    rule ~selector:(selector " code")
      [
        color (Var prose_code_var);
        font_weight (Weight 600);
        font_size (Em 0.875);
      ];
    rule ~selector:(selector " code::before") [ content "\"\\`\"" ];
    rule ~selector:(selector " code::after") [ content "\"\\`\"" ];
    rule ~selector:(selector " pre")
      [
        color (Var prose_pre_code_var);
        background_color (Var prose_pre_bg_var);
        overflow_x Auto;
        font_weight (Weight 400);
        font_size (Em 0.875);
        line_height (Num 1.7142857);
        margin_top (Em 1.7142857);
        margin_bottom (Em 1.7142857);
        border_radius (Rem 0.375);
        padding_top (Em 0.8571429);
        padding_right (Em 1.1428571);
        padding_bottom (Em 0.8571429);
        padding_left (Em 1.1428571);
      ];
    rule ~selector:(selector " pre code")
      [
        background_color Transparent;
        border_width Zero;
        border_radius Zero;
        padding Zero;
        font_weight Inherit;
        color Inherit;
        font_size Inherit;
        font_family [ Inherit ];
        line_height Inherit;
      ];
    rule ~selector:(selector " pre code::before") [ content "none" ];
    rule ~selector:(selector " pre code::after") [ content "none" ];
  ]

(* Table styles *)
let base_table_rules selector =
  [
    rule ~selector:(selector " table")
      [
        width (Pct 100.0);
        table_layout Auto;
        text_align Left;
        margin_top (Em 2.0);
        margin_bottom (Em 2.0);
        font_size (Em 0.875);
        line_height (Num 1.7142857);
      ];
    rule ~selector:(selector " thead")
      [ border_bottom_width (Px 1); border_color (Var prose_th_borders_var) ];
    rule ~selector:(selector " thead th")
      [
        color (Var prose_headings_var);
        font_weight (Weight 600);
        vertical_align Bottom;
        padding_right (Em 0.5714286);
        padding_bottom (Em 0.5714286);
        padding_left (Em 0.5714286);
      ];
    rule ~selector:(selector " tbody tr")
      [ border_bottom_width (Px 1); border_color (Var prose_td_borders_var) ];
    rule
      ~selector:(selector " tbody tr:last-child")
      [ border_bottom_width Zero ];
    rule ~selector:(selector " tbody td")
      [
        vertical_align Baseline;
        padding_top (Em 0.5714286);
        padding_right (Em 0.5714286);
        padding_bottom (Em 0.5714286);
        padding_left (Em 0.5714286);
      ];
  ]

(* Blockquote and other styles *)
let base_misc_rules selector =
  [
    rule ~selector:(selector " a")
      [
        color (Var prose_links_var);
        text_decoration None;
        font_weight (Weight 500);
        transition (Simple (Property "color", S 0.2));
      ];
    rule ~selector:(selector " a:hover")
      [ color (Rgb { r = 37; g = 99; b = 235 }) ];
    rule ~selector:(selector " blockquote")
      [
        font_weight (Weight 500);
        font_style Italic;
        color (Var prose_quotes_var);
        border_left_width (Rem 0.25);
        border_color (Var prose_quote_borders_var);
        quotes "\"\\201C\"\"\\201D\"\"\\2018\"\"\\2019\"";
        margin_top (Em 1.6);
        margin_bottom (Em 1.6);
        padding_left (Em 1.0);
      ];
    rule
      ~selector:(selector " blockquote p:first-of-type::before")
      [ content "open-quote" ];
    rule
      ~selector:(selector " blockquote p:last-of-type::after")
      [ content "close-quote" ];
    rule ~selector:(selector " hr")
      [
        border_color (Var prose_hr_var);
        border_top_width (Px 1);
        margin_top (Em 3.0);
        margin_bottom (Em 3.0);
      ];
    rule ~selector:(selector " strong")
      [ color (Var prose_bold_var); font_weight (Weight 600) ];
    rule ~selector:(selector " img")
      [ margin_top (Em 2.0); margin_bottom (Em 2.0) ];
  ]

let sm_size_rules selector =
  [
    rule ~selector:".prose-sm"
      [ font_size (Rem 0.875); line_height (Num 1.7142857) ];
    rule ~selector:(selector " p")
      [ margin_top (Em 1.1428571); margin_bottom (Em 1.1428571) ];
    rule ~selector:(selector " h1")
      [
        font_size (Em 2.1428571);
        margin_top Zero;
        margin_bottom (Em 0.8);
        line_height (Num 1.2);
      ];
    rule ~selector:(selector " h2")
      [
        font_size (Em 1.4285714);
        margin_top (Em 1.6);
        margin_bottom (Em 0.8);
        line_height (Num 1.4);
      ];
    rule ~selector:(selector " h3")
      [
        font_size (Em 1.2857143);
        margin_top (Em 1.5555556);
        margin_bottom (Em 0.4444444);
        line_height (Num 1.5555556);
      ];
  ]

let lg_size_rules selector =
  [
    rule ~selector:".prose-lg"
      [ font_size (Rem 1.125); line_height (Num 1.7777778) ];
    rule ~selector:(selector " p")
      [ margin_top (Em 1.3333333); margin_bottom (Em 1.3333333) ];
    rule ~selector:(selector " h1")
      [
        font_size (Em 2.6666667);
        margin_top Zero;
        margin_bottom (Em 0.8333333);
        line_height (Num 1.0);
      ];
    rule ~selector:(selector " h2")
      [
        font_size (Em 2.0);
        margin_top (Em 1.5555556);
        margin_bottom (Em 0.8888889);
        line_height (Num 1.3333333);
      ];
    rule ~selector:(selector " h3")
      [
        font_size (Em 1.6666667);
        margin_top (Em 1.6666667);
        margin_bottom (Em 0.6666667);
        line_height (Num 1.5);
      ];
  ]

let xl_size_rules selector =
  [
    rule ~selector:".prose-xl" [ font_size (Rem 1.25); line_height (Num 1.8) ];
    rule ~selector:(selector " p")
      [ margin_top (Em 1.2); margin_bottom (Em 1.2) ];
    rule ~selector:(selector " h1")
      [
        font_size (Em 2.8);
        margin_top Zero;
        margin_bottom (Em 0.8571429);
        line_height (Num 1.0);
      ];
    rule ~selector:(selector " h2")
      [
        font_size (Em 2.0);
        margin_top (Em 1.4);
        margin_bottom (Em 0.8);
        line_height (Num 1.2727273);
      ];
    rule ~selector:(selector " h3")
      [
        font_size (Em 1.6);
        margin_top (Em 1.5555556);
        margin_bottom (Em 0.6666667);
        line_height (Num 1.4444444);
      ];
  ]

let xl2_size_rules selector =
  [
    rule ~selector:".prose-2xl"
      [ font_size (Rem 1.5); line_height (Num 1.6666667) ];
    rule ~selector:(selector " p")
      [ margin_top (Em 1.3333333); margin_bottom (Em 1.3333333) ];
    rule ~selector:(selector " h1")
      [
        font_size (Em 2.6666667);
        margin_top Zero;
        margin_bottom (Em 0.875);
        line_height (Num 1.0);
      ];
    rule ~selector:(selector " h2")
      [
        font_size (Em 2.0);
        margin_top (Em 1.5);
        margin_bottom (Em 0.8333333);
        line_height (Num 1.0833333);
      ];
    rule ~selector:(selector " h3")
      [
        font_size (Em 1.5);
        margin_top (Em 1.5555556);
        margin_bottom (Em 0.6666667);
        line_height (Num 1.2222222);
      ];
  ]

let gray_color_rules =
  [
    rule ~selector:".prose-gray"
      [
        color_prop Var.Prose_body 107 114 128;
        color_prop Var.Prose_headings 31 41 55;
        color_prop Var.Prose_lead 75 85 99;
        color_prop Var.Prose_links 31 41 55;
        color_prop Var.Prose_bold 31 41 55;
        color_prop Var.Prose_counters 107 114 128;
        color_prop Var.Prose_bullets 209 213 219;
        color_prop Var.Prose_hr 229 231 235;
        color_prop Var.Prose_quotes 31 41 55;
        color_prop Var.Prose_quote_borders 229 231 235;
        color_prop Var.Prose_captions 156 163 175;
        color_prop Var.Prose_code 31 41 55;
        color_prop Var.Prose_pre_code 229 231 235;
        color_prop Var.Prose_pre_bg 31 41 55;
        color_prop Var.Prose_th_borders 209 213 219;
        color_prop Var.Prose_td_borders 229 231 235;
      ];
  ]

let slate_color_rules =
  [
    rule ~selector:".prose-slate"
      [
        color_prop Var.Prose_body 100 116 139;
        color_prop Var.Prose_headings 15 23 42;
        color_prop Var.Prose_lead 71 85 105;
        color_prop Var.Prose_links 15 23 42;
        color_prop Var.Prose_bold 15 23 42;
        color_prop Var.Prose_counters 100 116 139;
        color_prop Var.Prose_bullets 203 213 225;
        color_prop Var.Prose_hr 226 232 240;
        color_prop Var.Prose_quotes 15 23 42;
        color_prop Var.Prose_quote_borders 226 232 240;
        color_prop Var.Prose_captions 148 163 184;
        color_prop Var.Prose_code 15 23 42;
        color_prop Var.Prose_pre_code 226 232 240;
        color_prop Var.Prose_pre_bg 30 41 59;
        color_prop Var.Prose_th_borders 203 213 225;
        color_prop Var.Prose_td_borders 226 232 240;
      ];
  ]

let base_prose_rules selector =
  (* Base prose element with CSS variables *)
  let base_rule =
    rule ~selector:".prose"
      ([
         color (Var prose_body_var);
         max_width (Ch 65.0);
         font_size (Rem 1.0);
         line_height (Num 1.75);
       ]
      @ css_variables)
  in

  (* Combine all the rules *)
  [ base_rule ]
  @ base_heading_rules selector
  @ base_text_rules selector @ base_code_rules selector
  @ base_table_rules selector @ base_misc_rules selector

let to_css_rules variant =
  let class_name = to_class variant in
  let selector prefix = "." ^ class_name ^ prefix in

  match variant with
  | `Base -> base_prose_rules selector
  | `Sm -> sm_size_rules selector
  | `Lg -> lg_size_rules selector
  | `Xl -> xl_size_rules selector
  | `Xl2 -> xl2_size_rules selector
  | `Gray -> gray_color_rules
  | `Slate -> slate_color_rules

(** Get base CSS properties for inline styles *)
let to_base_properties variant =
  (* Extract the base properties for each variant *)
  match variant with
  | `Base ->
      [
        color (Var prose_body_var);
        max_width (Ch 65.0);
        font_size (Rem 1.0);
        line_height (Num 1.75);
      ]
  | `Sm -> [ font_size (Rem 0.875); line_height (Num 1.7142857) ]
  | `Lg -> [ font_size (Rem 1.125); line_height (Num 1.7777778) ]
  | `Xl -> [ font_size (Rem 1.25); line_height (Num 1.8) ]
  | `Xl2 -> [ font_size (Rem 1.5); line_height (Num 1.6666667) ]
  | `Gray | `Slate -> [] (* Color themes only affect CSS variables *)

let pp = function
  | `Base -> "Base"
  | `Sm -> "Sm"
  | `Lg -> "Lg"
  | `Xl -> "Xl"
  | `Xl2 -> "Xl2"
  | `Gray -> "Gray"
  | `Slate -> "Slate"

(** Prose utility constructors *)
let prose_style variant =
  let name = to_class variant in
  let rules = to_css_rules variant in
  let props = to_base_properties variant in
  Core.style ~rules:(Some rules) name props

let prose = prose_style `Base
let prose_sm = prose_style `Sm
let prose_lg = prose_style `Lg
let prose_xl = prose_style `Xl
let prose_2xl = prose_style `Xl2
let prose_gray = prose_style `Gray
let prose_slate = prose_style `Slate

(** Parse prose utilities from string *)
let of_string = function
  | [ "prose" ] -> Ok prose
  | [ "prose"; "sm" ] -> Ok prose_sm
  | [ "prose"; "lg" ] -> Ok prose_lg
  | [ "prose"; "xl" ] -> Ok prose_xl
  | [ "prose"; "2xl" ] -> Ok prose_2xl
  | [ "prose"; "gray" ] -> Ok prose_gray
  | [ "prose"; "slate" ] -> Ok prose_slate
  | _ -> Error (`Msg "Not a prose utility")

(** Generate complete prose stylesheet *)
let stylesheet () =
  (* Generate CSS for all prose variants *)
  let base_rules = to_css_rules `Base in
  let sm_rules = to_css_rules `Sm in
  let lg_rules = to_css_rules `Lg in
  let xl_rules = to_css_rules `Xl in
  let xl2_rules = to_css_rules `Xl2 in
  let gray_rules = to_css_rules `Gray in
  let slate_rules = to_css_rules `Slate in

  (* Combine all rules *)
  let all_rules =
    List.concat
      [
        base_rules;
        sm_rules;
        lg_rules;
        xl_rules;
        xl2_rules;
        gray_rules;
        slate_rules;
      ]
  in
  (* Return raw rules; caller composes into a stylesheet if needed *)
  all_rules
