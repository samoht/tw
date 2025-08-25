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

(** Prose variant types *)
type t =
  | Base  (** .prose *)
  | Sm  (** .prose-sm *)
  | Lg  (** .prose-lg *)
  | Xl  (** .prose-xl *)
  | Xl2  (** .prose-2xl *)
  | Gray  (** .prose-gray *)
  | Slate  (** .prose-slate *)

(** Default prose CSS variables for theming *)
let css_variables =
  [
    Css.custom_property "--tw-prose-body" "rgb(55 65 81)";
    Css.custom_property "--tw-prose-headings" "rgb(17 24 39)";
    Css.custom_property "--tw-prose-lead" "rgb(75 85 99)";
    Css.custom_property "--tw-prose-links" "rgb(17 24 39)";
    Css.custom_property "--tw-prose-bold" "rgb(17 24 39)";
    Css.custom_property "--tw-prose-counters" "rgb(107 114 128)";
    Css.custom_property "--tw-prose-bullets" "rgb(209 213 219)";
    Css.custom_property "--tw-prose-hr" "rgb(229 231 235)";
    Css.custom_property "--tw-prose-quotes" "rgb(17 24 39)";
    Css.custom_property "--tw-prose-quote-borders" "rgb(229 231 235)";
    Css.custom_property "--tw-prose-captions" "rgb(107 114 128)";
    Css.custom_property "--tw-prose-code" "rgb(17 24 39)";
    Css.custom_property "--tw-prose-pre-code" "rgb(229 231 235)";
    Css.custom_property "--tw-prose-pre-bg" "rgb(55 65 81)";
    Css.custom_property "--tw-prose-th-borders" "rgb(209 213 219)";
    Css.custom_property "--tw-prose-td-borders" "rgb(229 231 235)";
  ]

(** Convert prose variant to CSS class name *)
let to_class = function
  | Base -> "prose"
  | Sm -> "prose-sm"
  | Lg -> "prose-lg"
  | Xl -> "prose-xl"
  | Xl2 -> "prose-2xl"
  | Gray -> "prose-gray"
  | Slate -> "prose-slate"

(** Generate CSS rules for a prose variant *)
(* Helper functions for generating CSS rules *)

(* Heading styles for base prose *)
let base_heading_rules selector =
  [
    rule ~selector:(selector " h1")
      [
        color (Var "tw-prose-headings");
        font_weight (Weight 800);
        font_size (Em 2.25);
        margin_top Zero;
        margin_bottom (Em 0.8888889);
        line_height (Num 1.1111111);
      ];
    rule ~selector:(selector " h2")
      [
        color (Var "tw-prose-headings");
        font_weight (Weight 700);
        font_size (Em 1.5);
        margin_top (Em 2.0);
        margin_bottom (Em 1.0);
        line_height (Num 1.3333333);
      ];
    rule ~selector:(selector " h3")
      [
        color (Var "tw-prose-headings");
        font_weight (Weight 600);
        font_size (Em 1.25);
        margin_top (Em 1.6);
        margin_bottom (Em 0.6);
        line_height (Num 1.6);
      ];
    rule ~selector:(selector " h4")
      [
        color (Var "tw-prose-headings");
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
        color (Var "tw-prose-code");
        font_weight (Weight 600);
        font_size (Em 0.875);
      ];
    rule ~selector:(selector " code::before") [ content "\"\\`\"" ];
    rule ~selector:(selector " code::after") [ content "\"\\`\"" ];
    rule ~selector:(selector " pre")
      [
        color (Var "tw-prose-pre-code");
        background_color (Var "tw-prose-pre-bg");
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
      [
        border_bottom_width (Px 1);
        border_bottom_color (Var "tw-prose-th-borders");
      ];
    rule ~selector:(selector " thead th")
      [
        color (Var "tw-prose-headings");
        font_weight (Weight 600);
        vertical_align Bottom;
        padding_right (Em 0.5714286);
        padding_bottom (Em 0.5714286);
        padding_left (Em 0.5714286);
      ];
    rule ~selector:(selector " tbody tr")
      [
        border_bottom_width (Px 1);
        border_bottom_color (Var "tw-prose-td-borders");
      ];
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
        color (Var "tw-prose-links");
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
        color (Var "tw-prose-quotes");
        border_left_width (Rem 0.25);
        border_left_color (Var "tw-prose-quote-borders");
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
        border_color (Var "tw-prose-hr");
        border_top_width (Px 1);
        margin_top (Em 3.0);
        margin_bottom (Em 3.0);
      ];
    rule ~selector:(selector " strong")
      [ color (Var "tw-prose-bold"); font_weight (Weight 600) ];
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
        Css.custom_property "--tw-prose-body" "rgb(107 114 128)";
        Css.custom_property "--tw-prose-headings" "rgb(31 41 55)";
        Css.custom_property "--tw-prose-lead" "rgb(75 85 99)";
        Css.custom_property "--tw-prose-links" "rgb(31 41 55)";
        Css.custom_property "--tw-prose-bold" "rgb(31 41 55)";
        Css.custom_property "--tw-prose-counters" "rgb(107 114 128)";
        Css.custom_property "--tw-prose-bullets" "rgb(209 213 219)";
        Css.custom_property "--tw-prose-hr" "rgb(229 231 235)";
        Css.custom_property "--tw-prose-quotes" "rgb(31 41 55)";
        Css.custom_property "--tw-prose-quote-borders" "rgb(229 231 235)";
        Css.custom_property "--tw-prose-captions" "rgb(156 163 175)";
        Css.custom_property "--tw-prose-code" "rgb(31 41 55)";
        Css.custom_property "--tw-prose-pre-code" "rgb(229 231 235)";
        Css.custom_property "--tw-prose-pre-bg" "rgb(31 41 55)";
        Css.custom_property "--tw-prose-th-borders" "rgb(209 213 219)";
        Css.custom_property "--tw-prose-td-borders" "rgb(229 231 235)";
      ];
  ]

let slate_color_rules =
  [
    rule ~selector:".prose-slate"
      [
        Css.custom_property "--tw-prose-body" "rgb(100 116 139)";
        Css.custom_property "--tw-prose-headings" "rgb(15 23 42)";
        Css.custom_property "--tw-prose-lead" "rgb(71 85 105)";
        Css.custom_property "--tw-prose-links" "rgb(15 23 42)";
        Css.custom_property "--tw-prose-bold" "rgb(15 23 42)";
        Css.custom_property "--tw-prose-counters" "rgb(100 116 139)";
        Css.custom_property "--tw-prose-bullets" "rgb(203 213 225)";
        Css.custom_property "--tw-prose-hr" "rgb(226 232 240)";
        Css.custom_property "--tw-prose-quotes" "rgb(15 23 42)";
        Css.custom_property "--tw-prose-quote-borders" "rgb(226 232 240)";
        Css.custom_property "--tw-prose-captions" "rgb(148 163 184)";
        Css.custom_property "--tw-prose-code" "rgb(15 23 42)";
        Css.custom_property "--tw-prose-pre-code" "rgb(226 232 240)";
        Css.custom_property "--tw-prose-pre-bg" "rgb(30 41 59)";
        Css.custom_property "--tw-prose-th-borders" "rgb(203 213 225)";
        Css.custom_property "--tw-prose-td-borders" "rgb(226 232 240)";
      ];
  ]

let base_prose_rules selector =
  (* Base prose element with CSS variables *)
  let base_rule =
    rule ~selector:".prose"
      ([
         color (Var "tw-prose-body");
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
  | Base -> base_prose_rules selector
  | Sm -> sm_size_rules selector
  | Lg -> lg_size_rules selector
  | Xl -> xl_size_rules selector
  | Xl2 -> xl2_size_rules selector
  | Gray -> gray_color_rules
  | Slate -> slate_color_rules

(** Get base CSS properties for inline styles *)
let to_base_properties variant =
  (* Extract the base properties for each variant *)
  match variant with
  | Base ->
      [
        color (Var "tw-prose-body");
        max_width (Ch 65.0);
        font_size (Rem 1.0);
        line_height (Num 1.75);
      ]
  | Sm -> [ font_size (Rem 0.875); line_height (Num 1.7142857) ]
  | Lg -> [ font_size (Rem 1.125); line_height (Num 1.7777778) ]
  | Xl -> [ font_size (Rem 1.25); line_height (Num 1.8) ]
  | Xl2 -> [ font_size (Rem 1.5); line_height (Num 1.6666667) ]
  | Gray | Slate -> [] (* Color themes only affect CSS variables *)

let pp = function
  | Base -> "Base"
  | Sm -> "Sm"
  | Lg -> "Lg"
  | Xl -> "Xl"
  | Xl2 -> "Xl2"
  | Gray -> "Gray"
  | Slate -> "Slate"
