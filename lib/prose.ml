(** Prose typography utilities *)

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
    property "--tw-prose-body" "rgb(55 65 81)";
    property "--tw-prose-headings" "rgb(17 24 39)";
    property "--tw-prose-lead" "rgb(75 85 99)";
    property "--tw-prose-links" "rgb(17 24 39)";
    property "--tw-prose-bold" "rgb(17 24 39)";
    property "--tw-prose-counters" "rgb(107 114 128)";
    property "--tw-prose-bullets" "rgb(209 213 219)";
    property "--tw-prose-hr" "rgb(229 231 235)";
    property "--tw-prose-quotes" "rgb(17 24 39)";
    property "--tw-prose-quote-borders" "rgb(229 231 235)";
    property "--tw-prose-captions" "rgb(107 114 128)";
    property "--tw-prose-code" "rgb(17 24 39)";
    property "--tw-prose-pre-code" "rgb(229 231 235)";
    property "--tw-prose-pre-bg" "rgb(55 65 81)";
    property "--tw-prose-th-borders" "rgb(209 213 219)";
    property "--tw-prose-td-borders" "rgb(229 231 235)";
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
        color "var(--tw-prose-headings)";
        font_weight "800";
        font_size "2.25em";
        margin_top "0";
        margin_bottom "0.8888889em";
        line_height "1.1111111";
      ];
    rule ~selector:(selector " h2")
      [
        color "var(--tw-prose-headings)";
        font_weight "700";
        font_size "1.5em";
        margin_top "2em";
        margin_bottom "1em";
        line_height "1.3333333";
      ];
    rule ~selector:(selector " h3")
      [
        color "var(--tw-prose-headings)";
        font_weight "600";
        font_size "1.25em";
        margin_top "1.6em";
        margin_bottom "0.6em";
        line_height "1.6";
      ];
    rule ~selector:(selector " h4")
      [
        color "var(--tw-prose-headings)";
        font_weight "600";
        margin_top "1.5em";
        margin_bottom "0.5em";
        line_height "1.5";
      ];
  ]

(* List and paragraph styles *)
let base_text_rules selector =
  [
    rule ~selector:(selector " p")
      [ margin_top "1.25em"; margin_bottom "1.25em" ];
    rule ~selector:(selector " ol")
      [
        property "list-style-type" "decimal";
        margin_top "1.25em";
        margin_bottom "1.25em";
        padding_left "1.625em";
      ];
    rule ~selector:(selector " ul")
      [
        property "list-style-type" "disc";
        margin_top "1.25em";
        margin_bottom "1.25em";
        padding_left "1.625em";
      ];
    rule ~selector:(selector " li")
      [ margin_top "0.5em"; margin_bottom "0.5em" ];
  ]

(* Code and pre styles *)
let base_code_rules selector =
  [
    rule ~selector:(selector " code")
      [ color "var(--tw-prose-code)"; font_weight "600"; font_size "0.875em" ];
    rule ~selector:(selector " code::before") [ property "content" "\"\\`\"" ];
    rule ~selector:(selector " code::after") [ property "content" "\"\\`\"" ];
    rule ~selector:(selector " pre")
      [
        color "var(--tw-prose-pre-code)";
        background_color "var(--tw-prose-pre-bg)";
        overflow_x "auto";
        font_weight "400";
        font_size "0.875em";
        line_height "1.7142857";
        margin_top "1.7142857em";
        margin_bottom "1.7142857em";
        border_radius "0.375rem";
        padding_top "0.8571429em";
        padding_right "1.1428571em";
        padding_bottom "0.8571429em";
        padding_left "1.1428571em";
      ];
    rule ~selector:(selector " pre code")
      [
        background_color "transparent";
        border_width "0";
        border_radius "0";
        padding "0";
        font_weight "inherit";
        color "inherit";
        font_size "inherit";
        font_family "inherit";
        line_height "inherit";
      ];
    rule ~selector:(selector " pre code::before") [ property "content" "none" ];
    rule ~selector:(selector " pre code::after") [ property "content" "none" ];
  ]

(* Table styles *)
let base_table_rules selector =
  [
    rule ~selector:(selector " table")
      [
        width "100%";
        table_layout "auto";
        text_align "left";
        margin_top "2em";
        margin_bottom "2em";
        font_size "0.875em";
        line_height "1.7142857";
      ];
    rule ~selector:(selector " thead")
      [
        border_bottom_width "1px";
        property "border-bottom-color" "var(--tw-prose-th-borders)";
      ];
    rule ~selector:(selector " thead th")
      [
        color "var(--tw-prose-headings)";
        font_weight "600";
        vertical_align "bottom";
        padding_right "0.5714286em";
        padding_bottom "0.5714286em";
        padding_left "0.5714286em";
      ];
    rule ~selector:(selector " tbody tr")
      [
        border_bottom_width "1px";
        property "border-bottom-color" "var(--tw-prose-td-borders)";
      ];
    rule ~selector:(selector " tbody tr:last-child") [ border_bottom_width "0" ];
    rule ~selector:(selector " tbody td")
      [
        vertical_align "baseline";
        padding_top "0.5714286em";
        padding_right "0.5714286em";
        padding_bottom "0.5714286em";
        padding_left "0.5714286em";
      ];
  ]

(* Blockquote and other styles *)
let base_misc_rules selector =
  [
    rule ~selector:(selector " a")
      [
        color "var(--tw-prose-links)";
        text_decoration "none";
        font_weight "500";
        property "transition" "color 0.2s ease";
      ];
    rule ~selector:(selector " a:hover") [ color "rgb(37 99 235)" ];
    rule ~selector:(selector " blockquote")
      [
        font_weight "500";
        font_style "italic";
        color "var(--tw-prose-quotes)";
        property "border-left-width" "0.25rem";
        property "border-left-color" "var(--tw-prose-quote-borders)";
        property "quotes" "\"\\201C\"\"\\201D\"\"\\2018\"\"\\2019\"";
        margin_top "1.6em";
        margin_bottom "1.6em";
        padding_left "1em";
      ];
    rule
      ~selector:(selector " blockquote p:first-of-type::before")
      [ property "content" "open-quote" ];
    rule
      ~selector:(selector " blockquote p:last-of-type::after")
      [ property "content" "close-quote" ];
    rule ~selector:(selector " hr")
      [
        property "border-color" "var(--tw-prose-hr)";
        border_top_width "1px";
        margin_top "3em";
        margin_bottom "3em";
      ];
    rule ~selector:(selector " strong")
      [ color "var(--tw-prose-bold)"; font_weight "600" ];
    rule ~selector:(selector " img") [ margin_top "2em"; margin_bottom "2em" ];
  ]

let sm_size_rules selector =
  [
    rule ~selector:".prose-sm" [ font_size "0.875rem"; line_height "1.7142857" ];
    rule ~selector:(selector " p")
      [ margin_top "1.1428571em"; margin_bottom "1.1428571em" ];
    rule ~selector:(selector " h1")
      [
        font_size "2.1428571em";
        margin_top "0";
        margin_bottom "0.8em";
        line_height "1.2";
      ];
    rule ~selector:(selector " h2")
      [
        font_size "1.4285714em";
        margin_top "1.6em";
        margin_bottom "0.8em";
        line_height "1.4";
      ];
    rule ~selector:(selector " h3")
      [
        font_size "1.2857143em";
        margin_top "1.5555556em";
        margin_bottom "0.4444444em";
        line_height "1.5555556";
      ];
  ]

let lg_size_rules selector =
  [
    rule ~selector:".prose-lg" [ font_size "1.125rem"; line_height "1.7777778" ];
    rule ~selector:(selector " p")
      [ margin_top "1.3333333em"; margin_bottom "1.3333333em" ];
    rule ~selector:(selector " h1")
      [
        font_size "2.6666667em";
        margin_top "0";
        margin_bottom "0.8333333em";
        line_height "1";
      ];
    rule ~selector:(selector " h2")
      [
        font_size "2em";
        margin_top "1.5555556em";
        margin_bottom "0.8888889em";
        line_height "1.3333333";
      ];
    rule ~selector:(selector " h3")
      [
        font_size "1.6666667em";
        margin_top "1.6666667em";
        margin_bottom "0.6666667em";
        line_height "1.5";
      ];
  ]

let xl_size_rules selector =
  [
    rule ~selector:".prose-xl" [ font_size "1.25rem"; line_height "1.8" ];
    rule ~selector:(selector " p") [ margin_top "1.2em"; margin_bottom "1.2em" ];
    rule ~selector:(selector " h1")
      [
        font_size "2.8em";
        margin_top "0";
        margin_bottom "0.8571429em";
        line_height "1";
      ];
    rule ~selector:(selector " h2")
      [
        font_size "2em";
        margin_top "1.4em";
        margin_bottom "0.8em";
        line_height "1.2727273";
      ];
    rule ~selector:(selector " h3")
      [
        font_size "1.6em";
        margin_top "1.5555556em";
        margin_bottom "0.6666667em";
        line_height "1.4444444";
      ];
  ]

let xl2_size_rules selector =
  [
    rule ~selector:".prose-2xl" [ font_size "1.5rem"; line_height "1.6666667" ];
    rule ~selector:(selector " p")
      [ margin_top "1.3333333em"; margin_bottom "1.3333333em" ];
    rule ~selector:(selector " h1")
      [
        font_size "2.6666667em";
        margin_top "0";
        margin_bottom "0.875em";
        line_height "1";
      ];
    rule ~selector:(selector " h2")
      [
        font_size "2em";
        margin_top "1.5em";
        margin_bottom "0.8333333em";
        line_height "1.0833333";
      ];
    rule ~selector:(selector " h3")
      [
        font_size "1.5em";
        margin_top "1.5555556em";
        margin_bottom "0.6666667em";
        line_height "1.2222222";
      ];
  ]

let gray_color_rules =
  [
    rule ~selector:".prose-gray"
      [
        property "--tw-prose-body" "rgb(107 114 128)";
        property "--tw-prose-headings" "rgb(31 41 55)";
        property "--tw-prose-lead" "rgb(75 85 99)";
        property "--tw-prose-links" "rgb(31 41 55)";
        property "--tw-prose-bold" "rgb(31 41 55)";
        property "--tw-prose-counters" "rgb(107 114 128)";
        property "--tw-prose-bullets" "rgb(209 213 219)";
        property "--tw-prose-hr" "rgb(229 231 235)";
        property "--tw-prose-quotes" "rgb(31 41 55)";
        property "--tw-prose-quote-borders" "rgb(229 231 235)";
        property "--tw-prose-captions" "rgb(156 163 175)";
        property "--tw-prose-code" "rgb(31 41 55)";
        property "--tw-prose-pre-code" "rgb(229 231 235)";
        property "--tw-prose-pre-bg" "rgb(31 41 55)";
        property "--tw-prose-th-borders" "rgb(209 213 219)";
        property "--tw-prose-td-borders" "rgb(229 231 235)";
      ];
  ]

let slate_color_rules =
  [
    rule ~selector:".prose-slate"
      [
        property "--tw-prose-body" "rgb(100 116 139)";
        property "--tw-prose-headings" "rgb(15 23 42)";
        property "--tw-prose-lead" "rgb(71 85 105)";
        property "--tw-prose-links" "rgb(15 23 42)";
        property "--tw-prose-bold" "rgb(15 23 42)";
        property "--tw-prose-counters" "rgb(100 116 139)";
        property "--tw-prose-bullets" "rgb(203 213 225)";
        property "--tw-prose-hr" "rgb(226 232 240)";
        property "--tw-prose-quotes" "rgb(15 23 42)";
        property "--tw-prose-quote-borders" "rgb(226 232 240)";
        property "--tw-prose-captions" "rgb(148 163 184)";
        property "--tw-prose-code" "rgb(15 23 42)";
        property "--tw-prose-pre-code" "rgb(226 232 240)";
        property "--tw-prose-pre-bg" "rgb(30 41 59)";
        property "--tw-prose-th-borders" "rgb(203 213 225)";
        property "--tw-prose-td-borders" "rgb(226 232 240)";
      ];
  ]

let base_prose_rules selector =
  (* Base prose element with CSS variables *)
  let base_rule =
    rule ~selector:".prose"
      ([
         color "var(--tw-prose-body)";
         max_width "65ch";
         font_size "1rem";
         line_height "1.75";
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
        color "var(--tw-prose-body)";
        max_width "65ch";
        font_size "1rem";
        line_height "1.75";
      ]
  | Sm -> [ font_size "0.875rem"; line_height "1.7142857" ]
  | Lg -> [ font_size "1.125rem"; line_height "1.7777778" ]
  | Xl -> [ font_size "1.25rem"; line_height "1.8" ]
  | Xl2 -> [ font_size "1.5rem"; line_height "1.6666667" ]
  | Gray | Slate -> [] (* Color themes only affect CSS variables *)

let pp = function
  | Base -> "Base"
  | Sm -> "Sm"
  | Lg -> "Lg"
  | Xl -> "Xl"
  | Xl2 -> "Xl2"
  | Gray -> "Gray"
  | Slate -> "Slate"
