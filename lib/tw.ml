(** A type-safe, ergonomic DSL for Tailwind CSS using nominal types.

    This library takes inspiration from Tailwind CSS v3's utility-first approach
    while leveraging OCaml's type system for compile-time safety. We cherry-pick
    concepts that work well with OCaml and add our own innovations where
    appropriate.

    Key design decisions:
    - Pure OCaml implementation without external CSS dependencies
    - Type-safe API that prevents invalid CSS at compile time
    - Simplified spacing functions that accept integers directly
    - Support for modern CSS features like container queries and 3D transforms
    - Minimal bundle size for js_of_ocaml by avoiding Format module *)

open Css

(** {1 Core Types} *)

type breakpoint = [ `Sm | `Md | `Lg | `Xl | `Xl_2 ]
(** Responsive breakpoints matching Tailwind's default scale *)

(** A Tailwind utility modifier *)
type modifier =
  | Hover
  | Focus
  | Active
  | Disabled
  | Group_hover
  | Dark
  | Responsive of breakpoint
  | Peer_hover
  | Peer_focus
  | Peer_checked
  | Group_focus
  | Aria_checked
  | Aria_expanded
  | Aria_selected
  | Aria_disabled
  | Data_state of string
  | Data_variant of string
  | Data_active
  | Data_inactive
  | Data_custom of string * string
  | Container of container_query
  (* New v4 variants *)
  | Not of modifier (* not-* variant for negation *)
  | Has of string (* has-* variant for :has() pseudo-class *)
  | Group_has of string (* group-has-* variant *)
  | Peer_has of string (* peer-has-* variant *)
  | Starting (* starting variant for @starting-style *)
  | Focus_within (* focus-within variant *)
  | Focus_visible (* focus-visible variant *)
  | Motion_safe (* motion-safe variant *)
  | Motion_reduce (* motion-reduce variant *)
  | Contrast_more (* contrast-more variant *)
  | Contrast_less (* contrast-less variant *)

and container_query =
  | Container_sm
  | Container_md
  | Container_lg
  | Container_xl
  | Container_2xl
  | Container_named of string * int

(** A Tailwind utility class with its name, CSS properties, and required
    variables *)
type t =
  | Style of { name : string; props : Css.property list; vars : Css.var list }
  | Prose of Prose.t
  | Modified of modifier * t
  | Group of t list

(* Abstract color type *)
type color = Color.t

(* Common size variants used across multiple utilities *)
type size = [ `None | `Xs | `Sm | `Md | `Lg | `Xl | `Xl_2 | `Xl_3 | `Full ]

(* Polymorphic variant types for composable sizing *)
type spacing = [ `Px | `Full | `Rem of float ]
type margin = [ spacing | `Auto ]
type scale = [ spacing | size | `Screen | `Min | `Max | `Fit ]
type max_scale = [ scale | `Xl_4 | `Xl_5 | `Xl_6 | `Xl_7 ]
type shadow = [ size | `Inner ]

(* Re-export CSS module *)
module Css = Css

(** {1 Helpers} *)

(* Helper to create a style with no variable requirements *)
let style name props = Style { name; props; vars = [] }

(* Helper to create a style with variable requirements *)
let style_with_vars name props vars = Style { name; props; vars }

(** {1 CSS Generation} *)

(* Internal helper to extract CSS properties from a style *)
let rec to_css_properties = function
  | Style { props; _ } -> props
  | Prose variant ->
      (* For inline styles, we can only use the base prose properties, not the
         descendant selectors like .prose h1 *)
      Prose.to_base_properties variant
  | Modified (_modifier, t) -> to_css_properties t
  | Group styles -> List.concat_map to_css_properties styles

(* Extract all CSS variables required by styles *)
let rec extract_css_vars = function
  | Style { vars; _ } -> vars
  | Prose _variant -> [] (* Prose has its own variable handling *)
  | Modified (_modifier, t) -> extract_css_vars t
  | Group styles -> List.concat_map extract_css_vars styles

(* Generate CSS variables from Css.var list - new type-safe approach *)
let generate_vars_from_types vars =
  let dedup_vars =
    List.sort_uniq
      Css.(
        fun v1 v2 ->
          match (v1, v2) with
          | Color (name1, shade1), Color (name2, shade2) ->
              let cmp = String.compare name1 name2 in
              if cmp = 0 then Option.compare Int.compare shade1 shade2 else cmp
          | Spacing n1, Spacing n2 -> Int.compare n1 n2
          | Font f1, Font f2 -> String.compare f1 f2
          | Text_size s1, Text_size s2 -> String.compare s1 s2
          | Font_weight w1, Font_weight w2 -> String.compare w1 w2
          | Radius r1, Radius r2 -> String.compare r1 r2
          | Transition, Transition -> 0
          | Custom (n1, v1), Custom (n2, v2) ->
              let cmp = String.compare n1 n2 in
              if cmp = 0 then String.compare v1 v2 else cmp
          | _ -> Stdlib.compare v1 v2)
      vars
  in

  List.filter_map
    (function
      | Css.Color (name, Some shade) ->
          (* Handle arbitrary color values with brackets like [1da1f2] or
             [rgb(...)] *)
          if
            String.starts_with ~prefix:"[" name
            && String.ends_with ~suffix:"]" name
          then
            let color_value = String.sub name 1 (String.length name - 2) in
            (* Try to parse as different color formats *)
            let oklch_css =
              if String.starts_with ~prefix:"rgb(" color_value then
                (* Handle RGB function like rgb(29,161,242) *)
                let rgb_part =
                  String.sub color_value 4 (String.length color_value - 5)
                in
                let parts =
                  String.split_on_char ',' rgb_part |> List.map String.trim
                in
                match parts with
                | [ r; g; b ] -> (
                    try
                      let ri = int_of_string r
                      and gi = int_of_string g
                      and bi = int_of_string b in
                      let color = Color.rgb ri gi bi in
                      Color.to_oklch_css color 0
                    with _ -> "#" ^ color_value (* fallback *))
                | _ -> "#" ^ color_value
              else
                (* Handle hex colors *)
                let hex_with_hash =
                  if String.starts_with ~prefix:"#" color_value then color_value
                  else "#" ^ color_value
                in
                Color.hex_to_oklch_css hex_with_hash
            in
            Some
              (Css.property
                 ("--color-" ^ name ^ "-" ^ string_of_int shade)
                 oklch_css)
          else
            let color = Color.of_string name in
            Some
              (Css.property
                 ("--color-" ^ name ^ "-" ^ string_of_int shade)
                 (Color.to_oklch_css color shade))
      | Css.Color (name, None) ->
          (* Handle arbitrary color values with brackets like [1da1f2] or
             [rgb(...)] *)
          if
            String.starts_with ~prefix:"[" name
            && String.ends_with ~suffix:"]" name
          then
            let color_value = String.sub name 1 (String.length name - 2) in
            (* Try to parse as different color formats *)
            let oklch_css =
              if String.starts_with ~prefix:"rgb(" color_value then
                (* Handle RGB function like rgb(29,161,242) *)
                let rgb_part =
                  String.sub color_value 4 (String.length color_value - 5)
                in
                let parts =
                  String.split_on_char ',' rgb_part |> List.map String.trim
                in
                match parts with
                | [ r; g; b ] -> (
                    try
                      let ri = int_of_string r
                      and gi = int_of_string g
                      and bi = int_of_string b in
                      let color = Color.rgb ri gi bi in
                      Color.to_oklch_css color 0
                    with _ -> "#" ^ color_value (* fallback *))
                | _ -> "#" ^ color_value
              else
                (* Handle hex colors *)
                let hex_with_hash =
                  if String.starts_with ~prefix:"#" color_value then color_value
                  else "#" ^ color_value
                in
                Color.hex_to_oklch_css hex_with_hash
            in
            Some (Css.property ("--color-" ^ name) oklch_css)
          else
            let color = Color.of_string name in
            Some
              (Css.property ("--color-" ^ name) (Color.to_oklch_css color 500))
      | Css.Spacing 1 -> Some (Css.property "--spacing" "0.25rem")
      | _ -> None (* Add other variable types as needed *))
    dedup_vars

(* Helper to convert breakpoint to string *)
let string_of_breakpoint = function
  | `Sm -> "sm"
  | `Md -> "md"
  | `Lg -> "lg"
  | `Xl -> "xl"
  | `Xl_2 -> "2xl"

(* Helper to convert container query to CSS prefix *)
let container_query_to_css_prefix = function
  | Container_sm -> "@container (min-width: 640px)"
  | Container_md -> "@container (min-width: 768px)"
  | Container_lg -> "@container (min-width: 1024px)"
  | Container_xl -> "@container (min-width: 1280px)"
  | Container_2xl -> "@container (min-width: 1536px)"
  | Container_named ("", width) ->
      Printf.sprintf "@container (min-width: %dpx)" width
  | Container_named (name, width) ->
      Printf.sprintf "@container %s (min-width: %dpx)" name width

(* Generate CSS variables from the collected requirements *)
let container_query_to_class_prefix = function
  | Container_sm -> "@container-sm"
  | Container_md -> "@container-md"
  | Container_lg -> "@container-lg"
  | Container_xl -> "@container-xl"
  | Container_2xl -> "@container-2xl"
  | Container_named ("", width) -> "@container-" ^ string_of_int width
  | Container_named (name, width) ->
      "@container-" ^ name ^ "-" ^ string_of_int width

(* Helper to get breakpoint for responsive prefix *)
let responsive_breakpoint = function
  | "sm" -> "40rem" (* 640px / 16 = 40rem *)
  | "md" -> "48rem" (* 768px / 16 = 48rem *)
  | "lg" -> "64rem" (* 1024px / 16 = 64rem *)
  | "xl" -> "80rem" (* 1280px / 16 = 80rem *)
  | "2xl" -> "96rem" (* 1536px / 16 = 96rem *)
  | _ -> "0rem"

(* Extract selector and properties from a single Tw style *)
let extract_selector_props tw =
  let rec extract = function
    | Style { name = class_name; props; _ } -> [ ("." ^ class_name, props) ]
    | Prose variant ->
        (* Convert prose rules to selector/props pairs *)
        Prose.to_css_rules variant
        |> List.map (fun rule -> (Css.selector rule, Css.properties rule))
    | Modified (modifier, t) ->
        let base = extract t in
        List.map
          (fun (selector, props) ->
            let base_class =
              String.sub selector 1 (String.length selector - 1)
            in
            match modifier with
            | Hover -> (".hover\\:" ^ base_class ^ ":hover", props)
            | Focus -> (".focus\\:" ^ base_class ^ ":focus", props)
            | Active -> (".active\\:" ^ base_class ^ ":active", props)
            | Disabled -> (".disabled\\:" ^ base_class ^ ":disabled", props)
            | Group_hover -> (".group:hover .group-hover\\:" ^ base_class, props)
            | Group_focus -> (".group:focus .group-focus\\:" ^ base_class, props)
            | Peer_hover -> (".peer:hover ~ .peer-hover\\:" ^ base_class, props)
            | Peer_focus -> (".peer:focus ~ .peer-focus\\:" ^ base_class, props)
            | Peer_checked ->
                (".peer:checked ~ .peer-checked\\:" ^ base_class, props)
            | Aria_checked ->
                ( ".aria-checked\\:" ^ base_class ^ "[aria-checked=\"true\"]",
                  props )
            | Aria_expanded ->
                ( ".aria-expanded\\:" ^ base_class ^ "[aria-expanded=\"true\"]",
                  props )
            | Aria_selected ->
                ( ".aria-selected\\:" ^ base_class ^ "[aria-selected=\"true\"]",
                  props )
            | Aria_disabled ->
                ( ".aria-disabled\\:" ^ base_class ^ "[aria-disabled=\"true\"]",
                  props )
            | Data_state value ->
                (selector ^ "[data-state=\"" ^ value ^ "\"]", props)
            | Data_variant value ->
                (selector ^ "[data-variant=\"" ^ value ^ "\"]", props)
            | Data_active ->
                (".data-\\[active\\]\\:" ^ base_class ^ "[data-active]", props)
            | Data_inactive ->
                ( ".data-\\[inactive\\]\\:" ^ base_class ^ "[data-inactive]",
                  props )
            | Data_custom (key, value) ->
                (selector ^ "[data-" ^ key ^ "=\"" ^ value ^ "\"]", props)
            | Dark ->
                ("@media (prefers-color-scheme: dark) { " ^ selector, props)
            | Responsive breakpoint ->
                let prefix = string_of_breakpoint breakpoint in
                (* Create the selector with media query prefix for grouping *)
                ( "@media (min-width: "
                  ^ responsive_breakpoint prefix
                  ^ ") ." ^ prefix ^ "\\:"
                  ^ String.sub selector 1 (String.length selector - 1),
                  props )
            | Container query ->
                let query_str = container_query_to_css_prefix query in
                (query_str ^ " { " ^ selector, props)
            (* New v4 modifiers *)
            | Not modifier ->
                (* Recursively apply the Not modifier *)
                let inner_selectors =
                  extract (Modified (modifier, style base_class []))
                in
                List.map
                  (fun (inner_sel, _) ->
                    let cleaned =
                      String.sub inner_sel 1 (String.length inner_sel - 1)
                    in
                    (".not-" ^ cleaned ^ ":not(" ^ inner_sel ^ ")", props))
                  inner_selectors
                |> List.hd
            | Has selector_str ->
                ( ".has-\\[" ^ selector_str ^ "\\]\\:" ^ base_class ^ ":has("
                  ^ selector_str ^ ")",
                  props )
            | Group_has selector_str ->
                ( ".group:has(" ^ selector_str ^ ") .group-has-\\["
                  ^ selector_str ^ "\\]\\:" ^ base_class,
                  props )
            | Peer_has selector_str ->
                ( ".peer:has(" ^ selector_str ^ ") ~ .peer-has-\\["
                  ^ selector_str ^ "\\]\\:" ^ base_class,
                  props )
            | Starting -> ("@starting-style { ." ^ base_class, props)
            | Focus_within ->
                (".focus-within\\:" ^ base_class ^ ":focus-within", props)
            | Focus_visible ->
                (".focus-visible\\:" ^ base_class ^ ":focus-visible", props)
            | Motion_safe ->
                ( "@media (prefers-reduced-motion: no-preference) { \
                   .motion-safe\\:" ^ base_class,
                  props )
            | Motion_reduce ->
                ( "@media (prefers-reduced-motion: reduce) { .motion-reduce\\:"
                  ^ base_class,
                  props )
            | Contrast_more ->
                ( "@media (prefers-contrast: more) { .contrast-more\\:"
                  ^ base_class,
                  props )
            | Contrast_less ->
                ( "@media (prefers-contrast: less) { .contrast-less\\:"
                  ^ base_class,
                  props ))
          base
    | Group styles -> List.concat_map extract styles
  in
  extract tw

(* Group properties by selector *)
let group_by_selector rules =
  List.fold_left
    (fun acc (selector, props) ->
      let existing = try List.assoc selector acc with Not_found -> [] in
      (selector, existing @ props) :: List.remove_assoc selector acc)
    [] rules

(* Base reset CSS rules *)
(* Simple, clean reset rules for Tailwind v4 *)
let generate_reset_rules () =
  [
    (* Base reset *)
    Css.rule ~selector:"*, ::before, ::after"
      [
        Css.box_sizing "border-box";
        Css.margin "0";
        Css.padding "0";
        Css.property "border" "0 solid";
      ];
    Css.rule ~selector:"html, :host"
      [
        Css.font_size "16px";
        Css.line_height "1.5";
        Css.property "-webkit-text-size-adjust" "100%";
        Css.font_family "var(--default-font-family)";
      ];
    Css.rule ~selector:"body" [ Css.margin "0"; Css.line_height "inherit" ];
    Css.rule ~selector:"h1, h2, h3, h4, h5, h6"
      [ Css.font_size "inherit"; Css.font_weight "inherit" ];
    Css.rule ~selector:"a"
      [ Css.color "inherit"; Css.text_decoration "inherit" ];
    Css.rule ~selector:"img, video"
      [
        Css.display "block"; Css.property "max-width" "100%"; Css.height "auto";
      ];
  ]

(* Check if prose styles are being used *)
let uses_prose tw_classes =
  let rec check = function
    | Style _ -> false
    | Prose _ -> true
    | Modified (_, t) -> check t
    | Group styles -> List.exists check styles
  in
  List.exists check tw_classes

(* Re-export Color module *)
module Color = Color

(* Generate CSS rules for all used Tw classes *)
let to_css ?(reset = true) tw_classes =
  let all_rules =
    tw_classes |> List.concat_map extract_selector_props |> group_by_selector
  in
  (* Separate media query rules from regular rules *)
  let is_media_query selector = String.starts_with ~prefix:"@media" selector in
  let regular_rules, media_rules =
    List.partition (fun (sel, _) -> not (is_media_query sel)) all_rules
  in

  (* Separate hover rules from regular rules *)
  let is_hover_rule selector =
    String.contains selector ':'
    && String.contains selector 'h'
    && String.contains selector 'o'
    && String.contains selector 'v'
    && String.contains selector 'e'
    && String.contains selector 'r'
  in
  let non_hover_rules, hover_rules =
    List.partition (fun (sel, _) -> not (is_hover_rule sel)) regular_rules
  in

  (* Create regular CSS rules *)
  let rules =
    List.map
      (fun (selector, props) ->
        Css.rule ~selector (Css.deduplicate_properties props))
      non_hover_rules
  in

  (* Group media query rules by their condition *)
  let media_queries_map =
    List.fold_left
      (fun acc (selector, props) ->
        if String.starts_with ~prefix:"@media (min-width:" selector then
          (* Extract the media condition and actual selector *)
          let idx = String.index selector ')' in
          let condition = String.sub selector 0 (idx + 1) in
          let actual_selector =
            let rest =
              String.sub selector (idx + 1) (String.length selector - idx - 1)
            in
            String.trim rest
          in
          let rules = try List.assoc condition acc with Not_found -> [] in
          (condition, (actual_selector, props) :: rules)
          :: List.remove_assoc condition acc
        else acc)
      [] media_rules
  in

  (* Add hover rules to media query map *)
  let media_queries_map =
    if hover_rules = [] then media_queries_map
    else
      let hover_condition = "@media (hover: hover)" in
      let existing_hover_rules =
        try List.assoc hover_condition media_queries_map with Not_found -> []
      in
      (hover_condition, hover_rules @ existing_hover_rules)
      :: List.remove_assoc hover_condition media_queries_map
  in

  (* Create media query objects *)
  let media_queries =
    List.map
      (fun (condition, rule_list) ->
        let rules =
          List.map
            (fun (sel, props) ->
              Css.rule ~selector:sel (Css.deduplicate_properties props))
            rule_list
        in
        let media_condition =
          String.sub condition 7 (String.length condition - 7)
        in
        Css.media ~condition:media_condition rules)
      media_queries_map
  in

  (* Build the complete stylesheet with layers - just like Tailwind v4 *)
  if reset then
    (* Theme layer with CSS variables - JIT mode (only used variables) *)
    let all_vars = List.concat_map extract_css_vars tw_classes in
    let theme_vars = generate_vars_from_types all_vars in
    let theme_layer =
      Css.layered_rules ~layer:Css.Theme
        [ Css.rule ~selector:":root, :host" theme_vars ]
    in

    (* Base layer with reset rules *)
    let base_layer =
      Css.layered_rules ~layer:Css.Base (generate_reset_rules ())
    in

    (* Utilities layer with the actual utility classes AND media queries *)
    let utilities_layer =
      Css.layered_rules ~layer:Css.Utilities ~media_queries rules
    in

    (* Add prose styles if needed *)
    let layers =
      if uses_prose tw_classes then
        let prose_rules =
          tw_classes
          |> List.filter_map (function
               | Prose variant -> Some (Prose.to_css_rules variant)
               | _ -> None)
          |> List.concat
        in
        [
          theme_layer;
          base_layer;
          utilities_layer;
          Css.layered_rules ~layer:Css.Utilities prose_rules;
        ]
      else [ theme_layer; base_layer; utilities_layer ]
    in

    Css.stylesheet ~layers []
  else
    (* No reset - just raw rules and media queries, no layers *)
    Css.stylesheet ~layers:[] ~media_queries rules

(* Convert Tw styles to inline style attribute value *)
let to_inline_style styles =
  let all_props = List.concat_map to_css_properties styles in
  let deduped = Css.deduplicate_properties all_props in
  Css.properties_to_inline_style deduped

(** {1 Helper Functions} *)

(** Convert hex color to rgb format - now only handles hex strings *)

(** Convert any color to RGB space-separated string format (e.g., "255 0 0") *)

let color_to_rgb_string color shade = Color.to_rgb_string color shade

let spacing_to_rem = function
  | 0 -> "0"
  | 1 -> "0.25rem"
  | 2 -> "0.5rem"
  | 3 -> "0.75rem"
  | 4 -> "1rem"
  | 6 -> "1.5rem"
  | 8 -> "2rem"
  | 10 -> "2.5rem"
  | 12 -> "3rem"
  | 16 -> "4rem"
  | 20 -> "5rem"
  | 24 -> "6rem"
  | 56 -> "14rem"
  | n -> Pp.float (float_of_int n *. 0.25) ^ "rem"

(** {1 Public API} *)

(** {1 Colors} *)

let color_name color = Color.to_name color

(* Color constructors *)
let black = Color.black
let white = Color.white
let gray = Color.gray
let slate = Color.slate
let zinc = Color.zinc
let neutral = Color.neutral
let stone = Color.stone
let red = Color.red
let orange = Color.orange
let amber = Color.amber
let yellow = Color.yellow
let lime = Color.lime
let green = Color.green
let emerald = Color.emerald
let teal = Color.teal
let cyan = Color.cyan
let sky = Color.sky
let blue = Color.blue
let indigo = Color.indigo
let violet = Color.violet
let purple = Color.purple
let fuchsia = Color.fuchsia
let pink = Color.pink
let rose = Color.rose
let hex s = Color.hex s
let rgb r g b = Color.rgb r g b

(* Value constructors *)

(* Spacing constructors *)
let rem f = `Rem f
let int n = rem (float_of_int n *. 0.25)
let one_px = `Px
let full = `Full

(* Size constructors *)
let screen = `Screen
let min = `Min
let max = `Max
let fit = `Fit

(* Max width constructors *)
let none = `None
let xs = `Xs
let sm = `Sm
let md = `Md
let lg = `Lg
let xl = `Xl
let xl_2 = `Xl_2
let xl_3 = `Xl_3
let xl_4 = `Xl_4
let xl_5 = `Xl_5
let xl_6 = `Xl_6
let xl_7 = `Xl_7

let bg color shade =
  let class_name =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "bg-"; color_name color ]
    else Pp.str [ "bg-"; color_name color; "-"; string_of_int shade ]
  in
  (* For custom colors (hex, rgb, oklch), use direct values; for others use CSS
     variables *)
  if Color.is_custom_color color then
    (* Direct arbitrary value - no CSS variable *)
    let direct_value =
      match color with
      | Color.Hex h -> h
      | Color.Rgb { red; green; blue } ->
          Pp.str
            [
              "rgb(";
              string_of_int red;
              ",";
              string_of_int green;
              ",";
              string_of_int blue;
              ")";
            ]
      | Color.Oklch oklch -> Color.oklch_to_css oklch
      | _ -> Color.to_oklch_css color shade (* Fallback *)
    in
    style class_name [ background_color direct_value ]
  else
    (* Use CSS variable reference *)
    let var_ref =
      if Color.is_base_color color then
        Pp.str [ "var(--color-"; color_name color; ")" ]
      else
        Pp.str
          [ "var(--color-"; color_name color; "-"; string_of_int shade; ")" ]
    in
    (* Track the color variable requirement *)
    let color_var =
      if Color.is_base_color color then Css.Color (color_name color, None)
      else Css.Color (color_name color, Some shade)
    in
    style_with_vars class_name [ background_color var_ref ] [ color_var ]

let bg_transparent = style "bg-transparent" [ background_color "transparent" ]
let bg_current = style "bg-current" [ background_color "currentColor" ]

(* Default color backgrounds - using shade 500 *)
let bg_black = bg black 500
let bg_white = bg white 500
let bg_gray = bg gray 500
let bg_slate = bg slate 500
let bg_zinc = bg zinc 500
let bg_neutral = bg neutral 500
let bg_stone = bg stone 500
let bg_red = bg red 500
let bg_orange = bg orange 500
let bg_amber = bg amber 500
let bg_yellow = bg yellow 500
let bg_lime = bg lime 500
let bg_green = bg green 500
let bg_emerald = bg emerald 500
let bg_teal = bg teal 500
let bg_cyan = bg cyan 500
let bg_sky = bg sky 500
let bg_blue = bg blue 500
let bg_indigo = bg indigo 500
let bg_violet = bg violet 500
let bg_purple = bg purple 500
let bg_fuchsia = bg fuchsia 500
let bg_pink = bg pink 500
let bg_rose = bg rose 500

let text color shade =
  let class_name =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "text-"; color_name color ]
    else Pp.str [ "text-"; color_name color; "-"; string_of_int shade ]
  in
  (* For custom colors (hex, rgb, oklch), use direct values; for others use CSS
     variables *)
  if Color.is_custom_color color then
    (* Direct arbitrary value - no CSS variable *)
    let direct_value =
      match color with
      | Color.Hex h -> h
      | Color.Rgb { red; green; blue } ->
          Pp.str
            [
              "rgb(";
              string_of_int red;
              ",";
              string_of_int green;
              ",";
              string_of_int blue;
              ")";
            ]
      | Color.Oklch oklch -> Color.oklch_to_css oklch
      | _ -> Color.to_oklch_css color shade (* Fallback *)
    in
    style class_name [ Css.color direct_value ]
  else
    (* Use CSS variable reference *)
    let var_ref =
      if Color.is_base_color color then
        Pp.str [ "var(--color-"; color_name color; ")" ]
      else
        Pp.str
          [ "var(--color-"; color_name color; "-"; string_of_int shade; ")" ]
    in
    (* Track the color variable requirement *)
    let color_var =
      if Color.is_base_color color then Css.Color (color_name color, None)
      else Css.Color (color_name color, Some shade)
    in
    style_with_vars class_name [ Css.color var_ref ] [ color_var ]

let text_transparent = style "text-transparent" [ Css.color "transparent" ]
let text_current = style "text-current" [ Css.color "currentColor" ]

(* Default text colors - using shade 500 *)
let text_black = text black 500
let text_white = text white 500
let text_gray = text gray 500
let text_slate = text slate 500
let text_zinc = text zinc 500
let text_neutral = text neutral 500
let text_stone = text stone 500
let text_red = text red 500
let text_orange = text orange 500
let text_amber = text amber 500
let text_yellow = text yellow 500
let text_lime = text lime 500
let text_green = text green 500
let text_emerald = text emerald 500
let text_teal = text teal 500
let text_cyan = text cyan 500
let text_sky = text sky 500
let text_blue = text blue 500
let text_indigo = text indigo 500
let text_violet = text violet 500
let text_purple = text purple 500
let text_fuchsia = text fuchsia 500
let text_pink = text pink 500
let text_rose = text rose 500

let border_color color shade =
  let class_name =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "border-"; color_name color ]
    else Pp.str [ "border-"; color_name color; "-"; string_of_int shade ]
  in
  (* For custom colors (hex, rgb, oklch), use direct values; for others use CSS
     variables *)
  if Color.is_custom_color color then
    (* Direct arbitrary value - no CSS variable *)
    let direct_value =
      match color with
      | Color.Hex h -> h
      | Color.Rgb { red; green; blue } ->
          Pp.str
            [
              "rgb(";
              string_of_int red;
              ",";
              string_of_int green;
              ",";
              string_of_int blue;
              ")";
            ]
      | Color.Oklch oklch -> Color.oklch_to_css oklch
      | _ -> Color.to_oklch_css color shade (* Fallback *)
    in
    style class_name [ Css.border_color direct_value ]
  else
    (* Use CSS variable reference *)
    let var_ref =
      if Color.is_base_color color then
        Pp.str [ "var(--color-"; color_name color; ")" ]
      else
        Pp.str
          [ "var(--color-"; color_name color; "-"; string_of_int shade; ")" ]
    in
    (* Track the color variable requirement *)
    let color_var =
      if Color.is_base_color color then Css.Color (color_name color, None)
      else Css.Color (color_name color, Some shade)
    in
    style_with_vars class_name [ Css.border_color var_ref ] [ color_var ]

let border_transparent =
  style "border-transparent" [ Css.border_color "transparent" ]

let border_current = style "border-current" [ Css.border_color "currentColor" ]

(* Default border colors - using shade 500 *)
let border_black = border_color black 500
let border_white = border_color white 500
let border_gray = border_color gray 500
let border_slate = border_color slate 500
let border_zinc = border_color zinc 500
let border_neutral = border_color neutral 500
let border_stone = border_color stone 500
let border_red = border_color red 500
let border_orange = border_color orange 500
let border_amber = border_color amber 500
let border_yellow = border_color yellow 500
let border_lime = border_color lime 500
let border_green = border_color green 500
let border_emerald = border_color emerald 500
let border_teal = border_color teal 500
let border_cyan = border_color cyan 500
let border_sky = border_color sky 500
let border_blue = border_color blue 500
let border_indigo = border_color indigo 500
let border_violet = border_color violet 500
let border_purple = border_color purple 500
let border_fuchsia = border_color fuchsia 500
let border_pink = border_color pink 500
let border_rose = border_color rose 500

let pp_spacing_suffix : spacing -> string = function
  | `Px -> "px"
  | `Full -> "full"
  | `Rem f ->
      (* Convert rem values back to Tailwind scale *)
      let n = int_of_float (f /. 0.25) in
      string_of_int (abs n)

let pp_size_suffix : size -> string = function
  | `None -> "none"
  | `Xs -> "xs"
  | `Sm -> "sm"
  | `Md -> "md"
  | `Lg -> "lg"
  | `Xl -> "xl"
  | `Xl_2 -> "2xl"
  | `Xl_3 -> "3xl"
  | `Full -> "full"

let pp_scale_suffix : scale -> string = function
  | `Screen -> "screen"
  | `Min -> "min"
  | `Max -> "max"
  | `Fit -> "fit"
  | #spacing as s -> pp_spacing_suffix s
  | #size as s -> pp_size_suffix s

let pp_max_scale_suffix : max_scale -> string = function
  | `Xl_4 -> "4xl"
  | `Xl_5 -> "5xl"
  | `Xl_6 -> "6xl"
  | `Xl_7 -> "7xl"
  | #scale as s -> pp_scale_suffix s

let pp_margin_suffix : margin -> string = function
  | `Auto -> "auto"
  | #spacing as s -> pp_spacing_suffix s

let pp_spacing : spacing -> string = function
  | `Px -> "1px"
  | `Full -> "100%"
  | `Rem f ->
      let n = int_of_float (f /. 0.25) in
      Pp.str [ "calc(var(--spacing) * "; string_of_int n; ")" ]

let pp_margin : margin -> string = function
  | `Auto -> "auto"
  | #spacing as s -> pp_spacing s

let pp_size : size -> string = function
  | `None -> "0"
  | `Xs -> "0.125rem"
  | `Sm -> "0.25rem"
  | `Md -> "0.375rem"
  | `Lg -> "0.5rem"
  | `Xl -> "0.75rem"
  | `Xl_2 -> "1rem"
  | `Xl_3 -> "1.5rem"
  | `Full -> "100%"

let pp_scale : scale -> string = function
  | `Screen -> "100vh"
  | `Min -> "min-content"
  | `Max -> "max-content"
  | `Fit -> "fit-content"
  | #spacing as s -> pp_spacing s
  | #size as s -> pp_size s

let pp_max_scale : max_scale -> string = function
  | `Xl_4 -> "56rem"
  | `Xl_5 -> "64rem"
  | `Xl_6 -> "72rem"
  | `Xl_7 -> "80rem"
  | #scale as s -> pp_scale s

(** {1 Spacing} *)

(* Helper to extract spacing variables from spacing types *)
let spacing_vars = function
  | `Rem _ -> [ Css.Spacing 1 ] (* Track spacing variable for calc() *)
  | _ -> []

(* Helper to extract spacing variables from scale types *)
let scale_vars = function
  | `Rem _ -> [ Css.Spacing 1 ] (* Track spacing variable for calc() *)
  | _ -> []

(* Helper to extract spacing variables from margin types *)
let margin_vars = function
  | `Rem _ -> [ Css.Spacing 1 ] (* Track spacing variable for calc() *)
  | _ -> []

(* Typed spacing functions with ' suffix *)
let p' (s : spacing) =
  let class_name = "p-" ^ pp_spacing_suffix s in
  let value = pp_spacing s in
  style_with_vars class_name [ padding value ] (spacing_vars s)

let px' (s : spacing) =
  let v = pp_spacing s in
  let class_name = "px-" ^ pp_spacing_suffix s in
  style_with_vars class_name [ padding_inline v ] (spacing_vars s)

let py' (s : spacing) =
  let v = pp_spacing s in
  let class_name = "py-" ^ pp_spacing_suffix s in
  style_with_vars class_name [ padding_block v ] (spacing_vars s)

let pt' (s : spacing) =
  let class_name = "pt-" ^ pp_spacing_suffix s in
  let value = pp_spacing s in
  style_with_vars class_name [ padding_top value ] (spacing_vars s)

let pr' (s : spacing) =
  let class_name = "pr-" ^ pp_spacing_suffix s in
  let value = pp_spacing s in
  style_with_vars class_name [ padding_right value ] (spacing_vars s)

let pb' (s : spacing) =
  let class_name = "pb-" ^ pp_spacing_suffix s in
  let value = pp_spacing s in
  style_with_vars class_name [ padding_bottom value ] (spacing_vars s)

let pl' (s : spacing) =
  let class_name = "pl-" ^ pp_spacing_suffix s in
  let value = pp_spacing s in
  style_with_vars class_name [ padding_left value ] (spacing_vars s)

(* Int-based spacing functions (convenience wrappers) *)
let p n = p' (int n)
let px n = px' (int n)
let py n = py' (int n)
let pt n = pt' (int n)
let pr n = pr' (int n)
let pb n = pb' (int n)
let pl n = pl' (int n)

(* Special padding values *)
let p_px = p' `Px
let p_full = p' `Full
let px_px = px' `Px
let px_full = px' `Full
let py_px = py' `Px
let py_full = py' `Full
let pt_px = pt' `Px
let pt_full = pt' `Full
let pr_px = pr' `Px
let pr_full = pr' `Full
let pb_px = pb' `Px
let pb_full = pb' `Full
let pl_px = pl' `Px
let pl_full = pl' `Full

(* Typed margin functions with ' suffix *)
let m' (m : margin) =
  let class_name = "m-" ^ pp_margin_suffix m in
  let value = pp_margin m in
  style_with_vars class_name [ margin value ] (margin_vars m)

let mx' (m : margin) =
  let v = pp_margin m in
  let class_name = "mx-" ^ pp_margin_suffix m in
  style_with_vars class_name [ margin_inline v ] (margin_vars m)

let my' (m : margin) =
  let v = pp_margin m in
  let class_name = "my-" ^ pp_margin_suffix m in
  style_with_vars class_name [ margin_block v ] (margin_vars m)

let mt' (m : margin) =
  let class_name = "mt-" ^ pp_margin_suffix m in
  let value = pp_margin m in
  style_with_vars class_name [ margin_top value ] (margin_vars m)

let mr' (m : margin) =
  let class_name = "mr-" ^ pp_margin_suffix m in
  let value = pp_margin m in
  style_with_vars class_name [ margin_right value ] (margin_vars m)

let mb' (m : margin) =
  let class_name = "mb-" ^ pp_margin_suffix m in
  let value = pp_margin m in
  style_with_vars class_name [ margin_bottom value ] (margin_vars m)

let ml' (m : margin) =
  let class_name = "ml-" ^ pp_margin_suffix m in
  let value = pp_margin m in
  style_with_vars class_name [ margin_left value ] (margin_vars m)

(* Int-based margin functions - now support negative values *)
let m n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "m-" ^ pp_spacing_suffix s in
  style class_name [ margin (pp_margin s) ]

let mx n =
  let s = int n in
  let v = pp_margin s in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mx-" ^ pp_spacing_suffix s in
  style class_name [ margin_inline v ]

let my n =
  let s = int n in
  let v = pp_margin s in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "my-" ^ pp_spacing_suffix s in
  style class_name [ margin_block v ]

let mt n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mt-" ^ pp_spacing_suffix s in
  style class_name [ margin_top (pp_margin s) ]

let mr n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mr-" ^ pp_spacing_suffix s in
  style class_name [ margin_right (pp_margin s) ]

let mb n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mb-" ^ pp_spacing_suffix s in
  style class_name [ margin_bottom (pp_margin s) ]

let ml n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "ml-" ^ pp_spacing_suffix s in
  style class_name [ margin_left (pp_margin s) ]

(* Common margin utilities *)
let m_auto = m' `Auto
let mx_auto = mx' `Auto (* Very common for centering *)
let my_auto = my' `Auto
let mt_auto = mt' `Auto
let mr_auto = mr' `Auto
let mb_auto = mb' `Auto
let ml_auto = ml' `Auto

(* Typed gap functions with ' suffix *)
let gap' (s : spacing) =
  let class_name = "gap-" ^ pp_spacing_suffix s in
  let value = pp_spacing s in
  style_with_vars class_name [ gap value ] (spacing_vars s)

let gap_x' (s : spacing) =
  let class_name = "gap-x-" ^ pp_spacing_suffix s in
  let value = pp_spacing s in
  style_with_vars class_name [ column_gap value ] (spacing_vars s)

let gap_y' (s : spacing) =
  let class_name = "gap-y-" ^ pp_spacing_suffix s in
  let value = pp_spacing s in
  style_with_vars class_name [ row_gap value ] (spacing_vars s)

(* Int-based gap functions (convenience wrappers) *)
let gap n = gap' (int n)
let gap_x n = gap_x' (int n)
let gap_y n = gap_y' (int n)

(* Special gap values *)
let gap_px = gap' `Px
let gap_full = gap' `Full

(* Space between utilities *)
let space_x n =
  let class_name = "space-x-" ^ string_of_int n in
  style class_name [ margin_left (spacing_to_rem n) ]

let space_y n =
  let class_name = "space-y-" ^ string_of_int n in
  style class_name [ margin_top (spacing_to_rem n) ]

(** {1 Sizing} *)

(* Typed scale functions with ' suffix *)
let w' (s : scale) =
  let class_name = "w-" ^ pp_scale_suffix s in
  let value = pp_scale s in
  style_with_vars class_name [ width value ] (scale_vars s)

let h' (s : scale) =
  let class_name = "h-" ^ pp_scale_suffix s in
  let value = pp_scale s in
  style_with_vars class_name [ height value ] (scale_vars s)

let min_w' (s : scale) =
  let class_name = "min-w-" ^ pp_scale_suffix s in
  style class_name [ min_width (pp_scale s) ]

let min_h' (s : scale) =
  let class_name = "min-h-" ^ pp_scale_suffix s in
  style class_name [ min_height (pp_scale s) ]

let max_w' (s : max_scale) =
  let class_name = "max-w-" ^ pp_max_scale_suffix s in
  style class_name [ max_width (pp_max_scale s) ]

let max_h' (s : max_scale) =
  let class_name = "max-h-" ^ pp_max_scale_suffix s in
  style class_name [ max_height (pp_max_scale s) ]

(* Int-based scale functions (convenience wrappers) *)
let w n = w' (int n)
let h n = h' (int n)
let min_w n = min_w' (int n)
let min_h n = min_h' (int n)
let max_w n = max_w' (int n)
let max_h n = max_h' (int n)

(* Common size utilities *)
let w_full = w' `Full (* Very common for full width *)
let h_full = h' `Full (* Very common for full height *)
let w_fit = w' `Fit (* Common for fit-content sizing *)
let h_fit = h' `Fit (* Common for fit-content sizing *)
let w_screen = w' `Screen (* Full viewport width *)
let h_screen = h' `Screen (* Full viewport height *)
let w_min = w' `Min (* Min-content width *)
let h_min = h' `Min (* Min-content height *)
let w_max = w' `Max (* Max-content width *)
let h_max = h' `Max (* Max-content height *)
let min_h_screen = min_h' `Screen (* Common for full viewport height *)
let min_w_full = min_w' `Full (* Minimum width 100% *)
let min_h_full = min_h' `Full (* Minimum height 100% *)
let max_w_2xl = max_w' `Xl_2 (* Common for article text *)
let max_w_3xl = max_w' `Xl_3 (* Common for content width *)
let max_w_4xl = max_w' `Xl_4 (* Common for content width *)
let max_w_none = max_w' `None (* No maximum width *)
let max_w_full = max_w' `Full (* Maximum width 100% *)
let max_h_full = max_h' `Full (* Maximum height 100% *)

(** {1 Typography} *)

let text_xs =
  style "text-xs"
    [
      font_size "var(--text-xs)";
      line_height "var(--tw-leading, var(--text-xs--line-height))";
    ]

let text_sm =
  style "text-sm"
    [
      font_size "var(--text-sm)";
      line_height "var(--tw-leading, var(--text-sm--line-height))";
    ]

let text_xl =
  style "text-xl"
    [
      font_size "var(--text-xl)";
      line_height "var(--tw-leading, var(--text-xl--line-height))";
    ]

let text_2xl =
  style "text-2xl"
    [
      font_size "var(--text-2xl)";
      line_height "var(--tw-leading, var(--text-2xl--line-height))";
    ]

let text_center = style "text-center" [ text_align "center" ]

(** {1 Layout} *)

let block = style "block" [ display "block" ]
let inline = style "inline" [ display "inline" ]
let inline_block = style "inline-block" [ display "inline-block" ]
let hidden = style "hidden" [ display "none" ]

(** {1 Flexbox} *)

let flex = style "flex" [ display "flex" ]
let flex_shrink_0 = style "flex-shrink-0" [ Css.flex_shrink "0" ]
let flex_col = style "flex-col" [ flex_direction "column" ]
let flex_row = style "flex-row" [ flex_direction "row" ]
let flex_wrap = style "flex-wrap" [ Css.flex_wrap "wrap" ]
let flex_row_reverse = style "flex-row-reverse" [ flex_direction "row-reverse" ]

let flex_col_reverse =
  style "flex-col-reverse" [ flex_direction "column-reverse" ]

let flex_wrap_reverse =
  style "flex-wrap-reverse" [ Css.flex_wrap "wrap-reverse" ]

let flex_nowrap = style "flex-nowrap" [ Css.flex_wrap "nowrap" ]
let flex_1 = style "flex-1" [ Css.flex "1" ]
let flex_auto = style "flex-auto" [ Css.flex "1 1 auto" ]
let flex_initial = style "flex-initial" [ Css.flex "0 1 auto" ]
let flex_none = style "flex-none" [ Css.flex "none" ]
let flex_grow = style "flex-grow" [ Css.flex_grow "1" ]
let flex_grow_0 = style "flex-grow-0" [ Css.flex_grow "0" ]
let flex_shrink = style "flex-shrink" [ Css.flex_shrink "1" ]

(* center *)

let items_center = style "items-center" [ align_items "center" ]

let justify_between =
  style "justify-between" [ justify_content "space-between" ]

(** {1 Positioning} *)

let relative = style "relative" [ position "relative" ]
let absolute = style "absolute" [ position "absolute" ]
let fixed = style "fixed" [ position "fixed" ]
let sticky = style "sticky" [ position "sticky" ]

(* Borders *)

(* Modifiers *)

(** {1 CSS Generation} *)

let text_base =
  style "text-base"
    [
      font_size "var(--text-base)";
      line_height "var(--tw-leading, var(--text-base--line-height))";
    ]

let text_lg =
  style "text-lg"
    [
      font_size "var(--text-lg)";
      line_height "var(--tw-leading, var(--text-lg--line-height))";
    ]

let text_3xl =
  style "text-3xl"
    [
      font_size "var(--text-3xl)";
      line_height "var(--tw-leading, var(--text-3xl--line-height))";
    ]

let text_4xl =
  style "text-4xl"
    [
      font_size "var(--text-4xl)";
      line_height "var(--tw-leading, var(--text-4xl--line-height))";
    ]

let text_5xl =
  style "text-5xl"
    [
      font_size "var(--text-5xl)";
      line_height "var(--tw-leading, var(--text-5xl--line-height))";
    ]

let font_thin =
  style_with_vars "font-thin"
    [
      Css.property "--tw-font-weight" "var(--font-weight-thin)";
      font_weight "var(--font-weight-thin)";
    ]
    []

let font_light =
  style_with_vars "font-light"
    [
      Css.property "--tw-font-weight" "var(--font-weight-light)";
      font_weight "var(--font-weight-light)";
    ]
    []

let font_normal =
  style_with_vars "font-normal"
    [
      Css.property "--tw-font-weight" "var(--font-weight-normal)";
      font_weight "var(--font-weight-normal)";
    ]
    []

let font_medium =
  style_with_vars "font-medium"
    [
      Css.property "--tw-font-weight" "var(--font-weight-medium)";
      font_weight "var(--font-weight-medium)";
    ]
    []

let font_semibold =
  style_with_vars "font-semibold"
    [
      Css.property "--tw-font-weight" "var(--font-weight-semibold)";
      font_weight "var(--font-weight-semibold)";
    ]
    []

let font_bold =
  style_with_vars "font-bold"
    [
      Css.property "--tw-font-weight" "var(--font-weight-bold)";
      font_weight "var(--font-weight-bold)";
    ]
    []

let font_extrabold =
  style_with_vars "font-extrabold"
    [
      Css.property "--tw-font-weight" "var(--font-weight-extrabold)";
      font_weight "var(--font-weight-extrabold)";
    ]
    []

let font_black =
  style_with_vars "font-black"
    [
      Css.property "--tw-font-weight" "var(--font-weight-black)";
      font_weight "var(--font-weight-black)";
    ]
    []

(* Font family utilities *)
let font_sans = style "font-sans" [ font_family "var(--font-sans)" ]
let font_serif = style "font-serif" [ font_family "var(--font-serif)" ]
let font_mono = style "font-mono" [ font_family "var(--font-mono)" ]
let italic = style "italic" [ font_style "italic" ]
let not_italic = style "not-italic" [ font_style "normal" ]
let underline = style "underline" [ text_decoration "underline" ]
let line_through = style "line-through" [ text_decoration "line-through" ]
let no_underline = style "no-underline" [ text_decoration "none" ]
let text_left = style "text-left" [ text_align "left" ]
let text_right = style "text-right" [ text_align "right" ]
let text_justify = style "text-justify" [ text_align "justify" ]
let leading_none = style "leading-none" [ line_height "1" ]
let leading_tight = style "leading-tight" [ line_height "1.25" ]
let leading_snug = style "leading-snug" [ line_height "1.375" ]
let leading_normal = style "leading-normal" [ line_height "1.5" ]
let leading_relaxed = style "leading-relaxed" [ line_height "1.625" ]
let leading_loose = style "leading-loose" [ line_height "2" ]
let leading_6 = style "leading-6" [ line_height "1.5rem" ]
let tracking_tighter = style "tracking-tighter" [ letter_spacing "-0.05em" ]
let tracking_tight = style "tracking-tight" [ letter_spacing "-0.025em" ]
let tracking_normal = style "tracking-normal" [ letter_spacing "0" ]
let tracking_wide = style "tracking-wide" [ letter_spacing "0.025em" ]
let tracking_wider = style "tracking-wider" [ letter_spacing "0.05em" ]
let tracking_widest = style "tracking-widest" [ letter_spacing "0.1em" ]
let whitespace_normal = style "whitespace-normal" [ white_space "normal" ]
let whitespace_nowrap = style "whitespace-nowrap" [ white_space "nowrap" ]
let whitespace_pre = style "whitespace-pre" [ white_space "pre" ]
let whitespace_pre_line = style "whitespace-pre-line" [ white_space "pre-line" ]
let whitespace_pre_wrap = style "whitespace-pre-wrap" [ white_space "pre-wrap" ]
let inline_flex = style "inline-flex" [ display "inline-flex" ]
let grid = style "grid" [ display "grid" ]
let inline_grid = style "inline-grid" [ display "inline-grid" ]
let items_start = style "items-start" [ align_items "flex-start" ]
let items_end = style "items-end" [ align_items "flex-end" ]
let items_baseline = style "items-baseline" [ align_items "baseline" ]
let items_stretch = style "items-stretch" [ align_items "stretch" ]
let justify_start = style "justify-start" [ justify_content "flex-start" ]
let justify_end = style "justify-end" [ justify_content "flex-end" ]
let justify_center = style "justify-center" [ justify_content "center" ]
let justify_around = style "justify-around" [ justify_content "space-around" ]
let justify_evenly = style "justify-evenly" [ justify_content "space-evenly" ]

(* Align content utilities - for multi-line flex/grid containers *)
let content_start = style "content-start" [ Css.align_content "flex-start" ]
let content_end = style "content-end" [ Css.align_content "flex-end" ]
let content_center = style "content-center" [ Css.align_content "center" ]

let content_between =
  style "content-between" [ Css.align_content "space-between" ]

let content_around = style "content-around" [ Css.align_content "space-around" ]
let content_evenly = style "content-evenly" [ Css.align_content "space-evenly" ]
let content_stretch = style "content-stretch" [ Css.align_content "stretch" ]

(* Place content utilities - shorthand for align-content and justify-content in
   Grid *)
let place_content_start =
  style "place-content-start" [ Css.place_content "start" ]

let place_content_end = style "place-content-end" [ Css.place_content "end" ]

let place_content_center =
  style "place-content-center" [ Css.place_content "center" ]

let place_content_between =
  style "place-content-between" [ Css.place_content "space-between" ]

let place_content_around =
  style "place-content-around" [ Css.place_content "space-around" ]

let place_content_evenly =
  style "place-content-evenly" [ Css.place_content "space-evenly" ]

let place_content_stretch =
  style "place-content-stretch" [ Css.place_content "stretch" ]

(* Place items utilities - shorthand for align-items and justify-items in
   Grid *)
let place_items_start = style "place-items-start" [ Css.place_items "start" ]
let place_items_end = style "place-items-end" [ Css.place_items "end" ]
let place_items_center = style "place-items-center" [ Css.place_items "center" ]

let place_items_stretch =
  style "place-items-stretch" [ Css.place_items "stretch" ]

(* Place self utilities - shorthand for align-self and justify-self *)
let place_self_auto = style "place-self-auto" [ Css.place_self "auto" ]
let place_self_start = style "place-self-start" [ Css.place_self "start" ]
let place_self_end = style "place-self-end" [ Css.place_self "end" ]
let place_self_center = style "place-self-center" [ Css.place_self "center" ]
let place_self_stretch = style "place-self-stretch" [ Css.place_self "stretch" ]

(* Align self utilities *)
let self_auto = style "self-auto" [ Css.align_self "auto" ]
let self_start = style "self-start" [ Css.align_self "flex-start" ]
let self_end = style "self-end" [ Css.align_self "flex-end" ]
let self_center = style "self-center" [ Css.align_self "center" ]
let self_baseline = style "self-baseline" [ Css.align_self "baseline" ]
let self_stretch = style "self-stretch" [ Css.align_self "stretch" ]

(* Justify self utilities - for Grid items *)
let justify_self_auto = style "justify-self-auto" [ Css.justify_self "auto" ]
let justify_self_start = style "justify-self-start" [ Css.justify_self "start" ]
let justify_self_end = style "justify-self-end" [ Css.justify_self "end" ]

let justify_self_center =
  style "justify-self-center" [ Css.justify_self "center" ]

let justify_self_stretch =
  style "justify-self-stretch" [ Css.justify_self "stretch" ]

let grid_cols n =
  let class_name = "grid-cols-" ^ string_of_int n in
  style class_name
    [
      grid_template_columns ("repeat(" ^ string_of_int n ^ ", minmax(0, 1fr))");
    ]

let grid_rows n =
  let class_name = "grid-rows-" ^ string_of_int n in
  style class_name
    [ grid_template_rows ("repeat(" ^ string_of_int n ^ ", minmax(0, 1fr))") ]

let static = style "static" [ position "static" ]
let inset_0 = style "inset-0" [ top "0"; right "0"; bottom "0"; left "0" ]
let inset_x_0 = style "inset-x-0" [ left "0"; right "0" ]
let inset_y_0 = style "inset-y-0" [ bottom "0"; top "0" ]

let top n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "top-" ^ string_of_int (abs n) in
  let value = Pp.str [ "calc(var(--spacing) * "; string_of_int (abs n); ")" ] in
  style_with_vars class_name [ top value ] [ Css.Spacing (abs n) ]

let right n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "right-" ^ string_of_int (abs n) in
  let value = Pp.str [ "calc(var(--spacing) * "; string_of_int (abs n); ")" ] in
  style_with_vars class_name [ right value ] [ Css.Spacing (abs n) ]

let bottom n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "bottom-" ^ string_of_int (abs n) in
  let value = Pp.str [ "calc(var(--spacing) * "; string_of_int (abs n); ")" ] in
  style_with_vars class_name [ bottom value ] [ Css.Spacing (abs n) ]

let left n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "left-" ^ string_of_int (abs n) in
  let value = Pp.str [ "calc(var(--spacing) * "; string_of_int (abs n); ")" ] in
  style_with_vars class_name [ left value ] [ Css.Spacing (abs n) ]

let z n =
  let class_name = "z-" ^ string_of_int n in
  style class_name [ z_index (string_of_int n) ]

(* Inset utilities *)
let inset n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "inset-" ^ string_of_int (abs n) in
  let value = spacing_to_rem n in
  style class_name
    [ Css.top value; Css.right value; Css.bottom value; Css.left value ]

(* Fractional position utilities *)
let top_1_2 = style "top-1/2" [ Css.top "50%" ]
let left_1_2 = style "left-1/2" [ Css.left "50%" ]

let neg_translate_x_1_2 =
  style "-translate-x-1/2" [ transform "translateX(-50%)" ]

let neg_translate_y_1_2 =
  style "-translate-y-1/2" [ transform "translateY(-50%)" ]

type width = size

let border_internal (w : width) =
  let width_px, class_suffix =
    match w with
    | `None -> ("0", "-0")
    | `Xs -> ("1px", "" (* Default border is 1px *))
    | `Sm -> ("2px", "-2")
    | `Md -> ("4px", "-4" (* For borders, Md maps to 4px *))
    | `Lg -> ("4px", "-4")
    | `Xl -> ("8px", "-8")
    | `Xl_2 -> ("8px", "-8")
    | `Xl_3 -> ("8px", "-8")
    | `Full -> ("8px", "-8")
  in
  let class_name = "border" ^ class_suffix in
  style class_name
    [ border_style "var(--tw-border-style)"; border_width width_px ]

let border_none = border_internal `None
let border_xs = border_internal `Xs
let border = border_internal `Xs (* Default border is xs/1px *)
let border_sm = border_internal `Sm
let border_md = border_internal `Md
let border_lg = border_internal `Lg
let border_xl = border_internal `Xl
let border_2xl = border_internal `Xl_2
let border_3xl = border_internal `Xl_3
let border_full = border_internal `Full
let border_t = style "border-t" [ border_top_width "1px" ]
let border_r = style "border-r" [ border_right_width "1px" ]
let border_b = style "border-b" [ border_bottom_width "1px" ]
let border_l = style "border-l" [ border_left_width "1px" ]

(* Border styles *)
let border_solid =
  style "border-solid"
    [ Css.property "--tw-border-style" "solid"; border_style "solid" ]

let border_dashed =
  style "border-dashed"
    [ Css.property "--tw-border-style" "dashed"; border_style "dashed" ]

let border_dotted =
  style "border-dotted"
    [ Css.property "--tw-border-style" "dotted"; border_style "dotted" ]

let border_double =
  style "border-double"
    [ Css.property "--tw-border-style" "double"; border_style "double" ]

let border_none_style =
  style "border-none"
    [ Css.property "--tw-border-style" "none"; border_style "none" ]

let rounded_value : size -> string = function
  | `None -> "0"
  | `Sm -> "0.125rem"
  | `Md -> "0.375rem"
  | `Lg -> "0.5rem"
  | `Xl -> "0.75rem"
  | `Xl_2 -> "1rem"
  | `Xl_3 -> "1.5rem"
  | `Full -> "9999px"
  | `Xs -> "0.0625rem"

let pp_rounded_suffix : size -> string = function
  | `None -> "none"
  | `Sm -> "sm"
  | `Md -> "md"
  | `Lg -> "lg"
  | `Xl -> "xl"
  | `Xl_2 -> "2xl"
  | `Xl_3 -> "3xl"
  | `Full -> "full"
  | `Xs -> "xs"

let rounded_internal r =
  let class_name = "rounded-" ^ pp_rounded_suffix r in
  let var_name =
    match r with
    | `None -> "0" (* rounded-none uses 0 directly *)
    | `Full -> "3.40282e38px" (* rounded-full uses max float in v4 *)
    | _ -> "var(--radius-" ^ pp_rounded_suffix r ^ ")"
  in
  style class_name [ border_radius var_name ]

let rounded_none = rounded_internal `None
let rounded_sm = rounded_internal `Sm
let rounded = rounded_internal `Md (* Default rounded *)
let rounded_md = rounded_internal `Md
let rounded_lg = rounded_internal `Lg
let rounded_xl = rounded_internal `Xl
let rounded_2xl = rounded_internal `Xl_2
let rounded_3xl = rounded_internal `Xl_3
let rounded_full = rounded_internal `Full

let shadow_value : shadow -> string = function
  | `None -> "0 0 #0000"
  | `Sm ->
      "0 1px 3px 0 var(--tw-shadow-color, #0000001a), 0 1px 2px -1px \
       var(--tw-shadow-color, #0000001a)"
  | `Md ->
      "0 4px 6px -1px var(--tw-shadow-color, #0000001a), 0 2px 4px -2px \
       var(--tw-shadow-color, #0000001a)"
  | `Lg ->
      "0 10px 15px -3px var(--tw-shadow-color, #0000001a), 0 4px 6px -4px \
       var(--tw-shadow-color, #0000001a)"
  | `Xl ->
      "0 20px 25px -5px var(--tw-shadow-color, #0000001a), 0 8px 10px -6px \
       var(--tw-shadow-color, #0000001a)"
  | `Xl_2 -> "0 25px 50px -12px var(--tw-shadow-color, #00000040)"
  | `Inner -> "inset 0 2px 4px 0 var(--tw-shadow-color, #0000000f)"
  | `Xs -> "0 1px 1px 0 var(--tw-shadow-color, #0000000d)"
  | `Xl_3 -> "0 35px 60px -15px var(--tw-shadow-color, #00000059)"
  | `Full -> "0 0 0 0 #0000" (* no shadow, same as none *)

let pp_shadow_suffix : shadow -> string = function
  | `None -> "none"
  | `Sm -> "sm"
  | `Md -> "md"
  | `Lg -> "lg"
  | `Xl -> "xl"
  | `Xl_2 -> "2xl"
  | `Inner -> "inner"
  | `Xs -> "xs"
  | `Xl_3 -> "3xl"
  | `Full -> "full"

let shadow_colored_value : shadow -> string = function
  | `None -> "0 0 #0000"
  | `Sm -> "0 1px 2px 0 var(--tw-shadow-color)"
  | `Md ->
      "0 4px 6px -1px var(--tw-shadow-color),0 2px 4px -2px \
       var(--tw-shadow-color)"
  | `Lg ->
      "0 10px 15px -3px var(--tw-shadow-color),0 4px 6px -4px \
       var(--tw-shadow-color)"
  | `Xl ->
      "0 20px 25px -5px var(--tw-shadow-color),0 8px 10px -6px \
       var(--tw-shadow-color)"
  | `Xl_2 -> "0 25px 50px -12px var(--tw-shadow-color)"
  | `Inner -> "inset 0 2px 4px 0 var(--tw-shadow-color)"
  | `Xs -> "0 1px 1px 0 var(--tw-shadow-color)"
  | `Xl_3 -> "0 35px 60px -15px var(--tw-shadow-color)"
  | `Full -> "0 0 #0000"

let shadow_internal s =
  let class_name = "shadow-" ^ pp_shadow_suffix s in
  let custom_props = [ property "--tw-shadow" (shadow_value s) ] in
  let box_shadow_prop =
    box_shadow
      "var(--tw-inset-shadow), var(--tw-inset-ring-shadow), \
       var(--tw-ring-offset-shadow), var(--tw-ring-shadow), var(--tw-shadow)"
  in
  style class_name (custom_props @ [ box_shadow_prop ])

let shadow_none = shadow_internal `None
let shadow_sm = shadow_internal `Sm
let shadow = shadow_internal `Md (* Default shadow *)
let shadow_md = shadow_internal `Md
let shadow_lg = shadow_internal `Lg
let shadow_xl = shadow_internal `Xl
let shadow_2xl = shadow_internal `Xl_2
let shadow_inner = shadow_internal `Inner

let opacity n =
  let class_name = "opacity-" ^ string_of_int n in
  let value = if n = 0 then "0" else string_of_int n ^ "%" in
  style class_name [ opacity value ]

let transition_none = style "transition-none" [ transition "none" ]

let transition_all =
  style "transition-all" [ transition "all 150ms cubic-bezier(0.4, 0, 0.2, 1)" ]

let transition_colors =
  style "transition-colors"
    [
      transition
        "background-color, border-color, color, fill, stroke 150ms \
         cubic-bezier(0.4, 0, 0.2, 1)";
    ]

let transition_opacity =
  style "transition-opacity"
    [ transition "opacity 150ms cubic-bezier(0.4, 0, 0.2, 1)" ]

let transition_shadow =
  style "transition-shadow"
    [ transition "box-shadow 150ms cubic-bezier(0.4, 0, 0.2, 1)" ]

let transition_transform =
  style "transition-transform"
    [ transition "transform 150ms cubic-bezier(0.4, 0, 0.2, 1)" ]

let rotate n =
  let class_name = "rotate-" ^ string_of_int n in
  style class_name [ property "rotate" (string_of_int n ^ "deg") ]

let translate_x n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "translate-x-" ^ string_of_int (abs n) in
  style class_name [ transform ("translateX(" ^ spacing_to_rem n ^ ")") ]

let translate_y n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "translate-y-" ^ string_of_int (abs n) in
  style class_name [ transform ("translateY(" ^ spacing_to_rem n ^ ")") ]

(** 3D Transform utilities - inspired by modern CSS capabilities

    While Tailwind CSS traditionally focused on 2D transforms, modern CSS
    supports full 3D transformations. These utilities enable sophisticated
    animations and visual effects like card flips, 3D rotations, and depth. We
    include these as they represent useful CSS features that complement OCaml's
    approach to building interactive UIs. *)

let rotate_x n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "rotate-x-" ^ string_of_int (abs n) in
  style class_name [ transform ("rotateX(" ^ string_of_int n ^ "deg)") ]

let rotate_y n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "rotate-y-" ^ string_of_int (abs n) in
  style class_name [ transform ("rotateY(" ^ string_of_int n ^ "deg)") ]

let rotate_z n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "rotate-z-" ^ string_of_int (abs n) in
  style class_name [ transform ("rotateZ(" ^ string_of_int n ^ "deg)") ]

let translate_z n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "translate-z-" ^ string_of_int (abs n) in
  style class_name [ transform ("translateZ(" ^ string_of_int n ^ "px)") ]

let scale_z n =
  let value = float_of_int n /. 100.0 in
  let class_name = "scale-z-" ^ string_of_int n in
  style class_name
    [
      property "--tw-scale-z" (Pp.float value);
      transform
        "translate(var(--tw-translate-x), var(--tw-translate-y)) \
         translateZ(var(--tw-translate-z, 0)) rotate(var(--tw-rotate)) \
         rotateX(var(--tw-rotate-x, 0)) rotateY(var(--tw-rotate-y, 0)) \
         rotateZ(var(--tw-rotate-z, 0)) skewX(var(--tw-skew-x)) \
         skewY(var(--tw-skew-y)) scaleX(var(--tw-scale-x)) \
         scaleY(var(--tw-scale-y)) scaleZ(var(--tw-scale-z, 1))";
    ]

let perspective n =
  let class_name = "perspective-" ^ string_of_int n in
  let value = if n = 0 then "none" else string_of_int n ^ "px" in
  style class_name [ property "perspective" value ]

let perspective_origin_center =
  style "perspective-origin-center" [ property "perspective-origin" "center" ]

let perspective_origin_top =
  style "perspective-origin-top" [ property "perspective-origin" "top" ]

let perspective_origin_bottom =
  style "perspective-origin-bottom" [ property "perspective-origin" "bottom" ]

let perspective_origin_left =
  style "perspective-origin-left" [ property "perspective-origin" "left" ]

let perspective_origin_right =
  style "perspective-origin-right" [ property "perspective-origin" "right" ]

let transform_style_3d =
  style "transform-style-3d" [ property "transform-style" "preserve-3d" ]

let transform_style_flat =
  style "transform-style-flat" [ property "transform-style" "flat" ]

let backface_visible =
  style "backface-visible" [ property "backface-visibility" "visible" ]

let backface_hidden =
  style "backface-hidden" [ property "backface-visibility" "hidden" ]

(** Container query utilities - inspired by modern CSS capabilities

    Container queries allow elements to respond to their container's size rather
    than the viewport. This is particularly useful for component-based design
    where a component might be used in different sized containers. While
    Tailwind CSS v4 includes container queries, we implement them here as
    they're a valuable CSS feature that works well with OCaml's approach. *)
let container_type_size =
  style "container-type-size" [ property "container-type" "size" ]

let container_type_inline_size =
  style "container-type-inline-size" [ property "container-type" "inline-size" ]

let container_type_normal =
  style "container-type-normal" [ property "container-type" "normal" ]

let container_name name =
  style ("container-" ^ name) [ property "container-name" name ]

(* Container query breakpoints *)
let on_container_sm styles =
  Group (List.map (fun t -> Modified (Container Container_sm, t)) styles)

let on_container_md styles =
  Group (List.map (fun t -> Modified (Container Container_md, t)) styles)

let on_container_lg styles =
  Group (List.map (fun t -> Modified (Container Container_lg, t)) styles)

let on_container_xl styles =
  Group (List.map (fun t -> Modified (Container Container_xl, t)) styles)

let on_container_2xl styles =
  Group (List.map (fun t -> Modified (Container Container_2xl, t)) styles)

(* Named container queries - using on_container as the user suggested *)
let on_container ?name min_width styles =
  let query =
    match name with
    | None -> Container (Container_named ("", min_width))
    | Some n -> Container (Container_named (n, min_width))
  in
  Group (List.map (fun t -> Modified (query, t)) styles)

let cursor_auto = style "cursor-auto" [ cursor "auto" ]
let cursor_default = style "cursor-default" [ cursor "default" ]
let cursor_pointer = style "cursor-pointer" [ cursor "pointer" ]
let cursor_wait = style "cursor-wait" [ cursor "wait" ]
let cursor_move = style "cursor-move" [ cursor "move" ]
let cursor_not_allowed = style "cursor-not-allowed" [ cursor "not-allowed" ]
let select_none = style "select-none" [ user_select "none" ]
let select_text = style "select-text" [ user_select "text" ]
let select_all = style "select-all" [ user_select "all" ]
let select_auto = style "select-auto" [ user_select "auto" ]
let pointer_events_none = style "pointer-events-none" [ pointer_events "none" ]
let pointer_events_auto = style "pointer-events-auto" [ pointer_events "auto" ]
let outline_none = style "outline-none" [ outline "none" ]

let ring_internal (w : width) =
  let width, class_suffix =
    match w with
    | `None -> ("0", "0")
    | `Xs -> ("1px", "1")
    | `Sm -> ("2px", "2")
    | `Md -> ("3px", "" (* Default ring width is 3px *))
    | `Lg -> ("4px", "4")
    | `Xl -> ("8px", "8")
    | `Xl_2 -> ("8px", "8" (* Map Xl_2 to 8px as well *))
    | `Xl_3 -> ("8px", "8" (* Map Xl_3 to 8px as well *))
    | `Full -> ("8px", "8" (* Map Full to 8px as well *))
  in
  let class_name =
    if class_suffix = "" then "ring" else "ring-" ^ class_suffix
  in
  let shadow_value =
    if width = "0" then "0 0 #0000"
    else "0 0 0 " ^ width ^ " var(--tw-ring-color)"
  in
  style class_name
    [
      property "--tw-ring-color" "rgb(59 130 246 / 0.5)";
      box_shadow
        "var(--tw-ring-offset-shadow,0 0 #0000),var(--tw-ring-shadow,0 0 \
         #0000),var(--tw-shadow,0 0 #0000)";
      property "--tw-ring-shadow" shadow_value;
    ]

let ring_none = ring_internal `None
let ring_xs = ring_internal `Xs
let ring_sm = ring_internal `Sm
let ring = ring_internal `Md (* Default ring *)
let ring_md = ring_internal `Md
let ring_lg = ring_internal `Lg
let ring_xl = ring_internal `Xl

let ring_color color shade =
  let class_name =
    if Color.is_base_color color then Pp.str [ "ring-"; color_name color ]
    else Pp.str [ "ring-"; color_name color; "-"; string_of_int shade ]
  in
  style class_name []

let isolate = style "isolate" [ display "isolate" ]
let overflow_auto = style "overflow-auto" [ overflow "auto" ]
let overflow_hidden = style "overflow-hidden" [ overflow "hidden" ]
let overflow_visible = style "overflow-visible" [ overflow "visible" ]
let overflow_scroll = style "overflow-scroll" [ overflow "scroll" ]

(* Overflow variants *)
let overflow_x_auto = style "overflow-x-auto" [ overflow_x "auto" ]
let overflow_x_hidden = style "overflow-x-hidden" [ overflow_x "hidden" ]
let overflow_x_visible = style "overflow-x-visible" [ overflow_x "visible" ]
let overflow_x_scroll = style "overflow-x-scroll" [ overflow_x "scroll" ]
let overflow_y_auto = style "overflow-y-auto" [ overflow_y "auto" ]
let overflow_y_hidden = style "overflow-y-hidden" [ overflow_y "hidden" ]
let overflow_y_visible = style "overflow-y-visible" [ overflow_y "visible" ]
let overflow_y_scroll = style "overflow-y-scroll" [ overflow_y "scroll" ]

(* Scroll snap utilities *)
let snap_none = style "snap-none" [ scroll_snap_type "none" ]

let snap_x =
  style "snap-x" [ scroll_snap_type "x var(--tw-scroll-snap-strictness)" ]

let snap_y =
  style "snap-y" [ scroll_snap_type "y var(--tw-scroll-snap-strictness)" ]

let snap_both =
  style "snap-both" [ scroll_snap_type "both var(--tw-scroll-snap-strictness)" ]

let snap_mandatory =
  style "snap-mandatory" [ property "--tw-scroll-snap-strictness" "mandatory" ]

let snap_proximity =
  style "snap-proximity" [ property "--tw-scroll-snap-strictness" "proximity" ]

let snap_start = style "snap-start" [ scroll_snap_align "start" ]
let snap_end = style "snap-end" [ scroll_snap_align "end" ]
let snap_center = style "snap-center" [ scroll_snap_align "center" ]
let snap_align_none = style "snap-align-none" [ scroll_snap_align "none" ]
let snap_normal = style "snap-normal" [ scroll_snap_stop "normal" ]
let snap_always = style "snap-always" [ scroll_snap_stop "always" ]
let scroll_auto = style "scroll-auto" [ scroll_behavior "auto" ]
let scroll_smooth = style "scroll-smooth" [ scroll_behavior "smooth" ]
let object_contain = style "object-contain" [ object_fit "contain" ]
let object_cover = style "object-cover" [ object_fit "cover" ]
let object_fill = style "object-fill" [ object_fit "fill" ]
let object_none = style "object-none" [ object_fit "none" ]
let object_scale_down = style "object-scale-down" [ object_fit "scale-down" ]

let sr_only =
  style "sr-only"
    [
      position "absolute";
      width "1px";
      height "1px";
      padding "0";
      margin "-1px";
      overflow "hidden";
      clip "rect(0, 0, 0, 0)";
      white_space "nowrap";
      border_width "0";
    ]

let not_sr_only =
  style "not-sr-only"
    [
      position "static";
      width "auto";
      height "auto";
      padding "0";
      margin "0";
      overflow "visible";
      clip "auto";
      white_space "normal";
    ]

(* Responsive and state modifiers *)

let focus_visible =
  style "focus-visible"
    [ outline "2px solid transparent"; outline_offset "2px" ]

(* New on_ style modifiers that take lists *)
let on_hover styles = Group (List.map (fun t -> Modified (Hover, t)) styles)
let on_focus styles = Group (List.map (fun t -> Modified (Focus, t)) styles)
let on_active styles = Group (List.map (fun t -> Modified (Active, t)) styles)

let on_disabled styles =
  Group (List.map (fun t -> Modified (Disabled, t)) styles)

let on_group_hover styles =
  Group (List.map (fun t -> Modified (Group_hover, t)) styles)

let on_group_focus styles =
  Group (List.map (fun t -> Modified (Group_focus, t)) styles)

let on_dark styles = Group (List.map (fun t -> Modified (Dark, t)) styles)

(* New Tailwind v4 modifiers *)
(* The not_ function should be used with other modifiers, not standalone *)
let not_ modifier_fn =
  match modifier_fn with
  | Modified (mod_type, inner) -> Modified (Not mod_type, inner)
  | _ -> modifier_fn (* If not a modifier, return as-is *)

let has selector styles =
  Group (List.map (fun t -> Modified (Has selector, t)) styles)

let group_has selector styles =
  Group (List.map (fun t -> Modified (Group_has selector, t)) styles)

let peer_has selector styles =
  Group (List.map (fun t -> Modified (Peer_has selector, t)) styles)

let on_focus_within styles =
  Group (List.map (fun t -> Modified (Focus_within, t)) styles)

let on_focus_visible styles =
  Group (List.map (fun t -> Modified (Focus_visible, t)) styles)

let motion_safe styles =
  Group (List.map (fun t -> Modified (Motion_safe, t)) styles)

let motion_reduce styles =
  Group (List.map (fun t -> Modified (Motion_reduce, t)) styles)

let contrast_more styles =
  Group (List.map (fun t -> Modified (Contrast_more, t)) styles)

let contrast_less styles =
  Group (List.map (fun t -> Modified (Contrast_less, t)) styles)

let starting styles = Group (List.map (fun t -> Modified (Starting, t)) styles)

(* Additional on_* functions for consistency *)
let on_peer_hover styles =
  Group (List.map (fun t -> Modified (Peer_hover, t)) styles)

let on_peer_focus styles =
  Group (List.map (fun t -> Modified (Peer_focus, t)) styles)

let on_aria_disabled styles =
  Group (List.map (fun t -> Modified (Aria_disabled, t)) styles)

let on_data_active styles =
  Group (List.map (fun t -> Modified (Data_active, t)) styles)

let on_data_inactive styles =
  Group (List.map (fun t -> Modified (Data_inactive, t)) styles)

(* Check if a style already has a responsive modifier *)
let rec has_responsive_modifier = function
  | Style _ -> false
  | Prose _ -> false
  | Modified (Responsive _, _) -> true
  | Modified (_, t) -> has_responsive_modifier t
  | Group styles -> List.exists has_responsive_modifier styles

let validate_no_nested_responsive styles =
  List.iter
    (fun style ->
      if has_responsive_modifier style then
        failwith
          "Cannot apply responsive modifiers to styles that already have \
           responsive modifiers")
    styles

let on_sm styles =
  validate_no_nested_responsive styles;
  Group (List.map (fun t -> Modified (Responsive `Sm, t)) styles)

let on_md styles =
  validate_no_nested_responsive styles;
  Group (List.map (fun t -> Modified (Responsive `Md, t)) styles)

let on_lg styles =
  validate_no_nested_responsive styles;
  Group (List.map (fun t -> Modified (Responsive `Lg, t)) styles)

let on_xl styles =
  validate_no_nested_responsive styles;
  Group (List.map (fun t -> Modified (Responsive `Xl, t)) styles)

let on_2xl styles =
  validate_no_nested_responsive styles;
  Group (List.map (fun t -> Modified (Responsive `Xl_2, t)) styles)

let bg_gradient_to_b =
  style "bg-gradient-to-b"
    [ background_image "linear-gradient(to bottom, var(--tw-gradient-stops))" ]

let bg_gradient_to_br =
  style "bg-gradient-to-br"
    [
      background_image
        "linear-gradient(to bottom right, var(--tw-gradient-stops))";
    ]

let bg_gradient_to_t =
  style "bg-gradient-to-t"
    [ background_image "linear-gradient(to top, var(--tw-gradient-stops))" ]

let bg_gradient_to_tr =
  style "bg-gradient-to-tr"
    [
      background_image "linear-gradient(to top right, var(--tw-gradient-stops))";
    ]

let bg_gradient_to_r =
  style "bg-gradient-to-r"
    [ background_image "linear-gradient(to right, var(--tw-gradient-stops))" ]

let bg_gradient_to_bl =
  style "bg-gradient-to-bl"
    [
      background_image
        "linear-gradient(to bottom left, var(--tw-gradient-stops))";
    ]

let bg_gradient_to_l =
  style "bg-gradient-to-l"
    [ background_image "linear-gradient(to left, var(--tw-gradient-stops))" ]

let bg_gradient_to_tl =
  style "bg-gradient-to-tl"
    [
      background_image "linear-gradient(to top left, var(--tw-gradient-stops))";
    ]

(** Gradient color stops *)
let from_color ?(shade = 500) color =
  let class_name =
    if Color.is_base_color color || Color.is_custom_color color then
      "from-" ^ color_name color
    else Pp.str [ "from-"; color_name color; "-"; string_of_int shade ]
  in
  let color_var =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "var(--color-"; color_name color; ")" ]
    else
      Pp.str [ "var(--color-"; color_name color; "-"; string_of_int shade; ")" ]
  in
  style class_name
    [
      property "--tw-gradient-from" color_var;
      property "--tw-gradient-stops"
        "var(--tw-gradient-via-stops, var(--tw-gradient-position), \
         var(--tw-gradient-from) var(--tw-gradient-from-position), \
         var(--tw-gradient-to) var(--tw-gradient-to-position))";
    ]

let via_color ?(shade = 500) color =
  let class_name =
    if Color.is_base_color color || Color.is_custom_color color then
      "via-" ^ color_name color
    else Pp.str [ "via-"; color_name color; "-"; string_of_int shade ]
  in
  let color_var =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "var(--color-"; color_name color; ")" ]
    else
      Pp.str [ "var(--color-"; color_name color; "-"; string_of_int shade; ")" ]
  in
  style class_name
    [
      property "--tw-gradient-via" color_var;
      property "--tw-gradient-via-stops"
        "var(--tw-gradient-position), var(--tw-gradient-from) \
         var(--tw-gradient-from-position), var(--tw-gradient-via) \
         var(--tw-gradient-via-position), var(--tw-gradient-to) \
         var(--tw-gradient-to-position)";
      property "--tw-gradient-stops" "var(--tw-gradient-via-stops)";
    ]

let to_color ?(shade = 500) color =
  let class_name =
    if Color.is_base_color color || Color.is_custom_color color then
      "to-" ^ color_name color
    else Pp.str [ "to-"; color_name color; "-"; string_of_int shade ]
  in
  let color_var =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "var(--color-"; color_name color; ")" ]
    else
      Pp.str [ "var(--color-"; color_name color; "-"; string_of_int shade; ")" ]
  in
  style class_name
    [
      property "--tw-gradient-to" color_var;
      property "--tw-gradient-stops"
        "var(--tw-gradient-via-stops, var(--tw-gradient-position), \
         var(--tw-gradient-from) var(--tw-gradient-from-position), \
         var(--tw-gradient-to) var(--tw-gradient-to-position))";
    ]

let antialiased =
  style "antialiased"
    [ webkit_font_smoothing "antialiased"; moz_osx_font_smoothing "grayscale" ]

(* Text transformation utilities *)
let uppercase = style "uppercase" [ Css.text_transform "uppercase" ]
let lowercase = style "lowercase" [ Css.text_transform "lowercase" ]
let capitalize = style "capitalize" [ Css.text_transform "capitalize" ]
let normal_case = style "normal-case" [ Css.text_transform "none" ]

(* Text decoration style utilities *)
let underline_solid =
  style "underline-solid" [ Css.text_decoration_style "solid" ]

let underline_double =
  style "underline-double" [ Css.text_decoration_style "double" ]

let underline_dotted =
  style "underline-dotted" [ Css.text_decoration_style "dotted" ]

let underline_dashed =
  style "underline-dashed" [ Css.text_decoration_style "dashed" ]

let underline_wavy = style "underline-wavy" [ Css.text_decoration_style "wavy" ]

(* Text underline offset utilities *)
let underline_offset_auto =
  style "underline-offset-auto" [ Css.text_underline_offset "auto" ]

let underline_offset_0 =
  style "underline-offset-0" [ Css.text_underline_offset "0" ]

let underline_offset_1 =
  style "underline-offset-1" [ Css.text_underline_offset "1px" ]

let underline_offset_2 =
  style "underline-offset-2" [ Css.text_underline_offset "2px" ]

let underline_offset_4 =
  style "underline-offset-4" [ Css.text_underline_offset "4px" ]

let underline_offset_8 =
  style "underline-offset-8" [ Css.text_underline_offset "8px" ]

(* Additional functions needed *)
let aspect_ratio width height =
  let class_name =
    Pp.str [ "aspect-["; Pp.float width; "/"; Pp.float height; "]" ]
  in
  (* aspect-ratio isn't widely supported in CSS yet, skip for now *)
  style class_name []

let clip_path _value =
  (* clip-path is a modern CSS property, skip for now *)
  style "clip-path-custom" []

let transform =
  style "transform"
    [
      property "--tw-translate-x" "0";
      property "--tw-translate-y" "0";
      property "--tw-rotate" "0";
      property "--tw-skew-x" "0";
      property "--tw-skew-y" "0";
      property "--tw-scale-x" "1";
      property "--tw-scale-y" "1";
      transform
        "translateX(var(--tw-translate-x)) translateY(var(--tw-translate-y)) \
         rotate(var(--tw-rotate)) skewX(var(--tw-skew-x)) \
         skewY(var(--tw-skew-y)) scaleX(var(--tw-scale-x)) \
         scaleY(var(--tw-scale-y))";
    ]

let transform_none = style "transform-none" [ Css.transform "none" ]
let transform_gpu = style "transform-gpu" [ Css.transform "translateZ(0)" ]

let brightness n =
  let class_name = "brightness-" ^ string_of_int n in
  let value = Pp.float (float_of_int n /. 100.0) in
  style class_name [ filter ("brightness(" ^ value ^ ")") ]

let contrast n =
  let class_name = "contrast-" ^ string_of_int n in
  let value = Pp.float (float_of_int n /. 100.0) in
  style class_name [ filter ("contrast(" ^ value ^ ")") ]

let blur_internal = function
  | `None -> style "blur-none" [ filter "blur(0)" ]
  | `Xs -> style "blur-xs" [ filter "blur(2px)" ]
  | `Sm -> style "blur-sm" [ filter "blur(4px)" ]
  | `Md -> style "blur" [ filter "blur(8px)" ]
  | `Lg -> style "blur-lg" [ filter "blur(16px)" ]
  | `Xl -> style "blur-xl" [ filter "blur(24px)" ]
  | `Xl_2 -> style "blur-2xl" [ filter "blur(40px)" ]
  | `Xl_3 -> style "blur-3xl" [ filter "blur(64px)" ]
  | `Full -> style "blur-full" [ filter "blur(9999px)" ]

let blur_none = blur_internal `None
let blur_xs = blur_internal `Xs
let blur_sm = blur_internal `Sm
let blur = blur_internal `Md (* Default blur *)
let blur_md = blur_internal `Md
let blur_lg = blur_internal `Lg
let blur_xl = blur_internal `Xl
let blur_2xl = blur_internal `Xl_2
let blur_3xl = blur_internal `Xl_3

let grayscale n =
  let class_name = if n = 0 then "grayscale-0" else "grayscale" in
  let value = Pp.float (float_of_int n /. 100.0) in
  style class_name [ filter ("grayscale(" ^ value ^ ")") ]

let saturate n =
  let class_name = "saturate-" ^ string_of_int n in
  let value = Pp.float (float_of_int n /. 100.0) in
  style class_name [ filter ("saturate(" ^ value ^ ")") ]

let sepia n =
  let class_name = if n = 0 then "sepia-0" else "sepia" in
  let value = Pp.float (float_of_int n /. 100.0) in
  style class_name [ filter ("sepia(" ^ value ^ ")") ]

let invert n =
  let class_name = if n = 0 then "invert-0" else "invert" in
  let value = Pp.float (float_of_int n /. 100.0) in
  style class_name [ filter ("invert(" ^ value ^ ")") ]

let hue_rotate n =
  let class_name = "hue-rotate-" ^ string_of_int n in
  let value = string_of_int n ^ "deg" in
  style class_name [ filter ("hue-rotate(" ^ value ^ ")") ]

let backdrop_brightness n =
  let class_name = Pp.str [ "backdrop-brightness-"; string_of_int n ] in
  style class_name
    [
      backdrop_filter
        (Pp.str [ "brightness("; Pp.float (float_of_int n /. 100.); ")" ]);
    ]

let backdrop_contrast n =
  let class_name = Pp.str [ "backdrop-contrast-"; string_of_int n ] in
  style class_name
    [
      backdrop_filter
        (Pp.str [ "contrast("; Pp.float (float_of_int n /. 100.); ")" ]);
    ]

let backdrop_opacity n =
  let class_name = Pp.str [ "backdrop-opacity-"; string_of_int n ] in
  style class_name
    [
      backdrop_filter
        (Pp.str [ "opacity("; Pp.float (float_of_int n /. 100.); ")" ]);
    ]

let backdrop_saturate n =
  let class_name = Pp.str [ "backdrop-saturate-"; string_of_int n ] in
  style class_name
    [
      backdrop_filter
        (Pp.str [ "saturate("; Pp.float (float_of_int n /. 100.); ")" ]);
    ]

let backdrop_blur_internal = function
  | `None -> style "backdrop-blur-none" [ backdrop_filter "blur(0)" ]
  | `Xs -> style "backdrop-blur-xs" [ backdrop_filter "blur(2px)" ]
  | `Sm -> style "backdrop-blur-sm" [ backdrop_filter "blur(4px)" ]
  | `Md -> style "backdrop-blur" [ backdrop_filter "blur(8px)" ]
  | `Lg -> style "backdrop-blur-lg" [ backdrop_filter "blur(12px)" ]
  | `Xl -> style "backdrop-blur-xl" [ backdrop_filter "blur(16px)" ]
  | `Xl_2 -> style "backdrop-blur-2xl" [ backdrop_filter "blur(24px)" ]
  | `Xl_3 -> style "backdrop-blur-3xl" [ backdrop_filter "blur(40px)" ]
  | `Full -> style "backdrop-blur-full" [ backdrop_filter "blur(9999px)" ]

let backdrop_blur_none = backdrop_blur_internal `None
let backdrop_blur_xs = backdrop_blur_internal `Xs
let backdrop_blur_sm = backdrop_blur_internal `Sm
let backdrop_blur = backdrop_blur_internal `Md (* Default backdrop blur *)
let backdrop_blur_md = backdrop_blur_internal `Md
let backdrop_blur_lg = backdrop_blur_internal `Lg
let backdrop_blur_xl = backdrop_blur_internal `Xl
let backdrop_blur_2xl = backdrop_blur_internal `Xl_2
let backdrop_blur_3xl = backdrop_blur_internal `Xl_3

(* Animation utilities *)
let animate_none = style "animate-none" [ animation "none" ]
let animate_spin = style "animate-spin" [ animation "spin 1s linear infinite" ]

let animate_ping =
  style "animate-ping"
    [ animation "ping 1s cubic-bezier(0, 0, 0.2, 1) infinite" ]

let animate_pulse =
  style "animate-pulse"
    [ animation "pulse 2s cubic-bezier(0.4, 0, 0.6, 1) infinite" ]

let animate_bounce = style "animate-bounce" [ animation "bounce 1s infinite" ]

(* Transition utilities *)
let duration n =
  let class_name = "duration-" ^ string_of_int n in
  let value = string_of_int n ^ "ms" in
  style class_name [ property "transition-duration" value ]

let ease_linear =
  style "ease-linear" [ property "transition-timing-function" "linear" ]

let ease_in =
  style "ease-in"
    [ property "transition-timing-function" "cubic-bezier(0.4, 0, 1, 1)" ]

let ease_out =
  style "ease-out"
    [ property "transition-timing-function" "cubic-bezier(0, 0, 0.2, 1)" ]

let ease_in_out =
  style "ease-in-out"
    [ property "transition-timing-function" "cubic-bezier(0.4, 0, 0.2, 1)" ]

(** Transform utilities *)
let scale n =
  let value = string_of_int n ^ "%" in
  let class_name = "scale-" ^ string_of_int n in
  style class_name
    [
      property "--tw-scale-x" value;
      property "--tw-scale-y" value;
      Css.transform
        "translate(var(--tw-translate-x), var(--tw-translate-y)) \
         rotate(var(--tw-rotate)) skewX(var(--tw-skew-x)) \
         skewY(var(--tw-skew-y)) scaleX(var(--tw-scale-x)) \
         scaleY(var(--tw-scale-y))";
    ]

(* Appearance utilities *)
let appearance_none = style "appearance-none" [ appearance "none" ]

(* Resize utilities *)
let resize_none = style "resize-none" [ Css.resize "none" ]
let resize_y = style "resize-y" [ Css.resize "vertical" ]
let resize_x = style "resize-x" [ Css.resize "horizontal" ]
let resize = style "resize" [ Css.resize "both" ]

(* Will-change utilities *)
let will_change_auto =
  style "will-change-auto" [ property "will-change" "auto" ]

let will_change_scroll =
  style "will-change-scroll" [ property "will-change" "scroll-position" ]

let will_change_contents =
  style "will-change-contents" [ property "will-change" "contents" ]

let will_change_transform =
  style "will-change-transform" [ property "will-change" "transform" ]

(* Contain utilities *)
let contain_none = style "contain-none" [ property "contain" "none" ]
let contain_content = style "contain-content" [ property "contain" "content" ]
let contain_layout = style "contain-layout" [ property "contain" "layout" ]
let contain_paint = style "contain-paint" [ property "contain" "paint" ]
let contain_size = style "contain-size" [ property "contain" "size" ]

(* Object position utilities *)
let object_top = style "object-top" [ property "object-position" "top" ]
let object_right = style "object-right" [ property "object-position" "right" ]

let object_bottom =
  style "object-bottom" [ property "object-position" "bottom" ]

let object_left = style "object-left" [ property "object-position" "left" ]

let object_center =
  style "object-center" [ property "object-position" "center" ]

(* Table utilities *)
let table_auto = style "table-auto" [ table_layout "auto" ]
let table_fixed = style "table-fixed" [ table_layout "fixed" ]
let border_collapse = style "border-collapse" [ Css.border_collapse "collapse" ]
let border_separate = style "border-separate" [ Css.border_collapse "separate" ]

let border_spacing n =
  let value = spacing_to_rem n in
  style ("border-spacing-" ^ string_of_int n) [ border_spacing value ]

(* Form utilities - equivalent to @tailwindcss/forms plugin *)
let form_input =
  style "form-input"
    [
      appearance "none";
      background_color "white";
      Css.border_color "rgb(209 213 219)";
      border_width "1px";
      border_radius "0.375rem";
      padding_top "0.5rem";
      padding_right "0.75rem";
      padding_bottom "0.5rem";
      padding_left "0.75rem";
      font_size "1rem";
      line_height "1.5rem";
      property "outline" "2px solid transparent";
      property "outline-offset" "2px";
    ]

let form_textarea =
  style "form-textarea"
    [
      appearance "none";
      background_color "white";
      Css.border_color "rgb(209 213 219)";
      border_width "1px";
      border_radius "0.375rem";
      padding_top "0.5rem";
      padding_right "0.75rem";
      padding_bottom "0.5rem";
      padding_left "0.75rem";
      font_size "1rem";
      line_height "1.5rem";
      Css.resize "vertical";
    ]

let form_select =
  style "form-select"
    [
      appearance "none";
      background_color "white";
      Css.border_color "rgb(209 213 219)";
      border_width "1px";
      border_radius "0.375rem";
      padding_top "0.5rem";
      padding_right "2.5rem";
      padding_bottom "0.5rem";
      padding_left "0.75rem";
      font_size "1rem";
      line_height "1.5rem";
      background_image
        "url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' \
         fill='none' viewBox='0 0 20 20'%3e%3cpath stroke='%236b7280' \
         stroke-linecap='round' stroke-linejoin='round' stroke-width='1.5' \
         d='M6 8l4 4 4-4'/%3e%3c/svg%3e\")";
      background_position "right 0.5rem center";
      background_repeat "no-repeat";
      background_size "1.5em 1.5em";
    ]

let form_checkbox =
  style "form-checkbox"
    [
      appearance "none";
      width "1rem";
      height "1rem";
      background_color "white";
      Css.border_color "rgb(209 213 219)";
      border_width "1px";
      border_radius "0.25rem";
      Css.color "rgb(59 130 246)";
      Css.flex_shrink "0";
      display "inline-block";
      vertical_align "middle";
    ]

let form_radio =
  style "form-radio"
    [
      appearance "none";
      width "1rem";
      height "1rem";
      background_color "white";
      Css.border_color "rgb(209 213 219)";
      border_width "1px";
      border_radius "100%";
      Css.color "rgb(59 130 246)";
      Css.flex_shrink "0";
      display "inline-block";
      vertical_align "middle";
    ]

(* Peer and group utilities *)
let peer = style "peer" [] (* Marker class for peer relationships *)
let group = style "group" [] (* Marker class for group relationships *)

(* Peer modifiers *)
let on_peer_checked styles =
  Group (List.map (fun s -> Modified (Peer_checked, s)) styles)

(* ARIA state modifiers *)
let on_aria_checked styles =
  Group (List.map (fun s -> Modified (Aria_checked, s)) styles)

let on_aria_expanded styles =
  Group (List.map (fun s -> Modified (Aria_expanded, s)) styles)

let on_aria_selected styles =
  Group (List.map (fun s -> Modified (Aria_selected, s)) styles)

(* Data attribute modifiers *)
let data_state value style = Modified (Data_state value, style)
let data_variant value style = Modified (Data_variant value, style)
let data_custom key value style = Modified (Data_custom (key, value), style)
let color_to_string = color_name

(* Helper function for breakpoint conversion *)
let string_of_breakpoint = function
  | `Sm -> "sm"
  | `Md -> "md"
  | `Lg -> "lg"
  | `Xl -> "xl"
  | `Xl_2 -> "2xl"

(* Convert breakpoint to media query min-width *)
let responsive_breakpoint = function
  | "sm" -> "40rem" (* 640px / 16 = 40rem *)
  | "md" -> "48rem" (* 768px / 16 = 48rem *)
  | "lg" -> "64rem" (* 1024px / 16 = 64rem *)
  | "xl" -> "80rem" (* 1280px / 16 = 80rem *)
  | "2xl" -> "96rem" (* 1536px / 16 = 96rem *)
  | _ -> "48rem" (* Default to md *)

(* Class generation functions *)
let rec pp = function
  | Style { name = class_name; _ } -> class_name
  | Prose variant -> Prose.to_class variant
  | Modified (modifier, t) -> (
      let base_class = pp t in
      match modifier with
      | Hover -> "hover:" ^ base_class
      | Focus -> "focus:" ^ base_class
      | Active -> "active:" ^ base_class
      | Disabled -> "disabled:" ^ base_class
      | Group_hover -> "group-hover:" ^ base_class
      | Group_focus -> "group-focus:" ^ base_class
      | Peer_hover -> "peer-hover:" ^ base_class
      | Peer_focus -> "peer-focus:" ^ base_class
      | Peer_checked -> "peer-checked:" ^ base_class
      | Aria_checked -> "aria-checked:" ^ base_class
      | Aria_expanded -> "aria-expanded:" ^ base_class
      | Aria_selected -> "aria-selected:" ^ base_class
      | Aria_disabled -> "aria-disabled:" ^ base_class
      | Data_state value -> "data-[state=" ^ value ^ "]:" ^ base_class
      | Data_variant value -> "data-[variant=" ^ value ^ "]:" ^ base_class
      | Data_active -> "data-[active]:" ^ base_class
      | Data_inactive -> "data-[inactive]:" ^ base_class
      | Data_custom (key, value) ->
          "data-[" ^ key ^ "=" ^ value ^ "]:" ^ base_class
      | Dark -> "dark:" ^ base_class
      | Responsive breakpoint ->
          string_of_breakpoint breakpoint ^ ":" ^ base_class
      | Container query ->
          container_query_to_class_prefix query ^ ":" ^ base_class
      (* New v4 modifiers *)
      | Not _modifier -> "not-" ^ base_class (* Simplified for class names *)
      | Has selector -> "has-[" ^ selector ^ "]:" ^ base_class
      | Group_has selector -> "group-has-[" ^ selector ^ "]:" ^ base_class
      | Peer_has selector -> "peer-has-[" ^ selector ^ "]:" ^ base_class
      | Starting -> "starting:" ^ base_class
      | Focus_within -> "focus-within:" ^ base_class
      | Focus_visible -> "focus-visible:" ^ base_class
      | Motion_safe -> "motion-safe:" ^ base_class
      | Motion_reduce -> "motion-reduce:" ^ base_class
      | Contrast_more -> "contrast-more:" ^ base_class
      | Contrast_less -> "contrast-less:" ^ base_class)
  | Group styles -> styles |> List.map pp |> String.concat " "

let to_classes styles = styles |> List.map pp |> String.concat " "
let classes_to_string = to_classes

(* to_inline_style is now included from Core module *)

(* Prose utilities for beautiful typography *)
let prose = Prose Base
let prose_sm = Prose Sm
let prose_lg = Prose Lg
let prose_xl = Prose Xl
let prose_2xl = Prose Xl2
let prose_gray = Prose Gray
let prose_slate = Prose Slate

(* Expose Prose module for convenient access *)
module Prose = Prose

(* Generate complete prose stylesheet *)
let prose_stylesheet () =
  (* Generate CSS for all prose variants *)
  let all_variants =
    [
      Prose.Base;
      Prose.Sm;
      Prose.Lg;
      Prose.Xl;
      Prose.Xl2;
      Prose.Gray;
      Prose.Slate;
    ]
  in
  let all_rules = List.concat_map Prose.to_css_rules all_variants in
  (* Add CSS variables to root *)
  let root_rule = Css.rule ~selector:":root" Prose.css_variables in
  Css.stylesheet (root_rule :: all_rules)

(* Line clamp utility function *)
let line_clamp n =
  let class_name = "line-clamp-" ^ string_of_int n in
  if n = 0 then style "line-clamp-none" [ webkit_line_clamp "none" ]
  else style class_name [ webkit_line_clamp (string_of_int n) ]

(* Opacity utilities *)

(* Helper parsing functions *)
let color_of_string = function
  | "black" -> Ok black
  | "white" -> Ok white
  | "gray" -> Ok gray
  | "slate" -> Ok slate
  | "zinc" -> Ok zinc
  | "red" -> Ok red
  | "orange" -> Ok orange
  | "amber" -> Ok amber
  | "yellow" -> Ok yellow
  | "lime" -> Ok lime
  | "green" -> Ok green
  | "emerald" -> Ok emerald
  | "teal" -> Ok teal
  | "cyan" -> Ok cyan
  | "sky" -> Ok sky
  | "blue" -> Ok blue
  | "indigo" -> Ok indigo
  | "violet" -> Ok violet
  | "purple" -> Ok purple
  | "fuchsia" -> Ok fuchsia
  | "pink" -> Ok pink
  | "rose" -> Ok rose
  | color -> Error (`Msg ("Unknown color: " ^ color))

let text_size_of_string = function
  | "xs" -> Ok text_xs
  | "sm" -> Ok text_sm
  | "base" -> Ok text_base
  | "lg" -> Ok text_lg
  | "xl" -> Ok text_xl
  | "2xl" -> Ok text_2xl
  | "3xl" -> Ok text_3xl
  | "4xl" -> Ok text_4xl
  | "5xl" -> Ok text_5xl
  | s -> Error (`Msg ("Unknown text size: " ^ s))

let shadow_of_string = function
  | "none" -> Ok shadow_none
  | "sm" -> Ok shadow_sm
  | "md" -> Ok shadow_md
  | "lg" -> Ok shadow_lg
  | "xl" -> Ok shadow_xl
  | "2xl" -> Ok shadow_2xl
  | "inner" -> Ok shadow_inner
  | "" -> Ok shadow (* default shadow *)
  | s -> Error (`Msg ("Unknown shadow size: " ^ s))

let int_of_string_positive name s =
  match int_of_string_opt s with
  | None -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))
  | Some n when n >= 0 -> Ok n
  | Some _ -> Error (`Msg (name ^ " must be non-negative: " ^ s))

let int_of_string_bounded name min max s =
  match int_of_string_opt s with
  | None -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))
  | Some n when n >= min && n <= max -> Ok n
  | Some _ ->
      Error
        (`Msg
           (Pp.str
              [
                name;
                " must be between ";
                string_of_int min;
                " and ";
                string_of_int max;
                ": ";
                s;
              ]))

let leading_of_string n =
  match float_of_string_opt n with
  | None -> Error (`Msg ("Invalid leading value: " ^ n))
  | Some value ->
      let rem_value = value /. 4.0 in
      let class_name = Pp.str [ "leading-"; n ] in
      let css_value = Pp.str [ Pp.float rem_value; "rem" ] in
      Ok (style class_name [ line_height css_value ])

(* Helper for Result.bind-like operation *)
let ( >>= ) r f = match r with Error _ as e -> e | Ok x -> f x

(* Helper for "try this or else try that" *)
let ( <|> ) r1 r2 = match r1 with Ok _ -> r1 | Error _ -> r2

(* Helper for Result.map-like operation *)
let ( >|= ) r f = match r with Error _ as e -> e | Ok x -> Ok (f x)

(* Helper to parse shade from string *)
let shade_of_string s =
  match int_of_string_opt s with
  | None -> Error (`Msg ("Invalid shade: " ^ s))
  | Some shade -> Ok shade

(* Parse modifiers (responsive, states) from class string *)
let modifiers_of_string class_str =
  let parts = String.split_on_char ':' class_str in
  match List.rev parts with
  | [] -> ([], class_str)
  | base_class :: modifiers -> (List.rev modifiers, base_class)

(* Apply modifiers to a base style *)
let apply_modifiers modifiers base_style =
  List.fold_left
    (fun acc modifier ->
      match modifier with
      | "sm" -> (
          match acc with
          | Group styles -> on_sm styles
          | single -> on_sm [ single ])
      | "md" -> (
          match acc with
          | Group styles -> on_md styles
          | single -> on_md [ single ])
      | "lg" -> (
          match acc with
          | Group styles -> on_lg styles
          | single -> on_lg [ single ])
      | "xl" -> (
          match acc with
          | Group styles -> on_xl styles
          | single -> on_xl [ single ])
      | "2xl" -> (
          match acc with
          | Group styles -> on_2xl styles
          | single -> on_2xl [ single ])
      | "hover" -> (
          match acc with
          | Group styles -> on_hover styles
          | single -> on_hover [ single ])
      | "focus" -> (
          match acc with
          | Group styles -> on_focus styles
          | single -> on_focus [ single ])
      | "active" -> (
          match acc with
          | Group styles -> on_active styles
          | single -> on_active [ single ])
      | "disabled" -> (
          match acc with
          | Group styles -> on_disabled styles
          | single -> on_disabled [ single ])
      | "dark" -> (
          match acc with
          | Group styles -> on_dark styles
          | single -> on_dark [ single ])
      | _ -> acc (* ignore unknown modifiers for now *))
    base_style modifiers

(* Helper functions for parsing *)
let spacing_of_string prefix px_var full_var int_fn = function
  | [ p; "px" ] when p = prefix -> Ok px_var
  | [ p; "full" ] when p = prefix -> Ok full_var
  | [ p; n ] when p = prefix ->
      let name =
        if prefix = "p" then "padding"
        else if prefix = "px" then "padding-x"
        else if prefix = "py" then "padding-y"
        else "padding-" ^ String.sub prefix 1 (String.length prefix - 1)
      in
      int_of_string_positive name n >|= int_fn
  | _ -> Error (`Msg "")

let margin_of_string prefix auto_var int_fn = function
  | [ p; "auto" ] when p = prefix -> Ok auto_var
  | [ p; n ] when p = prefix ->
      let name =
        if prefix = "m" then "margin"
        else if prefix = "mx" then "margin-x"
        else if prefix = "my" then "margin-y"
        else "margin-" ^ String.sub prefix 1 (String.length prefix - 1)
      in
      int_of_string_positive name n >|= int_fn
  | _ -> Error (`Msg "")

let width_of_string = function
  | [ "w"; "full" ] -> Ok w_full
  | [ "w"; "screen" ] -> Ok (w' screen)
  | [ "w"; "min" ] -> Ok (w' min)
  | [ "w"; "max" ] -> Ok (w' max)
  | [ "w"; "fit" ] -> Ok w_fit
  | [ "w"; "px" ] -> Ok (w' one_px)
  | [ "w"; "auto" ] -> Ok (w' none)
  | [ "w"; n ] -> int_of_string_positive "width" n >|= w
  | _ -> Error (`Msg "")

let height_of_string = function
  | [ "h"; "full" ] -> Ok h_full
  | [ "h"; "screen" ] -> Ok (h' screen)
  | [ "h"; "min" ] -> Ok (h' min)
  | [ "h"; "max" ] -> Ok (h' max)
  | [ "h"; "fit" ] -> Ok (h' fit)
  | [ "h"; "px" ] -> Ok (h' one_px)
  | [ "h"; "auto" ] -> Ok (h' none)
  | [ "h"; n ] -> int_of_string_positive "height" n >|= h
  | _ -> Error (`Msg "")

let gap_of_string = function
  | [ "gap"; "px" ] -> Ok (gap' `Px)
  | [ "gap"; "full" ] -> Ok (gap' `Full)
  | [ "gap"; n ] -> int_of_string_positive "gap" n >|= gap
  | [ "gap"; "x"; "px" ] -> Ok (gap_x' `Px)
  | [ "gap"; "x"; "full" ] -> Ok (gap_x' `Full)
  | [ "gap"; "x"; n ] -> int_of_string_positive "gap-x" n >|= gap_x
  | [ "gap"; "y"; "px" ] -> Ok (gap_y' `Px)
  | [ "gap"; "y"; "full" ] -> Ok (gap_y' `Full)
  | [ "gap"; "y"; n ] -> int_of_string_positive "gap-y" n >|= gap_y
  | _ -> Error (`Msg "")

let min_width_of_string = function
  | [ "min"; "w"; "full" ] -> Ok (min_w' full)
  | [ "min"; "w"; "min" ] -> Ok (min_w' min)
  | [ "min"; "w"; "max" ] -> Ok (min_w' max)
  | [ "min"; "w"; "fit" ] -> Ok (min_w' fit)
  | [ "min"; "w"; "px" ] -> Ok (min_w' one_px)
  | [ "min"; "w"; n ] -> int_of_string_positive "min-width" n >|= min_w
  | _ -> Error (`Msg "")

let min_height_of_string = function
  | [ "min"; "h"; "full" ] -> Ok (min_h' full)
  | [ "min"; "h"; "screen" ] -> Ok (min_h' screen)
  | [ "min"; "h"; "min" ] -> Ok (min_h' min)
  | [ "min"; "h"; "max" ] -> Ok (min_h' max)
  | [ "min"; "h"; "fit" ] -> Ok (min_h' fit)
  | [ "min"; "h"; "px" ] -> Ok (min_h' one_px)
  | [ "min"; "h"; n ] -> int_of_string_positive "min-height" n >|= min_h
  | _ -> Error (`Msg "")

let max_width_of_string = function
  | [ "max"; "w"; "none" ] -> Ok (max_w' none)
  | [ "max"; "w"; "xs" ] -> Ok (max_w' xs)
  | [ "max"; "w"; "sm" ] -> Ok (max_w' sm)
  | [ "max"; "w"; "md" ] -> Ok (max_w' md)
  | [ "max"; "w"; "lg" ] -> Ok (max_w' lg)
  | [ "max"; "w"; "xl" ] -> Ok (max_w' xl)
  | [ "max"; "w"; "2xl" ] -> Ok (max_w' xl_2)
  | [ "max"; "w"; "3xl" ] -> Ok (max_w' xl_3)
  | [ "max"; "w"; "4xl" ] -> Ok (max_w' xl_4)
  | [ "max"; "w"; "5xl" ] -> Ok (max_w' xl_5)
  | [ "max"; "w"; "6xl" ] -> Ok (max_w' xl_6)
  | [ "max"; "w"; "7xl" ] -> Ok (max_w' xl_7)
  | [ "max"; "w"; "full" ] -> Ok (max_w' full)
  | [ "max"; "w"; "min" ] -> Ok (max_w' min)
  | [ "max"; "w"; "max" ] -> Ok (max_w' max)
  | [ "max"; "w"; "fit" ] -> Ok (max_w' fit)
  | [ "max"; "w"; "px" ] -> Ok (max_w' one_px)
  | [ "max"; "w"; n ] -> int_of_string_positive "max-width" n >|= max_w
  | _ -> Error (`Msg "")

let max_height_of_string = function
  | [ "max"; "h"; "full" ] -> Ok (max_h' full)
  | [ "max"; "h"; "screen" ] -> Ok (max_h' screen)
  | [ "max"; "h"; "min" ] -> Ok (max_h' min)
  | [ "max"; "h"; "max" ] -> Ok (max_h' max)
  | [ "max"; "h"; "fit" ] -> Ok (max_h' fit)
  | [ "max"; "h"; "px" ] -> Ok (max_h' one_px)
  | [ "max"; "h"; "none" ] -> Ok (max_h' none)
  | [ "max"; "h"; n ] -> int_of_string_positive "max-height" n >|= max_h
  | _ -> Error (`Msg "")

(* Parse color-related classes *)
let color_classes_of_string = function
  | [ "bg"; "transparent" ] -> Ok bg_transparent
  | [ "bg"; "current" ] -> Ok bg_current
  | [ "bg"; color; shade ] ->
      color_of_string color >>= fun color ->
      shade_of_string shade >|= fun shade -> bg color shade
  | [ "bg"; color ] -> color_of_string color >|= fun color -> bg color 500
  | [ "text"; "transparent" ] -> Ok text_transparent
  | [ "text"; "current" ] -> Ok text_current
  | [ "text"; "center" ] -> Ok text_center
  | [ "text"; "left" ] -> Ok text_left
  | [ "text"; "right" ] -> Ok text_right
  | [ "text"; "justify" ] -> Ok text_justify
  | [ "text"; color; shade ] ->
      color_of_string color >>= fun color ->
      shade_of_string shade >|= fun shade -> text color shade
  | [ "text"; single ] ->
      (* Try size first, then color *)
      text_size_of_string single
      <|> (color_of_string single >|= fun color -> text color 500)
  | [ "border" ] -> Ok border
  | [ "border"; color; shade ] ->
      color_of_string color >>= fun color ->
      shade_of_string shade >|= fun shade -> border_color color shade
  | [ "border"; "transparent" ] -> Ok border_transparent
  | [ "border"; "current" ] -> Ok border_current
  | [ "border"; color ] -> color_of_string color >|= fun c -> border_color c 500
  | _ -> Error (`Msg "")

(* Parse layout and typography classes *)
let layout_typography_of_string = function
  | [ "flex" ] -> Ok flex
  | [ "flex"; "col" ] -> Ok flex_col
  | [ "flex"; "row" ] -> Ok flex_row
  | [ "flex"; "wrap" ] -> Ok flex_wrap
  | [ "flex"; "nowrap" ] -> Ok flex_nowrap
  | [ "flex"; "1" ] -> Ok flex_1
  | [ "flex"; "auto" ] -> Ok flex_auto
  | [ "flex"; "initial" ] -> Ok flex_initial
  | [ "flex"; "none" ] -> Ok flex_none
  | [ "block" ] -> Ok block
  | [ "inline" ] -> Ok inline
  | [ "inline"; "block" ] -> Ok inline_block
  | [ "inline"; "grid" ] -> Ok inline_grid
  | [ "grid" ] -> Ok grid
  | [ "grid"; "cols"; n ] -> int_of_string_positive "grid cols" n >|= grid_cols
  | [ "grid"; "rows"; n ] -> int_of_string_positive "grid rows" n >|= grid_rows
  | [ "hidden" ] -> Ok hidden
  | [ "items"; "center" ] -> Ok items_center
  | [ "items"; "start" ] -> Ok items_start
  | [ "items"; "end" ] -> Ok items_end
  | [ "items"; "stretch" ] -> Ok items_stretch
  | [ "items"; "baseline" ] -> Ok items_baseline
  | [ "justify"; "center" ] -> Ok justify_center
  | [ "justify"; "start" ] -> Ok justify_start
  | [ "justify"; "end" ] -> Ok justify_end
  | [ "justify"; "between" ] -> Ok justify_between
  | [ "justify"; "around" ] -> Ok justify_around
  | [ "justify"; "evenly" ] -> Ok justify_evenly
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
  | [ "underline" ] -> Ok underline
  | [ "no"; "underline" ] -> Ok no_underline
  | [ "leading"; "none" ] -> Ok leading_none
  | [ "leading"; "tight" ] -> Ok leading_tight
  | [ "leading"; "snug" ] -> Ok leading_snug
  | [ "leading"; "normal" ] -> Ok leading_normal
  | [ "leading"; "relaxed" ] -> Ok leading_relaxed
  | [ "leading"; "loose" ] -> Ok leading_loose
  | [ "leading"; n ] -> leading_of_string n
  | _ -> Error (`Msg "")

(* Parse utility and effect classes *)
let utility_classes_of_string = function
  | [ "rounded" ] -> Ok rounded
  | [ "rounded"; size ] -> (
      match size with
      | "none" -> Ok rounded_none
      | "sm" -> Ok rounded_sm
      | "md" -> Ok rounded_md
      | "lg" -> Ok rounded_lg
      | "xl" -> Ok rounded_xl
      | "2xl" -> Ok rounded_2xl
      | "3xl" -> Ok rounded_3xl
      | "full" -> Ok rounded_full
      | s -> Error (`Msg ("Unknown rounded size: " ^ s)))
  | [ "shadow" ] -> shadow_of_string ""
  | [ "shadow"; size ] -> shadow_of_string size
  | [ "ring" ] -> Ok ring
  | [ "ring"; "0" ] -> Ok ring_none
  | [ "ring"; "1" ] -> Ok ring_xs
  | [ "ring"; "2" ] -> Ok ring_sm
  | [ "ring"; "3" ] -> Ok ring_md
  | [ "ring"; "4" ] -> Ok ring_lg
  | [ "ring"; "8" ] -> Ok ring_xl
  | [ "relative" ] -> Ok relative
  | [ "absolute" ] -> Ok absolute
  | [ "fixed" ] -> Ok fixed
  | [ "sticky" ] -> Ok sticky
  | [ "static" ] -> Ok static
  | [ "opacity"; n ] -> int_of_string_bounded "Opacity" 0 100 n >|= opacity
  | [ "transition" ] -> Ok transition_all
  | [ "transition"; "none" ] -> Ok transition_none
  | [ "transition"; "all" ] -> Ok transition_all
  | [ "transition"; "colors" ] -> Ok transition_colors
  | [ "transition"; "opacity" ] -> Ok transition_opacity
  | [ "transition"; "shadow" ] -> Ok transition_shadow
  | [ "transition"; "transform" ] -> Ok transition_transform
  | [ "duration"; n ] -> int_of_string_positive "duration" n >|= duration
  | [ "scale"; n ] -> int_of_string_positive "scale" n >|= scale
  | [ "prose" ] -> Ok prose
  | [ "prose"; "sm" ] -> Ok prose_sm
  | [ "prose"; "lg" ] -> Ok prose_lg
  | [ "prose"; "xl" ] -> Ok prose_xl
  | [ "prose"; "2xl" ] -> Ok prose_2xl
  | [ "prose"; "gray" ] -> Ok prose_gray
  | [ "prose"; "slate" ] -> Ok prose_slate
  | _ -> Error (`Msg "")

(* Parse a single class string into a Tw.t *)
let of_string class_str =
  let modifiers, base_class = modifiers_of_string class_str in
  let parts = String.split_on_char '-' base_class in
  let base_result =
    (* Try color classes first *)
    color_classes_of_string parts
    <|>
    (* Try spacing/sizing with helper functions *)
    (match parts with
    | "p" :: _ -> spacing_of_string "p" p_px p_full p parts
    | "px" :: _ -> spacing_of_string "px" px_px px_full px parts
    | "py" :: _ -> spacing_of_string "py" py_px py_full py parts
    | "pt" :: _ -> spacing_of_string "pt" pt_px pt_full pt parts
    | "pr" :: _ -> spacing_of_string "pr" pr_px pr_full pr parts
    | "pb" :: _ -> spacing_of_string "pb" pb_px pb_full pb parts
    | "pl" :: _ -> spacing_of_string "pl" pl_px pl_full pl parts
    | "m" :: _ -> margin_of_string "m" m_auto m parts
    | "mx" :: _ -> margin_of_string "mx" mx_auto mx parts
    | "my" :: _ -> margin_of_string "my" my_auto my parts
    | "mt" :: _ -> margin_of_string "mt" mt_auto mt parts
    | "mr" :: _ -> margin_of_string "mr" mr_auto mr parts
    | "mb" :: _ -> margin_of_string "mb" mb_auto mb parts
    | "ml" :: _ -> margin_of_string "ml" ml_auto ml parts
    | "w" :: _ -> width_of_string parts
    | "h" :: _ -> height_of_string parts
    | "gap" :: _ -> gap_of_string parts
    | "min" :: "w" :: _ -> min_width_of_string parts
    | "min" :: "h" :: _ -> min_height_of_string parts
    | "max" :: "w" :: _ -> max_width_of_string parts
    | "max" :: "h" :: _ -> max_height_of_string parts
    | _ -> Error (`Msg ""))
    <|>
    (* Try layout and typography classes *)
    layout_typography_of_string parts
    <|>
    (* Try utility classes *)
    utility_classes_of_string parts
    <|>
    (* Unknown class *)
    Error (`Msg ("Unknown class: " ^ class_str))
  in
  match base_result with
  | Error _ as e -> e
  | Ok base_style -> Ok (apply_modifiers modifiers base_style)
