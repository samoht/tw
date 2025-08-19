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
  | Group_focus
  | Dark
  | Responsive of breakpoint
  | Peer_hover
  | Peer_focus
  | Peer_checked
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

(** Tailwind CSS variable types - specific to Tailwind's design system *)
type var = Color of { name : string; shade : int option } | Spacing of int

let color_var ?shade name = Color { name; shade }

(** A Tailwind utility class with its name, CSS properties, and required
    variables *)
type t =
  | Style of { name : string; props : Css.declaration list; vars : var list }
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

(* Helper to convert breakpoint to string *)
let string_of_breakpoint = function
  | `Sm -> "sm"
  | `Md -> "md"
  | `Lg -> "lg"
  | `Xl -> "xl"
  | `Xl_2 -> "2xl"

(* Helper to convert container query to CSS prefix *)
let container_query_to_css_prefix = function
  | Container_sm -> "@container (min-width:24rem)"
  | Container_md -> "@container (min-width:28rem)"
  | Container_lg -> "@container (min-width:32rem)"
  | Container_xl -> "@container (min-width:36rem)"
  | Container_2xl -> "@container (min-width:42rem)"
  | Container_named ("", width) ->
      Pp.str [ "@container (min-width:"; string_of_int width; "px)" ]
  | Container_named (name, width) ->
      Pp.str [ "@container "; name; " (min-width:"; string_of_int width; "px)" ]

(* Generate CSS variables from the collected requirements *)
let container_query_to_class_prefix = function
  | Container_sm -> "@sm"
  | Container_md -> "@md"
  | Container_lg -> "@lg"
  | Container_xl -> "@xl"
  | Container_2xl -> "@2xl"
  | Container_named ("", width) -> "@" ^ string_of_int width ^ "px"
  | Container_named (name, width) -> "@" ^ name ^ "/" ^ string_of_int width

(* Helper to get breakpoint for responsive prefix *)
let responsive_breakpoint = function
  | "sm" -> "40rem" (* 640px / 16 = 40rem *)
  | "md" -> "48rem" (* 768px / 16 = 48rem *)
  | "lg" -> "64rem" (* 1024px / 16 = 64rem *)
  | "xl" -> "80rem" (* 1280px / 16 = 80rem *)
  | "2xl" -> "96rem" (* 1536px / 16 = 96rem *)
  | _ -> "0rem"

(* Type to represent structured CSS rule output *)
type rule_output =
  | Regular of string * Css.declaration list (* selector, properties *)
  | MediaQuery of
      string
      * string
      * Css.declaration list (* condition, selector, properties *)
  | ContainerQuery of
      string
      * string
      * Css.declaration list (* condition, selector, properties *)
  | StartingStyle of string * Css.declaration list (* selector, properties *)

(* Helper to escape special characters in CSS class names *)
let escape_class_name name =
  (* Escape special characters in arbitrary values *)
  let buf = Buffer.create (String.length name * 2) in
  String.iter
    (function
      | '[' -> Buffer.add_string buf "\\["
      | ']' -> Buffer.add_string buf "\\]"
      | '(' -> Buffer.add_string buf "\\("
      | ')' -> Buffer.add_string buf "\\)"
      | ',' -> Buffer.add_string buf "\\,"
      | c -> Buffer.add_char buf c)
    name;
  Buffer.contents buf

(* Extract selector and properties from a single Tw style *)
let extract_selector_props tw =
  let rec extract = function
    | Style { name = class_name; props; _ } ->
        let escaped_name = escape_class_name class_name in
        [ Regular ("." ^ escaped_name, props) ]
    | Prose variant ->
        (* Convert prose rules to selector/props pairs *)
        Prose.to_css_rules variant
        |> List.map (fun rule ->
               Regular (Css.selector rule, Css.declarations rule))
    | Modified (modifier, t) ->
        let base = extract t in
        List.concat_map
          (fun rule_out ->
            match rule_out with
            | Regular (selector, props) -> (
                let base_class =
                  String.sub selector 1 (String.length selector - 1)
                in
                match modifier with
                | Hover ->
                    [ Regular (".hover\\:" ^ base_class ^ ":hover", props) ]
                | Focus ->
                    [ Regular (".focus\\:" ^ base_class ^ ":focus", props) ]
                | Active ->
                    [ Regular (".active\\:" ^ base_class ^ ":active", props) ]
                | Disabled ->
                    [
                      Regular (".disabled\\:" ^ base_class ^ ":disabled", props);
                    ]
                | Group_hover ->
                    [
                      Regular
                        (".group:hover .group-hover\\:" ^ base_class, props);
                    ]
                | Group_focus ->
                    [
                      Regular
                        (".group:focus .group-focus\\:" ^ base_class, props);
                    ]
                | Peer_hover ->
                    [
                      Regular
                        (".peer:hover ~ .peer-hover\\:" ^ base_class, props);
                    ]
                | Peer_focus ->
                    [
                      Regular
                        (".peer:focus ~ .peer-focus\\:" ^ base_class, props);
                    ]
                | Peer_checked ->
                    [
                      Regular
                        (".peer:checked ~ .peer-checked\\:" ^ base_class, props);
                    ]
                | Aria_checked ->
                    [
                      Regular
                        ( ".aria-checked\\:" ^ base_class
                          ^ "[aria-checked=\"true\"]",
                          props );
                    ]
                | Aria_expanded ->
                    [
                      Regular
                        ( ".aria-expanded\\:" ^ base_class
                          ^ "[aria-expanded=\"true\"]",
                          props );
                    ]
                | Aria_selected ->
                    [
                      Regular
                        ( ".aria-selected\\:" ^ base_class
                          ^ "[aria-selected=\"true\"]",
                          props );
                    ]
                | Aria_disabled ->
                    [
                      Regular
                        ( ".aria-disabled\\:" ^ base_class
                          ^ "[aria-disabled=\"true\"]",
                          props );
                    ]
                | Data_state value ->
                    [
                      Regular
                        (selector ^ "[data-state=\"" ^ value ^ "\"]", props);
                    ]
                | Data_variant value ->
                    [
                      Regular
                        (selector ^ "[data-variant=\"" ^ value ^ "\"]", props);
                    ]
                | Data_active ->
                    [
                      Regular
                        ( ".data-\\[active\\]\\:" ^ base_class ^ "[data-active]",
                          props );
                    ]
                | Data_inactive ->
                    [
                      Regular
                        ( ".data-\\[inactive\\]\\:" ^ base_class
                          ^ "[data-inactive]",
                          props );
                    ]
                | Data_custom (key, value) ->
                    [
                      Regular
                        ( selector ^ "[data-" ^ key ^ "=\"" ^ value ^ "\"]",
                          props );
                    ]
                | Dark ->
                    [
                      MediaQuery
                        ("(prefers-color-scheme: dark)", selector, props);
                    ]
                | Responsive breakpoint ->
                    let prefix = string_of_breakpoint breakpoint in
                    let condition =
                      "(min-width:" ^ responsive_breakpoint prefix ^ ")"
                    in
                    let sel = "." ^ prefix ^ "\\:" ^ base_class in
                    [ MediaQuery (condition, sel, props) ]
                | Container query ->
                    let prefix = container_query_to_class_prefix query in
                    let escaped_class = ".\\" ^ prefix ^ "\\:" ^ base_class in
                    let condition = container_query_to_css_prefix query in
                    (* Extract just the condition part after @container *)
                    let cond =
                      if String.starts_with ~prefix:"@container " condition then
                        String.sub condition 11 (String.length condition - 11)
                      else "(min-width: 0)"
                    in
                    [ ContainerQuery (cond, escaped_class, props) ]
                (* New v4 modifiers *)
                | Not modifier -> (
                    (* Recursively apply the Not modifier *)
                    let inner_rules =
                      extract (Modified (modifier, style base_class []))
                    in
                    match inner_rules with
                    | Regular (inner_sel, _) :: _ ->
                        let cleaned =
                          String.sub inner_sel 1 (String.length inner_sel - 1)
                        in
                        [
                          Regular
                            ( ".not-" ^ cleaned ^ ":not(" ^ inner_sel ^ ")",
                              props );
                        ]
                    | _ -> [ Regular (selector, props) ])
                | Has selector_str ->
                    [
                      Regular
                        ( ".has-\\[" ^ selector_str ^ "\\]\\:" ^ base_class
                          ^ ":has(" ^ selector_str ^ ")",
                          props );
                    ]
                | Group_has selector_str ->
                    [
                      Regular
                        ( ".group:has(" ^ selector_str ^ ") .group-has-\\["
                          ^ selector_str ^ "\\]\\:" ^ base_class,
                          props );
                    ]
                | Peer_has selector_str ->
                    [
                      Regular
                        ( ".peer:has(" ^ selector_str ^ ") ~ .peer-has-\\["
                          ^ selector_str ^ "\\]\\:" ^ base_class,
                          props );
                    ]
                | Starting -> [ StartingStyle ("." ^ base_class, props) ]
                | Focus_within ->
                    [
                      Regular
                        ( ".focus-within\\:" ^ base_class ^ ":focus-within",
                          props );
                    ]
                | Focus_visible ->
                    [
                      Regular
                        ( ".focus-visible\\:" ^ base_class ^ ":focus-visible",
                          props );
                    ]
                | Motion_safe ->
                    [
                      MediaQuery
                        ( "(prefers-reduced-motion: no-preference)",
                          ".motion-safe\\:" ^ base_class,
                          props );
                    ]
                | Motion_reduce ->
                    [
                      MediaQuery
                        ( "(prefers-reduced-motion: reduce)",
                          ".motion-reduce\\:" ^ base_class,
                          props );
                    ]
                | Contrast_more ->
                    [
                      MediaQuery
                        ( "(prefers-contrast: more)",
                          ".contrast-more\\:" ^ base_class,
                          props );
                    ]
                | Contrast_less ->
                    [
                      MediaQuery
                        ( "(prefers-contrast: less)",
                          ".contrast-less\\:" ^ base_class,
                          props );
                    ])
            | _ -> [ rule_out ])
          base
    | Group styles -> List.concat_map extract styles
  in
  extract tw

(* Group properties by selector for Regular rules *)
let group_regular_rules rules =
  List.fold_left
    (fun acc rule ->
      match rule with
      | Regular (selector, props) ->
          let existing = try List.assoc selector acc with Not_found -> [] in
          let without = List.remove_assoc selector acc in
          without @ [ (selector, existing @ props) ]
      | _ -> acc)
    [] rules

(* Base reset CSS rules *)
(* Exact match with Tailwind v4.1.11 base reset order *)
let generate_reset_rules () =
  [
    (* Universal reset *)
    Css.rule ~selector:"*, :after, :before, ::backdrop"
      [
        Css.box_sizing Border_box;
        Css.border "0 solid";
        Css.margin Zero;
        Css.padding Zero;
      ];
    (* File selector button - first occurrence *)
    Css.rule ~selector:"::file-selector-button"
      [
        Css.box_sizing Border_box;
        Css.border "0 solid";
        Css.margin Zero;
        Css.padding Zero;
      ];
    (* HTML and host *)
    Css.rule ~selector:"html, :host"
      [
        Css.webkit_text_size_adjust "100%";
        (* TODO: should take typed value *)
        Css.tab_size "4";
        Css.line_height (Num 1.5);
        Css.font_family
          [
            Css.Var
              {
                name = "default-font-family";
                fallback =
                  Some
                    [
                      Css.Ui_sans_serif;
                      Css.System_ui;
                      Css.Sans_serif;
                      Css.Apple_color_emoji;
                      Css.Segoe_ui_emoji;
                      Css.Segoe_ui_symbol;
                      Css.Noto_color_emoji;
                    ];
              };
          ];
        Css.font_feature_settings "var(--default-font-feature-settings, normal)";
        Css.font_variation_settings
          "var(--default-font-variation-settings, normal)";
        Css.webkit_tap_highlight_color "transparent";
      ];
    (* Horizontal rule *)
    Css.rule ~selector:"hr"
      [ Css.height Zero; Css.color Inherit; Css.border_top_width (Px 1) ];
    (* Abbreviations *)
    Css.rule ~selector:"abbr:where([title])"
      [
        Css.webkit_text_decoration "underline dotted";
        (* text-decoration with multiple values *)
        Css.text_decoration Underline;
      ];
    (* Headings *)
    Css.rule ~selector:"h1, h2, h3, h4, h5, h6"
      [ Css.font_size Inherit; Css.font_weight Inherit ];
    (* Links *)
    Css.rule ~selector:"a"
      [
        Css.color Inherit;
        Css.webkit_text_decoration "inherit";
        (* TODO: should take Inherit constructor *)
        Css.text_decoration Inherit;
      ];
    (* Bold elements *)
    Css.rule ~selector:"b, strong" [ Css.font_weight Bolder ];
    (* Code elements *)
    Css.rule ~selector:"code, kbd, samp, pre"
      [
        Css.font_family
          [
            Css.Var
              {
                name = "default-mono-font-family";
                fallback =
                  Some
                    [
                      Css.Ui_monospace;
                      Css.SFMono_regular;
                      Css.Menlo;
                      Css.Monaco;
                      Css.Consolas;
                      Css.Liberation_mono;
                      Css.Courier_new;
                      Css.Monospace;
                    ];
              };
          ];
        Css.font_feature_settings
          "var(--default-mono-font-feature-settings, normal)";
        Css.font_variation_settings
          "var(--default-mono-font-variation-settings, normal)";
        Css.font_size (Em 1.0);
      ];
    (* Small text *)
    Css.rule ~selector:"small" [ Css.font_size (Pct 80.0) ];
    (* Sub and sup *)
    Css.rule ~selector:"sub, sup"
      [
        Css.vertical_align Baseline;
        Css.font_size (Pct 75.0);
        Css.line_height Zero;
        Css.position Relative;
      ];
    Css.rule ~selector:"sub" [ Css.bottom (Em (-0.25)) ];
    Css.rule ~selector:"sup" [ Css.top (Em (-0.5)) ];
    (* Table *)
    Css.rule ~selector:"table"
      [
        Css.text_indent Zero;
        Css.border_color Inherit;
        Css.border_collapse Collapse;
      ];
    (* Firefox focusring *)
    Css.rule ~selector:":-moz-focusring" [ Css.outline "auto" ];
    (* Progress *)
    Css.rule ~selector:"progress" [ Css.vertical_align Baseline ];
    (* Summary *)
    Css.rule ~selector:"summary" [ Css.display List_item ];
    (* Lists *)
    Css.rule ~selector:"ol, ul, menu" [ Css.list_style "none" ];
    (* Media elements *)
    Css.rule ~selector:"img, svg, video, canvas, audio, iframe, embed, object"
      [ Css.vertical_align Middle; Css.display Block ];
    Css.rule ~selector:"img, video"
      [ Css.max_width (Pct 100.0); Css.height Auto ];
    (* Form elements *)
    Css.rule ~selector:"button, input, select, optgroup, textarea"
      [
        Css.font "inherit";
        Css.font_feature_settings "inherit";
        Css.font_variation_settings "inherit";
        Css.letter_spacing Inherit;
        Css.color Inherit;
        Css.opacity 1.0;
        Css.background_color Transparent;
        Css.border_radius Zero;
      ];
    (* File selector button - second occurrence with font properties *)
    Css.rule ~selector:"::file-selector-button"
      [
        Css.font "inherit";
        Css.font_feature_settings "inherit";
        Css.font_variation_settings "inherit";
        Css.letter_spacing Inherit;
        Css.color Inherit;
        Css.opacity 1.0;
        Css.background_color Transparent;
        Css.border_radius Zero;
      ];
    (* Select with optgroup *)
    Css.rule ~selector:":where(select:is([multiple], [size])) optgroup"
      [ Css.font_weight Bolder ];
    Css.rule ~selector:":where(select:is([multiple], [size])) optgroup option"
      [ Css.padding_inline_start (Px 20) ];
    (* File selector button - third occurrence with margin *)
    Css.rule ~selector:"::file-selector-button" [ Css.margin_inline_end (Px 4) ];
    (* Placeholder - basic *)
    Css.rule ~selector:"::placeholder" [ Css.opacity 1.0 ];
    (* Textarea *)
    Css.rule ~selector:"textarea" [ Css.resize Vertical ];
    (* Search decoration *)
    Css.rule ~selector:"::-webkit-search-decoration"
      [ Css.webkit_appearance "none" ];
    (* Webkit datetime inputs *)
    Css.rule ~selector:"::-webkit-date-and-time-value"
      [
        Css.min_height (Em 1.0);
        (* 1lh approximated as 1em *)
        Css.text_align Inherit;
      ];
    Css.rule ~selector:"::-webkit-datetime-edit" [ Css.display Inline_flex ];
    Css.rule ~selector:"::-webkit-datetime-edit-fields-wrapper"
      [ Css.padding Zero ];
    Css.rule ~selector:"::-webkit-datetime-edit" [ Css.padding_block Zero ];
    Css.rule ~selector:"::-webkit-datetime-edit-year-field"
      [ Css.padding_block Zero ];
    Css.rule ~selector:"::-webkit-datetime-edit-month-field"
      [ Css.padding_block Zero ];
    Css.rule ~selector:"::-webkit-datetime-edit-day-field"
      [ Css.padding_block Zero ];
    Css.rule ~selector:"::-webkit-datetime-edit-hour-field"
      [ Css.padding_block Zero ];
    Css.rule ~selector:"::-webkit-datetime-edit-minute-field"
      [ Css.padding_block Zero ];
    Css.rule ~selector:"::-webkit-datetime-edit-second-field"
      [ Css.padding_block Zero ];
    Css.rule ~selector:"::-webkit-datetime-edit-millisecond-field"
      [ Css.padding_block Zero ];
    Css.rule ~selector:"::-webkit-datetime-edit-meridiem-field"
      [ Css.padding_block Zero ];
    (* Firefox-specific *)
    Css.rule ~selector:":-moz-ui-invalid" [ Css.box_shadow "none" ];
    (* Button-like inputs *)
    Css.rule
      ~selector:
        "button, input:where([type=button], [type=reset], [type=submit])"
      [ Css.appearance Button ];
    (* File selector button - fourth occurrence with appearance *)
    Css.rule ~selector:"::file-selector-button" [ Css.appearance Button ];
    (* Webkit spin buttons *)
    Css.rule ~selector:"::-webkit-inner-spin-button" [ Css.height Auto ];
    Css.rule ~selector:"::-webkit-outer-spin-button" [ Css.height Auto ];
    (* Hidden elements *)
    Css.rule ~selector:"[hidden]:where(:not([hidden=until-found]))"
      [ Css.display None ];
    (* TODO: needs !important flag *)
    (* Placeholder styling with @supports - added as a raw rule string for now *)
    (* This needs to be added as part of the base layer but after the main rules *)
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
  let all_rules = tw_classes |> List.concat_map extract_selector_props in
  (* Separate rules by type *)
  let regular_rules, media_rules, container_rules, _starting_rules =
    List.fold_left
      (fun (reg, media, cont, start) rule ->
        match rule with
        | Regular _ -> (rule :: reg, media, cont, start)
        | MediaQuery _ -> (reg, rule :: media, cont, start)
        | ContainerQuery _ -> (reg, media, rule :: cont, start)
        | StartingStyle _ -> (reg, media, cont, rule :: start))
      ([], [], [], []) all_rules
  in

  (* Group regular rules by selector *)
  let grouped_regular = group_regular_rules regular_rules in

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
    List.partition (fun (sel, _) -> not (is_hover_rule sel)) grouped_regular
  in

  (* Create regular CSS rules *)
  let rules =
    List.map
      (fun (selector, props) ->
        Css.rule ~selector (Css.deduplicate_declarations props))
      non_hover_rules
  in

  (* Group media query rules by their condition *)
  let media_queries_map =
    List.fold_left
      (fun acc rule ->
        match rule with
        | MediaQuery (condition, selector, props) ->
            let rules = try List.assoc condition acc with Not_found -> [] in
            (condition, (selector, props) :: rules)
            :: List.remove_assoc condition acc
        | _ -> acc)
      [] media_rules
  in

  (* Add hover rules to media query map *)
  let media_queries_map =
    if hover_rules = [] then media_queries_map
    else
      let hover_condition = "(hover:hover)" in
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
              Css.rule ~selector:sel (Css.deduplicate_declarations props))
            rule_list
        in
        Css.media ~condition rules)
      media_queries_map
  in

  (* Group container query rules by their condition *)
  let container_queries_map =
    List.fold_left
      (fun acc rule ->
        match rule with
        | ContainerQuery (condition, selector, props) ->
            let rules = try List.assoc condition acc with Not_found -> [] in
            (condition, (selector, props) :: rules)
            :: List.remove_assoc condition acc
        | _ -> acc)
      [] container_rules
  in

  (* Create container query objects *)
  let container_queries =
    List.map
      (fun (condition, rule_list) ->
        let rules =
          List.map
            (fun (sel, props) ->
              Css.rule ~selector:sel (Css.deduplicate_declarations props))
            rule_list
        in
        Css.container ~condition rules)
      container_queries_map
  in

  (* Build the complete stylesheet with layers - just like Tailwind v4 *)
  if reset then
    (* Extract which Tailwind CSS variables are actually used *)
    (* Collect all properties from all styles to analyze variable usage *)
    let rec collect_all_props style =
      match style with
      | Style { props; _ } -> props
      | Modified (_, t) -> collect_all_props t
      | Group styles -> List.concat_map collect_all_props styles
      | Prose _ -> []
    in

    let all_props = tw_classes |> List.concat_map collect_all_props in

    (* Analyze variable usage to determine what properties layer is needed *)
    let var_tally = Var.analyze_properties all_props in

    (* Properties layer - only if needed based on composition groups *)
    let properties_layer_opt =
      let properties = Var.generate_properties_layer var_tally in
      if properties <> [] then
        let css_props =
          List.map
            (fun (var, value) -> Css.custom_property var value)
            properties
        in
        Some
          (Css.layered_rules ~layer:Css.Properties
             ~supports_queries:
               [
                 Css.supports
                   ~condition:
                     "(((-webkit-hyphens:none)) and (not \
                      (margin-trim:inline))) or ((-moz-orient:inline) and (not \
                      (color:rgb(from red r g b))))"
                   [
                     Css.rule ~selector:"*, :before, :after, ::backdrop"
                       css_props;
                   ];
               ]
             [])
      else None
    in

    (* Theme layer with CSS variables - JIT mode (only used variables) *)
    (* Extract all CSS variables referenced in properties *)
    let directly_referenced_vars =
      tw_classes
      |> List.concat_map (fun tw ->
             let selector_props = extract_selector_props tw in
             List.concat_map
               (fun rule ->
                 match rule with
                 | Regular (_, props)
                 | MediaQuery (_, _, props)
                 | ContainerQuery (_, _, props)
                 | StartingStyle (_, props) ->
                     Css.all_vars props)
               selector_props)
      |> List.sort_uniq String.compare
    in

    (* Theme variables that always exist and may reference other variables *)
    let default_font_props =
      [
        Css.custom_property "--default-font-family" "var(--font-sans)";
        Css.custom_property "--default-mono-font-family" "var(--font-mono)";
      ]
    in

    (* Recursively resolve variable dependencies *)
    (* This function takes a list of properties and finds all vars they reference *)
    let rec resolve_all_var_deps props_to_analyze seen_vars =
      let new_vars =
        props_to_analyze
        |> List.concat_map (fun prop -> Css.all_vars [ prop ])
        |> List.filter (fun v -> not (List.mem v seen_vars))
        |> List.sort_uniq String.compare
      in
      if new_vars = [] then seen_vars
      else
        (* For each new var, we need to check if we need to generate a property
           for it *)
        let new_props =
          new_vars
          |> List.filter_map (fun var ->
                 match var with
                 | "--font-sans" ->
                     Some
                       (Css.custom_property "--font-sans"
                          "ui-sans-serif, system-ui, sans-serif, \"Apple Color \
                           Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \
                           \"Noto Color Emoji\"")
                 | "--font-serif" ->
                     Some
                       (Css.custom_property "--font-serif"
                          "ui-serif, Georgia, Cambria, \"Times New Roman\", \
                           Times, serif")
                 | "--font-mono" ->
                     Some
                       (Css.custom_property "--font-mono"
                          "ui-monospace, SFMono-Regular, Menlo, Monaco, \
                           Consolas, \"Liberation Mono\", \"Courier New\", \
                           monospace")
                 | _ -> None)
        in
        resolve_all_var_deps new_props (seen_vars @ new_vars)
    in

    (* Start with default font props and resolve their dependencies *)
    let vars_from_defaults = resolve_all_var_deps default_font_props [] in

    (* Combine all referenced variables *)
    let all_referenced_vars =
      directly_referenced_vars @ vars_from_defaults
      |> List.sort_uniq String.compare
    in

    (* Generate values for theme variables *)
    let theme_generated_vars =
      all_referenced_vars
      |> List.concat_map (fun var_name ->
             match var_name with
             | "--spacing" -> [ Css.custom_property "--spacing" "0.25rem" ]
             | "--text-xs" ->
                 [
                   Css.custom_property "--text-xs" "0.75rem";
                   Css.custom_property "--text-xs--line-height" "calc(1/.75)";
                 ]
             | "--text-sm" ->
                 [
                   Css.custom_property "--text-sm" "0.875rem";
                   Css.custom_property "--text-sm--line-height"
                     "calc(1.25/.875)";
                 ]
             | "--text-base" ->
                 [
                   Css.custom_property "--text-base" "1rem";
                   Css.custom_property "--text-base--line-height" "calc(1.5/1)";
                 ]
             | "--text-lg" ->
                 [
                   Css.custom_property "--text-lg" "1.125rem";
                   Css.custom_property "--text-lg--line-height"
                     "calc(1.75/1.125)";
                 ]
             | "--text-xl" ->
                 [
                   Css.custom_property "--text-xl" "1.25rem";
                   Css.custom_property "--text-xl--line-height"
                     "calc(1.75/1.25)";
                 ]
             | "--text-2xl" ->
                 [
                   Css.custom_property "--text-2xl" "1.5rem";
                   Css.custom_property "--text-2xl--line-height" "calc(2/1.5)";
                 ]
             | "--text-3xl" ->
                 [
                   Css.custom_property "--text-3xl" "1.875rem";
                   Css.custom_property "--text-3xl--line-height"
                     "calc(2.25/1.875)";
                 ]
             | "--text-4xl" ->
                 [
                   Css.custom_property "--text-4xl" "2.25rem";
                   Css.custom_property "--text-4xl--line-height"
                     "calc(2.5/2.25)";
                 ]
             | "--text-5xl" ->
                 [
                   Css.custom_property "--text-5xl" "3rem";
                   Css.custom_property "--text-5xl--line-height" "1";
                 ]
             | "--font-weight-thin" ->
                 [ Css.custom_property "--font-weight-thin" "100" ]
             | "--font-weight-light" ->
                 [ Css.custom_property "--font-weight-light" "300" ]
             | "--font-weight-normal" ->
                 [ Css.custom_property "--font-weight-normal" "400" ]
             | "--font-weight-medium" ->
                 [ Css.custom_property "--font-weight-medium" "500" ]
             | "--font-weight-semibold" ->
                 [ Css.custom_property "--font-weight-semibold" "600" ]
             | "--font-weight-bold" ->
                 [ Css.custom_property "--font-weight-bold" "700" ]
             | "--font-weight-extrabold" ->
                 [ Css.custom_property "--font-weight-extrabold" "800" ]
             | "--font-weight-black" ->
                 [ Css.custom_property "--font-weight-black" "900" ]
             | "--radius-sm" -> [ Css.custom_property "--radius-sm" ".25rem" ]
             | "--radius-md" -> [ Css.custom_property "--radius-md" ".375rem" ]
             | "--radius-lg" -> [ Css.custom_property "--radius-lg" ".5rem" ]
             | "--radius-xl" -> [ Css.custom_property "--radius-xl" ".75rem" ]
             | "--radius-2xl" -> [ Css.custom_property "--radius-2xl" "1rem" ]
             | "--radius-3xl" -> [ Css.custom_property "--radius-3xl" "1.5rem" ]
             | var_name when String.starts_with ~prefix:"--color-" var_name -> (
                 (* Handle color variables *)
                 let color_part =
                   String.sub var_name 8 (String.length var_name - 8)
                 in
                 (* Check if it's a base color (no shade) or with shade *)
                 match String.split_on_char '-' color_part with
                 | [ color_name ] ->
                     (* Base color like --color-white or --color-black *)
                     if color_name = "white" then
                       [ Css.custom_property "--color-white" "#fff" ]
                     else if color_name = "black" then
                       [ Css.custom_property "--color-black" "#000" ]
                     else [] (* Other base colors need shade *)
                 | color_parts -> (
                     (* Try to extract shade from the end *)
                     match List.rev color_parts with
                     | shade_str :: rev_color_parts -> (
                         try
                           let shade = int_of_string shade_str in
                           let color_name =
                             String.concat "-" (List.rev rev_color_parts)
                           in
                           let color = Color.of_string color_name in
                           [
                             Css.custom_property var_name
                               (Color.to_oklch_css color shade);
                           ]
                         with _ -> [])
                     | _ -> []))
             | _ -> [])
      |> List.sort_uniq compare
    in

    (* Determine which font variables to include based on what's referenced *)
    let font_names_to_include =
      all_referenced_vars
      |> List.filter_map (fun var ->
             match var with
             | "--font-sans" -> Some ("sans", var)
             | "--font-serif" -> Some ("serif", var)
             | "--font-mono" -> Some ("mono", var)
             | _ -> None)
    in

    (* Canonical Tailwind order for built-in fonts *)
    let canonical_font_order name =
      match name with "sans" -> 0 | "serif" -> 1 | "mono" -> 2 | _ -> 1000
    in

    (* Sort by canonical order *)
    let font_names_to_include =
      font_names_to_include
      |> List.sort_uniq (fun (a, _) (b, _) ->
             let order_a = canonical_font_order a in
             let order_b = canonical_font_order b in
             if order_a <> order_b then compare order_a order_b else compare a b)
    in

    (* Generate the font property declarations *)
    let font_vars =
      font_names_to_include
      |> List.map (fun (_, var_name) ->
             match var_name with
             | "--font-sans" ->
                 Css.custom_property "--font-sans"
                   "ui-sans-serif, system-ui, sans-serif, \"Apple Color \
                    Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto \
                    Color Emoji\""
             | "--font-serif" ->
                 Css.custom_property "--font-serif"
                   "ui-serif, Georgia, Cambria, \"Times New Roman\", Times, \
                    serif"
             | "--font-mono" ->
                 Css.custom_property "--font-mono"
                   "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, \
                    \"Liberation Mono\", \"Courier New\", monospace"
             | _ -> failwith "Unexpected font variable")
    in

    let theme_vars_with_fonts =
      font_vars @ theme_generated_vars @ default_font_props
    in

    let theme_layer =
      Css.layered_rules ~layer:Css.Theme
        [
          Css.rule_to_nested
            (Css.rule ~selector:":root, :host" theme_vars_with_fonts);
        ]
    in

    (* Base layer with reset rules *)
    let base_rules = generate_reset_rules () in

    (* Split the base rules to insert @supports after ::placeholder rule *)
    let rec split_after_placeholder acc = function
      | [] -> (List.rev acc, [])
      | h :: t ->
          if Css.selector h = "::placeholder" then (List.rev (h :: acc), t)
          else split_after_placeholder (h :: acc) t
    in
    let before_placeholder, after_placeholder =
      split_after_placeholder [] base_rules
    in

    (* Create placeholder @supports block *)
    let placeholder_supports =
      Css.supports_nested
        ~condition:
          "(not ((-webkit-appearance:-apple-pay-button))) or \
           (contain-intrinsic-size:1px)"
        [ Css.rule ~selector:"::placeholder" [ Css.color Current ] ]
        [
          Css.supports ~condition:"(color:color-mix(in lab, red, red))"
            [
              Css.rule ~selector:"::placeholder"
                [
                  (* TODO: add support for color-mix() in color type *)
                  Css.color (Css.Rgba { r = 0; g = 0; b = 0; a = 0.5 });
                  (* placeholder approximation *)
                ];
            ];
        ]
    in

    (* Build base layer with rules and nested @supports in correct order *)
    let base_layer_content =
      (before_placeholder |> List.map Css.rule_to_nested)
      @ [ Css.supports_to_nested placeholder_supports ]
      @ (after_placeholder |> List.map Css.rule_to_nested)
    in
    let base_layer = Css.layered_rules ~layer:Css.Base base_layer_content in

    (* Components layer - standard Tailwind v4 layer *)
    let components_layer = Css.layered_rules ~layer:Css.Components [] in

    (* Utilities layer with the actual utility classes AND media queries *)
    (* Tailwind v4 uses conflict-aware grouping for CSS rule ordering.
       Utilities are grouped by which CSS properties they can conflict with,
       ensuring proper specificity resolution. More specific utilities override
       shorthand ones within each group. *)

    (* Conflict-aware utility sorter based on Tailwind v4's approach *)
    let get_conflict_group selector =
      (* Extract the class name from selector (e.g., ".p-4" -> "p-4") *)
      let core =
        if String.starts_with ~prefix:"." selector then
          String.sub selector 1 (String.length selector - 1)
        else selector
      in

      (* Helper: check if string starts with prefix *)
      let starts prefix s =
        let lp = String.length prefix and ls = String.length s in
        ls >= lp && String.sub s 0 lp = prefix
      in

      (* Conflict groups: (group_priority, intra_group_specificity) Lower
         numbers = earlier in output Based on Tailwind v4's actual output
         order *)

      (* 0xx: layout/display *)
      if core = "hidden" then (10, 3) (* later than display values *)
      else if
        List.exists
          (fun p -> starts p core)
          [
            "block";
            "inline";
            "inline-";
            "flex";
            "grid";
            "table";
            "contents";
            "flow-root";
          ]
      then (10, 1)
      else if
        List.exists
          (fun p -> starts p core)
          [ "static"; "fixed"; "absolute"; "relative"; "sticky" ]
      then (11, 0)
        (* 1xx: margin - comes BEFORE background in Tailwind's order *)
      else if starts "m-" core || starts "-m-" core then (100, 0)
      else if
        starts "mx-" core || starts "my-" core || starts "-mx-" core
        || starts "-my-" core
      then (100, 1)
      else if
        List.exists
          (fun p -> starts p core)
          [ "mt-"; "mr-"; "mb-"; "ml-"; "-mt-"; "-mr-"; "-mb-"; "-ml-" ]
      then (100, 2)
        (* 2xx: background & color utilities - comes BETWEEN margin and
           padding *)
      else if
        List.exists
          (fun p -> starts p core)
          [ "bg-"; "from-"; "via-"; "to-" (* gradients *) ]
      then (200, 0) (* 3xx: padding - comes AFTER background *)
      else if starts "p-" core then (300, 0)
      else if starts "px-" core || starts "py-" core then (300, 1)
      else if
        List.exists (fun p -> starts p core) [ "pt-"; "pr-"; "pb-"; "pl-" ]
      then (300, 2) (* 4xx: typography including text colors *)
      else if
        List.exists
          (fun p -> starts p core)
          [
            "font-";
            "text-";
            "tracking-";
            "leading-";
            "whitespace-";
            "break-";
            "list-";
            "content-";
          ]
      then (400, 0) (* 5xx: borders *)
      else if
        List.exists (fun p -> starts p core) [ "border"; "rounded"; "outline-" ]
      then (500, 0) (* 6xx: sizing *)
      else if
        List.exists
          (fun p -> starts p core)
          [ "w-"; "h-"; "min-w-"; "min-h-"; "max-w-"; "max-h-" ]
      then (600, 0) (* 7xx: effects, transforms, transitions *)
      else if
        List.exists
          (fun p -> starts p core)
          [
            "shadow-";
            "shadow";
            "opacity-";
            "mix-blend-";
            "background-blend-";
            "transform";
            "translate-";
            "scale-";
            "rotate-";
            "skew-";
            "transition";
            "duration-";
            "ease-";
            "delay-";
            "animate-";
          ]
      then (700, 0) (* 8xx: misc / accessibility / interop *)
      else if
        List.exists
          (fun p -> starts p core)
          [
            "cursor-";
            "select-";
            "pointer-events-";
            "sr-";
            "appearance-";
            "accent-";
            "caret-";
            "resize-";
            "scroll-";
            "overflow-";
            "overscroll-";
          ]
      then (800, 0) (* 9xx: flexbox/grid specifics *)
      else if
        List.exists
          (fun p -> starts p core)
          [
            "flex-";
            "grow";
            "shrink";
            "basis-";
            "order-";
            "grid-cols-";
            "col-";
            "grid-rows-";
            "row-";
            "grid-flow-";
            "auto-cols-";
            "auto-rows-";
            "justify-";
            "items-";
            "content-";
            "self-";
            "place-";
            "gap-";
            "space-";
          ]
      then (900, 0) (* 10xx: container, prose *)
      else if core = "container" || starts "prose" core then (1000, 0)
      (* fallback bucket *)
        else (9999, 0)
    in

    (* Sort rules with stable sort to preserve order within groups *)
    let sorted_rules =
      List.stable_sort
        (fun r1 r2 ->
          let group1, sub1 = get_conflict_group (Css.selector r1) in
          let group2, sub2 = get_conflict_group (Css.selector r2) in
          let group_cmp = Int.compare group1 group2 in
          if group_cmp <> 0 then group_cmp
          else Int.compare sub1 sub2 (* Compare specificity within group *))
        rules
    in
    let utilities_layer =
      Css.layered_rules ~layer:Css.Utilities ~media_queries ~container_queries
        (sorted_rules |> List.map Css.rule_to_nested)
    in

    (* Add prose styles if needed *)
    let base_layers =
      [ theme_layer; base_layer; components_layer; utilities_layer ]
    in
    let layers_with_prose =
      if uses_prose tw_classes then
        let prose_rules =
          tw_classes
          |> List.filter_map (function
               | Prose variant -> Some (Prose.to_css_rules variant)
               | _ -> None)
          |> List.concat
        in
        base_layers
        @ [
            Css.layered_rules ~layer:Css.Utilities
              (prose_rules |> List.map Css.rule_to_nested);
          ]
      else base_layers
    in

    (* Add properties layer if needed *)
    let layers =
      match properties_layer_opt with
      | Some props_layer -> props_layer :: layers_with_prose
      | None -> layers_with_prose
    in

    (* Generate @property rules for variables that need them *)
    let at_properties =
      Var.needs_at_property var_tally
      |> List.map (fun var_name ->
             match var_name with
             | "--tw-font-weight" ->
                 Css.at_property ~name:var_name ~syntax:"*" ~initial_value:""
                   ~inherits:false ()
             | "--tw-border-style" ->
                 Css.at_property ~name:var_name ~syntax:"*"
                   ~initial_value:"solid" ~inherits:false ()
             (* Shadow variables *)
             | "--tw-shadow" | "--tw-inset-shadow" | "--tw-ring-shadow"
             | "--tw-inset-ring-shadow" | "--tw-ring-offset-shadow" ->
                 Css.at_property ~name:var_name ~syntax:"*"
                   ~initial_value:"0 0 #0000" ~inherits:false ()
             | "--tw-shadow-color" | "--tw-inset-shadow-color"
             | "--tw-ring-color" | "--tw-inset-ring-color" | "--tw-ring-inset"
               ->
                 Css.at_property ~name:var_name ~syntax:"*" ~initial_value:""
                   ~inherits:false ()
             | "--tw-shadow-alpha" | "--tw-inset-shadow-alpha" ->
                 Css.at_property ~name:var_name ~syntax:"<percentage>"
                   ~initial_value:"100%" ~inherits:false ()
             | "--tw-ring-offset-width" ->
                 Css.at_property ~name:var_name ~syntax:"<length>"
                   ~initial_value:"0" ~inherits:false ()
             | "--tw-ring-offset-color" ->
                 Css.at_property ~name:var_name ~syntax:"*"
                   ~initial_value:"#fff" ~inherits:false ()
             (* Gradient variables *)
             | "--tw-gradient-position" | "--tw-gradient-stops"
             | "--tw-gradient-via-stops" ->
                 Css.at_property ~name:var_name ~syntax:"*" ~initial_value:""
                   ~inherits:false ()
             | "--tw-gradient-from" | "--tw-gradient-via" | "--tw-gradient-to"
               ->
                 Css.at_property ~name:var_name ~syntax:"<color>"
                   ~initial_value:"#0000" ~inherits:false ()
             | "--tw-gradient-from-position" | "--tw-gradient-to-position" ->
                 Css.at_property ~name:var_name ~syntax:"<length-percentage>"
                   ~initial_value:
                     (if var_name = "--tw-gradient-to-position" then "100%"
                      else "0%")
                   ~inherits:false ()
             | "--tw-gradient-via-position" ->
                 Css.at_property ~name:var_name ~syntax:"<length-percentage>"
                   ~initial_value:"50%" ~inherits:false ()
             | _ ->
                 (* Default for other variables if needed *)
                 Css.at_property ~name:var_name ~syntax:"*" ~initial_value:""
                   ~inherits:false ())
    in

    (* Don't add empty Properties layer *)
    (* Media queries and container queries are already included in the utilities layer, don't duplicate *)
    let items =
      List.map (fun l -> Css.Layer l) layers
      @ List.map (fun a -> Css.At_property a) at_properties
    in
    Css.stylesheet items
  else
    (* No reset - just raw rules, media queries, and container queries, no
       layers *)
    let items =
      List.map (fun r -> Css.Rule r) rules
      @ List.map (fun m -> Css.Media m) media_queries
      @ List.map (fun c -> Css.Container c) container_queries
    in
    Css.stylesheet items

(* Convert Tw styles to inline style attribute value *)
let to_inline_style styles =
  let all_props = List.concat_map to_css_properties styles in
  let deduped = Css.deduplicate_declarations all_props in
  Css.inline_style_of_declarations deduped

(** {1 Helper Functions} *)

(** Convert hex color to rgb format - now only handles hex strings *)

(** Convert any color to RGB space-separated string format (e.g., "255 0 0") *)

let int_to_length n =
  if n = 0 then Css.Zero else Css.Rem (float_of_int n *. 0.25)

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
    (* Convert to proper color constructor *)
    let css_color =
      match color with
      | Color.Hex hex ->
          (* For hex colors, use Css.Hex *)
          Css.Hex (String.uppercase_ascii hex)
      | Color.Oklch oklch ->
          (* Use the new Oklch constructor *)
          Css.Oklch { l = oklch.l; c = oklch.c; h = oklch.h }
      | _ ->
          (* For other colors, get OKLCH data directly *)
          let oklch = Color.to_oklch color shade in
          Css.Oklch { l = oklch.l; c = oklch.c; h = oklch.h }
    in
    style class_name [ Css.background_color css_color ]
  else
    (* Use CSS variable reference *)
    let var_name =
      if Color.is_base_color color then Pp.str [ "color-"; color_name color ]
      else Pp.str [ "color-"; color_name color; "-"; string_of_int shade ]
    in
    (* Track the color variable requirement *)
    let var =
      if Color.is_base_color color then color_var (color_name color)
      else color_var ~shade (color_name color)
    in
    style_with_vars class_name
      [ Css.background_color (Css.Var var_name) ]
      [ var ]

let bg_transparent = style "bg-transparent" [ background_color Transparent ]
let bg_current = style "bg-current" [ background_color Current ]

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
    (* Convert to proper color constructor *)
    let css_color =
      match color with
      | Color.Hex hex ->
          (* For hex colors, use Css.Hex *)
          Css.Hex (String.uppercase_ascii hex)
      | Color.Oklch oklch ->
          (* Use the new Oklch constructor *)
          Css.Oklch { l = oklch.l; c = oklch.c; h = oklch.h }
      | _ ->
          (* For other colors, get OKLCH data directly *)
          let oklch = Color.to_oklch color shade in
          Css.Oklch { l = oklch.l; c = oklch.c; h = oklch.h }
    in
    style class_name [ Css.color css_color ]
  else
    (* Use CSS variable reference *)
    let var_name =
      if Color.is_base_color color then Pp.str [ "color-"; color_name color ]
      else Pp.str [ "color-"; color_name color; "-"; string_of_int shade ]
    in
    (* Track the color variable requirement *)
    let var =
      if Color.is_base_color color then color_var (color_name color)
      else color_var ~shade (color_name color)
    in
    style_with_vars class_name [ Css.color (Css.Var var_name) ] [ var ]

let text_transparent = style "text-transparent" [ Css.color Transparent ]
let text_current = style "text-current" [ Css.color Current ]

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
    (* Convert to proper color constructor *)
    let css_color =
      match color with
      | Color.Hex hex ->
          (* For hex colors, use Css.Hex *)
          Css.Hex (String.uppercase_ascii hex)
      | Color.Oklch oklch ->
          (* Use the new Oklch constructor *)
          Css.Oklch { l = oklch.l; c = oklch.c; h = oklch.h }
      | _ ->
          (* For other colors, get OKLCH data directly *)
          let oklch = Color.to_oklch color shade in
          Css.Oklch { l = oklch.l; c = oklch.c; h = oklch.h }
    in
    style class_name [ Css.border_color css_color ]
  else
    (* Use CSS variable reference *)
    let var_name =
      if Color.is_base_color color then Pp.str [ "color-"; color_name color ]
      else Pp.str [ "color-"; color_name color; "-"; string_of_int shade ]
    in
    (* Track the color variable requirement *)
    let var =
      if Color.is_base_color color then color_var (color_name color)
      else color_var ~shade (color_name color)
    in
    style_with_vars class_name [ Css.border_color (Css.Var var_name) ] [ var ]

let border_transparent =
  style "border-transparent" [ Css.border_color Transparent ]

let border_current = style "border-current" [ Css.border_color Current ]

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

let spacing_to_length : spacing -> Css.length = function
  | `Px -> Css.Px 1
  | `Full -> Css.Pct 100.0
  | `Rem f ->
      let n = int_of_float (f /. 0.25) in
      Css.Calc
        (Css.Calc.mul (Css.Calc.var "spacing")
           (Css.Calc.float (float_of_int n)))

let margin_to_length : margin -> Css.length = function
  | `Auto -> Css.Auto
  | #spacing as s -> spacing_to_length s

let rec scale_to_length : scale -> Css.length = function
  | `Screen -> Css.Vh 100.0
  | `Min | `Max | `Fit ->
      Css.Auto (* These need special handling, using Auto for now *)
  | #spacing as s -> spacing_to_length s
  | #size as s -> size_to_length s

and size_to_length : size -> Css.length = function
  | `None -> Css.Zero
  | `Xs -> Css.Rem 0.125
  | `Sm -> Css.Rem 0.25
  | `Md -> Css.Rem 0.375
  | `Lg -> Css.Rem 0.5
  | `Xl -> Css.Rem 0.75
  | `Xl_2 -> Css.Rem 1.0
  | `Xl_3 -> Css.Rem 1.5
  | `Full -> Css.Pct 100.0

let max_scale_to_length : max_scale -> Css.length = function
  | `Xl_4 -> Css.Rem 56.0
  | `Xl_5 -> Css.Rem 64.0
  | `Xl_6 -> Css.Rem 72.0
  | `Xl_7 -> Css.Rem 80.0
  | #scale as s -> scale_to_length s

let spacing_to_length : spacing -> Css.length = function
  | `Px -> Css.Px 1
  | `Full -> Css.Pct 100.0
  | `Rem f ->
      let n = int_of_float (f /. 0.25) in
      Css.Calc
        (Css.Calc.mul (Css.Calc.var "spacing")
           (Css.Calc.float (float_of_int n)))

(** {1 Spacing} *)

(* Helper to extract spacing variables from spacing types *)
let spacing_vars = function
  | `Rem _ ->
      [] (* The --spacing variable is handled via string parsing in all_vars *)
  | _ -> []

(* Helper to extract spacing variables from scale types *)
let scale_vars = function
  | `Rem _ ->
      [] (* The --spacing variable is handled via string parsing in all_vars *)
  | _ -> []

(* Helper to extract spacing variables from margin types *)
let margin_vars = function
  | `Rem _ ->
      [] (* The --spacing variable is handled via string parsing in all_vars *)
  | _ -> []

(* Typed spacing functions with ' suffix *)
let p' (s : spacing) =
  let class_name = "p-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding len ] (spacing_vars s)

let px' (s : spacing) =
  let class_name = "px-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding_inline len ] (spacing_vars s)

let py' (s : spacing) =
  let class_name = "py-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding_block len ] (spacing_vars s)

let pt' (s : spacing) =
  let class_name = "pt-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding_top len ] (spacing_vars s)

let pr' (s : spacing) =
  let class_name = "pr-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding_right len ] (spacing_vars s)

let pb' (s : spacing) =
  let class_name = "pb-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding_bottom len ] (spacing_vars s)

let pl' (s : spacing) =
  let class_name = "pl-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.padding_left len ] (spacing_vars s)

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
  let len = margin_to_length m in
  style_with_vars class_name [ Css.margin len ] (margin_vars m)

let mx' (m : margin) =
  let v = margin_to_length m in
  let class_name = "mx-" ^ pp_margin_suffix m in
  style_with_vars class_name [ Css.margin_inline v ] (margin_vars m)

let my' (m : margin) =
  let v = margin_to_length m in
  let class_name = "my-" ^ pp_margin_suffix m in
  style_with_vars class_name [ Css.margin_block v ] (margin_vars m)

let mt' (m : margin) =
  let class_name = "mt-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ Css.margin_top len ] (margin_vars m)

let mr' (m : margin) =
  let class_name = "mr-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ Css.margin_right len ] (margin_vars m)

let mb' (m : margin) =
  let class_name = "mb-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ Css.margin_bottom len ] (margin_vars m)

let ml' (m : margin) =
  let class_name = "ml-" ^ pp_margin_suffix m in
  let len = margin_to_length m in
  style_with_vars class_name [ Css.margin_left len ] (margin_vars m)

(* Int-based margin functions - now support negative values *)
let m n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "m-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.margin len ] [ Spacing (abs n) ]

let mx n =
  let s = int n in
  let len = spacing_to_length s in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mx-" ^ pp_spacing_suffix s in
  style_with_vars class_name [ Css.margin_inline len ] (spacing_vars s)

let my n =
  let s = int n in
  let len = spacing_to_length s in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "my-" ^ pp_spacing_suffix s in
  style_with_vars class_name [ Css.margin_block len ] (spacing_vars s)

let mt n =
  let s = int n in
  let len = spacing_to_length s in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mt-" ^ pp_spacing_suffix s in
  style_with_vars class_name [ Css.margin_top len ] [ Spacing (abs n) ]

let mr n =
  let s = int n in
  let len = spacing_to_length s in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mr-" ^ pp_spacing_suffix s in
  style_with_vars class_name [ Css.margin_right len ] [ Spacing (abs n) ]

let mb n =
  let s = int n in
  let len = spacing_to_length s in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "mb-" ^ pp_spacing_suffix s in
  style_with_vars class_name [ Css.margin_bottom len ] [ Spacing (abs n) ]

let ml n =
  let s = int n in
  let len = spacing_to_length s in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "ml-" ^ pp_spacing_suffix s in
  style_with_vars class_name [ Css.margin_left len ] [ Spacing (abs n) ]

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
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.gap len ] (spacing_vars s)

let gap_x' (s : spacing) =
  let class_name = "gap-x-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.column_gap len ] (spacing_vars s)

let gap_y' (s : spacing) =
  let class_name = "gap-y-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style_with_vars class_name [ Css.row_gap len ] (spacing_vars s)

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
  style class_name [ Css.margin_left (int_to_length n) ]

let space_y n =
  let class_name = "space-y-" ^ string_of_int n in
  style class_name [ Css.margin_top (int_to_length n) ]

(** {1 Sizing} *)

(* Typed scale functions with ' suffix *)
let w' (s : scale) =
  let class_name = "w-" ^ pp_scale_suffix s in
  let decl =
    match s with
    | `Fit -> Css.width Fit_content
    | `Min -> Css.width Min_content
    | `Max -> Css.width Max_content
    | _ -> Css.width (scale_to_length s)
  in
  style_with_vars class_name [ decl ] (scale_vars s)

let h' (s : scale) =
  let class_name = "h-" ^ pp_scale_suffix s in
  let decl =
    match s with
    | `Fit -> Css.height Fit_content
    | `Min -> Css.height Min_content
    | `Max -> Css.height Max_content
    | _ -> Css.height (scale_to_length s)
  in
  style_with_vars class_name [ decl ] (scale_vars s)

let min_w' (s : scale) =
  let class_name = "min-w-" ^ pp_scale_suffix s in
  let len = scale_to_length s in
  style_with_vars class_name [ Css.min_width len ] (scale_vars s)

let min_h' (s : scale) =
  let class_name = "min-h-" ^ pp_scale_suffix s in
  let len = scale_to_length s in
  style_with_vars class_name [ Css.min_height len ] (scale_vars s)

let max_w' (s : max_scale) =
  let class_name = "max-w-" ^ pp_max_scale_suffix s in
  let len = max_scale_to_length s in
  let vars = match s with #scale as sc -> scale_vars sc | _ -> [] in
  style_with_vars class_name [ Css.max_width len ] vars

let max_h' (s : max_scale) =
  let class_name = "max-h-" ^ pp_max_scale_suffix s in
  let len = max_scale_to_length s in
  let vars = match s with #scale as sc -> scale_vars sc | _ -> [] in
  style_with_vars class_name [ Css.max_height len ] vars

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
      Css.font_size (Css.Var (Css.var "text-xs"));
      Css.line_height
        (Css.Var
           (Css.var
              ~fallback:(Css.Var (Css.var "text-xs--line-height"))
              "tw-leading"));
    ]

let text_sm =
  style "text-sm"
    [
      Css.font_size (Css.Var (Css.var "text-sm"));
      Css.line_height
        (Css.Var
           (Css.var
              ~fallback:(Css.Var (Css.var "text-sm--line-height"))
              "tw-leading"));
    ]

let text_xl =
  style "text-xl"
    [
      Css.font_size (Css.Var (Css.var "text-xl"));
      Css.line_height
        (Css.Var
           (Css.var
              ~fallback:(Css.Var (Css.var "text-xl--line-height"))
              "tw-leading"));
    ]

let text_2xl =
  style "text-2xl"
    [
      Css.font_size (Css.Var (Css.var "text-2xl"));
      Css.line_height
        (Css.Var
           (Css.var
              ~fallback:(Css.Var (Css.var "text-2xl--line-height"))
              "tw-leading"));
    ]

let text_center = style "text-center" [ Css.text_align Css.Center ]

(** {1 Layout} *)

let block = style "block" [ Css.display Css.Block ]
let inline = style "inline" [ Css.display Css.Inline ]
let inline_block = style "inline-block" [ Css.display Css.Inline_block ]
let hidden = style "hidden" [ Css.display Css.None ]

(** {1 Flexbox} *)

let flex = style "flex" [ Css.display Css.Flex ]
let flex_shrink_0 = style "flex-shrink-0" [ Css.Flex.shrink 0.0 ]
let flex_col = style "flex-col" [ Css.Flex.direction Css.Column ]
let flex_row = style "flex-row" [ Css.Flex.direction Css.Row ]
let flex_wrap = style "flex-wrap" [ Css.Flex.wrap Css.Wrap ]

let flex_row_reverse =
  style "flex-row-reverse" [ Css.Flex.direction Css.Row_reverse ]

let flex_col_reverse =
  style "flex-col-reverse" [ Css.Flex.direction Css.Column_reverse ]

let flex_wrap_reverse =
  style "flex-wrap-reverse" [ Css.Flex.wrap Css.Wrap_reverse ]

let flex_nowrap = style "flex-nowrap" [ Css.Flex.wrap Css.Nowrap ]
let flex_1 = style "flex-1" [ Css.Flex.flex (Css.Grow 1.0) ]
let flex_auto = style "flex-auto" [ Css.Flex.flex Css.Auto ]
let flex_initial = style "flex-initial" [ Css.Flex.flex Css.Initial ]
let flex_none = style "flex-none" [ Css.Flex.flex Css.None ]
let flex_grow = style "flex-grow" [ Css.Flex.grow 1.0 ]
let flex_grow_0 = style "flex-grow-0" [ Css.Flex.grow 0.0 ]
let flex_shrink = style "flex-shrink" [ Css.Flex.shrink 1.0 ]

(* center *)

let items_center = style "items-center" [ Css.align_items Css.Center ]

let justify_between =
  style "justify-between" [ Css.justify_content Css.Space_between ]

(** {1 Positioning} *)

let relative = style "relative" [ Css.position Css.Relative ]
let absolute = style "absolute" [ Css.position Css.Absolute ]
let fixed = style "fixed" [ Css.position Css.Fixed ]
let sticky = style "sticky" [ Css.position Css.Sticky ]

(* Borders *)

(* Modifiers *)

(** {1 CSS Generation} *)

let text_base =
  style "text-base"
    [
      Css.font_size (Css.Var (Css.var "text-base"));
      Css.line_height
        (Css.Var
           (Css.var
              ~fallback:(Css.Var (Css.var "text-base--line-height"))
              "tw-leading"));
    ]

let text_lg =
  style "text-lg"
    [
      Css.font_size (Css.Var (Css.var "text-lg"));
      Css.line_height
        (Css.Var
           (Css.var
              ~fallback:(Css.Var (Css.var "text-lg--line-height"))
              "tw-leading"));
    ]

let text_3xl =
  style "text-3xl"
    [
      Css.font_size (Css.Var (Css.var "text-3xl"));
      Css.line_height
        (Css.Var
           (Css.var
              ~fallback:(Css.Var (Css.var "text-3xl--line-height"))
              "tw-leading"));
    ]

let text_4xl =
  style "text-4xl"
    [
      Css.font_size (Css.Var (Css.var "text-4xl"));
      Css.line_height
        (Css.Var
           (Css.var
              ~fallback:(Css.Var (Css.var "text-4xl--line-height"))
              "tw-leading"));
    ]

let text_5xl =
  style "text-5xl"
    [
      Css.font_size (Css.Var (Css.var "text-5xl"));
      Css.line_height
        (Css.Var
           (Css.var
              ~fallback:(Css.Var (Css.var "text-5xl--line-height"))
              "tw-leading"));
    ]

let font_thin =
  style_with_vars "font-thin"
    [
      Css.custom_property "--tw-font-weight" "var(--font-weight-thin)";
      Css.font_weight (Var (Css.var "font-weight-thin"));
    ]
    []

let font_light =
  style_with_vars "font-light"
    [
      Css.custom_property "--tw-font-weight" "var(--font-weight-light)";
      Css.font_weight (Var (Css.var "font-weight-light"));
    ]
    []

let font_normal =
  style_with_vars "font-normal"
    [
      Css.custom_property "--tw-font-weight" "var(--font-weight-normal)";
      Css.font_weight (Var (Css.var "font-weight-normal"));
    ]
    []

let font_medium =
  style_with_vars "font-medium"
    [
      Css.custom_property "--tw-font-weight" "var(--font-weight-medium)";
      Css.font_weight (Var (Css.var "font-weight-medium"));
    ]
    []

let font_semibold =
  style_with_vars "font-semibold"
    [
      Css.custom_property "--tw-font-weight" "var(--font-weight-semibold)";
      Css.font_weight (Var (Css.var "font-weight-semibold"));
    ]
    []

let font_bold =
  style_with_vars "font-bold"
    [
      Css.custom_property "--tw-font-weight" "var(--font-weight-bold)";
      Css.font_weight (Var (Css.var "font-weight-bold"));
    ]
    []

let font_extrabold =
  style_with_vars "font-extrabold"
    [
      Css.custom_property "--tw-font-weight" "var(--font-weight-extrabold)";
      Css.font_weight (Var (Css.var "font-weight-extrabold"));
    ]
    []

let font_black =
  style_with_vars "font-black"
    [
      Css.custom_property "--tw-font-weight" "var(--font-weight-black)";
      Css.font_weight (Var (Css.var "font-weight-black"));
    ]
    []

(* Font family utilities *)
let font_sans =
  style "font-sans"
    [ font_family [ Css.Var { name = "font-sans"; fallback = None } ] ]

let font_serif =
  style "font-serif"
    [ font_family [ Css.Var { name = "font-serif"; fallback = None } ] ]

let font_mono =
  style "font-mono"
    [ Css.font_family [ Css.Var { name = "font-mono"; fallback = None } ] ]

let italic = style "italic" [ Css.font_style Css.Italic ]
let not_italic = style "not-italic" [ Css.font_style Css.Font_normal ]
let underline = style "underline" [ Css.text_decoration Css.Underline ]
let line_through = style "line-through" [ Css.text_decoration Css.Line_through ]
let no_underline = style "no-underline" [ Css.text_decoration Css.None ]
let text_left = style "text-left" [ Css.text_align Css.Left ]
let text_right = style "text-right" [ Css.text_align Css.Right ]
let text_justify = style "text-justify" [ Css.text_align Css.Justify ]
let leading_none = style "leading-none" [ Css.line_height (Css.Num 1.0) ]
let leading_tight = style "leading-tight" [ Css.line_height (Css.Num 1.25) ]
let leading_snug = style "leading-snug" [ Css.line_height (Css.Num 1.375) ]
let leading_normal = style "leading-normal" [ Css.line_height (Css.Num 1.5) ]

let leading_relaxed =
  style "leading-relaxed" [ Css.line_height (Css.Num 1.625) ]

let leading_loose = style "leading-loose" [ Css.line_height (Css.Num 2.0) ]
let leading_6 = style "leading-6" [ Css.line_height (Css.Rem 1.5) ]

let tracking_tighter =
  style "tracking-tighter" [ Css.letter_spacing (Em (-0.05)) ]

let tracking_tight = style "tracking-tight" [ Css.letter_spacing (Em (-0.025)) ]
let tracking_normal = style "tracking-normal" [ Css.letter_spacing Zero ]
let tracking_wide = style "tracking-wide" [ Css.letter_spacing (Em 0.025) ]
let tracking_wider = style "tracking-wider" [ Css.letter_spacing (Em 0.05) ]
let tracking_widest = style "tracking-widest" [ Css.letter_spacing (Em 0.1) ]
let whitespace_normal = style "whitespace-normal" [ Css.white_space Normal ]
let whitespace_nowrap = style "whitespace-nowrap" [ Css.white_space Nowrap ]
let whitespace_pre = style "whitespace-pre" [ Css.white_space Pre ]

let whitespace_pre_line =
  style "whitespace-pre-line" [ Css.white_space Pre_line ]

let whitespace_pre_wrap =
  style "whitespace-pre-wrap" [ Css.white_space Pre_wrap ]

let inline_flex = style "inline-flex" [ Css.display Css.Inline_flex ]
let grid = style "grid" [ Css.display Css.Grid ]
let inline_grid = style "inline-grid" [ Css.display Css.Inline_grid ]
let items_start = style "items-start" [ Css.align_items Css.Flex_start ]
let items_end = style "items-end" [ Css.align_items Css.Flex_end ]
let items_baseline = style "items-baseline" [ Css.align_items Css.Baseline ]
let items_stretch = style "items-stretch" [ Css.align_items Css.Stretch ]
let justify_start = style "justify-start" [ Css.justify_content Css.Flex_start ]
let justify_end = style "justify-end" [ Css.justify_content Css.Flex_end ]
let justify_center = style "justify-center" [ Css.justify_content Css.Center ]

let justify_around =
  style "justify-around" [ Css.justify_content Css.Space_around ]

let justify_evenly =
  style "justify-evenly" [ Css.justify_content Css.Space_evenly ]

(* Align content utilities - for multi-line flex/grid containers *)
let content_start = style "content-start" [ Css.align_content Flex_start ]
let content_end = style "content-end" [ Css.align_content Flex_end ]
let content_center = style "content-center" [ Css.align_content Center ]

let content_between =
  style "content-between" [ Css.align_content Space_between ]

let content_around = style "content-around" [ Css.align_content Space_around ]
let content_evenly = style "content-evenly" [ Css.align_content Space_evenly ]
let content_stretch = style "content-stretch" [ Css.align_content Stretch ]

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
let self_auto = style "self-auto" [ Css.align_self Auto ]
let self_start = style "self-start" [ Css.align_self Flex_start ]
let self_end = style "self-end" [ Css.align_self Flex_end ]
let self_center = style "self-center" [ Css.align_self Center ]
let self_baseline = style "self-baseline" [ Css.align_self Baseline ]
let self_stretch = style "self-stretch" [ Css.align_self Stretch ]

(* Justify self utilities - for Grid items *)
let justify_self_auto = style "justify-self-auto" [ Css.justify_self Auto ]
let justify_self_start = style "justify-self-start" [ Css.justify_self Start ]
let justify_self_end = style "justify-self-end" [ Css.justify_self End ]

let justify_self_center =
  style "justify-self-center" [ Css.justify_self Center ]

let justify_self_stretch =
  style "justify-self-stretch" [ Css.justify_self Stretch ]

let grid_cols n =
  let class_name = "grid-cols-" ^ string_of_int n in
  style class_name
    [
      Css.Grid.template_columns
        (Css.Repeat (n, Css.Min_max (Css.Px 0, Css.Fr 1.)));
    ]

let grid_rows n =
  let class_name = "grid-rows-" ^ string_of_int n in
  style class_name
    [
      Css.Grid.template_rows (Css.Repeat (n, Css.Min_max (Css.Px 0, Css.Fr 1.)));
    ]

let static = style "static" [ Css.position Css.Static ]

let inset_0 =
  style "inset-0"
    [ Css.top Zero; Css.right Zero; Css.bottom Zero; Css.left Zero ]

let inset_x_0 = style "inset-x-0" [ Css.left Zero; Css.right Zero ]
let inset_y_0 = style "inset-y-0" [ Css.bottom Zero; Css.top Zero ]

let top n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "top-" ^ string_of_int (abs n) in
  let value =
    Css.Calc
      (Css.Calc.mul (Css.Calc.var "spacing")
         (Css.Calc.float (float_of_int (abs n))))
  in
  style_with_vars class_name [ Css.top value ] [ Spacing (abs n) ]

let right n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "right-" ^ string_of_int (abs n) in
  let value =
    Css.Calc
      (Css.Calc.mul (Css.Calc.var "spacing")
         (Css.Calc.float (float_of_int (abs n))))
  in
  style_with_vars class_name [ Css.right value ] [ Spacing (abs n) ]

let bottom n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "bottom-" ^ string_of_int (abs n) in
  let value =
    Css.Calc
      (Css.Calc.mul (Css.Calc.var "spacing")
         (Css.Calc.float (float_of_int (abs n))))
  in
  style_with_vars class_name [ Css.bottom value ] [ Spacing (abs n) ]

let left n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "left-" ^ string_of_int (abs n) in
  let value =
    Css.Calc
      (Css.Calc.mul (Css.Calc.var "spacing")
         (Css.Calc.float (float_of_int (abs n))))
  in
  style_with_vars class_name [ Css.left value ] [ Spacing (abs n) ]

let z n =
  let class_name = "z-" ^ string_of_int n in
  style class_name [ Css.z_index n ]

(* Inset utilities *)
let inset n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "inset-" ^ string_of_int (abs n) in
  let value =
    Css.Calc
      (Css.Calc.mul (Css.Calc.var "spacing")
         (Css.Calc.float (float_of_int (abs n))))
  in
  style_with_vars class_name
    [ Css.top value; Css.right value; Css.bottom value; Css.left value ]
    [ Spacing (abs n) ]

(* Fractional position utilities *)
let top_1_2 = style "top-1/2" [ Css.top (Pct 50.0) ]
let left_1_2 = style "left-1/2" [ Css.left (Pct 50.0) ]

let neg_translate_x_1_2 =
  style "-translate-x-1/2"
    [ Css.transform [ Css.Translate_x (Css.Pct (-50.0)) ] ]

let neg_translate_y_1_2 =
  style "-translate-y-1/2"
    [ Css.transform [ Css.Translate_y (Css.Pct (-50.0)) ] ]

type width = size

let border_internal (w : width) =
  let width_len, class_suffix =
    match w with
    | `None -> (Css.Zero, "-0")
    | `Xs -> (Css.Px 1, "" (* Default border is 1px *))
    | `Sm -> (Css.Px 2, "-2")
    | `Md -> (Css.Px 4, "-4" (* For borders, Md maps to 4px *))
    | `Lg -> (Css.Px 4, "-4")
    | `Xl -> (Css.Px 8, "-8")
    | `Xl_2 -> (Css.Px 8, "-8")
    | `Xl_3 -> (Css.Px 8, "-8")
    | `Full -> (Css.Px 8, "-8")
  in
  let class_name = "border" ^ class_suffix in
  style class_name
    [
      Css.border_style (Var (Css.var "tw-border-style"));
      Css.border_width width_len;
    ]

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
let border_t = style "border-t" [ Css.border_top_width (Css.Px 1) ]
let border_r = style "border-r" [ Css.border_right_width (Css.Px 1) ]
let border_b = style "border-b" [ Css.border_bottom_width (Css.Px 1) ]
let border_l = style "border-l" [ Css.border_left_width (Css.Px 1) ]

(* Border styles *)
let border_solid =
  style "border-solid"
    [
      Css.custom_property "--tw-border-style" "solid";
      Css.border_style Css.Solid;
    ]

let border_dashed =
  style "border-dashed"
    [
      Css.custom_property "--tw-border-style" "dashed";
      Css.border_style Css.Dashed;
    ]

let border_dotted =
  style "border-dotted"
    [
      Css.custom_property "--tw-border-style" "dotted";
      Css.border_style Css.Dotted;
    ]

let border_double =
  style "border-double"
    [
      Css.custom_property "--tw-border-style" "double";
      Css.border_style Css.Double;
    ]

let border_none_style =
  style "border-none"
    [
      Css.custom_property "--tw-border-style" "none"; Css.border_style Css.None;
    ]

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
  let decl =
    match r with
    | `None -> Css.border_radius Css.Zero
    | `Full ->
        Css.border_radius (Css.Px 9999) (* Large value for full rounding *)
    | _ ->
        Css.border_radius (Css.Var (Css.var ("radius-" ^ pp_rounded_suffix r)))
  in
  style class_name [ decl ]

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

let shadow_internal s =
  let class_name = "shadow-" ^ pp_shadow_suffix s in
  let custom_props = [ Css.custom_property "--tw-shadow" (shadow_value s) ] in
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
  let value = float_of_int n /. 100.0 in
  style class_name [ Css.opacity value ]

let transition_none =
  style "transition-none" [ Css.transition (Css.Simple (Css.None, Css.S 0.0)) ]

let transition_all =
  style "transition-all"
    [
      Css.transition
        (Css.With_timing
           (Css.All, Css.Ms 150, Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0)));
    ]

let transition_colors =
  style "transition-colors"
    [
      Css.transition
        (Css.Multiple
           [
             Css.With_timing
               ( Css.Property "background-color",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
             Css.With_timing
               ( Css.Property "border-color",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
             Css.With_timing
               ( Css.Property "color",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
             Css.With_timing
               ( Css.Property "fill",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
             Css.With_timing
               ( Css.Property "stroke",
                 Css.Ms 150,
                 Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
           ]);
    ]

let transition_opacity =
  style "transition-opacity"
    [
      Css.transition
        (Css.With_timing
           ( Css.Property "opacity",
             Css.Ms 150,
             Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) ));
    ]

let transition_shadow =
  style "transition-shadow"
    [
      Css.transition
        (Css.With_timing
           ( Css.Property "box-shadow",
             Css.Ms 150,
             Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) ));
    ]

let transition_transform =
  style "transition-transform"
    [
      Css.transition
        (Css.With_timing
           ( Css.Property "transform",
             Css.Ms 150,
             Css.Cubic_bezier (0.4, 0.0, 0.2, 1.0) ));
    ]

let rotate n =
  let class_name = "rotate-" ^ string_of_int n in
  style class_name [ Css.rotate (Css.Deg (float_of_int n)) ]

let translate_x n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "translate-x-" ^ string_of_int (abs n) in
  let len = if n = 0 then Css.Zero else Css.Rem (float_of_int n *. 0.25) in
  style class_name [ Css.transform [ Css.Translate_x len ] ]

let translate_y n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "translate-y-" ^ string_of_int (abs n) in
  let len = if n = 0 then Css.Zero else Css.Rem (float_of_int n *. 0.25) in
  style class_name [ Css.transform [ Css.Translate_y len ] ]

(** 3D Transform utilities - inspired by modern CSS capabilities

    While Tailwind CSS traditionally focused on 2D transforms, modern CSS
    supports full 3D transformations. These utilities enable sophisticated
    animations and visual effects like card flips, 3D rotations, and depth. We
    include these as they represent useful CSS features that complement OCaml's
    approach to building interactive UIs. *)

let rotate_x n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "rotate-x-" ^ string_of_int (abs n) in
  style class_name [ Css.transform [ Css.Rotate_x (Css.Deg (float_of_int n)) ] ]

let rotate_y n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "rotate-y-" ^ string_of_int (abs n) in
  style class_name [ Css.transform [ Css.Rotate_y (Css.Deg (float_of_int n)) ] ]

let rotate_z n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "rotate-z-" ^ string_of_int (abs n) in
  style class_name [ Css.transform [ Css.Rotate_z (Css.Deg (float_of_int n)) ] ]

let translate_z n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "translate-z-" ^ string_of_int (abs n) in
  style class_name [ Css.transform [ Css.Translate_z (Css.Px n) ] ]

let scale_z n =
  let value = float_of_int n /. 100.0 in
  let class_name = "scale-z-" ^ string_of_int n in
  style class_name
    [
      Css.custom_property "--tw-scale-z" (Pp.float value);
      Css.transform
        [
          Css.Translate_var
            {
              var_name = "tw-translate-x, var(--tw-translate-y";
              fallback = None;
            };
          Css.Translate_z (Css.Var (Css.var "tw-translate-z"));
          Css.Rotate_var { var_name = "tw-rotate"; fallback = None };
          Css.Rotate_x
            (Css.Angle_var { var_name = "tw-rotate-x"; fallback = Some 0.0 });
          Css.Rotate_y
            (Css.Angle_var { var_name = "tw-rotate-y"; fallback = Some 0.0 });
          Css.Rotate_z
            (Css.Angle_var { var_name = "tw-rotate-z"; fallback = Some 0.0 });
          Css.Skew_var
            { var_name = "tw-skew-x) skewY(var(--tw-skew-y"; fallback = None };
          Css.Scale_x
            (Css.Scale_var { var_name = "tw-scale-x"; fallback = None });
          Css.Scale_y
            (Css.Scale_var { var_name = "tw-scale-y"; fallback = None });
          Css.Scale_z
            (Css.Scale_var { var_name = "tw-scale-z"; fallback = Some 1.0 });
        ];
    ]

let perspective n =
  let class_name = "perspective-" ^ string_of_int n in
  let value = if n = 0 then Css.Zero else Css.Px n in
  style class_name [ Css.perspective value ]

let perspective_origin_center =
  style "perspective-origin-center" [ Css.perspective_origin "center" ]

let perspective_origin_top =
  style "perspective-origin-top" [ Css.perspective_origin "top" ]

let perspective_origin_bottom =
  style "perspective-origin-bottom" [ Css.perspective_origin "bottom" ]

let perspective_origin_left =
  style "perspective-origin-left" [ Css.perspective_origin "left" ]

let perspective_origin_right =
  style "perspective-origin-right" [ Css.perspective_origin "right" ]

let transform_style_3d =
  style "transform-style-3d" [ Css.transform_style Preserve_3d ]

let transform_style_flat =
  style "transform-style-flat" [ Css.transform_style Flat ]

let backface_visible =
  style "backface-visible" [ Css.backface_visibility Visible ]

let backface_hidden = style "backface-hidden" [ Css.backface_visibility Hidden ]

(** Container query utilities - inspired by modern CSS capabilities

    Container queries allow elements to respond to their container's size rather
    than the viewport. This is particularly useful for component-based design
    where a component might be used in different sized containers. While
    Tailwind CSS v4 includes container queries, we implement them here as
    they're a valuable CSS feature that works well with OCaml's approach. *)
let container_type_size =
  style "container-type-size" [ Css.container_type Size ]

let container_type_inline_size =
  style "container-type-inline-size" [ Css.container_type Inline_size ]

let container_type_normal =
  style "container-type-normal" [ Css.container_type Normal ]

let container_name name =
  style ("container-" ^ name) [ Css.container_name name ]

(* Container query breakpoints *)
let on_container_sm styles = Modified (Container Container_sm, Group styles)
let on_container_md styles = Modified (Container Container_md, Group styles)
let on_container_lg styles = Modified (Container Container_lg, Group styles)
let on_container_xl styles = Modified (Container Container_xl, Group styles)
let on_container_2xl styles = Modified (Container Container_2xl, Group styles)

(* Named container queries - using on_container as the user suggested *)
let on_container ?name min_width styles =
  let query =
    match name with
    | None -> Container (Container_named ("", min_width))
    | Some n -> Container (Container_named (n, min_width))
  in
  Group (List.map (fun t -> Modified (query, t)) styles)

let cursor_auto = style "cursor-auto" [ Css.cursor Auto ]
let cursor_default = style "cursor-default" [ Css.cursor Default ]
let cursor_pointer = style "cursor-pointer" [ Css.cursor Pointer ]
let cursor_wait = style "cursor-wait" [ Css.cursor Wait ]
let cursor_move = style "cursor-move" [ Css.cursor Move ]
let cursor_not_allowed = style "cursor-not-allowed" [ Css.cursor Not_allowed ]
let select_none = style "select-none" [ Css.user_select None ]
let select_text = style "select-text" [ Css.user_select Text ]
let select_all = style "select-all" [ Css.user_select All ]
let select_auto = style "select-auto" [ Css.user_select Css.Auto ]

let pointer_events_none =
  style "pointer-events-none"
    [ Css.pointer_events (None : Css.pointer_events_value) ]

let pointer_events_auto =
  style "pointer-events-auto"
    [ Css.pointer_events (Auto : Css.pointer_events_value) ]

let outline_none = style "outline-none" [ Css.outline "none" ]

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
      Css.custom_property "--tw-ring-color" "rgb(59 130 246 / 0.5)";
      box_shadow
        "var(--tw-ring-offset-shadow,0 0 #0000),var(--tw-ring-shadow,0 0 \
         #0000),var(--tw-shadow,0 0 #0000)";
      Css.custom_property "--tw-ring-shadow" shadow_value;
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

let isolate = style "isolate" [ Css.isolation Css.Isolate ]
let overflow_auto = style "overflow-auto" [ Css.overflow Css.Auto ]
let overflow_hidden = style "overflow-hidden" [ Css.overflow Css.Hidden ]
let overflow_visible = style "overflow-visible" [ Css.overflow Css.Visible ]
let overflow_scroll = style "overflow-scroll" [ Css.overflow Css.Scroll ]

(* Overflow variants *)
let overflow_x_auto = style "overflow-x-auto" [ Css.overflow_x Css.Auto ]
let overflow_x_hidden = style "overflow-x-hidden" [ Css.overflow_x Css.Hidden ]

let overflow_x_visible =
  style "overflow-x-visible" [ Css.overflow_x Css.Visible ]

let overflow_x_scroll = style "overflow-x-scroll" [ Css.overflow_x Css.Scroll ]
let overflow_y_auto = style "overflow-y-auto" [ Css.overflow_y Css.Auto ]
let overflow_y_hidden = style "overflow-y-hidden" [ Css.overflow_y Css.Hidden ]

let overflow_y_visible =
  style "overflow-y-visible" [ Css.overflow_y Css.Visible ]

let overflow_y_scroll = style "overflow-y-scroll" [ Css.overflow_y Css.Scroll ]

(* Scroll snap utilities *)
let snap_none = style "snap-none" [ Css.scroll_snap_type Css.None ]

let snap_x =
  style "snap-x"
    [ Css.scroll_snap_type (X_var "var(--tw-scroll-snap-strictness)") ]

let snap_y =
  style "snap-y"
    [ Css.scroll_snap_type (Y_var "var(--tw-scroll-snap-strictness)") ]

let snap_both =
  style "snap-both"
    [ Css.scroll_snap_type (Both_var "var(--tw-scroll-snap-strictness)") ]

let snap_mandatory =
  style "snap-mandatory"
    [ Css.custom_property "--tw-scroll-snap-strictness" "mandatory" ]

let snap_proximity =
  style "snap-proximity"
    [ Css.custom_property "--tw-scroll-snap-strictness" "proximity" ]

let snap_start = style "snap-start" [ Css.scroll_snap_align Css.Start ]
let snap_end = style "snap-end" [ Css.scroll_snap_align Css.End ]
let snap_center = style "snap-center" [ Css.scroll_snap_align Css.Center ]
let snap_align_none = style "snap-align-none" [ Css.scroll_snap_align Css.None ]
let snap_normal = style "snap-normal" [ Css.scroll_snap_stop Css.Normal ]
let snap_always = style "snap-always" [ Css.scroll_snap_stop Css.Always ]
let scroll_auto = style "scroll-auto" [ Css.scroll_behavior Css.Auto ]
let scroll_smooth = style "scroll-smooth" [ Css.scroll_behavior Css.Smooth ]
let object_contain = style "object-contain" [ Css.object_fit Contain ]
let object_cover = style "object-cover" [ Css.object_fit Cover ]
let object_fill = style "object-fill" [ Css.object_fit Fill ]
let object_none = style "object-none" [ Css.object_fit None ]
let object_scale_down = style "object-scale-down" [ Css.object_fit Scale_down ]

let sr_only =
  style "sr-only"
    [
      Css.position Css.Absolute;
      Css.width (Css.Px 1);
      Css.height (Css.Px 1);
      Css.padding Css.Zero;
      Css.margin (Css.Px (-1));
      Css.overflow Css.Hidden;
      Css.clip "rect(0, 0, 0, 0)";
      Css.white_space Nowrap;
      Css.border_width Css.Zero;
    ]

let not_sr_only =
  style "not-sr-only"
    [
      Css.position Css.Static;
      Css.width Css.Auto;
      Css.height Css.Auto;
      Css.padding Css.Zero;
      Css.margin Css.Zero;
      Css.overflow Css.Visible;
      Css.clip "auto";
      Css.white_space Normal;
    ]

(* Responsive and state modifiers *)

let focus_visible =
  style "focus-visible"
    [ outline "2px solid transparent"; outline_offset (Px 2) ]

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
    [
      Css.background_image
        "linear-gradient(to bottom, var(--tw-gradient-stops))";
    ]

let bg_gradient_to_br =
  style "bg-gradient-to-br"
    [
      Css.background_image
        "linear-gradient(to bottom right, var(--tw-gradient-stops))";
    ]

let bg_gradient_to_t =
  style "bg-gradient-to-t"
    [ Css.background_image "linear-gradient(to top, var(--tw-gradient-stops))" ]

let bg_gradient_to_tr =
  style "bg-gradient-to-tr"
    [
      Css.background_image
        "linear-gradient(to top right, var(--tw-gradient-stops))";
    ]

let bg_gradient_to_r =
  style "bg-gradient-to-r"
    [
      Css.background_image "linear-gradient(to right, var(--tw-gradient-stops))";
    ]

let bg_gradient_to_bl =
  style "bg-gradient-to-bl"
    [
      Css.background_image
        "linear-gradient(to bottom left, var(--tw-gradient-stops))";
    ]

let bg_gradient_to_l =
  style "bg-gradient-to-l"
    [
      Css.background_image "linear-gradient(to left, var(--tw-gradient-stops))";
    ]

let bg_gradient_to_tl =
  style "bg-gradient-to-tl"
    [
      Css.background_image
        "linear-gradient(to top left, var(--tw-gradient-stops))";
    ]

(** Gradient color stops *)
let from_color ?(shade = 500) color =
  let class_name =
    if Color.is_base_color color || Color.is_custom_color color then
      "from-" ^ color_name color
    else Pp.str [ "from-"; color_name color; "-"; string_of_int shade ]
  in
  let var_str =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "var(--color-"; color_name color; ")" ]
    else
      Pp.str [ "var(--color-"; color_name color; "-"; string_of_int shade; ")" ]
  in
  style class_name
    [
      Css.custom_property "--tw-gradient-from" var_str;
      Css.custom_property "--tw-gradient-stops"
        "var(--tw-gradient-via-stops,var(--tw-gradient-position),var(--tw-gradient-from)var(--tw-gradient-from-position),var(--tw-gradient-to)var(--tw-gradient-to-position))";
    ]

let via_color ?(shade = 500) color =
  let class_name =
    if Color.is_base_color color || Color.is_custom_color color then
      "via-" ^ color_name color
    else Pp.str [ "via-"; color_name color; "-"; string_of_int shade ]
  in
  let var_str =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "var(--color-"; color_name color; ")" ]
    else
      Pp.str [ "var(--color-"; color_name color; "-"; string_of_int shade; ")" ]
  in
  style class_name
    [
      Css.custom_property "--tw-gradient-via" var_str;
      Css.custom_property "--tw-gradient-via-stops"
        "var(--tw-gradient-position),var(--tw-gradient-from)var(--tw-gradient-from-position),var(--tw-gradient-via)var(--tw-gradient-via-position),var(--tw-gradient-to)var(--tw-gradient-to-position)";
      Css.custom_property "--tw-gradient-stops" "var(--tw-gradient-via-stops)";
    ]

let to_color ?(shade = 500) color =
  let class_name =
    if Color.is_base_color color || Color.is_custom_color color then
      "to-" ^ color_name color
    else Pp.str [ "to-"; color_name color; "-"; string_of_int shade ]
  in
  let var_str =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "var(--color-"; color_name color; ")" ]
    else
      Pp.str [ "var(--color-"; color_name color; "-"; string_of_int shade; ")" ]
  in
  style class_name
    [
      Css.custom_property "--tw-gradient-to" var_str;
      Css.custom_property "--tw-gradient-stops"
        "var(--tw-gradient-via-stops,var(--tw-gradient-position),var(--tw-gradient-from)var(--tw-gradient-from-position),var(--tw-gradient-to)var(--tw-gradient-to-position))";
    ]

let antialiased =
  style "antialiased"
    [
      Css.webkit_font_smoothing Antialiased;
      Css.moz_osx_font_smoothing Grayscale;
    ]

(* Text transformation utilities *)
let uppercase = style "uppercase" [ Css.text_transform Uppercase ]
let lowercase = style "lowercase" [ Css.text_transform Lowercase ]
let capitalize = style "capitalize" [ Css.text_transform Capitalize ]
let normal_case = style "normal-case" [ Css.text_transform None ]

(* Text decoration style utilities *)
let underline_solid =
  style "underline-solid" [ Css.text_decoration_style Solid ]

let underline_double =
  style "underline-double" [ Css.text_decoration_style Double ]

let underline_dotted =
  style "underline-dotted" [ Css.text_decoration_style Dotted ]

let underline_dashed =
  style "underline-dashed" [ Css.text_decoration_style Dashed ]

let underline_wavy = style "underline-wavy" [ Css.text_decoration_style Wavy ]

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
      Css.custom_property "--tw-translate-x" "0";
      Css.custom_property "--tw-translate-y" "0";
      Css.custom_property "--tw-rotate" "0";
      Css.custom_property "--tw-skew-x" "0";
      Css.custom_property "--tw-skew-y" "0";
      Css.custom_property "--tw-scale-x" "1";
      Css.custom_property "--tw-scale-y" "1";
      Css.transform
        [
          Css.Translate_x (Css.Var (Css.var "tw-translate-x"));
          Css.Translate_y (Css.Var (Css.var "tw-translate-y"));
          Css.Rotate_var { var_name = "tw-rotate"; fallback = None };
          Css.Skew_x (Css.Angle_var { var_name = "tw-skew-x"; fallback = None });
          Css.Skew_y (Css.Angle_var { var_name = "tw-skew-y"; fallback = None });
          Css.Scale_x
            (Css.Scale_var { var_name = "tw-scale-x"; fallback = None });
          Css.Scale_y
            (Css.Scale_var { var_name = "tw-scale-y"; fallback = None });
        ];
    ]

let transform_none =
  style "transform-none" [ Css.transform [ Css.Transform_none ] ]

let transform_gpu =
  style "transform-gpu" [ Css.transform [ Css.Translate_z Css.Zero ] ]

let brightness n =
  let class_name = "brightness-" ^ string_of_int n in
  let value = Pp.float (float_of_int n /. 100.0) in
  style class_name [ Css.filter ("brightness(" ^ value ^ ")") ]

let contrast n =
  let class_name = "contrast-" ^ string_of_int n in
  let value = Pp.float (float_of_int n /. 100.0) in
  style class_name [ Css.filter ("contrast(" ^ value ^ ")") ]

let blur_internal = function
  | `None -> style "blur-none" [ Css.filter "blur(0)" ]
  | `Xs -> style "blur-xs" [ Css.filter "blur(2px)" ]
  | `Sm -> style "blur-sm" [ Css.filter "blur(4px)" ]
  | `Md -> style "blur" [ Css.filter "blur(8px)" ]
  | `Lg -> style "blur-lg" [ Css.filter "blur(16px)" ]
  | `Xl -> style "blur-xl" [ Css.filter "blur(24px)" ]
  | `Xl_2 -> style "blur-2xl" [ Css.filter "blur(40px)" ]
  | `Xl_3 -> style "blur-3xl" [ Css.filter "blur(64px)" ]
  | `Full -> style "blur-full" [ Css.filter "blur(9999px)" ]

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
      Css.backdrop_filter
        (Pp.str [ "brightness("; Pp.float (float_of_int n /. 100.); ")" ]);
    ]

let backdrop_contrast n =
  let class_name = Pp.str [ "backdrop-contrast-"; string_of_int n ] in
  style class_name
    [
      Css.backdrop_filter
        (Pp.str [ "contrast("; Pp.float (float_of_int n /. 100.); ")" ]);
    ]

let backdrop_opacity n =
  let class_name = Pp.str [ "backdrop-opacity-"; string_of_int n ] in
  style class_name
    [
      Css.backdrop_filter
        (Pp.str [ "opacity("; Pp.float (float_of_int n /. 100.); ")" ]);
    ]

let backdrop_saturate n =
  let class_name = Pp.str [ "backdrop-saturate-"; string_of_int n ] in
  style class_name
    [
      Css.backdrop_filter
        (Pp.str [ "saturate("; Pp.float (float_of_int n /. 100.); ")" ]);
    ]

let backdrop_blur_internal = function
  | `None -> style "backdrop-blur-none" [ Css.backdrop_filter "blur(0)" ]
  | `Xs -> style "backdrop-blur-xs" [ Css.backdrop_filter "blur(2px)" ]
  | `Sm -> style "backdrop-blur-sm" [ Css.backdrop_filter "blur(4px)" ]
  | `Md -> style "backdrop-blur" [ Css.backdrop_filter "blur(8px)" ]
  | `Lg -> style "backdrop-blur-lg" [ Css.backdrop_filter "blur(12px)" ]
  | `Xl -> style "backdrop-blur-xl" [ Css.backdrop_filter "blur(16px)" ]
  | `Xl_2 -> style "backdrop-blur-2xl" [ Css.backdrop_filter "blur(24px)" ]
  | `Xl_3 -> style "backdrop-blur-3xl" [ Css.backdrop_filter "blur(40px)" ]
  | `Full -> style "backdrop-blur-full" [ Css.backdrop_filter "blur(9999px)" ]

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
let animate_none = style "animate-none" [ Css.animation "none" ]

let animate_spin =
  style "animate-spin" [ Css.animation "spin 1s linear infinite" ]

let animate_ping =
  style "animate-ping"
    [ Css.animation "ping 1s cubic-bezier(0, 0, 0.2, 1) infinite" ]

let animate_pulse =
  style "animate-pulse"
    [ Css.animation "pulse 2s cubic-bezier(0.4, 0, 0.6, 1) infinite" ]

let animate_bounce =
  style "animate-bounce" [ Css.animation "bounce 1s infinite" ]

(* Transition utilities *)
let duration n =
  let class_name = "duration-" ^ string_of_int n in
  style class_name [ Css.transition_duration (Css.Ms n) ]

let ease_linear = style "ease-linear" [ Css.transition_timing_function Linear ]

let ease_in =
  style "ease-in"
    [ Css.transition_timing_function (Cubic_bezier (0.4, 0.0, 1.0, 1.0)) ]

let ease_out =
  style "ease-out"
    [ Css.transition_timing_function (Cubic_bezier (0.0, 0.0, 0.2, 1.0)) ]

let ease_in_out =
  style "ease-in-out"
    [ Css.transition_timing_function (Cubic_bezier (0.4, 0.0, 0.2, 1.0)) ]

(** Transform utilities *)
let scale n =
  let value = string_of_int n ^ "%" in
  let class_name = "scale-" ^ string_of_int n in
  style class_name
    [
      Css.custom_property "--tw-scale-x" value;
      Css.custom_property "--tw-scale-y" value;
      Css.transform
        [
          Css.Translate_var
            {
              var_name = "tw-translate-x, var(--tw-translate-y";
              fallback = None;
            };
          Css.Rotate_var { var_name = "tw-rotate"; fallback = None };
          Css.Skew_x (Css.Angle_var { var_name = "tw-skew-x"; fallback = None });
          Css.Skew_y (Css.Angle_var { var_name = "tw-skew-y"; fallback = None });
          Css.Scale_x
            (Css.Scale_var { var_name = "tw-scale-x"; fallback = None });
          Css.Scale_y
            (Css.Scale_var { var_name = "tw-scale-y"; fallback = None });
        ];
    ]

(* Appearance utilities *)
let appearance_none = style "appearance-none" [ Css.appearance None ]

(* Resize utilities *)
let resize_none = style "resize-none" [ Css.resize None ]
let resize_y = style "resize-y" [ Css.resize Vertical ]
let resize_x = style "resize-x" [ Css.resize Horizontal ]
let resize = style "resize" [ Css.resize Both ]

(* Will-change utilities *)
let will_change_auto = style "will-change-auto" [ Css.will_change "auto" ]

let will_change_scroll =
  style "will-change-scroll" [ Css.will_change "scroll-position" ]

let will_change_contents =
  style "will-change-contents" [ Css.will_change "contents" ]

let will_change_transform =
  style "will-change-transform" [ Css.will_change "transform" ]

(* Contain utilities *)
let contain_none = style "contain-none" [ Css.contain "none" ]
let contain_content = style "contain-content" [ Css.contain "content" ]
let contain_layout = style "contain-layout" [ Css.contain "layout" ]
let contain_paint = style "contain-paint" [ Css.contain "paint" ]
let contain_size = style "contain-size" [ Css.contain "size" ]

(* Object position utilities *)
let object_top = style "object-top" [ Css.object_position "top" ]
let object_right = style "object-right" [ Css.object_position "right" ]
let object_bottom = style "object-bottom" [ Css.object_position "bottom" ]
let object_left = style "object-left" [ Css.object_position "left" ]
let object_center = style "object-center" [ Css.object_position "center" ]

(* Table utilities *)
let table_auto = style "table-auto" [ Css.table_layout Auto ]
let table_fixed = style "table-fixed" [ Css.table_layout Fixed ]
let border_collapse = style "border-collapse" [ Css.border_collapse Collapse ]
let border_separate = style "border-separate" [ Css.border_collapse Separate ]

let border_spacing n =
  let s = int n in
  let len = spacing_to_length s in
  style ("border-spacing-" ^ pp_spacing_suffix s) [ Css.border_spacing len ]

(* Form utilities - equivalent to @tailwindcss/forms plugin *)
let form_input =
  style "form-input"
    [
      Css.appearance None;
      Css.background_color (Rgb { r = 255; g = 255; b = 255 });
      Css.border_color (Rgb { r = 209; g = 213; b = 219 });
      Css.border_width (Px 1);
      Css.border_radius (Css.Rem 0.375);
      Css.padding_top (Rem 0.5);
      Css.padding_right (Rem 0.75);
      Css.padding_bottom (Rem 0.5);
      Css.padding_left (Rem 0.75);
      Css.font_size (Rem 1.0);
      Css.line_height (Rem 1.5);
      Css.outline "2px solid transparent";
      Css.outline_offset (Px 2);
    ]

let form_textarea =
  style "form-textarea"
    [
      Css.appearance None;
      Css.background_color (Rgb { r = 255; g = 255; b = 255 });
      Css.border_color (Rgb { r = 209; g = 213; b = 219 });
      Css.border_width (Px 1);
      Css.border_radius (Css.Rem 0.375);
      Css.padding_top (Rem 0.5);
      Css.padding_right (Rem 0.75);
      Css.padding_bottom (Rem 0.5);
      Css.padding_left (Rem 0.75);
      Css.font_size (Rem 1.0);
      Css.line_height (Rem 1.5);
      Css.resize Vertical;
    ]

let form_select =
  style "form-select"
    [
      Css.appearance None;
      Css.background_color (Rgb { r = 255; g = 255; b = 255 });
      Css.border_color (Rgb { r = 209; g = 213; b = 219 });
      Css.border_width (Px 1);
      Css.border_radius (Css.Rem 0.375);
      Css.padding_top (Rem 0.5);
      Css.padding_right (Css.Rem 2.5);
      Css.padding_bottom (Rem 0.5);
      Css.padding_left (Rem 0.75);
      Css.font_size (Rem 1.0);
      Css.line_height (Rem 1.5);
      Css.background_image
        "url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' \
         fill='none' viewBox='0 0 20 20'%3e%3cpath stroke='%236b7280' \
         stroke-linecap='round' stroke-linejoin='round' stroke-width='1.5' \
         d='M6 8l4 4 4-4'/%3e%3c/svg%3e\")";
      Css.background_position "right 0.5rem center";
      Css.background_repeat No_repeat;
      Css.background_size "1.5em 1.5em";
    ]

let form_checkbox =
  style "form-checkbox"
    [
      Css.appearance None;
      Css.width (Rem 1.0);
      Css.height (Rem 1.0);
      Css.background_color (Rgb { r = 255; g = 255; b = 255 });
      Css.border_color (Rgb { r = 209; g = 213; b = 219 });
      Css.border_width (Px 1);
      Css.border_radius (Rem 0.25);
      Css.color (Rgb { r = 59; g = 130; b = 246 });
      Css.Flex.shrink 0.0;
      Css.display Inline_block;
      Css.vertical_align Middle;
    ]

let form_radio =
  style "form-radio"
    [
      Css.appearance None;
      Css.width (Rem 1.0);
      Css.height (Rem 1.0);
      Css.background_color (Rgb { r = 255; g = 255; b = 255 });
      Css.border_color (Rgb { r = 209; g = 213; b = 219 });
      Css.border_width (Px 1);
      Css.border_radius (Pct 100.0);
      Css.color (Rgb { r = 59; g = 130; b = 246 });
      Css.Flex.shrink 0.0;
      Css.display Inline_block;
      Css.vertical_align Middle;
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
  Css.stylesheet (List.map (fun r -> Css.Rule r) (root_rule :: all_rules))

(* Line clamp utility function *)
let line_clamp n =
  let class_name = "line-clamp-" ^ string_of_int n in
  if n = 0 then style "line-clamp-none" [ Css.webkit_line_clamp "none" ]
  else style class_name [ Css.webkit_line_clamp (string_of_int n) ]

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
      Ok (style class_name [ Css.line_height (Css.Rem rem_value) ])

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
