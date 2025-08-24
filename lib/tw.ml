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

(* Import and re-export core types from Core module *)
include Core

type color = Color.t

(* Backwards compatibility - keep scale type that hasn't been moved yet *)
type scale = [ spacing | size | `Screen | `Min | `Max | `Fit ]
type max_scale = [ scale | `Xl_4 | `Xl_5 | `Xl_6 | `Xl_7 ]

(* Re-export CSS module *)
module Css = Css

(* Import color constructors from Color module *)
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
let hex = Color.hex
let rgb = Color.rgb

(** {1 Helpers} *)

(* These are now imported from Core *)
let style = Core.style
let style_with_vars = Core.style_with_vars

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
                        ( ".peer-hover\\:" ^ base_class
                          ^ ":is(:where(.peer):hover~*)",
                          props );
                    ]
                | Peer_focus ->
                    [
                      Regular
                        ( ".peer-focus\\:" ^ base_class
                          ^ ":is(:where(.peer):focus~*)",
                          props );
                    ]
                | Peer_checked ->
                    [
                      Regular
                        ( ".peer-checked\\:" ^ base_class
                          ^ ":is(:where(.peer):checked~*)",
                          props );
                    ]
                | Aria_checked ->
                    [
                      Regular
                        ( ".aria-checked\\:" ^ base_class ^ "[aria-checked=true]",
                          props );
                    ]
                | Aria_expanded ->
                    [
                      Regular
                        ( ".aria-expanded\\:" ^ base_class
                          ^ "[aria-expanded=true]",
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
                          ^ "[aria-disabled=true]",
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
                    let prefix =
                      Containers.container_query_to_class_prefix query
                    in
                    let escaped_class = ".\\" ^ prefix ^ "\\:" ^ base_class in
                    let condition =
                      Containers.container_query_to_css_prefix query
                    in
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
        Css.text_decoration Underline_dotted;
      ];
    (* Headings *)
    Css.rule ~selector:"h1, h2, h3, h4, h5, h6"
      [ Css.font_size Inherit; Css.font_weight Inherit ];
    (* Links *)
    Css.rule ~selector:"a"
      [
        Css.color Inherit;
        Css.webkit_text_decoration "inherit";
        Css.webkit_text_decoration "inherit";
        Css.webkit_text_decoration "inherit";
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
        Css.min_height (Lh 1.0);
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
      [ Css.important (Css.display None) ]
    (* Placeholder styling with @supports - added as a raw rule string for now *)
    (* This needs to be added as part of the base layer but after the main rules *);
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

(* Canonical color ordering function - shared between variables and classes *)
let canonical_color_order color_name =
  match color_name with
  | "red" -> 0 (* Chromatic colors in spectrum order *)
  | "orange" -> 1
  | "amber" -> 2
  | "yellow" -> 3
  | "lime" -> 4
  | "green" -> 5
  | "emerald" -> 6
  | "teal" -> 7
  | "cyan" -> 8
  | "sky" -> 9
  | "blue" -> 10
  | "indigo" -> 11
  | "violet" -> 12
  | "purple" -> 13
  | "fuchsia" -> 14
  | "pink" -> 15
  | "rose" -> 16
  | "slate" -> 17 (* Neutral colors after chromatic *)
  | "gray" -> 18
  | "zinc" -> 19
  | "neutral" -> 20
  | "stone" -> 21
  | "black" -> 100 (* Special colors at the end *)
  | "white" -> 101
  | _ -> 200 (* Unknown colors last *)

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
  (* Reverse to maintain original order since we prepended *)
  let regular_rules = List.rev regular_rules in
  let media_rules = List.rev media_rules in
  let container_rules = List.rev container_rules in

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
      (* Just deduplicate without sorting alphabetically *)
      |> List.fold_left
           (fun acc v -> if List.mem v acc then acc else v :: acc)
           []
      |> List.rev
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
    (* Define canonical order for theme variables - spacing before radius *)
    let canonical_var_order var =
      match var with
      | "--spacing" -> 0
      | "--font-sans" -> 1
      | "--font-serif" -> 2
      | "--font-mono" -> 3
      | s when String.starts_with ~prefix:"--text-" s -> 10
      | s when String.starts_with ~prefix:"--font-weight-" s -> 20
      | s when String.starts_with ~prefix:"--radius-" s -> 30
      | s when String.starts_with ~prefix:"--color-" s ->
          (* Extract color name from --color-{name}-{shade} *)
          let color_part = String.sub s 8 (String.length s - 8) in
          let color_name =
            try
              let dash_pos = String.index color_part '-' in
              String.sub color_part 0 dash_pos
            with Not_found -> color_part
          in
          (* Use canonical color order for variables *)
          40 + canonical_color_order color_name
      | _ -> 200
    in
    let all_referenced_vars =
      directly_referenced_vars @ vars_from_defaults
      |> List.sort_uniq (fun a b ->
             let order_a = canonical_var_order a in
             let order_b = canonical_var_order b in
             if order_a <> order_b then Int.compare order_a order_b
             else String.compare a b)
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
                           let color = Color.of_string_exn color_name in
                           [
                             Css.custom_property var_name
                               (Color.to_oklch_css color shade);
                           ]
                         with _ -> [])
                     | _ -> []))
             | _ -> [])
      (* Don't sort - preserve the canonical order from all_referenced_vars *)
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
                  Css.color
                    (Css.Mix
                       {
                         in_space = Oklab;
                         color1 = Current;
                         percent1 = Some 50;
                         color2 = Transparent;
                         percent2 = None;
                       });
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
      else if starts "bg-" core then
        (* For background colors, use Tailwind's color ordering *)
        let sub_order =
          (* Extract color name from bg-color-shade pattern *)
          try
            let color_part = String.sub core 3 (String.length core - 3) in
            (* Find the color name (everything before the last hyphen) *)
            let color_name =
              try
                let last_dash = String.rindex color_part '-' in
                String.sub color_part 0 last_dash
              with Not_found -> color_part
            in
            (* Tailwind v4 uses alphabetical order for utility classes *)
            match color_name with
            | "amber" -> 0
            | "blue" -> 1
            | "cyan" -> 2
            | "emerald" -> 3
            | "fuchsia" -> 4
            | "gray" -> 5
            | "green" -> 6
            | "indigo" -> 7
            | "lime" -> 8
            | "neutral" -> 9
            | "orange" -> 10
            | "pink" -> 11
            | "purple" -> 12
            | "red" -> 13
            | "rose" -> 14
            | "sky" -> 15
            | "slate" -> 16
            | "stone" -> 17
            | "teal" -> 18
            | "violet" -> 19
            | "yellow" -> 20
            | "zinc" -> 21
            | _ -> 100 (* Other colors come after *)
          with _ -> 0
        in
        (200, sub_order)
      else if
        List.exists
          (fun p -> starts p core)
          [ "from-"; "via-"; "to-" (* gradients *) ]
      then (200, 50) (* 3xx: padding - comes AFTER background *)
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
          ]
      then (900, 0) (* Alignment and justification come before gap *)
      else if starts "items-" core then (901, 0)
      else if starts "justify-" core then (901, 1)
      else if
        List.exists (fun p -> starts p core) [ "content-"; "self-"; "place-" ]
      then (901, 2) (* Gap and space utilities come after alignment *)
      else if List.exists (fun p -> starts p core) [ "gap-"; "space-" ] then
        (902, 0) (* 10xx: container, prose *)
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
      |> List.filter_map (fun var ->
             match Var.at_property_config var with
             | None ->
                 (* Default for variables without specific config *)
                 let var_name = Var.to_string var in
                 Some
                   (Css.at_property ~name:var_name ~syntax:"*" ~initial_value:""
                      ~inherits:false ())
             | Some (name, syntax, inherits, initial_value) ->
                 Some
                   (Css.at_property ~name ~syntax ~initial_value ~inherits ()))
    in

    (* Don't add empty Properties layer *)
    (* Media queries and container queries are already included in the
       utilities layer, don't duplicate *)
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

(* int_to_length removed - no longer used *)

(** {1 Public API} *)

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

(* Include only the color application utilities we need *)
include struct
  open Color

  let bg c s = bg c s
  let bg_transparent = bg_transparent
  let bg_current = bg_current
  let bg_black = bg_black
  let bg_white = bg_white
  let bg_gray = bg_gray
  let bg_slate = bg_slate
  let bg_zinc = bg_zinc
  let bg_neutral = bg_neutral
  let bg_stone = bg_stone
  let bg_red = bg_red
  let bg_orange = bg_orange
  let bg_amber = bg_amber
  let bg_yellow = bg_yellow
  let bg_lime = bg_lime
  let bg_green = bg_green
  let bg_emerald = bg_emerald
  let bg_teal = bg_teal
  let bg_cyan = bg_cyan
  let bg_sky = bg_sky
  let bg_blue = bg_blue
  let bg_indigo = bg_indigo
  let bg_violet = bg_violet
  let bg_purple = bg_purple
  let bg_fuchsia = bg_fuchsia
  let bg_pink = bg_pink
  let bg_rose = bg_rose
  let text c s = text c s
  let text_transparent = text_transparent
  let text_current = text_current
  let text_black = text_black
  let text_white = text_white
  let text_gray = text_gray
  let text_slate = text_slate
  let text_zinc = text_zinc
  let text_neutral = text_neutral
  let text_stone = text_stone
  let text_red = text_red
  let text_orange = text_orange
  let text_amber = text_amber
  let text_yellow = text_yellow
  let text_lime = text_lime
  let text_green = text_green
  let text_emerald = text_emerald
  let text_teal = text_teal
  let text_cyan = text_cyan
  let text_sky = text_sky
  let text_blue = text_blue
  let text_indigo = text_indigo
  let text_violet = text_violet
  let text_purple = text_purple
  let text_fuchsia = text_fuchsia
  let text_pink = text_pink
  let text_rose = text_rose
  let border_color c s = border_color c s
  let border_transparent = border_transparent
  let border_current = border_current
  let border_black = border_black
  let border_white = border_white
  let border_gray = border_gray
  let border_slate = border_slate
  let border_zinc = border_zinc
  let border_neutral = border_neutral
  let border_stone = border_stone
  let border_red = border_red
  let border_orange = border_orange
  let border_amber = border_amber
  let border_yellow = border_yellow
  let border_lime = border_lime
  let border_green = border_green
  let border_emerald = border_emerald
  let border_teal = border_teal
  let border_cyan = border_cyan
  let border_sky = border_sky
  let border_blue = border_blue
  let border_indigo = border_indigo
  let border_violet = border_violet
  let border_purple = border_purple
  let border_fuchsia = border_fuchsia
  let border_pink = border_pink
  let border_rose = border_rose
end

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

(* pp_margin_suffix removed - no longer used *)

(* margin_to_length removed - no longer used *)

let rec scale_to_length : scale -> Css.length = function
  | `Screen -> Css.Vh 100.0
  | `Min | `Max | `Fit ->
      Css.Auto (* These need special handling, using Auto for now *)
  | #spacing as s -> Spacing.spacing_to_length s
  | #size as s -> size_to_length s

and size_to_length : size -> Css.length = function
  | `None -> Zero
  | `Xs -> Rem 0.125
  | `Sm -> Rem 0.25
  | `Md -> Rem 0.375
  | `Lg -> Rem 0.5
  | `Xl -> Rem 0.75
  | `Xl_2 -> Rem 1.0
  | `Xl_3 -> Rem 1.5
  | `Full -> Pct 100.0

let max_scale_to_length : max_scale -> Css.length = function
  | `Xl_4 -> Rem 56.0
  | `Xl_5 -> Rem 64.0
  | `Xl_6 -> Rem 72.0
  | `Xl_7 -> Rem 80.0
  | #scale as s -> scale_to_length s

(** {1 Spacing} *)

(* Helper to extract spacing variables from scale types - still needed for
   sizing *)
let scale_vars = function
  | `Rem _ ->
      [] (* The --spacing variable is handled via string parsing in all_vars *)
  | _ -> []

(* Include all spacing utilities *)
include Spacing

(* Special padding values for backward compatibility *)
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

(* Common margin utilities for backward compatibility *)
let m_auto = m' `Auto
let mx_auto = mx' `Auto
let my_auto = my' `Auto
let mt_auto = mt' `Auto
let mr_auto = mr' `Auto
let mb_auto = mb' `Auto
let ml_auto = ml' `Auto

(* Special gap values *)
let gap_px = gap' `Px
let gap_full = gap' `Full

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

(* Include all typography utilities *)
include Typography

(** {1 Layout} *)

(* Include all layout utilities *)
include Layout

(** {1 Borders} *)

(* Include all border utilities *)
include Borders

(** {1 Effects} *)

(* Include all effect utilities *)
include Effects

(** {1 Transforms} *)

(* Include all transform utilities *)
include Transforms

(** {1 Interactivity} *)

(* Include all interactivity utilities *)
include Interactivity

(** {1 Containers} *)

(* Include all container query utilities *)
include Containers

(** {1 Filters} *)

(* Include all filter utilities *)
include Filters

(* Include positioning utilities *)
include Positioning

(* Include animation and transition utilities *)
include Animations

(* Include form utilities *)
include Forms

(** {1 Sizing} *)

(* Sizing utilities are provided through wrapper functions, not included
   directly to avoid naming conflicts *)

(* Modifiers *)

(** {1 Additional Utilities} *)

(* Grid utilities that depend on dynamic values *)
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

(* Opacity wrapper for int-based API *)
let opacity n =
  let class_name = "opacity-" ^ string_of_int n in
  let value = float_of_int n /. 100.0 in
  style class_name [ Css.opacity value ]

(* These utilities haven't been moved to separate modules yet *)

(* Place content utilities - shorthand for align-content and justify-content in
   Grid *)
let content_stretch = style "content-stretch" [ Css.align_content Stretch ]

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

(* Justify self utilities - for Grid items *)
let justify_self_auto = style "justify-self-auto" [ Css.justify_self Auto ]
let justify_self_start = style "justify-self-start" [ Css.justify_self Start ]
let justify_self_end = style "justify-self-end" [ Css.justify_self End ]

let justify_self_center =
  style "justify-self-center" [ Css.justify_self Center ]

let justify_self_stretch =
  style "justify-self-stretch" [ Css.justify_self Stretch ]

(* Additional typography utilities *)
let leading_6 = style "leading-6" [ Css.line_height (Rem 1.5) ]
let whitespace_normal = style "whitespace-normal" [ Css.white_space Normal ]
let whitespace_nowrap = style "whitespace-nowrap" [ Css.white_space Nowrap ]
let whitespace_pre = style "whitespace-pre" [ Css.white_space Pre ]

let whitespace_pre_line =
  style "whitespace-pre-line" [ Css.white_space Pre_line ]

let whitespace_pre_wrap =
  style "whitespace-pre-wrap" [ Css.white_space Pre_wrap ]

(* Additional border utilities with custom implementations *)
let border_internal (w : size) =
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

let border_xs = border_internal `Xs
let border_sm = border_internal `Sm
let border_md = border_internal `Md
let border_lg = border_internal `Lg
let border_xl = border_internal `Xl
let border_2xl = border_internal `Xl_2
let border_3xl = border_internal `Xl_3
let border_full = border_internal `Full

let border_none_style =
  style "border-none"
    [
      Css.custom_property "--tw-border-style" "none"; Css.border_style Css.None;
    ]

(* Table utilities *)
let border_collapse = style "border-collapse" [ Css.border_collapse Collapse ]
let border_separate = style "border-separate" [ Css.border_collapse Separate ]

let border_spacing n =
  let class_name = "border-spacing-" ^ string_of_int n in
  style class_name [ Css.border_spacing (Rem (float_of_int n *. 0.25)) ]

let table_auto = style "table-auto" [ Css.table_layout Auto ]
let table_fixed = style "table-fixed" [ Css.table_layout Fixed ]

(* Appearance utilities *)
let appearance_none = style "appearance-none" [ Css.appearance None ]

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

type width = size

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
    if Color.is_base_color color then Pp.str [ "ring-"; Color.pp color ]
    else Pp.str [ "ring-"; Color.pp color; "-"; string_of_int shade ]
  in
  style class_name []

let isolate = style "isolate" [ Css.isolation Css.Isolate ]

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
      "from-" ^ Color.pp color
    else Pp.str [ "from-"; Color.pp color; "-"; string_of_int shade ]
  in
  let var_str =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "var(--color-"; Color.pp color; ")" ]
    else
      Pp.str [ "var(--color-"; Color.pp color; "-"; string_of_int shade; ")" ]
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
      "via-" ^ Color.pp color
    else Pp.str [ "via-"; Color.pp color; "-"; string_of_int shade ]
  in
  let var_str =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "var(--color-"; Color.pp color; ")" ]
    else
      Pp.str [ "var(--color-"; Color.pp color; "-"; string_of_int shade; ")" ]
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
      "to-" ^ Color.pp color
    else Pp.str [ "to-"; Color.pp color; "-"; string_of_int shade ]
  in
  let var_str =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "var(--color-"; Color.pp color; ")" ]
    else
      Pp.str [ "var(--color-"; Color.pp color; "-"; string_of_int shade; ")" ]
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
let color_to_string = Color.to_name

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
          Containers.container_query_to_class_prefix query ^ ":" ^ base_class
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

let int_of_string_positive name s =
  match int_of_string_opt s with
  | None -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))
  | Some n when n >= 0 -> Ok n
  | Some _ -> Error (`Msg (name ^ " must be non-negative: " ^ s))

(* Helper for Result.bind-like operation *)

(* Helper for "try this or else try that" *)
let ( <|> ) r1 r2 = match r1 with Ok _ -> r1 | Error _ -> r2

(* Helper for Result.map-like operation *)
let ( >|= ) r f = match r with Error _ as e -> e | Ok x -> Ok (f x)

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

(* Parse utility classes *)
let utility_classes_of_string = function
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
    Color.color_classes_of_string parts
    <|>
    (* Try spacing utilities *)
    Spacing.of_string parts
    <|>
    (* Try sizing utilities *)
    (match parts with
    | "w" :: _ -> width_of_string parts
    | "h" :: _ -> height_of_string parts
    | "min" :: "w" :: _ -> min_width_of_string parts
    | "min" :: "h" :: _ -> min_height_of_string parts
    | "max" :: "w" :: _ -> max_width_of_string parts
    | "max" :: "h" :: _ -> max_height_of_string parts
    | _ -> Error (`Msg ""))
    <|>
    (* Try layout utilities *)
    Layout.of_string parts
    <|>
    (* Try typography utilities *)
    Typography.of_string parts
    <|>
    (* Try border utilities *)
    Borders.of_string parts
    <|>
    (* Try effects utilities *)
    Effects.of_string parts
    <|>
    (* Try transform utilities *)
    Transforms.of_string parts
    <|>
    (* Try interactivity utilities *)
    Interactivity.of_string parts
    <|>
    (* Try container utilities *)
    Containers.of_string parts
    <|>
    (* Try filter utilities *)
    Filters.of_string parts
    <|>
    (* Try positioning utilities *)
    Positioning.of_string parts
    <|>
    (* Try animation and transition utilities *)
    Animations.of_string parts
    <|>
    (* Try form utilities *)
    Forms.of_string parts
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
