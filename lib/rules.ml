(** CSS rule generation and management *)

open Core
open Css

(** {1 Types} *)

type rule_output =
  | Regular of string * Css.declaration list (* selector, properties *)
  | Media_query of
      string
      * string
      * Css.declaration list (* condition, selector, properties *)
  | Container_query of
      string
      * string
      * Css.declaration list (* condition, selector, properties *)
  | Starting_style of string * Css.declaration list (* selector, properties *)

(** {1 Helper Functions} *)

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
      | '/' -> Buffer.add_string buf "\\/"
      | ':' -> Buffer.add_string buf "\\:"
      | '%' -> Buffer.add_string buf "\\%"
      | '.' -> Buffer.add_string buf "\\."
      | '#' -> Buffer.add_string buf "\\#"
      | ' ' -> Buffer.add_string buf "\\ "
      | '"' -> Buffer.add_string buf "\\\""
      | '\'' -> Buffer.add_string buf "\\'"
      | '@' -> Buffer.add_string buf "\\@"
      | c -> Buffer.add_char buf c)
    name;
  Buffer.contents buf

(* Extract selector and properties from a single Tw style *)
let extract_selector_props tw =
  let rec extract = function
    | Style { name = class_name; props; rules; _ } ->
        let escaped_name = escape_class_name class_name in
        (match rules with
        | None -> [ Regular ("." ^ escaped_name, props) ]
        | Some rule_list ->
            (* Convert custom rules to selector/props pairs *)
            rule_list
            |> List.map (fun rule ->
                   Regular (Css.selector rule, Css.declarations rule)))
    | Modified (modifier, t) ->
        let base = extract t in
        List.concat_map
          (fun rule_out ->
            match rule_out with
            | Regular (selector, props) ->
                let base_class =
                  String.sub selector 1 (String.length selector - 1)
                in
                (match modifier with
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
                      Media_query
                        ("(prefers-color-scheme: dark)", selector, props);
                    ]
                | Responsive breakpoint ->
                    let prefix = string_of_breakpoint breakpoint in
                    let condition =
                      "(min-width:" ^ responsive_breakpoint prefix ^ ")"
                    in
                    let sel = "." ^ prefix ^ "\\:" ^ base_class in
                    [ Media_query (condition, sel, props) ]
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
                    [ Container_query (cond, escaped_class, props) ]
                (* New v4 modifiers *)
                | Not _modifier ->
                    [
                      Regular
                        (".not-" ^ base_class ^ ":not(" ^ selector ^ ")", props);
                    ]
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
                | Starting -> [ Starting_style ("." ^ base_class, props) ]
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
                      Media_query
                        ( "(prefers-reduced-motion: no-preference)",
                          ".motion-safe\\:" ^ base_class,
                          props );
                    ]
                | Motion_reduce ->
                    [
                      Media_query
                        ( "(prefers-reduced-motion: reduce)",
                          ".motion-reduce\\:" ^ base_class,
                          props );
                    ]
                | Contrast_more ->
                    [
                      Media_query
                        ( "(prefers-contrast: more)",
                          ".contrast-more\\:" ^ base_class,
                          props );
                    ]
                | Contrast_less ->
                    [
                      Media_query
                        ( "(prefers-contrast: less)",
                          ".contrast-less\\:" ^ base_class,
                          props );
                    ]
                | Pseudo_before ->
                    [ Regular (".before\\:" ^ base_class ^ "::before", props) ]
                | Pseudo_after ->
                    [ Regular (".after\\:" ^ base_class ^ "::after", props) ])
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
        Css.tab_size 4;
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
        Css.webkit_tap_highlight_color Transparent;
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
      [ Css.webkit_appearance None ];
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

(* Helper: Separate rules by type *)
let separate_rules_by_type all_rules =
  let regular_rules, media_rules, container_rules, starting_rules =
    List.fold_left
      (fun (reg, media, cont, start) rule ->
        match rule with
        | Regular _ -> (rule :: reg, media, cont, start)
        | Media_query _ -> (reg, rule :: media, cont, start)
        | Container_query _ -> (reg, media, rule :: cont, start)
        | Starting_style _ -> (reg, media, cont, rule :: start))
      ([], [], [], []) all_rules
  in
  (* Reverse to maintain original order since we prepended *)
  ( List.rev regular_rules,
    List.rev media_rules,
    List.rev container_rules,
    List.rev starting_rules )

(* Helper: Check if a selector is a hover rule *)
let is_hover_rule selector =
  String.contains selector ':'
  && String.contains selector 'h'
  && String.contains selector 'o'
  && String.contains selector 'v'
  && String.contains selector 'e'
  && String.contains selector 'r'

(* Helper: Group media query rules by condition *)
let group_media_queries media_rules =
  List.fold_left
    (fun acc rule ->
      match rule with
      | Media_query (condition, selector, props) ->
          let rules = try List.assoc condition acc with Not_found -> [] in
          (condition, (selector, props) :: rules)
          :: List.remove_assoc condition acc
      | _ -> acc)
    [] media_rules

(* Helper: Group container query rules by condition *)
let group_container_queries container_rules =
  List.fold_left
    (fun acc rule ->
      match rule with
      | Container_query (condition, selector, props) ->
          let rules = try List.assoc condition acc with Not_found -> [] in
          (condition, (selector, props) :: rules)
          :: List.remove_assoc condition acc
      | _ -> acc)
    [] container_rules

(* Generate CSS rules for all used Tw classes *)
let to_css ?(reset = true) ?(mode = Css.Variables) tw_classes =
  (* Mode will be used when rendering CSS with variables/inline/optimize *)
  let _ = mode in
  let all_rules = tw_classes |> List.concat_map extract_selector_props in
  (* Separate rules by type *)
  let regular_rules, media_rules, container_rules, _starting_rules =
    separate_rules_by_type all_rules
  in

  (* Group regular rules by selector *)
  let grouped_regular = group_regular_rules regular_rules in

  (* Separate hover rules from regular rules *)
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
  let media_queries_map = group_media_queries media_rules in

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
  let container_queries_map = group_container_queries container_rules in

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
    let rec collect_all_vars style =
      match style with
      | Style { vars; _ } -> vars
      | Modified (_, t) -> collect_all_vars t
      | Group styles -> List.concat_map collect_all_vars styles
    in

    let all_vars = tw_classes |> List.concat_map collect_all_vars in

    (* Properties layer - with GADT declarations, analyze_properties returns
       empty *)
    let properties_layer_opt =
      let properties = [] in
      (* No properties layer needed *)
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
                 | Media_query (_, _, props)
                 | Container_query (_, _, props)
                 | Starting_style (_, props) ->
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

    (* Generate CSS properties from Core.var values *)
    let var_generated_props =
      all_vars
      |> List.sort_uniq compare (* Remove duplicates *)
      |> List.concat_map Core.var_to_css_properties
      |> List.map (fun (name, value) -> Css.custom_property name value)
    in

    (* Track which variables were already generated from Core.var *)
    let generated_var_names =
      all_vars
      |> List.concat_map Core.var_to_css_properties
      |> List.map fst (* Get just the property names *)
      |> List.sort_uniq String.compare
    in

    (* Generate values for theme variables *)
    let theme_generated_vars =
      all_referenced_vars
      |> List.filter (fun var_name ->
             (* Skip variables already generated from Core.var *)
             not (List.mem var_name generated_var_names))
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
      font_vars @ var_generated_props @ theme_generated_vars
      @ default_font_props
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

    (* Combine all layers *)
    let base_layers =
      [ theme_layer; base_layer; components_layer; utilities_layer ]
    in

    (* Add properties layer if needed *)
    let layers =
      match properties_layer_opt with
      | Some props_layer -> props_layer :: base_layers
      | None -> base_layers
    in

    (* Generate @property rules for variables that need them *)
    (* TODO: Implement var_tally tracking and at_property generation *)
    let at_properties = [] in

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
  let rec to_css_properties = function
    | Style { props; _ } -> props
    | Modified (_, t) -> to_css_properties t
    | Group styles -> List.concat_map to_css_properties styles
  in
  let all_props = List.concat_map to_css_properties styles in
  let deduped = Css.deduplicate_declarations all_props in
  Css.inline_style_of_declarations deduped
