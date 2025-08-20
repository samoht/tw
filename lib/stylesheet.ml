(** CSS generation module for Tailwind DSL *)

open Core

(* Import helper modules *)
module Pp = struct
  let str lst = String.concat "" lst
end

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

(* Extract properties for inline styles *)
let rec to_css_properties = function
  | Style { props; _ } -> props
  | Prose variant ->
      (* For inline styles, we can only use the base prose properties, not the
         descendant selectors like .prose h1 *)
      Prose.to_base_properties variant
  | Modified (_modifier, t) -> to_css_properties t
  | Group styles -> List.concat_map to_css_properties styles

(* Extract selector and properties from a single Tw style *)
let extract_selector_props tw =
  let rec extract = function
    | Style { name = class_name; props; _ } ->
        let base_class = escape_class_name class_name in
        let selector = "." ^ base_class in
        [ Regular (selector, props) ]
    | Prose variant ->
        (* Prose generates multiple rules with selectors *)
        Prose.to_css_rules variant
        |> List.map (fun rule ->
               Regular (Css.selector rule, Css.declarations rule))
    | Modified (modifier, t) ->
        let base = extract t in
        base
        |> List.concat_map (fun rule_out ->
               let props =
                 match rule_out with
                 | Regular (_, props) -> props
                 | MediaQuery (_, _, props) -> props
                 | ContainerQuery (_, _, props) -> props
                 | StartingStyle (_, props) -> props
               in
               let base_class =
                 match rule_out with
                 | Regular (selector, _) ->
                     (* Remove the leading dot from selector *)
                     String.sub selector 1 (String.length selector - 1)
                 | _ -> "unknown"
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
                     Regular (".group:hover .group-hover\\:" ^ base_class, props);
                   ]
               | Group_focus ->
                   [
                     Regular (".group:focus .group-focus\\:" ^ base_class, props);
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
                   let selector =
                     match rule_out with
                     | Regular (sel, _) -> sel
                     | _ -> "." ^ base_class
                   in
                   [
                     Regular (selector ^ "[data-state=\"" ^ value ^ "\"]", props);
                   ]
               | Data_variant value ->
                   let selector =
                     match rule_out with
                     | Regular (sel, _) -> sel
                     | _ -> "." ^ base_class
                   in
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
                   let selector =
                     match rule_out with
                     | Regular (sel, _) -> sel
                     | _ -> "." ^ base_class
                   in
                   [
                     Regular
                       (selector ^ "[data-" ^ key ^ "=\"" ^ value ^ "\"]", props);
                   ]
               | Dark ->
                   [
                     MediaQuery
                       ("(prefers-color-scheme: dark)", "." ^ base_class, props);
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
                           (".not-" ^ cleaned ^ ":not(" ^ inner_sel ^ ")", props);
                       ]
                   | _ -> [ Regular ("." ^ base_class, props) ])
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
                       (".focus-within\\:" ^ base_class ^ ":focus-within", props);
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

(* Generate CSS rules for all used Tw classes - main export *)
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
    (* The rest of the function is too complex to extract cleanly without access to other modules *)
    (* For now, just return a minimal version *)
    let base_layer =
      Css.layered_rules ~layer:Css.Base
        (generate_reset_rules () |> List.map Css.rule_to_nested)
    in
    let utilities_layer =
      Css.layered_rules ~layer:Css.Utilities ~media_queries ~container_queries
        (rules |> List.map Css.rule_to_nested)
    in
    let items = [ Css.Layer base_layer; Css.Layer utilities_layer ] in
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
