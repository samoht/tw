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

(* Helper: Handle modifier rules that need special treatment *)
let modifier_to_rule modifier base_class selector props =
  match modifier with
  | Data_state value ->
      Regular (selector ^ "[data-state=\"" ^ value ^ "\"]", props)
  | Data_variant value ->
      Regular (selector ^ "[data-variant=\"" ^ value ^ "\"]", props)
  | Data_custom (key, value) ->
      Regular (selector ^ "[data-" ^ key ^ "=\"" ^ value ^ "\"]", props)
  | Dark -> Media_query ("(prefers-color-scheme: dark)", selector, props)
  | Responsive breakpoint ->
      let prefix = string_of_breakpoint breakpoint in
      let condition = "(min-width:" ^ responsive_breakpoint prefix ^ ")" in
      let sel = "." ^ prefix ^ "\\:" ^ base_class in
      Media_query (condition, sel, props)
  | Container query ->
      let prefix = Containers.container_query_to_class_prefix query in
      let escaped_class = ".\\" ^ prefix ^ "\\:" ^ base_class in
      let condition = Containers.container_query_to_css_prefix query in
      let cond =
        if String.starts_with ~prefix:"@container " condition then
          String.sub condition 11 (String.length condition - 11)
        else "(min-width: 0)"
      in
      Container_query (cond, escaped_class, props)
  | Not _modifier ->
      Regular (".not-" ^ base_class ^ ":not(" ^ selector ^ ")", props)
  | Has selector_str ->
      Regular
        ( ".has-\\[" ^ selector_str ^ "\\]\\:" ^ base_class ^ ":has("
          ^ selector_str ^ ")",
          props )
  | Group_has selector_str ->
      Regular
        ( ".group:has(" ^ selector_str ^ ") .group-has-\\[" ^ selector_str
          ^ "\\]\\:" ^ base_class,
          props )
  | Peer_has selector_str ->
      Regular
        ( ".peer:has(" ^ selector_str ^ ") ~ .peer-has-\\[" ^ selector_str
          ^ "\\]\\:" ^ base_class,
          props )
  | Starting -> Starting_style ("." ^ base_class, props)
  | Motion_safe ->
      Media_query
        ( "(prefers-reduced-motion: no-preference)",
          ".motion-safe\\:" ^ base_class,
          props )
  | Motion_reduce ->
      Media_query
        ( "(prefers-reduced-motion: reduce)",
          ".motion-reduce\\:" ^ base_class,
          props )
  | Contrast_more ->
      Media_query
        ("(prefers-contrast: more)", ".contrast-more\\:" ^ base_class, props)
  | Contrast_less ->
      Media_query
        ("(prefers-contrast: less)", ".contrast-less\\:" ^ base_class, props)
  | _ ->
      (* For simple modifiers, use the selector helper from Modifiers *)
      let sel = Modifiers.to_selector modifier base_class in
      Regular (sel, props)

(* Extract selector and properties from a single Tw style *)
let extract_selector_props tw =
  let rec extract = function
    | Style { name = class_name; props; rules; _ } -> (
        let escaped_name = escape_class_name class_name in
        match rules with
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
                [ modifier_to_rule modifier base_class selector props ]
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
  (* A hover rule ends with :hover pseudo-class *)
  let len = String.length selector in
  len >= 6 && String.sub selector (len - 6) 6 = ":hover"

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

(** {1 Extracted helpers to reduce nesting in to_css} *)

(* Utility rule ordering: conflict-aware grouping (extracted) *)
let conflict_group selector =
  let core =
    if String.starts_with ~prefix:"." selector then
      String.sub selector 1 (String.length selector - 1)
    else selector
  in
  let starts prefix s =
    let lp = String.length prefix and ls = String.length s in
    ls >= lp && String.sub s 0 lp = prefix
  in
  if core = "hidden" then (10, 3)
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
  else if starts "bg-" core then
    let sub_order =
      try
        let color_part = String.sub core 3 (String.length core - 3) in
        let color_name =
          try
            let last_dash = String.rindex color_part '-' in
            String.sub color_part 0 last_dash
          with Not_found -> color_part
        in
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
        | _ -> 100
      with
      | Invalid_argument _ -> 0
      | Not_found -> 0
    in
    (200, sub_order)
  else if List.exists (fun p -> starts p core) [ "from-"; "via-"; "to-" ] then
    (200, 50)
  else if starts "p-" core then (300, 0)
  else if starts "px-" core || starts "py-" core then (300, 1)
  else if List.exists (fun p -> starts p core) [ "pt-"; "pr-"; "pb-"; "pl-" ]
  then (300, 2)
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
  then (400, 0)
  else if
    starts "rounded" core || starts "border" core || starts "outline-" core
  then (500, 0)
  else if
    List.exists
      (fun p -> starts p core)
      [ "w-"; "h-"; "min-w-"; "min-h-"; "max-w-"; "max-h-" ]
  then (600, 0)
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
  then (700, 0)
  else if
    List.exists
      (fun p -> starts p core)
      [ "cursor-"; "select-"; "resize-"; "scroll-"; "overflow-"; "overscroll-" ]
  then (800, 0)
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
  then (900, 0)
  else if starts "items-" core then (901, 0)
  else if starts "justify-" core then (901, 1)
  else if List.exists (fun p -> starts p core) [ "content-"; "self-"; "place-" ]
  then (901, 2)
  else if List.exists (fun p -> starts p core) [ "gap-"; "space-" ] then (902, 0)
  else if core = "container" || starts "prose" core then (1000, 0)
  else (9999, 0)

let build_utilities_layer ~rules ~media_queries ~container_queries =
  let sorted_rules =
    List.stable_sort
      (fun r1 r2 ->
        let group1, sub1 = conflict_group (Css.selector r1) in
        let group2, sub2 = conflict_group (Css.selector r2) in
        let group_cmp = Int.compare group1 group2 in
        if group_cmp <> 0 then group_cmp else Int.compare sub1 sub2)
      rules
  in
  Css.layered_rules ~layer:Css.Utilities ~media_queries ~container_queries
    (sorted_rules |> List.map Css.rule_to_nested)

let add_hover_to_media_map hover_rules media_map =
  if hover_rules = [] then media_map
  else
    let hover_condition = "(hover:hover)" in
    let existing_hover_rules =
      try List.assoc hover_condition media_map with Not_found -> []
    in
    (hover_condition, hover_rules @ existing_hover_rules)
    :: List.remove_assoc hover_condition media_map

let rule_sets tw_classes =
  let all_rules = tw_classes |> List.concat_map extract_selector_props in
  let regular_rules, media_rules, container_rules, _ =
    separate_rules_by_type all_rules
  in
  let grouped_regular = group_regular_rules regular_rules in
  let non_hover_rules, hover_rules =
    List.partition (fun (sel, _) -> not (is_hover_rule sel)) grouped_regular
  in
  let rules =
    List.map
      (fun (selector, props) ->
        Css.rule ~selector (Css.deduplicate_declarations props))
      non_hover_rules
  in
  let media_queries_map =
    group_media_queries media_rules |> add_hover_to_media_map hover_rules
  in
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
  let container_queries_map = group_container_queries container_rules in
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
  (rules, media_queries, container_queries)

let compute_properties_layer rules =
  let referenced_vars = Css.vars_of_rules rules in
  let var_tally = Var.tally_of_vars referenced_vars in
  let properties = Var.generate_properties_layer var_tally in
  let layer_opt =
    if properties <> [] then
      let css_props =
        List.map (fun (var, value) -> Css.custom_property var value) properties
      in
      Some
        (Css.layered_rules ~layer:Css.Properties
           ~supports_queries:
             [
               Css.supports
                 ~condition:
                   "(((-webkit-hyphens:none)) and (not (margin-trim:inline))) \
                    or ((-moz-orient:inline) and (not (color:rgb(from red r g \
                    b))))"
                 [
                   Css.rule ~selector:"*, :before, :after, ::backdrop" css_props;
                 ];
             ]
           [])
    else None
  in
  let vars_needing_properties = Var.needs_at_property var_tally in
  let at_properties =
    List.filter_map
      (fun var ->
        match Var.at_property_config var with
        | Some (name, syntax, inherits, initial_value) ->
            Some (Css.at_property ~name ~syntax ~initial_value ~inherits ())
        | None -> None)
      vars_needing_properties
  in
  (layer_opt, at_properties)

let compute_theme_layer tw_classes =
  let directly_referenced_vars =
    tw_classes
    |> List.concat_map (fun tw ->
           let selector_props = extract_selector_props tw in
           List.concat_map
             (function
               | Regular (_, props)
               | Media_query (_, _, props)
               | Container_query (_, _, props)
               | Starting_style (_, props) ->
                   Css.vars_of_declarations props)
             selector_props)
    |> List.fold_left (fun acc v -> if List.mem v acc then acc else v :: acc) []
    |> List.rev
  in
  let default_vars =
    [ "--default-font-family"; "--default-mono-font-family" ]
  in
  let rec resolve_dependencies vars_to_check resolved =
    match vars_to_check with
    | [] -> resolved
    | var :: rest ->
        if List.mem var resolved then resolve_dependencies rest resolved
        else
          let new_deps =
            match Var.of_string var with
            | Some v -> Var.to_css_properties v |> Css.vars_of_declarations
            | None -> []
          in
          let to_check =
            rest
            @ List.filter
                (fun d -> not (List.mem d resolved || List.mem d rest))
                new_deps
          in
          resolve_dependencies to_check (var :: resolved)
  in
  let initial_vars = directly_referenced_vars @ default_vars in
  let all_referenced_vars =
    resolve_dependencies initial_vars []
    |> List.sort_uniq (fun a b ->
           match (Var.of_string a, Var.of_string b) with
           | Some va, Some vb -> Var.compare va vb
           | Some _, None -> -1
           | None, Some _ -> 1
           | None, None -> String.compare a b)
  in
  let theme_generated_vars =
    all_referenced_vars
    |> List.concat_map (fun var_name ->
           match Var.of_string var_name with
           | Some v -> Var.to_css_properties v
           | None -> [])
  in
  Css.layered_rules ~layer:Css.Theme
    [
      Css.rule_to_nested
        (Css.rule ~selector:":root, :host" theme_generated_vars);
    ]

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

(* Generate CSS rules for all used Tw classes *)
let to_css ?(reset = true) ?(mode = Css.Variables) tw_classes =
  let _ = mode in
  (* FIXME: use mode! *)
  let rules, media_queries, container_queries = rule_sets tw_classes in

  if reset then
    let properties_layer_opt, at_properties = compute_properties_layer rules in
    let theme_layer = compute_theme_layer tw_classes in
    let base_rules = Preflight.stylesheet () in
    let rec split_after_placeholder acc = function
      | [] -> (List.rev acc, [])
      | h :: t ->
          if Css.selector h = "::placeholder" then (List.rev (h :: acc), t)
          else split_after_placeholder (h :: acc) t
    in
    let before_placeholder, after_placeholder =
      split_after_placeholder [] base_rules
    in
    let base_layer_content =
      (before_placeholder |> List.map Css.rule_to_nested)
      @ [ Css.supports_to_nested placeholder_supports ]
      @ (after_placeholder |> List.map Css.rule_to_nested)
    in
    let base_layer = Css.layered_rules ~layer:Css.Base base_layer_content in
    let components_layer = Css.layered_rules ~layer:Css.Components [] in
    (* Tailwind v4 uses conflict-aware grouping for CSS rule ordering. Utilities
       are grouped by which CSS properties they can conflict with, ensuring
       proper specificity resolution. More specific utilities override shorthand
       ones within each group. *)
    let utilities_layer =
      build_utilities_layer ~rules ~media_queries ~container_queries
    in
    let base_layers =
      [ theme_layer; base_layer; components_layer; utilities_layer ]
    in
    let layers =
      match properties_layer_opt with
      | Some props_layer -> props_layer :: base_layers
      | None -> base_layers
    in
    let items =
      List.map (fun l -> Css.Layer l) layers
      @ List.map (fun a -> Css.At_property a) at_properties
    in
    Css.stylesheet items
  else
    (* No reset - just raw rules, media queries, and container queries, no
       layers. TODO: is this the right choice for inline? *)
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
