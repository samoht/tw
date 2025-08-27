(** CSS rule generation and management

    This module converts Tailwind utility classes into optimized CSS rules.
    The complexity comes from several requirements:

    1. Rule Extraction - Transform modifier structures into CSS rules
    2. Conflict Resolution - Order utilities by specificity
    3. CSS Layers - Generate proper @layer directives
    4. Variable Resolution - Track CSS custom property dependencies
    5. Media/Container Queries - Handle responsive modifiers
*)

open Core
open Css

(* ======================================================================== *)
(* Types *)
(* ======================================================================== *)

type rule_output =
  | Regular of { selector : string; props : Css.declaration list }
  | Media_query of {
      condition : string;
      selector : string;
      props : Css.declaration list;
    }
  | Container_query of {
      condition : string;
      selector : string;
      props : Css.declaration list;
    }
  | Starting_style of { selector : string; props : Css.declaration list }

type separated_rules = {
  regular : rule_output list;
  media : rule_output list;
  container : rule_output list;
  starting : rule_output list;
}

(* ======================================================================== *)
(* Smart constructors for rule_output *)
(* ======================================================================== *)

let regular ~selector ~props = Regular { selector; props }

let media_query ~condition ~selector ~props =
  Media_query { condition; selector; props }

let container_query ~condition ~selector ~props =
  Container_query { condition; selector; props }

let starting_style ~selector ~props = Starting_style { selector; props }

(* ======================================================================== *)
(* Basic Utilities *)
(* ======================================================================== *)

let string_of_breakpoint = function
  | `Sm -> "sm"
  | `Md -> "md"
  | `Lg -> "lg"
  | `Xl -> "xl"
  | `Xl_2 -> "2xl"

let responsive_breakpoint = function
  | "sm" -> "40rem" (* 640px / 16 = 40rem *)
  | "md" -> "48rem" (* 768px / 16 = 48rem *)
  | "lg" -> "64rem" (* 1024px / 16 = 64rem *)
  | "xl" -> "80rem" (* 1280px / 16 = 80rem *)
  | "2xl" -> "96rem" (* 1536px / 16 = 96rem *)
  | _ -> "0rem"

let escape_class_name name =
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

(* ======================================================================== *)
(* Rule Extraction - Convert Core.t to CSS rules *)
(* ======================================================================== *)
let modifier_to_rule modifier base_class selector props =
  match modifier with
  | Data_state value ->
      regular ~selector:(selector ^ "[data-state=\"" ^ value ^ "\"]") ~props
  | Data_variant value ->
      regular ~selector:(selector ^ "[data-variant=\"" ^ value ^ "\"]") ~props
  | Data_custom (key, value) ->
      regular
        ~selector:(selector ^ "[data-" ^ key ^ "=\"" ^ value ^ "\"]")
        ~props
  | Dark ->
      media_query ~condition:"(prefers-color-scheme: dark)" ~selector ~props
  | Responsive breakpoint ->
      let prefix = string_of_breakpoint breakpoint in
      let condition = "(min-width:" ^ responsive_breakpoint prefix ^ ")" in
      let sel = "." ^ prefix ^ "\\:" ^ base_class in
      media_query ~condition ~selector:sel ~props
  | Container query ->
      let prefix = Containers.container_query_to_class_prefix query in
      let escaped_class = ".\\" ^ prefix ^ "\\:" ^ base_class in
      let condition = Containers.container_query_to_css_prefix query in
      let cond =
        if String.starts_with ~prefix:"@container " condition then
          String.sub condition 11 (String.length condition - 11)
        else "(min-width: 0)"
      in
      container_query ~condition:cond ~selector:escaped_class ~props
  | Not _modifier ->
      regular ~selector:(".not-" ^ base_class ^ ":not(" ^ selector ^ ")") ~props
  | Has selector_str ->
      regular
        ~selector:
          (".has-\\[" ^ selector_str ^ "\\]\\:" ^ base_class ^ ":has("
         ^ selector_str ^ ")")
        ~props
  | Group_has selector_str ->
      regular
        ~selector:
          (".group:has(" ^ selector_str ^ ") .group-has-\\[" ^ selector_str
         ^ "\\]\\:" ^ base_class)
        ~props
  | Peer_has selector_str ->
      regular
        ~selector:
          (".peer:has(" ^ selector_str ^ ") ~ .peer-has-\\[" ^ selector_str
         ^ "\\]\\:" ^ base_class)
        ~props
  | Starting -> starting_style ~selector:("." ^ base_class) ~props
  | Motion_safe ->
      media_query ~condition:"(prefers-reduced-motion: no-preference)"
        ~selector:(".motion-safe\\:" ^ base_class)
        ~props
  | Motion_reduce ->
      media_query ~condition:"(prefers-reduced-motion: reduce)"
        ~selector:(".motion-reduce\\:" ^ base_class)
        ~props
  | Contrast_more ->
      media_query ~condition:"(prefers-contrast: more)"
        ~selector:(".contrast-more\\:" ^ base_class)
        ~props
  | Contrast_less ->
      media_query ~condition:"(prefers-contrast: less)"
        ~selector:(".contrast-less\\:" ^ base_class)
        ~props
  | _ ->
      (* For simple modifiers, use the selector helper from Modifiers *)
      let sel = Modifiers.to_selector modifier base_class in
      regular ~selector:sel ~props

(* Extract selector and properties from a single Tw style *)
let extract_selector_props tw =
  let rec extract = function
    | Style { name; props; rules; _ } -> (
        let escaped_name = escape_class_name name in
        match rules with
        | None -> [ regular ~selector:("." ^ escaped_name) ~props ]
        | Some rule_list ->
            (* Convert custom rules to selector/props pairs *)
            rule_list
            |> List.map (fun rule ->
                   regular ~selector:(Css.selector rule)
                     ~props:(Css.declarations rule)))
    | Modified (modifier, t) ->
        let base = extract t in
        List.concat_map
          (fun rule_out ->
            match rule_out with
            | Regular { selector; props } ->
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
let group_by_selector rules =
  List.fold_left
    (fun acc rule ->
      match rule with
      | Regular { selector; props } ->
          let existing = try List.assoc selector acc with Not_found -> [] in
          let without = List.remove_assoc selector acc in
          without @ [ (selector, existing @ props) ]
      | _ -> acc)
    [] rules

(* ======================================================================== *)
(* Rule Processing - Group and organize rules *)
(* ======================================================================== *)

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
  {
    regular = List.rev regular_rules;
    media = List.rev media_rules;
    container = List.rev container_rules;
    starting = List.rev starting_rules;
  }

let is_hover_rule selector =
  (* A hover rule ends with :hover pseudo-class *)
  let len = String.length selector in
  len >= 6 && String.sub selector (len - 6) 6 = ":hover"

let group_media_queries media_rules =
  List.fold_left
    (fun acc rule ->
      match rule with
      | Media_query { condition; selector; props } ->
          let rules = try List.assoc condition acc with Not_found -> [] in
          (condition, (selector, props) :: rules)
          :: List.remove_assoc condition acc
      | _ -> acc)
    [] media_rules

let group_container_queries container_rules =
  List.fold_left
    (fun acc rule ->
      match rule with
      | Container_query { condition; selector; props } ->
          let rules = try List.assoc condition acc with Not_found -> [] in
          (condition, (selector, props) :: rules)
          :: List.remove_assoc condition acc
      | _ -> acc)
    [] container_rules

(* ======================================================================== *)
(* Conflict Resolution - Order utilities by specificity *)
(* ======================================================================== *)

let starts prefix s =
  let lp = String.length prefix and ls = String.length s in
  ls >= lp && String.sub s 0 lp = prefix

(* Color ordering for backgrounds *)
let color_order = function
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

(* Utility classifiers *)
let is_display_util core =
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

let is_position_util core =
  List.exists
    (fun p -> starts p core)
    [ "static"; "fixed"; "absolute"; "relative"; "sticky" ]

let is_margin_util core =
  starts "m-" core || starts "-m-" core || starts "mx-" core
  || starts "my-" core || starts "-mx-" core || starts "-my-" core
  || List.exists
       (fun p -> starts p core)
       [ "mt-"; "mr-"; "mb-"; "ml-"; "-mt-"; "-mr-"; "-mb-"; "-ml-" ]

let margin_suborder core =
  if starts "m-" core || starts "-m-" core then 0
  else if
    starts "mx-" core || starts "my-" core || starts "-mx-" core
    || starts "-my-" core
  then 1
  else 2

let is_padding_util core =
  starts "p-" core || starts "px-" core || starts "py-" core
  || List.exists (fun p -> starts p core) [ "pt-"; "pr-"; "pb-"; "pl-" ]

let padding_suborder core =
  if starts "p-" core then 0
  else if starts "px-" core || starts "py-" core then 1
  else 2

let is_typography_util core =
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

let is_border_util core =
  starts "rounded" core || starts "border" core || starts "outline-" core

let is_sizing_util core =
  List.exists
    (fun p -> starts p core)
    [ "w-"; "h-"; "min-w-"; "min-h-"; "max-w-"; "max-h-" ]

let is_effects_util core =
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

let is_interactivity_util core =
  List.exists
    (fun p -> starts p core)
    [ "cursor-"; "select-"; "resize-"; "scroll-"; "overflow-"; "overscroll-" ]

let is_flexbox_grid_util core =
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

let is_alignment_util core =
  if starts "items-" core then (901, 0)
  else if starts "justify-" core then (901, 1)
  else if List.exists (fun p -> starts p core) [ "content-"; "self-"; "place-" ]
  then (901, 2)
  else (-1, -1)

let is_gap_util core = List.exists (fun p -> starts p core) [ "gap-"; "space-" ]
let is_container_prose core = core = "container" || starts "prose" core

(* Main conflict resolution function *)
let conflict_group selector =
  let core =
    if String.starts_with ~prefix:"." selector then
      String.sub selector 1 (String.length selector - 1)
    else selector
  in

  (* Special cases first *)
  if core = "hidden" then (10, 3)
  else if is_display_util core then (10, 1)
  else if is_position_util core then (11, 0)
  else if is_margin_util core then (100, margin_suborder core)
  else if starts "bg-" core then
    let color_part = String.sub core 3 (String.length core - 3) in
    let color_name =
      try
        let last_dash = String.rindex color_part '-' in
        String.sub color_part 0 last_dash
      with Not_found -> color_part
    in
    (200, color_order color_name)
  else if List.exists (fun p -> starts p core) [ "from-"; "via-"; "to-" ] then
    (200, 50)
  else if is_padding_util core then (300, padding_suborder core)
  else if is_typography_util core then (400, 0)
  else if is_border_util core then (500, 0)
  else if is_sizing_util core then (600, 0)
  else if is_effects_util core then (700, 0)
  else if is_interactivity_util core then (800, 0)
  else if is_flexbox_grid_util core then (900, 0)
  else
    let align_group, align_sub = is_alignment_util core in
    if align_group >= 0 then (align_group, align_sub)
    else if is_gap_util core then (902, 0)
    else if is_container_prose core then (1000, 0)
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
  let separated = separate_rules_by_type all_rules in
  let grouped_regular = group_by_selector separated.regular in
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
    group_media_queries separated.media |> add_hover_to_media_map hover_rules
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
  let container_queries_map = group_container_queries separated.container in
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

(* ======================================================================== *)
(* Layer Generation - CSS @layer directives and variable resolution *)
(* ======================================================================== *)

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
               | Regular { props; _ }
               | Media_query { props; _ }
               | Container_query { props; _ }
               | Starting_style { props; _ } ->
                   Css.vars_of_declarations props)
             selector_props)
    |> List.fold_left (fun acc v -> if List.mem v acc then acc else v :: acc) []
    |> List.rev
  in
  let default_vars =
    [ "--default-font-family"; "--default-mono-font-family" ]
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

let split_after_placeholder rules =
  let rec split acc = function
    | [] -> (List.rev acc, [])
    | h :: t ->
        if Css.selector h = "::placeholder" then (List.rev (h :: acc), t)
        else split (h :: acc) t
  in
  split [] rules

let build_base_layer base_rules =
  let before_placeholder, after_placeholder =
    split_after_placeholder base_rules
  in
  let base_layer_content =
    (before_placeholder |> List.map Css.rule_to_nested)
    @ [ Css.supports_to_nested placeholder_supports ]
    @ (after_placeholder |> List.map Css.rule_to_nested)
  in
  Css.layered_rules ~layer:Css.Base base_layer_content

let build_reset_layers tw_classes rules media_queries container_queries =
  let properties_layer_opt, at_properties = compute_properties_layer rules in
  let theme_layer = compute_theme_layer tw_classes in
  let base_layer = build_base_layer (Preflight.stylesheet ()) in
  let components_layer = Css.layered_rules ~layer:Css.Components [] in
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
  (layers, at_properties)

let wrap_css_items ~rules ~media_queries ~container_queries =
  List.map (fun r -> Css.Rule r) rules
  @ List.map (fun m -> Css.Media m) media_queries
  @ List.map (fun c -> Css.Container c) container_queries

(* ======================================================================== *)
(* Main API - Convert Tw styles to CSS *)
(* ======================================================================== *)

let to_css ?(reset = true) ?(mode = Css.Variables) tw_classes =
  let rules, media_queries, container_queries = rule_sets tw_classes in

  if reset && mode = Css.Variables then
    (* Full layers with reset styles *)
    let layers, at_properties =
      build_reset_layers tw_classes rules media_queries container_queries
    in
    let items =
      List.map (fun l -> Css.Layer l) layers
      @ List.map (fun a -> Css.At_property a) at_properties
    in
    Css.stylesheet items
  else
    (* No layers - just raw rules for reset:false or non-Variables mode *)
    Css.stylesheet (wrap_css_items ~rules ~media_queries ~container_queries)

let to_inline_style styles =
  let rec to_css_properties = function
    | Style { props; _ } -> props
    | Modified (_, t) -> to_css_properties t
    | Group styles -> List.concat_map to_css_properties styles
  in
  let all_props = List.concat_map to_css_properties styles in
  let deduped = Css.deduplicate_declarations all_props in
  Css.inline_style_of_declarations deduped
