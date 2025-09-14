(** CSS rule generation and management

    This module converts Tailwind utility classes into optimized CSS rules. The
    complexity comes from several requirements:

    - Rule Extraction: Transform modifier structures into CSS rules
    - Conflict Resolution: Order utilities by specificity
    - CSS Layers: Generate proper [@layer] directives
    - Variable Resolution: Track CSS custom property dependencies
    - Media/Container Queries: Handle responsive modifiers *)

open Core

(* ======================================================================== *)
(* Types *)
(* ======================================================================== *)

type output =
  | Regular of {
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option; (* Base class name without the dot *)
      has_hover : bool; (* Track if this rule has hover modifier *)
    }
  | Media_query of {
      condition : string;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
    }
  | Container_query of {
      condition : string;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
    }
  | Starting_style of {
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
    }

type by_type = {
  regular : output list;
  media : output list;
  container : output list;
  starting : output list;
}

(* ======================================================================== *)
(* Smart constructors for output *)
(* ======================================================================== *)

let regular ~selector ~props ?base_class ?(has_hover = false) () =
  Regular { selector; props; base_class; has_hover }

let media_query ~condition ~selector ~props ?base_class () =
  Media_query { condition; selector; props; base_class }

let container_query ~condition ~selector ~props ?base_class () =
  Container_query { condition; selector; props; base_class }

let starting_style ~selector ~props ?base_class () =
  Starting_style { selector; props; base_class }

(* ======================================================================== *)
(* Basic Utilities *)
(* ======================================================================== *)

(* String manipulation helpers *)
let drop_prefix prefix s =
  let lp = String.length prefix in
  let ls = String.length s in
  if ls >= lp && String.sub s 0 lp = prefix then String.sub s lp (ls - lp)
  else s

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

(* Small memoization for escaping, as many utilities reuse the same base class
   names across modifiers. *)
let escape_cache : (string, string) Hashtbl.t = Hashtbl.create 256

let escape_class_name name =
  try Hashtbl.find escape_cache name
  with Not_found ->
    (* Escape special CSS selector characters for Tailwind class names. This
       covers the common characters used in Tailwind utilities like arbitrary
       values (p-[10px]), responsive prefixes (sm:p-4), fractions (w-1/2), and
       other special cases. Note: This is not a complete CSS.escape
       implementation but handles all characters typically found in Tailwind
       class names. *)
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
    let escaped = Buffer.contents buf in
    Hashtbl.add escape_cache name escaped;
    escaped

(* ======================================================================== *)
(* Rule Extraction - Convert Core.t to CSS rules *)
(* ======================================================================== *)

let selector_with_data_key selector key value =
  let attr_selector = Css.Selector.attribute key (Exact value) in
  Css.Selector.combine selector Descendant attr_selector

let media_modifier ~condition ~prefix base_class props =
  let selector_str = prefix ^ escape_class_name base_class in
  (* For now, create a simple class selector - this needs proper parsing *)
  let selector =
    Css.Selector.class_
      (String.sub selector_str 1 (String.length selector_str - 1))
  in
  media_query ~condition ~selector ~props ~base_class ()

let responsive_rule breakpoint base_class props =
  let prefix = string_of_breakpoint breakpoint in
  let condition = "(min-width:" ^ responsive_breakpoint prefix ^ ")" in
  let escaped_prefix = escape_class_name prefix in
  let sel = escaped_prefix ^ "\\:" ^ escape_class_name base_class in
  let selector = Css.Selector.class_ sel in
  media_query ~condition ~selector ~props ~base_class ()

let container_rule query base_class props =
  let prefix = Containers.container_query_to_class_prefix query in
  let escaped_prefix = escape_class_name prefix in
  let escaped_class = escaped_prefix ^ "\\:" ^ escape_class_name base_class in
  let selector = Css.Selector.class_ escaped_class in
  let condition = Containers.container_query_to_css_prefix query in
  let cond =
    if String.starts_with ~prefix:"@container " condition then
      drop_prefix "@container " condition
    else "(min-width: 0)"
  in
  container_query ~condition:cond ~selector ~props ~base_class ()

let has_like_selector kind selector_str base_class props =
  let open Css.Selector in
  let escaped_selector = escape_class_name selector_str in
  match kind with
  | `Has ->
      let sel =
        compound
          [
            class_
              ("has-\\[" ^ escaped_selector ^ "\\]\\:"
              ^ escape_class_name base_class);
            pseudo_class ("has(" ^ selector_str ^ ")");
          ]
      in
      regular ~selector:sel ~props ~base_class ()
  | `Group_has ->
      let left =
        compound [ class_ "group"; pseudo_class ("has(" ^ selector_str ^ ")") ]
      in
      let right =
        class_
          ("group-has-\\[" ^ escaped_selector ^ "\\]\\:"
          ^ escape_class_name base_class)
      in
      let sel = combine left Descendant right in
      regular ~selector:sel ~props ~base_class ()
  | `Peer_has ->
      let left =
        compound [ class_ "peer"; pseudo_class ("has(" ^ selector_str ^ ")") ]
      in
      let right =
        class_
          ("peer-has-\\[" ^ escaped_selector ^ "\\]\\:"
          ^ escape_class_name base_class)
      in
      let sel = combine left Subsequent_sibling right in
      regular ~selector:sel ~props ~base_class ()

let modifier_to_rule modifier base_class selector props =
  match modifier with
  | Data_state value ->
      regular
        ~selector:(selector_with_data_key selector "data-state" value)
        ~props ~base_class ()
  | Data_variant value ->
      regular
        ~selector:(selector_with_data_key selector "data-variant" value)
        ~props ~base_class ()
  | Data_custom (key, value) ->
      regular
        ~selector:(selector_with_data_key selector ("data-" ^ key) value)
        ~props ~base_class ()
  | Dark ->
      media_query ~condition:"(prefers-color-scheme: dark)" ~selector ~props
        ~base_class ()
  | Responsive breakpoint -> responsive_rule breakpoint base_class props
  | Container query -> container_rule query base_class props
  | Not _modifier ->
      regular
        ~selector:
          (Css.Selector.class_
             ("not-"
             ^ escape_class_name base_class
             ^ ":not("
             ^ Css.Selector.to_string selector
             ^ ")"))
        ~props ~base_class ()
  | Has selector_str -> has_like_selector `Has selector_str base_class props
  | Group_has selector_str ->
      has_like_selector `Group_has selector_str base_class props
  | Peer_has selector_str ->
      has_like_selector `Peer_has selector_str base_class props
  | Starting ->
      starting_style
        ~selector:(Css.Selector.class_ (escape_class_name base_class))
        ~props ~base_class ()
  | Motion_safe ->
      media_modifier ~condition:"(prefers-reduced-motion: no-preference)"
        ~prefix:".motion-safe\\:" base_class props
  | Motion_reduce ->
      media_modifier ~condition:"(prefers-reduced-motion: reduce)"
        ~prefix:".motion-reduce\\:" base_class props
  | Contrast_more ->
      media_modifier ~condition:"(prefers-contrast: more)"
        ~prefix:".contrast-more\\:" base_class props
  | Contrast_less ->
      media_modifier ~condition:"(prefers-contrast: less)"
        ~prefix:".contrast-less\\:" base_class props
  | Hover | Focus | Active | Focus_within | Focus_visible | Disabled ->
      let sel = Modifiers.to_selector modifier base_class in
      let has_hover = Modifiers.is_hover modifier in
      regular ~selector:sel ~props ~base_class ~has_hover ()
  | _ ->
      let sel = Modifiers.to_selector modifier base_class in
      let has_hover = Modifiers.is_hover modifier in
      regular ~selector:sel ~props ~base_class ~has_hover ()

(* Extract selector and properties from a single Tw style *)
let extract_selector_props tw =
  let rec extract = function
    | Style { name; props; rules; _ } -> (
        let escaped_name = escape_class_name name in
        let sel = Css.Selector.class_ escaped_name in
        match rules with
        | None -> [ regular ~selector:sel ~props ~base_class:name () ]
        | Some rule_list ->
            (* Convert custom rules to selector/props pairs *)
            let custom_rules =
              rule_list
              |> List.map (fun rule ->
                     regular ~selector:(Css.selector rule)
                       ~props:(Css.declarations rule) ~base_class:name ())
            in

            (* If there are base props, add them after the custom rules to match
               Tailwind's order *)
            if props = [] then custom_rules
            else
              custom_rules
              @ [ regular ~selector:sel ~props ~base_class:name () ])
    | Modified (modifier, t) ->
        let base = extract t in
        List.concat_map
          (fun rule_out ->
            match rule_out with
            | Regular { selector; props; base_class; _ } ->
                (* Use the base_class from the rule, not extract from
                   selector *)
                let bc = Option.value base_class ~default:"" in
                [ modifier_to_rule modifier bc selector props ]
            | _ -> [ rule_out ])
          base
    | Group styles -> List.concat_map extract styles
  in
  extract tw

(* Extract selector and props pairs from Regular rules. *)
let extract_selector_props_pairs rules =
  List.filter_map
    (fun rule ->
      match rule with
      | Regular { selector; props; _ } -> Some (selector, props)
      | _ -> None)
    rules

(* ======================================================================== *)
(* Rule Processing - Group and organize rules *)
(* ======================================================================== *)

let classify all_rules =
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

let is_hover_rule = function
  | Regular { has_hover; _ } -> has_hover
  | _ -> false

let group_media_queries media_rules =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun rule ->
      match rule with
      | Media_query { condition; selector; props; _ } ->
          let rules = try Hashtbl.find tbl condition with Not_found -> [] in
          Hashtbl.replace tbl condition ((selector, props) :: rules)
      | _ -> ())
    media_rules;
  (* Reverse once to restore original insertion order per condition *)
  Hashtbl.fold (fun k v acc -> (k, List.rev v) :: acc) tbl []

let group_container_queries container_rules =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun rule ->
      match rule with
      | Container_query { condition; selector; props; _ } ->
          let rules = try Hashtbl.find tbl condition with Not_found -> [] in
          Hashtbl.replace tbl condition ((selector, props) :: rules)
      | _ -> ())
    container_rules;
  (* Reverse once to restore original insertion order per condition *)
  Hashtbl.fold (fun k v acc -> (k, List.rev v) :: acc) tbl []

(* ======================================================================== *)
(* Conflict Resolution - Order utilities by specificity *)
(* ======================================================================== *)

(* Centralized color ordering - matches Tailwind's color palette order *)
(* Colors are ordered alphabetically for consistency and predictability *)
let color_order_map =
  [
    ("amber", 0);
    ("blue", 1);
    ("cyan", 2);
    ("emerald", 3);
    ("fuchsia", 4);
    ("gray", 5);
    ("green", 6);
    ("indigo", 7);
    ("lime", 8);
    ("neutral", 9);
    ("orange", 10);
    ("pink", 11);
    ("purple", 12);
    ("red", 13);
    ("rose", 14);
    ("sky", 15);
    ("slate", 16);
    ("stone", 17);
    ("teal", 18);
    ("violet", 19);
    ("yellow", 20);
    ("zinc", 21);
  ]

let color_order color_name =
  match List.assoc_opt color_name color_order_map with
  | Some order -> order
  | None -> 100 (* Unknown colors go last *)

(* Utility prefix constants for classification *)
let display_prefixes =
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

let position_prefixes = [ "static"; "fixed"; "absolute"; "relative"; "sticky" ]

let margin_prefixes =
  [
    "m-";
    "-m-";
    "mx-";
    "-mx-";
    "my-";
    "-my-";
    "mt-";
    "-mt-";
    "mr-";
    "-mr-";
    "mb-";
    "-mb-";
    "ml-";
    "-ml-";
  ]

let padding_prefixes = [ "p-"; "px-"; "py-"; "pt-"; "pr-"; "pb-"; "pl-" ]

let typography_prefixes =
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

let sizing_prefixes = [ "w-"; "h-"; "min-w-"; "min-h-"; "max-w-"; "max-h-" ]

let effects_prefixes =
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

let interactivity_prefixes =
  [ "cursor-"; "select-"; "resize-"; "scroll-"; "overflow-"; "overscroll-" ]

let flexbox_grid_prefixes =
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

let gap_prefixes = [ "gap-"; "space-" ]

(* Utility classification functions *)
let has_any_prefix prefixes core =
  List.exists (fun p -> String.starts_with ~prefix:p core) prefixes

let is_display_util = has_any_prefix display_prefixes
let is_position_util = has_any_prefix position_prefixes
let is_margin_util = has_any_prefix margin_prefixes
let is_padding_util = has_any_prefix padding_prefixes
let is_typography_util = has_any_prefix typography_prefixes
let is_sizing_util = has_any_prefix sizing_prefixes
let is_effects_util = has_any_prefix effects_prefixes
let is_interactivity_util = has_any_prefix interactivity_prefixes
let is_flexbox_grid_util = has_any_prefix flexbox_grid_prefixes
let is_gap_util = has_any_prefix gap_prefixes

let is_border_util core =
  String.starts_with ~prefix:"rounded" core
  || String.starts_with ~prefix:"border" core
  || String.starts_with ~prefix:"outline-" core

let is_container_or_prose core =
  core = "container" || String.starts_with ~prefix:"prose" core

(* Suborder functions for fine-grained sorting within groups *)
let margin_suborder core =
  if
    String.starts_with ~prefix:"m-" core
    || String.starts_with ~prefix:"-m-" core
  then 0 (* All margins *)
  else if
    String.starts_with ~prefix:"mx-" core
    || String.starts_with ~prefix:"-mx-" core
    || String.starts_with ~prefix:"my-" core
    || String.starts_with ~prefix:"-my-" core
  then 1 (* Axis margins *)
  else 2 (* Individual margins *)

let padding_suborder core =
  if String.starts_with ~prefix:"p-" core then 0 (* All padding *)
  else if
    String.starts_with ~prefix:"px-" core
    || String.starts_with ~prefix:"py-" core
  then 1 (* Axis padding *)
  else 2 (* Individual padding *)

let alignment_suborder core =
  if String.starts_with ~prefix:"items-" core then 0
  else if String.starts_with ~prefix:"justify-" core then 1
  else if has_any_prefix [ "content-"; "self-"; "place-" ] core then 2
  else -1

(* Conflict group classification table Groups are ordered by priority (lower
   number = higher priority) This ordering ensures proper cascade behavior in
   CSS *)
type utility_group = {
  priority : int;
  name : string;
  classifier : string -> bool;
  suborder : string -> int;
}

let utility_groups =
  [
    {
      priority = 10;
      name = "display";
      classifier = (fun c -> c = "hidden" || is_display_util c);
      suborder = (fun c -> if c = "hidden" then 3 else 1);
    };
    {
      priority = 11;
      name = "position";
      classifier = is_position_util;
      suborder = (fun _ -> 0);
    };
    {
      priority = 100;
      name = "margin";
      classifier = is_margin_util;
      suborder = margin_suborder;
    };
    {
      priority = 200;
      name = "background";
      classifier =
        (fun c ->
          String.starts_with ~prefix:"bg-" c
          || String.starts_with ~prefix:"from-" c
          || String.starts_with ~prefix:"via-" c
          || String.starts_with ~prefix:"to-" c);
      suborder =
        (fun c ->
          if String.starts_with ~prefix:"bg-" c then
            let color_part = drop_prefix "bg-" c in
            let color_name =
              try
                let last_dash = String.rindex color_part '-' in
                String.sub color_part 0 last_dash
              with Not_found -> color_part
            in
            color_order color_name
          else 50 (* Gradient utilities come after solid colors *));
    };
    {
      priority = 300;
      name = "padding";
      classifier = is_padding_util;
      suborder = padding_suborder;
    };
    {
      priority = 400;
      name = "typography";
      classifier = is_typography_util;
      suborder = (fun _ -> 0);
    };
    {
      priority = 500;
      name = "border";
      classifier = is_border_util;
      suborder = (fun _ -> 0);
    };
    {
      priority = 600;
      name = "sizing";
      classifier = is_sizing_util;
      suborder = (fun _ -> 0);
    };
    {
      priority = 700;
      name = "effects";
      classifier = is_effects_util;
      suborder = (fun _ -> 0);
    };
    {
      priority = 800;
      name = "interactivity";
      classifier = is_interactivity_util;
      suborder = (fun _ -> 0);
    };
    {
      priority = 900;
      name = "flexbox_grid";
      classifier = is_flexbox_grid_util;
      suborder = (fun _ -> 0);
    };
    {
      priority = 901;
      name = "alignment";
      classifier = (fun c -> alignment_suborder c >= 0);
      suborder = alignment_suborder;
    };
    {
      priority = 902;
      name = "gap";
      classifier = is_gap_util;
      suborder = (fun _ -> 0);
    };
    {
      priority = 1000;
      name = "container_prose";
      classifier = is_container_or_prose;
      suborder =
        (fun core ->
          (* Ensure .prose comes before .prose :where(...) *)
          if core = "prose" then 0
          else if String.starts_with ~prefix:"prose " core then 1
          else 2);
    };
  ]

(* Main conflict resolution function *)
let conflict_group selector =
  let core =
    if String.starts_with ~prefix:"." selector then
      String.sub selector 1 (String.length selector - 1)
    else selector
  in

  (* Find the first matching group *)
  match List.find_opt (fun g -> g.classifier core) utility_groups with
  | Some group -> (group.priority, group.suborder core)
  | None -> (9999, 0)
(* Unknown utilities go last *)

let build_utilities_layer ~rules ~media_queries ~container_queries =
  (* IMPORTANT: Do NOT sort rules here! Sorting changes cascade order and can
     cause non-adjacent rules with the same selector to become adjacent, which
     then get incorrectly merged by the optimizer. The original rule order from
     rule generation must be preserved to maintain CSS cascade semantics. *)
  let block =
    List.map (fun r -> Css.Rule r) rules
    @ List.concat_map
        (fun (condition, rules) -> Css.media ~condition rules)
        media_queries
    @ List.concat_map
        (fun (name, condition, rules) -> Css.container ?name ~condition rules)
        container_queries
  in
  Css.layer ~name:"utilities" block

let add_hover_to_media_map hover_rules media_map =
  (* Gate hover rules behind (hover:hover) media query to prevent them from
     applying on touch devices where :hover can stick after tapping. This
     follows modern CSS best practices for hover states. *)
  if hover_rules = [] then media_map
  else
    let hover_condition = "(hover:hover)" in
    let existing_hover_rules =
      try List.assoc hover_condition media_map with Not_found -> []
    in
    (hover_condition, hover_rules @ existing_hover_rules)
    :: List.remove_assoc hover_condition media_map

(* Convert selector/props pairs to CSS rules. *)
let rules_of_grouped ?(filter_custom_props = false) grouped_list =
  List.map
    (fun (selector, props) ->
      let filtered_props =
        if filter_custom_props then
          (* In utilities, keep only declarations explicitly tagged for the
             utilities layer via Var metadata. Non-custom declarations are
             always kept. Custom declarations without metadata are dropped. *)
          List.filter
            (fun decl ->
              match Css.custom_declaration_layer decl with
              | Some layer when layer = "utilities" -> true
              | Some _ -> false
              | None -> (
                  (* No fallback to name prefixes: keep only non-custom
                     declarations when metadata is missing. *)
                  match Css.custom_declaration_name decl with
                  | None -> true
                  | Some _ -> false))
            props
        else props
      in
      Css.rule ~selector filtered_props)
    grouped_list

let rule_sets tw_classes =
  let all_rules = tw_classes |> List.concat_map extract_selector_props in
  let separated = classify all_rules in
  (* First separate hover from non-hover rules *)
  let hover_regular, non_hover_regular =
    List.partition is_hover_rule separated.regular
  in
  let non_hover_pairs = extract_selector_props_pairs non_hover_regular in
  let hover_pairs = extract_selector_props_pairs hover_regular in
  let rules = rules_of_grouped ~filter_custom_props:true non_hover_pairs in
  let media_queries_map =
    group_media_queries separated.media |> add_hover_to_media_map hover_pairs
  in
  let media_queries =
    List.map
      (fun (condition, rule_list) ->
        (condition, rules_of_grouped ~filter_custom_props:true rule_list))
      media_queries_map
  in
  let container_queries_map = group_container_queries separated.container in
  let container_queries =
    List.map
      (fun (condition, rule_list) ->
        (None, condition, rules_of_grouped ~filter_custom_props:true rule_list))
      container_queries_map
  in
  (rules, media_queries, container_queries)

(* ======================================================================== *)
(* Layer Generation - CSS @layer directives and theme variable resolution *)
(* ======================================================================== *)

module Strings = Set.Make (String)

(* Helpers for theme layer extraction and ordering *)
let collect_selector_props tw_classes =
  List.concat_map extract_selector_props tw_classes

let extract_non_tw_custom_declarations selector_props =
  let var_list = ref [] in
  selector_props
  |> List.iter (function
         | Regular { props; _ }
         | Media_query { props; _ }
         | Container_query { props; _ }
         | Starting_style { props; _ }
         ->
         Css.extract_custom_declarations props
         |> List.iter (fun decl ->
                match Css.custom_declaration_layer decl with
                | Some layer when layer = "theme" ->
                    var_list := decl :: !var_list
                | _ -> ()));
  (* Deduplicate by variable name while preserving order *)
  let seen = ref Strings.empty in
  !var_list
  |> List.filter (fun decl ->
         match Css.custom_declaration_name decl with
         | Some name ->
             if Strings.mem name !seen then false
             else (
               seen := Strings.add name !seen;
               true)
         | None -> false)

(* Get Var.any from declaration metadata *)
let var_of_declaration_meta decl =
  match Css.meta_of_declaration decl with
  | Some meta -> Var.var_of_meta meta
  | None -> None

let assemble_theme_decls_metadata ~extracted ~default_vars =
  (* Build Var -> declaration mapping from defaults *)
  let default_var_map =
    let all_default_decls =
      Typography.default_font_declarations
      @ Typography.default_font_family_declarations
    in
    List.fold_left
      (fun acc decl ->
        match var_of_declaration_meta decl with
        | Some var_any -> Var.Map.add var_any decl acc
        | None -> acc)
      Var.Map.empty all_default_decls
  in

  (* Build set of vars already present in extracted *)
  let extracted_var_set =
    List.fold_left
      (fun acc decl ->
        match var_of_declaration_meta decl with
        | Some var_any -> Var.Set.add var_any acc
        | None -> acc)
      Var.Set.empty extracted
  in

  (* Get needed defaults using the map *)
  let needed_defaults =
    List.filter_map
      (fun default_var ->
        if Var.Set.mem default_var extracted_var_set then None
          (* already present *)
        else Var.Map.find_opt default_var default_var_map)
      default_vars
  in

  let all_decls = extracted @ needed_defaults in
  List.stable_sort
    (fun d1 d2 ->
      (* Try to use Var metadata comparison if both have it *)
      match (var_of_declaration_meta d1, var_of_declaration_meta d2) with
      | Some v1, Some v2 -> Var.compare v1 v2
      | _ ->
          (* Fall back to name comparison *)
          let name1 =
            match Css.custom_declaration_name d1 with Some n -> n | None -> ""
          in
          let name2 =
            match Css.custom_declaration_name d2 with Some n -> n | None -> ""
          in
          String.compare name1 name2)
    all_decls

let compute_theme_layer tw_classes =
  let default_vars =
    [
      Var.Any Var.Font_sans;
      Var.Any Var.Font_mono;
      Var.Any Var.Default_font_family;
      Var.Any Var.Default_mono_font_family;
    ]
  in

  let selector_props = collect_selector_props tw_classes in
  let extracted = extract_non_tw_custom_declarations selector_props in
  let all_var_decls = assemble_theme_decls_metadata ~extracted ~default_vars in

  let theme_generated_vars =
    List.stable_sort
      (fun a b -> Var.compare_declarations Var.Theme a b)
      all_var_decls
  in

  if theme_generated_vars = [] then Css.layer ~name:"theme" []
  else
    let selector =
      Css.Selector.(list [ pseudo_class "root"; pseudo_class "host" ])
    in
    Css.layer ~name:"theme"
      [ Css.Rule (Css.rule ~selector theme_generated_vars) ]

let placeholder_supports =
  let placeholder = Css.Selector.pseudo_element "placeholder" in
  Css.supports
    ~condition:
      "(not ((-webkit-appearance:-apple-pay-button))) or \
       (contain-intrinsic-size:1px)"
    ([ Css.Rule (Css.rule ~selector:placeholder [ Css.color Current ]) ]
    @ [
        Css.supports ~condition:"(color:color-mix(in lab, red, red))"
          [
            Css.Rule
              (Css.rule ~selector:placeholder
                 [
                   Css.color
                     (Css.color_mix ~in_space:Oklab ~percent1:50 Current
                        Transparent);
                 ]);
          ];
      ])

let split_after_placeholder rules =
  let rec split acc = function
    | [] -> (List.rev acc, [])
    | h :: t ->
        if Css.Selector.to_string (Css.selector h) = "::placeholder" then
          (List.rev (h :: acc), t)
        else split (h :: acc) t
  in
  split [] rules

(* Tailwind v4's exact vendor-targeted browser support condition *)
let tailwind_v4_supports_condition =
  "(((-webkit-hyphens:none)) and (not (margin-trim:inline))) or \
   ((-moz-orient:inline) and (not (color:rgb(from red r g b))))"

let build_properties_layer property_rules =
  match property_rules with
  | [] ->
      (* No property rules - omit the properties layer entirely *)
      None
  | _ ->
      (* Convert property_rules to declarations using CSS accessor *)
      let defaults =
        property_rules
        |> List.map (fun (r : Css.Stylesheet.property_rule) ->
               let name = r.name in
               let initial =
                 match r.initial_value with
                 | Css.Stylesheet.Universal s -> Some s
                 | Css.Stylesheet.None -> None
                 | Css.Stylesheet.V _ -> None (* Can't easily convert GADT *)
               in
               (* Special handling for Ring_offset_width: "0" becomes "0px" in
                  properties layer *)
               let initial_value =
                 match initial with
                 | Some "0" when name = "--tw-ring-offset-width" -> "0px"
                 | Some v -> v
                 | None -> "initial"
               in
               Css.custom_property name initial_value)
      in

      (* Target all elements including pseudo-elements *)
      let selector =
        Css.Selector.(
          list
            [
              universal;
              pseudo_class "before";
              pseudo_class "after";
              pseudo_element "backdrop";
            ])
      in

      (* Build the rule with defaults *)
      let defaults_rule = Css.rule ~selector defaults in

      (* Wrap in supports with Tailwind's exact condition *)
      let supports_block =
        Css.supports ~condition:tailwind_v4_supports_condition
          [ Css.Rule defaults_rule ]
      in

      (* Create properties layer with supports block nested inside *)
      Some (Css.layer ~name:"properties" [ supports_block ])

let build_base_layer base_rules =
  let before_placeholder, after_placeholder =
    split_after_placeholder base_rules
  in
  let base_layer_content =
    (before_placeholder |> List.map (fun r -> Css.Rule r))
    @ [ placeholder_supports ]
    @ (after_placeholder |> List.map (fun r -> Css.Rule r))
  in
  Css.layer ~name:"base" base_layer_content

(* Collect property rules from Core.t structures *)
let rec collect_property_rules = function
  | Core.Style { property_rules; _ } -> property_rules
  | Core.Modified (_, t) -> collect_property_rules t
  | Core.Group ts -> List.concat_map collect_property_rules ts

let build_layers ~include_base tw_classes rules media_queries container_queries
    =
  (* Collect property rules from utilities - preserve order while
     deduplicating *)
  let seen = ref [] in
  let property_rules_from_utilities =
    tw_classes
    |> List.concat_map collect_property_rules
    |> List.filter (fun r ->
           if List.mem r !seen then false
           else (
             seen := r :: !seen;
             true))
  in

  (* Build properties layer with collected rules (returns None if empty) *)
  let properties_layer_opt =
    build_properties_layer property_rules_from_utilities
  in

  (* Existing layers in exact order *)
  let theme_layer = compute_theme_layer tw_classes in
  let base_layer = build_base_layer (Preflight.stylesheet ()) in
  let components_layer = Css.layer ~name:"components" [] in
  (* Empty is ok *)
  let utilities_layer =
    build_utilities_layer ~rules ~media_queries ~container_queries
  in

  (* Build layer list, prepending properties layer only if present *)
  let base_layers =
    (if include_base then [ theme_layer; base_layer ] else [ theme_layer ])
    @ [ components_layer; utilities_layer ]
  in
  let layers =
    match properties_layer_opt with
    | None -> base_layers
    | Some properties_layer -> properties_layer :: base_layers
  in

  (* Return layers and the property_rules for @property emission after layers *)
  (layers, property_rules_from_utilities)

let wrap_css_items ~rules ~media_queries ~container_queries =
  List.map (fun r -> Css.Rule r) rules
  @ List.map
      (fun (condition, rules) ->
        Css.media condition (List.map (fun r -> Css.Rule r) rules))
      media_queries
  @ List.map
      (fun (name, condition, rules) ->
        Css.container ?name condition (List.map (fun r -> Css.Rule r) rules))
      container_queries

(* ======================================================================== *)
(* Main API - Convert Tw styles to CSS *)

(* ======================================================================== *)

type config = { base : bool; mode : Css.mode; optimize : bool }
(** Configuration for CSS generation *)

let default_config = { base = true; mode = Css.Variables; optimize = false }

let to_css ?(config = default_config) tw_classes =
  let rules, media_queries, container_queries = rule_sets tw_classes in

  (* Generate layers whenever mode = Variables. Include the base layer only when
     [reset=true]. In Inline mode, emit raw rules without layers. *)
  match config.mode with
  | Css.Variables ->
      let layers, property_rules =
        build_layers ~include_base:config.base tw_classes rules media_queries
          container_queries
      in
      let items =
        layers @ List.map (fun pr -> Css.property_stmt pr) property_rules
      in
      Css.stylesheet items
  | Css.Inline ->
      (* No layers - just raw utility rules *)
      Css.stylesheet (wrap_css_items ~rules ~media_queries ~container_queries)

let to_inline_style styles =
  let rec to_css_properties = function
    | Style { props; _ } -> props
    | Modified (_, t) -> to_css_properties t
    | Group styles -> List.concat_map to_css_properties styles
  in
  let all_props = List.concat_map to_css_properties styles in
  Css.inline_style_of_declarations all_props
