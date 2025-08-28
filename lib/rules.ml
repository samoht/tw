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

type output =
  | Regular of {
      selector : string;
      props : Css.declaration list;
      base_class : string option; (* Base class name without the dot *)
      has_hover : bool; (* Track if this rule has hover modifier *)
    }
  | Media_query of {
      condition : string;
      selector : string;
      props : Css.declaration list;
      base_class : string option;
    }
  | Container_query of {
      condition : string;
      selector : string;
      props : Css.declaration list;
      base_class : string option;
    }
  | Starting_style of {
      selector : string;
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

let escape_class_name name =
  (* Escape special CSS selector characters for Tailwind class names. This
     covers the common characters used in Tailwind utilities like arbitrary
     values (p-[10px]), responsive prefixes (sm:p-4), fractions (w-1/2), and
     other special cases. Note: This is not a complete CSS.escape implementation
     but handles all characters typically found in Tailwind class names. *)
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
      regular
        ~selector:(selector ^ "[data-state=\"" ^ value ^ "\"]")
        ~props ~base_class ()
  | Data_variant value ->
      regular
        ~selector:(selector ^ "[data-variant=\"" ^ value ^ "\"]")
        ~props ~base_class ()
  | Data_custom (key, value) ->
      regular
        ~selector:(selector ^ "[data-" ^ key ^ "=\"" ^ value ^ "\"]")
        ~props ~base_class ()
  | Dark ->
      media_query ~condition:"(prefers-color-scheme: dark)" ~selector ~props
        ~base_class ()
  | Responsive breakpoint ->
      let prefix = string_of_breakpoint breakpoint in
      let condition = "(min-width:" ^ responsive_breakpoint prefix ^ ")" in
      let escaped_prefix = escape_class_name prefix in
      let sel = "." ^ escaped_prefix ^ "\\:" ^ escape_class_name base_class in
      media_query ~condition ~selector:sel ~props ~base_class ()
  | Container query ->
      let prefix = Containers.container_query_to_class_prefix query in
      let escaped_prefix = escape_class_name prefix in
      let escaped_class =
        "." ^ escaped_prefix ^ "\\:" ^ escape_class_name base_class
      in
      let condition = Containers.container_query_to_css_prefix query in
      (* Extract the condition part from "@container (condition)" format *)
      let cond =
        if String.starts_with ~prefix:"@container " condition then
          drop_prefix "@container " condition
        else "(min-width: 0)"
      in
      container_query ~condition:cond ~selector:escaped_class ~props ~base_class
        ()
  | Not _modifier ->
      (* Note: This negates the entire selector, not just the base class. This
         is intentional to allow complex :not() conditions. *)
      regular
        ~selector:
          (".not-" ^ escape_class_name base_class ^ ":not(" ^ selector ^ ")")
        ~props ~base_class ()
  | Has selector_str ->
      (* Escape the selector string that appears in the class name *)
      let escaped_selector = escape_class_name selector_str in
      regular
        ~selector:
          (".has-\\[" ^ escaped_selector ^ "\\]\\:"
          ^ escape_class_name base_class
          ^ ":has(" ^ selector_str ^ ")")
        ~props ~base_class ()
  | Group_has selector_str ->
      (* Escape the selector string that appears in the class name *)
      let escaped_selector = escape_class_name selector_str in
      regular
        ~selector:
          (".group:has(" ^ selector_str ^ ") .group-has-\\[" ^ escaped_selector
         ^ "\\]\\:"
          ^ escape_class_name base_class)
        ~props ~base_class ()
  | Peer_has selector_str ->
      (* Escape the selector string that appears in the class name *)
      let escaped_selector = escape_class_name selector_str in
      regular
        ~selector:
          (".peer:has(" ^ selector_str ^ ") ~ .peer-has-\\[" ^ escaped_selector
         ^ "\\]\\:"
          ^ escape_class_name base_class)
        ~props ~base_class ()
  | Starting ->
      starting_style
        ~selector:("." ^ escape_class_name base_class)
        ~props ~base_class ()
  | Motion_safe ->
      media_query ~condition:"(prefers-reduced-motion: no-preference)"
        ~selector:(".motion-safe\\:" ^ escape_class_name base_class)
        ~props ~base_class ()
  | Motion_reduce ->
      media_query ~condition:"(prefers-reduced-motion: reduce)"
        ~selector:(".motion-reduce\\:" ^ escape_class_name base_class)
        ~props ~base_class ()
  | Contrast_more ->
      media_query ~condition:"(prefers-contrast: more)"
        ~selector:(".contrast-more\\:" ^ escape_class_name base_class)
        ~props ~base_class ()
  | Contrast_less ->
      media_query ~condition:"(prefers-contrast: less)"
        ~selector:(".contrast-less\\:" ^ escape_class_name base_class)
        ~props ~base_class ()
  | Hover | Focus | Active | Focus_within | Focus_visible | Disabled ->
      (* These are pseudo-class modifiers - track hover specifically *)
      let sel = Modifiers.to_selector modifier base_class in
      let has_hover = modifier = Hover in
      regular ~selector:sel ~props ~base_class ~has_hover ()
  | _ ->
      (* For other modifiers, use the selector helper from Modifiers *)
      let sel = Modifiers.to_selector modifier base_class in
      regular ~selector:sel ~props ~base_class ()

(* Extract selector and properties from a single Tw style *)
let extract_selector_props tw =
  let rec extract = function
    | Style { name; props; rules; _ } -> (
        let escaped_name = escape_class_name name in
        match rules with
        | None ->
            [
              regular ~selector:("." ^ escaped_name) ~props ~base_class:name ();
            ]
        | Some rule_list ->
            (* Convert custom rules to selector/props pairs *)
            rule_list
            |> List.map (fun rule ->
                   regular ~selector:(Css.selector rule)
                     ~props:(Css.declarations rule) ~base_class:name ()))
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

(* Group properties by selector for Regular rules *)
let group_by_selector rules =
  let tbl = Hashtbl.create 32 in
  List.iter
    (fun rule ->
      match rule with
      | Regular { selector; props; _ } ->
          let existing = try Hashtbl.find tbl selector with Not_found -> [] in
          Hashtbl.replace tbl selector (existing @ props)
      | _ -> ())
    rules;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []

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
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []

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
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []

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
      suborder = (fun _ -> 0);
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
  (* Sort rules by conflict group priority for proper cascade behavior.

     We use List.stable_sort to maintain relative order of rules with the same
     priority. This ensures deterministic output: - Rules are first grouped by
     utility type (display, margin, padding, etc.) - Within each group, rules
     are sorted by suborder (e.g., m- before mx- before mt-) - Rules with
     identical group and suborder maintain their original order

     This stable sorting is crucial for predictable CSS output when multiple
     Tailwind classes could conflict. The last class in the original list wins
     among equal-priority utilities. *)
  let sorted_rules =
    List.stable_sort
      (fun r1 r2 ->
        let group1, sub1 = conflict_group (Css.selector r1) in
        let group2, sub2 = conflict_group (Css.selector r2) in
        let group_cmp = Int.compare group1 group2 in
        if group_cmp <> 0 then group_cmp else Int.compare sub1 sub2)
      rules
  in
  Css.layer ~name:"utilities" ~media:media_queries ~container:container_queries
    (sorted_rules |> List.map Css.rule_to_nested)

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

(* Helper to convert grouped selector/props pairs to CSS rules with
   deduplication *)
let rules_of_grouped ?(filter_custom_props = false) grouped_list =
  List.map
    (fun (selector, props) ->
      let filtered_props =
        if filter_custom_props then
          (* Filter out ALL custom property declarations from utility layer They
             belong in theme or properties layer, not utilities *)
          List.filter
            (fun decl ->
              match Css.custom_declaration_name decl with
              | Some _ -> false (* Filter out custom declarations *)
              | None -> true)
            props
        else props
      in
      Css.rule ~selector (Css.deduplicate_declarations filtered_props))
    grouped_list

let rule_sets tw_classes =
  let all_rules = tw_classes |> List.concat_map extract_selector_props in
  let separated = classify all_rules in
  (* First separate hover from non-hover rules *)
  let hover_regular, non_hover_regular =
    List.partition is_hover_rule separated.regular
  in
  let grouped_regular = group_by_selector non_hover_regular in
  let grouped_hover = group_by_selector hover_regular in
  let non_hover_rules = grouped_regular in
  let hover_rules = grouped_hover in
  let rules = rules_of_grouped ~filter_custom_props:true non_hover_rules in
  let media_queries_map =
    group_media_queries separated.media |> add_hover_to_media_map hover_rules
  in
  let media_queries =
    List.map
      (fun (condition, rule_list) ->
        Css.media ~condition
          (rules_of_grouped ~filter_custom_props:true rule_list))
      media_queries_map
  in
  let container_queries_map = group_container_queries separated.container in
  let container_queries =
    List.map
      (fun (condition, rule_list) ->
        Css.container ~condition
          (rules_of_grouped ~filter_custom_props:true rule_list))
      container_queries_map
  in
  (rules, media_queries, container_queries)

(* ======================================================================== *)
(* Layer Generation - CSS @layer directives and variable resolution *)
(* ======================================================================== *)

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

(* Map for typed variables with theme ordering *)
module VarMap = Map.Make (struct
  type t = Var.any

  let compare = Var.compare_for Var.Theme
end)

let compute_properties_layer rules =
  let referenced_vars = Css.vars_of_rules rules in
  let var_tally = Var.tally_of_vars referenced_vars in
  (* Properties with defaults are now handled by the variables themselves
     through layers *)
  let properties = [] in
  let layer_opt =
    if properties <> [] then
      let css_props =
        List.map (fun (var, value) -> Css.custom_property var value) properties
      in
      Some
        (Css.layer ~name:"properties"
           ~supports:
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
  let vars_needing_properties = Var.needs_property_rule var_tally in
  let at_properties =
    List.filter_map
      (fun var ->
        match Var.property_rule_config var with
        | Some (name, syntax, inherits, initial_value) ->
            Some (Css.property ~name ~syntax ~initial_value ~inherits ())
        | None -> None)
      vars_needing_properties
  in
  (layer_opt, at_properties)

let compute_theme_layer ?(default_vars = []) tw_classes =
  (* Use Var module to get default font variables instead of hardcoding *)
  let default_vars =
    if default_vars = [] then
      (* Get the variable names for default font-related variables *)
      [
        Var.to_string Var.Font_sans;
        Var.to_string Var.Font_mono;
        Var.to_string Var.Default_font_family;
        Var.to_string Var.Default_mono_font_family;
      ]
    else default_vars
  in
  (* Extract all variable declarations (Custom_declaration) from the styles *)
  let all_var_declarations =
    let var_list = ref [] in
    tw_classes
    |> List.iter (fun tw ->
           let selector_props = extract_selector_props tw in
           List.iter
             (function
               | Regular { props; _ }
               | Media_query { props; _ }
               | Container_query { props; _ }
               | Starting_style { props; _ } ->
                   (* Extract custom declarations for theme layer *)
                   let custom_decls = Css.extract_custom_declarations props in
                   List.iter
                     (fun decl ->
                       (* Only extract non --tw- prefixed for theme layer *)
                       match Css.custom_declaration_name decl with
                       | Some var_name
                         when not (String.starts_with ~prefix:"--tw-" var_name)
                         ->
                           var_list := decl :: !var_list
                       | _ -> ())
                     custom_decls)
             selector_props);
    (* Deduplicate by variable name while preserving order *)
    let seen = ref StringSet.empty in
    !var_list
    |> List.filter (fun decl ->
           match Css.custom_declaration_name decl with
           | Some name ->
               if StringSet.mem name !seen then false
               else (
                 seen := StringSet.add name !seen;
                 true)
           | None -> false)
  in

  (* Also get variable names that are referenced (for fallback generation) *)
  let directly_referenced_vars =
    let var_set = ref StringSet.empty in
    tw_classes
    |> List.iter (fun tw ->
           let selector_props = extract_selector_props tw in
           List.iter
             (function
               | Regular { props; _ }
               | Media_query { props; _ }
               | Container_query { props; _ }
               | Starting_style { props; _ } ->
                   List.iter
                     (fun v -> var_set := StringSet.add v !var_set)
                     (Css.vars_of_declarations props))
             selector_props);
    StringSet.elements !var_set
  in

  (* Collect all variable declarations we need *)
  let all_var_decls =
    (* Get all unique variable names *)
    let all_var_names =
      StringSet.elements
        (StringSet.union
           (StringSet.of_list default_vars)
           (StringSet.of_list directly_referenced_vars))
    in
    (* Get declaration for each variable *)
    all_var_names
    |> List.filter_map (fun var_name ->
           (* First check if we have an extracted declaration for this var *)
           match
             List.find_opt
               (fun decl ->
                 match Css.custom_declaration_name decl with
                 | Some name -> name = var_name
                 | None -> false)
               all_var_declarations
           with
           | Some decl -> Some decl
           | None ->
               (* For default variables, get them from Var module *)
               if List.mem var_name default_vars then
                 let default_decls = Var.default_font_declarations () in
                 List.find_opt
                   (fun decl ->
                     match Css.custom_declaration_name decl with
                     | Some name -> name = var_name
                     | None -> false)
                   default_decls
               else None)
  in

  (* Sort all declarations using canonical theme order *)
  let theme_generated_vars =
    List.stable_sort
      (fun a b -> Var.compare_declarations Var.Theme a b)
      all_var_decls
  in

  if theme_generated_vars = [] then Css.layer ~name:"theme" []
  else
    Css.layer ~name:"theme"
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
  Css.layer ~name:"base" base_layer_content

(* Collect property rules from Core.t structures *)
let rec collect_property_rules = function
  | Core.Style { property_rules; _ } -> property_rules
  | Core.Modified (_, t) -> collect_property_rules t
  | Core.Group ts -> List.concat_map collect_property_rules ts

let build_reset_layers tw_classes rules media_queries container_queries =
  (* Collect property rules directly from utilities *)
  let property_rules_from_utilities =
    tw_classes
    |> List.concat_map collect_property_rules
    |> List.sort_uniq Stdlib.compare (* Deduplicate *)
  in

  (* Still compute properties layer for composition variables if needed *)
  let properties_layer_opt, inferred_properties =
    compute_properties_layer rules
  in

  (* Combine both sources of property rules *)
  let all_property_rules =
    property_rules_from_utilities @ inferred_properties
    |> List.sort_uniq Stdlib.compare (* Deduplicate again *)
  in

  let theme_layer = compute_theme_layer tw_classes in
  let base_layer = build_base_layer (Preflight.stylesheet ()) in
  let components_layer = Css.layer ~name:"components" [] in
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
  (layers, all_property_rules)

let wrap_css_items ~rules ~media_queries ~container_queries =
  List.map (fun r -> Css.Rule r) rules
  @ List.map (fun m -> Css.Media m) media_queries
  @ List.map (fun c -> Css.Container c) container_queries

(* ======================================================================== *)
(* Main API - Convert Tw styles to CSS *)
(* ======================================================================== *)

type config = { reset : bool; mode : Css.mode }
(** Configuration for CSS generation *)

let default_config = { reset = true; mode = Css.Variables }

let to_css ?(config = default_config) tw_classes =
  let rules, media_queries, container_queries = rule_sets tw_classes in

  (* Generate layers only when: 1. Reset is enabled AND 2. Mode is Variables

     This ensures a consistent mental model: - Variables mode with reset: Full
     Tailwind experience with layers - Any other combination: Raw CSS rules
     without layers *)
  if config.reset && config.mode = Css.Variables then
    (* Full layer structure: Theme, Properties, Base, Components, Utilities *)
    let layers, at_properties =
      build_reset_layers tw_classes rules media_queries container_queries
    in
    let items =
      List.map (fun l -> Css.Layer l) layers
      @ List.map (fun a -> Css.Property a) at_properties
    in
    Css.stylesheet items
  else
    (* No layers - just raw utility rules This applies when: - reset = false (no
       preflight/base styles needed) - mode != Variables (inline styles or
       resolved values) *)
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
