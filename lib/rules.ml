(** CSS rule generation and management

    This module converts Tailwind utility classes into optimized CSS rules. The
    complexity comes from several requirements:

    - Rule Extraction: Transform modifier structures into CSS rules
    - Conflict Resolution: Order utilities by specificity
    - CSS Layers: Generate proper [@layer] directives
    - Variable Resolution: Track CSS custom property dependencies
    - Media/Container Queries: Handle responsive modifiers *)

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
  if String.starts_with ~prefix s then
    let lp = String.length prefix in
    let ls = String.length s in
    String.sub s lp (ls - lp)
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
  match Hashtbl.find_opt escape_cache name with
  | Some v -> v
  | None ->
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
          | '*' -> Buffer.add_string buf "\\*"
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
  let selector_str = prefix ^ base_class in
  (* For now, create a simple class selector - this needs proper parsing *)
  let selector =
    Css.Selector.class_
      (String.sub selector_str 1 (String.length selector_str - 1))
  in
  media_query ~condition ~selector ~props ~base_class ()

(** Replace all occurrences of [old_class] with [new_class] in a selector *)
let replace_class_in_selector ~old_class ~new_class selector =
  Css.Selector.map
    (function
      | Css.Selector.Class cls when cls = old_class ->
          Css.Selector.Class new_class
      | other -> other)
    selector

let responsive_rule breakpoint base_class selector props =
  let prefix = string_of_breakpoint breakpoint in
  let condition = "(min-width:" ^ responsive_breakpoint prefix ^ ")" in
  let modified_class = prefix ^ ":" ^ base_class in
  let new_selector =
    replace_class_in_selector ~old_class:base_class ~new_class:modified_class
      selector
  in
  media_query ~condition ~selector:new_selector ~props ~base_class ()

let container_rule query base_class selector props =
  let prefix = Containers.container_query_to_class_prefix query in
  let modified_class = prefix ^ ":" ^ base_class in
  let new_selector =
    replace_class_in_selector ~old_class:base_class ~new_class:modified_class
      selector
  in
  let condition = Containers.container_query_to_css_prefix query in
  let cond =
    if String.starts_with ~prefix:"@container " condition then
      drop_prefix "@container " condition
    else "(min-width:0)"
  in
  container_query ~condition:cond ~selector:new_selector ~props ~base_class ()

let has_like_selector kind selector_str base_class props =
  let open Css.Selector in
  (* Parse the selector string to get a proper selector *)
  let reader = Css.Reader.of_string selector_str in
  let parsed_selector = Css.Selector.read reader in
  match kind with
  | `Has ->
      let sel =
        compound
          [
            class_ ("has-[" ^ selector_str ^ "]:" ^ base_class);
            has [ parsed_selector ];
          ]
      in
      regular ~selector:sel ~props ~base_class ()
  | `Group_has ->
      let left = compound [ Class "group"; has [ parsed_selector ] ] in
      let right = Class ("group-has-[" ^ selector_str ^ "]:" ^ base_class) in
      let sel = combine left Descendant right in
      regular ~selector:sel ~props ~base_class ()
  | `Peer_has ->
      let left = compound [ Class "peer"; has [ parsed_selector ] ] in
      let right = Class ("peer-has-[" ^ selector_str ^ "]:" ^ base_class) in
      let sel = combine left Subsequent_sibling right in
      regular ~selector:sel ~props ~base_class ()

(** Extract class name from a modified selector (with or without pseudo-class)
*)
let extract_modified_class_name modified_base_selector base_class =
  match modified_base_selector with
  | Css.Selector.Class cls -> cls
  | Css.Selector.Compound selectors ->
      (* For compound selectors like .hover:prose:hover, extract just the class
         name *)
      List.find_map
        (function Css.Selector.Class cls -> Some cls | _ -> None)
        selectors
      |> Option.value ~default:base_class
  | _ -> base_class

(** Transform selector by applying modifier to base class and updating
    descendants *)
let transform_selector_with_modifier modified_base_selector base_class
    modified_class selector =
  let replace_in_children =
    replace_class_in_selector ~old_class:base_class ~new_class:modified_class
  in
  let rec transform = function
    | Css.Selector.Class cls when cls = base_class -> modified_base_selector
    | Css.Selector.Combined (base_sel, combinator, complex_sel) ->
        Css.Selector.Combined
          (transform base_sel, combinator, replace_in_children complex_sel)
    | Css.Selector.Compound selectors ->
        Css.Selector.Compound (List.map transform selectors)
    | other -> other
  in
  transform selector

(** Handle data attribute modifiers (data-state, data-variant, etc.) *)
let handle_data_modifier key value selector props base_class =
  regular
    ~selector:(selector_with_data_key selector ("data-" ^ key) value)
    ~props ~base_class ()

(** Handle media query modifiers (dark, motion-safe, etc.) *)
let handle_media_modifier ~condition ?(prefix = None) base_class selector props
    =
  match prefix with
  | Some pfx -> media_modifier ~condition ~prefix:pfx base_class props
  | None -> media_query ~condition ~selector ~props ~base_class ()

(** Handle pseudo-class modifiers (hover, focus, active, etc.) *)
let handle_pseudo_class_modifier modifier base_class selector props =
  let modified_base_selector = Modifiers.to_selector modifier base_class in
  let modified_class =
    extract_modified_class_name modified_base_selector base_class
  in
  let has_hover = Modifiers.is_hover modifier in
  let modified_selector =
    transform_selector_with_modifier modified_base_selector base_class
      modified_class selector
  in
  (* Use modified_class as the new base_class to preserve the full modifier
     chain *)
  regular ~selector:modified_selector ~props ~base_class:modified_class
    ~has_hover ()

(** Convert a modifier and its context to a CSS rule *)
let modifier_to_rule modifier base_class selector props =
  match modifier with
  | Style.Data_state value ->
      handle_data_modifier "state" value selector props base_class
  | Style.Data_variant value ->
      handle_data_modifier "variant" value selector props base_class
  | Style.Data_custom (key, value) ->
      handle_data_modifier key value selector props base_class
  | Style.Dark ->
      handle_media_modifier ~condition:"(prefers-color-scheme: dark)" base_class
        selector props
  | Style.Motion_safe ->
      handle_media_modifier ~condition:"(prefers-reduced-motion: no-preference)"
        ~prefix:(Some ".motion-safe\\:") base_class selector props
  | Style.Motion_reduce ->
      handle_media_modifier ~condition:"(prefers-reduced-motion: reduce)"
        ~prefix:(Some ".motion-reduce\\:") base_class selector props
  | Style.Contrast_more ->
      handle_media_modifier ~condition:"(prefers-contrast: more)"
        ~prefix:(Some ".contrast-more\\:") base_class selector props
  | Style.Contrast_less ->
      handle_media_modifier ~condition:"(prefers-contrast: less)"
        ~prefix:(Some ".contrast-less\\:") base_class selector props
  | Style.Responsive breakpoint ->
      responsive_rule breakpoint base_class selector props
  | Style.Container query -> container_rule query base_class selector props
  | Style.Not _modifier ->
      regular
        ~selector:
          (Css.Selector.Class
             ("not-" ^ base_class ^ ":not("
             ^ Css.Selector.to_string selector
             ^ ")"))
        ~props ~base_class ()
  | Style.Has selector_str ->
      has_like_selector `Has selector_str base_class props
  | Style.Group_has selector_str ->
      has_like_selector `Group_has selector_str base_class props
  | Style.Peer_has selector_str ->
      has_like_selector `Peer_has selector_str base_class props
  | Style.Starting ->
      starting_style ~selector:(Css.Selector.Class base_class) ~props
        ~base_class ()
  | Style.Hover | Style.Focus | Style.Active | Style.Focus_within
  | Style.Focus_visible | Style.Disabled ->
      handle_pseudo_class_modifier modifier base_class selector props
  | _ ->
      let sel = Modifiers.to_selector modifier base_class in
      let has_hover = Modifiers.is_hover modifier in
      regular ~selector:sel ~props ~base_class ~has_hover ()

(* Extract selector and properties from a single Utility *)
let extract_selector_props util =
  let rec extract_with_class class_name util_inner = function
    | Style.Style { props; rules; _ } -> (
        let sel = Css.Selector.Class class_name in
        match rules with
        | None -> [ regular ~selector:sel ~props ~base_class:class_name () ]
        | Some rule_list ->
            (* Convert custom rules to selector/props pairs *)
            let custom_rules =
              rule_list
              |> List.map (fun rule ->
                     match Css.as_rule rule with
                     | Some (selector, declarations, _) ->
                         regular ~selector ~props:declarations
                           ~base_class:class_name ()
                     | None ->
                         regular ~selector:Css.Selector.universal ~props:[]
                           ~base_class:class_name ())
            in

            (* If there are base props, add them after the custom rules to match
               Tailwind's order *)
            if props = [] then custom_rules
            else
              custom_rules
              @ [ regular ~selector:sel ~props ~base_class:class_name () ])
    | Style.Modified (modifier, t) ->
        (* For Modified, we unwrap one level to process recursively. The
           base_class should be the full class name of inner_util (the utility
           one level down, preserving its modifiers). *)
        let inner_util, base_style =
          match util_inner with
          | Utility.Modified (_, u) -> (u, t)
          | _ -> (util_inner, t)
        in
        let base_class_name = Utility.to_class inner_util in
        let base = extract_with_class base_class_name inner_util base_style in
        List.concat_map
          (fun rule_out ->
            match rule_out with
            | Regular { selector; props; base_class; _ } ->
                (* Use the base_class from the rule *)
                let bc = Option.value base_class ~default:"" in
                [ modifier_to_rule modifier bc selector props ]
            | _ -> [ rule_out ])
          base
    | Style.Group styles -> (
        (* For Group, extract each element *)
        let extract_group_item style_item util_item =
          let class_name_item = Utility.to_class util_item in
          extract_with_class class_name_item util_item style_item
        in
        match util_inner with
        | Utility.Group util_items ->
            List.map2 extract_group_item styles util_items |> List.concat
        | _ -> List.concat_map (extract_with_class class_name util_inner) styles
        )
  in
  let class_name = Utility.to_class util in
  let style = Utility.to_style util in
  extract_with_class class_name util style

(* ======================================================================== *)
(* Conflict Resolution - Order utilities by specificity *)
(* ======================================================================== *)

(** Strip leading dot from selector string *)
let extract_core_selector selector =
  if String.starts_with ~prefix:"." selector then
    String.sub selector 1 (String.length selector - 1)
  else selector

(** Extract first class name before any space, combinator, etc. This avoids
    confusing descendant selectors like ".prose :where(p)" with modifier
    prefixes like "hover:prose" *)
let extract_first_class_name core =
  match String.index_opt core ' ' with
  | Some space_pos -> String.sub core 0 space_pos
  | None -> core

(** Strip CSS pseudo-selectors from the end of a class name. Pseudo-selectors
    like :has(img), :hover follow the pattern :name or :name(args). We look for
    colons followed by lowercase letters or opening parenthesis, indicating a
    CSS pseudo-selector rather than a modifier prefix. *)
let strip_pseudo_selectors s =
  let len = String.length s in
  let rec find_last_pseudo i =
    if i < 0 then s
    else if s.[i] = ':' && i + 1 < len then
      let next_char = s.[i + 1] in
      if (next_char >= 'a' && next_char <= 'z') || next_char = '(' then
        String.sub s 0 i
      else find_last_pseudo (i - 1)
    else find_last_pseudo (i - 1)
  in
  find_last_pseudo (len - 1)

(** Strip modifier prefixes (sm:, md:, hover:, etc.) to extract base utility
    name. Modifier prefixes come before the utility name. *)
let extract_base_utility class_name_no_pseudo =
  match String.rindex_opt class_name_no_pseudo ':' with
  | Some colon_pos ->
      String.sub class_name_no_pseudo (colon_pos + 1)
        (String.length class_name_no_pseudo - colon_pos - 1)
  | None -> class_name_no_pseudo

(** Parse utility and get ordering, with fallback for non-utility classes *)
let parse_utility_order base_utility =
  let parts = String.split_on_char '-' base_utility in
  match Utility.base_of_strings parts with
  | Ok u -> Utility.order u
  | Error _ ->
      (* Some selectors (like .group, .peer, .container) are marker classes that
         don't parse as utilities. Give them a default low priority. *)
      (9999, 0)

(** Compute conflict resolution order from a CSS selector string. Returns
    (priority, suborder) tuple for ordering utilities. *)
let conflict_order selector =
  selector |> extract_core_selector |> extract_first_class_name
  |> strip_pseudo_selectors |> extract_base_utility |> parse_utility_order

(* Extract selector and props pairs from Regular rules. *)
let extract_selector_props_pairs rules =
  List.filter_map
    (fun rule ->
      match rule with
      | Regular { selector; props; base_class; _ } ->
          (* Compute ordering from base_class if available, otherwise parse
             selector *)
          let order =
            match base_class with
            | Some class_name -> (
                match Utility.base_of_class class_name with
                | Ok u -> Utility.order u
                | Error _ ->
                    (* base_class doesn't parse as a utility (e.g. "group"
                       marker class). Fall back to parsing the selector
                       string. *)
                    let sel_str = Css.Selector.to_string selector in
                    conflict_order sel_str)
            | None ->
                (* Fallback: parse selector if base_class is missing *)
                let sel_str = Css.Selector.to_string selector in
                conflict_order sel_str
          in
          Some (selector, props, order)
      | _ -> None)
    rules

(* ======================================================================== *)
(* Rule Processing - Group and organize rules *)
(* ======================================================================== *)

let classify_by_type all_rules =
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
      | Media_query { condition; selector; props; base_class } ->
          let order =
            match base_class with
            | Some class_name -> (
                match Utility.base_of_class class_name with
                | Ok u -> Utility.order u
                | Error _ ->
                    let sel_str = Css.Selector.to_string selector in
                    conflict_order sel_str)
            | None ->
                let sel_str = Css.Selector.to_string selector in
                conflict_order sel_str
          in
          let rules = try Hashtbl.find tbl condition with Not_found -> [] in
          Hashtbl.replace tbl condition ((selector, props, order) :: rules)
      | _ -> ())
    media_rules;
  (* Extract and sort media queries by breakpoint order *)
  let media_list =
    Hashtbl.fold (fun k v acc -> (k, List.rev v) :: acc) tbl []
  in
  (* Sort by min-width values to ensure correct cascading order *)
  List.sort
    (fun (a, _) (b, _) ->
      (* Extract min-width values for comparison *)
      let extract_min_width condition =
        if String.contains condition '(' then
          try
            let start = String.index condition ':' + 1 in
            let end_ = String.index_from condition start ')' in
            let value = String.sub condition start (end_ - start) in
            (* Parse rem values to floats for comparison *)
            if String.contains value 'r' then
              float_of_string (String.sub value 0 (String.index value 'r'))
            else 0.0
          with _ -> 0.0
        else 0.0
      in
      compare (extract_min_width a) (extract_min_width b))
    media_list

let group_container_queries container_rules =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun rule ->
      match rule with
      | Container_query { condition; selector; props; base_class } ->
          let order =
            match base_class with
            | Some class_name -> (
                match Utility.base_of_class class_name with
                | Ok u -> Utility.order u
                | Error _ ->
                    let sel_str = Css.Selector.to_string selector in
                    conflict_order sel_str)
            | None ->
                let sel_str = Css.Selector.to_string selector in
                conflict_order sel_str
          in
          let rules = try Hashtbl.find tbl condition with Not_found -> [] in
          Hashtbl.replace tbl condition ((selector, props, order) :: rules)
      | _ -> ())
    container_rules;
  (* Reverse once to restore original insertion order per condition *)
  Hashtbl.fold (fun k v acc -> (k, List.rev v) :: acc) tbl []

let is_simple_class_selector sel =
  (* Check if selector is a simple class without combinators or
     pseudo-elements *)
  match sel with
  | Css.Selector.Class _ -> true
  | _ -> false

let compare_indexed ~filter_custom_props (i1, sel1, _, (prio1, sub1))
    (i2, sel2, _, (prio2, sub2)) =
  let prio_cmp = Int.compare prio1 prio2 in
  if prio_cmp <> 0 then prio_cmp
  else
    (* First sort by suborder *)
    let sub_cmp = Int.compare sub1 sub2 in
    if sub_cmp <> 0 then sub_cmp
    else if
      filter_custom_props
      && is_simple_class_selector sel1
      && is_simple_class_selector sel2
    then
      (* For utilities with same priority and suborder, sort alphabetically.
         This handles display utilities from different modules (flex, grid,
         block) which all have priority=4 and suborder=0. *)
      let sel1_str = Css.Selector.to_string sel1 in
      let sel2_str = Css.Selector.to_string sel2 in
      String.compare sel1_str sel2_str
    else
      (* For complex selectors or non-utilities, preserve original order *)
      Int.compare i1 i2

(* Convert selector/props/order triples to CSS rules with conflict ordering *)
let of_grouped ?(filter_custom_props = false) grouped_list =
  (* Sort by (priority, suborder, selector_name, original_index) to match
     Tailwind v4 ordering. *)
  let indexed =
    List.mapi (fun i (sel, props, order) -> (i, sel, props, order)) grouped_list
  in
  let sorted_indexed =
    List.sort (compare_indexed ~filter_custom_props) indexed
  in
  List.map
    (fun (_idx, selector, props, _order) ->
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
    sorted_indexed

let build_utilities_layer ~rules ~media_queries ~container_queries =
  (* Rules are already sorted by of_grouped in the correct conflict order. Don't
     re-sort them here as that would break the original source order for rules
     with the same selector (e.g., prose rules). *)
  let statements =
    rules
    @ List.map
        (fun (condition, rules) -> Css.media ~condition rules)
        media_queries
    @ List.map
        (fun (name, condition, rules) -> Css.container ?name ~condition rules)
        container_queries
  in

  Css.v [ Css.layer ~name:"utilities" statements ]

let add_hover_to_media_map hover_rules media_map =
  (* Gate hover rules behind (hover:hover) media query to prevent them from
     applying on touch devices where :hover can stick after tapping. This
     follows modern CSS best practices for hover states. *)
  if hover_rules = [] then media_map
  else
    let hover_condition = "(hover:hover)" in
    (* Update association list in-place to avoid hashtable churn. *)
    let rec update acc = function
      | [] -> List.rev ((hover_condition, hover_rules) :: acc)
      | (cond, rules) :: tl when String.equal cond hover_condition ->
          (* Prepend hover rules to existing for this condition *)
          List.rev_append acc ((hover_condition, hover_rules @ rules) :: tl)
      | hd :: tl -> update (hd :: acc) tl
    in
    update [] media_map

(* Deduplicate selector/props pairs while preserving first occurrence order *)
let deduplicate_selector_props triples =
  let seen = Hashtbl.create (List.length triples) in
  List.filter
    (fun (sel, props, _order) ->
      let key = (Css.Selector.to_string sel, props) in
      if Hashtbl.mem seen key then false
      else (
        Hashtbl.add seen key ();
        true))
    triples

(* Convert selector/props pairs to CSS rules. *)
(* Internal: build rule sets from pre-extracted outputs. *)
let rule_sets_from_selector_props all_rules =
  let separated = classify_by_type all_rules in
  (* First separate hover from non-hover rules *)
  let hover_regular, non_hover_regular =
    List.partition is_hover_rule separated.regular
  in
  let non_hover_pairs =
    extract_selector_props_pairs non_hover_regular |> deduplicate_selector_props
  in
  let hover_pairs =
    extract_selector_props_pairs hover_regular |> deduplicate_selector_props
  in
  let rules = of_grouped ~filter_custom_props:true non_hover_pairs in
  let media_queries_map =
    group_media_queries separated.media |> add_hover_to_media_map hover_pairs
  in
  let media_queries =
    List.map
      (fun (condition, rule_list) ->
        ( condition,
          of_grouped ~filter_custom_props:true
            (deduplicate_selector_props rule_list) ))
      media_queries_map
  in
  let container_queries_map = group_container_queries separated.container in
  let container_queries =
    List.map
      (fun (condition, rule_list) ->
        ( None,
          condition,
          of_grouped ~filter_custom_props:true
            (deduplicate_selector_props rule_list) ))
      container_queries_map
  in
  (rules, media_queries, container_queries)

let rule_sets tw_classes =
  let all_rules = tw_classes |> List.concat_map extract_selector_props in
  rule_sets_from_selector_props all_rules

(* ======================================================================== *)
(* Layer Generation - CSS @layer directives and theme variable resolution *)
(* ======================================================================== *)

module Strings = Set.Make (String)

(* Helpers for theme layer extraction and ordering *)
let collect_selector_props tw_classes =
  List.concat_map extract_selector_props tw_classes

let extract_non_tw_custom_declarations selector_props =
  (* Use Hashtbl to collect unique theme variables efficiently *)
  let theme_vars = Hashtbl.create 32 in
  let insertion_order = ref [] in

  selector_props
  |> List.iter (function
         | Regular { props; _ }
         | Media_query { props; _ }
         | Container_query { props; _ }
         | Starting_style { props; _ }
         ->
         Css.custom_declarations ~layer:"theme" props
         |> List.iter (fun decl ->
                (* Add to hashtable if not already present *)
                match Css.custom_declaration_name decl with
                | Some name when not (Hashtbl.mem theme_vars name) ->
                    Hashtbl.add theme_vars name decl;
                    insertion_order := decl :: !insertion_order
                | _ -> ()));
  (* Return in original insertion order *)
  List.rev !insertion_order

(* Get Var.any from declaration metadata *)
(* var_of_declaration_meta no longer used after refactor *)

(* assemble_theme_decls_metadata no longer used; ordering handled in
   compute_theme_layer *)

(* Internal helper to compute theme layer from pre-extracted outputs. *)
let compute_theme_layer_from_selector_props ?(default_decls = []) selector_props
    =
  let extracted = extract_non_tw_custom_declarations selector_props in
  (* Split defaults so we can place base font family vars before extracted
     tokens, and the indirection defaults (default-font-family,
     default-mono-font-family) after extracted tokens to match Tailwind's
     order. *)
  let is_default_family name =
    name = "default-font-family" || name = "default-mono-font-family"
  in
  let pre_defaults, post_defaults =
    List.partition
      (fun decl ->
        match Css.custom_declaration_name decl with
        | Some n -> not (is_default_family n)
        | None -> false)
      default_decls
  in
  (* Filter out defaults already present by name (use sets for faster
     lookups) *)
  let names_set_of lst =
    List.fold_left
      (fun acc d ->
        match Css.custom_declaration_name d with
        | Some n -> Strings.add n acc
        | None -> acc)
      Strings.empty lst
  in
  let extracted_names = names_set_of extracted in
  let pre =
    pre_defaults
    |> List.filter (fun d ->
           match Css.custom_declaration_name d with
           | Some n -> not (Strings.mem n extracted_names)
           | None -> false)
  in
  let pre_names = names_set_of pre in
  let post =
    post_defaults
    |> List.filter (fun d ->
           match Css.custom_declaration_name d with
           | Some n ->
               (not (Strings.mem n extracted_names))
               && not (Strings.mem n pre_names)
           | None -> false)
  in
  let theme_generated_vars = pre @ extracted @ post in

  (* Sort variables by their order metadata *)
  (* Pre-extract metadata once to avoid repeated extraction during sorting *)
  let vars_with_order =
    List.map
      (fun decl ->
        let order = Var.order_of_declaration decl in
        (decl, order))
      theme_generated_vars
  in
  let sorted_vars_with_order =
    List.sort
      (fun (_, order_a) (_, order_b) ->
        match (order_a, order_b) with
        | Some (prio_a, sub_a), Some (prio_b, sub_b) ->
            let prio_cmp = Int.compare prio_a prio_b in
            if prio_cmp = 0 then Int.compare sub_a sub_b else prio_cmp
        | Some _, None -> -1
        | None, Some _ -> 1
        | None, None -> 0)
      vars_with_order
  in
  let sorted_vars = List.map fst sorted_vars_with_order in

  if sorted_vars = [] then Css.v [ Css.layer ~name:"theme" [] ]
  else
    let selector = Css.Selector.(list [ Root; host () ]) in
    Css.v [ Css.layer ~name:"theme" [ Css.rule ~selector sorted_vars ] ]

let compute_theme_layer ?(default_decls = []) tw_classes =
  let selector_props = collect_selector_props tw_classes in
  compute_theme_layer_from_selector_props ~default_decls selector_props

let placeholder_supports =
  let placeholder = Css.Selector.Placeholder in

  (* Create the inner @supports for modern browsers *)
  let modern_rule =
    Css.rule ~selector:placeholder
      [
        Css.color
          (Css.color_mix ~in_space:Oklab ~percent1:50 Current Transparent);
      ]
  in
  let modern_support_stmt =
    Css.supports ~condition:"(color:color-mix(in lab, red, red))"
      [ modern_rule ]
  in

  (* Create the outer @supports with the fallback rule and nested modern
     support *)
  let fallback_rule = Css.rule ~selector:placeholder [ Css.color Current ] in
  let outer_support_content = [ fallback_rule; modern_support_stmt ] in

  Css.v
    [
      Css.supports
        ~condition:
          "(not ((-webkit-appearance:-apple-pay-button))) or \
           (contain-intrinsic-size:1px)"
        outer_support_content;
    ]

let build_base_layer ?supports () =
  let base = Preflight.stylesheet ?placeholder_supports:supports () in
  Css.layer_of ~name:"base" base

(* Use the centralized conversion function from Var module *)

(* Build the properties layer with browser detection for initial values *)
(* Returns (properties_layer, property_rules) - @property rules are separate *)
let build_properties_layer explicit_property_rules_statements =
  (* Split statements into @property rules and other statements *)
  let property_rules, other_statements =
    List.partition
      (fun stmt ->
        match Css.as_property stmt with Some _ -> true | None -> false)
      explicit_property_rules_statements
  in

  (* Deduplicate @property rules by property name, preserving first occurrence
     order *)
  let deduplicated_property_rules =
    let seen = Hashtbl.create 16 in
    List.filter
      (fun stmt ->
        match Css.as_property stmt with
        | Some (Css.Property_info { name; _ }) ->
            if Hashtbl.mem seen name then false
            else (
              Hashtbl.add seen name ();
              true)
        | None -> true)
      property_rules
  in

  (* Extract variable initial values from @property declarations *)
  let variable_initial_values =
    List.fold_left
      (fun acc stmt ->
        match Css.as_property stmt with
        | Some (Css.Property_info info as prop_info) ->
            let value = Var.property_initial_string prop_info in
            (info.name, value) :: acc
        | None -> acc)
      [] deduplicated_property_rules
    |> List.rev
  in

  if deduplicated_property_rules = [] && variable_initial_values = [] then
    (Css.empty, [])
  else
    (* Build the properties layer with browser detection but WITHOUT @property
       rules *)
    let browser_detection_condition =
      "(((-webkit-hyphens:none)) and (not (margin-trim:inline))) or \
       ((-moz-orient:inline) and (not (color:rgb(from red r g b))))"
    in
    (* Create the selector for universal + pseudo-elements *)
    let selector = Css.Selector.(list [ universal; Before; After; Backdrop ]) in
    (* Create initial declarations for each property with their actual initial
       values *)
    let initial_declarations =
      List.map
        (fun (name, value) -> Css.custom_property name value)
        variable_initial_values
    in
    let rule = Css.rule ~selector initial_declarations in
    let supports_content = [ rule ] in
    let supports_stmt =
      Css.supports ~condition:browser_detection_condition supports_content
    in
    (* Properties layer only has the supports statement and other statements,
       NOT @property rules *)
    let layer_content = [ supports_stmt ] @ other_statements in
    let layer = Css.v [ Css.layer ~name:"properties" layer_content ] in
    (layer, deduplicated_property_rules)

(** Extract variables and property rules from utility styles recursively *)
let rec extract_vars_and_property_rules_from_style = function
  | Style.Style { props; rules; property_rules; _ } ->
      let vars_from_props = Css.vars_of_declarations props in
      let vars_from_rules =
        match rules with Some r -> Css.vars_of_rules r | None -> []
      in
      (vars_from_props @ vars_from_rules, [ property_rules ])
  | Style.Modified (_, t) -> extract_vars_and_property_rules_from_style t
  | Style.Group ts ->
      let vars_list, prop_rules_list =
        List.split (List.map extract_vars_and_property_rules_from_style ts)
      in
      (List.concat vars_list, List.concat prop_rules_list)

(** Collect all property rules: explicit ones and auto-generated ones *)
let collect_all_property_rules vars_from_utilities
    explicit_property_rules_statements =
  (* Get variables that need @property rules *)
  let vars_needing_property =
    vars_from_utilities
    |> List.filter (fun (Css.V v) -> Var.var_needs_property v)
  in
  (* Compute names of variables that already have explicit @property rules *)
  let explicit_property_var_names_set =
    explicit_property_rules_statements
    |> List.filter_map (fun stmt ->
           match Css.as_property stmt with
           | Some (Css.Property_info info) -> Some info.name
           | None -> None)
    |> List.fold_left (fun acc n -> Strings.add n acc) Strings.empty
  in
  (* Generate @property rules for variables without explicit rules *)
  let property_rules_from_utilities =
    vars_needing_property
    |> List.filter (fun (Css.V v) ->
           let var_name = "--" ^ Css.var_name v in
           not (Strings.mem var_name explicit_property_var_names_set))
    |> List.map (fun (Css.V v) ->
           let var_name = "--" ^ Css.var_name v in
           Css.property ~name:var_name Css.Universal ~inherits:false ())
  in
  let property_rules_from_utilities_as_statements =
    property_rules_from_utilities |> List.concat_map Css.statements
  in
  explicit_property_rules_statements
  @ property_rules_from_utilities_as_statements

(** Build layer declaration list based on which layers are present *)
let build_layer_declaration ~has_properties ~include_base =
  let names =
    (if has_properties then [ "properties" ] else [])
    @
    if include_base then [ "theme"; "base"; "components"; "utilities" ]
    else [ "theme"; "components"; "utilities" ]
  in
  Css.v [ Css.layer_decl names ]

(** Assemble all CSS layers in the correct order *)
let assemble_all_layers ~include_base ~properties_layer ~theme_layer ~base_layer
    ~utilities_layer ~property_rules_for_end =
  let base_layers =
    if include_base then [ theme_layer; base_layer ] else [ theme_layer ]
  in
  let components_declaration = Css.v [ Css.layer_decl [ "components" ] ] in
  let layer_names =
    build_layer_declaration
      ~has_properties:(Option.is_some properties_layer)
      ~include_base
  in
  let initial_layers =
    match properties_layer with None -> [] | Some l -> [ l ]
  in
  let layers_without_property =
    [ layer_names ] @ initial_layers @ base_layers
    @ [ components_declaration; utilities_layer ]
  in
  let property_rules_css =
    if property_rules_for_end = [] then [] else [ Css.v property_rules_for_end ]
  in
  layers_without_property @ property_rules_css

(** Build all CSS layers from utilities and rules *)
let build_layers ~include_base ~selector_props tw_classes rules media_queries
    container_queries =
  (* Convert Utility.t list to Style.t list *)
  let styles = List.map Utility.to_style tw_classes in
  (* Extract variables and property rules in a single pass *)
  let vars_from_utilities, property_rules_lists =
    let results = List.map extract_vars_and_property_rules_from_style styles in
    let vars_list, prop_rules_list = List.split results in
    (List.concat vars_list, List.concat prop_rules_list)
  in
  let explicit_property_rules_statements =
    property_rules_lists |> List.concat_map Css.statements
  in
  (* Collect all property rules (explicit + auto-generated) *)
  let all_property_statements =
    collect_all_property_rules vars_from_utilities
      explicit_property_rules_statements
  in
  (* Build individual layers *)
  let theme_defaults = Typography.default_font_family_declarations in
  let theme_layer =
    compute_theme_layer_from_selector_props ~default_decls:theme_defaults
      selector_props
  in
  let base_layer = build_base_layer ~supports:placeholder_supports () in
  let properties_layer, property_rules_for_end =
    if all_property_statements = [] then (None, [])
    else
      let layer, prop_rules = build_properties_layer all_property_statements in
      if layer = Css.empty then (None, prop_rules) else (Some layer, prop_rules)
  in
  let utilities_layer =
    build_utilities_layer ~rules ~media_queries ~container_queries
  in
  (* Assemble everything in the correct order *)
  assemble_all_layers ~include_base ~properties_layer ~theme_layer ~base_layer
    ~utilities_layer ~property_rules_for_end

let wrap_css_items ~rules ~media_queries ~container_queries =
  let rules_stylesheet = Css.v rules in
  let media_stylesheets =
    List.map
      (fun (condition, rules) -> Css.v [ Css.media ~condition rules ])
      media_queries
  in
  let container_stylesheets =
    List.map
      (fun (name, condition, rules) ->
        Css.v [ Css.container ?name ~condition rules ])
      container_queries
  in
  Css.concat (rules_stylesheet :: (media_stylesheets @ container_stylesheets))

(* ======================================================================== *)
(* Main API - Convert Tw styles to CSS *)

(* ======================================================================== *)

type config = { base : bool; mode : Css.mode; optimize : bool }
(** Configuration for CSS generation *)

let default_config = { base = true; mode = Css.Variables; optimize = false }

let to_css ?(config = default_config) tw_classes =
  (* Extract once and share for rule sets and theme layer *)
  let selector_props = List.concat_map extract_selector_props tw_classes in

  let rules, media_queries, container_queries =
    rule_sets_from_selector_props selector_props
  in

  (* Generate layers whenever mode = Variables. Include the base layer only when
     [reset=true]. In Inline mode, emit raw rules without layers. *)
  let stylesheet =
    match config.mode with
    | Css.Variables ->
        let layers =
          build_layers ~include_base:config.base ~selector_props tw_classes
            rules media_queries container_queries
        in
        Css.concat layers
    | Css.Inline ->
        (* No layers - just raw utility rules with var() resolved to fallback
           values *)
        wrap_css_items ~rules ~media_queries ~container_queries
  in
  (* Apply optimization if requested *)
  if config.optimize then Css.optimize stylesheet else stylesheet

let to_inline_style utilities =
  (* Convert Utility.t list to Style.t list *)
  let styles = List.map Utility.to_style utilities in
  (* Collect all declarations from props and embedded rules. Build in reverse
     using [rev_append] to avoid quadratic concatenations, then reverse once. *)
  let rec collect acc = function
    | Style.Style { props; rules; _ } ->
        let from_rules =
          match rules with
          | None -> []
          | Some rs ->
              List.concat
                (List.filter_map
                   (fun rule ->
                     match Css.as_rule rule with
                     | Some (_selector, declarations, _important) ->
                         Some declarations
                     | None -> None)
                   rs)
        in
        let acc = List.rev_append from_rules acc in
        List.rev_append props acc
    | Style.Modified (_, t) -> collect acc t
    | Style.Group ts -> List.fold_left collect acc ts
  in
  let all_props = List.rev (List.fold_left collect [] styles) in
  (* Filter out CSS custom properties (variables) - they shouldn't be in inline
     styles *)
  let non_variable_props =
    List.filter (fun decl -> Css.custom_declaration_name decl = None) all_props
  in
  Css.inline_style_of_declarations non_variable_props
