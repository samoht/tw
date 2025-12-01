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
      nested : Css.statement list; (* Nested statements (e.g., @media) *)
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

(* Indexed rule for sorting with typed fields *)
type indexed_rule = {
  index : int;
  rule_type :
    [ `Regular | `Media of string | `Container of string | `Starting ];
  selector : Css.Selector.t;
  props : Css.declaration list;
  order : int * int;
  nested : Css.statement list;
  base_class : string option;
}

(* Result of building individual layers *)
type layers_result = {
  theme_layer : Css.t;
  base_layer : Css.t;
  properties_layer : Css.t option;
  utilities_layer : Css.t;
  property_rules : Css.statement list;
}

(* ======================================================================== *)
(* Smart constructors for output *)
(* ======================================================================== *)

let regular ~selector ~props ?base_class ?(has_hover = false) ?(nested = []) ()
    =
  Regular { selector; props; base_class; has_hover; nested }

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
  regular ~selector:modified_selector ~props ~base_class:modified_class
    ~has_hover ()

(* Route data modifiers to appropriate handler *)
let route_data_modifier modifier base_class selector props =
  let key, value =
    match modifier with
    | Style.Data_state v -> ("state", v)
    | Style.Data_variant v -> ("variant", v)
    | Style.Data_custom (k, v) -> (k, v)
    | _ -> failwith "Invalid data modifier"
  in
  handle_data_modifier key value selector props base_class

(* Route preference media modifiers to appropriate handler *)
let route_preference_modifier modifier base_class selector props =
  let kind, value =
    match modifier with
    | Style.Motion_safe -> ("reduced-motion", "no-preference")
    | Style.Motion_reduce -> ("reduced-motion", "reduce")
    | Style.Contrast_more -> ("contrast", "more")
    | Style.Contrast_less -> ("contrast", "less")
    | _ -> failwith "Invalid preference modifier"
  in
  let prefix_map =
    [
      (("reduced-motion", "no-preference"), ".motion-safe:");
      (("reduced-motion", "reduce"), ".motion-reduce:");
      (("contrast", "more"), ".contrast-more:");
      (("contrast", "less"), ".contrast-less:");
    ]
  in
  let prefix = List.assoc_opt (kind, value) prefix_map in
  handle_media_modifier
    ~condition:(Printf.sprintf "(prefers-%s:%s)" kind value)
    ~prefix base_class selector props

(* Route :has() variants to appropriate handler *)
let route_has_modifier modifier base_class props =
  let kind, selector_str =
    match modifier with
    | Style.Has s -> (`Has, s)
    | Style.Group_has s -> (`Group_has, s)
    | Style.Peer_has s -> (`Peer_has, s)
    | _ -> failwith "Invalid has modifier"
  in
  has_like_selector kind selector_str base_class props

(* Handle fallback for unmatched modifiers *)
let handle_fallback_modifier modifier base_class props =
  let sel = Modifiers.to_selector modifier base_class in
  let has_hover = Modifiers.is_hover modifier in
  regular ~selector:sel ~props ~base_class ~has_hover ()

(** Convert a modifier and its context to a CSS rule *)
let modifier_to_rule modifier base_class selector props =
  match modifier with
  (* Data modifiers *)
  | Style.Data_state _ | Style.Data_variant _ | Style.Data_custom _ ->
      route_data_modifier modifier base_class selector props
  (* Color scheme *)
  | Style.Dark ->
      handle_media_modifier ~condition:"(prefers-color-scheme:dark)"
        ~prefix:(Some ".dark:") base_class selector props
  (* Preference media modifiers *)
  | Style.Motion_safe | Style.Motion_reduce | Style.Contrast_more
  | Style.Contrast_less ->
      route_preference_modifier modifier base_class selector props
  (* Responsive and container *)
  | Style.Responsive breakpoint ->
      responsive_rule breakpoint base_class selector props
  | Style.Container query -> container_rule query base_class selector props
  (* :not() pseudo-class *)
  | Style.Not _modifier ->
      regular
        ~selector:
          (Css.Selector.Class
             ("not-" ^ base_class ^ ":not("
             ^ Css.Selector.to_string selector
             ^ ")"))
        ~props ~base_class ()
  (* :has() variants *)
  | Style.Has _ | Style.Group_has _ | Style.Peer_has _ ->
      route_has_modifier modifier base_class props
  (* Starting style *)
  | Style.Starting ->
      starting_style ~selector:(Css.Selector.Class base_class) ~props
        ~base_class ()
  (* Interactive pseudo-classes *)
  | Style.Hover | Style.Focus | Style.Active | Style.Focus_within
  | Style.Focus_visible | Style.Disabled ->
      handle_pseudo_class_modifier modifier base_class selector props
  (* Fallback for other modifiers *)
  | _ -> handle_fallback_modifier modifier base_class props

(* Extract selector and properties from a single Utility *)
(* Apply modifier to extracted rule *)
let apply_modifier_to_rule modifier = function
  | Regular { selector; props; base_class; _ } ->
      let bc = Option.value base_class ~default:"" in
      [ modifier_to_rule modifier bc selector props ]
  | other -> [ other ]

(* Handle Modified style by recursively extracting and applying modifier *)
let handle_modified util_inner modifier base_style extract_fn =
  let inner_util, style =
    match util_inner with
    | Utility.Modified (_, u) -> (u, base_style)
    | _ -> (util_inner, base_style)
  in
  let base_class_name = Utility.to_class inner_util in
  let base_rules = extract_fn base_class_name inner_util style in
  List.concat_map (apply_modifier_to_rule modifier) base_rules

(* Handle Group style by extracting each item *)
let handle_group class_name util_inner styles extract_fn =
  match util_inner with
  | Utility.Group util_items ->
      let extract_item style_item util_item =
        let class_name_item = Utility.to_class util_item in
        extract_fn class_name_item util_item style_item
      in
      List.map2 extract_item styles util_items |> List.concat
  | _ -> List.concat_map (extract_fn class_name util_inner) styles

let extract_selector_props util =
  let rec extract_with_class class_name util_inner = function
    | Style.Style { props; rules; _ } -> (
        let sel = Css.Selector.Class class_name in
        match rules with
        | None -> [ regular ~selector:sel ~props ~base_class:class_name () ]
        | Some rule_list ->
            (* Extract media queries as separate Media_query outputs *)
            let media_queries =
              rule_list
              |> List.filter_map (fun stmt ->
                     match Css.as_media stmt with
                     | Some (condition, statements) ->
                         statements
                         |> List.filter_map (fun inner ->
                                match Css.as_rule inner with
                                | Some (selector, declarations, _) ->
                                    Some
                                      (media_query ~condition ~selector
                                         ~props:declarations
                                         ~base_class:class_name ())
                                | None -> None)
                         |> fun l -> Some l
                     | None -> None)
              |> List.concat
            in
            (* Extract container queries as separate outputs *)
            let container_queries =
              rule_list
              |> List.filter_map (fun stmt ->
                     match Css.as_container stmt with
                     | Some (_, condition, statements) ->
                         statements
                         |> List.filter_map (fun inner ->
                                match Css.as_rule inner with
                                | Some (selector, declarations, _) ->
                                    Some
                                      (container_query ~condition ~selector
                                         ~props:declarations
                                         ~base_class:class_name ())
                                | None -> None)
                         |> fun l -> Some l
                     | None -> None)
              |> List.concat
            in
            (* Extract plain rules as separate outputs *)
            let plain_rules =
              rule_list
              |> List.filter_map (fun stmt ->
                     match Css.as_rule stmt with
                     | Some (selector, declarations, _) ->
                         Some
                           (regular ~selector ~props:declarations
                              ~base_class:class_name ())
                     | None -> None)
            in
            (* Base rule (only if it has props) *)
            let base_rule =
              if props = [] then []
              else [ regular ~selector:sel ~props ~base_class:class_name () ]
            in
            (* Combine: plain rules first, then base rule, then media/container
               queries. This ensures that when a Style has both props and rules,
               the custom rules come before the base props. *)
            plain_rules @ base_rule @ media_queries @ container_queries)
    | Style.Modified (modifier, base_style) ->
        handle_modified util_inner modifier base_style extract_with_class
    | Style.Group styles ->
        handle_group class_name util_inner styles extract_with_class
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

let build_utilities_layer ~statements =
  (* Statements are already in the correct order with media queries interleaved.
     Consecutive media queries with the same condition will be merged by the
     optimizer (css/optimize.ml) while preserving cascade order. *)
  Css.v [ Css.layer ~name:"utilities" statements ]

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

(* Type-directed helpers for rule sorting and construction *)

(* Determine sort group for rule types. Regular and Media are grouped together
   to preserve utility grouping - media queries appear immediately after their
   base utility rule. *)
let rule_type_order = function
  | `Regular -> 0
  | `Media _ -> 0 (* Same as Regular to keep grouped *)
  | `Container _ -> 1
  | `Starting -> 2

(* Extract media query sort key for ordering. Responsive breakpoints come last,
   sorted by breakpoint size. Other media queries like hover and prefers-* come
   first. Returns a tuple for comparison: higher values sort later. *)
let extract_media_sort_key = function
  | `Media cond ->
      if
        (* Check if this is a responsive breakpoint (min-width:XXrem) *)
        String.length cond > 10 && String.sub cond 0 10 = "(min-width"
      then
        try
          let start = String.index cond ':' + 1 in
          let end_pos = String.index_from cond start 'r' in
          let value_str = String.sub cond start (end_pos - start) in
          (* Responsive breakpoints: use 1000 + breakpoint to sort after
             others *)
          (1000 + int_of_string value_str, 0)
        with _ -> (1000, 0) (* Fallback for unparseable responsive queries *)
      else
        (* Non-responsive media queries like hover come first *)
        (0, 0)
  | _ -> (0, 0)

(* Compare rules by priority, then suborder, then alphabetically (for simple
   class selectors), then by original index. Lower priority values come
   first. *)
let compare_by_priority_suborder_alpha sel1 sel2 (p1, s1) (p2, s2) i1 i2 =
  let prio_cmp = Int.compare p1 p2 in
  if prio_cmp <> 0 then prio_cmp
  else
    let sub_cmp = Int.compare s1 s2 in
    if sub_cmp <> 0 then sub_cmp
    else if is_simple_class_selector sel1 && is_simple_class_selector sel2 then
      String.compare (Css.Selector.to_string sel1) (Css.Selector.to_string sel2)
    else Int.compare i1 i2

(* Compare two regular rules *)
let compare_regular_rules sel1 sel2 order1 order2 i1 i2 =
  compare_by_priority_suborder_alpha sel1 sel2 order1 order2 i1 i2

(* Compare two media query rules - sort by media type first (responsive last),
   then by breakpoint size, then by base utility priority/suborder *)
let compare_media_rules typ1 typ2 sel1 sel2 order1 order2 i1 i2 =
  let key1, _ = extract_media_sort_key typ1 in
  let key2, _ = extract_media_sort_key typ2 in
  let key_cmp = Int.compare key1 key2 in
  if key_cmp <> 0 then key_cmp
  else compare_by_priority_suborder_alpha sel1 sel2 order1 order2 i1 i2

(* Compare Regular vs Media rules.

   When they share the same base_class, preserve original order so media queries
   appear immediately after their base utility (e.g., .container followed by its
   responsive @media blocks).

   When they have different base classes: Compare by priority first. This
   ensures container (p=0) and its media queries stay at the top before other
   utilities with higher priority values. Only when priorities are equal do we
   put Regular before Media for proper CSS cascade ordering. *)
let compare_regular_vs_media base_class1 base_class2 i1 i2 order1 order2 _sel1
    _sel2 =
  match (base_class1, base_class2) with
  | Some bc1, Some bc2 when bc1 = bc2 ->
      (* Same base class - preserve original order so container and its
         responsive @media blocks stay together *)
      Int.compare i1 i2
  | _ ->
      (* Different base classes: compare by priority/suborder only. This ensures
         low-priority utilities (like container p=0) and their media queries
         appear before higher-priority utilities (like colors p=21). When
         priorities are equal, Regular comes before Media. *)
      let (p1, s1), (p2, s2) = (order1, order2) in
      let prio_cmp = Int.compare p1 p2 in
      if prio_cmp <> 0 then prio_cmp
      else
        let sub_cmp = Int.compare s1 s2 in
        if sub_cmp <> 0 then sub_cmp
        else -1 (* Regular before Media at same priority *)

(* Compare indexed rules for sorting *)
let compare_indexed_rules r1 r2 =
  let type_cmp =
    Int.compare (rule_type_order r1.rule_type) (rule_type_order r2.rule_type)
  in
  if type_cmp <> 0 then type_cmp
  else
    match (r1.rule_type, r2.rule_type) with
    | `Regular, `Regular ->
        compare_regular_rules r1.selector r2.selector r1.order r2.order r1.index
          r2.index
    | `Media _, `Media _ ->
        compare_media_rules r1.rule_type r2.rule_type r1.selector r2.selector
          r1.order r2.order r1.index r2.index
    | `Regular, `Media _ ->
        compare_regular_vs_media r1.base_class r2.base_class r1.index r2.index
          r1.order r2.order r1.selector r2.selector
    | `Media _, `Regular ->
        (* Negate because we're comparing in the opposite direction *)
        -compare_regular_vs_media r2.base_class r1.base_class r2.index r1.index
           r2.order r1.order r2.selector r1.selector
    | _, _ -> Int.compare r1.index r2.index

(* Filter properties to only include utilities layer declarations *)
let filter_utility_properties props =
  List.filter
    (fun decl ->
      match Css.custom_declaration_layer decl with
      | Some layer when layer = "utilities" -> true
      | Some _ -> false
      | None -> (
          match Css.custom_declaration_name decl with
          | None -> true
          | Some _ -> false))
    props

(* Convert indexed rule to CSS statement *)
let indexed_rule_to_statement r =
  let filtered_props = filter_utility_properties r.props in
  match r.rule_type with
  | `Regular | `Starting ->
      Css.rule ~selector:r.selector ~nested:r.nested filtered_props
  | `Media condition ->
      Css.media ~condition [ Css.rule ~selector:r.selector filtered_props ]
  | `Container condition ->
      Css.container ~condition [ Css.rule ~selector:r.selector filtered_props ]

(* Deduplicate typed triples while preserving first occurrence order *)
let deduplicate_typed_triples triples =
  let seen = Hashtbl.create (List.length triples) in
  List.filter
    (fun (typ, sel, props, _order, nested, _base_class) ->
      let key = (typ, Css.Selector.to_string sel, props, nested) in
      if Hashtbl.mem seen key then false
      else (
        Hashtbl.add seen key ();
        true))
    triples

(* Get utility order from base class, with fallback to conflict order *)
let order_of_base base_class selector =
  match base_class with
  | Some class_name -> (
      match Utility.base_of_class class_name with
      | Ok u -> Utility.order u
      | Error _ -> conflict_order (Css.Selector.to_string selector))
  | None -> conflict_order (Css.Selector.to_string selector)

(* Convert each rule type to typed triple *)
let rule_to_triple = function
  | Regular { selector; props; base_class; nested; _ } ->
      Some
        ( `Regular,
          selector,
          props,
          order_of_base base_class selector,
          nested,
          base_class )
  | Media_query { condition; selector; props; base_class } ->
      Some
        ( `Media condition,
          selector,
          props,
          order_of_base base_class selector,
          [],
          base_class )
  | Container_query { condition; selector; props; base_class } ->
      Some
        ( `Container condition,
          selector,
          props,
          order_of_base base_class selector,
          [],
          base_class )
  | Starting_style { selector; props; base_class } ->
      Some
        ( `Starting,
          selector,
          props,
          order_of_base base_class selector,
          [],
          base_class )

(* Add index to each triple for stable sorting *)
let add_index triples =
  List.mapi
    (fun i (typ, sel, props, order, nested, base_class) ->
      {
        index = i;
        rule_type = typ;
        selector = sel;
        props;
        order;
        nested;
        base_class;
      })
    triples

(* Build hover media query block from hover rules *)
let hover_media_block hover_rules =
  let pairs =
    extract_selector_props_pairs hover_rules |> deduplicate_selector_props
  in
  if pairs = [] then None
  else
    let rules = of_grouped ~filter_custom_props:true pairs in
    Some (Css.media ~condition:"(hover:hover)" rules)

(* Convert selector/props pairs to CSS rules. *)
(* Internal: build rule sets from pre-extracted outputs. *)
let rule_sets_from_selector_props all_rules =
  let hover_rules, non_hover_rules = List.partition is_hover_rule all_rules in

  let css_statements =
    non_hover_rules
    |> List.filter_map rule_to_triple
    |> deduplicate_typed_triples |> add_index
    |> List.sort compare_indexed_rules
    |> List.map indexed_rule_to_statement
  in

  (* The sorted statements already have the correct interleaved order where
     low-priority utilities (like container p=0) and their media queries appear
     before higher-priority utilities. Insert hover media block at the end. *)
  match hover_media_block hover_rules with
  | None -> css_statements
  | Some hover_media -> css_statements @ [ hover_media ]

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

(* Check if declaration name is a default font family indirection *)
let is_default_family_name = function
  | "default-font-family" | "default-mono-font-family" -> true
  | _ -> false

(* Build set of declaration names for fast lookup *)
let names_set_of decls =
  List.fold_left
    (fun acc d ->
      match Css.custom_declaration_name d with
      | Some n -> Strings.add n acc
      | None -> acc)
    Strings.empty decls

(* Filter declarations whose names are not in the excluded set *)
let filter_non_duplicates excluded_names decls =
  List.filter
    (fun d ->
      match Css.custom_declaration_name d with
      | Some n -> not (Strings.mem n excluded_names)
      | None -> false)
    decls

(* Split defaults into pre (font families) and post (default-* indirections) *)
let split_defaults defaults =
  List.partition
    (fun decl ->
      match Css.custom_declaration_name decl with
      | Some n -> not (is_default_family_name n)
      | None -> false)
    defaults

(* Compare two order pairs *)
let compare_orders order_a order_b =
  match (order_a, order_b) with
  | Some (prio_a, sub_a), Some (prio_b, sub_b) ->
      let prio_cmp = Int.compare prio_a prio_b in
      if prio_cmp = 0 then Int.compare sub_a sub_b else prio_cmp
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> 0

(* Sort declarations by their Var order metadata *)
let sort_by_var_order decls =
  decls
  |> List.map (fun d -> (d, Var.order_of_declaration d))
  |> List.sort (fun (_, a) (_, b) -> compare_orders a b)
  |> List.map fst

(* Build theme layer rule from declarations *)
let theme_layer_rule = function
  | [] -> Css.v [ Css.layer ~name:"theme" [] ]
  | decls ->
      let selector = Css.Selector.(list [ Root; host () ]) in
      Css.v [ Css.layer ~name:"theme" [ Css.rule ~selector decls ] ]

(* Internal helper to compute theme layer from pre-extracted outputs. *)
let compute_theme_layer_from_selector_props ?(default_decls = []) selector_props
    =
  let extracted = extract_non_tw_custom_declarations selector_props in
  let pre_defaults, post_defaults = split_defaults default_decls in

  (* Filter defaults to remove duplicates of extracted vars *)
  let extracted_names = names_set_of extracted in
  let pre = filter_non_duplicates extracted_names pre_defaults in
  let post =
    filter_non_duplicates
      (Strings.union extracted_names (names_set_of pre))
      post_defaults
  in

  pre @ extracted @ post |> sort_by_var_order |> theme_layer_rule

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

(* Partition statements into @property rules and other statements *)
let partition_properties statements =
  List.partition
    (fun stmt ->
      match Css.as_property stmt with Some _ -> true | None -> false)
    statements

(* Deduplicate @property rules by name, preserving first occurrence *)
let dedup_properties property_rules =
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

(* Extract variable initial values from @property declarations *)
let initial_values_of property_rules =
  List.fold_left
    (fun acc stmt ->
      match Css.as_property stmt with
      | Some (Css.Property_info info as prop_info) ->
          let value = Var.property_initial_string prop_info in
          (info.name, value) :: acc
      | None -> acc)
    [] property_rules
  |> List.rev

(* Browser detection condition for properties layer *)
let browser_detection =
  "(((-webkit-hyphens:none)) and (not (margin-trim:inline))) or \
   ((-moz-orient:inline) and (not (color:rgb(from red r g b))))"

(* Build property layer content with browser detection *)
let property_layer_content initial_values other_statements =
  let selector = Css.Selector.(list [ universal; Before; After; Backdrop ]) in
  let initial_declarations =
    List.map
      (fun (name, value) -> Css.custom_property name value)
      initial_values
  in
  let rule = Css.rule ~selector initial_declarations in
  let supports_stmt = Css.supports ~condition:browser_detection [ rule ] in
  let layer_content = [ supports_stmt ] @ other_statements in
  Css.v [ Css.layer ~name:"properties" layer_content ]

(* Build the properties layer with browser detection for initial values *)
(* Returns (properties_layer, property_rules) - @property rules are separate *)
let build_properties_layer explicit_property_rules_statements =
  let property_rules, other_statements =
    partition_properties explicit_property_rules_statements
  in
  let deduplicated = dedup_properties property_rules in
  let initial_values = initial_values_of deduplicated in

  if deduplicated = [] && initial_values = [] then (Css.empty, [])
  else
    let layer = property_layer_content initial_values other_statements in
    (layer, deduplicated)

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

(* Filter variables that need @property rules *)
let vars_needing_property vars =
  List.filter (fun (Css.V v) -> Var.var_needs_property v) vars

(* Extract names from explicit @property rules into a set *)
let property_names_of statements =
  statements
  |> List.filter_map (fun stmt ->
         match Css.as_property stmt with
         | Some (Css.Property_info info) -> Some info.name
         | None -> None)
  |> List.fold_left (fun acc n -> Strings.add n acc) Strings.empty

(* Generate @property rules for variables not in explicit set *)
let property_rules_for vars excluded_names =
  vars
  |> List.filter (fun (Css.V v) ->
         let var_name = "--" ^ Css.var_name v in
         not (Strings.mem var_name excluded_names))
  |> List.map (fun (Css.V v) ->
         let var_name = "--" ^ Css.var_name v in
         Css.property ~name:var_name Css.Universal ~inherits:false ())

(** Collect all property rules: explicit ones and auto-generated ones *)
let collect_all_property_rules vars_from_utilities
    explicit_property_rules_statements =
  let needing_property = vars_needing_property vars_from_utilities in
  let explicit_names = property_names_of explicit_property_rules_statements in
  let generated_rules = property_rules_for needing_property explicit_names in
  let generated_statements =
    generated_rules |> List.concat_map Css.statements
  in
  explicit_property_rules_statements @ generated_statements

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

(* Extract variables and property rules from all utilities *)
let extract_vars_and_rules utilities =
  let styles = List.map Utility.to_style utilities in
  let results = List.map extract_vars_and_property_rules_from_style styles in
  let vars_list, prop_rules_list = List.split results in
  (List.concat vars_list, List.concat prop_rules_list)

(* Flatten property rules into CSS statements *)
let flatten_property_rules property_rules_lists =
  property_rules_lists |> List.concat_map Css.statements

(* Build individual CSS layers *)
let build_individual_layers selector_props all_property_statements statements =
  let theme_defaults = Typography.default_font_family_declarations in
  let theme_layer =
    compute_theme_layer_from_selector_props ~default_decls:theme_defaults
      selector_props
  in
  let base_layer = build_base_layer ~supports:placeholder_supports () in
  let properties_layer, property_rules =
    if all_property_statements = [] then (None, [])
    else
      let layer, prop_rules = build_properties_layer all_property_statements in
      if layer = Css.empty then (None, prop_rules) else (Some layer, prop_rules)
  in
  let utilities_layer = build_utilities_layer ~statements in
  { theme_layer; base_layer; properties_layer; utilities_layer; property_rules }

(** Build all CSS layers from utilities and rules *)
let build_layers ~include_base ~selector_props tw_classes statements =
  let vars_from_utilities, property_rules_lists =
    extract_vars_and_rules tw_classes
  in
  let explicit_property_rules = flatten_property_rules property_rules_lists in
  let all_property_statements =
    collect_all_property_rules vars_from_utilities explicit_property_rules
  in
  let layers =
    build_individual_layers selector_props all_property_statements statements
  in
  assemble_all_layers ~include_base ~properties_layer:layers.properties_layer
    ~theme_layer:layers.theme_layer ~base_layer:layers.base_layer
    ~utilities_layer:layers.utilities_layer
    ~property_rules_for_end:layers.property_rules

let wrap_css_items statements =
  (* For inline mode, just wrap the statements in a stylesheet *)
  Css.v statements

(* ======================================================================== *)
(* Main API - Convert Tw styles to CSS *)

(* ======================================================================== *)

type config = { base : bool; mode : Css.mode; optimize : bool }
(** Configuration for CSS generation *)

let default_config = { base = true; mode = Css.Variables; optimize = false }

let to_css ?(config = default_config) tw_classes =
  (* Extract once and share for rule sets and theme layer *)
  let selector_props = List.concat_map extract_selector_props tw_classes in

  let statements = rule_sets_from_selector_props selector_props in

  (* Generate layers whenever mode = Variables. Include the base layer only when
     [reset=true]. In Inline mode, emit raw rules without layers. *)
  let stylesheet =
    match config.mode with
    | Css.Variables ->
        let layers =
          build_layers ~include_base:config.base ~selector_props tw_classes
            statements
        in
        Css.concat layers
    | Css.Inline ->
        (* No layers - just raw utility rules with var() resolved to fallback
           values *)
        wrap_css_items statements
  in
  (* Apply optimization if requested *)
  if config.optimize then Css.optimize stylesheet else stylesheet

(* Recursively collect all declarations from a style *)
let rec collect_declarations acc = function
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
  | Style.Modified (_, t) -> collect_declarations acc t
  | Style.Group ts -> List.fold_left collect_declarations acc ts

(* Filter out CSS custom properties (variables) *)
let filter_non_variables decls =
  List.filter (fun decl -> Css.custom_declaration_name decl = None) decls

let to_inline_style utilities =
  let styles = List.map Utility.to_style utilities in
  let all_props = List.rev (List.fold_left collect_declarations [] styles) in
  let non_variable_props = filter_non_variables all_props in
  Css.inline_style_of_declarations non_variable_props
