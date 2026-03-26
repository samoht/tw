(** CSS layer building.

    Converts assembled, sorted CSS rules into CSS layer directives following
    Tailwind v4's architecture. *)

open Output

(* ======================================================================== *)
(* Conflict Resolution - Order utilities by specificity *)
(* ======================================================================== *)

(** Strip modifier prefixes (sm:, md:, hover:, etc.) to extract base utility
    name. Modifier prefixes come before the utility name. Colons inside bracket
    values (e.g., [family-name:var(...)]) are not modifier separators. *)
let extract_base_utility class_name_no_pseudo =
  let len = String.length class_name_no_pseudo in
  (* Find the last colon that is NOT inside brackets or parens *)
  let rec find_last_colon i bracket_depth paren_depth last_colon =
    if i >= len then last_colon
    else
      match class_name_no_pseudo.[i] with
      | '[' ->
          find_last_colon (i + 1) (bracket_depth + 1) paren_depth last_colon
      | ']' ->
          find_last_colon (i + 1)
            (max 0 (bracket_depth - 1))
            paren_depth last_colon
      | '(' ->
          find_last_colon (i + 1) bracket_depth (paren_depth + 1) last_colon
      | ')' ->
          find_last_colon (i + 1) bracket_depth
            (max 0 (paren_depth - 1))
            last_colon
      | ':' when bracket_depth = 0 && paren_depth = 0 ->
          find_last_colon (i + 1) bracket_depth paren_depth (Some i)
      | _ -> find_last_colon (i + 1) bracket_depth paren_depth last_colon
  in
  match find_last_colon 0 0 0 None with
  | Some colon_pos ->
      String.sub class_name_no_pseudo (colon_pos + 1) (len - colon_pos - 1)
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

(** Compute conflict resolution order from selector string using the AST. Parses
    the selector, finds the first class token (ignoring pseudo-tokens), strips
    modifier prefixes (e.g., "hover:"), and maps to Utility.order. Falls back to
    a default low priority when no class is found. *)
let conflict_order selector =
  let reader = Css.Reader.of_string selector in
  let sel = Css.Selector.read reader in
  match Css.Selector.first_class sel with
  | Some class_name -> class_name |> extract_base_utility |> parse_utility_order
  | None -> (9999, 0)

(* Extract selector and props pairs from Regular rules. *)
let selector_props_pairs rules =
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
    (* Then by suborder *)
    let sub_cmp = Int.compare sub1 sub2 in
    if sub_cmp <> 0 then sub_cmp
    else if
      filter_custom_props
      && is_simple_class_selector sel1
      && is_simple_class_selector sel2
    then
      (* Same priority/suborder: sort alphabetically for simple class selectors,
         then by original index for stability. *)
      let sel_cmp =
        String.compare
          (Css.Selector.to_string sel1)
          (Css.Selector.to_string sel2)
      in
      if sel_cmp <> 0 then sel_cmp else Int.compare i1 i2
    else Int.compare i1 i2

(* Convert selector/props/order triples to CSS rules with conflict ordering *)
(* Helper to filter custom properties for utilities layer *)
let should_keep_in_utilities decl =
  match Css.custom_declaration_layer decl with
  | Some layer when layer = "utilities" -> true
  | Some _ -> false
  | None -> (
      (* No fallback to name prefixes: keep only non-custom declarations when
         metadata is missing. *)
      match Css.custom_declaration_name decl with
      | None -> true
      | Some _ -> false)

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
        if filter_custom_props then List.filter should_keep_in_utilities props
        else props
      in
      Css.rule ~selector filtered_props)
    sorted_indexed

let rec filter_utility_properties props =
  List.filter_map
    (fun decl ->
      match Css.as_theme_guarded decl with
      | Some (var_name, inner) -> (
          let filtered = filter_utility_properties [ inner ] in
          match filtered with
          | [ d ] -> Some (Css.theme_guarded ~var_name d)
          | _ -> None)
      | None -> (
          match Css.custom_declaration_layer decl with
          | Some layer when layer = "utilities" -> Some decl
          | Some _ -> None
          | None -> (
              match Css.custom_declaration_name decl with
              | None -> Some decl
              | Some _ -> None)))
    props

(* Recursively filter theme declarations from nested statements *)
let rec filter_theme_from_statements statements =
  List.map
    (fun stmt ->
      match Css.as_declarations stmt with
      | Some decls ->
          (* Bare declarations block - filter theme properties *)
          let filtered_decls = filter_utility_properties decls in
          Css.declarations filtered_decls
      | None -> (
          match Css.as_rule stmt with
          | Some (selector, decls, nested) ->
              let filtered_decls = filter_utility_properties decls in
              let filtered_nested = filter_theme_from_statements nested in
              Css.rule ~selector ~nested:filtered_nested filtered_decls
          | None -> (
              match Css.as_media stmt with
              | Some (condition, content) ->
                  Css.media ~condition (filter_theme_from_statements content)
              | None -> (
                  match Css.as_layer stmt with
                  | Some (name, content) ->
                      Css.layer ?name (filter_theme_from_statements content)
                  | None -> (
                      match Css.as_container stmt with
                      | Some (name, condition, content) ->
                          Css.container ?name ~condition
                            (filter_theme_from_statements content)
                      | None -> stmt)))))
    statements

(* Compute merge key from a base class name as a fallback when the utility
   handler does not provide a typed merge_key via Style.t. For bracket
   utilities, strips both bracket content and opacity so that e.g.
   accent-[#0088cc]/50 and accent-[#0088cc]/[0.5] share key "accent-". For
   non-bracket utilities, strips opacity suffix so that e.g. outline-red-500/50
   and outline-red-500/[0.5] share key "outline-red-500". Handlers that need
   finer control (e.g. preventing merging for named bracket colors) should set
   merge_key via Style.t instead. *)
let merge_key_of_base_class base_class =
  match base_class with
  | None -> None
  | Some class_name ->
      let base = extract_base_utility class_name in
      let key =
        match String.index_opt base '[' with
        | Some bracket_pos ->
            let k = String.sub base 0 bracket_pos in
            (* Strip trailing / before [ so "bg-red-500/[50%]" and
               "bg-red-500/50" share the same key "bg-red-500" *)
            if String.ends_with ~suffix:"/" k then
              String.sub k 0 (String.length k - 1)
            else k
        | None -> (
            match String.index_opt base '/' with
            | Some slash_pos -> String.sub base 0 slash_pos
            | None -> base)
      in
      Some key

(* Convert indexed rule to CSS statement *)
let indexed_rule_to_statement (r : Sort.indexed_rule) =
  let filtered_props = filter_utility_properties r.props in
  let filtered_nested = filter_theme_from_statements r.nested in
  let merge_key =
    match r.merge_key with
    | Some _ as mk -> mk
    | None -> merge_key_of_base_class r.base_class
  in
  match r.rule_type with
  | `Regular ->
      Css.rule ~selector:r.selector ?merge_key ~nested:filtered_nested
        filtered_props
  | `Starting ->
      (* Wrap selector+declarations in @starting-style block
         (Tailwind-compatible format) *)
      Css.starting_style [ Css.rule ~selector:r.selector filtered_props ]
  | `Media condition ->
      (* For compound modifiers (e.g., dark:hover:), nested contains the inner
         media query. Otherwise, just emit a simple rule inside the media. *)
      if filtered_nested <> [] then
        (* Has nested statements (e.g., @media (hover:hover) { ... }) *)
        Css.media ~condition filtered_nested
      else
        Css.media ~condition
          [ Css.rule ~selector:r.selector ?merge_key filtered_props ]
  | `Container condition ->
      Css.container ~condition
        [ Css.rule ~selector:r.selector ?merge_key filtered_props ]
  | `Supports condition ->
      Css.supports ~condition
        [ Css.rule ~selector:r.selector ?merge_key filtered_props ]

(* Deduplicate typed triples while preserving first occurrence order *)
let deduplicate_typed_triples triples =
  let seen = Hashtbl.create (List.length triples) in
  let buf = Buffer.create 256 in
  List.filter
    (fun (typ, sel, props, _order, nested, _base_class, _merge_key, _not_order)
       ->
      Buffer.clear buf;
      Css.Selector.to_buffer buf sel;
      let key = (typ, Buffer.contents buf, props, nested) in
      if Hashtbl.mem seen key then false
      else (
        Hashtbl.add seen key ();
        true))
    triples

(* Get utility order from base class, with fallback to conflict order. Note:
   base_class may contain modifier prefixes (e.g., "md:grid-cols-2"), so we need
   to strip those before looking up the utility. Pseudo-element modifiers
   (before:, after:) use a fixed high suborder to preserve source order. *)
let order_of_base base_class selector =
  match base_class with
  | Some class_name -> (
      (* Check if this has a pseudo-element modifier prefix *)
      let has_pseudo_element_modifier =
        String.starts_with ~prefix:"before:" class_name
        || String.starts_with ~prefix:"after:" class_name
      in
      (* Strip modifier prefix to get base utility name *)
      let base_utility = extract_base_utility class_name in
      let parts = String.split_on_char '-' base_utility in
      match Utility.base_of_strings parts with
      | Ok u ->
          let prio, suborder = Utility.order u in
          if has_pseudo_element_modifier then
            (* Pseudo-element modifiers add 5000 to the base utility's suborder.
               This keeps them near their base utility but after all regular
               utilities, matching Tailwind v4 behavior where pseudo-elements
               appear late. *)
            (prio, suborder + 5000)
          else Utility.order u
      | Error _ -> conflict_order (Css.Selector.to_string selector))
  | None -> conflict_order (Css.Selector.to_string selector)

(* Adjust order with not-variant offset *)
let apply_not_order (prio, sub) not_order =
  if not_order = 0 then (prio, sub) else (prio, sub + not_order)

(* Convert each rule type to typed triple *)
let triple typ ~selector ~props ~order ~nested ~base_class ~merge_key ~not_order
    =
  Some (typ, selector, props, order, nested, base_class, merge_key, not_order)

let rule_to_triple = function
  | Regular
      { selector; props; base_class; nested; has_hover; merge_key; not_order }
    ->
      let order =
        apply_not_order (order_of_base base_class selector) not_order
      in
      let typ = if has_hover then `Media Css.Media.Hover else `Regular in
      triple typ ~selector ~props ~order ~nested ~base_class ~merge_key
        ~not_order
  | Media_query { condition; selector; props; base_class; nested; not_order } ->
      let order =
        apply_not_order (order_of_base base_class selector) not_order
      in
      triple (`Media condition) ~selector ~props ~order ~nested ~base_class
        ~merge_key:None ~not_order
  | Container_query { condition; selector; props; base_class } ->
      triple (`Container condition) ~selector ~props
        ~order:(order_of_base base_class selector)
        ~nested:[] ~base_class ~merge_key:None ~not_order:0
  | Starting_style { selector; props; base_class } ->
      triple `Starting ~selector ~props
        ~order:(order_of_base base_class selector)
        ~nested:[] ~base_class ~merge_key:None ~not_order:0
  | Supports_query
      { condition; selector; props; base_class; merge_key; not_order } ->
      let order =
        apply_not_order (order_of_base base_class selector) not_order
      in
      triple (`Supports condition) ~selector ~props ~order ~nested:[]
        ~base_class ~merge_key ~not_order

(* Add index to each triple for stable sorting *)
let add_index triples =
  let buf = Buffer.create 256 in
  List.mapi
    (fun i (typ, sel, props, order, nested, base_class, merge_key, not_order) ->
      Buffer.clear buf;
      Css.Selector.to_buffer buf sel;
      let selector_str = Buffer.contents buf in
      ({
         index = i;
         rule_type = typ;
         selector = sel;
         selector_str;
         selector_kind = Sort.classify_selector sel;
         has_modifier_colon = Css.Selector.contains_modifier_colon sel;
         props;
         order;
         nested;
         base_class;
         merge_key;
         not_order;
         variant_order = Rule.compute_variant_order base_class sel;
       }
        : Sort.indexed_rule))
    triples

(* Convert selector/props pairs to CSS rules. *)
(* Internal: build rule sets from pre-extracted outputs. *)
let rule_sets_from_selector_props all_rules =
  (* All rules (including hover) are now sorted together. Hover rules are
     converted to Media "(hover:hover)" rules in rule_to_triple, so they
     participate in the normal media query sorting. *)
  let indexed =
    all_rules
    |> List.filter_map rule_to_triple
    |> deduplicate_typed_triples |> add_index
  in
  let sorted = List.sort Sort.compare_indexed_rules indexed in
  if Sort.debug_compare_enabled () then
    List.iter
      (fun (r : Sort.indexed_rule) ->
        Printf.eprintf "SORTED: vo=%d base=%s type=%s nested=%d\n"
          r.variant_order
          (match r.base_class with Some s -> s | None -> "<none>")
          (match r.rule_type with
          | `Regular -> "R"
          | `Media m -> "M:" ^ Css.Media.to_string m
          | `Container _ -> "C"
          | `Starting -> "S"
          | `Supports _ -> "U")
          (List.length r.nested))
      sorted;
  sorted |> List.map indexed_rule_to_statement

let utilities_layer ~layers ~statements =
  (* Statements are already in the correct order with media queries interleaved.
     Consecutive media queries with the same condition will be merged by the
     optimizer (css/optimize.ml) while preserving cascade order. *)
  if layers then Css.v [ Css.layer ~name:"utilities" statements ]
  else Css.v statements

(* Get sorted indexed rules - used for extracting first-usage order of
   variables *)
let sorted_indexed_rules all_rules =
  all_rules
  |> List.filter_map rule_to_triple
  |> deduplicate_typed_triples |> add_index
  |> List.sort Sort.compare_indexed_rules

(* Sort var names by property_order. Names include -- prefix. *)
let sort_vars_by_property_order vars =
  let get_order name =
    (* Strip -- prefix for lookup *)
    let name_without_prefix =
      if String.starts_with ~prefix:"--" name then
        String.sub name 2 (String.length name - 2)
      else name
    in
    match Var.property_order name_without_prefix with
    | Some o -> o
    | None -> 1000 (* Default for vars without property_order *)
  in
  List.sort (fun n1 n2 -> compare (get_order n1) (get_order n2)) vars

(* Extract all var names from sorted indexed rules in utility order. For each
   utility, collects: 1. Vars that are SET (custom declarations) 2. Vars that
   are REFERENCED and need @property (e.g., transform refs rotate/skew) Within
   each utility, vars are sorted by property_order to ensure consistent family
   ordering (e.g., ring before inset-ring regardless of CSS value order). *)
let var_names_of_sorted_rules sorted_rules =
  sorted_rules
  |> List.concat_map (fun (r : Sort.indexed_rule) ->
      (* Vars that this utility SETS *)
      let filtered = filter_utility_properties r.props in
      let set_vars = Css.custom_prop_names filtered in
      (* Vars that this utility REFERENCES and need @property *)
      let all_vars = Css.vars_of_declarations r.props in
      let ref_vars =
        all_vars
        |> List.filter (fun (Css.V v) ->
            let name = Css.var_name v in
            Var.needs_property name)
        |> List.map (fun (Css.V v) -> "--" ^ Css.var_name v)
      in
      (* Sort all vars from this utility by property_order *)
      sort_vars_by_property_order (set_vars @ ref_vars))

let rule_sets tw_classes =
  let all_rules = tw_classes |> List.concat_map Rule.outputs in
  rule_sets_from_selector_props all_rules

(* ======================================================================== *)
(* Layer Generation - CSS @layer directives and theme variable resolution *)
(* ======================================================================== *)

module Strings = Set.Make (String)

(* Helpers for theme layer extraction and ordering *)
let collect_selector_props tw_classes = List.concat_map Rule.outputs tw_classes

(* Helper to extract theme declarations from nested CSS statements
   recursively *)
let rec extract_theme_from_statements theme_vars insertion_order statements =
  List.iter
    (fun stmt ->
      (* Check if this is a rule with declarations *)
      (match Css.statement_declarations stmt with
      | Some props ->
          Css.custom_declarations ~layer:"theme" props
          |> List.iter (fun decl ->
              match Css.custom_declaration_name decl with
              | Some name when not (Hashtbl.mem theme_vars name) ->
                  Hashtbl.add theme_vars name decl;
                  insertion_order := decl :: !insertion_order
              | _ -> ())
      | None -> ());
      (* Recurse into nested statements *)
      (match Css.as_rule stmt with
      | Some (_, _, nested) ->
          extract_theme_from_statements theme_vars insertion_order nested
      | None -> ());
      (match Css.as_media stmt with
      | Some (_, content) ->
          extract_theme_from_statements theme_vars insertion_order content
      | None -> ());
      (match Css.as_layer stmt with
      | Some (_, content) ->
          extract_theme_from_statements theme_vars insertion_order content
      | None -> ());
      match Css.as_container stmt with
      | Some (_, _, content) ->
          extract_theme_from_statements theme_vars insertion_order content
      | None -> ())
    statements

let extract_non_tw_custom_declarations selector_props =
  (* Use Hashtbl to collect unique theme variables efficiently *)
  let theme_vars = Hashtbl.create 32 in
  let insertion_order = ref [] in

  selector_props
  |> List.iter (function
    | Regular { props; nested; _ } ->
        (* Extract from top-level props *)
        Css.custom_declarations ~layer:"theme" props
        |> List.iter (fun decl ->
            match Css.custom_declaration_name decl with
            | Some name when not (Hashtbl.mem theme_vars name) ->
                Hashtbl.add theme_vars name decl;
                insertion_order := decl :: !insertion_order
            | _ -> ());
        (* Extract from nested statements *)
        extract_theme_from_statements theme_vars insertion_order nested
    | Media_query { props; _ }
    | Container_query { props; _ }
    | Starting_style { props; _ }
    | Supports_query { props; _ } ->
        Css.custom_declarations ~layer:"theme" props
        |> List.iter (fun decl ->
            match Css.custom_declaration_name decl with
            | Some name when not (Hashtbl.mem theme_vars name) ->
                Hashtbl.add theme_vars name decl;
                insertion_order := decl :: !insertion_order
            | _ -> ()));
  (* Return in original insertion order *)
  List.rev !insertion_order

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

(* Sort declarations by their Var order metadata, alphabetical fallback *)
let sort_by_var_order decls =
  decls
  |> List.map (fun d ->
      (d, Var.order_of_declaration d, Css.custom_declaration_name d))
  |> List.sort (fun (_, a, na) (_, b, nb) ->
      let c = compare_orders a b in
      if c <> 0 then c else compare na nb)
  |> List.map (fun (d, _, _) -> d)

(* Build theme layer rule from declarations *)
let theme_layer_rule ~layers = function
  | [] -> if layers then Css.v [ Css.layer ~name:"theme" [] ] else Css.empty
  | decls ->
      let selector = Css.Selector.(list [ Root; host () ]) in
      let rule = Css.rule ~selector decls in
      if layers then Css.v [ Css.layer ~name:"theme" [ rule ] ]
      else Css.v [ rule ]

(* Internal helper to compute theme layer from pre-extracted outputs. *)
let theme_layer_of_props ?(layers = true) ?(default_decls = []) selector_props =
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

  pre @ extracted @ post |> sort_by_var_order |> theme_layer_rule ~layers

let theme_layer_of ?(default_decls = []) tw_classes =
  let selector_props = collect_selector_props tw_classes in
  theme_layer_of_props ~default_decls selector_props

let placeholder_supports =
  let placeholder = Css.Selector.Placeholder in

  (* Create the inner @supports for modern browsers *)
  let modern_rule =
    Css.rule ~selector:placeholder
      [
        Css.color
          (Css.color_mix ~in_space:Oklab ~percent1:50. Current Transparent);
      ]
  in
  let modern_support_stmt =
    Css.supports
      ~condition:
        (Css.Supports.Property ("color", "color-mix(in lab, red, red)"))
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
          (Css.Supports.Or
             ( Css.Supports.Not
                 (Css.Supports.Property
                    ("-webkit-appearance", "-apple-pay-button")),
               Css.Supports.Property ("contain-intrinsic-size", "1px") ))
        outer_support_content;
    ]

let base_layer ?supports ?(forms_base = false) () =
  let preflight =
    Preflight.stylesheet ?placeholder_supports:supports ~forms:forms_base ()
  in
  let base =
    if forms_base then Css.concat [ preflight; Forms.base_stylesheet () ]
    else preflight
  in
  Css.layer_of ~name:"base" base

(* Use the centralized conversion function from Var module *)

(* Property helpers are centralized in Property module *)
let partition_properties = Property.split
let dedup_properties = Property.dedup
let initial_values_of = Property.initial_values

(* Browser detection condition for properties layer. Detects browsers that need
   @property fallbacks: Safari <15.4 or Firefox <128. *)
let browser_detection =
  let open Css.Supports in
  Or
    ( And
        ( Property ("-webkit-hyphens", "none"),
          Not (Property ("margin-trim", "inline")) ),
      And
        ( Property ("-moz-orient", "inline"),
          Not (Property ("color", "rgb(from red r g b)")) ) )

(* Build a mapping from property names to their first-usage index. Tailwind
   orders properties in @supports and @property by first usage order in the
   sorted utilities output. Names already include -- prefix. *)
let first_usage_order set_var_names =
  let seen = Hashtbl.create 16 in
  let idx = ref 0 in
  List.iter
    (fun name ->
      (* Names from custom_prop_names already include -- prefix *)
      if not (Hashtbl.mem seen name) then (
        Hashtbl.add seen name !idx;
        incr idx))
    set_var_names;
  seen

(* Get property order from static registry. *)
let property_order_from name =
  match Var.property_order name with
  | Some o -> o
  | None ->
      failwith
        ("Missing property_order for variable '" ^ name
       ^ "'. Register ~property_order when defining the variable \
          (Var.channel/property_default).")

(* Build family first-usage order from the first_usage_order hashtbl. Returns a
   hashtbl mapping family to its first occurrence index. *)
let family_order first_usage_order =
  let family_order = Hashtbl.create 16 in
  Hashtbl.iter
    (fun name idx ->
      match Var.family name with
      | Some fam -> (
          match Hashtbl.find_opt family_order fam with
          | None -> Hashtbl.add family_order fam idx
          | Some existing ->
              if idx < existing then Hashtbl.replace family_order fam idx)
      | None -> ())
    first_usage_order;
  family_order

let gradient_family_index n =
  if not (String.starts_with ~prefix:"--tw-gradient-" n) then 100
  else
    match n with
    | "--tw-gradient-position" -> 0
    | "--tw-gradient-from" -> 1
    | "--tw-gradient-via" -> 2
    | "--tw-gradient-to" -> 3
    | "--tw-gradient-stops" -> 4
    | "--tw-gradient-via-stops" -> 5
    | "--tw-gradient-from-position" -> 6
    | "--tw-gradient-via-position" -> 7
    | "--tw-gradient-to-position" -> 8
    | _ -> 100

let uses_direct_property_order = function
  | Some
      ( `Gradient | `Translate | `Rotate | `Skew | `Scale | `Duration
      | `Font_weight | `Leading ) ->
      false
      (* Transforms, gradient, duration, and typography use first-usage order *)
  | Some _ -> true (* All other named families use property_order directly *)
  | None -> false
(* Variables without families (e.g. --tw-ease) are NOT direct; get_family_order
   returns 1000 for None, placing them last *)

let compare_property_vars ~get_family_order ~get_first_usage n1 n2 po1 po2 fam1
    fam2 =
  (* Variables with negative property_order and no family come FIRST *)
  match (fam1, po1 < 0, fam2, po2 < 0) with
  | None, true, None, true -> compare po1 po2
  | None, true, _, _ -> -1
  | _, _, None, true -> 1
  | Some `Gradient, _, Some `Gradient, _ ->
      compare (gradient_family_index n1) (gradient_family_index n2)
  | _ when uses_direct_property_order fam1 && uses_direct_property_order fam2 ->
      if fam1 = fam2 && fam1 = Some `Border then
        (* Border family: use first-usage order to match Tailwind's per-utility
           declaration ordering (divide-x emits x-reverse then border-style,
           divide-y emits y-reverse, so the combined order interleaves them). *)
        compare (get_first_usage n1) (get_first_usage n2)
      else
        (* All other families (Ring, Inset_ring, Shadow, etc.) and cross-family
           comparisons use property_order for the carefully chosen ordering. *)
        compare po1 po2
  | _ ->
      let fo1 = get_family_order n1 in
      let fo2 = get_family_order n2 in
      if fo1 <> fo2 then compare fo1 fo2 else compare po1 po2

let sort_properties_by_order first_usage_order initial_values =
  let family_order = family_order first_usage_order in
  let get_family_order name =
    match Var.family name with
    | Some fam -> (
        match Hashtbl.find_opt family_order fam with
        | Some o -> o
        | None -> 1000)
    | None -> 1000
  in
  let get_first_usage name =
    match Hashtbl.find_opt first_usage_order name with
    | Some idx -> idx
    | None -> 10000
  in
  let cmp (n1, _) (n2, _) =
    let fam1 = Var.family n1 in
    let fam2 = Var.family n2 in
    let po1 = property_order_from n1 in
    let po2 = property_order_from n2 in
    compare_property_vars ~get_family_order ~get_first_usage n1 n2 po1 po2 fam1
      fam2
  in
  List.sort cmp initial_values

(* Build property layer content with browser detection *)
let property_layer_content first_usage_order initial_values other_statements =
  let selector = Css.Selector.(list [ universal; Before; After; Backdrop ]) in
  let sorted_values =
    sort_properties_by_order first_usage_order initial_values
  in
  let initial_declarations =
    List.map (fun (name, value) -> Css.custom_property name value) sorted_values
  in
  let rule = Css.rule ~selector initial_declarations in
  let supports_stmt = Css.supports ~condition:browser_detection [ rule ] in
  let layer_content = [ supports_stmt ] @ other_statements in
  Css.v [ Css.layer ~name:"properties" layer_content ]

(* Build the properties layer with browser detection for initial values *)
(* Returns (properties_layer, property_rules) - @property rules are separate *)
let properties_layer first_usage_order explicit_property_rules_statements =
  let property_rules, other_statements =
    partition_properties explicit_property_rules_statements
  in
  let deduplicated = dedup_properties property_rules in
  let initial_values = initial_values_of deduplicated in

  if deduplicated = [] && initial_values = [] then (Css.empty, [])
  else
    let layer =
      property_layer_content first_usage_order initial_values other_statements
    in
    (layer, deduplicated)

(** Extract SET variable names from Custom_declarations *)
let set_var_names_from_props props = Css.custom_prop_names props

(** Extract variables and property rules from utility styles recursively.
    Returns (all_vars, set_var_names, property_rules) where:
    - all_vars: all referenced variables (for theme layer)
    - set_var_names: names of variables that are SET via Custom_declaration
    - property_rules: explicit property rules from utilities *)
let rec extract_style_vars_and_rules = function
  | Style.Style { props; rules; property_rules; _ } ->
      let vars_from_props = Css.vars_of_declarations props in
      let vars_from_rules =
        match rules with Some r -> Css.vars_of_rules r | None -> []
      in
      let set_names = set_var_names_from_props props in
      (vars_from_props @ vars_from_rules, set_names, [ property_rules ])
  | Style.Modified (_, t) -> extract_style_vars_and_rules t
  | Style.Group ts ->
      let results = List.map extract_style_vars_and_rules ts in
      let vars_list, set_names_list, prop_rules_list =
        List.fold_right
          (fun (v, s, p) (vs, ss, ps) -> (v :: vs, s :: ss, p :: ps))
          results ([], [], [])
      in
      ( List.concat vars_list,
        List.concat set_names_list,
        List.concat prop_rules_list )

(* Filter variables that need @property rules *)
let vars_needing_property vars =
  List.filter (fun (Css.V v) -> Var.needs_property_rule v) vars

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

(** Collect all property rules: explicit ones and auto-generated ones. Only
    auto-generates [\@property] for variables that are: 1. Actually SET (via
    Custom_declaration) in the utilities 2. Have needs_property=true in their
    metadata *)
let collect_all_property_rules vars_from_utilities set_var_names
    explicit_property_rules_statements =
  let set_names_set =
    List.fold_left (fun acc n -> Strings.add n acc) Strings.empty set_var_names
  in
  (* Filter to only vars that are SET, not just referenced *)
  let needing_property =
    vars_needing_property vars_from_utilities
    |> List.filter (fun (Css.V v) ->
        let var_name = "--" ^ Css.var_name v in
        Strings.mem var_name set_names_set)
  in
  let explicit_names = property_names_of explicit_property_rules_statements in
  let generated_rules = property_rules_for needing_property explicit_names in
  let generated_statements =
    generated_rules |> List.concat_map Css.statements
  in
  explicit_property_rules_statements @ generated_statements

(** Build layer declaration list based on which layers are present *)
let layer_declaration ~has_properties ~include_base =
  let names =
    (if has_properties then [ "properties" ] else [])
    @
    if include_base then [ "theme"; "base"; "components"; "utilities" ]
    else [ "theme"; "components"; "utilities" ]
  in
  Css.v [ Css.layer_decl names ]

(** Sort [@property] rules using first-usage order.
    Variables are ordered by when they first appear across all utilities.
    For variables within the same family that both use direct property_order,
    first-usage order is used as primary sort key (this matches Tailwind's
    behavior where per-utility declaration order determines @property order).
    Falls back to family order then property_order for cross-family sorting. *)
let sort_property_rules_by_usage first_usage_order property_rules_for_end =
  let family_order = family_order first_usage_order in
  let get_family_order name =
    match Var.family name with
    | Some fam -> (
        match Hashtbl.find_opt family_order fam with
        | Some o -> o
        | None -> 1000)
    | None -> 1000
  in
  let get_first_usage name =
    match Hashtbl.find_opt first_usage_order name with
    | Some idx -> idx
    | None -> 10000
  in
  property_rules_for_end
  |> List.sort (fun s1 s2 ->
      match (Css.as_property s1, Css.as_property s2) with
      | ( Some (Css.Property_info { name = n1; _ }),
          Some (Css.Property_info { name = n2; _ }) ) ->
          let fam1 = Var.family n1 in
          let fam2 = Var.family n2 in
          let po1 = property_order_from n1 in
          let po2 = property_order_from n2 in
          (* Variables with no family and negative property_order (e.g.
             --tw-space-x-reverse) always come before family variables *)
          let no_family_negative_first =
            match (fam1, fam2) with
            | None, Some _ when po1 < 0 -> -1
            | Some _, None when po2 < 0 -> 1
            | _ -> 0
          in
          if no_family_negative_first <> 0 then no_family_negative_first
          else if
            uses_direct_property_order fam1 && uses_direct_property_order fam2
          then
            if fam1 = fam2 && fam1 = Some `Border then
              (* Border family: use first-usage order to match Tailwind's
                 per-utility declaration ordering. *)
              compare (get_first_usage n1) (get_first_usage n2)
            else
              (* All other families and cross-family: use property_order. *)
              compare po1 po2
          else
            let fo1 = get_family_order n1 in
            let fo2 = get_family_order n2 in
            if fo1 <> fo2 then compare fo1 fo2 else compare po1 po2
      | _ -> 0)

(** Deduplicate keyframes by name, keeping first occurrence, then convert to CSS
    statements *)
let dedup_keyframes_to_css keyframes =
  let seen = Hashtbl.create 8 in
  let deduped =
    List.filter
      (fun (name, _) ->
        if Hashtbl.mem seen name then false
        else (
          Hashtbl.add seen name ();
          true))
      keyframes
  in
  let stmts =
    List.map (fun (name, frames) -> Css.keyframes name frames) deduped
  in
  if stmts = [] then [] else [ Css.v stmts ]

(** Assemble all CSS layers in the correct order *)
let assemble_all_layers ~layers ~include_base ~properties_layer ~theme_layer
    ~base_layer ~utilities_layer ~property_rules_for_end ~keyframes
    ~first_usage_order =
  let base_layers =
    if include_base then [ theme_layer; base_layer ] else [ theme_layer ]
  in
  let initial_layers =
    match properties_layer with None -> [] | Some l -> [ l ]
  in
  let layers_without_property =
    if layers then
      let components_declaration = Css.v [ Css.layer_decl [ "components" ] ] in
      let layer_names =
        layer_declaration
          ~has_properties:(Option.is_some properties_layer)
          ~include_base
      in
      [ layer_names ] @ initial_layers @ base_layers
      @ [ components_declaration; utilities_layer ]
    else initial_layers @ base_layers @ [ utilities_layer ]
  in
  let sorted_property_rules =
    sort_property_rules_by_usage first_usage_order property_rules_for_end
  in
  let property_rules_css =
    if sorted_property_rules = [] then [] else [ Css.v sorted_property_rules ]
  in
  let keyframes_css = dedup_keyframes_to_css keyframes in
  layers_without_property @ property_rules_css @ keyframes_css

(* Extract variables, set var names, and property rules from all utilities *)
let extract_vars_and_rules utilities =
  let styles = List.map Utility.to_style utilities in
  let results = List.map extract_style_vars_and_rules styles in
  let vars_list, set_names_list, prop_rules_list =
    List.fold_right
      (fun (v, s, p) (vs, ss, ps) -> (v :: vs, s :: ss, p :: ps))
      results ([], [], [])
  in
  ( List.concat vars_list,
    List.concat set_names_list,
    List.concat prop_rules_list )

(* Flatten property rules into CSS statements *)
let flatten_property_rules property_rules_lists =
  property_rules_lists |> List.concat_map Css.statements

(* Build individual CSS layers *)
(* Detect if forms utilities are used - triggers including forms base layer *)
let has_forms_utilities tw_classes =
  let rec check_utility = function
    | Utility.Base u ->
        let name = Utility.name_of_base u in
        name = "forms" || name = "forms_select"
    | Utility.Modified (_, u) -> check_utility u
    | Utility.Group us -> List.exists check_utility us
  in
  List.exists check_utility tw_classes

(* Detect if before/after pseudo-elements are used - triggers content var
   property rule *)
let has_pseudo_elements tw_classes =
  let rec has_pseudo = function
    | Style.Pseudo_before | Style.Pseudo_after -> true
    | _ -> false
  and check_utility = function
    | Utility.Base _ -> false
    | Utility.Modified (modifier, u) -> has_pseudo modifier || check_utility u
    | Utility.Group us -> List.exists check_utility us
  in
  List.exists check_utility tw_classes

let has_transition_utility selector_props =
  List.exists
    (fun r ->
      let bc =
        match r with
        | Regular { base_class; _ }
        | Media_query { base_class; _ }
        | Container_query { base_class; _ }
        | Starting_style { base_class; _ }
        | Supports_query { base_class; _ } ->
            base_class
      in
      match bc with
      | Some c ->
          String.length c >= 10
          && String.sub c 0 10 = "transition"
          && c <> "transition-none"
      | None -> false)
    selector_props

(* Result of building individual layers *)
type layers_result = {
  theme_layer : Css.t;
  base_layer : Css.t;
  properties_layer : Css.t option;
  utilities_layer : Css.t;
  property_rules : Css.statement list;
}

let individual_layers ~layers ~include_base ~forms_base first_usage_order
    selector_props all_property_statements statements =
  let theme_defaults =
    let font_defaults =
      if include_base then Typography.default_font_family_declarations else []
    in
    let transition_defaults =
      if include_base && has_transition_utility selector_props then
        Transitions.default_transition_declarations
      else []
    in
    font_defaults @ transition_defaults
  in
  let theme_layer =
    theme_layer_of_props ~layers ~default_decls:theme_defaults selector_props
  in
  let base_layer = base_layer ~supports:placeholder_supports ~forms_base () in
  let properties_layer, property_rules =
    if all_property_statements = [] then (None, [])
    else
      let layer, prop_rules =
        properties_layer first_usage_order all_property_statements
      in
      if layer = Css.empty then (None, prop_rules) else (Some layer, prop_rules)
  in
  let utilities_layer = utilities_layer ~layers ~statements in
  { theme_layer; base_layer; properties_layer; utilities_layer; property_rules }

(* Extract @keyframes from Style.rules *)
let rec collect_keyframes acc = function
  | Style.Style { rules = Some rs; _ } ->
      List.fold_left
        (fun acc stmt ->
          match Css.as_keyframes stmt with
          | Some (name, frames) -> (name, frames) :: acc
          | None -> acc)
        acc rs
  | Style.Style { rules = None; _ } -> acc
  | Style.Modified (_, t) -> collect_keyframes acc t
  | Style.Group ts -> List.fold_left collect_keyframes acc ts

(** Sort keyframes by their associated theme variable order. Keyframes like
    "spin"/"pulse"/"bounce" are associated with theme variables
    "animate-spin"/"animate-pulse"/"animate-bounce" that have explicit
    (priority, suborder) tuples registered. *)
let sort_keyframes_by_var_order keyframes =
  keyframes
  |> List.sort (fun (name1, _) (name2, _) ->
      let keyframe_var_order name =
        match Var.order ("animate-" ^ name) with
        | Some (p, s) -> (p * 1000) + s
        | None -> 1000000 (* Unknown keyframes sort last *)
      in
      let order_cmp =
        Int.compare (keyframe_var_order name1) (keyframe_var_order name2)
      in
      if order_cmp <> 0 then order_cmp
      else String.compare name1 name2 (* Stable sort for same order *))

(** Build all CSS layers from utilities and rules *)
let layers ~layers ~include_base ?forms ~selector_props tw_classes statements =
  let styles = List.map Utility.to_style tw_classes in
  let vars_from_utilities, set_var_names, property_rules_lists =
    extract_vars_and_rules tw_classes
  in
  (* Get sorted indexed_rules to extract first-usage order from sorted output *)
  let sorted_rules = sorted_indexed_rules selector_props in
  (* Build first-usage order from ALL vars per utility in utility order. For
     each utility, collects SET vars then REFERENCED vars needing @property.
     Within each utility, vars are sorted by property_order (done in
     var_names_of_sorted_rules). Across utilities, we preserve first-usage order
     to match Tailwind's behavior. *)
  let all_vars = var_names_of_sorted_rules sorted_rules in
  let first_usage_order = first_usage_order all_vars in
  let base_property_rules = flatten_property_rules property_rules_lists in
  (* Add content_var's property_rule if before/after pseudo-elements are used *)
  let explicit_property_rules =
    if has_pseudo_elements tw_classes then
      let content_property_rule =
        Var.property_rules Typography.content_var |> Css.statements
      in
      base_property_rules @ content_property_rule
    else base_property_rules
  in
  let all_property_statements =
    collect_all_property_rules vars_from_utilities set_var_names
      explicit_property_rules
  in
  (* Use explicit forms flag if provided, otherwise auto-detect from
     utilities *)
  let forms_base =
    match forms with Some f -> f | None -> has_forms_utilities tw_classes
  in
  let individual =
    individual_layers ~layers ~include_base ~forms_base first_usage_order
      selector_props all_property_statements statements
  in
  let keyframes =
    List.fold_left collect_keyframes [] styles
    |> List.rev |> sort_keyframes_by_var_order
  in
  assemble_all_layers ~layers ~include_base
    ~properties_layer:individual.properties_layer
    ~theme_layer:individual.theme_layer ~base_layer:individual.base_layer
    ~utilities_layer:individual.utilities_layer
    ~property_rules_for_end:individual.property_rules ~keyframes
    ~first_usage_order

(* ======================================================================== *)
(* CSS Generation API *)
(* ======================================================================== *)

type config = {
  base : bool;
  forms : bool option;
  mode : Css.mode;
  layers : bool;
  optimize : bool;
}

let default_config =
  {
    base = true;
    forms = None;
    mode = Css.Variables;
    layers = true;
    optimize = false;
  }

let to_css ?(config = default_config) tw_classes =
  let selector_props = List.concat_map Rule.outputs tw_classes in
  let statements = rule_sets_from_selector_props selector_props in
  let stylesheet =
    match config.mode with
    | Css.Variables ->
        let layer_results =
          layers ~layers:config.layers ~include_base:config.base
            ?forms:config.forms ~selector_props tw_classes statements
        in
        Css.concat layer_results
    | Css.Inline -> Css.v statements
  in
  if config.optimize then Css.optimize stylesheet else stylesheet

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

let to_inline_style utilities =
  let styles = List.map Utility.to_style utilities in
  let all_props = List.rev (List.fold_left collect_declarations [] styles) in
  let non_vars =
    List.filter (fun d -> Css.custom_declaration_name d = None) all_props
  in
  Css.inline_style_of_declarations non_vars
