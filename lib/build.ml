(** CSS layer building.

    Converts assembled, sorted CSS rules into CSS layer directives following
    Tailwind v4's architecture. *)

module Css = Cascade.Css
open Output
module Metadata = Map.Make (String)

let metadata_name name =
  if String.starts_with ~prefix:"--" name then
    String.sub name 2 (String.length name - 2)
  else name

let metadata_score metadata =
  Option.fold ~none:0 ~some:(fun _ -> 1) (Var.metadata_order metadata)
  + Option.fold ~none:0
      ~some:(fun _ -> 1)
      (Var.metadata_property_order metadata)
  + Option.fold ~none:0 ~some:(fun _ -> 1) (Var.metadata_family metadata)
  + (if Var.metadata_needs_property metadata then 1 else 0)
  + Option.fold ~none:0 ~some:(fun _ -> 1) (Var.metadata_default_css metadata)

let add_metadata index metadata =
  let name = Var.metadata_name metadata in
  match Metadata.find_opt name index with
  | Some existing when metadata_score existing >= metadata_score metadata ->
      index
  | _ -> Metadata.add name metadata index

let add_var_metadata index (Css.V var) =
  match Var.metadata_of_var var with
  | Some metadata -> add_metadata index metadata
  | None -> index

let add_declaration_metadata index declaration =
  let index =
    match Var.metadata_of_declaration declaration with
    | Some metadata -> add_metadata index metadata
    | None -> index
  in
  List.fold_left add_var_metadata index
    (Css.vars_of_declarations [ declaration ])

let add_declarations_metadata index declarations =
  List.fold_left add_declaration_metadata index declarations

let add_vars_metadata index vars = List.fold_left add_var_metadata index vars

let metadata_of_sorted_rules rules =
  List.fold_left
    (fun index (rule : Sort.indexed_rule) ->
      let index = add_declarations_metadata index rule.props in
      add_vars_metadata index (Css.vars_of_rules rule.nested))
    Metadata.empty rules

let metadata_for_name index name = Metadata.find_opt (metadata_name name) index

let metadata_order index name =
  Option.bind (metadata_for_name index name) Var.metadata_order

let metadata_property_order index name =
  Option.bind (metadata_for_name index name) Var.metadata_property_order

let metadata_family index name =
  Option.bind (metadata_for_name index name) Var.metadata_family

let metadata_index sorted_rules style_metadata =
  List.fold_left add_metadata
    (metadata_of_sorted_rules sorted_rules)
    style_metadata

(* ======================================================================== *)
(* Conflict Resolution - Order utilities by specificity *)
(* ======================================================================== *)

(** Strip modifier prefixes (sm:, md:, hover:, etc.) to extract base utility
    name. Modifier prefixes come before the utility name. Colons inside bracket
    values (e.g., [family-name:var(...)]) are not modifier separators. *)
let extract_base_utility class_name_no_pseudo =
  let base =
    match List.rev (Parse.split_on_colon class_name_no_pseudo) with
    | last :: _ -> last
    | [] -> class_name_no_pseudo
  in
  (* Strip a leading [!] important marker so [!flex] orders like [flex] rather
     than failing to parse and falling to the default (last) order. *)
  if String.length base > 0 && base.[0] = '!' then
    String.sub base 1 (String.length base - 1)
  else base

(** Parse utility and get ordering, with fallback for non-utility classes *)
let parse_utility_order base_utility =
  match Utility.base_of_class Scheme.default base_utility with
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
  let sel = Css.Selector.read (Cascade.Cursor.of_string selector) in
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
                match Utility.base_of_class Scheme.default class_name with
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
(* Theme tokens belong in the theme layer, so a utility rule that declares one
   to make its own value available has it stripped here. [Css.map] reaches every
   rule at any depth, [@starting-style] included, which the walk this replaces
   did not descend into: [md:starting:p-4] declared [--spacing] inside the rule
   as well as in the theme layer. A bare declarations block carries no selector,
   so [Css.map] does not see one and the top level is filtered before it. *)
let filter_theme_from_statements statements =
  List.map
    (fun stmt ->
      match Css.as_declarations stmt with
      | Some decls -> Css.declarations (filter_utility_properties decls)
      | None -> stmt)
    statements
  |> Css.map (fun selector decls ->
      Css.rule ~selector (filter_utility_properties decls))

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

(* Convert indexed rule to CSS statement. [verbatim] names the base classes
   whose rules arrived as finished CSS - a project's own [@utility] - so their
   declarations are emitted as written. The theme filter exists to split a
   utility's theme variables out of the utilities layer, and it recognises them
   by the layer metadata [Var.binding] attaches; a declaration parsed from
   author CSS carries none, so filtering it drops every [--tw-*] it sets. *)
let indexed_rule_to_statement ?(verbatim = fun _ -> false)
    (r : Sort.indexed_rule) =
  let keep_as_written =
    match r.base_class with Some c -> verbatim c | None -> false
  in
  let filtered_props =
    if keep_as_written then r.props else filter_utility_properties r.props
  in
  let filtered_nested =
    if keep_as_written then r.nested else filter_theme_from_statements r.nested
  in
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
      (* As for [`Media]: a variant stacked under [starting:] carries the inner
         query in [nested] and has no declarations of its own. *)
      if filtered_nested <> [] then Css.starting_style filtered_nested
      else Css.starting_style [ Css.rule ~selector:r.selector filtered_props ]
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
      (* As for [`Media]: a compound like [@md:hover:] carries the inner hover
         query in [nested] and has no declarations of its own. *)
      if filtered_nested <> [] then Css.container ~condition filtered_nested
      else
        Css.container ~condition
          [ Css.rule ~selector:r.selector ?merge_key filtered_props ]
  | `Supports condition ->
      if filtered_nested <> [] then Css.supports ~condition filtered_nested
      else
        Css.supports ~condition
          [ Css.rule ~selector:r.selector ?merge_key filtered_props ]

(* Two rules of the same kind. A condition is read through the equality its own
   cascade module states, which answers on the media the query selects rather
   than on how it is spelled. *)
let equal_rule_type a b =
  match (a, b) with
  | `Regular, `Regular | `Starting, `Starting -> true
  | `Media a, `Media b -> Css.Media.equal a b
  | `Container a, `Container b -> Css.Container.equal a b
  | `Supports a, `Supports b -> Css.Supports.equal a b
  | _ -> false

(* A rule kind's own identity, all a fingerprint may read of it: [Media] and
   [Container] answer equality on normalised queries, and no hash agrees with
   that, so the condition stays unread and two kinds share a bucket. *)
let rule_type_tag = function
  | `Regular -> 0
  | `Media _ -> 1
  | `Container _ -> 2
  | `Starting -> 3
  | `Supports _ -> 4

(* Deduplicate typed triples while preserving first occurrence order. Every part
   is compared through the equality its own cascade module states, and the
   bucket key carries the hashes those modules keep consistent with it. *)
let dedup_key (typ, sel, props, nested) =
  let combine acc h = (acc * 31) + h in
  let key = combine (rule_type_tag typ) (Css.Selector.hash sel) in
  let key =
    List.fold_left (fun key d -> combine key (Css.Declaration.hash d)) key props
  in
  List.fold_left (fun key st -> combine key (Css.hash_statement st)) key nested

let equal_dedup_key (typ, sel, props, nested) (typ', sel', props', nested') =
  equal_rule_type typ typ'
  && Css.Selector.equal sel sel'
  && List.equal Css.Declaration.equal_declaration props props'
  && List.equal Css.equal_statement nested nested'

let deduplicate_typed_triples triples =
  let seen = Hashtbl.create (List.length triples) in
  List.filter
    (fun (typ, sel, props, _order, nested, _base_class, _merge_key) ->
      let key = (typ, sel, props, nested) in
      let bucket = dedup_key key in
      if List.exists (equal_dedup_key key) (Hashtbl.find_all seen bucket) then
        false
      else (
        Hashtbl.add seen bucket key;
        true))
    triples

(* The base utility's order is [Utility.order] on its value, recovered here from
   the class string by re-parsing it through the handlers - the expensive part.
   A base class carries its modifier prefixes ([md:grid-cols-2]), so those are
   stripped before the lookup. [order_map] is populated by [Rule.outputs
   ~order_tbl] from the class strings it already builds, so the common case is a
   lookup; an unknown class (not in the input set) falls back to the parse, or
   to a selector-based conflict order when even that fails. *)

let order_of_base order_map base_class selector =
  match base_class with
  | Some class_name -> (
      let base_utility = extract_base_utility class_name in
      match Hashtbl.find_opt order_map base_utility with
      | Some order -> order
      | None -> (
          match Utility.base_of_class Scheme.default base_utility with
          | Ok u -> Utility.order u
          | Error _ -> conflict_order (Css.Selector.to_string selector)))
  | None -> conflict_order (Css.Selector.to_string selector)

(* Convert each rule type to typed triple *)
let triple typ ~selector ~props ~order ~nested ~base_class ~merge_key =
  Some (typ, selector, props, order, nested, base_class, merge_key)

(* The [(hover: hover)] media condition is the same for every hover rule. *)
let hover_media : Css.Media.t =
  Css.Media.Cond
    (Css.Media.Feature
       (Css.Media.Plain (Css.Media.Hover, Css.Media.Ident Css.Media.Hover)))

let rule_to_triple order_map = function
  | Regular { selector; props; base_class; nested; has_hover; merge_key } ->
      let order = order_of_base order_map base_class selector in
      let typ = if has_hover then `Media hover_media else `Regular in
      triple typ ~selector ~props ~order ~nested ~base_class ~merge_key
  | Media_query { condition; selector; props; base_class; nested } ->
      triple (`Media condition) ~selector ~props
        ~order:(order_of_base order_map base_class selector)
        ~nested ~base_class ~merge_key:None
  | Container_query { condition; selector; props; base_class; nested } ->
      triple (`Container condition) ~selector ~props
        ~order:(order_of_base order_map base_class selector)
        ~nested ~base_class ~merge_key:None
  | Starting_style { selector; props; base_class; nested } ->
      triple `Starting ~selector ~props
        ~order:(order_of_base order_map base_class selector)
        ~nested ~base_class ~merge_key:None
  | Supports_query { condition; selector; props; base_class; merge_key; nested }
    ->
      triple (`Supports condition) ~selector ~props
        ~order:(order_of_base order_map base_class selector)
        ~nested ~base_class ~merge_key

(* Add index to each triple for stable sorting *)
(* What [indexed_rule_to_statement] will emit, counted. For a built-in, theme
   declarations the utilities layer drops are not part of the rule Tailwind
   orders. A declared utility is finished author CSS, so all its declarations
   count, including custom properties without tw's internal layer annotation. *)
let rec declaration_count ~filter props nested =
  List.length (if filter then filter_utility_properties props else props)
  + List.fold_left
      (fun acc stmt ->
        acc
        +
        match Css.as_rule stmt with
        | Some (_, decls, inner) -> declaration_count ~filter decls inner
        | None -> (
            match Css.as_declarations stmt with
            | Some decls -> declaration_count ~filter decls []
            | None -> (
                match Css.as_media stmt with
                | Some (_, inner) -> declaration_count ~filter [] inner
                | None -> (
                    match Css.as_supports stmt with
                    | Some (_, inner) -> declaration_count ~filter [] inner
                    | None -> 0))))
      0 nested

let add_index ?theme ?(declared = fun _ -> false) triples =
  let buf = Buffer.create 256 in
  List.mapi
    (fun i (typ, sel, props, order, nested, base_class, merge_key) ->
      Buffer.clear buf;
      Css.Selector.to_buffer buf sel;
      let selector_str = Buffer.contents buf in
      let is_declared =
        match base_class with Some c -> declared c | None -> false
      in
      let media_key, nested_media_key = Sort.media_sort_keys typ nested in
      let responsive_media_key = Sort.responsive_media_key typ nested in
      let variant_order = Rule.compute_variant_order ~selector_str base_class in
      ({
         index = i;
         rule_type = typ;
         selector = sel;
         selector_str;
         selector_kind = Sort.classify_selector sel;
         has_modifier_colon = Css.Selector.contains_modifier_colon sel;
         props;
         declared = is_declared;
         declaration_count =
           declaration_count ~filter:(not is_declared) props nested;
         order;
         nested;
         base_class;
         merge_key;
         variant_order;
         variant_key = Sort.variant_sort_key base_class nested;
         variant_orders =
           Sort.variant_order_list ?theme base_class variant_order
             responsive_media_key;
         base_class_key = Option.value ~default:"" base_class;
         media_key;
         nested_media_key;
         responsive_media_key;
       }
        : Sort.indexed_rule))
    triples

(* The entrypoint expands one routed candidate into finished CSS before it hands
   the result to the regular build. That candidate can contain several top-level
   rules -- for example a selector branch, its [@supports] companion and a media
   branch. Tailwind keeps those rules together. Preserve their source order as
   one sortable block instead of letting the rule comparator interleave a later
   candidate between the branches. *)
let sort_indexed_blocks indexed =
  let groups = Hashtbl.create 8 in
  List.iter
    (fun (r : Sort.indexed_rule) ->
      match (r.declared, r.base_class) with
      | true, Some cls ->
          let previous =
            Option.value ~default:[] (Hashtbl.find_opt groups cls)
          in
          Hashtbl.replace groups cls (r :: previous)
      | _ -> ())
    indexed;
  let emitted = Hashtbl.create 8 in
  let blocks =
    List.filter_map
      (fun (r : Sort.indexed_rule) ->
        match (r.declared, r.base_class) with
        | true, Some cls when Hashtbl.mem emitted cls -> None
        | true, Some cls ->
            Hashtbl.add emitted cls ();
            Some (r, List.rev (Hashtbl.find groups cls))
        | _ -> Some (r, [ r ]))
      indexed
  in
  blocks
  |> List.sort (fun (r1, _) (r2, _) -> Sort.compare_indexed_rules r1 r2)
  |> List.concat_map snd

(* Convert selector/props pairs to CSS rules. *)
(* Internal: build rule sets from pre-extracted outputs. *)
let rule_sets_from_selector_props order_map all_rules =
  (* All rules (including hover) are now sorted together. Hover rules are
     converted to Media "(hover:hover)" rules in rule_to_triple, so they
     participate in the normal media query sorting. *)
  let indexed =
    all_rules
    |> List.filter_map (rule_to_triple order_map)
    |> deduplicate_typed_triples |> add_index
  in
  let sorted = sort_indexed_blocks indexed in
  if Sort.debug_compare_enabled () then
    List.iter
      (fun (r : Sort.indexed_rule) ->
        prerr_endline
          (Pp.str
             [
               "SORTED: vo=";
               Pp.int r.variant_order;
               " base=";
               (match r.base_class with Some s -> s | None -> "<none>");
               " type=";
               (match r.rule_type with
               | `Regular -> "R"
               | `Media m -> "M:" ^ Css.Media.to_string m
               | `Container _ -> "C"
               | `Starting -> "S"
               | `Supports _ -> "U");
               " nested=";
               Pp.int (List.length r.nested);
             ]))
      sorted;
  sorted |> List.map indexed_rule_to_statement

let utilities_layer ~layers ~statements =
  (* Statements are already in the correct order, with adjacent conditional
     groups of equal prelude merged before the layer is assembled. *)
  if layers then Css.v [ Css.layer ~name:[ "utilities" ] statements ]
  else Css.v statements

(* Each utility is wrapped in its own conditional group, so a run of utilities
   sharing one condition arrives here as a run of equal blocks where Tailwind
   has a single one. Cascade collapses each run. [@starting-style] carries no
   condition, so adjacency is its whole gate; the other three compare preludes
   as well. Every pass merges adjacent blocks only, which leaves the rule order
   the comparator produced untouched. *)
let statements_of_sorted_rules ?verbatim sorted_rules =
  List.map (indexed_rule_to_statement ?verbatim) sorted_rules
  |> Css.Optimize.merge_consecutive_starting_style
  |> Css.Optimize.merge_consecutive_media
  |> Css.Optimize.merge_consecutive_supports
  |> Css.Optimize.merge_consecutive_containers
  |> Css.Optimize.merge_distant_containers

(* Get sorted indexed rules - used for extracting first-usage order of
   variables *)
let sorted_indexed_rules ?theme ?declared order_map all_rules =
  all_rules
  |> List.filter_map (rule_to_triple order_map)
  |> deduplicate_typed_triples |> add_index ?theme ?declared
  |> sort_indexed_blocks

(* Sort var names by property_order. Names include -- prefix. *)
let sort_vars_by_property_order metadata vars =
  let get_order name =
    match metadata_property_order metadata name with
    | Some o -> o
    | None -> 1000 (* Default for vars without property_order *)
  in
  (* Decorate-sort-undecorate: [get_order] allocates a [String.sub] per call,
     and a comparator runs it on both operands of every comparison. *)
  vars
  |> List.map (fun name -> (get_order name, name))
  |> List.stable_sort (fun (o1, _) (o2, _) -> Int.compare o1 o2)
  |> List.map snd

(* Extract all var names from sorted indexed rules in utility order. For each
   utility, collects: 1. Vars that are SET (custom declarations) 2. Vars that
   are REFERENCED and need @property (e.g., transform refs rotate/skew) Within
   each utility, vars are sorted by property_order to ensure consistent family
   ordering (e.g., ring before inset-ring regardless of CSS value order). *)
let var_names_of_sorted_rules metadata sorted_rules =
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
            match Var.metadata_of_var v with
            | Some metadata -> Var.metadata_needs_property metadata
            | None -> false)
        |> List.map (fun (Css.V v) -> "--" ^ Css.var_name v)
      in
      (* Sort all vars from this utility by property_order *)
      sort_vars_by_property_order metadata (set_vars @ ref_vars))

let rule_sets tw_classes =
  let order_tbl = Hashtbl.create 256 in
  let all_rules = List.concat_map (Rule.outputs ~order_tbl) tw_classes in
  rule_sets_from_selector_props order_tbl all_rules

let indexed_rules tw_classes =
  let order_tbl = Hashtbl.create 256 in
  List.concat_map (Rule.outputs ~order_tbl) tw_classes
  |> List.filter_map (rule_to_triple order_tbl)
  |> deduplicate_typed_triples |> add_index

let compare_rules = Sort.compare_indexed_rules
let rule_selector (r : Sort.indexed_rule) = r.selector_str

(* ======================================================================== *)
(* Layer Generation - CSS @layer directives and theme variable resolution *)
(* ======================================================================== *)

module Strings = Set.Make (String)

(* Helpers for theme layer extraction and ordering *)
let collect_selector_props tw_classes = List.concat_map Rule.outputs tw_classes

(* Collect the theme-layer tokens a statement tree declares. A compound variant
   nests its rule under whichever at-rules its modifiers ask for, so the walk
   has to reach a declaration under any of them; [Css.Stylesheet]'s pair of
   exhaustive readers is that walk. *)
let rec extract_theme_from_statements theme_vars insertion_order statements =
  List.iter
    (fun stmt ->
      Css.Stylesheet.statement_declarations stmt
      |> Css.custom_declarations ~layer:"theme"
      |> List.iter (fun decl ->
          match Css.custom_declaration_name decl with
          | Some name when not (Hashtbl.mem theme_vars name) ->
              Hashtbl.add theme_vars name decl;
              insertion_order := decl :: !insertion_order
          | _ -> ());
      extract_theme_from_statements theme_vars insertion_order
        (Css.Stylesheet.statement_children stmt))
    statements

let extract_non_tw_custom_declarations selector_props =
  (* Use Hashtbl to collect unique theme variables efficiently *)
  let theme_vars = Hashtbl.create 32 in
  let insertion_order = ref [] in

  let add_props props =
    Css.custom_declarations ~layer:"theme" props
    |> List.iter (fun decl ->
        match Css.custom_declaration_name decl with
        | Some name when not (Hashtbl.mem theme_vars name) ->
            Hashtbl.add theme_vars name decl;
            insertion_order := decl :: !insertion_order
        | _ -> ())
  in
  selector_props
  |> List.iter (function
    (* A compound variant like [md:hover:] holds its declarations in [nested],
       so the tokens they declare are only reachable through it. *)
    | Regular { props; nested; _ }
    | Media_query { props; nested; _ }
    | Container_query { props; nested; _ } ->
        add_props props;
        extract_theme_from_statements theme_vars insertion_order nested
    | Starting_style { props; _ } | Supports_query { props; _ } ->
        add_props props);
  (* Return in original insertion order *)
  List.rev !insertion_order

(* Substitute per-render [@theme] token overrides into extracted theme-layer
   declarations. This is the threaded replacement for the override seam that
   used to live in [Var.binding]: a Theme-role variable whose token is
   overridden in [theme] emits the override value instead of its registered
   default. A token the block removed ([--spacing: initial]) has no declaration
   left to emit. [custom_declaration_name] returns the full [--name] form; the
   scheme keys overrides by the bare name. *)
let apply_token_override theme decl =
  match Css.custom_declaration_name decl with
  | Some full_name
    when String.length full_name > 2 && String.sub full_name 0 2 = "--" -> (
      let bare = String.sub full_name 2 (String.length full_name - 2) in
      match Scheme.token_override theme bare with
      | Some css -> Some (Css.custom_property ~layer:"theme" full_name css)
      | None -> if Scheme.is_removed theme bare then None else Some decl)
  | _ -> Some decl

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

(* The position of a theme token within the project's [@theme] declaration list,
   keyed by its bare name. [Scheme.token_overrides] preserves the source order
   the CSS entrypoint (or [Scheme.with_overrides] caller) declared them in. *)
let declared_index theme =
  List.mapi (fun i (name, _) -> (name, i)) theme.Scheme.token_overrides

(* [declared] is the bare-name -> declaration-index table [declared_index]
   built; strips the leading [--] a custom declaration's name carries before
   looking it up. *)
let declared_order declared name =
  match name with
  | None -> None
  | Some full ->
      let bare =
        if String.length full > 2 && String.sub full 0 2 = "--" then
          String.sub full 2 (String.length full - 2)
        else full
      in
      List.assoc_opt bare declared

(* Sort declarations by their Var order metadata, then declaration order within
   a shared slot, then alphabetical fallback. A project-named family
   (--font-<name>, --text-<name>, --leading-<name>, ...) funnels every member
   into one shared (priority, suborder) slot (see [Var.mli]), so two project
   tokens tie there; Tailwind keeps the order the [@theme] block wrote them in
   rather than sorting by name. *)
let sort_by_var_order ~metadata ~theme decls =
  let declared = declared_index theme in
  decls
  |> List.map (fun d ->
      let name = Css.custom_declaration_name d in
      let order =
        match Var.order_of_declaration d with
        | Some _ as order -> order
        | None -> Option.bind name (metadata_order metadata)
      in
      (d, order, name, declared_order declared name))
  |> List.sort (fun (_, a, na, ia) (_, b, nb, ib) ->
      let c = compare_orders a b in
      if c <> 0 then c
      else
        match (ia, ib) with
        | Some ia, Some ib -> Int.compare ia ib
        | _ -> compare na nb)
  |> List.map (fun (d, _, _, _) -> d)

(* Build theme layer rule from declarations *)
let theme_layer_rule ~layers = function
  | [] -> if layers then Css.v [ Css.layer ~name:[ "theme" ] [] ] else Css.empty
  | decls ->
      let selector = Css.Selector.(list [ Root; host () ]) in
      let rule = Css.rule ~selector decls in
      if layers then Css.v [ Css.layer ~name:[ "theme" ] [ rule ] ]
      else Css.v [ rule ]

(* Read every [var()] from a declaration's serialized value. The normal typed
   walk is faster, but a nested calculation can hold another typed [calc()]
   inside a [Val] node (space-x's reverse multiplier is one example), and that
   node is intentionally opaque to the generic calc walker. Tokenising the value
   directly supplies the exhaustive dependency view this build decision needs
   without imposing custom-property declaration grammar on an arbitrary
   property's otherwise-valid value. *)
let add_declaration_var_names names declarations =
  List.fold_left
    (fun names declaration ->
      let value = Css.declaration_value declaration in
      Css.Variables.var_refs_in_value_string value
      |> List.fold_left (fun names name -> Strings.add name names) names)
    names declarations

let rec add_statement_var_names names statements =
  List.fold_left
    (fun names statement ->
      let names =
        add_declaration_var_names names
          (Css.Stylesheet.statement_declarations statement)
      in
      add_statement_var_names names
        (Css.Stylesheet.statement_children statement))
    names statements

(* Every var() referenced anywhere in a utility's output (top-level props and
   nested @media/@supports). *)
let add_output_var_names names = function
  | Regular { props; nested; _ }
  | Media_query { props; nested; _ }
  | Container_query { props; nested; _ }
  | Starting_style { props; nested; _ }
  | Supports_query { props; nested; _ } ->
      let names = add_declaration_var_names names props in
      add_statement_var_names names nested

let referenced_var_names selector_props =
  List.fold_left add_output_var_names Strings.empty selector_props

let add_output_metadata index = function
  | Regular { props; nested; _ }
  | Media_query { props; nested; _ }
  | Container_query { props; nested; _ }
  | Starting_style { props; nested; _ }
  | Supports_query { props; nested; _ } ->
      let index = add_declarations_metadata index props in
      add_vars_metadata index (Css.vars_of_rules nested)

(* Theme tokens referenced via var() (e.g. an arbitrary [color:var(--color-red-
   500)]) must appear in @layer theme, but the extractor above only collects
   tokens utilities SET. Emit the catalogued colour tokens those references name
   (typed value + canonical order via [Color.Handler.theme_color_decl]), and any
   other token the project's own [@theme] declared: nothing else in the sheet
   declares it, so a utility that only reads it would name a variable with no
   value. An [@theme inline] token has no declaration by definition, and an
   [@theme reference] one is declared outside the sheet, so neither is emitted.
   [exclude] holds the already-emitted (set) token names. *)
let referenced_theme_decls ~theme ~exclude selector_props =
  referenced_var_names selector_props
  |> Strings.to_list
  |> List.filter_map (fun full ->
      if
        String.length full <= 2
        || String.sub full 0 2 <> "--"
        || Strings.mem full exclude
      then None
      else
        let bare = String.sub full 2 (String.length full - 2) in
        match bare with
        (* An arbitrary value may name the spacing scale directly, as
           [p-[calc(--spacing(2)+1px)]] does. *)
        | "spacing" ->
            let decl, _ =
              Var.binding Theme.spacing_var
                (Option.value
                   (Option.bind
                      (Scheme.theme_value (Some theme) "spacing")
                      Css.parse_length)
                   ~default:Theme.spacing_base)
            in
            Some decl
        | _ -> (
            match Color.Handler.theme_color_decl ~theme bare with
            | Some _ as decl -> decl
            | None ->
                if
                  Scheme.is_inline_token theme bare
                  || Scheme.is_reference_token theme bare
                then None
                else
                  Option.map
                    (Css.custom_property ~layer:"theme" full)
                    (Scheme.token_override theme bare)))

(* [--default-font-family] points at [--font-sans], [--default-mono-font-family]
   at [--font-mono]. Tailwind spells the pair [--theme(--font-sans, initial)],
   which reads two ways. When the project declared the source token in an
   [@theme inline] block it has no declaration of its own, so the default
   carries its value instead of a reference nothing resolves. When the project
   took the source token out of its theme the [initial] fallback applies, and
   the default goes with it rather than naming a variable nothing declares. A
   default the project gave a value of its own points elsewhere, so neither
   reading touches it. *)
let resolve_default_family theme decl =
  match Css.custom_declaration_name decl with
  | Some name -> (
      let token =
        match name with
        | "--default-font-family" -> Some "font-sans"
        | "--default-mono-font-family" -> Some "font-mono"
        | _ -> None
      in
      match token with
      | Some t
        when String.trim (Css.declaration_value decl) = "var(--" ^ t ^ ")" ->
          if Scheme.is_removed theme t then None
          else if Scheme.is_inline_token theme t then
            match Scheme.theme_value (Some theme) t with
            | Some v -> Some (Css.custom_property ~layer:"theme" name v)
            | None -> Some decl
          else Some decl
      | _ -> Some decl)
  | None -> Some decl

(* Tailwind derives the default font-feature settings from the sans and mono
   tokens the project declared. *)
let derived_font_feature_decls ~theme ~have =
  [
    ("font-sans--font-feature-settings", "default-font-feature-settings");
    ("font-mono--font-feature-settings", "default-mono-font-feature-settings");
  ]
  |> List.filter_map (fun (token, name) ->
      match Scheme.theme_value (Some theme) token with
      | Some v when not (Strings.mem ("--" ^ name) have) ->
          Some (Css.custom_property ~layer:"theme" ("--" ^ name) v)
      | _ -> None)

(* A runtime theme declaration attached to a utility is a dependency carrier,
   not independent output. A zero-valued spacing utility, for example, has
   already folded its value to [0px] and no longer reads [--spacing]. Non-
   runtime theme declarations retain Tailwind's emission semantics even when the
   utility folded their value. [theme(static)] is the explicit runtime
   exception: it asks for the whole theme whether a utility reads each token or
   not. *)
let keep_extracted_theme_decl ~theme ~referenced decl =
  theme.Scheme.static_theme
  || (not (Var.is_runtime_declaration decl))
  ||
  match Css.custom_declaration_name decl with
  | Some name -> Strings.mem name referenced
  | None -> false

(* [theme(static)] on the package import emits every theme variable, not only
   the ones a utility used. The palette is by far the biggest part of it. *)
let add_static_theme_decls ~theme extracted =
  if not theme.Scheme.static_theme then extracted
  else
    let have = names_set_of extracted in
    let registered =
      Scheme.all_default_tokens ()
      |> List.map (fun (name, css) ->
          Css.custom_property ~layer:"theme" ("--" ^ name) css)
    in
    extracted
    @ List.filter
        (fun d ->
          match Css.custom_declaration_name d with
          | Some n -> not (Strings.mem n have)
          | None -> true)
        (Color.Handler.all_palette_declarations ~theme () @ registered)

(* Internal helper to compute theme layer from pre-extracted outputs. *)
let theme_layer_of_props ?(theme = Scheme.default) ?(layers = true)
    ?(default_decls = []) ?metadata selector_props =
  let metadata =
    match metadata with
    | Some metadata -> metadata
    | None -> List.fold_left add_output_metadata Metadata.empty selector_props
  in
  let referenced = referenced_var_names selector_props in
  let extracted =
    extract_non_tw_custom_declarations selector_props
    |> List.filter_map (apply_token_override theme)
    |> List.filter (keep_extracted_theme_decl ~theme ~referenced)
  in
  let extracted =
    extracted
    @ referenced_theme_decls ~theme ~exclude:(names_set_of extracted)
        selector_props
  in
  let extracted =
    extracted @ derived_font_feature_decls ~theme ~have:(names_set_of extracted)
  in
  let extracted = add_static_theme_decls ~theme extracted in
  let pre_defaults, post_defaults = split_defaults default_decls in

  (* Filter defaults to remove duplicates of extracted vars *)
  let extracted_names = names_set_of extracted in
  let pre = filter_non_duplicates extracted_names pre_defaults in
  let post =
    filter_non_duplicates
      (Strings.union extracted_names (names_set_of pre))
      post_defaults
  in

  (* A project [@theme] override wins wherever the declaration came from: the
     built-in defaults carry the same token names as the extracted ones. *)
  pre @ extracted @ post
  |> List.filter_map (apply_token_override theme)
  |> List.filter_map (resolve_default_family theme)
  |> sort_by_var_order ~metadata ~theme
  |> theme_layer_rule ~layers

let theme_layer_of ?theme ?(default_decls = []) tw_classes =
  let selector_props = collect_selector_props tw_classes in
  theme_layer_of_props ?theme ~default_decls selector_props

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
      ~condition:(Css.Supports.property "color" "color-mix(in lab, red, red)")
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
                 (Css.Supports.property "-webkit-appearance" "-apple-pay-button"),
               Css.Supports.property "contain-intrinsic-size" "1px" ))
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
  Css.layer_of ~name:[ "base" ] base

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
        ( property "-webkit-hyphens" "none",
          Not (property "margin-trim" "inline") ),
      And
        ( property "-moz-orient" "inline",
          Not (property "color" "rgb(from red r g b)") ) )

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

let property_order_from metadata fallback_order ~fallback name =
  match metadata_property_order metadata name with
  | Some o -> o
  | None ->
      Option.value ~default:fallback (Hashtbl.find_opt fallback_order name)

let property_statement_order statements =
  let order = Hashtbl.create 32 in
  List.iteri
    (fun index statement ->
      match Css.as_property statement with
      | Some (Css.Property_info info) ->
          if not (Hashtbl.mem order info.name) then
            Hashtbl.add order info.name index
      | None -> ())
    statements;
  order

(* Build family first-usage order from the first_usage_order hashtbl. Returns a
   hashtbl mapping family to its first occurrence index. *)
let family_order metadata first_usage_order =
  let family_order = Hashtbl.create 16 in
  Hashtbl.iter
    (fun name idx ->
      match metadata_family metadata name with
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

(* Canonical CSS-property rank for a [--tw-*] variable, following Tailwind's
   @property emission order. The [`Border] family spans several slots, so split
   it by name. *)
let canonical_property_rank metadata name =
  match metadata_family metadata name with
  | Some (`Translate | `Scale | `Rotate | `Skew) -> 16 (* transform *)
  | Some `Gradient -> 28 (* background-image *)
  | Some `Leading -> 39 (* line-height *)
  | Some `Font_weight -> 40
  | Some `Tracking -> 41 (* letter-spacing *)
  | Some (`Shadow | `Inset_shadow | `Ring | `Inset_ring) -> 51 (* box-shadow *)
  | Some `Filter -> 53
  | Some `Drop_shadow -> 54
  | Some `Backdrop_filter -> 55
  | Some `Duration -> 56 (* transition *)
  | Some `Content -> 57
  | Some `Text_shadow -> 59 (* emitted last *)
  | Some `Border ->
      if String.starts_with ~prefix:"--tw-outline" name then 52
      else if String.starts_with ~prefix:"--tw-space" name then 17
      else if String.starts_with ~prefix:"--tw-divide" name then 18
      else 27 (* border-style *)
  | None -> 1000

(* The transform block and duration order by first-usage against EVERY family,
   not just each other: a variable declared only via a variant (e.g.
   hover:scale) must emit late even next to a base box-shadow variable. Every
   other family follows canonical property order. *)
let in_transform_group = function
  | Some (`Translate | `Scale | `Rotate | `Skew | `Duration) -> true
  | _ -> false

(* First-usage rather than canonical rank when either variable is in the
   transform block, or both are Border-family (which interleaves its reverse
   flags with border-style per declaration order: divide-x emits x-reverse then
   border-style, divide-y emits y-reverse). *)
let order_by_first_usage fam1 fam2 =
  in_transform_group fam1 || in_transform_group fam2
  || (fam1 = Some `Border && fam2 = Some `Border)

(* Order two variables sharing a canonical rank (the within-group suborder). *)
let compare_property_vars_same_rank ~get_family_order ~get_first_usage n1 n2 po1
    po2 fam1 fam2 =
  match (fam1, fam2) with
  | Some `Gradient, Some `Gradient ->
      compare (gradient_family_index n1) (gradient_family_index n2)
  | _ when uses_direct_property_order fam1 && uses_direct_property_order fam2 ->
      if fam1 = fam2 && fam1 = Some `Border then
        compare (get_first_usage n1) (get_first_usage n2)
      else compare po1 po2
  | _ ->
      let fo1 = get_family_order n1 in
      let fo2 = get_family_order n2 in
      if fo1 <> fo2 then compare fo1 fo2 else compare po1 po2

let compare_property_vars ~metadata ~get_family_order ~get_first_usage n1 n2 po1
    po2 fam1 fam2 =
  (* Variables with negative property_order and no family come FIRST *)
  match (fam1, po1 < 0, fam2, po2 < 0) with
  | None, true, None, true -> compare po1 po2
  | None, true, _, _ -> -1
  | _, _, None, true -> 1
  | _ when order_by_first_usage fam1 fam2 ->
      if fam1 = Some `Border && fam2 = Some `Border then
        (* Border interleaves reverse flags with border-style per declaration
           order, so key on the variable's own first-usage. *)
        compare (get_first_usage n1) (get_first_usage n2)
      else
        let fo1 = get_family_order n1 in
        let fo2 = get_family_order n2 in
        if fo1 <> fo2 then compare fo1 fo2 else compare po1 po2
  | _ ->
      let cr =
        compare
          (canonical_property_rank metadata n1)
          (canonical_property_rank metadata n2)
      in
      if cr <> 0 then cr
      else
        compare_property_vars_same_rank ~get_family_order ~get_first_usage n1 n2
          po1 po2 fam1 fam2

(* Shared by [sort_properties_by_order] (the @layer properties initial values)
   and [sort_property_rules_by_usage] (the @property rules): the two MUST sort
   variable names in lockstep, since one produces the initial-value order and
   the other the @property emission order for the same variables, and a mismatch
   would emit a properties layer that contradicts its own @property rules. *)
let property_var_comparator metadata fallback_order first_usage_order =
  let family_order = family_order metadata first_usage_order in
  let get_family_order name =
    match metadata_family metadata name with
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
  fun n1 n2 ->
    let fam1 = metadata_family metadata n1 in
    let fam2 = metadata_family metadata n2 in
    let po1 =
      property_order_from metadata fallback_order ~fallback:(get_first_usage n1)
        n1
    in
    let po2 =
      property_order_from metadata fallback_order ~fallback:(get_first_usage n2)
        n2
    in
    compare_property_vars ~metadata ~get_family_order ~get_first_usage n1 n2 po1
      po2 fam1 fam2

let sort_properties_by_order metadata fallback_order first_usage_order
    initial_values =
  let cmp_name =
    property_var_comparator metadata fallback_order first_usage_order
  in
  let cmp (n1, _) (n2, _) = cmp_name n1 n2 in
  List.sort cmp initial_values

(* Build property layer content with browser detection *)
let property_layer_content metadata fallback_order first_usage_order
    initial_values other_statements =
  let selector =
    Css.Selector.(list [ universal; Before Single; After Single; Backdrop ])
  in
  let sorted_values =
    sort_properties_by_order metadata fallback_order first_usage_order
      initial_values
  in
  let initial_declarations = List.map snd sorted_values in
  let rule = Css.rule ~selector initial_declarations in
  let supports_stmt = Css.supports ~condition:browser_detection [ rule ] in
  let layer_content = [ supports_stmt ] @ other_statements in
  Css.v [ Css.layer ~name:[ "properties" ] layer_content ]

(* Build the properties layer with browser detection for initial values *)
(* Returns (properties_layer, property_rules) - @property rules are separate *)
let properties_layer metadata fallback_order first_usage_order
    explicit_property_rules_statements =
  let property_rules, other_statements =
    partition_properties explicit_property_rules_statements
  in
  let deduplicated = dedup_properties property_rules in
  let initial_values = initial_values_of deduplicated in

  if deduplicated = [] && initial_values = [] then (Css.empty, [])
  else
    let layer =
      property_layer_content metadata fallback_order first_usage_order
        initial_values other_statements
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
  | Style.Style { props; rules; property_rules; metadata; _ } ->
      let vars_from_props = Css.vars_of_declarations props in
      let vars_from_rules =
        match rules with Some r -> Css.vars_of_rules r | None -> []
      in
      let set_names = set_var_names_from_props props in
      ( vars_from_props @ vars_from_rules,
        set_names,
        [ property_rules ],
        metadata )
  | Style.Modified (_, t) -> extract_style_vars_and_rules t
  | Style.Group ts ->
      let results = List.map extract_style_vars_and_rules ts in
      let vars_list, set_names_list, prop_rules_list, metadata_list =
        List.fold_right
          (fun (v, s, p, m) (vs, ss, ps, ms) ->
            (v :: vs, s :: ss, p :: ps, m :: ms))
          results ([], [], [], [])
      in
      ( List.concat vars_list,
        List.concat set_names_list,
        List.concat prop_rules_list,
        List.concat metadata_list )

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
  |> List.filter_map (fun (Css.V v) -> Var.property_rule_of_var v)

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
        (* --tw-content is special: Tailwind emits @property --tw-content (and
           its universal seed) only for before/after pseudo-elements, never for
           the content-* utilities that merely set the variable. The pseudo path
           adds it explicitly via has_pseudo_elements, so exclude it from the
           set-based auto-collection here. *)
        var_name <> "--tw-content" && Strings.mem var_name set_names_set)
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
  Css.v [ Css.layer_decl (List.map (fun n -> [ n ]) names) ]

(* Sort [@property] rules using first-usage order. Variables are ordered by when
   they first appear across all utilities. For variables within the same family
   that both use direct property_order, first-usage order is used as primary
   sort key (this matches Tailwind's behavior where per-utility declaration
   order determines @property order). Falls back to family order then
   property_order for cross-family sorting. *)
let sort_property_rules_by_usage metadata fallback_order first_usage_order
    property_rules_for_end =
  let cmp_name =
    property_var_comparator metadata fallback_order first_usage_order
  in
  property_rules_for_end
  |> List.sort (fun s1 s2 ->
      match (Css.as_property s1, Css.as_property s2) with
      | ( Some (Css.Property_info { name = n1; _ }),
          Some (Css.Property_info { name = n2; _ }) ) ->
          cmp_name n1 n2
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
    ~base_layer ~utilities_layer ~property_rules_for_end ~keyframes ~metadata
    ~fallback_order ~first_usage_order =
  let base_layers =
    if include_base then [ theme_layer; base_layer ] else [ theme_layer ]
  in
  let initial_layers =
    match properties_layer with None -> [] | Some l -> [ l ]
  in
  let layers_without_property =
    if layers then
      let components_declaration =
        Css.v [ Css.layer_decl [ [ "components" ] ] ]
      in
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
    sort_property_rules_by_usage metadata fallback_order first_usage_order
      property_rules_for_end
  in
  let property_rules_css =
    if sorted_property_rules = [] then [] else [ Css.v sorted_property_rules ]
  in
  let keyframes_css = dedup_keyframes_to_css keyframes in
  layers_without_property @ property_rules_css @ keyframes_css

(* Extract variables, set var names, and property rules from all utilities *)
(* Takes the already-built styles rather than the utilities: [layers] has just
   built the same tree, and [Utility.to_style] dispatches through every
   registered handler and allocates the whole declaration tree per class. *)
let extract_vars_and_rules styles =
  let results = List.map extract_style_vars_and_rules styles in
  let vars_list, set_names_list, prop_rules_list, metadata_list =
    List.fold_right
      (fun (v, s, p, m) (vs, ss, ps, ms) ->
        (v :: vs, s :: ss, p :: ps, m :: ms))
      results ([], [], [], [])
  in
  ( List.concat vars_list,
    List.concat set_names_list,
    List.concat prop_rules_list,
    List.concat metadata_list )

(* Flatten property rules into CSS statements *)
let flatten_property_rules property_rules_lists =
  property_rules_lists |> List.concat_map Css.statements

(* Build individual CSS layers *)
(* Detect if before/after pseudo-elements are used - triggers content var
   property rule *)
let has_pseudo_elements tw_classes =
  let has_pseudo = function
    | Style.Pseudo_before | Style.Pseudo_after -> true
    | _ -> false
  in
  let rec check_utility = function
    | Utility.Base _ -> false
    | Utility.Modified (modifier, u) -> has_pseudo modifier || check_utility u
    | Utility.Group us -> List.exists check_utility us
    | Utility.Important (_, u) -> check_utility u
    | Utility.Aliased (_, u) | Utility.Theme_bound (_, u) -> check_utility u
  in
  List.exists check_utility tw_classes

(* The default duration and timing function are theme declarations every
   [transition-*] rule reads, so a sheet carrying one of those utilities needs
   them however the utility is dressed. Reading the name off the emitted class
   missed [hover:transition] and every other variant, whose class is
   [hover:transition], and the rule then referenced a variable nothing
   declared. *)
let has_transition_utility tw_classes =
  let rec check = function
    | Utility.Base b ->
        let c = Utility.class_of_base b in
        String.length c >= 10
        && String.sub c 0 10 = "transition"
        && not (String.equal c "transition-none")
    | Utility.Modified (_, u)
    | Utility.Important (_, u)
    | Utility.Aliased (_, u)
    | Utility.Theme_bound (_, u) ->
        check u
    | Utility.Group us -> List.exists check us
  in
  List.exists check tw_classes

(* Result of building individual layers *)
type layers_result = {
  theme_layer : Css.t;
  base_layer : Css.t;
  properties_layer : Css.t option;
  utilities_layer : Css.t;
  property_rules : Css.statement list;
}

let individual_layers ~theme ~layers ~include_base ~forms_base ~has_transition
    ~metadata ~fallback_order first_usage_order selector_props
    all_property_statements statements =
  let theme_defaults =
    let font_defaults =
      if include_base then Typography.default_font_family_declarations else []
    in
    let transition_defaults =
      if include_base && has_transition then
        Transitions.default_transition_declarations
      else []
    in
    font_defaults @ transition_defaults
  in
  let theme_layer =
    theme_layer_of_props ~theme ~layers ~default_decls:theme_defaults ~metadata
      selector_props
  in
  let base_layer = base_layer ~supports:placeholder_supports ~forms_base () in
  let properties_layer, property_rules =
    if all_property_statements = [] then (None, [])
    else
      let layer, prop_rules =
        properties_layer metadata fallback_order first_usage_order
          all_property_statements
      in
      match Css.statements layer with
      | [] -> (None, prop_rules)
      | _ -> (Some layer, prop_rules)
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
let sort_keyframes_by_var_order metadata keyframes =
  keyframes
  |> List.sort (fun (name1, _) (name2, _) ->
      let keyframe_var_order name =
        match metadata_order metadata ("animate-" ^ name) with
        | Some (p, s) -> (p * 1000) + s
        | None -> 1000000 (* Unknown keyframes sort last *)
      in
      let order_cmp =
        Int.compare (keyframe_var_order name1) (keyframe_var_order name2)
      in
      if order_cmp <> 0 then order_cmp
      else String.compare name1 name2 (* Stable sort for same order *))

(** Build all CSS layers from utilities and rules *)
let layers ~theme ~layers ~include_base ?forms ~selector_props ~sorted_rules
    tw_classes statements =
  let styles = List.map (Utility.to_style theme) tw_classes in
  let vars_from_utilities, set_var_names, property_rules_lists, style_metadata =
    extract_vars_and_rules styles
  in
  let metadata = metadata_index sorted_rules style_metadata in
  (* Build first-usage order from ALL vars per utility in utility order. For
     each utility, collects SET vars then REFERENCED vars needing @property.
     Within each utility, vars are sorted by property_order (done in
     var_names_of_sorted_rules). Across utilities, we preserve first-usage order
     to match Tailwind's behavior. *)
  let all_vars = var_names_of_sorted_rules metadata sorted_rules in
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
  let fallback_order = property_statement_order all_property_statements in
  (* The forms base layer is the plugin's [base] strategy: a global reset of
     native form controls. It is opt-in only ([~forms:true]), mirroring
     Tailwind's [\@plugin '@tailwindcss/forms']. The [.form-*] class-strategy
     utilities emit their own per-class styles when used, independent of this
     flag, so utility presence must not auto-enable the global base. *)
  let forms_base = match forms with Some f -> f | None -> false in
  let individual =
    individual_layers ~theme ~layers ~include_base ~forms_base
      ~has_transition:(has_transition_utility tw_classes)
      ~metadata ~fallback_order first_usage_order selector_props
      all_property_statements statements
  in
  let keyframes =
    List.fold_left collect_keyframes [] styles
    |> List.rev
    |> sort_keyframes_by_var_order metadata
  in
  assemble_all_layers ~layers ~include_base
    ~properties_layer:individual.properties_layer
    ~theme_layer:individual.theme_layer ~base_layer:individual.base_layer
    ~utilities_layer:individual.utilities_layer
    ~property_rules_for_end:individual.property_rules ~keyframes ~metadata
    ~fallback_order ~first_usage_order

(* ======================================================================== *)
(* CSS Generation API *)
(* ======================================================================== *)

type config = { base : bool; forms : bool option; layers : bool }

let default_config = { base = true; forms = None; layers = true }

(* A statement a project's own [@utility] produced, in the shape a built-in
   utility yields, so the utilities layer sorts it by its order rather than
   receiving it after everything else. *)
let rec first_selector stmt =
  match Css.as_rule stmt with
  | Some (selector, _, _) -> Some selector
  | None ->
      let inner =
        match Css.as_media stmt with
        | Some (_, inner) -> inner
        | None -> (
            match Css.as_supports stmt with
            | Some (_, inner) -> inner
            | None -> (
                match Css.as_container stmt with
                | Some (_, _, inner) -> inner
                | None -> []))
      in
      List.find_map first_selector inner

let outputs_of_statement ~base_class stmt =
  (* An at-rule nested in another - what a project's own [dark] variant builds
     around a colour utility's [@supports] - keeps the inner one verbatim in
     [nested], the same shape a compound modifier already uses. Decomposing it
     instead would emit the inner rule without the outer condition. *)
  let of_inner wrap inner =
    if List.for_all (fun st -> Css.as_rule st <> None) inner then
      List.concat_map
        (fun st ->
          match Css.as_rule st with
          | Some (selector, props, _) -> [ wrap ~selector ~props ~nested:[] ]
          | None -> [])
        inner
    else
      match List.find_map first_selector inner with
      | Some selector -> [ wrap ~selector ~props:[] ~nested:inner ]
      | None -> []
  in
  match Css.as_rule stmt with
  | Some (selector, props, nested) ->
      [ Output.regular ~selector ~props ~base_class ~nested () ]
  | None -> (
      match Css.as_media stmt with
      | Some (condition, inner) ->
          of_inner
            (fun ~selector ~props ~nested ->
              Output.media_query ~condition ~selector ~props ~base_class ~nested
                ())
            inner
      | None -> (
          match Css.as_supports stmt with
          | Some (condition, inner) ->
              of_inner
                (fun ~selector ~props ~nested:_ ->
                  Output.supports_query ~condition ~selector ~props ~base_class
                    ())
                inner
          | None -> (
              match Css.as_container stmt with
              | Some (_, Some condition, inner) ->
                  of_inner
                    (fun ~selector ~props ~nested ->
                      Output.container_query ~condition ~selector ~props
                        ~base_class ~nested ())
                    inner
              | _ -> [])))

let output_base_class_and_props = function
  | Output.Regular { base_class; props; _ }
  | Media_query { base_class; props; _ }
  | Container_query { base_class; props; _ }
  | Starting_style { base_class; props; _ }
  | Supports_query { base_class; props; _ } ->
      (base_class, props)

let output_ordering_property output =
  let _, props = output_base_class_and_props output in
  Utility.ordering_property props

let builtins_in_property_family order_map property builtins =
  List.filter_map
    (fun output ->
      let base_class, props = output_base_class_and_props output in
      match (base_class, Utility.ordering_property props) with
      | Some cls, Some key when Css.Declaration.equal_prop_key key property ->
          let base = extract_base_utility cls in
          Option.map
            (fun order -> (base, order))
            (Hashtbl.find_opt order_map base)
      | _ -> None)
    builtins

(* Tailwind orders utilities that write the same property by candidate name.
   Built-in values carry distinct numeric suborders in TW, so when a declared
   utility joins one of those property families, normalize that family's
   suborder for this render and let the existing candidate-name tiebreaker
   interleave both kinds of utility. A negative minimum is a built-in prelude,
   such as [sr-only]/[not-sr-only] before ordinary position utilities. Preserve
   that walk and put the declared utility in the preceding property slot. *)
let normalize_declared_property_families order_map builtins extra_outputs =
  List.iter
    (fun (class_name, (priority, _), outputs) ->
      let modifiers, _ = Modifiers.of_string class_name in
      (* A custom-variant expansion also arrives through [extra], already
         carrying the order of the built-in it wraps. It is not a new utility
         joining that property family, so flattening the family around it
         destroys the built-ins' property walk. *)
      if modifiers = [] then
        match List.find_map output_ordering_property outputs with
        | None -> ()
        | Some property ->
            let family =
              builtins_in_property_family order_map property builtins
            in
            let suborder =
              List.fold_left
                (fun acc (_, (p, s)) ->
                  if p <> priority then acc
                  else Some (Option.fold ~none:s ~some:(Int.min s) acc))
                None family
            in
            Option.iter
              (fun suborder ->
                if suborder < 0 then
                  Hashtbl.replace order_map
                    (extract_base_utility class_name)
                    (priority, suborder - 1)
                else (
                  List.iter
                    (fun (base, (p, _)) ->
                      if p = priority then
                        Hashtbl.replace order_map base (priority, suborder))
                    family;
                  Hashtbl.replace order_map
                    (extract_base_utility class_name)
                    (priority, suborder)))
              suborder)
    extra_outputs

let to_css ?(theme = Scheme.default) ?(config = default_config) ?(extra = [])
    tw_classes =
  (* [Rule.outputs ~order_tbl] records each base utility's order under the class
     name it already builds, so [order_of_base] looks it up instead of
     re-parsing the class string while building/sorting rules. *)
  let order_map = Hashtbl.create 256 in
  let builtin_selector_props =
    List.concat_map (Rule.outputs ~theme ~order_tbl:order_map) tw_classes
  in
  (* A declared utility means nothing to the handlers, so its order arrives with
     it and is seeded under the same key [order_of_base] looks up. The key is
     the base name, which a plain utility of the same name shares: seed it only
     when it is free, so an incoming order never moves a rule the handlers
     already placed. *)
  let extra_outputs =
    List.map
      (fun (class_name, order, statements) ->
        let key = extract_base_utility class_name in
        if not (Hashtbl.mem order_map key) then Hashtbl.add order_map key order;
        ( class_name,
          order,
          List.concat_map
            (outputs_of_statement ~base_class:class_name)
            statements ))
      extra
  in
  normalize_declared_property_families order_map builtin_selector_props
    extra_outputs;
  let selector_props =
    builtin_selector_props
    @ List.concat_map (fun (_, _, outputs) -> outputs) extra_outputs
  in
  (* [sorted_rules] (the filter_map/dedup/index/sort pass) feeds both the
     utilities-layer statements and the variable first-usage order, so compute
     it once and share it rather than recomputing inside [layers]. *)
  let verbatim =
    let names = Hashtbl.create 8 in
    List.iter (fun (cls, _, _) -> Hashtbl.replace names cls ()) extra;
    fun cls -> Hashtbl.mem names cls
  in
  let sorted_rules =
    sorted_indexed_rules ~theme ~declared:verbatim order_map selector_props
  in
  let statements = statements_of_sorted_rules ~verbatim sorted_rules in
  let layer_results =
    layers ~theme ~layers:config.layers ~include_base:config.base
      ?forms:config.forms ~selector_props ~sorted_rules tw_classes statements
  in
  Css.concat layer_results

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

let to_inline_style ?(theme = Scheme.default) utilities =
  let styles = List.map (Utility.to_style theme) utilities in
  let all_props = List.rev (List.fold_left collect_declarations [] styles) in
  let non_vars =
    List.filter (fun d -> Css.custom_declaration_name d = None) all_props
  in
  Css.inline_style_of_declarations non_vars
