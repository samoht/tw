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

(* Colons inside bracket values ([family-name:var(...)]) are not modifier
   separators. *)
let extract_base_utility class_name_no_pseudo =
  let base =
    match List.rev (Parse.split_on_colon class_name_no_pseudo) with
    | last :: _ -> last
    | [] -> class_name_no_pseudo
  in
  (* A leading [!] marks the same utility: [!flex] is [flex]. *)
  if String.length base > 0 && base.[0] = '!' then
    String.sub base 1 (String.length base - 1)
  else base

let selector_props_pairs outputs =
  List.filter_map
    (function
      | order, Regular { selector; props; _ } -> Some (selector, props, order)
      | _ -> None)
    outputs

let is_simple_class_selector sel =
  match sel with Css.Selector.Class _ -> true | _ -> false

let compare_indexed ~filter_custom_props (i1, sel1, _, (prio1, sub1))
    (i2, sel2, _, (prio2, sub2)) =
  let prio_cmp = Int.compare prio1 prio2 in
  if prio_cmp <> 0 then prio_cmp
  else
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

(* Theme tokens belong in the theme layer, so a utility rule that declares one
   to make its own value available has it stripped here. [Css.map] reaches every
   rule at any depth, [@starting-style] included, so [md:starting:p-4] declares
   [--spacing] only in the theme layer. A bare declarations block carries no
   selector, so [Css.map] does not see one and the top level is filtered before
   it. *)
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

let triple typ ~selector ~props ~order ~nested ~base_class ~merge_key =
  (typ, selector, props, order, nested, base_class, merge_key)

(* The [(hover: hover)] media condition is the same for every hover rule. *)
let hover_media : Css.Media.t =
  Css.Media.Cond
    (Css.Media.Feature
       (Css.Media.Plain (Css.Media.Hover, Css.Media.Ident Css.Media.Hover)))

let rule_to_triple (order, rule) =
  match rule with
  | Regular { selector; props; base_class; nested; has_hover; merge_key } ->
      let typ = if has_hover then `Media hover_media else `Regular in
      triple typ ~selector ~props ~order ~nested ~base_class ~merge_key
  | Media_query { condition; selector; props; base_class; nested } ->
      triple (`Media condition) ~selector ~props ~order ~nested ~base_class
        ~merge_key:None
  | Container_query { condition; selector; props; base_class; nested } ->
      triple (`Container condition) ~selector ~props ~order ~nested ~base_class
        ~merge_key:None
  | Starting_style { selector; props; base_class; nested } ->
      triple `Starting ~selector ~props ~order ~nested ~base_class
        ~merge_key:None
  | Supports_query { condition; selector; props; base_class; merge_key; nested }
    ->
      triple (`Supports condition) ~selector ~props ~order ~nested ~base_class
        ~merge_key

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
      let variant_order =
        Rule.compute_variant_order ?theme ~selector:sel base_class
      in
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

let rule_sets_from_selector_props all_rules =
  (* Hover rules sort with the others: [rule_to_triple] turns them into
     [(hover:hover)] media rules, so they take part in the media query sort. *)
  let indexed =
    all_rules |> List.map rule_to_triple |> deduplicate_typed_triples
    |> add_index
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
               Option.value ~default:"<none>" r.base_class;
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

(* A [sm:dark:] utility nests one conditional group inside another, so merging
   the outer run builds a body that is itself a run of equal blocks. Cascade's
   passes work one level at a time, and [~optimize_merged_block] is how a pass
   is handed what to run over the body it just built.

   Only [@media] recurses. Tailwind emits a fallback rule and its [@supports]
   copy per utility, interleaved, where tw's comparator groups the fallbacks
   together and leaves the [@supports] blocks adjacent; collapsing that run
   would be correct CSS and the wrong sheet. The same reasoning keeps
   [merge_distant_media] out of the pipeline below: it hoists a block past the
   statements between, which Tailwind does not do. Canonical diffing normalises
   both regroupings away, so neither shows under [--diff] -- use
   [--diff-mode=tree]. *)
let rec merge_nested_media statements =
  Css.Optimize.merge_consecutive_media ~optimize_merged_block:merge_nested_media
    statements

(* Each utility is wrapped in its own conditional group, so a run of utilities
   sharing one condition arrives here as a run of equal blocks where Tailwind
   has a single one. Cascade collapses each run. [@starting-style] carries no
   condition, so adjacency is its whole gate; the other three compare preludes
   as well. Every pass here merges adjacent blocks, except
   [merge_distant_containers], which reaches past a statement that cannot
   conflict. *)
let statements_of_sorted_rules ?verbatim sorted_rules =
  List.map (indexed_rule_to_statement ?verbatim) sorted_rules
  |> Css.Optimize.merge_consecutive_starting_style
  |> Css.Optimize.merge_consecutive_media
       ~optimize_merged_block:merge_nested_media
  |> Css.Optimize.merge_consecutive_supports
  |> Css.Optimize.merge_consecutive_containers
  |> Css.Optimize.merge_distant_containers

(* The sorted rules give the first-usage order of variables. *)
let sorted_indexed_rules ?theme ?declared all_rules =
  all_rules |> List.map rule_to_triple |> deduplicate_typed_triples
  |> add_index ?theme ?declared |> sort_indexed_blocks

(* Names include the [--] prefix. A var without a property_order sorts at
   1000. *)
let sort_vars_by_property_order metadata vars =
  let get_order name =
    Option.value ~default:1000 (metadata_property_order metadata name)
  in
  (* Decorate-sort-undecorate: [get_order] allocates a [String.sub] per call,
     and a comparator runs it on both operands of every comparison. *)
  vars
  |> List.map (fun name -> (get_order name, name))
  |> List.stable_sort (fun (o1, _) (o2, _) -> Int.compare o1 o2)
  |> List.map snd

(* The vars each utility sets, and those it references that need [@property] (a
   transform references rotate and skew), in utility order. Within one utility
   they sort by property_order, so ring precedes inset-ring whatever the order
   in the CSS value. *)
let var_names_of_sorted_rules metadata sorted_rules =
  sorted_rules
  |> List.concat_map (fun (r : Sort.indexed_rule) ->
      let filtered = filter_utility_properties r.props in
      let set_vars = Css.custom_prop_names filtered in
      let all_vars = Css.vars_of_declarations r.props in
      let ref_vars =
        all_vars
        |> List.filter (fun (Css.V v) ->
            match Var.metadata_of_var v with
            | Some metadata -> Var.metadata_needs_property metadata
            | None -> false)
        |> List.map (fun (Css.V v) -> "--" ^ Css.var_name v)
      in
      sort_vars_by_property_order metadata (set_vars @ ref_vars))

let rule_sets tw_classes =
  rule_sets_from_selector_props
    (List.concat_map Rule.ordered_outputs tw_classes)

let indexed_rules tw_classes =
  List.concat_map Rule.ordered_outputs tw_classes
  |> List.map rule_to_triple |> deduplicate_typed_triples |> add_index

let compare_rules = Sort.compare_indexed_rules
let rule_selector (r : Sort.indexed_rule) = r.selector_str

module Strings = Set.Make (String)

let collect_selector_props tw_classes = List.concat_map Rule.outputs tw_classes

(* Collect the theme-layer tokens a utility's output declares, in first-seen
   order. A compound variant nests its rule under whichever at-rules its
   modifiers ask for, so the walk has to reach a declaration under any of them;
   [Css.Stylesheet.iter_declarations] is that walk. *)
let extract_non_tw_custom_declarations selector_props =
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
        Css.Stylesheet.iter_declarations add_props nested
    | Starting_style { props; _ } | Supports_query { props; _ } ->
        add_props props);
  List.rev !insertion_order

(* Substitute per-render [@theme] token overrides into extracted theme-layer
   declarations: a Theme-role variable whose token is overridden in [theme]
   emits the override value instead of its registered default. A token the block
   removed ([--spacing: initial]) has no declaration left to emit.
   [custom_declaration_name] returns the full [--name] form; the scheme keys
   overrides by the bare name. *)
let apply_token_override theme decl =
  match Css.custom_declaration_name decl with
  | Some full_name
    when String.starts_with ~prefix:"--" full_name
         && String.length full_name > 2 -> (
      let bare = String.sub full_name 2 (String.length full_name - 2) in
      (* An [@theme reference] token is declared somewhere else, so it has no
         declaration here to override. *)
      if Scheme.is_reference_token theme bare then None
      else
        match Scheme.token_override theme bare with
        (* The spacing token is a runtime override point, and the override keeps
           that: a carrier the project re-valued is still dropped when no
           utility reads it, as an inline token's must be. *)
        | Some css
          when String.equal bare (Var.name Theme.spacing_var)
               && Var.is_runtime_declaration decl -> (
            match Css.parse_length css with
            | Some length -> Some (Var.set Theme.spacing_var length)
            | None -> Some (Css.custom_property ~layer:"theme" full_name css))
        | Some css -> Some (Css.custom_property ~layer:"theme" full_name css)
        | None -> if Scheme.is_removed theme bare then None else Some decl)
  | _ -> Some decl

let is_default_family_name = function
  | "default-font-family" | "default-mono-font-family" -> true
  | _ -> false

let names_set_of decls =
  List.fold_left
    (fun acc d ->
      match Css.custom_declaration_name d with
      | Some n -> Strings.add n acc
      | None -> acc)
    Strings.empty decls

let filter_non_duplicates excluded_names decls =
  List.filter
    (fun d ->
      match Css.custom_declaration_name d with
      | Some n -> not (Strings.mem n excluded_names)
      | None -> false)
    decls

(* Font families come before the [default-*] indirections. *)
let split_defaults defaults =
  List.partition
    (fun decl ->
      match Css.custom_declaration_name decl with
      | Some n -> not (is_default_family_name n)
      | None -> false)
    defaults

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

(* [declared] is keyed by bare name, so the leading [--] goes first. *)
let declared_order declared name =
  match name with
  | None -> None
  | Some full ->
      List.assoc_opt
        (Option.value ~default:full (Parse.bare_name full))
        declared

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

let add_statement_var_names names statements =
  Css.Stylesheet.fold_declarations add_declaration_var_names names statements

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

(* The first ident of a [var()]: its name, up to the comma a fallback
   follows. *)
let rec var_name = function
  | [] -> None
  | Cascade.Component.Preserved { kind = Cascade.Token.Ident n; _ } :: _ ->
      Some n
  | Cascade.Component.Preserved { kind = Cascade.Token.Comma; _ } :: _ -> None
  | _ :: rest -> var_name rest

(* The variables a value reads: every [var(--x)] outside a fallback. In
   [var(--a, var(--b))] the rule reads [--a], and [--b] stands in only when
   nothing declares [--a], which a static utility relies on where its theme
   token is optional; so [--b] is not a read. Walked as component values, so a
   [var(] inside a string or a [url()] is data. *)
let rec var_reads acc (components : Cascade.Component.t list) =
  List.fold_left
    (fun acc (c : Cascade.Component.t) ->
      match c with
      | Func { node = { name; arguments; _ }; _ }
        when String.lowercase_ascii name = "var" -> (
          match var_name arguments with Some n -> n :: acc | None -> acc)
      | Func { node = { arguments; _ }; _ } -> var_reads acc arguments
      | Block { node = { value; _ }; _ } -> var_reads acc value
      | Preserved _ -> acc)
    acc components

let declaration_reads acc declarations =
  List.fold_left
    (fun acc declaration ->
      let value = Css.declaration_value declaration in
      var_reads acc
        (Cascade.Parser.list_of_component_values
           (Cascade.Reader.of_string value))
          .value)
    acc declarations

let removed_token_read ~theme ~authored outputs =
  let reads =
    List.fold_left
      (fun acc -> function
        | Regular { props; nested; _ }
        | Media_query { props; nested; _ }
        | Container_query { props; nested; _ }
        | Starting_style { props; nested; _ }
        | Supports_query { props; nested; _ } ->
            Css.Stylesheet.fold_declarations declaration_reads
              (declaration_reads acc props)
              nested)
      [] outputs
  in
  List.find_map
    (fun full ->
      match Parse.bare_name full with
      | Some bare when Scheme.is_removed_token theme bare && not (authored full)
        ->
          Some bare
      | _ -> None)
    (List.rev reads)

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
      match Parse.bare_name full with
      | None -> None
      | Some _ when Strings.mem full exclude -> None
      (* An arbitrary value may name the spacing scale directly, as
         [p-[calc(--spacing(2)+1px)]] does. *)
      | Some "spacing" ->
          let decl =
            Var.set Theme.spacing_var
              (Option.value
                 (Option.bind
                    (Scheme.theme_value (Some theme) "spacing")
                    Css.parse_length)
                 ~default:Theme.spacing_base)
          in
          Some decl
      | Some bare -> (
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
   not independent output. A zero-valued spacing utility, for example, folds its
   value to [0px] and does not read [--spacing]. Non-runtime theme declarations
   retain Tailwind's emission semantics even when the utility folded their
   value. [theme(static)] is the explicit runtime exception: it asks for the
   whole theme whether a utility reads each token or not. *)
let keep_extracted_theme_decl ~theme ~referenced decl =
  theme.Scheme.static_theme
  || (not (Var.is_runtime_declaration decl))
  ||
  match Css.custom_declaration_name decl with
  | Some name -> Strings.mem name referenced
  | None -> false

(* A token an [@theme static] block declared is declared whether or not a
   utility reads it, with the value the block gave it. A namespace reset in such
   a block names no token. *)
let static_block_decls ~theme have =
  List.filter_map
    (fun name ->
      if String.contains name '*' || Strings.mem ("--" ^ name) have then None
      else
        Option.map
          (fun css -> Css.custom_property ~layer:"theme" ("--" ^ name) css)
          (Scheme.token_override theme name))
    theme.Scheme.static_tokens

(* [theme(static)] on the package import emits every theme variable, not only
   the ones a utility used. The palette is by far the biggest part of it. A
   token the project declared in an [\@theme inline] or [\@theme reference]
   block is the exception: those blocks say the sheet declares it nowhere, and
   asking for the whole theme does not undo that. *)
let add_static_theme_decls ~theme extracted =
  let extracted =
    extracted @ static_block_decls ~theme (names_set_of extracted)
  in
  if not theme.Scheme.static_theme then extracted
  else
    let have = names_set_of extracted in
    let registered =
      Scheme.all_default_tokens ()
      |> List.filter (fun (name, _) ->
          not
            (Scheme.is_inline_token theme name
            || Scheme.is_reference_token theme name))
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

let base_layer ?theme ?supports ?(forms_base = false) () =
  let preflight =
    Preflight.stylesheet ?theme ?placeholder_supports:supports ~forms:forms_base
      ()
  in
  let base =
    if forms_base then Css.concat [ preflight; Forms.base_stylesheet () ]
    else preflight
  in
  Css.layer_of ~name:[ "base" ] base

let partition_properties = Property.split
let initial_values_of = Property.initial_values

(* Detects the browsers that need [@property] fallbacks: Safari <15.4 or Firefox
   <128. *)
let browser_detection =
  let open Css.Supports in
  Or
    ( And
        ( property "-webkit-hyphens" "none",
          Not (property "margin-trim" "inline") ),
      And
        ( property "-moz-orient" "inline",
          Not (property "color" "rgb(from red r g b)") ) )

(* Tailwind orders properties in [@supports] and [@property] by first usage in
   the sorted utilities output. Names include the [--] prefix. *)
let first_usage_order set_var_names =
  let seen = Hashtbl.create 16 in
  let idx = ref 0 in
  List.iter
    (fun name ->
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

(* Each family's first usage is the earliest of its members'. *)
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

(* Transforms, gradient, duration and typography order by first usage, every
   other named family by property_order. A variable with no family ([--tw-ease])
   is not direct: its family order is 1000, so it goes last. *)
let uses_direct_property_order = function
  | Some
      ( `Gradient | `Translate | `Rotate | `Skew | `Scale | `Duration
      | `Font_weight | `Leading ) ->
      false
  | Some _ -> true
  | None -> false

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
    Option.value ~default:1000
      (Option.bind
         (metadata_family metadata name)
         (Hashtbl.find_opt family_order))
  in
  let get_first_usage name =
    Option.value ~default:10000 (Hashtbl.find_opt first_usage_order name)
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

(* A project's own [@property] rules get the fallback block Tailwind writes for
   its own: a browser without [@property] never applies an initial value, so
   each is declared under the same browser-detection guard, a non-inheriting
   property on every element and an inheriting one on the root. The first rule
   of a name is the one that counts. *)
let author_property_fallbacks property_rules =
  let inheriting, other =
    Property.dedup property_rules
    |> List.filter_map (fun stmt ->
        match Css.as_property stmt with
        | Some (Css.Property_info { inherits; _ } as info) ->
            Some (inherits, Var.property_initial_declaration info)
        | None -> None)
    |> List.partition fst
  in
  let rule selector = function
    | [] -> []
    | decls -> [ Css.rule ~selector (List.map snd decls) ]
  in
  match
    rule Css.Selector.(list [ Root; host () ]) inheriting
    @ rule
        Css.Selector.(list [ universal; Before Single; After Single; Backdrop ])
        other
  with
  | [] -> []
  | rules ->
      [
        Css.layer ~name:[ "properties" ]
          [ Css.supports ~condition:browser_detection rules ];
      ]

(* The [@property] rules are returned beside the layer, not inside it. *)
let properties_layer metadata fallback_order first_usage_order
    explicit_property_rules_statements =
  let property_rules, other_statements =
    partition_properties explicit_property_rules_statements
  in
  let deduplicated = Property.dedup property_rules in
  let initial_values = initial_values_of deduplicated in

  if deduplicated = [] && initial_values = [] then (Css.empty, [])
  else
    let layer =
      property_layer_content metadata fallback_order first_usage_order
        initial_values other_statements
    in
    (layer, deduplicated)

let set_var_names_from_props props = Css.custom_prop_names props

(* [(vars, set_names, property_rules, metadata)]: every variable a style reads,
   the names it sets, and its explicit [@property] rules. *)
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

let vars_needing_property vars =
  List.filter (fun (Css.V v) -> Var.needs_property_rule v) vars

let property_names_of statements =
  statements
  |> List.filter_map (fun stmt ->
      match Css.as_property stmt with
      | Some (Css.Property_info info) -> Some info.name
      | None -> None)
  |> List.fold_left (fun acc n -> Strings.add n acc) Strings.empty

let property_rules_for vars excluded_names =
  vars
  |> List.filter (fun (Css.V v) ->
      let var_name = "--" ^ Css.var_name v in
      not (Strings.mem var_name excluded_names))
  |> List.filter_map (fun (Css.V v) -> Var.property_rule_of_var v)

(* The explicit [@property] rules, then one for each variable a utility sets
   (not merely references) whose metadata asks for one. *)
let collect_all_property_rules vars_from_utilities set_var_names
    explicit_property_rules_statements =
  let set_names_set =
    List.fold_left (fun acc n -> Strings.add n acc) Strings.empty set_var_names
  in
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

(* The first [@keyframes] of a name wins. *)
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

let flatten_property_rules property_rules_lists =
  property_rules_lists |> List.concat_map Css.statements

(* A [before:] or [after:] variant needs the [--tw-content] [@property] rule. *)
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
   them under any variant, so the check reads the base utility rather than the
   emitted class ([hover:transition]). The classes that start with [transition]
   and set no property to transition read neither: [transition-none], and the
   behaviour utilities. *)
let reads_no_transition_defaults = function
  | "transition-none" | "transition-discrete" | "transition-normal" -> true
  | _ -> false

let has_transition_utility tw_classes =
  let rec check = function
    | Utility.Base b ->
        let c = Utility.class_of_base b in
        String.starts_with ~prefix:"transition" c
        && not (reads_no_transition_defaults c)
    | Utility.Modified (_, u)
    | Utility.Important (_, u)
    | Utility.Aliased (_, u)
    | Utility.Theme_bound (_, u) ->
        check u
    | Utility.Group us -> List.exists check us
  in
  List.exists check tw_classes

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
    (* The base layer reads [--font-sans] and [--font-mono] through the two
       [--default-*-font-family] tokens, which is what puts the stacks in the
       theme layer; a block that took a default token away leaves its stack
       unread, and a utility reading it declares it on its own. *)
    let font_defaults =
      if include_base then
        List.filter
          (fun decl ->
            match Css.custom_declaration_name decl with
            | Some "--font-sans" ->
                not (Scheme.is_removed theme "default-font-family")
            | Some "--font-mono" ->
                not (Scheme.is_removed theme "default-mono-font-family")
            | _ -> true)
          Typography.default_font_family_declarations
      else []
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
  let base_layer =
    base_layer ~theme ~supports:placeholder_supports ~forms_base ()
  in
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

(* Keyframes [spin] sort at the theme order of [--animate-spin]; one with no
   such token sorts last, and a tie sorts by name. *)
let sort_keyframes_by_var_order metadata keyframes =
  keyframes
  |> List.sort (fun (name1, _) (name2, _) ->
      let keyframe_var_order name =
        match metadata_order metadata ("animate-" ^ name) with
        | Some (p, s) -> (p * 1000) + s
        | None -> 1000000
      in
      let order_cmp =
        Int.compare (keyframe_var_order name1) (keyframe_var_order name2)
      in
      if order_cmp <> 0 then order_cmp else String.compare name1 name2)

(* [theme(static)] asks for the whole theme, and the default theme's animations
   name these keyframes whether a utility uses them or not. They follow the ones
   a utility pulled in, which the dedup keeps. *)
let with_static_keyframes ~theme keyframes =
  if not theme.Scheme.static_theme then keyframes
  else keyframes @ List.filter_map Css.as_keyframes Animations.builtin_keyframes

(* The keyframes the utilities name, in the theme's variable order, with the
   static theme's own when the entrypoint asks for them. *)
let sheet_keyframes ~theme metadata styles =
  List.fold_left collect_keyframes [] styles
  |> List.rev
  |> sort_keyframes_by_var_order metadata
  |> with_static_keyframes ~theme

(* The forms base layer is the plugin's [base] strategy: a global reset of
   native form controls. It is opt-in only ([~forms:true]), mirroring Tailwind's
   [\@plugin '@tailwindcss/forms']. The [.form-*] class-strategy utilities emit
   their own per-class styles when used, independent of this flag, so utility
   presence must not auto-enable the global base. The reset is the plugin's, not
   preflight's: a sheet without preflight still carries it, in a base layer of
   its own. *)
let written_base_layer ~include_base ~forms_base base_layer =
  if include_base then (true, base_layer)
  else if forms_base then
    (true, Css.layer_of ~name:[ "base" ] (Forms.base_stylesheet ()))
  else (false, base_layer)

let layers ~theme ~layers ~include_base ?forms ~selector_props ~sorted_rules
    tw_classes statements =
  let styles = List.map (Utility.to_style theme) tw_classes in
  let vars_from_utilities, set_var_names, property_rules_lists, style_metadata =
    extract_vars_and_rules styles
  in
  let metadata = metadata_index sorted_rules style_metadata in
  let all_vars = var_names_of_sorted_rules metadata sorted_rules in
  let first_usage_order = first_usage_order all_vars in
  let base_property_rules = flatten_property_rules property_rules_lists in
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
  let forms_base = Option.value forms ~default:false in
  let individual =
    individual_layers ~theme ~layers ~include_base ~forms_base
      ~has_transition:(has_transition_utility tw_classes)
      ~metadata ~fallback_order first_usage_order selector_props
      all_property_statements statements
  in
  let keyframes = sheet_keyframes ~theme metadata styles in
  let include_base_layer, base_layer =
    written_base_layer ~include_base ~forms_base individual.base_layer
  in
  assemble_all_layers ~layers ~include_base:include_base_layer
    ~properties_layer:individual.properties_layer
    ~theme_layer:individual.theme_layer ~base_layer
    ~utilities_layer:individual.utilities_layer
    ~property_rules_for_end:individual.property_rules ~keyframes ~metadata
    ~fallback_order ~first_usage_order

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
              | Some (name, Some condition, inner) ->
                  (* The parsed query keeps the container's name beside its
                     condition, and the typed condition carries it inside. *)
                  let condition =
                    Option.fold ~none:condition
                      ~some:(fun name -> Css.Container.Named (name, condition))
                      name
                  in
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

(* The order a rule sorts at: the one it was built with, unless
   [normalize_declared_property_families] moved its utility's family for this
   render. *)
let current_order moved (order, output) =
  match Output.base_class output with
  | None -> order
  | Some cls ->
      Option.value ~default:order
        (Hashtbl.find_opt moved (extract_base_utility cls))

let builtins_in_property_family moved property builtins =
  List.filter_map
    (fun ((_, output) as rule) ->
      let base_class, props = output_base_class_and_props output in
      match (base_class, Utility.ordering_property props) with
      | Some cls, Some key when Css.Declaration.equal_prop_key key property ->
          Some (extract_base_utility cls, current_order moved rule)
      | _ -> None)
    builtins

(* Tailwind orders utilities that write the same property by candidate name.
   Built-in values carry distinct numeric suborders in TW, so when a declared
   utility joins one of those property families, normalize that family's
   suborder for this render and let the existing candidate-name tiebreaker
   interleave both kinds of utility. A negative minimum is a built-in prelude,
   such as [sr-only]/[not-sr-only] before ordinary position utilities. Preserve
   that walk and put the declared utility in the preceding property slot. The
   result maps a base utility name to the order its rules move to. *)
let normalize_declared_property_families builtins extra_outputs =
  let moved = Hashtbl.create 8 in
  List.iter
    (fun (class_name, (priority, _), outputs) ->
      let modifiers, _ = Modifiers.of_string class_name in
      (* A custom-variant expansion also arrives through [extra], already
         carrying the order of the built-in it wraps. It is not a new utility
         joining that property family, so flattening the family around it
         destroys the built-ins' property walk. *)
      if modifiers = [] then
        match
          List.find_map (fun (_, o) -> output_ordering_property o) outputs
        with
        | None -> ()
        | Some property ->
            let family = builtins_in_property_family moved property builtins in
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
                  Hashtbl.replace moved
                    (extract_base_utility class_name)
                    (priority, suborder - 1)
                else (
                  List.iter
                    (fun (base, (p, _)) ->
                      if p = priority then
                        Hashtbl.replace moved base (priority, suborder))
                    family;
                  Hashtbl.replace moved
                    (extract_base_utility class_name)
                    (priority, suborder)))
              suborder)
    extra_outputs;
  moved

(* [prefix(tw)] moves every theme token and leaves the [--tw-*] channels a
   utility sets for itself alone. Those two sets are told apart by the name:
   Tailwind spells its own internal variables [--tw-*] and a theme token never
   does, which is why the reference leaves them put. Measured against the pinned
   CLI with [prefix(tw)] and [prefix(app)] alike - [--spacing] becomes
   [--app-spacing], [--tw-shadow] and its [@property] rule do not move.

   A declared token is not the test: the base layer reads
   [--default-font-feature-settings] through a fallback and nothing declares it,
   and the reference prefixes it all the same.

   The rename goes to the printer rather than the sheet because a [var()]
   reference sits inside a typed value, so moving it in the AST would mean
   rebuilding every value that holds one. *)
(* Whether [name] is a theme key: a registered default, a palette colour, a
   token the project's [@theme] declared or a [--default-*] the base layer
   reads. The prefix moves those and nothing else: a [var(--brand)] the author
   wrote into an arbitrary value or an inline token's value is their own. *)
let is_theme_key theme name =
  Option.is_some (Scheme.token_default name)
  || Option.is_some (Scheme.theme_value (Some theme) name)
  || Scheme.is_inline_token theme name
  || Scheme.is_reference_token theme name
  || String.starts_with ~prefix:"default-" name
  || Option.is_some (Color.Handler.theme_color_decl ~theme name)

let theme_token_rename ~theme =
  match theme.Scheme.prefix with
  | None -> None
  | Some prefix ->
      Some
        (fun name ->
          if String.starts_with ~prefix:"tw-" name && String.length name > 3
          then name
          else if is_theme_key theme name then prefix ^ "-" ^ name
          else name)

(* A declared utility means nothing to the handlers, so its order arrives with
   it and goes on every rule it declares. A declared utility is a utility, so
   [important] on the import marks it as it marks a built-in one. *)
let declared_outputs ~theme extra =
  List.map
    (fun (class_name, order, statements) ->
      let statements =
        if theme.Scheme.important then List.map Style.important_stmt statements
        else statements
      in
      ( class_name,
        order,
        List.concat_map
          (fun statement ->
            List.map
              (fun output -> (order, output))
              (outputs_of_statement ~base_class:class_name statement))
          statements ))
    extra

(* The value a reference token carries as its fallback: the project's own, the
   registered default, or the palette's, which [Scheme] does not hold. *)
let reference_value ~theme bare =
  match Scheme.token theme bare with
  | Some _ as value -> value
  | None ->
      Option.map
        (fun decl -> String.trim (Css.declaration_value decl))
        (Color.Handler.theme_color_decl ~theme bare)

(* Every reference the generated sheet makes to a reference token carries its
   value as the fallback, since nothing here declares the token and the sheet
   has to resolve where no declaration reaches: an [@theme reference] block's
   tokens, and under [@reference "tailwindcss"] the whole default theme. The
   author's own CSS is not generated, and keeps the references it wrote. *)
let with_reference_fallbacks ~theme sheet =
  if theme.Scheme.reference_tokens = [] && not theme.Scheme.reference_theme then
    sheet
  else
    Css.add_var_fallbacks
      (fun name ->
        let bare = Option.value ~default:name (Parse.bare_name name) in
        if Scheme.is_reference_token theme bare then reference_value ~theme bare
        else None)
      sheet

let to_css ?(theme = Scheme.default) ?(config = default_config) ?(extra = [])
    tw_classes =
  let builtins = List.concat_map (Rule.ordered_outputs ~theme) tw_classes in
  let extra_outputs = declared_outputs ~theme extra in
  let moved = normalize_declared_property_families builtins extra_outputs in
  let ordered =
    List.map
      (fun ((_, output) as rule) -> (current_order moved rule, output))
      (builtins @ List.concat_map (fun (_, _, outputs) -> outputs) extra_outputs)
  in
  let selector_props = List.map snd ordered in
  (* [sorted_rules] (the filter_map/dedup/index/sort pass) feeds both the
     utilities-layer statements and the variable first-usage order, so compute
     it once and share it rather than recomputing inside [layers]. *)
  let verbatim =
    let names = Hashtbl.create 8 in
    List.iter (fun (cls, _, _) -> Hashtbl.replace names cls ()) extra;
    fun cls -> Hashtbl.mem names cls
  in
  let sorted_rules = sorted_indexed_rules ~theme ~declared:verbatim ordered in
  let statements = statements_of_sorted_rules ~verbatim sorted_rules in
  let layer_results =
    layers ~theme ~layers:config.layers ~include_base:config.base
      ?forms:config.forms ~selector_props ~sorted_rules tw_classes statements
  in
  Css.concat layer_results |> with_reference_fallbacks ~theme

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
