(** CSS tree difference analysis for structural comparison. *)

(* ===== Type Definitions ===== *)

type declaration = {
  property_name : string;
  expected_value : string;
  actual_value : string;
}

type rule_diff =
  | Rule_added of { selector : string; declarations : Css.declaration list }
  | Rule_removed of { selector : string; declarations : Css.declaration list }
  | Rule_content_changed of {
      selector : string;
      old_declarations : Css.declaration list;
      new_declarations : Css.declaration list;
      property_changes : declaration list;
    }
  | Rule_selector_changed of {
      old_selector : string;
      new_selector : string;
      declarations : Css.declaration list;
    }
  | Rule_reordered of { selector : string }

type container_info = {
  container_type : [ `Media | `Layer | `Supports | `Container | `Property ];
  condition : string;
  rules : Css.statement list;
}

type container_diff =
  | Container_added of container_info
  | Container_removed of container_info
  | Container_modified of {
      info : container_info;
      rule_changes : rule_diff list;
      container_changes : container_diff list; (* Nested container changes *)
    }

type t = { rules : rule_diff list; containers : container_diff list }

(* ===== Constants ===== *)

let default_truncation_length = String_diff.default_max_width

(* ===== Helper Functions ===== *)

let is_empty d = d.rules = [] && d.containers = []

(* ===== Pretty Printing Functions ===== *)

(* Print a list of CSS declarations with an action prefix *)
let pp_declarations fmt action decls =
  List.iter
    (fun decl ->
      let prop_name = Css.declaration_name decl in
      (* Use non-minified values to preserve unit differences like 0px vs 0 *)
      let prop_value = Css.declaration_value ~minify:false decl in
      let truncated_value =
        String_diff.truncate_middle default_truncation_length prop_value
      in
      Fmt.pf fmt "    - %s: %s %s@," action prop_name truncated_value)
    decls

let pp_property_diff fmt { property_name; expected_value; actual_value } =
  match String_diff.first_diff_pos expected_value actual_value with
  | None ->
      (* Shouldn't happen but handle gracefully *)
      Fmt.pf fmt "    - modify: %s (no diff detected)@," property_name
  | Some _ ->
      let len1 = String.length expected_value in
      let len2 = String.length actual_value in
      if len1 <= 30 && len2 <= 30 then
        (* Short values: show inline as a modification *)
        Fmt.pf fmt "    - modify: %s %s -> %s@," property_name expected_value
          actual_value
      else
        (* Long values: truncate and show as separate lines *)
        let exp_truncated =
          String_diff.truncate_middle default_truncation_length expected_value
        in
        let act_truncated =
          String_diff.truncate_middle default_truncation_length actual_value
        in
        Fmt.pf fmt "    - modify: %s@," property_name;
        Fmt.pf fmt "        - %s@," exp_truncated;
        Fmt.pf fmt "        + %s@," act_truncated

let pp_property_diffs fmt prop_diffs =
  List.iter (pp_property_diff fmt) prop_diffs

let pp_reorder decls1 decls2 fmt =
  let prop_names1 = List.map Css.declaration_name decls1 in
  let prop_names2 = List.map Css.declaration_name decls2 in
  let same_length = List.length prop_names1 = List.length prop_names2 in
  let sorted1 = List.sort String.compare prop_names1 in
  let sorted2 = List.sort String.compare prop_names2 in
  if same_length && sorted1 = sorted2 && prop_names1 <> prop_names2 then
    let old_str = String.concat ", " prop_names1 in
    let new_str = String.concat ", " prop_names2 in
    let max_inline_length = default_truncation_length in
    if
      String.length old_str <= max_inline_length
      && String.length new_str <= max_inline_length
    then Fmt.pf fmt "    - reorder: [%s] -> [%s]@," old_str new_str
    else (
      Fmt.pf fmt "    - reorder:@,";
      Fmt.pf fmt "        from: [%s]@," old_str;
      Fmt.pf fmt "        to:   [%s]@," new_str)

let pp_rule_diff fmt = function
  | Rule_added { selector; declarations } ->
      Fmt.pf fmt "- %s:@," selector;
      pp_declarations fmt "add" declarations
  | Rule_removed { selector; declarations } ->
      Fmt.pf fmt "- %s:@," selector;
      pp_declarations fmt "remove" declarations
  | Rule_content_changed
      { selector; old_declarations; new_declarations; property_changes } ->
      Fmt.pf fmt "- %s:@," selector;
      (* Show property changes *)
      pp_property_diffs fmt property_changes;
      (* Check for declaration order changes *)
      pp_reorder old_declarations new_declarations fmt
  | Rule_selector_changed { old_selector; new_selector; declarations } ->
      Fmt.pf fmt "- selector changed:@,";
      Fmt.pf fmt "    from: %s@," old_selector;
      Fmt.pf fmt "    to:   %s@," new_selector;
      if declarations <> [] then pp_declarations fmt "declarations" declarations
  | Rule_reordered { selector } -> Fmt.pf fmt "- %s: (reordered)@," selector

let pp_rule_diff_simple fmt = function
  | Rule_added { selector; _ } -> Fmt.pf fmt "Added(%s)" selector
  | Rule_removed { selector; _ } -> Fmt.pf fmt "Removed(%s)" selector
  | Rule_content_changed { selector; _ } -> Fmt.pf fmt "Changed(%s)" selector
  | Rule_selector_changed { old_selector; new_selector; _ } ->
      Fmt.pf fmt "SelectorChanged(%s->%s)" old_selector new_selector
  | Rule_reordered { selector } -> Fmt.pf fmt "Reordered(%s)" selector

let get_meaningful_rules rules =
  List.filter (function Rule_reordered _ -> false | _ -> true) rules

(** Query functions *)
let single_rule_diff (diff : t) =
  match diff.rules with [ rule ] -> Some rule | _ -> None

let rec count_containers_in_list container_type containers =
  List.fold_left
    (fun count cont ->
      let this_count =
        match cont with
        | Container_added { container_type = ct; _ }
        | Container_removed { container_type = ct; _ } ->
            if ct = container_type then 1 else 0
        | Container_modified
            { info = { container_type = ct; _ }; container_changes; _ } ->
            let nested_count =
              count_containers_in_list container_type container_changes
            in
            (if ct = container_type then 1 else 0) + nested_count
      in
      count + this_count)
    0 containers

let count_containers_by_type container_type (diff : t) =
  count_containers_in_list container_type diff.containers

let has_container_added_of_type container_type (diff : t) =
  List.exists
    (function
      | Container_added { container_type = ct; _ } -> ct = container_type
      | _ -> false)
    diff.containers

let has_container_removed_of_type container_type (diff : t) =
  List.exists
    (function
      | Container_removed { container_type = ct; _ } -> ct = container_type
      | _ -> false)
    diff.containers

let container_prefix = function
  | `Media -> "@media"
  | `Layer -> "@layer"
  | `Supports -> "@supports"
  | `Container -> "@container"
  | `Property -> "@property"

let rec pp_container_diff ?(indent = 0) fmt = function
  | Container_added { container_type; condition; rules } ->
      let prefix = container_prefix container_type in
      let indent_str = String.make indent ' ' in
      Fmt.pf fmt "%s- %s %s: (added)@," indent_str prefix condition;
      if rules <> [] then
        List.iter
          (fun stmt ->
            match Css.as_rule stmt with
            | Some (selector, _, _) ->
                Fmt.pf fmt "%s  - %s: (added)@," indent_str
                  (Css.Selector.to_string selector)
            | None -> ())
          rules
  | Container_removed { container_type; condition; rules } ->
      let prefix = container_prefix container_type in
      let indent_str = String.make indent ' ' in
      Fmt.pf fmt "%s- %s %s: (removed)@," indent_str prefix condition;
      if rules <> [] then
        List.iter
          (fun stmt ->
            match Css.as_rule stmt with
            | Some (selector, _, _) ->
                Fmt.pf fmt "%s  - %s: (removed)@," indent_str
                  (Css.Selector.to_string selector)
            | None -> ())
          rules
  | Container_modified
      {
        info = { container_type; condition; rules = _ };
        rule_changes;
        container_changes;
      } ->
      let prefix = container_prefix container_type in
      let indent_str = String.make indent ' ' in
      Fmt.pf fmt "%s- %s %s:@," indent_str prefix condition;
      (* Show rule changes at this level *)
      List.iter
        (fun rule_diff ->
          Fmt.pf fmt "%s  " indent_str;
          pp_rule_diff fmt rule_diff)
        rule_changes;
      (* Show nested container changes with increased indentation *)
      List.iter (pp_container_diff ~indent:(indent + 2) fmt) container_changes

let pp ?(expected = "Expected") ?(actual = "Actual") fmt { rules; containers } =
  if rules = [] && containers = [] then
    Fmt.pf fmt
      "Structural differences detected in nested contexts (e.g., @media inside \
       @layer)@,\
       but no rule-level differences found.@,\
       This may indicate reordering or subtle changes in rule organization."
  else (
    (* Print diff headers like a unified diff *)
    Fmt.pf fmt "--- %s@," expected;
    Fmt.pf fmt "+++ %s@," actual;
    Fmt.pf fmt "@[<v>";
    let meaningful_rules = get_meaningful_rules rules in
    let reordered_rules =
      List.filter (function Rule_reordered _ -> true | _ -> false) rules
    in

    if reordered_rules <> [] then
      Fmt.pf fmt "Rules reordered (%d rules):@," (List.length reordered_rules);

    (* Show the actual differences *)
    List.iter (pp_rule_diff fmt) meaningful_rules;

    (* Show container differences *)
    List.iter (pp_container_diff ~indent:0 fmt) containers;
    Fmt.pf fmt "@]")

(* ===== Tree Diff Computation Functions ===== *)

(* Helper to extract rule information from statements *)
let convert_rule_to_strings stmt =
  match Css.as_rule stmt with
  | Some (selector, decls, _) ->
      let selector_str = Css.Selector.to_string selector in
      (selector_str, decls)
  | None -> ("", [])

(* Helper to extract property-value pairs from declarations for comparison *)
let decl_to_prop_value decl =
  let name = Css.declaration_name decl in
  (* Preserve original formatting to catch differences such as 0px vs 0 *)
  let value = Css.declaration_value ~minify:false decl in
  let value =
    if Css.declaration_is_important decl then value ^ " !important" else value
  in
  (name, value)

(* Normalized signature of a declaration list for comparison/reordering
   checks *)
let decls_signature (decls : Css.declaration list) =
  List.map decl_to_prop_value decls |> List.sort compare

(* Normalize a selector string by sorting comma-separated selector items. This
   ensures we consider ".a,.b" equivalent to ".b,.a" when matching.

   Policy: Selector lists with the same items in different orders are considered
   equivalent for matching purposes. This means: - ".a, .b" and ".b, .a" will
   match as the same selector - Reordering within a list is not considered a
   structural change - This prevents false positives when CSS tools reorder
   selector lists *)
let normalize_selector_string s =
  s |> String.split_on_char ',' |> List.map String.trim
  |> List.filter (fun x -> x <> "")
  |> List.sort String.compare |> String.concat ","

let get_rule_selector stmt =
  match Css.statement_selector stmt with
  | Some s -> s
  | None -> Css.Selector.universal

let selector_key_of_selector sel =
  Css.Selector.to_string sel |> normalize_selector_string

let selector_key_of_stmt stmt =
  selector_key_of_selector (get_rule_selector stmt)

let get_rule_declarations stmt =
  match Css.statement_declarations stmt with Some d -> d | None -> []

(* Generic helper for finding added/removed/modified items between two lists.
   Works with any item type that has a key for comparison. *)
let find_diffs ~(key_of : 'item -> 'key) ~(key_equal : 'key -> 'key -> bool)
    ~(is_empty_diff : 'item -> 'item -> bool) items1 items2 =
  let find_by_key key items =
    List.find_opt (fun item -> key_equal (key_of item) key) items
  in
  let added =
    List.filter
      (fun item2 ->
        not
          (List.exists
             (fun item1 -> key_equal (key_of item1) (key_of item2))
             items1))
      items2
  in
  let removed =
    List.filter
      (fun item1 ->
        not
          (List.exists
             (fun item2 -> key_equal (key_of item1) (key_of item2))
             items2))
      items1
  in
  let modified =
    List.filter_map
      (fun item1 ->
        match find_by_key (key_of item1) items2 with
        | Some item2 when not (is_empty_diff item1 item2) -> Some (item1, item2)
        | _ -> None)
      items1
  in
  (added, removed, modified)

let rules_added_diff rules1 rules2 =
  let key_of = selector_key_of_stmt in
  let key_equal = String.equal in
  let is_empty_diff _ _ = true in
  let added, _removed, _modified =
    find_diffs ~key_of ~key_equal ~is_empty_diff rules1 rules2
  in
  added

let rules_removed_diff rules1 rules2 =
  let key_of = selector_key_of_stmt in
  let key_equal = String.equal in
  let is_empty_diff _ _ = true in
  let _added, removed, _modified =
    find_diffs ~key_of ~key_equal ~is_empty_diff rules1 rules2
  in
  removed

let rules_modified_diff rules1 rules2 =
  (* Create lookup tables for O(1) access *)
  let rules2_by_key = Hashtbl.create (List.length rules2) in
  let rules2_by_props = Hashtbl.create (List.length rules2) in

  (* Populate lookup tables *)
  List.iter
    (fun r ->
      let key = selector_key_of_stmt r in
      let decls = get_rule_declarations r in
      let props = decls_signature decls in

      (* Add to key-based lookup (multiple rules can have same key) *)
      let existing_key =
        try Hashtbl.find rules2_by_key key with Not_found -> []
      in
      Hashtbl.replace rules2_by_key key (r :: existing_key);

      (* Add to props-based lookup (multiple rules can have same props) *)
      let existing_props =
        try Hashtbl.find rules2_by_props props with Not_found -> []
      in
      Hashtbl.replace rules2_by_props props (r :: existing_props))
    rules2;

  (* Track used rules to avoid double-matching *)
  let used_rules = Hashtbl.create (List.length rules2) in

  let rec aux acc = function
    | [] -> List.rev acc
    | r1 :: t1 ->
        let key1 = selector_key_of_stmt r1 in
        let d1 = get_rule_declarations r1 in
        let props1 = decls_signature d1 in

        (* Try exact match by key and declarations first *)
        let exact_match =
          let candidates =
            try Hashtbl.find rules2_by_key key1 with Not_found -> []
          in
          List.find_opt
            (fun r ->
              (not (Hashtbl.mem used_rules r)) && get_rule_declarations r = d1)
            candidates
        in

        let pick =
          match exact_match with
          | Some exact ->
              Hashtbl.replace used_rules exact ();
              let sel1 = get_rule_selector r1 in
              let sel2 = get_rule_selector exact in
              let sel1_str = Css.Selector.to_string sel1 in
              let sel2_str = Css.Selector.to_string sel2 in
              if sel1_str <> sel2_str then Some (sel1, sel2, d1, d1) else None
          | None -> (
              (* Try any rule with same key *)
              let same_key_match =
                let candidates =
                  try Hashtbl.find rules2_by_key key1 with Not_found -> []
                in
                List.find_opt
                  (fun r -> not (Hashtbl.mem used_rules r))
                  candidates
              in
              match same_key_match with
              | Some r2 ->
                  Hashtbl.replace used_rules r2 ();
                  let d2 = get_rule_declarations r2 in
                  Some (get_rule_selector r1, get_rule_selector r2, d1, d2)
              | None -> (
                  (* Fallback: find equivalent rule by properties, but only if
                     selectors share a parent *)
                  let candidates =
                    try Hashtbl.find rules2_by_props props1
                    with Not_found -> []
                  in
                  (* Helper to check if two selectors share a common parent *)
                  let share_parent sel1_str sel2_str =
                    (* Simple heuristic: check if they have a common prefix
                       before the last part *)
                    let parts1 =
                      String.split_on_char ' ' sel1_str |> List.rev
                    in
                    let parts2 =
                      String.split_on_char ' ' sel2_str |> List.rev
                    in
                    match (parts1, parts2) with
                    | _ :: p1_rest, _ :: p2_rest ->
                        List.rev p1_rest = List.rev p2_rest && p1_rest <> []
                    | _ -> false
                  in
                  let sel1_str =
                    Css.Selector.to_string (get_rule_selector r1)
                  in
                  match
                    List.find_opt
                      (fun r ->
                        if Hashtbl.mem used_rules r then false
                        else
                          let sel2_str =
                            Css.Selector.to_string (get_rule_selector r)
                          in
                          share_parent sel1_str sel2_str)
                      candidates
                  with
                  | Some r2 ->
                      Hashtbl.replace used_rules r2 ();
                      let d2 = get_rule_declarations r2 in
                      Some (get_rule_selector r1, get_rule_selector r2, d1, d2)
                  | None -> None))
        in

        let acc = match pick with None -> acc | Some x -> x :: acc in
        aux acc t1
  in
  aux [] rules1

let has_same_selectors rules1 rules2 =
  if List.length rules1 <> List.length rules2 then false
  else
    (* Use hash table for O(n) comparison instead of O(n log n) sorting *)
    let keys1_counts = Hashtbl.create (List.length rules1) in
    List.iter
      (fun r ->
        let key = selector_key_of_stmt r in
        let count = try Hashtbl.find keys1_counts key with Not_found -> 0 in
        Hashtbl.replace keys1_counts key (count + 1))
      rules1;

    let keys2_counts = Hashtbl.create (List.length rules2) in
    List.iter
      (fun r ->
        let key = selector_key_of_stmt r in
        let count = try Hashtbl.find keys2_counts key with Not_found -> 0 in
        Hashtbl.replace keys2_counts key (count + 1))
      rules2;

    (* Check if hash tables are equivalent *)
    try
      Hashtbl.iter
        (fun key count1 ->
          let count2 =
            try Hashtbl.find keys2_counts key with Not_found -> 0
          in
          if count1 <> count2 then raise Exit)
        keys1_counts;

      Hashtbl.iter
        (fun key count2 ->
          let count1 =
            try Hashtbl.find keys1_counts key with Not_found -> 0
          in
          if count1 <> count2 then raise Exit)
        keys2_counts;

      true
    with Exit -> false

let create_ordering_diff rules1 rules2 =
  (* Create maps from selector to declarations for both rule lists *)
  let map1 =
    List.fold_left
      (fun acc rule ->
        let sel = get_rule_selector rule in
        let decls = get_rule_declarations rule in
        (sel, decls) :: acc)
      [] rules1
    |> List.rev
  in
  let map2 =
    List.fold_left
      (fun acc rule ->
        let sel = get_rule_selector rule in
        let decls = get_rule_declarations rule in
        (sel, decls) :: acc)
      [] rules2
    |> List.rev
  in

  (* Find ordering discrepancies by checking if selectors appear in different
     positions *)
  let rec find_ordering_issues acc remaining1 remaining2 =
    match (remaining1, remaining2) with
    | [], [] -> List.rev acc
    | (sel1, decls1) :: rest1, (sel2, decls2) :: rest2 ->
        let sel1_key = selector_key_of_selector sel1 in
        let sel2_key = selector_key_of_selector sel2 in
        if sel1_key <> sel2_key then
          (* Selectors differ at this position - this could be an ordering issue *)
          (* Check if sel1 appears later in remaining2 *)
          let sel1_in_remaining2 =
            List.exists
              (fun (s, _) -> selector_key_of_selector s = sel1_key)
              remaining2
          in
          (* Check if sel2 appears later in remaining1 *)
          let sel2_in_remaining1 =
            List.exists
              (fun (s, _) -> selector_key_of_selector s = sel2_key)
              remaining1
          in

          if sel1_in_remaining2 && sel2_in_remaining1 then
            (* This is likely an ordering issue - both selectors exist but in
               wrong positions. Find the correct declarations for sel1 from
               map2 *)
            (* Prefer an exact declaration match for the same selector key if available *)
            let decls1_from_map2, sel2_opt =
              match
                List.find_opt
                  (fun (s, d) ->
                    selector_key_of_selector s = sel1_key && d = decls1)
                  map2
              with
              | Some (s, d) -> (d, Some s)
              | None -> (
                  match
                    List.find_opt
                      (fun (s, _) -> selector_key_of_selector s = sel1_key)
                      map2
                  with
                  | Some (s, d) -> (d, Some s)
                  | None -> (decls2, None))
            in
            (* Whether pure reordering or actual modification, capture the pair
               so the caller can classify it. *)
            let sel2 = match sel2_opt with Some s -> s | None -> sel1 in
            find_ordering_issues
              ((sel1, sel2, decls1, decls1_from_map2) :: acc)
              rest1 rest2
          else
            (* This might be a true content difference, not just ordering *)
            find_ordering_issues acc rest1 rest2
        else
          (* Selectors match, continue *)
          find_ordering_issues acc rest1 rest2
    | _, _ -> List.rev acc (* Lists have different lengths *)
  in

  find_ordering_issues [] map1 map2

(* no-op: pure rule ordering is handled in handle_structural_diff via
   has_ordering_changes/create_ordering_diff *)

let handle_structural_diff rules1 rules2 =
  (* First, detect selector changes with parent context matching *)
  let all_added_candidates = rules_added_diff rules1 rules2 in
  let all_removed_candidates = rules_removed_diff rules1 rules2 in

  (* Track which rules are matched for selector changes *)
  let matched_added = ref [] in
  let matched_removed = ref [] in
  let selector_changes = ref [] in

  (* Helper function to check if two selectors share meaningful parent
     context *)
  let share_parent_ast sel1 sel2 =
    let module S = Css.Selector in
    (* Extract parent string from a selector *)
    let extract_parent_string sel =
      (* Convert to string and split on spaces to find parent *)
      let sel_str = S.to_string sel in
      (* Split by spaces, considering pseudo-classes stay with their element *)
      let parts = String.split_on_char ' ' sel_str in
      match parts with
      | [] | [ _ ] -> None (* No parent if single element *)
      | parent :: _ ->
          (* Strip pseudo-classes from parent to get base selector *)
          let base_parent =
            (* Remove :hover, :focus, etc. but keep the base class/element *)
            match String.index_opt parent ':' with
            | Some idx -> String.sub parent 0 idx
            | None -> parent
          in
          Some base_parent
    in

    match (extract_parent_string sel1, extract_parent_string sel2) with
    | Some p1, Some p2 -> p1 = p2
    | _ -> false
  in

  (* Try to match removed rules with added rules for selector changes *)
  List.iter
    (fun removed_rule ->
      let removed_sel = get_rule_selector removed_rule in
      let removed_decls = get_rule_declarations removed_rule in
      let removed_props = decls_signature removed_decls in

      (* Look for a matching added rule with same properties but different
         selector *)
      let matching_added =
        List.find_opt
          (fun added_rule ->
            let added_sel = get_rule_selector added_rule in
            let added_decls = get_rule_declarations added_rule in
            let added_props = decls_signature added_decls in

            (* Same properties but different selectors that share parent
               context *)
            removed_props = added_props
            && removed_sel <> added_sel
            && share_parent_ast removed_sel added_sel)
          all_added_candidates
      in

      match matching_added with
      | Some added_rule ->
          let added_sel = get_rule_selector added_rule in

          (* Record this as a selector change *)
          selector_changes :=
            (removed_sel, added_sel, removed_decls, removed_decls)
            :: !selector_changes;
          matched_removed := removed_rule :: !matched_removed;
          matched_added := added_rule :: !matched_added
      | None -> ())
    all_removed_candidates;

  (* Filter out matched rules from add/remove lists *)
  let added =
    List.filter (fun r -> not (List.memq r !matched_added)) all_added_candidates
  in
  let removed =
    List.filter
      (fun r -> not (List.memq r !matched_removed))
      all_removed_candidates
  in

  (* Get other types of modifications (content changes, etc.) but exclude the
     ones we already found *)
  let other_modified = rules_modified_diff rules1 rules2 in

  (* Filter out duplicates - if we already found a selector change, don't
     include it from other_modified *)
  let selector_change_selectors =
    List.map
      (fun (sel1, sel2, _, _) ->
        (Css.Selector.to_string sel1, Css.Selector.to_string sel2))
      !selector_changes
  in

  let filtered_other_modified =
    List.filter
      (fun (sel1, sel2, _, _) ->
        let sel1_str = Css.Selector.to_string sel1 in
        let sel2_str = Css.Selector.to_string sel2 in
        not (List.mem (sel1_str, sel2_str) selector_change_selectors))
      other_modified
  in

  let modified = !selector_changes @ filtered_other_modified in

  let has_structural_changes = added <> [] || removed <> [] || modified <> [] in
  let has_ordering_changes =
    (not has_structural_changes)
    && has_same_selectors rules1 rules2
    && List.map selector_key_of_stmt rules1
       <> List.map selector_key_of_stmt rules2
  in

  let modified_with_order =
    if has_ordering_changes then create_ordering_diff rules1 rules2 @ modified
    else modified
  in

  (added, removed, modified_with_order)

let rule_diffs rules1 rules2 = handle_structural_diff rules1 rules2

(* Helper function to compute property diffs between two declaration lists,
   including added and removed properties *)
let properties_diff decls1 decls2 : declaration list * string list * string list
    =
  let props1 = List.map decl_to_prop_value decls1 in
  let props2 = List.map decl_to_prop_value decls2 in

  (* Find modified properties *)
  let modified =
    List.fold_left
      (fun acc (p1, v1) ->
        match List.assoc_opt p1 props2 with
        | Some v2 when v1 <> v2 ->
            { property_name = p1; expected_value = v1; actual_value = v2 }
            :: acc
        | _ -> acc)
      [] props1
    |> List.rev
  in

  (* Find added properties (in actual but not in expected) *)
  let added =
    List.fold_left
      (fun acc (p2, _v2) ->
        if not (List.mem_assoc p2 props1) then p2 :: acc else acc)
      [] props2
    |> List.rev
  in

  (* Find removed properties (in expected but not in actual) *)
  let removed =
    List.fold_left
      (fun acc (p1, _v1) ->
        if not (List.mem_assoc p1 props2) then p1 :: acc else acc)
      [] props1
    |> List.rev
  in

  (modified, added, removed)

(* Helper functions for converting rule changes - moved here for mutual
   recursion *)
let convert_added_rule stmt =
  let sel, decls = convert_rule_to_strings stmt in
  Rule_added { selector = sel; declarations = decls }

let convert_removed_rule stmt =
  let sel, decls = convert_rule_to_strings stmt in
  Rule_removed { selector = sel; declarations = decls }

let convert_modified_rule (sel1, sel2, decls1, decls2) =
  let sel1_str = Css.Selector.to_string sel1 in
  let sel2_str = Css.Selector.to_string sel2 in
  if sel1_str <> sel2_str then
    Rule_selector_changed
      {
        old_selector = sel1_str;
        new_selector = sel2_str;
        declarations = decls2;
      }
  else if decls1 = decls2 then
    (* Same selector, same declarations in same order - this is pure
       reordering *)
    Rule_reordered { selector = sel1_str }
  else
    (* Check if it's just property reordering (same properties, different
       order) *)
    let property_changes, _added_props, _removed_props =
      properties_diff decls1 decls2
    in
    if property_changes = [] && decls_signature decls1 = decls_signature decls2
    then
      (* Properties are the same but reordered *)
      Rule_reordered { selector = sel1_str }
    else
      Rule_content_changed
        {
          selector = sel1_str;
          old_declarations = decls1;
          new_declarations = decls2;
          property_changes;
        }

(* Assemble rule changes (added/removed/modified) between two rule lists *)
let to_rule_changes rules1 rules2 : rule_diff list =
  let r_added, r_removed, r_modified = rule_diffs rules1 rules2 in
  List.map convert_added_rule r_added
  @ List.map convert_removed_rule r_removed
  @ List.map convert_modified_rule r_modified

(* Mutual recursion declarations *)
let rec media_diff items1 items2 =
  let key_of (cond, _) = cond in
  let key_equal = String.equal in

  (* Check if two media queries have any differences (immediate or nested) *)
  let is_empty_diff (_, rules1) (_, rules2) =
    let added_r, removed_r, modified_r = rule_diffs rules1 rules2 in
    let has_immediate_diffs =
      added_r <> [] || removed_r <> [] || modified_r <> []
    in
    if has_immediate_diffs then false
    else
      (* Also check for nested differences *)
      let nested_diffs = find_nested_differences ~depth:1 rules1 rules2 in
      nested_diffs = []
  in
  let added, removed, modified_pairs =
    find_diffs ~key_of ~key_equal ~is_empty_diff items1 items2
  in

  (* Transform to expected format *)
  let added = List.map (fun (cond, rules) -> (cond, rules)) added in
  let removed = List.map (fun (cond, rules) -> (cond, rules)) removed in
  let modified =
    List.map
      (fun ((cond, rules1), (_, rules2)) -> (cond, rules1, rules2))
      modified_pairs
  in
  (added, removed, modified)

(* Generic helper for processing nested containers *)
and process_nested_containers ~container_type ~extract_fn ~diff_fn ~depth stmts1
    stmts2 =
  let items1 = List.filter_map extract_fn stmts1 in
  let items2 = List.filter_map extract_fn stmts2 in
  let added, removed, modified = diff_fn items1 items2 in

  let diffs = ref [] in

  (* Process added containers *)
  List.iter
    (fun (cond, rules) ->
      diffs :=
        Container_added { container_type; condition = cond; rules } :: !diffs)
    added;

  (* Process removed containers *)
  List.iter
    (fun (cond, rules) ->
      diffs :=
        Container_removed { container_type; condition = cond; rules } :: !diffs)
    removed;

  (* Process modified containers *)
  List.iter
    (fun (cond, rules1, rules2) ->
      let rule_changes = to_rule_changes rules1 rules2 in
      (* Recursively check deeper nesting *)
      let nested_containers =
        find_nested_differences ~depth:(depth + 1) rules1 rules2
      in
      if rule_changes <> [] || nested_containers <> [] then
        diffs :=
          Container_modified
            {
              info = { container_type; condition = cond; rules = rules1 };
              rule_changes;
              container_changes = nested_containers;
            }
          :: !diffs)
    modified;

  !diffs

(* Layer diff function *)
and layer_diff items1 items2 =
  let key_of (name_opt, _) = Option.value ~default:"" name_opt in
  let key_equal = String.equal in
  let is_empty_diff (_, rules1) (_, rules2) =
    let a_r, r_r, m_r = rule_diffs rules1 rules2 in
    let has_immediate_diffs = a_r <> [] || r_r <> [] || m_r <> [] in
    if has_immediate_diffs then false
    else
      (* Also check for nested differences *)
      let nested_diffs = find_nested_differences ~depth:1 rules1 rules2 in
      nested_diffs = []
  in
  let added, removed, modified_pairs =
    find_diffs ~key_of ~key_equal ~is_empty_diff items1 items2
  in
  (* Transform to consistent format with media_diff *)
  let added =
    List.map
      (fun (name_opt, rules) -> (Option.value ~default:"" name_opt, rules))
      added
  in
  let removed =
    List.map
      (fun (name_opt, rules) -> (Option.value ~default:"" name_opt, rules))
      removed
  in
  let modified =
    List.map
      (fun ((name_opt, rules1), (_, rules2)) ->
        (Option.value ~default:"" name_opt, rules1, rules2))
      modified_pairs
  in
  (added, removed, modified)

(* Process layers separately due to different type signature *)
and process_nested_layers ~depth stmts1 stmts2 =
  let items1 = List.filter_map Css.as_layer stmts1 in
  let items2 = List.filter_map Css.as_layer stmts2 in
  let added, removed, modified = layer_diff items1 items2 in

  let diffs = ref [] in

  (* Process added/removed/modified using same logic as media *)
  List.iter
    (fun (name, rules) ->
      diffs :=
        Container_added { container_type = `Layer; condition = name; rules }
        :: !diffs)
    added;

  List.iter
    (fun (name, rules) ->
      diffs :=
        Container_removed { container_type = `Layer; condition = name; rules }
        :: !diffs)
    removed;

  List.iter
    (fun (name, rules1, rules2) ->
      let rule_changes = to_rule_changes rules1 rules2 in
      let nested_containers =
        find_nested_differences ~depth:(depth + 1) rules1 rules2
      in
      if rule_changes <> [] || nested_containers <> [] then
        diffs :=
          Container_modified
            {
              info =
                { container_type = `Layer; condition = name; rules = rules1 };
              rule_changes;
              container_changes = nested_containers;
            }
          :: !diffs)
    modified;

  !diffs

(* Container diff function for @container rules *)
and container_diff items1 items2 =
  let key_of (name_opt, condition, _) =
    (* Use both name and condition as key to distinguish different containers *)
    match name_opt with
    | Some name -> name ^ ":" ^ condition
    | None -> condition
  in
  let key_equal = String.equal in
  let is_empty_diff (_, _, rules1) (_, _, rules2) =
    let a_r, r_r, m_r = rule_diffs rules1 rules2 in
    let has_immediate_diffs = a_r <> [] || r_r <> [] || m_r <> [] in
    if has_immediate_diffs then false
    else
      (* Also check for nested differences *)
      let nested_diffs = find_nested_differences ~depth:1 rules1 rules2 in
      nested_diffs = []
  in
  let added, removed, modified_pairs =
    find_diffs ~key_of ~key_equal ~is_empty_diff items1 items2
  in
  (* Transform to consistent format with media_diff *)
  let added =
    List.map
      (fun (name_opt, condition, rules) ->
        let condition_str =
          match name_opt with
          | Some name -> name ^ " " ^ condition
          | None -> condition
        in
        (condition_str, rules))
      added
  in
  let removed =
    List.map
      (fun (name_opt, condition, rules) ->
        let condition_str =
          match name_opt with
          | Some name -> name ^ " " ^ condition
          | None -> condition
        in
        (condition_str, rules))
      removed
  in
  let modified =
    List.map
      (fun ((name_opt, condition, rules1), (_, _, rules2)) ->
        let condition_str =
          match name_opt with
          | Some name -> name ^ " " ^ condition
          | None -> condition
        in
        (condition_str, rules1, rules2))
      modified_pairs
  in
  (added, removed, modified)

(* Property diff function for @property rules *)
and property_diff items1 items2 =
  let key_of (Css.Property_info { name; _ }) = name in
  let key_equal = String.equal in
  let is_empty_diff prop1 prop2 =
    (* For @property, we consider them empty diff if they're structurally
       equivalent *)
    let (Css.Property_info { name = n1; inherits = i1; _ }) = prop1 in
    let (Css.Property_info { name = n2; inherits = i2; _ }) = prop2 in
    n1 = n2 && i1 = i2
    (* Note: We can't easily compare syntax and initial_value due to existential
       types *)
  in
  let added, removed, modified_pairs =
    find_diffs ~key_of ~key_equal ~is_empty_diff items1 items2
  in
  (* Transform to format compatible with container processing *)
  let added =
    List.map (fun (Css.Property_info { name; _ }) -> (name, [])) added
  in
  let removed =
    List.map (fun (Css.Property_info { name; _ }) -> (name, [])) removed
  in
  let modified =
    List.map
      (fun (Css.Property_info { name; _ }, _) -> (name, [], []))
      modified_pairs
  in
  (added, removed, modified)

(* Process container rules *)
and process_nested_containers_with_name ~depth stmts1 stmts2 =
  let items1 = List.filter_map Css.as_container stmts1 in
  let items2 = List.filter_map Css.as_container stmts2 in
  let added, removed, modified = container_diff items1 items2 in

  let diffs = ref [] in

  List.iter
    (fun (condition, rules) ->
      diffs :=
        Container_added { container_type = `Container; condition; rules }
        :: !diffs)
    added;

  List.iter
    (fun (condition, rules) ->
      diffs :=
        Container_removed { container_type = `Container; condition; rules }
        :: !diffs)
    removed;

  List.iter
    (fun (condition, rules1, rules2) ->
      let rule_changes = to_rule_changes rules1 rules2 in
      let nested_containers =
        find_nested_differences ~depth:(depth + 1) rules1 rules2
      in
      if rule_changes <> [] || nested_containers <> [] then
        diffs :=
          Container_modified
            {
              info = { container_type = `Container; condition; rules = rules1 };
              rule_changes;
              container_changes = nested_containers;
            }
          :: !diffs)
    modified;

  !diffs

(* Process property rules *)
and process_nested_properties ~depth stmts1 stmts2 =
  let items1 = List.filter_map Css.as_property stmts1 in
  let items2 = List.filter_map Css.as_property stmts2 in
  let added, removed, modified = property_diff items1 items2 in

  let diffs = ref [] in

  List.iter
    (fun (name, rules) ->
      diffs :=
        Container_added { container_type = `Property; condition = name; rules }
        :: !diffs)
    added;

  List.iter
    (fun (name, rules) ->
      diffs :=
        Container_removed
          { container_type = `Property; condition = name; rules }
        :: !diffs)
    removed;

  List.iter
    (fun (name, rules1, rules2) ->
      let rule_changes = to_rule_changes rules1 rules2 in
      let nested_containers =
        find_nested_differences ~depth:(depth + 1) rules1 rules2
      in
      (* For @property, always create Container_modified even with empty
         rule_changes since the modification is in the @property declaration
         itself, not in nested rules *)
      diffs :=
        Container_modified
          {
            info =
              { container_type = `Property; condition = name; rules = rules1 };
            rule_changes;
            container_changes = nested_containers;
          }
        :: !diffs)
    modified;

  !diffs

(* Main recursive function for nested differences *)
and find_nested_differences ?(depth = 0) (stmts1 : Css.statement list)
    (stmts2 : Css.statement list) : container_diff list =
  if depth > 3 then [] (* Prevent infinite recursion *)
  else
    (* Process media queries *)
    process_nested_containers ~container_type:`Media ~extract_fn:Css.as_media
      ~diff_fn:media_diff ~depth stmts1 stmts2
    (* Process layers - different type signature *)
    @ process_nested_layers ~depth stmts1 stmts2
    (* Process supports - reuses media_diff since they have the same
       structure *)
    @ process_nested_containers ~container_type:`Supports
        ~extract_fn:Css.as_supports ~diff_fn:media_diff ~depth stmts1 stmts2
    (* Process container queries *)
    @ process_nested_containers_with_name ~depth stmts1 stmts2
    (* Process property declarations *)
    @ process_nested_properties ~depth stmts1 stmts2
    (* Process keyframes animations *)
    @ process_nested_keyframes ~depth stmts1 stmts2
    (* Process font-face rules *)
    @ process_font_face_rules ~depth stmts1 stmts2

(* Process keyframes animations *)
and process_nested_keyframes ~depth:_ stmts1 stmts2 =
  let items1 = List.filter_map Css.as_keyframes stmts1 in
  let items2 = List.filter_map Css.as_keyframes stmts2 in
  let added, removed, modified = keyframes_diff items1 items2 in

  let diffs = ref [] in

  (* Process added keyframes *)
  List.iter
    (fun (name, _frames) ->
      (* Treat entire keyframes block as a container *)
      (* We don't convert frames to rules - just track that the keyframes was added *)
      diffs :=
        Container_added
          {
            container_type = `Layer;
            condition = "@keyframes " ^ name;
            rules = [] (* Frames are not rules, so we leave this empty *);
          }
        :: !diffs)
    added;

  (* Process removed keyframes *)
  List.iter
    (fun (name, _frames) ->
      diffs :=
        Container_removed
          {
            container_type = `Layer;
            condition = "@keyframes " ^ name;
            rules = [];
          }
        :: !diffs)
    removed;

  (* Process modified keyframes *)
  List.iter
    (fun ((name, frames1), (_, frames2)) ->
      (* Diff the keyframe frames directly *)
      let frame_diffs = keyframe_frames_diff frames1 frames2 in
      if frame_diffs <> [] then
        diffs :=
          Container_modified
            {
              info =
                {
                  container_type = `Layer;
                  condition = "@keyframes " ^ name;
                  rules = [];
                };
              rule_changes = frame_diffs;
              container_changes = [];
              (* Keyframes don't have nested containers *)
            }
          :: !diffs)
    modified;

  !diffs

(* Diff keyframe frames *)
and keyframe_frames_diff frames1 frames2 =
  (* Key by frame selector (e.g., "0%", "from", "to", "100%") *)
  let key_of (frame : Css.keyframe) = frame.keyframe_selector in
  let key_equal = String.equal in
  let is_empty_diff (f1 : Css.keyframe) (f2 : Css.keyframe) =
    f1.keyframe_selector = f2.keyframe_selector
    && f1.keyframe_declarations = f2.keyframe_declarations
  in
  let added, removed, modified_pairs =
    find_diffs ~key_of ~key_equal ~is_empty_diff frames1 frames2
  in

  (* Convert frame changes to rule_diff format *)
  let changes = ref [] in

  (* Added frames *)
  List.iter
    (fun (frame : Css.keyframe) ->
      changes :=
        Rule_added
          {
            selector = frame.keyframe_selector;
            declarations = [];
            (* Keyframe declarations have a different type, keep empty *)
          }
        :: !changes)
    added;

  (* Removed frames *)
  List.iter
    (fun (frame : Css.keyframe) ->
      changes :=
        Rule_removed
          {
            selector = frame.keyframe_selector;
            declarations = [];
            (* Keyframe declarations have a different type, keep empty *)
          }
        :: !changes)
    removed;

  (* Modified frames *)
  List.iter
    (fun ((f1 : Css.keyframe), (f2 : Css.keyframe)) ->
      (* For now, just track that the frame changed without detailed property
         diffs *)
      if f1.keyframe_declarations <> f2.keyframe_declarations then
        changes :=
          Rule_content_changed
            {
              selector = f1.keyframe_selector;
              old_declarations = [];
              new_declarations = [];
              property_changes = [];
              (* Can't easily diff due to type mismatch *)
            }
          :: !changes)
    modified_pairs;

  List.rev !changes

(* Keyframes diff function *)
and keyframes_diff items1 items2 =
  let key_of (name, _) = name in
  let key_equal = String.equal in
  let is_empty_diff (name1, frames1) (name2, frames2) =
    name1 = name2 && frames1 = frames2
  in
  find_diffs ~key_of ~key_equal ~is_empty_diff items1 items2

(* Process font-face rules *)
and process_font_face_rules ~depth:_ stmts1 stmts2 =
  let items1 = List.filter_map Css.as_font_face stmts1 in
  let items2 = List.filter_map Css.as_font_face stmts2 in

  (* For font-face, we compare the entire descriptor list *)
  let diffs = ref [] in

  (* If there are any font-face rules in either side, check if they differ *)
  match (items1, items2) with
  | [], [] -> []
  | [], _ ->
      (* Font-face added *)
      diffs :=
        Container_added
          { container_type = `Layer; condition = "@font-face"; rules = [] }
        :: !diffs;
      !diffs
  | _, [] ->
      (* Font-face removed *)
      diffs :=
        Container_removed
          { container_type = `Layer; condition = "@font-face"; rules = [] }
        :: !diffs;
      !diffs
  | descs1 :: _, descs2 :: _ ->
      (* Font-face modified - compare descriptors *)
      if descs1 <> descs2 then
        diffs :=
          Container_modified
            {
              info =
                {
                  container_type = `Layer;
                  condition = "@font-face";
                  rules = [];
                };
              rule_changes = [];
              (* Font-face descriptors are not rules *)
              container_changes = [];
              (* Font-face doesn't have nested containers *)
            }
          :: !diffs;
      !diffs

(* Main diff function *)
let diff ~(expected : Css.t) ~(actual : Css.t) : t =
  let rules1 = Css.statements expected in
  let rules2 = Css.statements actual in
  let added, removed, modified = rule_diffs rules1 rules2 in

  let rule_changes =
    List.map convert_added_rule added
    @ List.map convert_removed_rule removed
    @ List.map convert_modified_rule modified
  in

  (* Delegate all container and nested-container diffs to the generic walker *)
  let containers =
    let stmts1 = Css.statements expected in
    let stmts2 = Css.statements actual in
    find_nested_differences ~depth:0 stmts1 stmts2
  in

  { rules = rule_changes; containers }
