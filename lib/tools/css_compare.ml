(** CSS comparison utilities for testing using the proper CSS parser *)

type declaration = {
  property_name : string;
  expected_value : string;
  actual_value : string;
}

type 'a diff = Added of 'a | Removed of 'a | Changed of 'a * 'a

type custom_property_definition = {
  name : string;
  syntax : string;
  inherits : bool;
  initial_value : string option;
}

type rule = {
  selector : string;
  change : (Css.declaration list * declaration list) diff;
}

type media_query = { condition : string; change : rule list diff }
type layer = { name : string; change : rule list diff }
type supports_query = { condition : string; change : rule list diff }

type container_query = {
  name : string option;
  condition : string;
  change : rule list diff;
}

type custom_property = {
  name : string;
  change : custom_property_definition diff;
}

type t = {
  rules : rule list;
  media_queries : media_query list;
  layers : layer list;
  supports_queries : supports_query list;
  container_queries : container_query list;
  custom_properties : custom_property list;
}

(* Helper to truncate long values and show context *)
let truncate_with_context ~max_len expected actual =
  let exp_len = String.length expected in
  let act_len = String.length actual in
  if exp_len <= max_len && act_len <= max_len then (expected, actual)
  else
    (* Find the first difference to show context around it *)
    let rec find_diff i =
      if i >= exp_len || i >= act_len then min exp_len act_len
      else if expected.[i] <> actual.[i] then i
      else find_diff (i + 1)
    in
    let diff_pos = find_diff 0 in
    let context_size = (max_len - 10) / 2 in
    (* Reserve space for "..." *)
    let start_pos = max 0 (diff_pos - context_size) in
    let end_pos_exp = min exp_len (start_pos + max_len - 6) in
    let end_pos_act = min act_len (start_pos + max_len - 6) in

    let truncate_string s start_pos end_pos original_len =
      let prefix = if start_pos > 0 then "..." else "" in
      let suffix = if end_pos < original_len then "..." else "" in
      let content = String.sub s start_pos (end_pos - start_pos) in
      prefix ^ content ^ suffix
    in

    let exp_truncated =
      truncate_string expected start_pos end_pos_exp exp_len
    in
    let act_truncated = truncate_string actual start_pos end_pos_act act_len in
    (exp_truncated, act_truncated)

let pp_declaration fmt
    ({ property_name; expected_value; actual_value } : declaration) =
  let exp_short, act_short =
    truncate_with_context ~max_len:80 expected_value actual_value
  in
  Fmt.pf fmt "%s:@,  - %s@,  + %s" property_name exp_short act_short

let pp_diff ?(expected = "Expected") ?(actual = "Actual") pp_value fmt =
  function
  | Added value -> Fmt.pf fmt "Only in %s: %a" actual pp_value value
  | Removed value -> Fmt.pf fmt "Only in %s: %a" expected pp_value value
  | Changed (old_val, new_val) ->
      Fmt.pf fmt "Changed:@,  %s: %a@,  %s: %a" expected pp_value old_val actual
        pp_value new_val

let pp_rule ?(_expected = "Expected") ?(_actual = "Actual") fmt
    { selector; change } =
  let pp_rule_data fmt (declarations, decl_diffs) =
    (* Only show property diffs for conciseness, skip the full declaration
       lists *)
    if decl_diffs <> [] then
      Fmt.pf fmt "%a" (Fmt.list ~sep:(Fmt.any "@,") pp_declaration) decl_diffs
    else if declarations <> [] then
      (* If no diffs but has declarations, show count *)
      Fmt.pf fmt "%d properties" (List.length declarations)
  in
  match change with
  | Added rule_data -> Fmt.pf fmt "+ %s: %a" selector pp_rule_data rule_data
  | Removed rule_data -> Fmt.pf fmt "- %s: %a" selector pp_rule_data rule_data
  | Changed (old_data, new_data) ->
      Fmt.pf fmt "%s:" selector;
      let _, old_diffs = old_data in
      let _, new_diffs = new_data in
      if old_diffs = [] && new_diffs <> [] then
        Fmt.pf fmt "@,  %a" pp_rule_data new_data
      else if old_diffs <> [] && new_diffs = [] then
        Fmt.pf fmt "@,  %a" pp_rule_data old_data
      else Fmt.pf fmt "@,  %a" pp_rule_data new_data

let _pp_media_query ?(expected = "Expected") ?(actual = "Actual") fmt
    ({ condition; change } : media_query) =
  let pp_rules fmt rules =
    Fmt.pf fmt "Rules: %a" (Fmt.list ~sep:(Fmt.any "@,  ") pp_rule) rules
  in
  Fmt.pf fmt "@[<v 2>@media %s: %a@]" condition
    (pp_diff ~expected ~actual pp_rules)
    change

let pp_layer ?(_expected = "Expected") ?(_actual = "Actual") fmt
    ({ name; change } : layer) =
  match change with
  | Added rules -> Fmt.pf fmt "+ @layer %s: %d rules" name (List.length rules)
  | Removed rules -> Fmt.pf fmt "- @layer %s: %d rules" name (List.length rules)
  | Changed (_old_rules, new_rules) ->
      Fmt.pf fmt "@layer %s:@,%a" name
        (Fmt.list ~sep:(Fmt.any "@,") pp_rule)
        new_rules

let _pp_supports_query ?(expected = "Expected") ?(actual = "Actual") fmt
    ({ condition; change } : supports_query) =
  let pp_rules fmt rules =
    Fmt.pf fmt "Rules: %a" (Fmt.list ~sep:(Fmt.any "@,  ") pp_rule) rules
  in
  Fmt.pf fmt "@[<v 2>@supports %s: %a@]" condition
    (pp_diff ~expected ~actual pp_rules)
    change

let _pp_container_query ?(expected = "Expected") ?(actual = "Actual") fmt
    ({ name; condition; change } : container_query) =
  let name_pp = match name with None -> "" | Some n -> n ^ " " in
  let pp_rules fmt rules =
    Fmt.pf fmt "Rules: %a" (Fmt.list ~sep:(Fmt.any "@,  ") pp_rule) rules
  in
  Fmt.pf fmt "@[<v 2>@container %s(%s): %a@]" name_pp condition
    (pp_diff ~expected ~actual pp_rules)
    change

let _pp_custom_property ?(expected = "Expected") ?(actual = "Actual") fmt
    ({ name; change } : custom_property) =
  let pp_custom_property_def fmt
      { name = prop_name; syntax; inherits; initial_value } =
    Fmt.pf fmt "name=%s, syntax=%s, inherits=%b, initial=%s" prop_name syntax
      inherits
      (Option.value ~default:"(none)" initial_value)
  in
  Fmt.pf fmt "@[<v 2>@property %s: %a@]" name
    (pp_diff ~expected ~actual pp_custom_property_def)
    change

let pp ?(expected = "Expected") ?(actual = "Actual") fmt
    {
      rules;
      media_queries;
      layers;
      supports_queries;
      container_queries;
      custom_properties;
    } =
  let[@warning "-27"] _unused = (expected, actual) in
  if
    rules = [] && media_queries = [] && layers = [] && supports_queries = []
    && container_queries = [] && custom_properties = []
  then () (* Output nothing for empty diff *)
  else (
    if rules <> [] then
      Fmt.pf fmt "%a@," (Fmt.list ~sep:(Fmt.any "@,") pp_rule) rules;
    if media_queries <> [] then
      Fmt.pf fmt "@media changes: %d@," (List.length media_queries);
    if layers <> [] then
      Fmt.pf fmt "%a@," (Fmt.list ~sep:(Fmt.any "@,") pp_layer) layers;
    if supports_queries <> [] then
      Fmt.pf fmt "@supports changes: %d@," (List.length supports_queries);
    if container_queries <> [] then
      Fmt.pf fmt "@container changes: %d@," (List.length container_queries);
    if custom_properties <> [] then
      Fmt.pf fmt "@property changes: %d@," (List.length custom_properties))

let equal_declaration_diff (p1 : declaration) (p2 : declaration) =
  p1.property_name = p2.property_name
  && p1.expected_value = p2.expected_value
  && p1.actual_value = p2.actual_value

let equal_diff equal_value d1 d2 =
  match (d1, d2) with
  | Added v1, Added v2 | Removed v1, Removed v2 -> equal_value v1 v2
  | Changed (o1, n1), Changed (o2, n2) -> equal_value o1 o2 && equal_value n1 n2
  | _ -> false

let equal_rule_change r1 r2 =
  let equal_rule_data (props1, diffs1) (props2, diffs2) =
    props1 = props2
    && List.length diffs1 = List.length diffs2
    && List.for_all2 equal_declaration_diff diffs1 diffs2
  in
  r1.selector = r2.selector && equal_diff equal_rule_data r1.change r2.change

let equal_media_query (m1 : media_query) (m2 : media_query) =
  let equal_rules_list rules1 rules2 =
    List.length rules1 = List.length rules2
    && List.for_all2 equal_rule_change rules1 rules2
  in
  m1.condition = m2.condition && equal_diff equal_rules_list m1.change m2.change

let equal_layer (l1 : layer) (l2 : layer) =
  let equal_rules_list rules1 rules2 =
    List.length rules1 = List.length rules2
    && List.for_all2 equal_rule_change rules1 rules2
  in
  l1.name = l2.name && equal_diff equal_rules_list l1.change l2.change

let equal_supports_query (s1 : supports_query) (s2 : supports_query) =
  let equal_rules_list rules1 rules2 =
    List.length rules1 = List.length rules2
    && List.for_all2 equal_rule_change rules1 rules2
  in
  s1.condition = s2.condition && equal_diff equal_rules_list s1.change s2.change

let equal_container_query (c1 : container_query) (c2 : container_query) =
  let equal_rules_list rules1 rules2 =
    List.length rules1 = List.length rules2
    && List.for_all2 equal_rule_change rules1 rules2
  in
  c1.name = c2.name
  && c1.condition = c2.condition
  && equal_diff equal_rules_list c1.change c2.change

let equal_custom_property (p1 : custom_property) (p2 : custom_property) =
  let equal_property_data (def1 : custom_property_definition)
      (def2 : custom_property_definition) =
    def1.name = def2.name && def1.syntax = def2.syntax
    && def1.inherits = def2.inherits
    && def1.initial_value = def2.initial_value
  in
  p1.name = p2.name && equal_diff equal_property_data p1.change p2.change

let equal (d1 : t) (d2 : t) =
  List.length d1.rules = List.length d2.rules
  && List.for_all2 equal_rule_change d1.rules d2.rules
  && List.length d1.media_queries = List.length d2.media_queries
  && List.for_all2 equal_media_query d1.media_queries d2.media_queries
  && List.length d1.layers = List.length d2.layers
  && List.for_all2 equal_layer d1.layers d2.layers
  && List.length d1.supports_queries = List.length d2.supports_queries
  && List.for_all2 equal_supports_query d1.supports_queries d2.supports_queries
  && List.length d1.container_queries = List.length d2.container_queries
  && List.for_all2 equal_container_query d1.container_queries
       d2.container_queries
  && List.length d1.custom_properties = List.length d2.custom_properties
  && List.for_all2 equal_custom_property d1.custom_properties
       d2.custom_properties

let strip_header css =
  (* Strip a leading /*!...*/ header comment with simpler flow to reduce
     nesting *)
  if not (String.starts_with ~prefix:"/*!" css) then css
  else
    let len = String.length css in
    (* Find the end of the opening header comment "*/" starting at index 3 *)
    let rec find_comment_end i =
      if i + 1 >= len then None
      else if css.[i] = '*' && css.[i + 1] = '/' then Some (i + 2)
      else find_comment_end (i + 1)
    in
    match find_comment_end 3 with
    | None -> css
    | Some j ->
        let start_pos = if j < len && css.[j] = '\n' then j + 1 else j in
        if start_pos >= len then ""
        else String.sub css start_pos (len - start_pos)

(* Helper to extract property-value pairs from declarations for comparison *)
let decl_to_prop_value decl =
  let name = Css.declaration_name decl in
  let value = Css.declaration_value ~minify:true decl in
  let value =
    if Css.declaration_is_important decl then value ^ " !important" else value
  in
  (name, value)

let rec collect_supports (acc : (string * Css.statement list) list)
    (stmts : Css.statement list) =
  List.fold_left
    (fun acc stmt ->
      let acc =
        match Css.as_supports stmt with
        | Some (cond, block) -> (cond, block) :: acc
        | None -> acc
      in
      match Css.statement_nested stmt with
      | Some nested -> collect_supports acc nested
      | None -> acc)
    acc stmts

let supports_blocks ast = Css.statements ast |> collect_supports [] |> List.rev

let rec collect_containers
    (acc : (string option * string * Css.statement list) list)
    (stmts : Css.statement list) =
  List.fold_left
    (fun acc stmt ->
      let acc =
        match Css.as_container stmt with
        | Some (name, cond, block) -> (name, cond, block) :: acc
        | None -> acc
      in
      match Css.statement_nested stmt with
      | Some nested -> collect_containers acc nested
      | None -> acc)
    acc stmts

let container_blocks ast =
  Css.statements ast |> collect_containers [] |> List.rev

(* Extract rules that match a class name *)
let extract_rules_with_class css class_name =
  match Css.of_string css with
  | Ok ast ->
      let items = Css.rules ast in
      let rec extract acc = function
        | [] -> List.rev acc
        | stmt :: rest -> (
            match Css.statement_selector stmt with
            | Some selector ->
                let selector_str = Css.Selector.to_string selector in
                if
                  String.length class_name > 0
                  && String.length selector_str >= String.length class_name
                then
                  let rec contains i =
                    if i > String.length selector_str - String.length class_name
                    then false
                    else if
                      String.sub selector_str i (String.length class_name)
                      = class_name
                    then true
                    else contains (i + 1)
                  in
                  if contains 0 then extract (stmt :: acc) rest
                  else extract acc rest
                else extract acc rest
            | None -> extract acc rest)
      in
      extract [] items
  | Error _ -> []

let string_contains str sub =
  let str_len = String.length str in
  let sub_len = String.length sub in
  if sub_len = 0 then true
  else if sub_len > str_len then false
  else
    let rec loop i =
      if i > str_len - sub_len then false
      else if String.sub str i sub_len = sub then true
      else loop (i + 1)
    in
    loop 0

let count_css_class_patterns css class_name =
  let rules = extract_rules_with_class css class_name in
  let selector_strings =
    List.filter_map
      (fun stmt ->
        match Css.statement_selector stmt with
        | Some sel -> Some (Css.Selector.to_string sel)
        | None -> None)
      rules
  in
  let base_pattern = "." ^ class_name in
  let is_base s =
    s = base_pattern
    ||
    let blen = String.length base_pattern in
    String.length s > blen
    && String.sub s 0 blen = base_pattern
    && s.[blen] = ':'
  in
  let base_count = List.length (List.filter is_base selector_strings) in
  let where_count =
    List.length
      (List.filter (fun s -> string_contains s ":where(") selector_strings)
  in
  (base_count, where_count, List.length rules)

let dominant_css_class css =
  match Css.of_string css with
  | Ok ast -> (
      (* Convert stylesheet to a list of rules *)
      let items = Css.rules ast in
      let rec count_classes acc = function
        | [] -> acc
        | stmt :: rest ->
            let selector_str =
              match Css.statement_selector stmt with
              | Some sel -> Css.Selector.to_string sel
              | None -> ""
            in
            (* Split grouped selectors by comma and process each *)
            let individual_selectors =
              String.split_on_char ',' selector_str
              |> List.map String.trim
              |> List.filter (fun s -> s <> "")
            in
            let acc =
              List.fold_left
                (fun acc sel ->
                  (* Extract base class/element from selector, stripping
                     pseudo-classes/elements *)
                  let cls =
                    if String.starts_with ~prefix:"." sel then
                      (* Class selector - strip pseudo-classes *)
                      match String.index_opt sel ' ' with
                      | Some idx -> String.sub sel 0 idx
                      | None -> (
                          match String.index_opt sel ':' with
                          | Some idx -> String.sub sel 0 idx
                          | None -> sel)
                    else
                      (* Element selector - strip pseudo-classes *)
                      match String.index_opt sel ':' with
                      | Some idx -> String.sub sel 0 idx
                      | None -> sel
                  in
                  let count = try List.assoc cls acc with Not_found -> 0 in
                  (cls, count + 1) :: List.remove_assoc cls acc)
                acc individual_selectors
            in
            count_classes acc rest
      in
      let class_counts = count_classes [] items in
      match List.sort (fun (_, c1) (_, c2) -> compare c2 c1) class_counts with
      | (cls, count) :: _ -> (cls, count)
      | [] -> ("", 0))
  | Error _ -> ("", 0)

let extract_base_rules css class_name =
  let rules = extract_rules_with_class css class_name in
  List.filter_map
    (fun stmt ->
      match Css.statement_selector stmt with
      | Some sel -> Some (Css.Selector.to_string sel)
      | None -> None)
    rules

(* Analyze differences between two parsed CSS ASTs, returning structural
   changes *)
(* moved above: props_of_decls *)

let convert_rule_to_strings stmt =
  match Css.as_rule stmt with
  | Some (selector, decls, _) ->
      let selector_str = Css.Selector.to_string selector in
      (selector_str, decls)
  | None -> ("", [])

let rule_diffs rules1 rules2 =
  let get_selector stmt =
    match Css.statement_selector stmt with
    | Some s -> s
    | None -> Css.Selector.universal
  in
  let get_declarations stmt =
    match Css.statement_declarations stmt with Some d -> d | None -> []
  in
  let find_rule sel rules =
    List.find_opt (fun r -> get_selector r = sel) rules
  in

  let added =
    List.filter_map
      (fun r ->
        let sel = get_selector r in
        if not (List.exists (fun r1 -> get_selector r1 = sel) rules1) then
          Some r
        else None)
      rules2
  in
  let removed =
    List.filter_map
      (fun r ->
        let sel = get_selector r in
        if not (List.exists (fun r2 -> get_selector r2 = sel) rules2) then
          Some r
        else None)
      rules1
  in
  let modified =
    List.filter_map
      (fun r1 ->
        let sel = get_selector r1 in
        match find_rule sel rules2 with
        | Some r2 ->
            let d1 = get_declarations r1 in
            let d2 = get_declarations r2 in
            if d1 <> d2 then Some (sel, d1, d2) else None
        | None -> None)
      rules1
  in
  (added, removed, modified)

(* Media and layer diffs intentionally omitted to avoid relying on hidden Css
   internals; we currently focus on top-level rule differences. *)
let media_diffs items1 items2 =
  let added =
    List.filter
      (fun (cond, _) -> not (List.exists (fun (c, _) -> c = cond) items1))
      items2
    |> List.map (fun (cond, _rules2) -> (cond, []))
  in
  let removed =
    List.filter
      (fun (cond, _) -> not (List.exists (fun (c, _) -> c = cond) items2))
      items1
    |> List.map (fun (cond, _rules1) -> (cond, []))
  in
  let modified =
    List.filter_map
      (fun (cond, rules1) ->
        match List.find_opt (fun (c, _) -> c = cond) items2 with
        | Some (_, rules2) ->
            let added_r, removed_r, modified_r = rule_diffs rules1 rules2 in
            if added_r = [] && removed_r = [] && modified_r = [] then None
            else Some (cond, rules1, rules2)
        | None -> None)
      items1
  in
  (added, removed, modified)

(* Extract layers with their actual content from statements *)
let extract_layers_with_content statements =
  List.filter_map
    (fun stmt ->
      match Css.as_layer stmt with
      | Some (name_opt, stmts) ->
          let name = Option.value ~default:"" name_opt in
          Some (name, stmts)
      | None -> None)
    statements

let layer_diffs_with_content css1 css2 =
  let layers1 = extract_layers_with_content (Css.statements css1) in
  let layers2 = extract_layers_with_content (Css.statements css2) in

  let find_layer name layers = List.find_opt (fun (n, _) -> n = name) layers in

  let added =
    List.filter
      (fun (name, _) -> not (List.exists (fun (n, _) -> n = name) layers1))
      layers2
    |> List.map (fun (name, stmts) -> (name, stmts))
  in

  let removed =
    List.filter
      (fun (name, _) -> not (List.exists (fun (n, _) -> n = name) layers2))
      layers1
    |> List.map (fun (name, stmts) -> (name, stmts))
  in

  let modified =
    List.filter_map
      (fun (name1, stmts1) ->
        match find_layer name1 layers2 with
        | Some (_, stmts2) when stmts1 <> stmts2 -> Some (name1, stmts1, stmts2)
        | _ -> None)
      layers1
  in

  (added, removed, modified)

(* Helper function to compute property diffs between two declaration lists *)
let property_diffs decls1 decls2 : declaration list =
  let props1 = List.map decl_to_prop_value decls1 in
  let props2 = List.map decl_to_prop_value decls2 in
  List.fold_left
    (fun acc (p1, v1) ->
      match List.assoc_opt p1 props2 with
      | Some v2 when v1 <> v2 ->
          { property_name = p1; expected_value = v1; actual_value = v2 } :: acc
      | _ -> acc)
    [] props1
  |> List.rev

(* Extract @property declarations from statements *)
let extract_properties statements =
  List.filter_map
    (fun stmt ->
      match Css.as_property stmt with
      | Some (name, syntax, inherits, initial_value) ->
          Some (name, syntax, inherits, initial_value)
      | None -> None)
    statements

let at_property_diffs props1 props2 =
  let find_property name props =
    List.find_opt (fun (n, _, _, _) -> n = name) props
  in

  let added =
    List.filter
      (fun (name, _, _, _) ->
        not (List.exists (fun (n, _, _, _) -> n = name) props1))
      props2
  in
  let removed =
    List.filter
      (fun (name, _, _, _) ->
        not (List.exists (fun (n, _, _, _) -> n = name) props2))
      props1
  in
  let modified =
    List.filter_map
      (fun (name1, syntax1, inherits1, initial1) ->
        match find_property name1 props2 with
        | Some (_, syntax2, inherits2, initial2) ->
            let diffs = [] in
            let diffs =
              if syntax1 <> syntax2 then
                ({
                   property_name = "syntax";
                   expected_value = syntax1;
                   actual_value = syntax2;
                 }
                  : declaration)
                :: diffs
              else diffs
            in
            let diffs =
              if inherits1 <> inherits2 then
                ({
                   property_name = "inherits";
                   expected_value = string_of_bool inherits1;
                   actual_value = string_of_bool inherits2;
                 }
                  : declaration)
                :: diffs
              else diffs
            in
            let diffs =
              if initial1 <> initial2 then
                let our_init = Option.value ~default:"(none)" initial1 in
                let their_init = Option.value ~default:"(none)" initial2 in
                ({
                   property_name = "initial-value";
                   expected_value = our_init;
                   actual_value = their_init;
                 }
                  : declaration)
                :: diffs
              else diffs
            in
            if diffs <> [] then Some (name1, List.rev diffs) else None
        | None -> None)
      props1
  in
  (added, removed, modified)

(* Helper to convert rule diffs to rule_change list *)
let rules_to_changes (added, removed, modified) : rule list =
  let added_changes =
    List.map
      (fun stmt ->
        let sel, props = convert_rule_to_strings stmt in
        { selector = sel; change = Added (props, []) })
      added
  in
  let removed_changes =
    List.map
      (fun stmt ->
        let sel, props = convert_rule_to_strings stmt in
        { selector = sel; change = Removed (props, []) })
      removed
  in
  let modified_changes =
    List.map
      (fun (sel, decls1, decls2) ->
        let sel_str = Css.Selector.to_string sel in
        let prop_diffs = property_diffs decls1 decls2 in
        {
          selector = sel_str;
          change = Changed ((decls1, []), (decls2, prop_diffs));
        })
      modified
  in
  added_changes @ removed_changes @ modified_changes

let diff_ast ~(expected : Css.t) ~(actual : Css.t) =
  let rules1 = Css.rules expected in
  let rules2 = Css.rules actual in
  let rule_changes = rules_to_changes (rule_diffs rules1 rules2) in

  let media1 = Css.media_queries expected in
  let media2 = Css.media_queries actual in
  let at_added, at_removed, at_modified = media_diffs media1 media2 in
  let media_querys =
    let rule_changes_of rules1 rules2 =
      rules_to_changes (rule_diffs rules1 rules2)
    in
    List.map
      (fun (cond, rules) ->
        let rc = rule_changes_of [] rules in
        ({ condition = cond; change = Added rc } : media_query))
      at_added
    @ List.map
        (fun (cond, rules) ->
          let rc = rule_changes_of rules [] in
          ({ condition = cond; change = Removed rc } : media_query))
        at_removed
    @ List.filter_map
        (fun (cond, rules1, rules2) ->
          let rc = rule_changes_of rules1 rules2 in
          if rc = [] then None
          else
            Some
              ({
                 condition = cond;
                 change = Changed (rule_changes_of rules1 [], rc);
               }
                : media_query))
        at_modified
  in

  let layer_added, layer_removed, layer_modified =
    layer_diffs_with_content expected actual
  in
  let layer_diffs =
    List.map
      (fun (name, stmts) ->
        let rules = List.filter Css.is_rule stmts in
        let rc = rules_to_changes (rules, [], []) in
        ({ name; change = Added rc } : layer))
      layer_added
    @ List.map
        (fun (name, stmts) ->
          let rules = List.filter Css.is_rule stmts in
          let rc = rules_to_changes ([], rules, []) in
          ({ name; change = Removed rc } : layer))
        layer_removed
    @ List.filter_map
        (fun (name, stmts1, stmts2) ->
          let rules1 = List.filter Css.is_rule stmts1 in
          let rules2 = List.filter Css.is_rule stmts2 in
          let rc = rules_to_changes (rule_diffs rules1 rules2) in
          if rc = [] then None
          else
            Some
              ({
                 name;
                 change = Changed (rules_to_changes ([], rules1, []), rc);
               }
                : layer))
        layer_modified
  in

  (* Build supports diffs *)
  let supports1 = supports_blocks expected in
  let supports2 = supports_blocks actual in
  let supports_added =
    List.filter
      (fun (c, _) -> not (List.exists (fun (c2, _) -> c = c2) supports1))
      supports2
  in
  let supports_removed =
    List.filter
      (fun (c, _) -> not (List.exists (fun (c2, _) -> c = c2) supports2))
      supports1
  in
  let supports_modified =
    List.filter_map
      (fun (cond, block1) ->
        match List.find_opt (fun (c, _) -> c = cond) supports2 with
        | Some (_, block2) ->
            let rules1 = List.filter Css.is_rule block1 in
            let rules2 = List.filter Css.is_rule block2 in
            let a_r, r_r, m_r = rule_diffs rules1 rules2 in
            if a_r = [] && r_r = [] && m_r = [] then None
            else Some (cond, rules1, rules2)
        | None -> None)
      supports1
  in
  let supports_querys : supports_query list =
    List.map
      (fun (cond, block) ->
        let rules = List.filter Css.is_rule block in
        let rc = rules_to_changes (rules, [], []) in
        ({ condition = cond; change = Added rc } : supports_query))
      supports_added
    @ List.map
        (fun (cond, block) ->
          let rules = List.filter Css.is_rule block in
          let rc = rules_to_changes ([], rules, []) in
          ({ condition = cond; change = Removed rc } : supports_query))
        supports_removed
    @ List.map
        (fun (cond, rules1, rules2) ->
          let rc = rules_to_changes (rule_diffs rules1 rules2) in
          ({
             condition = cond;
             change = Changed (rules_to_changes ([], rules1, []), rc);
           }
            : supports_query))
        supports_modified
  in

  (* Build container diffs *)
  let containers1 = container_blocks expected in
  let containers2 = container_blocks actual in
  let containers_added =
    List.filter
      (fun (n, c, _) ->
        not (List.exists (fun (n2, c2, _) -> n = n2 && c = c2) containers1))
      containers2
  in
  let containers_removed =
    List.filter
      (fun (n, c, _) ->
        not (List.exists (fun (n2, c2, _) -> n = n2 && c = c2) containers2))
      containers1
  in
  let containers_modified =
    List.filter_map
      (fun (name, cond, block1) ->
        match
          List.find_opt (fun (n, c, _) -> n = name && c = cond) containers2
        with
        | Some (_, _, block2) ->
            let rules1 = List.filter Css.is_rule block1 in
            let rules2 = List.filter Css.is_rule block2 in
            let a_r, r_r, m_r = rule_diffs rules1 rules2 in
            if a_r = [] && r_r = [] && m_r = [] then None
            else Some (name, cond, rules1, rules2)
        | None -> None)
      containers1
  in
  let container_querys : container_query list =
    List.map
      (fun (n, cond, block) ->
        let rules = List.filter Css.is_rule block in
        let rc = rules_to_changes (rules, [], []) in
        ({ name = n; condition = cond; change = Added rc } : container_query))
      containers_added
    @ List.map
        (fun (n, cond, block) ->
          let rules = List.filter Css.is_rule block in
          let rc = rules_to_changes ([], rules, []) in
          ({ name = n; condition = cond; change = Removed rc }
            : container_query))
        containers_removed
    @ List.map
        (fun (n, cond, rules1, rules2) ->
          let rc = rules_to_changes (rule_diffs rules1 rules2) in
          ({
             name = n;
             condition = cond;
             change = Changed (rules_to_changes ([], rules1, []), rc);
           }
            : container_query))
        containers_modified
  in

  (* Build property diffs *)
  let properties1 = extract_properties (Css.statements expected) in
  let properties2 = extract_properties (Css.statements actual) in
  let prop_added, prop_removed, prop_modified =
    at_property_diffs properties1 properties2
  in
  let custom_propertys : custom_property list =
    List.map
      (fun (name, syntax, inherits, initial_value) ->
        { name; change = Added { name; syntax; inherits; initial_value } })
      prop_added
    @ List.map
        (fun (name, syntax, inherits, initial_value) ->
          { name; change = Removed { name; syntax; inherits; initial_value } })
        prop_removed
    @ List.map
        (fun (name, _details) ->
          (* Find the old and new property data *)
          let old_name, old_syntax, old_inherits, old_initial =
            List.find (fun (n, _, _, _) -> n = name) properties1
          in
          let new_name, new_syntax, new_inherits, new_initial =
            List.find (fun (n, _, _, _) -> n = name) properties2
          in
          let old_def =
            {
              name = old_name;
              syntax = old_syntax;
              inherits = old_inherits;
              initial_value = old_initial;
            }
          in
          let new_def =
            {
              name = new_name;
              syntax = new_syntax;
              inherits = new_inherits;
              initial_value = new_initial;
            }
          in
          { name; change = Changed (old_def, new_def) })
        prop_modified
  in

  {
    rules = rule_changes;
    media_queries = media_querys;
    layers = layer_diffs;
    supports_queries = supports_querys;
    container_queries = container_querys;
    custom_properties = custom_propertys;
  }

(* Compare two CSS ASTs directly *)
let compare_css css1 css2 =
  let css1 = strip_header css1 in
  let css2 = strip_header css2 in
  match (Css.of_string css1, Css.of_string css2) with
  | Ok expected, Ok actual ->
      let d = diff_ast ~expected ~actual in
      d.rules = [] && d.media_queries = [] && d.layers = []
      && d.supports_queries = [] && d.container_queries = []
      && d.custom_properties = []
  | _ -> css1 = css2

(* Parse two CSS strings and return their diff or parse errors *)
type diff_result =
  | Diff of t
  | Both_errors of Css.parse_error * Css.parse_error
  | Expected_error of Css.parse_error
  | Actual_error of Css.parse_error

let diff ~expected ~actual =
  let expected = strip_header expected in
  let actual = strip_header actual in
  match (Css.of_string expected, Css.of_string actual) with
  | Ok expected_ast, Ok actual_ast ->
      Diff (diff_ast ~expected:expected_ast ~actual:actual_ast)
  | Error e1, Error e2 -> Both_errors (e1, e2)
  | Ok _, Error e -> Actual_error e
  | Error e, Ok _ -> Expected_error e

(* Helper function to show string diff with context and caret *)
let show_string_diff_context ~expected ~actual =
  let find_first_diff s1 s2 =
    let len1 = String.length s1 in
    let len2 = String.length s2 in
    let rec find_diff i =
      if i >= len1 || i >= len2 then if len1 = len2 then None else Some i
      else if s1.[i] <> s2.[i] then Some i
      else find_diff (i + 1)
    in
    find_diff 0
  in
  match find_first_diff expected actual with
  | None -> None
  | Some pos ->
      (* Terminal width constraint - center the diff *)
      let max_width = 78 in
      (* Leave room for "- " prefix *)
      let half_width = max_width / 2 in

      (* Calculate window to center the diff position *)
      let start_pos = max 0 (pos - half_width) in

      (* Extract context window *)
      let extract_context s start_p =
        let len = String.length s in
        let end_p = min len (start_p + max_width) in
        let snippet = String.sub s start_p (end_p - start_p) in
        (* Replace newlines/tabs with spaces for single-line display *)
        String.map
          (fun c -> match c with '\n' | '\r' | '\t' -> ' ' | c -> c)
          snippet
      in

      let context_expected = extract_context expected start_pos in
      let context_actual = extract_context actual start_pos in

      (* Add ellipsis if truncated *)
      let add_ellipsis s orig_start orig_len =
        let prefix = if orig_start > 0 then "..." else "" in
        let suffix =
          if orig_start + String.length s < orig_len then "..." else ""
        in
        prefix ^ s ^ suffix
      in

      let context_expected =
        add_ellipsis context_expected start_pos (String.length expected)
      in
      let context_actual =
        add_ellipsis context_actual start_pos (String.length actual)
      in

      (* Calculate caret position - should be roughly centered *)
      let char_pos =
        let base_pos = pos - start_pos in
        let prefix_len = if start_pos > 0 then 3 else 0 in
        (* account for "..." *)
        base_pos + prefix_len
      in

      Some (context_expected, context_actual, (0, char_pos), pos)

(* Format the result of diff_strings with optional labels *)
let pp_diff_result ?(expected = "Expected") ?(actual = "Actual")
    ?(expected_str = "") ?(actual_str = "") fmt = function
  | Diff d
    when d.rules = [] && d.media_queries = [] && d.layers = []
         && d.supports_queries = [] && d.container_queries = []
         && d.custom_properties = [] ->
      (* No structural differences - could be whitespace only *)
      (* Check if we have the original strings to show a context diff *)
      if expected_str <> "" && actual_str <> "" && expected_str <> actual_str
      then
        match
          show_string_diff_context ~expected:expected_str ~actual:actual_str
        with
        | Some (exp_ctx, act_ctx, (_diff_line, char_pos), pos) ->
            Fmt.pf fmt "CSS has no structural differences\n\n";
            Fmt.pf fmt "String difference at position %d:\n" pos;
            (* Format each line with prefix *)
            let exp_lines = String.split_on_char '\n' exp_ctx in
            let act_lines = String.split_on_char '\n' act_ctx in
            List.iter (fun line -> Fmt.pf fmt "- %s\n" line) exp_lines;
            List.iter (fun line -> Fmt.pf fmt "+ %s\n" line) act_lines;
            (* Create caret line for the diff position *)
            Fmt.pf fmt "  %s^" (String.make char_pos ' ')
        | None -> Fmt.pf fmt "CSS has no structural differences"
      else Fmt.pf fmt "CSS has no structural differences"
  | Diff d -> pp ~expected ~actual fmt d
  | Both_errors (e1, e2) ->
      let err1 = Css.pp_parse_error e1 in
      let err2 = Css.pp_parse_error e2 in
      if String.equal err1 err2 then
        Fmt.pf fmt "Both CSS have same parse error: %s" err1
      else
        Fmt.pf fmt "Parse errors:\n  %s: %s\n  %s: %s" expected err1 actual err2
  | Expected_error e ->
      Fmt.pf fmt "%s CSS parse error: %s" expected (Css.pp_parse_error e)
  | Actual_error e ->
      Fmt.pf fmt "%s CSS parse error: %s" actual (Css.pp_parse_error e)
