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
  context : string list;
      (* e.g. ["@media (min-width: 768px)"; "@layer utilities"] *)
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

let pp_diff ?(expected = "Expected") ?(actual = "Actual") pp_value fmt =
  function
  | Added value -> Fmt.pf fmt "Only in %s: %a" actual pp_value value
  | Removed value -> Fmt.pf fmt "Only in %s: %a" expected pp_value value
  | Changed (old_val, new_val) ->
      Fmt.pf fmt "Changed:@,  %s: %a@,  %s: %a" expected pp_value old_val actual
        pp_value new_val

(* Truncate long CSS values with context for readability *)
let truncate_with_context max_len value =
  let len = String.length value in
  if len <= max_len then value
  else
    let half_len = (max_len - 3) / 2 in
    (* Account for "..." *)
    let start_part = String.sub value 0 half_len in
    let end_part = String.sub value (len - half_len) half_len in
    start_part ^ "..." ^ end_part

(* Helper to find first character difference between two strings *)
let find_first_diff s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  let rec find i =
    if i >= len1 || i >= len2 then if len1 = len2 then None else Some i
    else if s1.[i] <> s2.[i] then Some i
    else find (i + 1)
  in
  find 0

(* Format a single-line string diff with optional truncation and caret *)
let format_single_line_diff ?(max_width = 60) expected actual =
  match find_first_diff expected actual with
  | None -> None
  | Some diff_pos ->
      let len1 = String.length expected in
      let len2 = String.length actual in

      (* If both strings fit, show them completely *)
      if len1 <= max_width && len2 <= max_width then
        Some (expected, actual, diff_pos)
      else
        (* Calculate window centered on the diff position *)
        let half = max_width / 2 in
        let window_start = max 0 (diff_pos - half) in
        let window_end = min (max len1 len2) (window_start + max_width) in

        (* Helper to extract and format a window from a string *)
        let extract_window s len =
          if window_start >= len then ("...", 3)
            (* Just ellipsis if window starts beyond string *)
          else
            let actual_end = min len window_end in
            let snippet =
              String.sub s window_start (actual_end - window_start)
            in
            let has_prefix = window_start > 0 in
            let has_suffix = window_end < len in
            let prefix = if has_prefix then "..." else "" in
            let suffix = if has_suffix then "..." else "" in
            let full_string = prefix ^ snippet ^ suffix in
            let prefix_len = if has_prefix then 3 else 0 in
            (full_string, prefix_len)
        in

        let s1_display, prefix_len1 = extract_window expected len1 in
        let s2_display, _prefix_len2 = extract_window actual len2 in

        (* Adjust caret position to account for window and prefix *)
        (* The caret should point to the diff position within the displayed window *)
        let adjusted_pos =
          if diff_pos < window_start then 0
            (* Diff is before window, point to start *)
          else if diff_pos >= window_end then
            String.length s1_display
            - 1 (* Diff is after window, point to end *)
          else
            (* Diff is within window: position relative to window start +
               prefix *)
            prefix_len1 + (diff_pos - window_start)
        in

        Some (s1_display, s2_display, adjusted_pos)

(* Print a caret pointing to a specific position *)
let pp_caret ?(indent = 0) fmt pos =
  Fmt.pf fmt "%s^@," (String.make (pos + indent) ' ')

let pp_rule fmt { selector; context; change } =
  let context_str =
    match context with [] -> "" | ctx -> String.concat " > " ctx ^ " > "
  in
  Fmt.pf fmt "- %s%s:@," context_str selector;

  match change with
  | Added (decls, _) ->
      List.iter
        (fun decl ->
          let prop_name = Css.declaration_name decl in
          let prop_value = Css.declaration_value ~minify:true decl in
          let truncated_value = truncate_with_context 60 prop_value in
          Fmt.pf fmt "    - add: %s %s@," prop_name truncated_value)
        decls
  | Removed (decls, _) ->
      List.iter
        (fun decl ->
          let prop_name = Css.declaration_name decl in
          let prop_value = Css.declaration_value ~minify:true decl in
          let truncated_value = truncate_with_context 60 prop_value in
          Fmt.pf fmt "    - remove: %s %s@," prop_name truncated_value)
        decls
  | Changed ((decls1, _), (decls2, prop_diffs)) ->
      (* Show property changes using the new format *)
      List.iter
        (fun { property_name; expected_value; actual_value } ->
          let short_threshold = 30 in
          (* Values under this show on one line *)

          (* Check if both values are short enough for inline display *)
          let exp_len = String.length expected_value in
          let act_len = String.length actual_value in

          if exp_len <= short_threshold && act_len <= short_threshold then
            (* Short values: show inline *)
            Fmt.pf fmt "    - modify: %s %s -> %s@," property_name
              expected_value actual_value
          else
            (* Long values: use shared single-line diff formatting *)
            match format_single_line_diff expected_value actual_value with
            | Some (exp_display, act_display, adjusted_pos) ->
                (* Show with ellipsis and caret *)
                Fmt.pf fmt "    - modify: %s@," property_name;
                Fmt.pf fmt "        - %s@," exp_display;
                Fmt.pf fmt "        + %s@," act_display;
                pp_caret ~indent:10 fmt
                  adjusted_pos (* indent:10 for " + " prefix *)
            | None ->
                (* No diff found but values differ - shouldn't happen *)
                Fmt.pf fmt "    - modify: %s %s -> %s@," property_name
                  (truncate_with_context 60 expected_value)
                  (truncate_with_context 60 actual_value))
        prop_diffs;

      (* Check for declaration order changes *)
      let prop_names1 = List.map (fun d -> Css.declaration_name d) decls1 in
      let prop_names2 = List.map (fun d -> Css.declaration_name d) decls2 in
      let common_props1 =
        List.filter (fun p -> List.mem p prop_names2) prop_names1
      in
      let common_props2 =
        List.filter (fun p -> List.mem p prop_names1) prop_names2
      in
      if common_props1 <> [] && common_props1 <> common_props2 then
        let old_str = String.concat ", " common_props1 in
        let new_str = String.concat ", " common_props2 in
        Fmt.pf fmt "    - reorder: [%s] -> [%s]@," old_str new_str

let _pp_media_query ?(expected = "Expected") ?(actual = "Actual") fmt
    ({ condition; change } : media_query) =
  let pp_rules fmt rules =
    Fmt.pf fmt "Rules: %a" (Fmt.list ~sep:(Fmt.any "@,  ") pp_rule) rules
  in
  Fmt.pf fmt "@[<v 2>@media %s: %a@]" condition
    (pp_diff ~expected ~actual pp_rules)
    change

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
  if
    rules = [] && media_queries = [] && layers = [] && supports_queries = []
    && container_queries = [] && custom_properties = []
  then Fmt.pf fmt "No differences found"
  else (
    (* Git-style diff header *)
    Fmt.pf fmt "--- %s@," expected;
    Fmt.pf fmt "+++ %s@," actual;
    (* All rules (including nested ones) are now flattened with context *)
    List.iter (pp_rule fmt) rules;

    (* Process nested rules from media queries *)
    List.iter
      (fun (mq : media_query) ->
        let media_rules : rule list =
          match mq.change with
          | Added rules | Removed rules -> rules
          | Changed (_, rules) -> rules
        in
        List.iter (pp_rule fmt) media_rules)
      media_queries;

    (* Process nested rules from layers *)
    List.iter
      (fun (layer : layer) ->
        match layer.change with
        | Added rules ->
            Fmt.pf fmt "- @layer %s: (added)@," layer.name;
            List.iter (pp_rule fmt) rules
        | Removed rules ->
            Fmt.pf fmt "- @layer %s: (removed)@," layer.name;
            List.iter (pp_rule fmt) rules
        | Changed (_, rules) ->
            if rules <> [] then (
              Fmt.pf fmt "- @layer %s:@," layer.name;
              List.iter (pp_rule fmt) rules))
      layers;

    (* Process nested rules from support queries *)
    List.iter
      (fun (sq : supports_query) ->
        let supports_rules : rule list =
          match sq.change with
          | Added rules | Removed rules -> rules
          | Changed (_, rules) -> rules
        in
        List.iter (pp_rule fmt) supports_rules)
      supports_queries;

    (* Process nested rules from container queries *)
    List.iter
      (fun (cq : container_query) ->
        let container_rules : rule list =
          match cq.change with
          | Added rules | Removed rules -> rules
          | Changed (_, rules) -> rules
        in
        List.iter (pp_rule fmt) container_rules)
      container_queries;

    (* Process custom properties (@property rules) *)
    List.iter
      (fun (cp : custom_property) ->
        match cp.change with
        | Added def ->
            Fmt.pf fmt "- @property %s:@," cp.name;
            Fmt.pf fmt "    - add: syntax=%s inherits=%b initial=%s@,"
              def.syntax def.inherits
              (Option.value ~default:"(none)" def.initial_value)
        | Removed def ->
            Fmt.pf fmt "- @property %s:@," cp.name;
            Fmt.pf fmt "    - remove: syntax=%s inherits=%b initial=%s@,"
              def.syntax def.inherits
              (Option.value ~default:"(none)" def.initial_value)
        | Changed (old_def, new_def) ->
            Fmt.pf fmt "- @property %s:@," cp.name;
            if old_def.syntax <> new_def.syntax then
              Fmt.pf fmt "    - modify: syntax %s -> %s@," old_def.syntax
                new_def.syntax;
            if old_def.inherits <> new_def.inherits then
              Fmt.pf fmt "    - modify: inherits %b -> %b@," old_def.inherits
                new_def.inherits;
            if old_def.initial_value <> new_def.initial_value then
              let old_init =
                Option.value ~default:"(none)" old_def.initial_value
              in
              let new_init =
                Option.value ~default:"(none)" new_def.initial_value
              in
              Fmt.pf fmt "    - modify: initial-value %s -> %s@," old_init
                new_init)
      custom_properties)

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

(* Helper for comparing rule lists *)
let equal_rules_list rules1 rules2 =
  List.length rules1 = List.length rules2
  && List.for_all2 equal_rule_change rules1 rules2

let equal_media_query (m1 : media_query) (m2 : media_query) =
  m1.condition = m2.condition && equal_diff equal_rules_list m1.change m2.change

let equal_layer (l1 : layer) (l2 : layer) =
  l1.name = l2.name && equal_diff equal_rules_list l1.change l2.change

let equal_supports_query (s1 : supports_query) (s2 : supports_query) =
  s1.condition = s2.condition && equal_diff equal_rules_list s1.change s2.change

let equal_container_query (c1 : container_query) (c2 : container_query) =
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
let rules_to_changes ?(context = []) (added, removed, modified) : rule list =
  let added_changes =
    List.map
      (fun stmt ->
        let sel, props = convert_rule_to_strings stmt in
        { selector = sel; context; change = Added (props, []) })
      added
  in
  let removed_changes =
    List.map
      (fun stmt ->
        let sel, props = convert_rule_to_strings stmt in
        { selector = sel; context; change = Removed (props, []) })
      removed
  in
  let modified_changes =
    List.map
      (fun (sel, decls1, decls2) ->
        let sel_str = Css.Selector.to_string sel in
        let prop_diffs = property_diffs decls1 decls2 in
        {
          selector = sel_str;
          context;
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
  (* Generic helper for building query diffs *)
  let build_simple_query_diffs prefix make_query (added, removed, modified) =
    let make_context cond = [ prefix ^ " " ^ cond ] in
    let rule_changes_of ?(context = []) rules1 rules2 =
      rules_to_changes ~context (rule_diffs rules1 rules2)
    in
    List.map
      (fun (cond, rules) ->
        let context = make_context cond in
        let rc = rule_changes_of ~context [] rules in
        make_query cond (Added rc))
      added
    @ List.map
        (fun (cond, rules) ->
          let context = make_context cond in
          let rc = rule_changes_of ~context rules [] in
          make_query cond (Removed rc))
        removed
    @ List.filter_map
        (fun (cond, rules1, rules2) ->
          let context = make_context cond in
          let rc = rule_changes_of ~context rules1 rules2 in
          if rc = [] then None
          else
            Some
              (make_query cond
                 (Changed (rule_changes_of ~context rules1 [], rc))))
        modified
  in

  let media_querys =
    build_simple_query_diffs "@media"
      (fun cond change -> ({ condition = cond; change } : media_query))
      (at_added, at_removed, at_modified)
  in

  let layer_added, layer_removed, layer_modified =
    layer_diffs_with_content expected actual
  in
  let layer_diffs =
    List.map
      (fun (name, stmts) ->
        let layer_context = [ "@layer " ^ name ] in
        let rules = List.filter Css.is_rule stmts in
        let rc = rules_to_changes ~context:layer_context (rules, [], []) in
        ({ name; change = Added rc } : layer))
      layer_added
    @ List.map
        (fun (name, stmts) ->
          let layer_context = [ "@layer " ^ name ] in
          let rules = List.filter Css.is_rule stmts in
          let rc = rules_to_changes ~context:layer_context ([], rules, []) in
          ({ name; change = Removed rc } : layer))
        layer_removed
    @ List.filter_map
        (fun (name, stmts1, stmts2) ->
          let layer_context = [ "@layer " ^ name ] in
          let rules1 = List.filter Css.is_rule stmts1 in
          let rules2 = List.filter Css.is_rule stmts2 in
          let rc =
            rules_to_changes ~context:layer_context (rule_diffs rules1 rules2)
          in
          if rc = [] then None
          else
            Some
              ({
                 name;
                 change =
                   Changed
                     ( rules_to_changes ~context:layer_context ([], rules1, []),
                       rc );
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
  (* Process supports queries with similar pattern but block extraction *)
  let supports_added_processed =
    List.map
      (fun (cond, block) -> (cond, List.filter Css.is_rule block))
      supports_added
  in
  let supports_removed_processed =
    List.map
      (fun (cond, block) -> (cond, List.filter Css.is_rule block))
      supports_removed
  in

  let supports_querys =
    build_simple_query_diffs "@supports"
      (fun cond change -> ({ condition = cond; change } : supports_query))
      (supports_added_processed, supports_removed_processed, supports_modified)
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
  (* Container queries need special handling due to the name field *)
  let container_querys : container_query list =
    let rule_changes_of ?(context = []) rules1 rules2 =
      rules_to_changes ~context (rule_diffs rules1 rules2)
    in
    List.map
      (fun (n, cond, block) ->
        let rules = List.filter Css.is_rule block in
        let context = [ "@container " ^ cond ] in
        let rc = rule_changes_of ~context [] rules in
        ({ name = n; condition = cond; change = Added rc } : container_query))
      containers_added
    @ List.map
        (fun (n, cond, block) ->
          let rules = List.filter Css.is_rule block in
          let context = [ "@container " ^ cond ] in
          let rc = rule_changes_of ~context rules [] in
          ({ name = n; condition = cond; change = Removed rc }
            : container_query))
        containers_removed
    @ List.filter_map
        (fun (n, cond, rules1, rules2) ->
          let context = [ "@container " ^ cond ] in
          let rc = rule_changes_of ~context rules1 rules2 in
          if rc = [] then None
          else
            Some
              ({
                 name = n;
                 condition = cond;
                 change = Changed (rule_changes_of ~context rules1 [], rc);
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

(* String diff information for character-level differences *)
type string_diff = {
  position : int; (* Character position of first difference *)
  line_expected : int;
  column_expected : int;
  line_actual : int;
  column_actual : int;
  context_before : (string * string) list;
      (* (expected, actual) line pairs before diff *)
  diff_lines : string * string; (* The lines containing the difference *)
  context_after : (string * string) list;
      (* (expected, actual) line pairs after diff *)
}

(* Parse two CSS strings and return their diff or parse errors *)
type diff_result =
  | Diff of t (* CSS AST differences found *)
  | String_diff of string_diff (* No structural diff but strings differ *)
  | No_diff (* Strings are identical *)
  | Both_errors of Css.parse_error * Css.parse_error
  | Expected_error of Css.parse_error
  | Actual_error of Css.parse_error

let is_empty d =
  d.rules = [] && d.media_queries = [] && d.layers = []
  && d.supports_queries = [] && d.container_queries = []
  && d.custom_properties = []

let show_string_diff_context ~expected ~actual =
  match find_first_diff expected actual with
  | None -> None
  | Some pos ->
      (* Split into lines to find the line containing the diff *)
      let lines_expected = String.split_on_char '\n' expected in
      let lines_actual = String.split_on_char '\n' actual in

      (* Find which line contains position pos *)
      let find_line_and_column lines pos =
        let rec find line_num char_count = function
          | [] -> (line_num - 1, pos - char_count, [])
          | line :: rest ->
              let line_len = String.length line + 1 in
              (* +1 for newline *)
              if char_count + line_len > pos then
                (line_num, pos - char_count, line :: rest)
              else find (line_num + 1) (char_count + line_len) rest
        in
        find 0 0 lines
      in

      let line_exp, col_exp, remaining_exp =
        find_line_and_column lines_expected pos
      in
      let line_act, col_act, remaining_act =
        find_line_and_column lines_actual pos
      in

      (* Get context lines (3 before, 3 after) *)
      let context_size = 3 in

      let get_context_before lines line_num =
        let rec take n lines =
          if n <= 0 || lines = [] then []
          else match lines with [] -> [] | h :: t -> h :: take (n - 1) t
        in
        let before_lines = take line_num lines in
        let context_start = max 0 (List.length before_lines - context_size) in
        let rec drop n lst =
          if n <= 0 then lst
          else match lst with [] -> [] | _ :: t -> drop (n - 1) t
        in
        drop context_start before_lines
      in

      let context_before_exp = get_context_before lines_expected line_exp in
      let context_before_act = get_context_before lines_actual line_act in

      (* Pair up context lines *)
      let rec zip l1 l2 =
        match (l1, l2) with
        | [], [] -> []
        | h1 :: t1, [] -> (h1, "") :: zip t1 []
        | [], h2 :: t2 -> ("", h2) :: zip [] t2
        | h1 :: t1, h2 :: t2 -> (h1, h2) :: zip t1 t2
      in

      let context_before = zip context_before_exp context_before_act in

      (* Get the diff lines *)
      let diff_line_exp = match remaining_exp with [] -> "" | h :: _ -> h in
      let diff_line_act = match remaining_act with [] -> "" | h :: _ -> h in

      (* Get context after *)
      let context_after_exp =
        match remaining_exp with
        | [] -> []
        | _ :: t ->
            let rec take n = function
              | [] -> []
              | _ when n <= 0 -> []
              | h :: t -> h :: take (n - 1) t
            in
            take context_size t
      in
      let context_after_act =
        match remaining_act with
        | [] -> []
        | _ :: t ->
            let rec take n = function
              | [] -> []
              | _ when n <= 0 -> []
              | h :: t -> h :: take (n - 1) t
            in
            take context_size t
      in

      let context_after = zip context_after_exp context_after_act in

      Some
        {
          position = pos;
          line_expected = line_exp;
          column_expected = col_exp;
          line_actual = line_act;
          column_actual = col_act;
          context_before;
          diff_lines = (diff_line_exp, diff_line_act);
          context_after;
        }

let diff ~expected ~actual =
  let expected = strip_header expected in
  let actual = strip_header actual in
  match (Css.of_string expected, Css.of_string actual) with
  | Ok expected_ast, Ok actual_ast ->
      let structural_diff =
        diff_ast ~expected:expected_ast ~actual:actual_ast
      in
      if is_empty structural_diff then
        (* No structural differences, check string differences *)
        if expected = actual then No_diff
        else
          match show_string_diff_context ~expected ~actual with
          | Some sdiff -> String_diff sdiff
          | None -> No_diff (* Shouldn't happen if strings differ *)
      else Diff structural_diff
  | Error e1, Error e2 -> Both_errors (e1, e2)
  | Ok _, Error e -> Actual_error e
  | Error e, Ok _ -> Expected_error e

(* Pretty-print a string diff in git unified diff format *)
let pp_string_diff fmt (sdiff : string_diff) =
  Fmt.pf fmt "@[<v>@@ -%d,%d +%d,%d @@@,"
    (max 1 (sdiff.line_expected - List.length sdiff.context_before))
    (List.length sdiff.context_before + 1 + List.length sdiff.context_after)
    (max 1 (sdiff.line_actual - List.length sdiff.context_before))
    (List.length sdiff.context_before + 1 + List.length sdiff.context_after);

  (* Print context before *)
  List.iter
    (fun (exp, act) ->
      if exp = act then Fmt.pf fmt " %s@," exp (* Common line *)
      else (
        if exp <> "" then Fmt.pf fmt "-%s@," exp;
        if act <> "" then Fmt.pf fmt "+%s@," act))
    sdiff.context_before;

  (* Print the diff lines with caret if on same line *)
  let diff_exp, diff_act = sdiff.diff_lines in
  Fmt.pf fmt "-%s@," diff_exp;
  if sdiff.line_expected = sdiff.line_actual then
    (* Show caret for single-line diffs *)
    pp_caret ~indent:1 fmt sdiff.column_expected;
  Fmt.pf fmt "+%s@," diff_act;

  (* Print context after *)
  List.iter
    (fun (exp, act) ->
      if exp = act then Fmt.pf fmt " %s@," exp
      else (
        if exp <> "" then Fmt.pf fmt "-%s@," exp;
        if act <> "" then Fmt.pf fmt "+%s@," act))
    sdiff.context_after;
  Fmt.pf fmt "@]"

(* Format the result of diff with optional labels *)
let pp_diff_result ?(expected = "Expected") ?(actual = "Actual") fmt = function
  | Diff d ->
      (* Show structural differences *)
      pp ~expected ~actual fmt d
  | String_diff sdiff ->
      Fmt.pf fmt
        "@[<v>CSS has no structural differences but strings differ:@,@,";
      pp_string_diff fmt sdiff
  | No_diff ->
      (* No output for identical files *)
      ()
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

let pp_stats ~expected_str ~actual_str fmt = function
  | Diff d ->
      Fmt.pf fmt "@[<v>CSS strings differ: %d chars vs %d chars@,"
        (String.length actual_str)
        (String.length expected_str);
      Fmt.pf fmt
        "Diff found: %d rules, %d media, %d layers, %d supports, %d \
         containers, %d @property@]"
        (List.length d.rules)
        (List.length d.media_queries)
        (List.length d.layers)
        (List.length d.supports_queries)
        (List.length d.container_queries)
        (List.length d.custom_properties)
  | String_diff sdiff ->
      Fmt.pf fmt "@[<v>CSS strings differ at position %d (line %d, col %d)@]"
        sdiff.position sdiff.line_expected sdiff.column_expected
  | No_diff -> Fmt.pf fmt "CSS files are identical"
  | Both_errors _ -> Fmt.pf fmt "Both CSS files have parse errors"
  | Expected_error _ -> Fmt.pf fmt "Expected CSS has parse error"
  | Actual_error _ -> Fmt.pf fmt "Actual CSS has parse error"
