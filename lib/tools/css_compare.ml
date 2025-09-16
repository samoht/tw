(** CSS comparison utilities for testing using the proper CSS parser *)

type property_diff = {
  property : string;
  our_value : string;
  their_value : string;
}

type change = Added | Removed | Modified of property_diff list

type rule_change = {
  selector : string;
  change : change;
  properties : (string * string) list; (* For added/removed rules *)
}

type media_change = {
  condition : string;
  change : change;
  rules : rule_change list;
}

type layer_change = { name : string; change : change; rules : rule_change list }

type t = {
  rules : rule_change list;
  media : media_change list;
  layers : layer_change list;
}

let pp_property_diff fmt { property; our_value; their_value } =
  Fmt.pf fmt "@[<v 2>Property %s:@,Our:   %s@,Their: %s@]" property our_value
    their_value

let pp_change fmt = function
  | Added -> Fmt.pf fmt "Added"
  | Removed -> Fmt.pf fmt "Removed"
  | Modified props ->
      Fmt.pf fmt "Modified:@,  %a"
        (Fmt.list ~sep:(Fmt.any "@,  ") pp_property_diff)
        props

let pp_rule_change fmt { selector; change; properties } =
  Fmt.pf fmt "@[<v 2>%s: %a" selector pp_change change;
  (match change with
  | (Added | Removed) when properties <> [] ->
      Fmt.pf fmt "@,Properties:@,  %a"
        (Fmt.list ~sep:(Fmt.any "@,  ") (fun fmt (p, v) ->
             Fmt.pf fmt "%s: %s" p v))
        properties
  | _ -> ());
  Fmt.pf fmt "@]"

let pp_media_change fmt { condition; change; rules } =
  Fmt.pf fmt "@[<v 2>@media %s: %a" condition pp_change change;
  if rules <> [] then
    Fmt.pf fmt "@,Rules:@,  %a"
      (Fmt.list ~sep:(Fmt.any "@,  ") pp_rule_change)
      rules;
  Fmt.pf fmt "@]"

let pp_layer_change fmt { name; change; rules } =
  Fmt.pf fmt "@[<v 2>@layer %s: %a" name pp_change change;
  if rules <> [] then
    Fmt.pf fmt "@,Rules:@,  %a"
      (Fmt.list ~sep:(Fmt.any "@,  ") pp_rule_change)
      rules;
  Fmt.pf fmt "@]"

let pp fmt { rules; media; layers } =
  if rules = [] && media = [] && layers = [] then ()
    (* Output nothing for empty diff *)
  else (
    Fmt.pf fmt "@[<v 2>CSS Diff:@,";
    if rules <> [] then
      Fmt.pf fmt "Rules:@,%a@,"
        (Fmt.list ~sep:(Fmt.any "@,") pp_rule_change)
        rules;
    if media <> [] then
      Fmt.pf fmt "Media:@,%a@,"
        (Fmt.list ~sep:(Fmt.any "@,") pp_media_change)
        media;
    if layers <> [] then
      Fmt.pf fmt "Layers:@,%a@,"
        (Fmt.list ~sep:(Fmt.any "@,") pp_layer_change)
        layers;
    Fmt.pf fmt "@]")

let equal_property_diff p1 p2 =
  p1.property = p2.property
  && p1.our_value = p2.our_value
  && p1.their_value = p2.their_value

let equal_change c1 c2 =
  match (c1, c2) with
  | Added, Added | Removed, Removed -> true
  | Modified props1, Modified props2 ->
      List.length props1 = List.length props2
      && List.for_all2 equal_property_diff props1 props2
  | _ -> false

let equal_rule_change r1 r2 =
  r1.selector = r2.selector
  && equal_change r1.change r2.change
  && r1.properties = r2.properties

let equal_media_change m1 m2 =
  m1.condition = m2.condition
  && equal_change m1.change m2.change
  && List.length m1.rules = List.length m2.rules
  && List.for_all2 equal_rule_change m1.rules m2.rules

let equal_layer_change l1 l2 =
  l1.name = l2.name
  && equal_change l1.change l2.change
  && List.length l1.rules = List.length l2.rules
  && List.for_all2 equal_rule_change l1.rules l2.rules

let equal d1 d2 =
  List.length d1.rules = List.length d2.rules
  && List.for_all2 equal_rule_change d1.rules d2.rules
  && List.length d1.media = List.length d2.media
  && List.for_all2 equal_media_change d1.media d2.media
  && List.length d1.layers = List.length d2.layers
  && List.for_all2 equal_layer_change d1.layers d2.layers

let strip_header css =
  (* Strip a leading /*!...*/ header comment *)
  if String.starts_with ~prefix:"/*!" css then
    let len = String.length css in
    let rec find_end i =
      if i + 1 >= len then None
      else if css.[i] = '*' && css.[i + 1] = '/' then Some (i + 2)
      else find_end (i + 1)
    in
    match find_end 3 with
    | Some j when j < len ->
        (* Skip the header and optionally a following newline *)
        let start_pos = if css.[j] = '\n' then j + 1 else j in
        String.sub css start_pos (len - start_pos)
    | _ -> css
  else css

(* Compare two CSS ASTs directly *)
let compare_css css1 css2 =
  let css1 = strip_header css1 in
  let css2 = strip_header css2 in

  (* Use the CSS parser to parse both *)
  match (Css.of_string css1, Css.of_string css2) with
  | Ok ast1, Ok ast2 ->
      (* Direct AST comparison *)
      ast1 = ast2
  | _ ->
      (* If either fails to parse, fall back to string comparison *)
      css1 = css2

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
  (* Count selectors that are exactly .classname or start with .classname: (for
     pseudo-classes) *)
  let base_pattern = "." ^ class_name in
  let base_count =
    List.length
      (List.filter
         (fun s ->
           s = base_pattern
           || String.length s > String.length base_pattern
              && String.sub s 0 (String.length base_pattern) = base_pattern
              && s.[String.length base_pattern] = ':')
         selector_strings)
  in
  let where_count =
    let contains str sub =
      let str_len = String.length str in
      let sub_len = String.length sub in
      if sub_len = 0 then true
      else if sub_len > str_len then false
      else
        let rec check i =
          if i > str_len - sub_len then false
          else if String.sub str i sub_len = sub then true
          else check (i + 1)
        in
        check 0
    in
    List.length (List.filter (fun s -> contains s ":where(") selector_strings)
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

let show_css_property_differences add_line selector props1 props2 =
  let p1_map = List.fold_left (fun acc (k, v) -> (k, v) :: acc) [] props1 in
  let p2_map = List.fold_left (fun acc (k, v) -> (k, v) :: acc) [] props2 in

  add_line (Fmt.str "Selector: %s" selector);

  (* Properties only in first *)
  List.iter
    (fun (k, v) ->
      if not (List.mem_assoc k p2_map) then
        add_line (Fmt.str "  - Only in ours: %s: %s" k v))
    p1_map;

  (* Properties only in second *)
  List.iter
    (fun (k, v) ->
      if not (List.mem_assoc k p1_map) then
        add_line (Fmt.str "  - Only in theirs: %s: %s" k v))
    p2_map;

  (* Properties with different values *)
  List.iter
    (fun (k, v1) ->
      match List.assoc_opt k p2_map with
      | Some v2 when v1 <> v2 ->
          add_line (Fmt.str "  - Different values for %s:" k);
          add_line (Fmt.str "    Ours:   %s" v1);
          add_line (Fmt.str "    Theirs: %s" v2)
      | _ -> ())
    p1_map

(* Analyze differences between two parsed CSS ASTs, returning structural
   changes *)
let props_of_decls decls =
  (* Use inline_style_of_declarations for structured access, then parse
     minimally *)
  let inline = Css.inline_style_of_declarations ~minify:true decls in
  inline |> String.split_on_char ';'
  |> List.filter_map (fun s ->
         let s = String.trim s in
         if s = "" then None
         else
           match String.index_opt s ':' with
           | None -> None
           | Some idx ->
               let name = String.sub s 0 idx |> String.trim in
               let value =
                 String.sub s (idx + 1) (String.length s - idx - 1)
                 |> String.trim
               in
               Some (name, value))

let convert_rule_to_strings stmt =
  match Css.as_rule stmt with
  | Some (selector, decls, _) ->
      let selector_str = Css.Selector.to_string selector in
      let props = props_of_decls decls in
      (selector_str, props)
  | None -> ("", [])

let compute_rule_diffs rules1 rules2 =
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
        | Some r2 when get_declarations r1 <> get_declarations r2 ->
            Some (sel, get_declarations r1, get_declarations r2)
        | _ -> None)
      rules1
  in
  (added, removed, modified)

(* Media and layer diffs intentionally omitted to avoid relying on hidden Css
   internals; we currently focus on top-level rule differences. *)
let compute_media_diffs items1 items2 =
  let added =
    List.filter
      (fun (cond, _) ->
        not (List.exists (fun (cond1, _) -> cond1 = cond) items1))
      items2
    |> List.map (fun (cond, _) -> ("@media " ^ cond, []))
  in
  let removed =
    List.filter
      (fun (cond, _) ->
        not (List.exists (fun (cond2, _) -> cond2 = cond) items2))
      items1
    |> List.map (fun (cond, _) -> ("@media " ^ cond, []))
  in
  let modified =
    List.filter_map
      (fun (cond1, rules1) ->
        match List.find_opt (fun (cond2, _) -> cond1 = cond2) items2 with
        | Some (_, rules2) when rules1 <> rules2 ->
            Some ("@media " ^ cond1, rules1, rules2)
        | _ -> None)
      items1
  in
  (added, removed, modified)

let compute_layer_diffs (layers1 : string list) (layers2 : string list) =
  let added =
    List.filter (fun name -> not (List.exists (( = ) name) layers1)) layers2
    |> List.map (fun name -> ("@layer " ^ name, []))
  in
  let removed =
    List.filter (fun name -> not (List.exists (( = ) name) layers2)) layers1
    |> List.map (fun name -> ("@layer " ^ name, []))
  in
  let modified = [] in
  (added, removed, modified)

let analyze_css_differences (ast1 : Css.t) (ast2 : Css.t) =
  let rules1 = Css.rules ast1 in
  let rules2 = Css.rules ast2 in

  let added, removed, modified = compute_rule_diffs rules1 rules2 in
  let media1 = Css.media_queries ast1 in
  let media2 = Css.media_queries ast2 in
  let at_added, at_removed, at_modified = compute_media_diffs media1 media2 in
  let layers1 = Css.layers ast1 in
  let layers2 = Css.layers ast2 in
  let layer_added, layer_removed, layer_modified =
    compute_layer_diffs layers1 layers2
  in

  let added_str = List.map convert_rule_to_strings added in
  let removed_str = List.map convert_rule_to_strings removed in
  let modified_str =
    List.map
      (fun (sel, decls1, decls2) ->
        let sel_str = Css.Selector.to_string sel in
        let props1 = props_of_decls decls1 in
        let props2 = props_of_decls decls2 in
        (sel_str, props1, props2))
      modified
  in

  ( added_str,
    removed_str,
    modified_str,
    at_added,
    at_removed,
    at_modified,
    layer_added,
    layer_removed,
    layer_modified )

let diff (ast1 : Css.t) (ast2 : Css.t) =
  let ( added,
        removed,
        modified,
        at_added,
        at_removed,
        at_modified,
        layer_added,
        layer_removed,
        layer_modified ) =
    analyze_css_differences ast1 ast2
  in

  let rule_changes =
    List.map
      (fun (sel, props) ->
        { selector = sel; change = Added; properties = props })
      added
    @ List.map
        (fun (sel, props) ->
          { selector = sel; change = Removed; properties = props })
        removed
    @ List.map
        (fun (sel, props1, props2) ->
          let prop_map1 =
            List.fold_left (fun acc (k, v) -> (k, v) :: acc) [] props1
          in
          let prop_map2 =
            List.fold_left (fun acc (k, v) -> (k, v) :: acc) [] props2
          in

          (* Find properties that changed values *)
          let prop_diffs =
            List.fold_left
              (fun acc (p1, v1) ->
                match List.assoc_opt p1 prop_map2 with
                | Some v2 when v1 <> v2 ->
                    { property = p1; our_value = v1; their_value = v2 } :: acc
                | _ -> acc)
              [] prop_map1
          in

          (* Find added and removed properties *)
          let removed_props =
            List.filter_map
              (fun (p1, v1) ->
                if not (List.mem_assoc p1 prop_map2) then Some (p1, v1)
                else None)
              prop_map1
          in
          let added_props =
            List.filter_map
              (fun (p2, v2) ->
                if not (List.mem_assoc p2 prop_map1) then Some (p2, v2)
                else None)
              prop_map2
          in

          {
            selector = sel;
            change = Modified (List.rev prop_diffs);
            properties = removed_props @ added_props;
          })
        modified
  in

  let media_changes =
    List.map
      (fun (cond, _) -> { condition = cond; change = Added; rules = [] })
      at_added
    @ List.map
        (fun (cond, _) -> { condition = cond; change = Removed; rules = [] })
        at_removed
    @ List.map
        (fun (cond, _, _) ->
          { condition = cond; change = Modified []; rules = [] })
        at_modified
  in

  let layer_changes =
    List.map (fun (name, _) -> { name; change = Added; rules = [] }) layer_added
    @ List.map
        (fun (name, _) -> { name; change = Removed; rules = [] })
        layer_removed
    @ List.map
        (fun (name, _, _) -> { name; change = Modified []; rules = [] })
        layer_modified
  in

  { rules = rule_changes; media = media_changes; layers = layer_changes }

(* Simple wrapper for backward compatibility *)
let format_parse_error (err : Css.parse_error) = Css.pp_parse_error err

let format_css_diff css1 css2 =
  let css1 = strip_header css1 in
  let css2 = strip_header css2 in
  match (Css.of_string css1, Css.of_string css2) with
  | Ok ast1, Ok ast2 ->
      let diff_result = diff ast1 ast2 in
      Fmt.str "%a" pp diff_result
  | Error msg1, Error msg2 ->
      Fmt.str "Parse errors in both CSS:@.First: %s@.Second: %s"
        (format_parse_error msg1) (format_parse_error msg2)
  | Error msg, _ ->
      Fmt.str "Failed to parse our CSS: %s" (format_parse_error msg)
  | _, Error msg ->
      Fmt.str "Failed to parse their CSS: %s" (format_parse_error msg)

let format_labeled_css_diff ~tw_label ~tailwind_label ?css1 ?css2 _ _ =
  match (css1, css2) with
  | Some c1, Some c2 -> (
      let c1 = strip_header c1 in
      let c2 = strip_header c2 in
      match (Css.of_string c1, Css.of_string c2) with
      | Ok ast1, Ok ast2 ->
          let ( added,
                removed,
                modified,
                at_added,
                at_removed,
                at_modified,
                _layer_added,
                _layer_removed,
                _layer_modified ) =
            analyze_css_differences ast1 ast2
          in

          let lines = ref [] in
          let add_line s = lines := s :: !lines in

          add_line
            (Fmt.str "=== CSS Comparison: %s vs %s ===" tw_label tailwind_label);

          if
            added = [] && removed = [] && modified = [] && at_added = []
            && at_removed = [] && at_modified = []
          then add_line "✓ CSS structures match perfectly!"
          else (
            if removed <> [] then (
              add_line (Fmt.str "@,@[<v>[Rules only in %s]@]" tw_label);
              List.iter
                (fun (sel, props) ->
                  add_line (Fmt.str "  %s" sel);
                  List.iter
                    (fun (p, v) -> add_line (Fmt.str "    %s: %s" p v))
                    props)
                removed);

            if added <> [] then (
              add_line (Fmt.str "@,@[<v>[Rules only in %s]@]" tailwind_label);
              List.iter
                (fun (sel, props) ->
                  add_line (Fmt.str "  %s" sel);
                  List.iter
                    (fun (p, v) -> add_line (Fmt.str "    %s: %s" p v))
                    props)
                added);

            if modified <> [] then (
              add_line "\n[Rules with different properties]";
              List.iter
                (fun (sel, props1, props2) ->
                  show_css_property_differences add_line sel props1 props2)
                modified));

          String.concat "\n" (List.rev !lines)
      | Error msg1, Error msg2 ->
          Fmt.str "Parse errors in both CSS:@.First: %s@.Second: %s"
            (format_parse_error msg1) (format_parse_error msg2)
      | Error msg, _ ->
          Fmt.str "Failed to parse %s CSS: %s" tw_label (format_parse_error msg)
      | _, Error msg ->
          Fmt.str "Failed to parse %s CSS: %s" tailwind_label
            (format_parse_error msg))
  | _, _ -> "Missing CSS content for comparison"
