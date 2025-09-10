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
  (* Strip any header comment if present *)
  if String.starts_with ~prefix:"/*!" css then
    (* Find the end of the comment - look for star-slash pattern *)
    let rec find_end i =
      if i + 1 >= String.length css then css
      else if css.[i] = '*' && css.[i + 1] = '/' then
        String.sub css (i + 2) (String.length css - i - 2)
      else find_end (i + 1)
    in
    find_end 3 (* Start after the initial comment marker *)
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
      let items = Css.stylesheet_items ast in
      let rec extract acc = function
        | [] -> List.rev acc
        | Css.Rule rule :: rest ->
            let selector_str = Css.Selector.to_string (Css.selector rule) in
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
              if contains 0 then extract (rule :: acc) rest
              else extract acc rest
            else extract acc rest
        | _ :: rest -> extract acc rest
      in
      extract [] items
  | Error _ -> []

let count_css_class_patterns css class_name =
  let rules = extract_rules_with_class css class_name in
  let selector_strings =
    List.map (fun r -> Css.Selector.to_string (Css.selector r)) rules
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

let find_dominant_css_class css =
  match Css.of_string css with
  | Ok ast -> (
      let items = Css.stylesheet_items ast in
      let rec count_classes acc = function
        | [] -> acc
        | Css.Rule rule :: rest ->
            let selector_str = Css.Selector.to_string (Css.selector rule) in
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
        | _ :: rest -> count_classes acc rest
      in
      let class_counts = count_classes [] items in
      match List.sort (fun (_, c1) (_, c2) -> compare c2 c1) class_counts with
      | (cls, count) :: _ -> (cls, count)
      | [] -> ("", 0))
  | Error _ -> ("", 0)

let extract_base_rules css class_name =
  let rules = extract_rules_with_class css class_name in
  List.map (fun r -> Css.Selector.to_string (Css.selector r)) rules

let show_css_property_differences add_line selector props1 props2 =
  let p1_map = List.fold_left (fun acc (k, v) -> (k, v) :: acc) [] props1 in
  let p2_map = List.fold_left (fun acc (k, v) -> (k, v) :: acc) [] props2 in

  add_line (Printf.sprintf "Selector: %s" selector);

  (* Properties only in first *)
  List.iter
    (fun (k, v) ->
      if not (List.mem_assoc k p2_map) then
        add_line (Printf.sprintf "  - Only in ours: %s: %s" k v))
    p1_map;

  (* Properties only in second *)
  List.iter
    (fun (k, v) ->
      if not (List.mem_assoc k p1_map) then
        add_line (Printf.sprintf "  - Only in theirs: %s: %s" k v))
    p2_map;

  (* Properties with different values *)
  List.iter
    (fun (k, v1) ->
      match List.assoc_opt k p2_map with
      | Some v2 when v1 <> v2 ->
          add_line (Printf.sprintf "  - Different values for %s:" k);
          add_line (Printf.sprintf "    Ours:   %s" v1);
          add_line (Printf.sprintf "    Theirs: %s" v2)
      | _ -> ())
    p1_map

(* Analyze differences between two parsed CSS ASTs, returning structural
   changes *)
let analyze_css_differences ast1 ast2 =
  let items1 = Css.stylesheet_items ast1 in
  let items2 = Css.stylesheet_items ast2 in

  let rules1 =
    List.filter_map (function Css.Rule r -> Some r | _ -> None) items1
  in
  let rules2 =
    List.filter_map (function Css.Rule r -> Some r | _ -> None) items2
  in

  let find_rule sel rules =
    List.find_opt (fun r -> Css.selector r = sel) rules
  in

  (* Find added rules (in ast2 but not ast1) *)
  let added =
    List.filter_map
      (fun r ->
        let sel = Css.selector r in
        if not (List.exists (fun r1 -> Css.selector r1 = sel) rules1) then
          (* Return the selector and declarations directly *)
          Some (sel, Css.declarations r)
        else None)
      rules2
  in

  (* Find removed rules (in ast1 but not ast2) *)
  let removed =
    List.filter_map
      (fun r ->
        let sel = Css.selector r in
        if not (List.exists (fun r2 -> Css.selector r2 = sel) rules2) then
          (* Return the selector and declarations directly *)
          Some (sel, Css.declarations r)
        else None)
      rules1
  in

  (* Find modified rules *)
  let modified =
    List.filter_map
      (fun r1 ->
        let sel = Css.selector r1 in
        match find_rule sel rules2 with
        | Some r2 when Css.declarations r1 <> Css.declarations r2 ->
            (* Return structured data - will be converted to strings later *)
            Some (sel, Css.declarations r1, Css.declarations r2)
        | _ -> None)
      rules1
  in

  (* Handle media and layer rules *)
  let media1 =
    List.filter_map (function Css.Media m -> Some m | _ -> None) items1
  in
  let media2 =
    List.filter_map (function Css.Media m -> Some m | _ -> None) items2
  in

  let layers1 =
    List.filter_map (function Css.Layer l -> Some l | _ -> None) items1
  in
  let layers2 =
    List.filter_map (function Css.Layer l -> Some l | _ -> None) items2
  in

  let at_added =
    List.filter
      (fun m ->
        not
          (List.exists
             (fun m1 -> Css.media_condition m1 = Css.media_condition m)
             media1))
      media2
    |> List.map (fun m -> ("@media " ^ Css.media_condition m, []))
  in

  let at_removed =
    List.filter
      (fun m ->
        not
          (List.exists
             (fun m2 -> Css.media_condition m2 = Css.media_condition m)
             media2))
      media1
    |> List.map (fun m -> ("@media " ^ Css.media_condition m, []))
  in

  let at_modified =
    List.filter_map
      (fun m1 ->
        match
          List.find_opt
            (fun m2 -> Css.media_condition m1 = Css.media_condition m2)
            media2
        with
        | Some m2 when Css.media_rules m1 <> Css.media_rules m2 ->
            Some
              ( "@media " ^ Css.media_condition m1,
                Css.media_rules m1,
                Css.media_rules m2 )
        | _ -> None)
      media1
  in

  (* Layer comparison logic *)
  let layer_added =
    List.filter
      (fun l ->
        not
          (List.exists (fun l1 -> Css.layer_name l1 = Css.layer_name l) layers1))
      layers2
    |> List.map (fun l -> ("@layer " ^ Css.layer_name l, []))
  in

  let layer_removed =
    List.filter
      (fun l ->
        not
          (List.exists (fun l2 -> Css.layer_name l2 = Css.layer_name l) layers2))
      layers1
    |> List.map (fun l -> ("@layer " ^ Css.layer_name l, []))
  in

  let layer_modified =
    List.filter_map
      (fun l1 ->
        match
          List.find_opt
            (fun l2 -> Css.layer_name l1 = Css.layer_name l2)
            layers2
        with
        | Some l2 when Css.layer_rules l1 <> Css.layer_rules l2 ->
            Some
              ( "@layer " ^ Css.layer_name l1,
                Css.layer_rules l1,
                Css.layer_rules l2 )
        | _ -> None)
      layers1
  in

  (* Convert structured data to expected string format *)
  let convert_rule_to_strings (selector, decls) =
    let selector_str = Css.Selector.to_string selector in
    let props =
      List.map
        (fun d ->
          (* Use a simple fallback to convert declaration to prop:value *)
          try
            let dummy_css =
              Css.to_string ~minify:true
                (Css.stylesheet
                   [
                     Css.Rule
                       (Css.rule ~selector:(Css.Selector.element "dummy") [ d ]);
                   ])
            in
            let start = String.index dummy_css '{' + 1 in
            let end_pos = String.index dummy_css '}' in
            let content = String.sub dummy_css start (end_pos - start) in
            match String.split_on_char ':' content with
            | prop :: rest ->
                let value = String.concat ":" rest |> String.trim in
                (String.trim prop, value)
            | [] -> ("", "")
          with _ -> ("", ""))
        decls
    in
    (selector_str, props)
  in

  let added_str = List.map convert_rule_to_strings added in
  let removed_str = List.map convert_rule_to_strings removed in
  let modified_str =
    List.map
      (fun (sel, decls1, decls2) ->
        let sel_str = Css.Selector.to_string sel in
        let props1 = snd (convert_rule_to_strings (sel, decls1)) in
        let props2 = snd (convert_rule_to_strings (sel, decls2)) in
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

let diff ast1 ast2 =
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
let format_parse_error (err : Css.parse_error) =
  let callstack_str =
    if err.callstack = [] then ""
    else "\n[Parse stack: " ^ String.concat " -> " err.callstack ^ "]"
  in
  Css.pp_parse_error err ^ "\n" ^ err.context_window ^ callstack_str ^ "\n"
  ^ String.make err.marker_pos ' '
  ^ "^"

let format_css_diff css1 css2 =
  let css1 = strip_header css1 in
  let css2 = strip_header css2 in
  match (Css.of_string css1, Css.of_string css2) with
  | Ok ast1, Ok ast2 ->
      let diff_result = diff ast1 ast2 in
      Fmt.str "%a" pp diff_result
  | Error msg1, Error msg2 ->
      Printf.sprintf "Parse errors in both CSS:\nFirst: %s\nSecond: %s"
        (format_parse_error msg1) (format_parse_error msg2)
  | Error msg, _ ->
      Printf.sprintf "Failed to parse our CSS: %s" (format_parse_error msg)
  | _, Error msg ->
      Printf.sprintf "Failed to parse their CSS: %s" (format_parse_error msg)

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
            (Printf.sprintf "=== CSS Comparison: %s vs %s ===" tw_label
               tailwind_label);

          if
            added = [] && removed = [] && modified = [] && at_added = []
            && at_removed = [] && at_modified = []
          then add_line "✓ CSS structures match perfectly!"
          else (
            if removed <> [] then (
              add_line (Printf.sprintf "\n[Rules only in %s]" tw_label);
              List.iter
                (fun (sel, props) ->
                  add_line (Printf.sprintf "  %s" sel);
                  List.iter
                    (fun (p, v) -> add_line (Printf.sprintf "    %s: %s" p v))
                    props)
                removed);

            if added <> [] then (
              add_line (Printf.sprintf "\n[Rules only in %s]" tailwind_label);
              List.iter
                (fun (sel, props) ->
                  add_line (Printf.sprintf "  %s" sel);
                  List.iter
                    (fun (p, v) -> add_line (Printf.sprintf "    %s: %s" p v))
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
          Printf.sprintf "Parse errors in both CSS:\nFirst: %s\nSecond: %s"
            (format_parse_error msg1) (format_parse_error msg2)
      | Error msg, _ ->
          Printf.sprintf "Failed to parse %s CSS: %s" tw_label
            (format_parse_error msg)
      | _, Error msg ->
          Printf.sprintf "Failed to parse %s CSS: %s" tailwind_label
            (format_parse_error msg))
  | _, _ -> "Missing CSS content for comparison"
