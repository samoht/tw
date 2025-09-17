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
  properties : Css.declaration list; (* For added/removed rules *)
}

type media_change = {
  condition : string;
  change : change;
  rules : rule_change list;
}

type layer_change = { name : string; change : change; rules : rule_change list }

type supports_change = {
  condition : string;
  change : change;
  rules : rule_change list;
}

type container_change = {
  name : string option;
  condition : string;
  change : change;
  rules : rule_change list;
}

type t = {
  rules : rule_change list;
  media : media_change list;
  layers : layer_change list;
  supports : supports_change list;
  containers : container_change list;
}

let pp_property_diff fmt { property; our_value; their_value } =
  Fmt.pf fmt "@[<v 2>Property %s:@,Expected: %s@,Actual:   %s@]" property
    our_value their_value

let pp_change ?(expected = "Expected") ?(actual = "Actual") fmt = function
  | Added -> Fmt.pf fmt "Only in %s" actual
  | Removed -> Fmt.pf fmt "Only in %s" expected
  | Modified props ->
      Fmt.pf fmt "Modified:@,  %a"
        (Fmt.list ~sep:(Fmt.any "@,  ") pp_property_diff)
        props

let pp_rule_change ?(expected = "Expected") ?(actual = "Actual") fmt
    { selector; change; properties } =
  Fmt.pf fmt "@[<v 2>%s: %a" selector (pp_change ~expected ~actual) change;
  (match change with
  | (Added | Removed) when properties <> [] ->
      Fmt.pf fmt "@,Properties:@,  %a"
        (Fmt.list ~sep:(Fmt.any "@,  ") (fun fmt d ->
             Fmt.string fmt (Css.string_of_declaration ~minify:true d)))
        properties
  | _ -> ());
  Fmt.pf fmt "@]"

let pp_media_change ?(expected = "Expected") ?(actual = "Actual") fmt
    ({ condition; change; rules } : media_change) =
  Fmt.pf fmt "@[<v 2>@media %s: %a" condition
    (pp_change ~expected ~actual)
    change;
  if rules <> [] then
    Fmt.pf fmt "@,Rules:@,  %a"
      (Fmt.list ~sep:(Fmt.any "@,  ") (pp_rule_change ~expected ~actual))
      rules;
  Fmt.pf fmt "@]"

let pp_layer_change ?(expected = "Expected") ?(actual = "Actual") fmt
    ({ name; change; rules } : layer_change) =
  Fmt.pf fmt "@[<v 2>@layer %s: %a" name (pp_change ~expected ~actual) change;
  if rules <> [] then
    Fmt.pf fmt "@,Rules:@,  %a"
      (Fmt.list ~sep:(Fmt.any "@,  ") (pp_rule_change ~expected ~actual))
      rules;
  Fmt.pf fmt "@]"

let pp_supports_change ?(expected = "Expected") ?(actual = "Actual") fmt
    ({ condition; change; rules } : supports_change) =
  Fmt.pf fmt "@[<v 2>@supports %s: %a" condition
    (pp_change ~expected ~actual)
    change;
  if rules <> [] then
    Fmt.pf fmt "@,Rules:@,  %a"
      (Fmt.list ~sep:(Fmt.any "@,  ") (pp_rule_change ~expected ~actual))
      rules;
  Fmt.pf fmt "@]"

let pp_container_change ?(expected = "Expected") ?(actual = "Actual") fmt
    ({ name; condition; change; rules } : container_change) =
  let name_pp = match name with None -> "" | Some n -> n ^ " " in
  Fmt.pf fmt "@[<v 2>@container %s(%s): %a" name_pp condition
    (pp_change ~expected ~actual)
    change;
  if rules <> [] then
    Fmt.pf fmt "@,Rules:@,  %a"
      (Fmt.list ~sep:(Fmt.any "@,  ") (pp_rule_change ~expected ~actual))
      rules;
  Fmt.pf fmt "@]"

let pp ?(expected = "Expected") ?(actual = "Actual") fmt
    { rules; media; layers; supports; containers } =
  if rules = [] && media = [] && layers = [] && supports = [] && containers = []
  then () (* Output nothing for empty diff *)
  else (
    Fmt.pf fmt "@[<v 2>CSS Diff:@,";
    if rules <> [] then
      Fmt.pf fmt "Rules:@,%a@,"
        (Fmt.list ~sep:(Fmt.any "@,") (pp_rule_change ~expected ~actual))
        rules;
    if media <> [] then
      Fmt.pf fmt "Media:@,%a@,"
        (Fmt.list ~sep:(Fmt.any "@,") (pp_media_change ~expected ~actual))
        media;
    if layers <> [] then
      Fmt.pf fmt "Layers:@,%a@,"
        (Fmt.list ~sep:(Fmt.any "@,") (pp_layer_change ~expected ~actual))
        layers;
    if supports <> [] then
      Fmt.pf fmt "Supports:@,%a@,"
        (Fmt.list ~sep:(Fmt.any "@,") (pp_supports_change ~expected ~actual))
        supports;
    if containers <> [] then
      Fmt.pf fmt "Containers:@,%a@,"
        (Fmt.list ~sep:(Fmt.any "@,") (pp_container_change ~expected ~actual))
        containers;
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

let equal_media_change (m1 : media_change) (m2 : media_change) =
  m1.condition = m2.condition
  && equal_change m1.change m2.change
  && List.length m1.rules = List.length m2.rules
  && List.for_all2 equal_rule_change m1.rules m2.rules

let equal_layer_change (l1 : layer_change) (l2 : layer_change) =
  l1.name = l2.name
  && equal_change l1.change l2.change
  && List.length l1.rules = List.length l2.rules
  && List.for_all2 equal_rule_change l1.rules l2.rules

let equal_supports_change (s1 : supports_change) (s2 : supports_change) =
  s1.condition = s2.condition
  && equal_change s1.change s2.change
  && List.length s1.rules = List.length s2.rules
  && List.for_all2 equal_rule_change s1.rules s2.rules

let equal_container_change (c1 : container_change) (c2 : container_change) =
  c1.name = c2.name
  && c1.condition = c2.condition
  && equal_change c1.change c2.change
  && List.length c1.rules = List.length c2.rules
  && List.for_all2 equal_rule_change c1.rules c2.rules

let equal (d1 : t) (d2 : t) =
  List.length d1.rules = List.length d2.rules
  && List.for_all2 equal_rule_change d1.rules d2.rules
  && List.length d1.media = List.length d2.media
  && List.for_all2 equal_media_change d1.media d2.media
  && List.length d1.layers = List.length d2.layers
  && List.for_all2 equal_layer_change d1.layers d2.layers
  && List.length d1.supports = List.length d2.supports
  && List.for_all2 equal_supports_change d1.supports d2.supports
  && List.length d1.containers = List.length d2.containers
  && List.for_all2 equal_container_change d1.containers d2.containers

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

let layer_diffs (layers1 : string list) (layers2 : string list) =
  let added =
    List.filter (fun name -> not (List.exists (( = ) name) layers1)) layers2
    |> List.map (fun name -> (name, []))
  in
  let removed =
    List.filter (fun name -> not (List.exists (( = ) name) layers2)) layers1
    |> List.map (fun name -> (name, []))
  in
  let modified = [] in
  (added, removed, modified)

(* Helper function to compute property diffs between two declaration lists *)
let property_diffs decls1 decls2 : property_diff list =
  let props1 = List.map decl_to_prop_value decls1 in
  let props2 = List.map decl_to_prop_value decls2 in
  List.fold_left
    (fun acc (p1, v1) ->
      match List.assoc_opt p1 props2 with
      | Some v2 when v1 <> v2 ->
          { property = p1; our_value = v1; their_value = v2 } :: acc
      | _ -> acc)
    [] props1
  |> List.rev

(* Helper to convert rule diffs to rule_change list *)
let rules_to_changes (added, removed, modified) : rule_change list =
  let added_changes =
    List.map
      (fun stmt ->
        let sel, props = convert_rule_to_strings stmt in
        { selector = sel; change = Added; properties = props })
      added
  in
  let removed_changes =
    List.map
      (fun stmt ->
        let sel, props = convert_rule_to_strings stmt in
        { selector = sel; change = Removed; properties = props })
      removed
  in
  let modified_changes =
    List.map
      (fun (sel, decls1, decls2) ->
        let sel_str = Css.Selector.to_string sel in
        let prop_diffs = property_diffs decls1 decls2 in
        { selector = sel_str; change = Modified prop_diffs; properties = [] })
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
  let media_changes =
    let rule_changes_of rules1 rules2 =
      rules_to_changes (rule_diffs rules1 rules2)
    in
    List.map
      (fun (cond, _) ->
        ({ condition = cond; change = Added; rules = [] } : media_change))
      at_added
    @ List.map
        (fun (cond, _) ->
          ({ condition = cond; change = Removed; rules = [] } : media_change))
        at_removed
    @ List.filter_map
        (fun (cond, rules1, rules2) ->
          let rc = rule_changes_of rules1 rules2 in
          if rc = [] then None
          else
            Some
              ({ condition = cond; change = Modified []; rules = rc }
                : media_change))
        at_modified
  in

  let layers1 = Css.layers expected in
  let layers2 = Css.layers actual in
  let layer_added, layer_removed, layer_modified =
    layer_diffs layers1 layers2
  in
  let layer_changes =
    List.map
      (fun (name, _) -> ({ name; change = Added; rules = [] } : layer_change))
      layer_added
    @ List.map
        (fun (name, _) ->
          ({ name; change = Removed; rules = [] } : layer_change))
        layer_removed
    @ List.map
        (fun (name, _, _) ->
          ({ name; change = Modified []; rules = [] } : layer_change))
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
  let supports_changes : supports_change list =
    List.map
      (fun (cond, _) ->
        ({ condition = cond; change = Added; rules = [] } : supports_change))
      supports_added
    @ List.map
        (fun (cond, _) ->
          ({ condition = cond; change = Removed; rules = [] } : supports_change))
        supports_removed
    @ List.map
        (fun (cond, rules1, rules2) ->
          let rc = rules_to_changes (rule_diffs rules1 rules2) in
          ({ condition = cond; change = Modified []; rules = rc }
            : supports_change))
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
  let container_changes : container_change list =
    List.map
      (fun (n, cond, _) ->
        ({ name = n; condition = cond; change = Added; rules = [] }
          : container_change))
      containers_added
    @ List.map
        (fun (n, cond, _) ->
          ({ name = n; condition = cond; change = Removed; rules = [] }
            : container_change))
        containers_removed
    @ List.map
        (fun (n, cond, rules1, rules2) ->
          let rc = rules_to_changes (rule_diffs rules1 rules2) in
          ({ name = n; condition = cond; change = Modified []; rules = rc }
            : container_change))
        containers_modified
  in

  {
    rules = rule_changes;
    media = media_changes;
    layers = layer_changes;
    supports = supports_changes;
    containers = container_changes;
  }

(* Compare two CSS ASTs directly *)
let compare_css css1 css2 =
  let css1 = strip_header css1 in
  let css2 = strip_header css2 in
  match (Css.of_string css1, Css.of_string css2) with
  | Ok expected, Ok actual ->
      let d = diff_ast ~expected ~actual in
      d.rules = [] && d.media = [] && d.layers = [] && d.supports = []
      && d.containers = []
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

(* Format the result of diff_strings with optional labels *)
let pp_diff_result ?(expected = "Expected") ?(actual = "Actual") fmt = function
  | Diff d
    when d.rules = [] && d.media = [] && d.layers = [] && d.supports = []
         && d.containers = [] ->
      (* No structural differences - could be whitespace only *)
      Fmt.pf fmt "CSS has no structural differences"
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
