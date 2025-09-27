(** CSS comparison utilities for testing using the proper CSS parser *)

(* ===== Constants ===== *)

(* Display and formatting constants *)
let default_max_width = 60
let default_short_threshold = 30
let default_truncation_length = 60
let default_context_size = 3
let ellipsis_length = 3 (* "..." *)
let caret_indent = 10
let stats_max_width = 80

(* Search and parsing constants *)
let header_comment_start = 3 (* Position after "/*" *)

(* ===== String Utilities ===== *)

(* Truncate long CSS values with context for readability *)
let truncate_with_context max_len value =
  let len = String.length value in
  if len <= max_len then value
  else
    let half_len = (max_len - ellipsis_length) / 2 in
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

(* ===== List Utilities ===== *)

(* Helper: take first n elements from a list *)
let rec list_take n = function
  | [] -> []
  | _ when n <= 0 -> []
  | h :: t -> h :: list_take (n - 1) t

(* Helper: drop first n elements from a list *)
let rec list_drop n = function
  | [] -> []
  | lst when n <= 0 -> lst
  | _ :: t -> list_drop (n - 1) t

(* Helper: zip two lists into pairs, padding with empty strings *)
let rec zip_with_empty l1 l2 =
  match (l1, l2) with
  | [], [] -> []
  | h1 :: t1, [] -> (h1, "") :: zip_with_empty t1 []
  | [], h2 :: t2 -> ("", h2) :: zip_with_empty [] t2
  | h1 :: t1, h2 :: t2 -> (h1, h2) :: zip_with_empty t1 t2

(* ===== Type Definitions ===== *)

type declaration = {
  property_name : string;
  expected_value : string;
  actual_value : string;
}

(* Individual rule changes *)
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

(* Container changes with proper separation of concerns *)
type container_diff =
  | Container_added of container_info
  | Container_removed of container_info
  | Container_modified of {
      info : container_info;
      rule_changes : rule_diff list;
    }

and container_info = {
  container_type : [ `Media | `Layer | `Supports | `Container | `Property ];
  condition : string;
}

(* Structured diff type *)
type tree_diff = { rules : rule_diff list; containers : container_diff list }

let is_empty d = d.rules = [] && d.containers = []

(* ===== Pretty Printing Helpers ===== *)

(* Unified diff formatting configuration *)
type diff_format_config = {
  max_width : int;
  short_threshold : int;
  show_caret : bool;
  indent : int;
}

let default_config =
  {
    max_width = default_max_width;
    short_threshold = default_short_threshold;
    show_caret = true;
    indent = 0;
  }

(* Core function to format a value diff with truncation and caret *)
let format_value_diff ?(config = default_config) expected actual =
  match find_first_diff expected actual with
  | None -> `No_diff
  | Some diff_pos ->
      let len1 = String.length expected in
      let len2 = String.length actual in

      if len1 <= config.short_threshold && len2 <= config.short_threshold then
        `Short_diff (expected, actual)
      else if len1 <= config.max_width && len2 <= config.max_width then
        `Medium_diff (expected, actual, diff_pos)
      else
        (* Calculate window centered on the diff position *)
        let half = config.max_width / 2 in
        let window_start = max 0 (diff_pos - half) in
        let window_end =
          min (max len1 len2) (window_start + config.max_width)
        in

        (* Helper to extract and format a window from a string *)
        let extract_window s len =
          if window_start >= len then ("...", 3)
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
        let adjusted_pos =
          if diff_pos < window_start then 0
          else if diff_pos >= window_end then String.length s1_display - 1
          else prefix_len1 + (diff_pos - window_start)
        in
        `Long_diff (s1_display, s2_display, adjusted_pos)

(* Print a caret pointing to a specific position *)
let pp_caret ?(indent = 0) fmt pos =
  Fmt.pf fmt "%s^@," (String.make (pos + indent) ' ')

(* Print a list of CSS declarations with an action prefix *)
let pp_declarations fmt action decls =
  List.iter
    (fun decl ->
      let prop_name = Css.declaration_name decl in
      let prop_value = Css.declaration_value ~minify:true decl in
      let truncated_value =
        truncate_with_context default_truncation_length prop_value
      in
      Fmt.pf fmt "    - %s: %s %s@," action prop_name truncated_value)
    decls

(* Print a line pair in unified diff format *)
let pp_line_pair : (string * string) Fmt.t =
 fun fmt (exp, act) ->
  if exp = act then Fmt.pf fmt " %s@," exp (* Common line *)
  else (
    if exp <> "" then Fmt.pf fmt "-%s@," exp;
    if act <> "" then Fmt.pf fmt "+%s@," act;
    ())

let pp_property_diff fmt { property_name; expected_value; actual_value } =
  let config = { default_config with short_threshold = 30 } in
  match format_value_diff ~config expected_value actual_value with
  | `No_diff ->
      (* Shouldn't happen but handle gracefully *)
      Fmt.pf fmt "    - modify: %s (no diff detected)@," property_name
  | `Short_diff (exp, act) ->
      (* Short values: show inline as a modification *)
      Fmt.pf fmt "    - modify: %s %s -> %s@," property_name exp act
  | `Medium_diff (exp, act, pos) | `Long_diff (exp, act, pos) ->
      (* Long values: show with ellipsis and caret *)
      Fmt.pf fmt "    - modify: %s@," property_name;
      Fmt.pf fmt "        - %s@," exp;
      Fmt.pf fmt "        + %s@," act;
      if config.show_caret then
        pp_caret ~indent:caret_indent fmt pos (* indent for " + " prefix *)

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

(* Pretty-print rule_diff *)
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

(* Pretty-print container_diff *)
let pp_container_diff fmt = function
  | Container_added { container_type; condition } ->
      let prefix =
        match container_type with
        | `Media -> "@media"
        | `Layer -> "@layer"
        | `Supports -> "@supports"
        | `Container -> "@container"
        | `Property -> "@property"
      in
      Fmt.pf fmt "- %s %s: (added)@," prefix condition
  | Container_removed { container_type; condition } ->
      let prefix =
        match container_type with
        | `Media -> "@media"
        | `Layer -> "@layer"
        | `Supports -> "@supports"
        | `Container -> "@container"
        | `Property -> "@property"
      in
      Fmt.pf fmt "- %s %s: (removed)@," prefix condition
  | Container_modified { info = { container_type; condition }; rule_changes } ->
      let prefix =
        match container_type with
        | `Media -> "@media"
        | `Layer -> "@layer"
        | `Supports -> "@supports"
        | `Container -> "@container"
        | `Property -> "@property"
      in
      Fmt.pf fmt "- %s %s:@," prefix condition;
      List.iter
        (fun rule_diff ->
          Fmt.pf fmt "  ";
          pp_rule_diff fmt rule_diff)
        rule_changes

let pp_tree_diff ?(expected = "Expected") ?(actual = "Actual") fmt
    { rules; containers } =
  if rules = [] && containers = [] then Fmt.pf fmt "No differences found"
  else (
    (* Git-style diff header *)
    Fmt.pf fmt "--- %s@," expected;
    Fmt.pf fmt "+++ %s@," actual;

    (* Separate reorderings from meaningful changes *)
    let reordered_rules =
      List.filter (function Rule_reordered _ -> true | _ -> false) rules
    in
    let meaningful_rules =
      List.filter (function Rule_reordered _ -> false | _ -> true) rules
    in

    (* Show a summary for reorderings instead of detailed diffs *)
    if List.length reordered_rules > 0 then
      Fmt.pf fmt
        "- Note: %d rules reordered (same content, different position)@,@,"
        (List.length reordered_rules);

    (* Show the actual differences *)
    List.iter (pp_rule_diff fmt) meaningful_rules;

    (* Show container differences *)
    List.iter (pp_container_diff fmt) containers)

let equal_rule_diff r1 r2 =
  match (r1, r2) with
  | ( Rule_added { selector = s1; declarations = d1 },
      Rule_added { selector = s2; declarations = d2 } ) ->
      s1 = s2 && d1 = d2
  | ( Rule_removed { selector = s1; declarations = d1 },
      Rule_removed { selector = s2; declarations = d2 } ) ->
      s1 = s2 && d1 = d2
  | ( Rule_content_changed
        {
          selector = s1;
          old_declarations = o1;
          new_declarations = n1;
          property_changes = p1;
        },
      Rule_content_changed
        {
          selector = s2;
          old_declarations = o2;
          new_declarations = n2;
          property_changes = p2;
        } ) ->
      s1 = s2 && o1 = o2 && n1 = n2 && p1 = p2
  | ( Rule_selector_changed
        { old_selector = o1; new_selector = n1; declarations = d1 },
      Rule_selector_changed
        { old_selector = o2; new_selector = n2; declarations = d2 } ) ->
      o1 = o2 && n1 = n2 && d1 = d2
  | Rule_reordered { selector = s1 }, Rule_reordered { selector = s2 } ->
      s1 = s2
  | _ -> false

let equal_container_diff c1 c2 =
  match (c1, c2) with
  | Container_added i1, Container_added i2 -> i1 = i2
  | Container_removed i1, Container_removed i2 -> i1 = i2
  | ( Container_modified { info = i1; rule_changes = r1 },
      Container_modified { info = i2; rule_changes = r2 } ) ->
      i1 = i2
      && List.length r1 = List.length r2
      && List.for_all2 equal_rule_diff r1 r2
  | _ -> false

let equal_tree_diff (d1 : tree_diff) (d2 : tree_diff) : bool =
  List.length d1.rules = List.length d2.rules
  && List.for_all2 equal_rule_diff d1.rules d2.rules
  && List.length d1.containers = List.length d2.containers
  && List.for_all2 equal_container_diff d1.containers d2.containers

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
    match find_comment_end header_comment_start with
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

let convert_rule_to_strings stmt =
  match Css.as_rule stmt with
  | Some (selector, decls, _) ->
      let selector_str = Css.Selector.to_string selector in
      (selector_str, decls)
  | None -> ("", [])

(* Helper functions for rule comparison *)
(* Normalize a selector string by sorting comma-separated selector items.
   This ensures we consider ".a,.b" equivalent to ".b,.a" when matching. *)
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

let rules_added_diff rules1 rules2 =
  (* Create lookup table for O(1) existence checks *)
  let keys1_table = Hashtbl.create (List.length rules1) in
  List.iter
    (fun r ->
      let key = selector_key_of_stmt r in
      Hashtbl.replace keys1_table key ())
    rules1;

  List.filter_map
    (fun r ->
      let key = selector_key_of_stmt r in
      if not (Hashtbl.mem keys1_table key) then Some r else None)
    rules2

let rules_removed_diff rules1 rules2 =
  (* Create lookup table for O(1) existence checks *)
  let keys2_table = Hashtbl.create (List.length rules2) in
  List.iter
    (fun r ->
      let key = selector_key_of_stmt r in
      Hashtbl.replace keys2_table key ())
    rules2;

  List.filter_map
    (fun r ->
      let key = selector_key_of_stmt r in
      if not (Hashtbl.mem keys2_table key) then Some r else None)
    rules1

let rules_modified_diff rules1 rules2 =
  (* Create lookup tables for O(1) access *)
  let rules2_by_key = Hashtbl.create (List.length rules2) in
  let rules2_by_props = Hashtbl.create (List.length rules2) in

  (* Populate lookup tables *)
  List.iter
    (fun r ->
      let key = selector_key_of_stmt r in
      let decls = get_rule_declarations r in
      let props = List.map decl_to_prop_value decls |> List.sort compare in

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
        let props1 = List.map decl_to_prop_value d1 |> List.sort compare in

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

  (* Helper function to check if two selectors share meaningful parent context
     using AST *)
  let share_parent_ast sel1 sel2 =
    let module S = Css.Selector in
    (* Extract the "context" part of a selector - the part that should be
       shared *)
    let extract_context = function
      | S.Combined (parent, combinator, _child) ->
          (* For combined selectors like "a > b", the context is "a >" *)
          Some (`Combined (S.to_string parent, combinator))
      | S.Compound selectors ->
          (* For compound selectors, extract functional pseudo-classes as
             context *)
          let context_parts =
            List.filter
              (function
                | S.Where _ | S.Not _ | S.Is _ | S.Has _ -> true
                | S.Class _ -> true (* Include class selectors as context *)
                | _ -> false)
              selectors
          in
          if List.length context_parts > 0 then
            Some (`Compound (List.map S.to_string context_parts))
          else None
      | _ -> None
    in

    match (extract_context sel1, extract_context sel2) with
    | Some (`Combined (parent1, comb1)), Some (`Combined (parent2, comb2)) ->
        parent1 = parent2 && comb1 = comb2
    | Some (`Compound context1), Some (`Compound context2) ->
        context1 = context2
    | _ -> false
  in

  (* Try to match removed rules with added rules for selector changes *)
  List.iter
    (fun removed_rule ->
      let removed_sel = get_rule_selector removed_rule in
      let removed_decls = get_rule_declarations removed_rule in
      let removed_props =
        List.map decl_to_prop_value removed_decls |> List.sort compare
      in

      (* Look for a matching added rule with same properties but different
         selector *)
      let matching_added =
        List.find_opt
          (fun added_rule ->
            let added_sel = get_rule_selector added_rule in
            let added_decls = get_rule_declarations added_rule in
            let added_props =
              List.map decl_to_prop_value added_decls |> List.sort compare
            in

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

let rule_diffs rules1 rules2 = handle_structural_diff rules1 rules2

(* Media and layer diffs intentionally omitted to avoid relying on hidden Css
   internals; we currently focus on top-level rule differences. *)
let media_diff items1 items2 =
  let key_of (cond, _) = cond in
  let key_equal = String.equal in
  let is_empty_diff (_, rules1) (_, rules2) =
    let added_r, removed_r, modified_r = rule_diffs rules1 rules2 in
    added_r = [] && removed_r = [] && modified_r = []
  in
  let added, removed, modified_pairs =
    find_diffs ~key_of ~key_equal ~is_empty_diff items1 items2
  in
  (* Transform to expected format *)
  let added = List.map (fun (cond, _) -> (cond, [])) added in
  let removed = List.map (fun (cond, _) -> (cond, [])) removed in
  let modified =
    List.map
      (fun ((cond, rules1), (_, rules2)) -> (cond, rules1, rules2))
      modified_pairs
  in
  (added, removed, modified)

(* Helper functions for converting rule changes *)
let convert_added_rule stmt =
  let sel, decls = convert_rule_to_strings stmt in
  Rule_added { selector = sel; declarations = decls }

let convert_removed_rule stmt =
  let sel, decls = convert_rule_to_strings stmt in
  Rule_removed { selector = sel; declarations = decls }

let rec convert_modified_rule (sel1, sel2, decls1, decls2) =
  let sel1_str = Css.Selector.to_string sel1 in
  let sel2_str = Css.Selector.to_string sel2 in
  if sel1_str <> sel2_str then
    Rule_selector_changed
      {
        old_selector = sel1_str;
        new_selector = sel2_str;
        declarations = decls2;
      }
  else
    let property_changes = properties_diff decls1 decls2 in
    Rule_content_changed
      {
        selector = sel1_str;
        old_declarations = decls1;
        new_declarations = decls2;
        property_changes;
      }

(* Helper function to compute property diffs between two declaration lists *)
and properties_diff decls1 decls2 : declaration list =
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

let tree_diff ~(expected : Css.t) ~(actual : Css.t) : tree_diff =
  let rules1 = Css.rules expected in
  let rules2 = Css.rules actual in
  let added, removed, modified = rule_diffs rules1 rules2 in

  let rule_changes =
    (* Convert added rules *)
    List.map
      (fun stmt ->
        let sel, decls = convert_rule_to_strings stmt in
        Rule_added { selector = sel; declarations = decls })
      added
    (* Convert removed rules *)
    @ List.map
        (fun stmt ->
          let sel, decls = convert_rule_to_strings stmt in
          Rule_removed { selector = sel; declarations = decls })
        removed
    @
    (* Convert modified rules *)
    List.filter_map
      (fun (sel1, sel2, decls1, decls2) ->
        let sel1_str = Css.Selector.to_string sel1 in
        let sel2_str = Css.Selector.to_string sel2 in
        let prop_diffs = properties_diff decls1 decls2 in
        let props1 = List.map decl_to_prop_value decls1 |> List.sort compare in
        let props2 = List.map decl_to_prop_value decls2 |> List.sort compare in
        let only_selector_changed = sel1_str <> sel2_str && props1 = props2 in
        let is_pure_reorder = props1 = props2 && decls1 <> decls2 in
        let is_rule_only_reorder = decls1 = decls2 && sel1_str = sel2_str in

        if only_selector_changed then
          Some
            (Rule_selector_changed
               {
                 old_selector = sel1_str;
                 new_selector = sel2_str;
                 declarations = decls1;
               })
        else if is_pure_reorder || is_rule_only_reorder then
          Some (Rule_reordered { selector = sel1_str })
        else if prop_diffs <> [] || decls1 <> decls2 then
          Some
            (Rule_content_changed
               {
                 selector = sel1_str;
                 old_declarations = decls1;
                 new_declarations = decls2;
                 property_changes = prop_diffs;
               })
        else None)
      modified
  in

  (* Helper to extract all statements of a specific type *)
  let extract_statements as_func ast =
    Css.statements ast |> List.filter_map as_func
  in

  (* Handle containers (media, layers, supports, properties) *)
  let containers =
    (* Media queries *)
    let media1 = Css.media_queries expected in
    let media2 = Css.media_queries actual in
    let media_added, media_removed, media_modified = media_diff media1 media2 in

    (* Layer blocks *)
    let layers1 = extract_statements Css.as_layer expected in
    let layers2 = extract_statements Css.as_layer actual in
    let layers_added, layers_removed, layers_modified =
      let key_of (name_opt, _) = Option.value ~default:"" name_opt in
      let key_equal = String.equal in
      let is_empty_diff (_, rules1) (_, rules2) =
        let a_r, r_r, m_r = rule_diffs rules1 rules2 in
        a_r = [] && r_r = [] && m_r = []
      in
      find_diffs ~key_of ~key_equal ~is_empty_diff layers1 layers2
    in

    (* Supports queries *)
    let supports1 = extract_statements Css.as_supports expected in
    let supports2 = extract_statements Css.as_supports actual in
    let supports_added, supports_removed, supports_modified =
      media_diff supports1 supports2
    in

    (* Container queries *)
    let containers1 = extract_statements Css.as_container expected in
    let containers2 = extract_statements Css.as_container actual in
    let container_added, container_removed, container_modified =
      let key_of (name, cond, _) = (name, cond) in
      let key_equal (n1, c1) (n2, c2) = n1 = n2 && c1 = c2 in
      let is_empty_diff (_, _, rules1) (_, _, rules2) =
        let a_r, r_r, m_r = rule_diffs rules1 rules2 in
        a_r = [] && r_r = [] && m_r = []
      in
      find_diffs ~key_of ~key_equal ~is_empty_diff containers1 containers2
    in

    (* @property rules *)
    let properties1 = extract_statements Css.as_property expected in
    let properties2 = extract_statements Css.as_property actual in
    let prop_added, prop_removed, prop_modified =
      let key_of prop_info =
        match prop_info with Css.Property_info { name; _ } -> name
      in
      let key_equal = String.equal in
      let is_empty_diff p1 p2 = p1 = p2 in
      find_diffs ~key_of ~key_equal ~is_empty_diff properties1 properties2
    in

    (* Convert media queries *)
    List.map
      (fun (cond, _) ->
        Container_added { container_type = `Media; condition = cond })
      media_added
    @ List.map
        (fun (cond, _) ->
          Container_removed { container_type = `Media; condition = cond })
        media_removed
    @ List.map
        (fun (cond, rules1, rules2) ->
          let r_added, r_removed, r_modified = rule_diffs rules1 rules2 in
          let rule_changes =
            List.map convert_added_rule r_added
            @ List.map convert_removed_rule r_removed
            @ List.map convert_modified_rule r_modified
          in
          Container_modified
            {
              info = { container_type = `Media; condition = cond };
              rule_changes;
            })
        media_modified
    (* Convert layers *)
    @ List.map
        (fun (name_opt, _) ->
          let name = Option.value ~default:"" name_opt in
          Container_added { container_type = `Layer; condition = name })
        layers_added
    @ List.map
        (fun (name_opt, _) ->
          let name = Option.value ~default:"" name_opt in
          Container_removed { container_type = `Layer; condition = name })
        layers_removed
    @ List.map
        (fun ((name_opt, rules1), (_, rules2)) ->
          let name = Option.value ~default:"" name_opt in
          let r_added, r_removed, r_modified = rule_diffs rules1 rules2 in
          let rule_changes =
            List.map convert_added_rule r_added
            @ List.map convert_removed_rule r_removed
            @ List.map convert_modified_rule r_modified
          in
          Container_modified
            {
              info = { container_type = `Layer; condition = name };
              rule_changes;
            })
        layers_modified
    (* Convert supports queries *)
    @ List.map
        (fun (cond, _) ->
          Container_added { container_type = `Supports; condition = cond })
        supports_added
    @ List.map
        (fun (cond, _) ->
          Container_removed { container_type = `Supports; condition = cond })
        supports_removed
    @ List.map
        (fun (cond, rules1, rules2) ->
          let r_added, r_removed, _r_modified = rule_diffs rules1 rules2 in
          let rule_changes =
            List.map convert_added_rule r_added
            @ List.map convert_removed_rule r_removed
          in
          Container_modified
            {
              info = { container_type = `Supports; condition = cond };
              rule_changes;
            })
        supports_modified
    (* Convert container queries *)
    @ List.map
        (fun (name, cond, _) ->
          let name_str = Option.value ~default:"" name in
          let full_condition =
            if name_str = "" then cond else name_str ^ " " ^ cond
          in
          Container_added
            { container_type = `Container; condition = full_condition })
        container_added
    @ List.map
        (fun (name, cond, _) ->
          let name_str = Option.value ~default:"" name in
          let full_condition =
            if name_str = "" then cond else name_str ^ " " ^ cond
          in
          Container_removed
            { container_type = `Container; condition = full_condition })
        container_removed
    @ List.map
        (fun ((name, cond, rules1), (_, _, rules2)) ->
          let name_str = Option.value ~default:"" name in
          let full_condition =
            if name_str = "" then cond else name_str ^ " " ^ cond
          in
          let r_added, r_removed, _r_modified = rule_diffs rules1 rules2 in
          let rule_changes =
            List.map convert_added_rule r_added
            @ List.map convert_removed_rule r_removed
          in
          Container_modified
            {
              info = { container_type = `Container; condition = full_condition };
              rule_changes;
            })
        container_modified
    (* Convert @property rules *)
    @ List.map
        (fun prop_info ->
          let name =
            match prop_info with Css.Property_info { name; _ } -> name
          in
          Container_added { container_type = `Property; condition = name })
        prop_added
    @ List.map
        (fun prop_info ->
          let name =
            match prop_info with Css.Property_info { name; _ } -> name
          in
          Container_removed { container_type = `Property; condition = name })
        prop_removed
    @ List.map
        (fun (prop1, _prop2) ->
          let name = match prop1 with Css.Property_info { name; _ } -> name in
          (* For @property rules, we can show the changes as rule changes *)
          Container_modified
            {
              info = { container_type = `Property; condition = name };
              rule_changes = [];
              (* @property changes would need special handling *)
            })
        prop_modified
  in

  { rules = rule_changes; containers }

(* Compare two CSS ASTs directly *)
let compare_css css1 css2 =
  let css1 = strip_header css1 in
  let css2 = strip_header css2 in
  match (Css.of_string css1, Css.of_string css2) with
  | Ok expected, Ok actual ->
      let d = tree_diff ~expected ~actual in
      is_empty d
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
type t =
  | Tree_diff of tree_diff (* CSS AST differences found *)
  | String_diff of string_diff (* No structural diff but strings differ *)
  | No_diff (* Strings are identical *)
  | Both_errors of Css.parse_error * Css.parse_error
  | Expected_error of Css.parse_error
  | Actual_error of Css.parse_error

(* Helper: find line number and column for a character position *)
let find_line_and_column lines pos =
  let rec find line_num char_count = function
    | [] -> (line_num - 1, pos - char_count, [])
    | line :: rest ->
        let line_len = String.length line + 1 in
        if char_count + line_len > pos then
          (line_num, pos - char_count, line :: rest)
        else find (line_num + 1) (char_count + line_len) rest
  in
  find 0 0 lines

(* Helper: extract context lines before a given line number *)
let get_context_before lines line_num context_size =
  let before_lines = list_take line_num lines in
  let context_start = max 0 (List.length before_lines - context_size) in
  list_drop context_start before_lines

(* Create a string diff with context for character-level differences *)
let string_diff ~expected ~actual =
  match find_first_diff expected actual with
  | None -> None
  | Some pos ->
      let context_size = default_context_size in
      let lines_expected = String.split_on_char '\n' expected in
      let lines_actual = String.split_on_char '\n' actual in

      (* Find line and column for the diff position *)
      let line_exp, col_exp, remaining_exp =
        find_line_and_column lines_expected pos
      in
      let line_act, col_act, remaining_act =
        find_line_and_column lines_actual pos
      in

      (* Get context lines before the diff *)
      let context_before_exp =
        get_context_before lines_expected line_exp context_size
      in
      let context_before_act =
        get_context_before lines_actual line_act context_size
      in
      let context_before =
        zip_with_empty context_before_exp context_before_act
      in

      (* Get the lines containing the diff *)
      let diff_line_exp = match remaining_exp with [] -> "" | h :: _ -> h in
      let diff_line_act = match remaining_act with [] -> "" | h :: _ -> h in

      (* Get context lines after the diff *)
      let context_after_exp =
        match remaining_exp with [] -> [] | _ :: t -> list_take context_size t
      in
      let context_after_act =
        match remaining_act with [] -> [] | _ :: t -> list_take context_size t
      in
      let context_after = zip_with_empty context_after_exp context_after_act in

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

let show_string_diff_context ~expected ~actual = string_diff ~expected ~actual

let diff ~expected ~actual =
  let expected = strip_header expected in
  let actual = strip_header actual in
  match (Css.of_string expected, Css.of_string actual) with
  | Ok expected_ast, Ok actual_ast ->
      let structural_diff =
        tree_diff ~expected:expected_ast ~actual:actual_ast
      in
      if is_empty structural_diff then
        (* No structural differences, check string differences *)
        if expected = actual then No_diff
        else
          match string_diff ~expected ~actual with
          | Some sdiff -> String_diff sdiff
          | None -> No_diff (* Shouldn't happen if strings differ *)
      else Tree_diff structural_diff
  | Error e1, Error e2 -> Both_errors (e1, e2)
  | Ok _, Error e -> Actual_error e
  | Error e, Ok _ -> Expected_error e

(* Pretty-print a string diff in format consistent with structural diffs *)
let pp_string_diff ?(expected = "Expected") ?(actual = "Actual") fmt
    (sdiff : string_diff) =
  Fmt.pf fmt "@[<v>CSS strings differ at position %d (line %d, col %d)@,@,"
    sdiff.position sdiff.line_expected sdiff.column_expected;
  (* Git-style diff header *)
  Fmt.pf fmt "--- %s@," expected;
  Fmt.pf fmt "+++ %s@," actual;
  Fmt.pf fmt "@@ position %d @@@," sdiff.position;

  (* Print context before *)
  List.iter (pp_line_pair fmt) sdiff.context_before;

  (* Print the diff lines with caret using unified formatter *)
  let diff_exp, diff_act = sdiff.diff_lines in
  let config =
    {
      default_config with
      max_width = stats_max_width;
      show_caret = true;
      indent = 1;
    }
  in
  match format_value_diff ~config diff_exp diff_act with
  | `No_diff ->
      (* Shouldn't happen if strings differ *)
      Fmt.pf fmt "-%s@," diff_exp;
      Fmt.pf fmt "+%s@," diff_act
  | `Short_diff (exp, act) ->
      (* Short lines, show them completely *)
      Fmt.pf fmt "-%s@," exp;
      Fmt.pf fmt "+%s@," act;
      if sdiff.line_expected = sdiff.line_actual then
        pp_caret ~indent:1 fmt sdiff.column_expected
  | `Medium_diff (exp, act, pos) | `Long_diff (exp, act, pos) ->
      (* Long lines with truncation *)
      Fmt.pf fmt "-%s@," exp;
      Fmt.pf fmt "+%s@," act;
      if sdiff.line_expected = sdiff.line_actual then pp_caret ~indent:1 fmt pos;

      (* Print context after *)
      List.iter (pp_line_pair fmt) sdiff.context_after;
      Fmt.pf fmt "@]"

(* Format the result of diff with optional labels *)
let pp ?(expected = "Expected") ?(actual = "Actual") fmt = function
  | Tree_diff d ->
      (* Show structural differences *)
      pp_tree_diff ~expected ~actual fmt d
  | String_diff sdiff -> pp_string_diff ~expected ~actual fmt sdiff
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
  | Tree_diff d ->
      let actual_len = String.length actual_str in
      let expected_len = String.length expected_str in
      let char_diff = abs (actual_len - expected_len) in
      let char_diff_pct =
        if expected_len > 0 then
          float_of_int char_diff *. 100.0 /. float_of_int expected_len
        else 0.0
      in

      Fmt.pf fmt "@[<v>CSS: %d chars vs %d chars (%.1f%% diff)@," actual_len
        expected_len char_diff_pct;

      (* Count only meaningful differences, ignoring reorderings *)
      let added_rules =
        List.filter (function Rule_added _ -> true | _ -> false) d.rules
      in
      let removed_rules =
        List.filter (function Rule_removed _ -> true | _ -> false) d.rules
      in
      let changed_rules =
        List.filter
          (function
            | Rule_content_changed _ | Rule_selector_changed _ -> true
            | _ -> false)
          d.rules
      in
      let reordered_rules =
        List.filter (function Rule_reordered _ -> true | _ -> false) d.rules
      in

      let meaningful_changes =
        List.length added_rules + List.length removed_rules
        + List.length changed_rules
      in

      if meaningful_changes > 0 then
        Fmt.pf fmt
          "Structural diff: +%d -%d ~%d rules (ignoring %d reorderings)@,"
          (List.length added_rules)
          (List.length removed_rules)
          (List.length changed_rules)
          (List.length reordered_rules)
      else if List.length reordered_rules > 0 then
        Fmt.pf fmt "Only reordering differences (%d rules moved)@,"
          (List.length reordered_rules)
      else if List.length d.containers > 0 then
        Fmt.pf fmt "Differences in containers (%d containers)@,"
          (List.length d.containers)
      else Fmt.pf fmt "Other differences in nested contexts@,";

      Fmt.pf fmt "@]"
  | String_diff sdiff ->
      Fmt.pf fmt "@[<v>CSS strings differ at position %d (line %d, col %d)@]"
        sdiff.position sdiff.line_expected sdiff.column_expected
  | No_diff -> Fmt.pf fmt "CSS files are identical"
  | Both_errors _ -> Fmt.pf fmt "Both CSS files have parse errors"
  | Expected_error _ -> Fmt.pf fmt "Expected CSS has parse error"
  | Actual_error _ -> Fmt.pf fmt "Actual CSS has parse error"
