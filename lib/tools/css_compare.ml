(** Simplified CSS comparison utilities for testing *)

let strip_header css =
  (* Strip any header comment if present *)
  if String.starts_with ~prefix:"/*!" css then
    match String.index_opt css '\n' with
    | Some idx -> String.sub css (idx + 1) (String.length css - idx - 1)
    | None -> css
  else css

(* CSS tokenizer for structural comparison *)
type token =
  | Selector of string
  | Property of string * string
  | Open_brace
  | Close_brace
  | At_rule of string
  | Semicolon
  | Comma

(* Tokenizer helpers (hoisted to reduce nesting/length) *)
let is_ws = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let rec skip_whitespace css len pos =
  if pos >= len then pos
  else if is_ws css.[pos] then skip_whitespace css len (pos + 1)
  else pos

let rec read_until_char css len chars start_pos =
  if start_pos >= len then start_pos
  else if List.mem css.[start_pos] chars then start_pos
  else read_until_char css len chars (start_pos + 1)

let rec read_string_literal css len quote_char pos escaped =
  if pos >= len then pos
  else if escaped then read_string_literal css len quote_char (pos + 1) false
  else
    match css.[pos] with
    | '\\' -> read_string_literal css len quote_char (pos + 1) true
    | c when c = quote_char -> pos + 1
    | _ -> read_string_literal css len quote_char (pos + 1) false

let rec read_property_value css len pos in_string paren_depth =
  if pos >= len then pos
  else
    match css.[pos] with
    | (';' | '}') when (not in_string) && paren_depth = 0 -> pos
    | ('"' | '\'') as quote when not in_string ->
        let end_pos = read_string_literal css len quote (pos + 1) false in
        read_property_value css len end_pos in_string paren_depth
    | '(' when not in_string ->
        read_property_value css len (pos + 1) in_string (paren_depth + 1)
    | ')' when (not in_string) && paren_depth > 0 ->
        read_property_value css len (pos + 1) in_string (paren_depth - 1)
    | _ -> read_property_value css len (pos + 1) in_string paren_depth

let skip_comment css len pos =
  if pos + 1 < len && css.[pos] = '/' && css.[pos + 1] = '*' then (
    let p = ref (pos + 2) in
    let result = ref None in
    while !p + 1 < len && !result = None do
      if css.[!p] = '*' && css.[!p + 1] = '/' then result := Some (!p + 2)
      else incr p
    done;
    match !result with Some v -> Some v | None -> Some len)
  else None

let read_at_rule css len pos =
  let end_pos = read_until_char css len [ '{'; ';' ] (pos + 1) in
  let at_rule = String.sub css pos (end_pos - pos) |> String.trim in
  (At_rule at_rule, end_pos)

let rec colon_pos css len start_pos =
  if start_pos >= len then None
  else
    match css.[start_pos] with
    | ':' -> Some start_pos
    | '{' | '}' | ';' -> None
    | _ -> colon_pos css len (start_pos + 1)

let read_property_or_selector css len pos =
  match colon_pos css len pos with
  | Some cpos when cpos > pos ->
      let prop = String.sub css pos (cpos - pos) |> String.trim in
      let value_end = read_property_value css len (cpos + 1) false 0 in
      let value =
        String.sub css (cpos + 1) (value_end - cpos - 1) |> String.trim
      in
      (Some (Property (prop, value)), value_end)
  | _ ->
      let end_pos = read_until_char css len [ '{'; '}'; ';'; ',' ] pos in
      let text = String.sub css pos (end_pos - pos) |> String.trim in
      if text <> "" then (Some (Selector text), end_pos) else (None, end_pos)

let rec tokenize_impl css len acc pos =
  let pos = skip_whitespace css len pos in
  if pos >= len then List.rev acc
  else
    match css.[pos] with
    | '{' -> tokenize_impl css len (Open_brace :: acc) (pos + 1)
    | '}' -> tokenize_impl css len (Close_brace :: acc) (pos + 1)
    | ';' -> tokenize_impl css len (Semicolon :: acc) (pos + 1)
    | ',' -> tokenize_impl css len (Comma :: acc) (pos + 1)
    | '/' -> (
        match skip_comment css len pos with
        | Some new_pos -> tokenize_impl css len acc new_pos
        | None -> (
            let tok, new_pos = read_property_or_selector css len pos in
            match tok with
            | Some t -> tokenize_impl css len (t :: acc) new_pos
            | None -> tokenize_impl css len acc new_pos))
    | '@' ->
        let token, new_pos = read_at_rule css len pos in
        tokenize_impl css len (token :: acc) new_pos
    | _ -> (
        let tok, new_pos = read_property_or_selector css len pos in
        match tok with
        | Some t -> tokenize_impl css len (t :: acc) new_pos
        | None -> tokenize_impl css len acc new_pos)

let tokenize css =
  let len = String.length css in
  tokenize_impl css len [] 0

(* Build CSS structure from tokens for better comparison *)
type css_rule = { selector : string; properties : (string * string) list }

type css_block =
  | Rule of css_rule
  | At_block of string * css_block list
  | Layer of string

let parse_blocks tokens =
  let rec parse_rule_body acc = function
    | Property (p, v) :: Semicolon :: rest ->
        parse_rule_body ((p, v) :: acc) rest
    | Property (p, v) :: rest -> parse_rule_body ((p, v) :: acc) rest
    | Close_brace :: rest -> (List.rev acc, rest)
    | _ :: rest -> parse_rule_body acc rest
    | [] -> (List.rev acc, [])
  in

  let rec parse_blocks acc = function
    | [] -> List.rev acc
    | Selector sel :: Open_brace :: rest ->
        let props, rest' = parse_rule_body [] rest in
        parse_blocks (Rule { selector = sel; properties = props } :: acc) rest'
    | At_rule at :: Open_brace :: rest ->
        let blocks, rest' = parse_nested_blocks [] rest in
        parse_blocks (At_block (at, blocks) :: acc) rest'
    | At_rule at :: Semicolon :: rest -> parse_blocks (Layer at :: acc) rest
    | _ :: rest -> parse_blocks acc rest
  and parse_nested_blocks acc = function
    | Close_brace :: rest -> (List.rev acc, rest)
    | tokens ->
        let blocks = parse_blocks [] tokens in
        (blocks @ acc, [])
  in
  parse_blocks [] tokens

let rec normalize_blocks = function
  | [] -> []
  | Rule { selector; properties } :: rest ->
      (* Sort properties by key for comparison *)
      let sorted_props =
        List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2) properties
      in
      Rule { selector; properties = sorted_props } :: normalize_blocks rest
  | At_block (at, nested) :: rest ->
      At_block (at, normalize_blocks nested) :: normalize_blocks rest
  | Layer l :: rest -> Layer l :: normalize_blocks rest

(* Helper: count CSS patterns for a specific class *)
let count_css_class_patterns css class_name =
  (* Match .class followed by optional pseudo-class/selector then space and { *)
  let base_pattern =
    Re.compile
      Re.(
        seq
          [
            char '.';
            str class_name;
            alt
              [
                seq [ rep space; char '{' ];
                (* .class { *)
                seq [ char ':'; rep1 (compl [ set " {" ]); rep space; char '{' ];
                (* .class:hover { *)
              ];
          ])
  in
  let where_pattern =
    Re.compile Re.(seq [ char '.'; str class_name; rep1 space; str ":where" ])
  in
  let base_count = List.length (Re.all ~pos:0 base_pattern css) in
  let where_count = List.length (Re.all ~pos:0 where_pattern css) in
  (base_count, where_count, base_count + where_count)

(* Helper: find most common CSS class in file *)
let find_dominant_css_class css =
  let class_pattern =
    Re.compile Re.(seq [ char '.'; rep1 (alt [ alnum; char '_'; char '-' ]) ])
  in
  let matches = Re.all ~pos:0 class_pattern css in
  let classes =
    List.map
      (fun match_info ->
        let matched = Re.Group.get match_info 0 in
        String.sub matched 1 (String.length matched - 1))
      matches
  in
  let counts =
    List.fold_left
      (fun acc cls ->
        let current = try List.assoc cls acc with Not_found -> 0 in
        (cls, current + 1) :: List.remove_assoc cls acc)
      [] classes
  in
  List.fold_left
    (fun (best_cls, best_count) (cls, count) ->
      if count > best_count then (cls, count) else (best_cls, best_count))
    ("", 0) counts

let compare_css css1 css2 =
  if css1 = css2 then true
  else
    (* Do raw CSS analysis to detect structural issues normalize_blocks might
       hide *)
    let css1 = strip_header css1 in
    let css2 = strip_header css2 in
    let dominant_class1, count1 = find_dominant_css_class css1 in
    let dominant_class2, count2 = find_dominant_css_class css2 in

    if dominant_class1 = dominant_class2 && count1 > 5 && count2 > 5 then
      let tw_base, _tw_where, _tw_total =
        count_css_class_patterns css1 dominant_class1
      in
      let tailwind_base, _tailwind_where, _tailwind_total =
        count_css_class_patterns css2 dominant_class1
      in
      (* If rule counts differ, files are structurally different *)
      if abs (tw_base - tailwind_base) > 0 then false
      else
        (* Fall back to normalized comparison *)
        let tokens1 = tokenize css1 in
        let tokens2 = tokenize css2 in
        let blocks1 = parse_blocks tokens1 |> normalize_blocks in
        let blocks2 = parse_blocks tokens2 |> normalize_blocks in
        blocks1 = blocks2
    else
      (* Fall back to normalized comparison *)
      let tokens1 = tokenize css1 in
      let tokens2 = tokenize css2 in
      let blocks1 = parse_blocks tokens1 |> normalize_blocks in
      let blocks2 = parse_blocks tokens2 |> normalize_blocks in
      blocks1 = blocks2

(* Extract actual .prose{} rules from CSS - handle nested braces *)
let extract_base_rules css class_name =
  let pattern =
    Re.compile Re.(seq [ char '.'; str class_name; rep space; char '{' ])
  in
  let matches = Re.all ~pos:0 pattern css in
  List.map
    (fun match_info ->
      let start_pos = Re.Group.start match_info 0 in
      let matched = Re.Group.get match_info 0 in
      let brace_pos = String.rindex matched '{' + start_pos in
      (* Find matching closing brace *)
      let rec find_closing pos depth =
        if pos >= String.length css then pos
        else
          match css.[pos] with
          | '{' -> find_closing (pos + 1) (depth + 1)
          | '}' when depth = 1 -> pos + 1
          | '}' -> find_closing (pos + 1) (depth - 1)
          | _ -> find_closing (pos + 1) depth
      in
      let end_pos = find_closing (brace_pos + 1) 1 in
      String.sub css start_pos (end_pos - start_pos))
    matches

(* Helper: show rule truncated to max length *)
let truncate_rule rule max_len =
  if String.length rule > max_len then String.sub rule 0 max_len ^ "..."
  else rule

(* Helper: print rule lists *)
let print_rule_list add_line label class_name rules =
  add_line (Fmt.str "%s has these .%s{} rules:" label class_name);
  List.iteri
    (fun i rule ->
      add_line (Fmt.str "  [%d] %s" (i + 1) (truncate_rule rule 100)))
    rules

(* Helper: analyze non-consecutive rule merging *)
let analyze_rule_merging add_line dominant_class1 css2 tailwind_rules tw_rules =
  let tw_rule = List.hd tw_rules in
  let tailwind_rules_sorted =
    List.sort (fun a b -> String.length a - String.length b) tailwind_rules
  in
  let short_rule = List.hd tailwind_rules_sorted in
  let long_rule = List.hd (List.tl tailwind_rules_sorted) in

  let where_pattern =
    Re.Posix.compile_pat (Fmt.str {|\.%s :where|} dominant_class1)
  in
  let where_count = List.length (Re.all ~pos:0 where_pattern css2) in

  if String.length tw_rule > String.length short_rule then (
    add_line
      (Fmt.str "%a CRITICAL ISSUE: Non-consecutive rule merging detected"
         Fmt.(styled `Red string)
         "ERROR:");
    add_line "  → TW incorrectly merges NON-ADJACENT rules:";
    add_line (Fmt.str "  → FIRST: %s" (truncate_rule short_rule 60));
    add_line (Fmt.str "  → (%d :where rules in between)" where_count);
    add_line (Fmt.str "  → LAST:  %s" (truncate_rule long_rule 60));
    add_line "";
    add_line
      (Fmt.str "%a This violates CSS ordering principles!"
         Fmt.(styled `Yellow string)
         "WARNING:"))
  else (
    add_line "  → TW is missing the CSS variables rule";
    add_line (Fmt.str "  → Need to add: %s" (truncate_rule long_rule 80)))

(* Helper: show missing rules analysis *)
let show_missing_rules_analysis add_line tw_label tailwind_label dominant_class1
    diff css1 css2 =
  add_line
    (Fmt.str "  → %s missing %d .%s{} base rules" tw_label diff dominant_class1);

  let tw_rules = extract_base_rules css1 dominant_class1 in
  let tailwind_rules = extract_base_rules css2 dominant_class1 in

  add_line "\n━━━ Missing Rule Analysis ━━━";
  print_rule_list add_line tw_label dominant_class1 tw_rules;
  add_line "";
  print_rule_list add_line tailwind_label dominant_class1 tailwind_rules;

  add_line "\n━━━ Rule Ordering Analysis ━━━";
  if List.length tailwind_rules = 2 && List.length tw_rules = 1 then
    analyze_rule_merging add_line dominant_class1 css2 tailwind_rules tw_rules

(* Simple CSS structure analysis with specific rule content *)
let analyze_raw_css_structure add_line tw_label tailwind_label css1 css2 =
  let dominant_class1, count1 = find_dominant_css_class css1 in
  let dominant_class2, count2 = find_dominant_css_class css2 in

  if dominant_class1 = dominant_class2 && count1 > 5 && count2 > 5 then
    let tw_base, tw_where, _tw_total =
      count_css_class_patterns css1 dominant_class1
    in
    let tailwind_base, tailwind_where, _tailwind_total =
      count_css_class_patterns css2 dominant_class1
    in

    if abs (tw_base - tailwind_base) > 0 then (
      add_line "\n━━━ CSS Structure Analysis ━━━";
      add_line
        (Fmt.str "%s: %d .%s{} base rules, %d :where rules" tw_label tw_base
           dominant_class1 tw_where);
      add_line
        (Fmt.str "%s: %d .%s{} base rules, %d :where rules" tailwind_label
           tailwind_base dominant_class1 tailwind_where);

      let diff = tailwind_base - tw_base in
      if diff > 0 then
        show_missing_rules_analysis add_line tw_label tailwind_label
          dominant_class1 diff css1 css2
      else if diff < 0 then
        add_line
          (Fmt.str "  → %s has %d extra .%s{} base rules" tw_label (-diff)
             dominant_class1))

(* Show actionable property differences *)
let show_property_differences add_line tw_sel tw_props tailwind_props =
  add_line (Fmt.str "  Selector: %a" Fmt.(styled `Cyan string) tw_sel);

  let p1_map = List.to_seq tw_props |> Hashtbl.of_seq in
  let p2_map = List.to_seq tailwind_props |> Hashtbl.of_seq in
  let all_keys =
    List.map fst tw_props @ List.map fst tailwind_props
    |> List.sort_uniq String.compare
  in

  let diffs_found = ref false in
  List.iter
    (fun key ->
      match (Hashtbl.find_opt p1_map key, Hashtbl.find_opt p2_map key) with
      | Some v1, Some v2 when v1 <> v2 ->
          if not !diffs_found then (
            add_line "  Property differences:";
            diffs_found := true);
          add_line
            (Fmt.str "    %s: %a → %a" key
               Fmt.(styled `Red string)
               v1
               Fmt.(styled `Green string)
               v2)
      | Some v, None ->
          if not !diffs_found then (
            add_line "  Property differences:";
            diffs_found := true);
          add_line
            (Fmt.str "    %a %s: %s" Fmt.(styled `Red string) "- MISSING" key v)
      | None, Some v ->
          if not !diffs_found then (
            add_line "  Property differences:";
            diffs_found := true);
          add_line
            (Fmt.str "    %a %s: %s" Fmt.(styled `Green string) "+ EXTRA" key v)
      | _ -> ())
    all_keys

(* Simple structural diff *)
let structured_diff ~tw_label ~tailwind_label ?(css1 = "") ?(css2 = "") blocks1
    blocks2 =
  let result = Buffer.create 1024 in
  let add_line s =
    Buffer.add_string result s;
    Buffer.add_char result '\n'
  in

  (* Raw CSS structure analysis first if CSS provided *)
  if css1 <> "" && css2 <> "" then (
    analyze_raw_css_structure add_line tw_label tailwind_label css1 css2;
    Buffer.contents result)
  else
    (* Fallback to simple block comparison if no raw CSS *)
    let rec compare_blocks pos blocks1 blocks2 =
      match (blocks1, blocks2) with
      | [], [] -> ()
      | [], remaining ->
          add_line
            (Fmt.str "%a %s has %d extra blocks"
               Fmt.(styled `Green string)
               "+" tailwind_label (List.length remaining))
      | remaining, [] ->
          add_line
            (Fmt.str "%a %s has %d extra blocks"
               Fmt.(styled `Red string)
               "-" tw_label (List.length remaining))
      | Rule r1 :: rest1, Rule r2 :: rest2 ->
          if r1.selector = r2.selector && r1.properties <> r2.properties then (
            add_line
              (Fmt.str "%a %s - Property mismatch"
                 Fmt.(styled `Yellow string)
                 "≠" r1.selector);
            show_property_differences add_line r1.selector r1.properties
              r2.properties)
          else if r1.selector <> r2.selector then (
            add_line
              (Fmt.str "%a Selector mismatch at position %d"
                 Fmt.(styled `Red string)
                 "✗" pos);
            add_line (Fmt.str "  %s: %s" tw_label r1.selector);
            add_line (Fmt.str "  %s: %s" tailwind_label r2.selector));
          compare_blocks (pos + 1) rest1 rest2
      | At_block (at1, nested1) :: rest1, At_block (at2, nested2) :: rest2 ->
          (if at1 = at2 && nested1 <> nested2 then
             let count1, count2 = (List.length nested1, List.length nested2) in
             if count1 <> count2 then
               add_line
                 (Fmt.str "%a @%s content differs (%d vs %d rules)"
                    Fmt.(styled `Yellow string)
                    "≠" at1 count1 count2));
          compare_blocks (pos + 1) rest1 rest2
      | _b1 :: rest1, _b2 :: rest2 ->
          add_line
            (Fmt.str "%a Block %d - Type mismatch"
               Fmt.(styled `Red string)
               "✗" pos);
          compare_blocks (pos + 1) rest1 rest2
    in

    compare_blocks 0 blocks1 blocks2;
    Buffer.contents result

let format_diff our_css tailwind_css =
  if our_css = tailwind_css then "✓ CSS files are identical\n"
  else
    let our_css = strip_header our_css in
    let tailwind_css = strip_header tailwind_css in

    (* Try to parse structure for better comparison *)
    try
      let our_blocks = tokenize our_css |> parse_blocks |> normalize_blocks in
      let tailwind_blocks =
        tokenize tailwind_css |> parse_blocks |> normalize_blocks
      in

      (* Always do the detailed analysis since normalize_blocks can hide
         important differences *)
      let header = "✗ CSS files differ structurally\n\n" in
      (* Don't pass css1/css2 for simple diffs - let it use block comparison *)
      let simple_diff =
        structured_diff ~tw_label:"tw" ~tailwind_label:"tailwind" our_blocks
          tailwind_blocks
      in
      header ^ simple_diff
    with _ ->
      (* Fall back to simple character diff if parsing fails *)
      let rec find_first_diff i =
        if i >= String.length our_css && i >= String.length tailwind_css then
          None
        else if i >= String.length our_css then
          Some
            ( i,
              "TW shorter",
              Fmt.str "Tailwind has extra: %s"
                (String.sub tailwind_css i
                   (min 200 (String.length tailwind_css - i))) )
        else if i >= String.length tailwind_css then
          Some
            ( i,
              "Tailwind shorter",
              Fmt.str "TW has extra: %s"
                (String.sub our_css i (min 200 (String.length our_css - i))) )
        else if our_css.[i] <> tailwind_css.[i] then
          let start = max 0 (i - 80) in
          let len1 = min 160 (String.length our_css - start) in
          let len2 = min 160 (String.length tailwind_css - start) in
          let context1 = String.sub our_css start len1 in
          let context2 = String.sub tailwind_css start len2 in
          Some
            ( i,
              "Character difference",
              Fmt.str "TW: ...%s...\nTailwind: ...%s..." context1 context2 )
        else find_first_diff (i + 1)
      in

      Fmt.str "✗ CSS files differ\n\nLength: TW=%d, Tailwind=%d\n\n%s"
        (String.length our_css)
        (String.length tailwind_css)
        (match find_first_diff 0 with
        | Some (pos, reason, context) ->
            Fmt.str "First difference at position %d (%s):\n\n%s" pos reason
              context
        | None -> "Files appear identical but comparison failed")
