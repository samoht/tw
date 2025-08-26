(** CSS comparison utilities for testing *)

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
        let end_pos = read_string_literal css len quote pos false in
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
    while !p + 1 < len && !result <> Some 0 do
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

let compare_css css1 css2 =
  let tokens1 = tokenize css1 in
  let tokens2 = tokenize css2 in
  let blocks1 = parse_blocks tokens1 |> normalize_blocks in
  let blocks2 = parse_blocks tokens2 |> normalize_blocks in
  blocks1 = blocks2

(* Labels and diff helpers *)
let label_of_css_header css =
  if String.starts_with ~prefix:"/* tw v" css then
    let end_idx = try String.index_from css 7 ' ' with Not_found -> 20 in
    "tw v" ^ String.sub css 7 (min (end_idx - 7) 20)
  else if String.starts_with ~prefix:"/* ≈ tailwindcss" css then "tailwind"
  else "unknown"

let pp_first_block_diff ~tw_label ~tailwind_label add_line blocks1 blocks2 =
  let rec aux b1 b2 =
    match (b1, b2) with
    | [], [] -> ()
    | [], rest ->
        add_line
          (Fmt.str "%a has %d additional blocks"
             Fmt.(styled `Blue string)
             tailwind_label (List.length rest))
    | rest, [] ->
        add_line
          (Fmt.str "%a missing %d blocks"
             Fmt.(styled `Green string)
             tw_label (List.length rest))
    | Rule r1 :: _, Rule r2 :: _ when r1.selector <> r2.selector ->
        add_line (Fmt.str "Selector mismatch: %s vs %s" r1.selector r2.selector)
    | Rule r1 :: _, Rule r2 :: _ when r1.properties <> r2.properties ->
        add_line (Fmt.str "Properties differ in %s" r1.selector)
    | At_block (at1, _) :: _, At_block (at2, _) :: _ when at1 <> at2 ->
        add_line (Fmt.str "@-rule mismatch: %s vs %s" at1 at2)
    | At_block (at1, sub1) :: t1, At_block (_, sub2) :: t2 when sub1 <> sub2 ->
        add_line
          (Fmt.str "%a in %a"
             Fmt.(styled `Yellow string)
             "Content differs"
             Fmt.(styled `Cyan string)
             at1);
        aux t1 t2
    | Layer l1 :: _, Layer l2 :: _ when l1 <> l2 ->
        add_line (Fmt.str "Layer mismatch: %s vs %s" l1 l2)
    | _ :: t1, _ :: t2 -> aux t1 t2
  in
  aux blocks1 blocks2

let format_diff our_css tailwind_css =
  let buf = Buffer.create 1024 in
  let add_line s =
    Buffer.add_string buf s;
    Buffer.add_char buf '\n'
  in
  let tw_label =
    let v = label_of_css_header our_css in
    if v = "unknown" then "tw" else v
  in
  let tailwind_label =
    let v = label_of_css_header tailwind_css in
    if v = "unknown" then "tailwind" else v
  in
  let our_css = strip_header our_css in
  let tailwind_css = strip_header tailwind_css in
  let our_blocks = tokenize our_css |> parse_blocks in
  let tailwind_blocks = tokenize tailwind_css |> parse_blocks in
  if our_blocks = tailwind_blocks then
    add_line "✓ CSS files are structurally identical"
  else (
    add_line "";
    pp_first_block_diff ~tw_label ~tailwind_label add_line our_blocks
      tailwind_blocks);
  Buffer.contents buf
