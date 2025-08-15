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
  | OpenBrace
  | CloseBrace
  | AtRule of string
  | Semicolon
  | Comma

let token_to_string = function
  | Selector s -> Fmt.str "Selector(%s)" s
  | Property (p, v) -> Fmt.str "Property(%s: %s)" p v
  | OpenBrace -> "{"
  | CloseBrace -> "}"
  | AtRule s -> Fmt.str "@%s" s
  | Semicolon -> ";"
  | Comma -> ","

let tokenize css =
  let len = String.length css in
  let rec skip_whitespace pos =
    if pos >= len then pos
    else
      match css.[pos] with
      | ' ' | '\t' | '\n' | '\r' -> skip_whitespace (pos + 1)
      | _ -> pos
  in

  let read_until_char chars start_pos =
    let rec loop pos =
      if pos >= len then pos
      else if List.mem css.[pos] chars then pos
      else loop (pos + 1)
    in
    loop start_pos
  in

  let read_string_literal quote_char start_pos =
    let rec loop pos escaped =
      if pos >= len then pos
      else if escaped then loop (pos + 1) false
      else
        match css.[pos] with
        | '\\' -> loop (pos + 1) true
        | c when c = quote_char -> pos + 1
        | _ -> loop (pos + 1) false
    in
    loop (start_pos + 1) false
  in

  let read_property_value start_pos =
    let rec loop pos in_string paren_depth =
      if pos >= len then pos
      else
        match css.[pos] with
        | (';' | '}') when (not in_string) && paren_depth = 0 -> pos
        | ('"' | '\'') as quote when not in_string ->
            let end_pos = read_string_literal quote pos in
            loop end_pos in_string paren_depth
        | '(' when not in_string -> loop (pos + 1) in_string (paren_depth + 1)
        | ')' when (not in_string) && paren_depth > 0 ->
            loop (pos + 1) in_string (paren_depth - 1)
        | _ -> loop (pos + 1) in_string paren_depth
    in
    loop start_pos false 0
  in

  let rec tokenize_impl acc pos =
    let pos = skip_whitespace pos in
    if pos >= len then List.rev acc
    else
      match css.[pos] with
      | '{' -> tokenize_impl (OpenBrace :: acc) (pos + 1)
      | '}' -> tokenize_impl (CloseBrace :: acc) (pos + 1)
      | ';' -> tokenize_impl (Semicolon :: acc) (pos + 1)
      | ',' -> tokenize_impl (Comma :: acc) (pos + 1)
      | '/' when pos + 1 < len && css.[pos + 1] = '*' ->
          (* Skip comments *)
          let rec find_end p =
            if p + 1 >= len then len
            else if css.[p] = '*' && css.[p + 1] = '/' then p + 2
            else find_end (p + 1)
          in
          tokenize_impl acc (find_end (pos + 2))
      | '@' ->
          (* Read at-rule *)
          let end_pos = read_until_char [ '{'; ';' ] (pos + 1) in
          let at_rule = String.sub css pos (end_pos - pos) |> String.trim in
          tokenize_impl (AtRule at_rule :: acc) end_pos
      | _ -> (
          (* Try to read property or selector *)
          let colon_pos = ref None in
          let rec find_colon p =
            if p >= len then ()
            else
              match css.[p] with
              | ':' -> colon_pos := Some p
              | '{' | '}' | ';' -> ()
              | _ -> find_colon (p + 1)
          in
          find_colon pos;

          match !colon_pos with
          | Some cpos when cpos > pos ->
              (* It's a property *)
              let prop = String.sub css pos (cpos - pos) |> String.trim in
              let value_end = read_property_value (cpos + 1) in
              let value =
                String.sub css (cpos + 1) (value_end - cpos - 1) |> String.trim
              in
              tokenize_impl (Property (prop, value) :: acc) value_end
          | _ ->
              (* It's a selector *)
              let end_pos = read_until_char [ '{'; '}'; ';'; ',' ] pos in
              let text = String.sub css pos (end_pos - pos) |> String.trim in
              if text <> "" then tokenize_impl (Selector text :: acc) end_pos
              else tokenize_impl acc end_pos)
  in
  tokenize_impl [] 0

(* Build CSS structure from tokens for better comparison *)
type css_rule = { selector : string; properties : (string * string) list }

type css_block =
  | Rule of css_rule
  | AtBlock of string * css_block list
  | Layer of string

let parse_blocks tokens =
  let rec parse_rule_body acc = function
    | Property (p, v) :: Semicolon :: rest ->
        parse_rule_body ((p, v) :: acc) rest
    | Property (p, v) :: rest -> parse_rule_body ((p, v) :: acc) rest
    | CloseBrace :: rest -> (List.rev acc, rest)
    | _ :: rest -> parse_rule_body acc rest
    | [] -> (List.rev acc, [])
  in

  let rec parse_blocks acc = function
    | [] -> List.rev acc
    | Selector sel :: OpenBrace :: rest ->
        let props, rest' = parse_rule_body [] rest in
        parse_blocks (Rule { selector = sel; properties = props } :: acc) rest'
    | AtRule at :: OpenBrace :: rest ->
        let blocks, rest' = parse_nested_blocks [] rest in
        parse_blocks (AtBlock (at, blocks) :: acc) rest'
    | AtRule at :: Semicolon :: rest -> parse_blocks (Layer at :: acc) rest
    | _ :: rest -> parse_blocks acc rest
  and parse_nested_blocks acc = function
    | CloseBrace :: rest -> (List.rev acc, rest)
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
  | AtBlock (at, nested) :: rest ->
      AtBlock (at, normalize_blocks nested) :: normalize_blocks rest
  | Layer l :: rest -> Layer l :: normalize_blocks rest

let compare_css css1 css2 =
  let tokens1 = tokenize css1 in
  let tokens2 = tokenize css2 in
  let blocks1 = parse_blocks tokens1 |> normalize_blocks in
  let blocks2 = parse_blocks tokens2 |> normalize_blocks in
  blocks1 = blocks2

let format_diff our_css tailwind_css =
  let buf = Buffer.create 1024 in
  let add_line s =
    Buffer.add_string buf s;
    Buffer.add_char buf '\n'
  in

  (* Strip headers *)
  let our_css = strip_header our_css in
  let tailwind_css = strip_header tailwind_css in

  (* Parse both files into blocks *)
  let our_tokens = tokenize our_css in
  let tailwind_tokens = tokenize tailwind_css in
  let our_blocks = parse_blocks our_tokens in
  let tailwind_blocks = parse_blocks tailwind_tokens in

  if our_blocks = tailwind_blocks then
    add_line "✓ CSS files are structurally identical"
  else (
    add_line "✗ CSS files differ structurally";
    add_line "";
    add_line "Comparing: [TW Library Output] vs [Tailwind CSS Output]";
    add_line "";

    (* Show structural differences *)
    let rec show_block_diff b1 b2 n =
      if n > 20 then add_line "... (more differences)"
      else
        match (b1, b2) with
        | [], [] -> ()
        | [], rest ->
            add_line
              (Fmt.str "+ Tailwind has %d additional blocks" (List.length rest))
        | rest, [] ->
            add_line
              (Fmt.str "- TW Library missing %d blocks" (List.length rest))
        | Rule r1 :: t1, Rule r2 :: t2 ->
            if r1.selector <> r2.selector then (
              add_line (Fmt.str "Block %d: Selector mismatch" n);
              add_line (Fmt.str "  TW Library: %s" r1.selector);
              add_line (Fmt.str "  Tailwind:   %s" r2.selector))
            else if r1.properties <> r2.properties then (
              add_line
                (Fmt.str "Block %d: Properties differ for %s" n r1.selector);
              let props1 = r1.properties in
              let props2 = r2.properties in
              List.iter
                (fun (p, v) ->
                  if not (List.mem (p, v) props2) then
                    add_line
                      (Fmt.str "  - TW Library has (Tailwind missing): %s: %s" p
                         v))
                props1;
              List.iter
                (fun (p, v) ->
                  if not (List.mem (p, v) props1) then
                    add_line
                      (Fmt.str "  + Tailwind has (TW Library missing): %s: %s" p
                         v))
                props2);
            show_block_diff t1 t2 (n + 1)
        | AtBlock (at1, sub1) :: t1, AtBlock (at2, sub2) :: t2 ->
            if at1 <> at2 then (
              add_line (Fmt.str "Block %d: @-rule mismatch" n);
              add_line (Fmt.str "  TW Library: %s" at1);
              add_line (Fmt.str "  Tailwind:   %s" at2))
            else if sub1 <> sub2 then
              add_line (Fmt.str "Block %d: Contents differ in %s" n at1);
            show_block_diff t1 t2 (n + 1)
        | Layer l1 :: t1, Layer l2 :: t2 ->
            if l1 <> l2 then (
              add_line (Fmt.str "Block %d: Layer declaration mismatch" n);
              add_line (Fmt.str "  TW Library: %s" l1);
              add_line (Fmt.str "  Tailwind:   %s" l2));
            show_block_diff t1 t2 (n + 1)
        | _ :: t1, _ :: t2 ->
            add_line (Fmt.str "Block %d: Type mismatch" n);
            show_block_diff t1 t2 (n + 1)
    in
    show_block_diff our_blocks tailwind_blocks 1);

  Buffer.contents buf
