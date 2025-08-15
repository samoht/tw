(** Script to compare two CSS files with structural parsing for better diffs
    Usage: compare_css.exe <css_file1> <css_file2> *)

open Fmt

let read_file path =
  try
    let ic = open_in path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Some content
  with Sys_error msg ->
    epr "Error reading %s: %s@." path msg;
    None

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

let compare_css css1 css2 =
  let tokens1 = tokenize css1 in
  let tokens2 = tokenize css2 in
  let blocks1 = parse_blocks tokens1 in
  let blocks2 = parse_blocks tokens2 in

  blocks1 = blocks2

let () =
  (* Parse command line arguments *)
  let args = Array.to_list Sys.argv |> List.tl in

  match args with
  | [ css_file1; css_file2 ] ->
      (* Check if files exist *)
      if not (Sys.file_exists css_file1) then (
        epr "Error: CSS file '%s' does not exist@." css_file1;
        exit 1);

      if not (Sys.file_exists css_file2) then (
        epr "Error: CSS file '%s' does not exist@." css_file2;
        exit 1);

      pr "Comparing %s with %s@." css_file1 css_file2;

      (* Read first CSS file *)
      let css1 =
        match read_file css_file1 with Some css -> css | None -> exit 1
      in

      (* Read second CSS file *)
      let css2 =
        match read_file css_file2 with Some css -> css | None -> exit 1
      in

      (* Strip header comments from both files *)
      let css1 = strip_header css1 in
      let css2 = strip_header css2 in

      (* Compare *)
      let identical = compare_css css1 css2 in

      if identical then pr "✓ CSS files are structurally identical@."
      else (
        pr "✗ CSS files differ structurally@.@.";

        (* Parse both files into blocks *)
        let tokens1 = tokenize css1 in
        let tokens2 = tokenize css2 in
        let blocks1 = parse_blocks tokens1 in
        let blocks2 = parse_blocks tokens2 in

        (* Show structural differences *)
        let rec show_block_diff b1 b2 n =
          if n > 20 then pr "... (more differences)@."
          else
            match (b1, b2) with
            | [], [] -> ()
            | [], rest ->
                pr "+ File 2 has %d additional blocks@." (List.length rest)
            | rest, [] -> pr "- File 2 missing %d blocks@." (List.length rest)
            | Rule r1 :: t1, Rule r2 :: t2 ->
                if r1.selector <> r2.selector then (
                  pr "Block %d: Selector mismatch@." n;
                  pr "  File 1: %s@." r1.selector;
                  pr "  File 2: %s@." r2.selector)
                else if r1.properties <> r2.properties then (
                  pr "Block %d: Properties differ for %s@." n r1.selector;
                  let props1 = r1.properties in
                  let props2 = r2.properties in
                  List.iter
                    (fun (p, v) ->
                      if not (List.mem (p, v) props2) then
                        pr "  - Missing in file 2: %s: %s@." p v)
                    props1;
                  List.iter
                    (fun (p, v) ->
                      if not (List.mem (p, v) props1) then
                        pr "  + Extra in file 2: %s: %s@." p v)
                    props2);
                show_block_diff t1 t2 (n + 1)
            | AtBlock (at1, sub1) :: t1, AtBlock (at2, sub2) :: t2 ->
                if at1 <> at2 then (
                  pr "Block %d: @-rule mismatch@." n;
                  pr "  File 1: %s@." at1;
                  pr "  File 2: %s@." at2)
                else if sub1 <> sub2 then
                  pr "Block %d: Contents differ in %s@." n at1;
                show_block_diff t1 t2 (n + 1)
            | Layer l1 :: t1, Layer l2 :: t2 ->
                if l1 <> l2 then (
                  pr "Block %d: Layer declaration mismatch@." n;
                  pr "  File 1: %s@." l1;
                  pr "  File 2: %s@." l2);
                show_block_diff t1 t2 (n + 1)
            | _ :: t1, _ :: t2 ->
                pr "Block %d: Type mismatch@." n;
                show_block_diff t1 t2 (n + 1)
        in
        show_block_diff blocks1 blocks2 1;

        pr "@.To see raw text differences, run:@.";
        pr "  diff -u %s %s@." css_file1 css_file2);

      (* Exit with appropriate code *)
      exit (if identical then 0 else 1)
  | [ "--help" ] | [ "-h" ] ->
      pr "Usage: %s <css_file1> <css_file2>@.@." Sys.argv.(0);
      pr "Compare two CSS files for byte-for-byte equality.@.@.";
      pr "Arguments:@.";
      pr "  css_file1    First CSS file@.";
      pr "  css_file2    Second CSS file@.@.";
      pr "Note: Header comments are automatically stripped.@.@.";
      pr "Example:@.";
      pr "  %s examples/simple.css examples/simple.tailwind.css@." Sys.argv.(0);
      exit 0
  | _ ->
      epr "Error: Invalid arguments@.";
      epr "Usage: %s <css_file1> <css_file2>@." Sys.argv.(0);
      epr "Run '%s --help' for more information@." Sys.argv.(0);
      exit 1
