let is_whitespace = function
  | 0x20 | 0x09 | 0x0a | 0x0d | 0x0c -> true
  | _ -> false

let split_whitespace s =
  let len = String.length s in
  let rec skip_ws i =
    if i < len && is_whitespace (Char.code s.[i]) then skip_ws (i + 1) else i
  in
  let rec find_ws i =
    if i < len && not (is_whitespace (Char.code s.[i])) then find_ws (i + 1)
    else i
  in
  let rec loop i acc =
    let start = skip_ws i in
    if start >= len then List.rev acc
    else
      let stop = find_ws start in
      loop stop (String.sub s start (stop - start) :: acc)
  in
  loop 0 []

type decoded = { starts : int array; chars : int array }

let decoded_utf_8 source =
  let chars_rev, starts_rev =
    Uutf.String.fold_utf_8
      (fun (chars, starts) byte_pos decoded ->
        let code =
          match decoded with `Uchar u -> Uchar.to_int u | `Malformed _ -> -1
        in
        (code :: chars, byte_pos :: starts))
      ([], []) source
  in
  {
    starts = Array.of_list (List.rev (String.length source :: starts_rev));
    chars = Array.of_list (List.rev chars_rev);
  }

let char_at d i = d.chars.(i)
let byte_at d i = d.starts.(i)
let is_ascii_digit c = c >= 0x30 && c <= 0x39

let is_candidate_start d i =
  match char_at d i with
  | c when c >= 0x61 && c <= 0x7a -> true
  | c when is_ascii_digit c -> true
  | 0x2d | 0x21 | 0x40 | 0x2a -> true
  | 0x5b ->
      (* [ opens an arbitrary value ([color:red]) or property, whose first
         character is neither a quote nor whitespace. [ followed by whitespace
         is a plain array bracket ([rows={[ ...]), not a candidate: consuming it
         as one would swallow every class named inside the array. *)
      i + 1 < Array.length d.chars
      &&
      let next = char_at d (i + 1) in
      next <> 0x22 && next <> 0x27 && next <> 0x60 && not (is_whitespace next)
  | _ -> false

let is_candidate_char = function
  | c when c >= 0x61 && c <= 0x7a -> true
  | c when c >= 0x41 && c <= 0x5a -> true
  | c when is_ascii_digit c -> true
  | 0x2d | 0x5f | 0x3a | 0x2f | 0x25 | 0x40 | 0x21 | 0x2a -> true
  | _ -> false

let trim_candidate s =
  let len = String.length s in
  let rec stop i =
    if i > 0 then match s.[i - 1] with '.' | ':' -> stop (i - 1) | _ -> i
    else i
  in
  let n = stop len in
  if n = len then s else String.sub s 0 n

let read_candidate d start =
  let len = Array.length d.chars in
  let rec loop i bracket_depth paren_depth quote escaped =
    if i >= len then i
    else
      let c = char_at d i in
      match quote with
      | Some q ->
          if escaped then loop (i + 1) bracket_depth paren_depth quote false
          else if c = 0x5c then
            loop (i + 1) bracket_depth paren_depth quote true
          else if c = q then loop (i + 1) bracket_depth paren_depth None false
          else loop (i + 1) bracket_depth paren_depth quote false
      | None when bracket_depth > 0 || paren_depth > 0 -> (
          match c with
          | 0x22 | 0x27 | 0x60 ->
              loop (i + 1) bracket_depth paren_depth (Some c) false
          | 0x5b -> loop (i + 1) (bracket_depth + 1) paren_depth None false
          | 0x5d when bracket_depth > 0 ->
              loop (i + 1) (bracket_depth - 1) paren_depth None false
          | 0x28 -> loop (i + 1) bracket_depth (paren_depth + 1) None false
          | 0x29 when paren_depth > 0 ->
              loop (i + 1) bracket_depth (paren_depth - 1) None false
          | _ -> loop (i + 1) bracket_depth paren_depth None false)
      | None -> (
          match c with
          | c when is_whitespace c -> i
          | 0x22 | 0x27 | 0x60 | 0x3c | 0x3e | 0x3d | 0x7b | 0x7d | 0x3b | 0x2c
          | 0x23 ->
              i
          | 0x5b -> loop (i + 1) 1 0 None false
          | 0x28 when i > start && char_at d (i - 1) = 0x2d ->
              loop (i + 1) 0 1 None false
          | 0x28 | 0x29 -> i
          | 0x2e
            when i > start
                 && i + 1 < len
                 && is_ascii_digit (char_at d (i - 1))
                 && is_ascii_digit (char_at d (i + 1)) ->
              loop (i + 1) 0 0 None false
          | 0x2e -> i
          | c when is_candidate_char c -> loop (i + 1) 0 0 None false
          | _ -> i)
  in
  loop start 0 0 None false

let candidates source =
  let d = decoded_utf_8 source in
  let len = Array.length d.chars in
  let rec loop i acc =
    if i >= len then List.rev acc
    else if is_candidate_start d i then
      let stop = read_candidate d i in
      if stop > i then
        let byte_start = byte_at d i in
        let byte_stop = byte_at d stop in
        let candidate =
          String.sub source byte_start (byte_stop - byte_start)
          |> trim_candidate
        in
        let acc = if candidate = "" then acc else candidate :: acc in
        loop stop acc
      else loop (i + 1) acc
    else loop (i + 1) acc
  in
  loop 0 [] |> List.sort_uniq String.compare

let read_file filename =
  let ic = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let candidates_from_file filename = filename |> read_file |> candidates
