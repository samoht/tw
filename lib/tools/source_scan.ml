let is_whitespace = function
  | 0x20 | 0x09 | 0x0a | 0x0d | 0x0c -> true
  | _ -> false

let split_whitespace = Tw.split_whitespace

type decoded = { starts : int array; chars : int array }

(* Grow the two arrays directly. Folding the file into lists first costs about
   twelve words per character and keeps both lists live until the arrays are
   built, and a source file is kilobytes. A byte can decode to at most one
   character, so the source length is a safe initial capacity. *)
let decoded_utf_8 source =
  let cap = max 16 (String.length source) in
  let starts = ref (Array.make (cap + 1) 0) in
  let chars = ref (Array.make cap 0) in
  let n = ref 0 in
  let push start code =
    if !n >= Array.length !chars then begin
      let bigger = Array.make (2 * Array.length !chars) 0 in
      Array.blit !chars 0 bigger 0 !n;
      chars := bigger;
      let bigger_starts = Array.make ((2 * Array.length !chars) + 1) 0 in
      Array.blit !starts 0 bigger_starts 0 !n;
      starts := bigger_starts
    end;
    !starts.(!n) <- start;
    !chars.(!n) <- code;
    incr n
  in
  Uutf.String.fold_utf_8
    (fun () byte_pos decoded ->
      match decoded with
      | `Uchar u -> push byte_pos (Uchar.to_int u)
      | `Malformed _ -> push byte_pos (-1))
    () source;
  (* [byte_at d stop] is read at [stop = length], so [starts] carries one more
     entry than [chars]. *)
  !starts.(!n) <- String.length source;
  { starts = Array.sub !starts 0 (!n + 1); chars = Array.sub !chars 0 !n }

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

(* A [(] opens a group only where a utility can hold one: after the [-] of
   [bg-(--x)] or the [/] of an alpha shorthand. Elsewhere it ends the token, so
   a call in the source is not read as a class. *)
let opens_paren_group prev = prev = 0x2d || prev = 0x2f

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
      (* A candidate never spans a line, whatever is open. Without this an
         unbalanced [[] or quote swallows the rest of the file into one
         token. *)
      if c = 0x0a || c = 0x0d then i
      else
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
            | 0x22 | 0x27 | 0x60 | 0x3c | 0x3e | 0x3d | 0x7b | 0x7d | 0x3b
            | 0x2c | 0x23 ->
                i
            | 0x5b -> loop (i + 1) 1 0 None false
            | 0x28 when i > start && opens_paren_group (char_at d (i - 1)) ->
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

(* Tailwind's extractor refuses a candidate whose [/modifier] opens on [-] or
   [_], and refuses the whole candidate rather than truncating it: the CLI emits
   nothing at all for [bg-red-500/-2]. Only a [/] the utility itself carries
   counts, so one inside a bracket or a paren group - [aspect-[16/9]],
   [bg-[url(a/_b)]] - is the value's, not a modifier's. *)
let modifier_opens_badly candidate =
  let len = String.length candidate in
  let rec loop i bracket paren =
    if i >= len then false
    else
      match candidate.[i] with
      | '[' -> loop (i + 1) (bracket + 1) paren
      | ']' when bracket > 0 -> loop (i + 1) (bracket - 1) paren
      | '(' -> loop (i + 1) bracket (paren + 1)
      | ')' when paren > 0 -> loop (i + 1) bracket (paren - 1)
      | '/' when bracket = 0 && paren = 0 ->
          (i + 1 < len && (candidate.[i + 1] = '-' || candidate.[i + 1] = '_'))
          || loop (i + 1) bracket paren
      | _ -> loop (i + 1) bracket paren
  in
  loop 0 0 0

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
        let acc =
          if candidate = "" || modifier_opens_badly candidate then acc
          else candidate :: acc
        in
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

let is_glob path =
  String.exists (function '*' | '?' | '{' -> true | _ -> false) path

let glob_root pattern =
  let rec split root = function
    | segment :: rest when not (is_glob segment) -> split (segment :: root) rest
    | rest -> (List.rev root, rest)
  in
  let root, rest = split [] (String.split_on_char '/' pattern) in
  let root =
    match root with [] -> "." | [ "" ] -> "/" | _ -> String.concat "/" root
  in
  (root, String.concat "/" rest)

(* [{a,b}] alternatives are expanded before anything is matched. *)
let rec glob_alternatives pattern =
  match String.index_opt pattern '{' with
  | None -> [ pattern ]
  | Some opening -> (
      match String.index_from_opt pattern opening '}' with
      | None -> [ pattern ]
      | Some closing ->
          let prefix = String.sub pattern 0 opening in
          let body = String.sub pattern (opening + 1) (closing - opening - 1) in
          let suffix =
            String.sub pattern (closing + 1)
              (String.length pattern - closing - 1)
          in
          String.split_on_char ',' body
          |> List.concat_map (fun alternative ->
              glob_alternatives (prefix ^ alternative ^ suffix)))

(* One path segment against one pattern segment: [*] is any run of characters,
   [?] any one character. *)
let segment_matches pattern segment =
  let p = String.length pattern and s = String.length segment in
  let rec go i j =
    if i = p then j = s
    else
      match pattern.[i] with
      | '*' -> go (i + 1) j || (j < s && go i (j + 1))
      | '?' -> j < s && go (i + 1) (j + 1)
      | c -> j < s && Char.equal c segment.[j] && go (i + 1) (j + 1)
  in
  go 0 0

let glob_matches ~pattern path =
  let segments s =
    String.split_on_char '/' s
    |> List.filter (fun segment -> segment <> "" && segment <> ".")
  in
  let rec go patterns path =
    match (patterns, path) with
    | [], [] -> true
    | "**" :: rest, _ -> (
        go rest path
        || match path with [] -> false | _ :: tl -> go patterns tl)
    | p :: rest, segment :: tl -> segment_matches p segment && go rest tl
    | _ -> false
  in
  List.exists
    (fun alternative -> go (segments alternative) (segments path))
    (glob_alternatives pattern)
