(* [prefix_loop] is top-level so it captures nothing and allocates no closure -
   unlike [String.starts_with]'s inner [aux], which allocates one per call in a
   non-flambda build. Indices are kept in bounds by [has_prefix]. *)
let rec prefix_loop prefix s i lp =
  i = lp || (prefix.[i] = s.[i] && prefix_loop prefix s (i + 1) lp)

let has_prefix ~prefix s =
  let lp = String.length prefix in
  lp <= String.length s && prefix_loop prefix s 0 lp

let int_any s =
  match int_of_string_opt s with
  | Some n -> Ok n
  | None -> Error (`Msg ("Invalid number: " ^ s))

let nonnegative_int ~name s =
  match int_of_string_opt s with
  | Some n when n >= 0 -> Some (Ok n)
  | Some _ -> Some (Error (`Msg (name ^ " must be non-negative: " ^ s)))
  | None -> None

let int_pos ~name s =
  match nonnegative_int ~name s with
  | Some result -> result
  | None -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))

(* Parse decimal values like "0.5", "1.5" for spacing utilities. Valid decimals
   must be multiples of 0.25 (i.e., value * 4 is integer). *)
let decimal_pos ~name s =
  try
    let f = Float.of_string s in
    if f < 0.0 then Error (`Msg (name ^ " must be non-negative: " ^ s))
    else
      let scaled = f *. 4.0 in
      if Float.is_integer scaled then Ok f
      else Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))
  with Failure _ -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))

(* Parse spacing values - handles both integers and decimals *)
let spacing_value ~name s =
  match nonnegative_int ~name s with
  | Some (Ok n) -> Ok (float_of_int n)
  | Some (Error _ as error) -> error
  | None -> decimal_pos ~name s

let int_bounded ~name ~min ~max s =
  match int_of_string_opt s with
  | Some n when n >= min && n <= max -> Ok n
  | Some _ ->
      Error
        (`Msg
           ("" ^ name ^ " must be between " ^ string_of_int min ^ " and "
          ^ string_of_int max ^ ": " ^ s))
  | None -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))

(* Check if a value is a valid theme variable name. Theme variable names must
   not contain '/' — values with '/' that aren't handled as explicit fractions
   (like 3/4) are invalid class suffixes, not theme references. *)
let is_valid_theme_name s = s <> "" && not (String.contains s '/')
let ( >|= ) r f = Result.map f r

(** Extract the bare variable name from a "var(--name)" string. Returns "name"
    for "var(--name)", or the original string if not a var() reference. *)
let extract_var_name s =
  let len = String.length s in
  if len > 6 && String.sub s 0 6 = "var(--" && s.[len - 1] = ')' then
    String.trim (String.sub s 6 (len - 7))
  else s

(** Check if a string is a bracket-wrapped value like "[...]" *)
let is_bracket_value s =
  String.length s > 2 && s.[0] = '[' && s.[String.length s - 1] = ']'

(** Extract the inner content from a bracket value "[foo]" → "foo" *)
let bracket_inner s =
  if is_bracket_value s then String.sub s 1 (String.length s - 2) else s

let decode_underscores s = String.map (fun c -> if c = '_' then ' ' else c) s

let function_name_before s i =
  let is_name_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true
    | _ -> false
  in
  let j = ref (i - 1) in
  while !j >= 0 && s.[!j] = ' ' do
    decr j
  done;
  let stop = !j + 1 in
  while !j >= 0 && is_name_char s.[!j] do
    decr j
  done;
  if stop > !j + 1 then
    String.lowercase_ascii (String.sub s (!j + 1) (stop - !j - 1))
  else ""

let is_css_math_function = function
  | "abs" | "calc" | "calc-size" | "clamp" | "hypot" | "max" | "min" | "mod"
  | "rem" | "round" | "sign" ->
      true
  | _ -> false

let is_css_non_math_function = function
  | "attr" | "env" | "url" | "var" -> true
  | _ -> false

let normalize_css_math_operators s =
  let len = String.length s in
  let buf = Buffer.create (len + 8) in
  let contexts = ref [] in
  let current_math () =
    match !contexts with math :: _ -> math | [] -> false
  in
  let rec prev_non_space i =
    if i < 0 then None
    else if s.[i] = ' ' then prev_non_space (i - 1)
    else Some s.[i]
  in
  let rec next_non_space i =
    if i >= len then None
    else if s.[i] = ' ' then next_non_space (i + 1)
    else Some s.[i]
  in
  let value_end = function
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '%' | ')' | ']' -> true
    | _ -> false
  in
  let value_start = function
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '.' | '(' -> true
    | _ -> false
  in
  for i = 0 to len - 1 do
    match s.[i] with
    | '(' ->
        let fn = function_name_before s i in
        let ctx =
          if is_css_math_function fn then true
          else if is_css_non_math_function fn then false
          else current_math ()
        in
        contexts := ctx :: !contexts;
        Buffer.add_char buf '('
    | ')' ->
        (match !contexts with _ :: rest -> contexts := rest | [] -> ());
        Buffer.add_char buf ')'
    | ('+' | '-') as op
      when current_math ()
           && (match prev_non_space (i - 1) with
             | Some c -> value_end c
             | None -> false)
           &&
           match next_non_space (i + 1) with
           | Some c -> value_start c
           | None -> false ->
        let last =
          let n = Buffer.length buf in
          if n = 0 then None else Some (Buffer.nth buf (n - 1))
        in
        (match last with
        | Some ' ' | None -> ()
        | Some _ -> Buffer.add_char buf ' ');
        Buffer.add_char buf op;
        Buffer.add_char buf ' '
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let decode_arbitrary_value s =
  s |> decode_underscores |> normalize_css_math_operators

(** Check if a string starts with "var(" — works on inner bracket content *)
let is_var s = String.length s > 4 && String.sub s 0 4 = "var("

(** Check if a bracket value contains a var() reference *)
let is_bracket_var s =
  if is_bracket_value s then is_var (bracket_inner s) else false

(** Check if a string looks like a CSS color function call (e.g., "rgba(...)",
    "hsl(...)", "oklch(...)"). Returns true for known CSS color function names
    followed by '('. *)
let is_css_color_fn s =
  let starts prefix =
    String.length s >= String.length prefix + 1
    && String.sub s 0 (String.length prefix) = prefix
    && s.[String.length prefix] = '('
  in
  starts "rgb" || starts "rgba" || starts "hsl" || starts "hsla" || starts "hwb"
  || starts "oklch" || starts "oklab" || starts "lch" || starts "lab"
  || starts "color" || starts "color-mix"

(** Check if a string is a bare var reference like "(--name)" *)
let is_bare_var s =
  String.length s > 4
  && s.[0] = '('
  && s.[String.length s - 1] = ')'
  && String.length s > 3
  && s.[1] = '-'
  && s.[2] = '-'

(** Extract the var name from a bare var "(--name)" → "--name" *)
let bare_var_inner s =
  if is_bare_var s then String.sub s 1 (String.length s - 2) else s

(** Split a class name on '-' but treat '[...]' as atomic. E.g.
    "m-[var(--value)]" → ["m"; "[var(--value)]"] E.g. "-m-[var(--value)]" →
    [""; "m"; "[var(--value)]"] *)
let split_class class_name =
  let len = String.length class_name in
  let buf = Buffer.create 16 in
  let parts = ref [] in
  let i = ref 0 in
  while !i < len do
    let c = class_name.[!i] in
    if c = '[' then (
      (* Read until matching ']', including nested brackets *)
      let depth = ref 1 in
      Buffer.add_char buf c;
      incr i;
      while !i < len && !depth > 0 do
        let c = class_name.[!i] in
        Buffer.add_char buf c;
        if c = '[' then incr depth else if c = ']' then decr depth;
        incr i
      done)
    else if c = '(' then (
      (* Read until matching ')', including nested parens *)
      let depth = ref 1 in
      Buffer.add_char buf c;
      incr i;
      while !i < len && !depth > 0 do
        let c = class_name.[!i] in
        Buffer.add_char buf c;
        if c = '(' then incr depth else if c = ')' then decr depth;
        incr i
      done)
    else if c = '-' then (
      parts := Buffer.contents buf :: !parts;
      Buffer.clear buf;
      incr i)
    else (
      Buffer.add_char buf c;
      incr i)
  done;
  parts := Buffer.contents buf :: !parts;
  List.rev !parts
