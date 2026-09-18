let is_digits s =
  s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s

let without_sign s =
  if s <> "" && s.[0] = '-' then String.sub s 1 (String.length s - 1) else s

let is_canonical_digits s = is_digits s && (String.length s = 1 || s.[0] <> '0')

(* A class suffix is written in plain decimal, but OCaml's literal grammar also
   admits [0x]/[0o]/[0b] bases, [_] digit separators, hex-float exponents and a
   leading [+]. Handing a suffix straight to [int_of_string_opt] therefore reads
   [p-0x4] as 4 and emits [.p-4] — a rule nobody wrote, and not a class Tailwind
   accepts. Both readers below check the spelling first. *)
let decimal_int s =
  if is_canonical_digits (without_sign s) then int_of_string_opt s else None

(* Plain decimal: digits, then at most one fractional part with digits of its
   own. [.5], [1.] and [1e2] are not spellings of a class suffix. *)
let decimal_float s =
  let digits = without_sign s in
  let plain =
    match String.index_opt digits '.' with
    | None -> is_canonical_digits digits
    | Some i ->
        let whole = String.sub digits 0 i in
        let fraction =
          String.sub digits (i + 1) (String.length digits - i - 1)
        in
        is_canonical_digits whole && is_digits fraction
        && fraction.[String.length fraction - 1] <> '0'
  in
  if plain then float_of_string_opt s else None

(* A fraction suffix is two plain decimals around one [/]: [w-1/2], [top-3/8],
   [basis-13/17]. Neither side carries a sign, neither takes a redundant leading
   zero, and the denominator is drawn from no fixed list — Tailwind divides
   whatever it is given. *)
let fraction s =
  match String.split_on_char '/' s with
  | [ n; m ] when is_canonical_digits n && is_canonical_digits m -> (
      match (int_of_string_opt n, int_of_string_opt m) with
      | Some n, Some m -> Some (n, m)
      | Some _, None | None, _ -> None)
  | _ -> None

(* [n/m] as the percentage Tailwind's [calc(n / m * 100%)] resolves to, folded
   to the six significant figures its own printer keeps (33.3333, 8.33333). A
   zero denominator has no percentage: Tailwind writes the division out for the
   browser to fail on, so a family that folds it here has nothing to write. *)
let fraction_percent n m =
  if m = 0 then None
  else if n = 0 then Some 0.
  else
    let pct = float_of_int n /. float_of_int m *. 100. in
    let digits = 6. -. Float.ceil (Float.log10 pct) in
    let factor = 10. ** digits in
    Some (Float.round (pct *. factor) /. factor)

let fraction_pct s =
  match fraction s with Some (n, m) -> fraction_percent n m | None -> None

(* [n / m * 100%], the expression Tailwind writes a fraction as. The browser
   resolves the division exactly, where a percentage folded from it cannot hold
   a non-terminating fraction: 33.3333% of 321px is 106.984px, a third is 107px.
   The [100%] leaf is the caller's, since each property has its own calc
   type. *)
let fraction_calc hundred n m =
  Cascade.Css.Calc.(
    mul (div (float (float_of_int n)) (float (float_of_int m))) hundred)

let fraction_length n m : Cascade.Css.length =
  Cascade.Css.Calc
    (fraction_calc (Cascade.Css.Calc.length (Cascade.Css.Pct 100.)) n m)

let neg_fraction_length n m : Cascade.Css.length =
  Cascade.Css.Calc
    Cascade.Css.Calc.(mul (length (fraction_length n m)) (float (-1.)))

let int_any s =
  match decimal_int s with
  | Some n -> Ok n
  | None -> Error (`Msg ("Invalid number: " ^ s))

let nonnegative_int ~name s =
  match decimal_int s with
  | Some n when n >= 0 -> Some (Ok n)
  | Some _ -> Some (Error (`Msg (name ^ " must be non-negative: " ^ s)))
  | None -> None

let int_pos ~name s =
  Option.value (nonnegative_int ~name s)
    ~default:(Error (`Msg ("Invalid " ^ name ^ " value: " ^ s)))

(* Parse decimal values like "0.5", "1.5" for spacing utilities. Valid decimals
   must be multiples of 0.25 (i.e., value * 4 is integer). *)
let decimal_pos ~name s =
  match decimal_float s with
  | None -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))
  | Some f ->
      if f < 0.0 then Error (`Msg (name ^ " must be non-negative: " ^ s))
      else if Float.is_integer (f *. 4.0) then Ok f
      else Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))

(* Parse spacing values - handles both integers and decimals *)
let spacing_value ~name s =
  match nonnegative_int ~name s with
  | Some (Ok n) -> Ok (float_of_int n)
  | Some (Error _ as error) -> error
  | None -> decimal_pos ~name s

let int_bounded ~name ~min ~max s =
  match decimal_int s with
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

let extract_var_name s =
  if not (String.starts_with ~prefix:"var(" s) then s
  else
    try
      let cursor = Cascade.Cursor.of_string s in
      match Cascade.Css.Variables.read_reference cursor with
      | name, None -> name
      | name, Some fallback -> name ^ ", " ^ fallback
    with Cascade.Cursor.Parse_error _ | Invalid_argument _ -> s

(* A quoted string runs to its closing quote, and to the end of the input when
   the value leaves it open, as the CSS Syntax 3 sec. 4.3 tokeniser leaves it.
   The result is the index just past the string. *)
let string_end s i quote =
  let len = String.length s in
  let rec go i =
    if i >= len then len
    else if s.[i] = '\\' then go (i + 2)
    else if s.[i] = quote then i + 1
    else go (i + 1)
  in
  go i

(* A []] the value quotes or escapes is part of the value, not the bracket's
   end, so the scan reads strings and the [\] escape the way the tokeniser does:
   [[background-image:url('a]b')]] is one bracket whose text carries a []]. A
   string left open swallows every later []], which is what Tailwind does with
   [bg-[url('a)]]. *)
let bracket_close s =
  let len = String.length s in
  let rec close i depth =
    if i >= len then None
    else
      match s.[i] with
      | '\\' -> close (i + 2) depth
      | '\'' | '"' -> close (string_end s (i + 1) s.[i]) depth
      | '[' -> close (i + 1) (depth + 1)
      | ']' -> if depth = 0 then Some i else close (i + 1) (depth - 1)
      | _ -> close (i + 1) depth
  in
  if len > 2 && s.[0] = '[' then close 1 0 else None

(* One bracket, not two: the closing bracket has to be the last character. A
   suffix carrying a second bracket - one bracket with a bracket modifier, or
   two brackets in a row - would otherwise read as one bracket whose inner text
   has a stray bracket in it, which no declaration value takes. *)
let is_bracket_value s = bracket_close s = Some (String.length s - 1)

(** Extract the inner content from a bracket value "[foo]" → "foo" *)
let bracket_inner s =
  if is_bracket_value s then String.sub s 1 (String.length s - 2) else s

(* How a span the decoder does not read as ordinary text is copied out.
   [Verbatim] keeps every character, escape included; [Literal_underscore] reads
   the text but leaves a bare [_] standing for itself. *)
type exempt_span = Verbatim | Literal_underscore

(* In an arbitrary value [_] stands for a space, and [\_] for a literal
   underscore — otherwise a value that needs one could not be written. The two
   readings differ only in what a bare [_] stands for, which [plain] carries,
   and in [exempt], the spans read under one of the rules above. *)
let read_underscores ~plain ~exempt s =
  let len = String.length s in
  let buf = Buffer.create len in
  (* One character of text, [under] standing for a bare [_]. *)
  let text under i =
    if s.[i] = '\\' && i + 1 < len && s.[i + 1] = '_' then begin
      Buffer.add_char buf '_';
      i + 2
    end
    else begin
      Buffer.add_char buf (if s.[i] = '_' then under else s.[i]);
      i + 1
    end
  in
  let rec go i spans =
    if i >= len then ()
    else
      match spans with
      | (start, stop, Verbatim) :: rest when i = start ->
          Buffer.add_substring buf s start (stop - start);
          go stop rest
      | (start, stop, Literal_underscore) :: rest when i = start ->
          let rec span j = if j >= stop then j else span (text '_' j) in
          go (span start) rest
      | _ -> go (text plain i) spans
  in
  go 0 exempt;
  Buffer.contents buf

(* Tailwind's value parser: a word runs to the next of [/ : , = > < ( )] or
   whitespace, and a [\] escape or a quoted string is part of the word it stands
   in. A [(] opens a function whose name is the word before it. *)
let breaks_word = function
  | '/' | ':' | ',' | '=' | '>' | '<' | '(' | ')' | ' ' | '\t' | '\n' -> true
  | _ -> false

(* The [)] closing the argument list opened before [i] in [s], or the end of [s]
   when it is left open, as the tokeniser leaves it. *)
let rec paren_end s i depth =
  let len = String.length s in
  if i >= len then len
  else
    match s.[i] with
    | '\\' -> paren_end s (i + 2) depth
    | '\'' | '"' -> paren_end s (string_end s (i + 1) s.[i]) depth
    | '(' -> paren_end s (i + 1) (depth + 1)
    | ')' -> if depth = 0 then i else paren_end s (i + 1) (depth - 1)
    | _ -> paren_end s (i + 1) depth

(* Where the word starting at [i] in [s] ends, which is where Tailwind's value
   parser breaks one. A [\] escape and a quoted string are part of the word. *)
let rec word_end s i =
  let len = String.length s in
  if i >= len then len
  else
    match s.[i] with
    | '\\' -> word_end s (i + 2)
    | '\'' | '"' -> word_end s (string_end s (i + 1) s.[i])
    | c when breaks_word c -> i
    | _ -> word_end s (i + 1)

(* Whether the function name before the [(] at [i] in [s], a name that started
   at [word], is [key] or ends in [_key]. *)
let names s i word key =
  let n = String.length key in
  i - word >= n
  && String.sub s (i - n) n = key
  && (i - word = n || s.[i - n - 1] = '_')

(* [exempt_spans s] is the list, in order, of the spans of [s] that the decoder
   does not read as ordinary text, each as a half-open range.

   Tailwind decodes an arbitrary value node by node, and two kinds of function
   node keep text the decoding would otherwise change. A [url()] keeps its whole
   argument list as written, escape included: a [_] there is part of a file
   name. A [var()] or [theme()] keeps the bare [_] of its first argument, which
   names a custom property, while the [\_] escape there still unescapes.

   The names are matched the way Tailwind matches them, on the word itself or on
   a name ending in [_url], [_var] or [_theme], because a class writes a space
   as [_] and the parser reads the whole run before the [(] as one name. That is
   what carries [shadow-[0_0_0_var(--my_var)]], whose function node is named
   [0_0_0_var]: the name decodes to [0 0 0 var] and the property it references
   stays whole. So [myurl(a_b)] and [a-url(a_b)] decode their arguments, and
   [--my-var(a_b)] is not a [var()].

   Only a first argument that is a word takes the rule. One opening a call of
   its own is a function node, which Tailwind recurses into instead: the
   arguments of [var(foo(--a_b))] decode. *)
let exempt_spans s =
  let len = String.length s in
  (* The first argument of the call opening at [i], when it is a word: none when
     the list opens on a separator or on the [(] of a nested call. *)
  let first_word i =
    let stop = word_end s (i + 1) in
    if stop > i + 1 && not (stop < len && s.[stop] = '(') then
      Some (i + 1, stop, Literal_underscore)
    else None
  in
  let rec scan i word acc =
    if i >= len then List.rev acc
    else
      match s.[i] with
      | '\\' -> scan (i + 2) word acc
      | '\'' | '"' -> scan (string_end s (i + 1) s.[i]) word acc
      | '(' when names s i word "url" ->
          let stop = paren_end s (i + 1) 0 in
          scan (stop + 1) (stop + 1) ((i + 1, stop, Verbatim) :: acc)
      | '(' when names s i word "var" || names s i word "theme" ->
          let acc =
            match first_word i with Some span -> span :: acc | None -> acc
          in
          scan (i + 1) (i + 1) acc
      | c -> scan (i + 1) (if breaks_word c then i + 1 else word) acc
  in
  scan 0 0 []

let decode_underscores s =
  read_underscores ~plain:' ' ~exempt:(exempt_spans s) s

(* A [url()] in an arbitrary value is CSS source, so an escape in it stands for
   one character of the URL and the quotes are the tokeniser's, not the file
   name's: [url(a\]b)], [url(a]b)] and [url('a]b')] all name [a]b]. Reading the
   token rather than slicing the text out of it keeps the backslash from
   reaching the value as a character of its own. *)
let url_token s =
  try
    let cursor = Cascade.Cursor.of_string s in
    let url = Cascade.Cursor.url cursor in
    if Cascade.Cursor.is_done cursor then Some url else None
  with Cascade.Cursor.Parse_error _ | Invalid_argument _ -> None

(* A property name has no spaces to spell, so its underscores stand for
   themselves and only the escape is undone. A name holds no [url()] either, so
   nothing is copied out unread. *)
let unescape_underscores s = read_underscores ~plain:'_' ~exempt:[] s

(* The inverse: a utility holding a decoded value writes its class name back,
   and the name has to read as the value it came from. *)
let encode_underscores s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | ' ' -> Buffer.add_char buf '_'
      | '_' -> Buffer.add_string buf {|\_|}
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

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

(* A substitution function ([var()], [attr()], [env()]) stands for a token
   stream, not a value CSS Values 4 sec. 10 math grammar parses, so entering one
   inside a math function must not inherit the surrounding operator context: a
   dash inside the name it carries (["--spacing-6"]) is not a minus sign.
   cascade exports no name table for this - [Properties.is_color_function]
   answers a different question (which functions are colours) and using it here
   instead corrupts [p-[calc(var(--spacing-6)-1px)]] into [calc(var(--spacing -
   6) - 1px)]. Kept hand-written on purpose. *)
let is_css_non_math_function = function
  | "attr" | "env" | "url" | "var" -> true
  | _ -> false

(* A math function stands for the value it computes, so a bracket opening with
   one denotes whatever the utility's numeric side takes. Utilities that tell a
   width from a colour by the bracket's first character need to know. *)
let starts_with_math_function s =
  match String.index_opt s '(' with
  | Some i ->
      Cascade.Css.Properties.is_math_function
        (String.lowercase_ascii (String.sub s 0 i))
  | None -> false

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
          if Cascade.Css.Properties.is_math_function fn then true
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

(* Tailwind's [--spacing(N)] shorthand: the spacing scale as a function. It is
   not CSS, so a value holding it fails to parse and the utility drops out. The
   scan skips over quoted CSS strings ['...'] and ["..."] verbatim (with their
   backslash escapes), so the same bytes inside a string literal - e.g. an
   arbitrary [content:'--spacing(1)'] - are left as literal text rather than
   expanded. *)
(* Copies a quoted string from [i], the character after its opening [quote],
   through its closing quote into [buf], backslash escapes included, and
   answers the index after it. *)
let copy_string s buf quote i =
  let len = String.length s in
  let rec at i =
    if i >= len then i
    else
      match s.[i] with
      | '\\' when i + 1 < len ->
          Buffer.add_char buf s.[i];
          Buffer.add_char buf s.[i + 1];
          at (i + 2)
      | c ->
          Buffer.add_char buf c;
          if c = quote then i + 1 else at (i + 1)
  in
  at i

let expand_spacing_fn s =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec close_paren i depth =
    if i >= len then (len, len)
    else
      match s.[i] with
      | '(' -> close_paren (i + 1) (depth + 1)
      | ')' when depth = 1 -> (i, i + 1)
      | ')' -> close_paren (i + 1) (depth - 1)
      | _ -> close_paren (i + 1) depth
  in
  let rec go i =
    if i >= len then ()
    else
      match s.[i] with
      | ('\'' | '"') as quote ->
          Buffer.add_char buf quote;
          go (copy_string s buf quote (i + 1))
      | _ ->
          if i + 10 <= len && String.sub s i 10 = "--spacing(" then (
            let stop, next = close_paren (i + 9) 0 in
            let n = String.sub s (i + 10) (stop - i - 10) in
            (* [--spacing(1)] is the scale itself; only a multiplier needs
               calc. *)
            if String.trim n = "1" then Buffer.add_string buf "var(--spacing)"
            else
              Buffer.add_string buf
                (String.concat "" [ "calc(var(--spacing) * "; n; ")" ]);
            go next)
          else (
            Buffer.add_char buf s.[i];
            go (i + 1))
  in
  go 0;
  Buffer.contents buf

let call_body name s =
  let head = name ^ "(" in
  let n = String.length s and m = String.length head in
  if n > m && String.starts_with ~prefix:head s && s.[n - 1] = ')' then
    Some (String.sub s m (n - m - 1))
  else None

let split_top_level sep s =
  let len = String.length s in
  let rec go depth start i acc =
    if i >= len then List.rev (String.sub s start (len - start) :: acc)
    else
      match s.[i] with
      | '(' | '[' | '{' -> go (depth + 1) start (i + 1) acc
      | ')' | ']' | '}' -> go (max 0 (depth - 1)) start (i + 1) acc
      | c when c = sep && depth = 0 ->
          go depth (i + 1) (i + 1) (String.sub s start (i - start) :: acc)
      | _ -> go depth start (i + 1) acc
  in
  go 0 0 0 []

(* A bare number's alpha as the percentage Tailwind scales it to, done on the
   text so that [0.2] is [20] rather than a float's idea of it: the decimal
   point moves two places right. [None] when [s] is not a plain decimal. *)
let alpha_percent_of_number s =
  let sign, digits =
    if String.starts_with ~prefix:"-" s || String.starts_with ~prefix:"+" s then
      (String.sub s 0 1, String.sub s 1 (String.length s - 1))
    else ("", s)
  in
  let all_digits = String.for_all (fun c -> c >= '0' && c <= '9') in
  let int_part, frac =
    match String.split_on_char '.' digits with
    | [ i ] -> (i, "")
    | [ i; f ] -> (i, f)
    | _ -> ("", "x")
  in
  if (int_part = "" && frac = "") || not (all_digits int_part && all_digits frac)
  then None
  else
    let frac = frac ^ "00" in
    let whole = int_part ^ String.sub frac 0 2 in
    let rest = String.sub frac 2 (String.length frac - 2) in
    let rec drop_zeros s =
      if String.ends_with ~suffix:"0" s then
        drop_zeros (String.sub s 0 (String.length s - 1))
      else s
    in
    let rec lead s =
      if String.length s > 1 && s.[0] = '0' then
        lead (String.sub s 1 (String.length s - 1))
      else s
    in
    let rest = drop_zeros rest in
    Some (sign ^ lead whole ^ if rest = "" then "" else "." ^ rest)

(* The two halves of an [--alpha()] body, trimmed, with the alpha scaled the way
   Tailwind scales a bare number. [None] when either half is empty, which
   Tailwind refuses. *)
let alpha_halves body =
  match List.map String.trim (split_top_level '/' body) with
  | colour :: alpha :: _ when colour <> "" && alpha <> "" ->
      let alpha =
        match alpha_percent_of_number alpha with
        | Some percent -> percent ^ "%"
        | None -> alpha
      in
      Some (colour, alpha)
  | _ -> None

let alpha_call s = Option.bind (call_body "--alpha" s) alpha_halves

(* Tailwind's [--alpha(<color>/<alpha>)] wherever it stands in a value, as
   [--spacing()] above: the [color-mix()] it denotes takes its place, a whole
   alpha leaves the colour itself, and a call missing either half stays as
   written for [holds_unresolved_call] to refuse. A call inside another's colour
   expands first. The scan skips quoted strings the way [expand_spacing_fn]
   does. *)
let rec alpha_substitute body =
  match alpha_halves (expand_alpha_fn body) with
  | Some (colour, "100%") -> Some colour
  | Some (colour, alpha) ->
      Some
        (String.concat ""
           [ "color-mix(in oklab, "; colour; " "; alpha; ", transparent)" ])
  | None -> None

and expand_alpha_fn s =
  let len = String.length s in
  let buf = Buffer.create len in
  let head = "--alpha(" in
  let h = String.length head in
  let rec close_paren i depth =
    if i >= len then None
    else
      match s.[i] with
      | '(' -> close_paren (i + 1) (depth + 1)
      | ')' when depth = 1 -> Some i
      | ')' -> close_paren (i + 1) (depth - 1)
      | _ -> close_paren (i + 1) depth
  in
  let call_at i =
    if i + h <= len && String.sub s i h = head then close_paren (i + h - 1) 0
    else None
  in
  let rec go i =
    if i >= len then ()
    else
      match s.[i] with
      | ('\'' | '"') as quote ->
          Buffer.add_char buf quote;
          go (copy_string s buf quote (i + 1))
      | _ -> (
          match call_at i with
          | Some stop ->
              let call = String.sub s i (stop + 1 - i) in
              let body = String.sub s (i + h) (stop - i - h) in
              Buffer.add_string buf
                (Option.value (alpha_substitute body) ~default:call);
              go (stop + 1)
          | None ->
              Buffer.add_char buf s.[i];
              go (i + 1))
  in
  go 0;
  Buffer.contents buf

let decode_arbitrary_value s =
  s |> decode_underscores |> expand_spacing_fn |> expand_alpha_fn
  |> normalize_css_math_operators

let arbitrary_length s = Cascade.Css.parse_length (decode_arbitrary_value s)

(* [<length-percentage>] is the length grammar minus the keywords the length
   reader also admits ([auto], [none], [max-content], ...) and minus a unitless
   number, which only reads as a length because [0] is one. The match is
   exhaustive so a length constructor cascade adds later has to be classified
   here rather than passing silently. *)
let length_percentage_of_length (l : Cascade.Css.length) :
    Cascade.Css.length_percentage option =
  match l with
  | Pct p -> Some (Pct p)
  | Px _ | Cm _ | Mm _ | Q _ | In _ | Pt _ | Pc _ | Rem _ | Em _ | Ex _ | Cap _
  | Ic _ | Ric _ | Rlh _ | Vw _ | Vh _ | Vmin _ | Vmax _ | Vi _ | Vb _ | Dvh _
  | Dvw _ | Dvmin _ | Dvmax _ | Lvh _ | Lvw _ | Lvmin _ | Lvmax _ | Svh _
  | Svw _ | Svmin _ | Svmax _ | Cqw _ | Cqh _ | Cqi _ | Cqb _ | Cqmin _
  | Cqmax _ | Ch _ | Lh _ | Dimension _ ->
      Some (Length l)
  (* Math functions and references resolve to a length at used-value time. *)
  | Clamp _ | Min _ | Max _ | Round _ | Mod _ | Rem_fn _ | Hypot _ | Abs _
  | Env _ | Var _ | Calc _ ->
      Some (Length l)
  | Zero -> None
  (* Keywords, and the functions that stand for an intrinsic size or an anchor
     position rather than for a length. *)
  | Size | Auto | None | Normal | Inherit | Initial | Unset | Revert
  | Revert_layer | Fit_content | Fit_content_arg _ | Content | Contain
  | Max_content | Min_content | Webkit_max_content | Webkit_min_content
  | Webkit_fit_content | Moz_max_content | Moz_min_content | Moz_fit_content
  | From_font | Hairline | Thin | Medium | Thick | Stretch | Minmax _
  | Calc_size _ | Anchor_size _ | Anchor _ | Attr _ ->
      None

let arbitrary_length_percentage s =
  Option.bind (arbitrary_length s) length_percentage_of_length

(* A CSS identifier, which is what a custom-ident or a property name written in
   an arbitrary value has to be. The docs pages carry [<value>] placeholders
   that are not CSS, and passing one through emits an invalid declaration. *)
let is_ident = Cascade.Syntax.is_ident

(* An arbitrary value reaching a custom property is author text, so it can carry
   a top-level [;] or [}] that ends the declaration early, or an unterminated
   function, block or string that swallows the rest of the rule. cascade refuses
   such a pair from [custom_property] by raising, which is right for a caller
   holding CSS it wrote; here the text comes from a class name, so the class is
   the thing to refuse. Tailwind refuses the same ones. *)
let is_declaration_value = Cascade.Css.Declaration.is_declaration_value

let data_type_hint inner =
  let len = String.length inner in
  let rec scan i =
    if i >= len then None
    else
      match inner.[i] with
      | ':' ->
          Some (String.sub inner 0 i, String.sub inner (i + 1) (len - i - 1))
      | 'a' .. 'z' | '-' -> scan (i + 1)
      | _ -> None
  in
  scan 0

let value_after_hint s =
  match data_type_hint s with
  (* A bracket opening with [:] has an empty hint, which names no longhand and
     no utility either. *)
  | Some ("", _) -> None
  | Some (_, value) -> Some value
  | None -> Some s

let declaration_value_of s =
  let value = decode_arbitrary_value s in
  if String.trim value <> "" && is_declaration_value value then Some value
  else None

(* Every family reaches its last resort with the bracket as the author wrote it,
   so refusing an empty hint here refuses it everywhere. *)
(* A [theme()], [--theme()] or [--alpha()] call is resolved before a family's
   reader sees the bracket, so one still standing in the text is a lookup that
   declined. Tailwind emits nothing for such a class, so it is not a token
   stream to forward: [p-[--theme(spacing.4)]] is the v4 spelling over a v3
   dot path, which resolves to nothing, and [text-[--alpha(red)]] is a call
   missing its alpha, which Tailwind refuses to compile. *)
let holds_unresolved_call s =
  (* Tokenise rather than search for text: a [<function-token>] is an ident
     immediately followed by [(], so the name is matched whole and a call
     spelled inside a string or a comment is not a call at all. *)
  let lexer = Cascade.Lexer.of_string s in
  let rec scan () =
    let token = Cascade.Lexer.next lexer in
    match token.Cascade.Token.kind with
    | Eof -> false
    | Function ("theme" | "--theme" | "--alpha") -> true
    | _ -> scan ()
  in
  scan ()

let arbitrary_declaration_value s =
  match Option.bind (value_after_hint s) declaration_value_of with
  | Some value when holds_unresolved_call value -> None
  | answer -> answer

let wrap_declaration_value ~before ~after value =
  if value = "" || not (is_declaration_value value) then None
  else
    let wrapped = before ^ value ^ after in
    if is_declaration_value wrapped then Some wrapped
    else
      (* A CSS comment is implicitly closed at EOF. Once the author value is
         embedded, however, an unterminated comment would consume the wrapper's
         closing tokens. Close that comment explicitly; the tokeniser drops the
         comment itself, as it does for the standalone value. *)
      let closed_comment = before ^ value ^ "*/" ^ after in
      if is_declaration_value closed_comment then Some closed_comment else None

let opaque_declaration property value =
  if value <> "" && is_declaration_value value then
    Cascade.Css.Declaration.parse_opaque_declaration property value
  else None

(** Check if a string starts with "var(" — works on inner bracket content *)
let is_var s = String.starts_with ~prefix:"var(" s && String.length s > 4

(** Check if a bracket value contains a var() reference *)
let is_bracket_var s =
  if is_bracket_value s then is_var (bracket_inner s) else false

(** Check if a string looks like a CSS color function call (e.g., "rgba(...)",
    "hsl(...)", "oklch(...)"). Returns true for known CSS color function names
    followed by '('. *)
let is_css_color_fn s =
  match String.index_opt s '(' with
  | Some i -> Cascade.Css.Properties.is_color_function (String.sub s 0 i)
  | None -> false

(** Check if a string is a bare var reference like "(--name)" *)
let is_bare_var s =
  String.length s > 4
  && s.[0] = '('
  && s.[String.length s - 1] = ')'
  && String.length s > 3
  && s.[1] = '-'
  && s.[2] = '-'

let bare_name s =
  if String.starts_with ~prefix:"--" s && String.length s > 2 then
    Some (String.sub s 2 (String.length s - 2))
  else None

(** Extract the var name from a bare var "(--name)" → "--name" *)
let bare_var_inner s =
  if is_bare_var s then String.sub s 1 (String.length s - 2) else s

(* {2 The color-mix() polyfill} *)

(* The tokens of [s] in order, and the source they were read from, which is what
   a rewrite slices: a comment between two tokens survives that way, and the
   text of a token an escape spells is the text as written. *)
let tokens_of s =
  let lexer = Cascade.Lexer.of_string s in
  let rec go acc =
    let token = Cascade.Lexer.next lexer in
    match token.Cascade.Token.kind with
    | Eof -> Array.of_list (List.rev acc)
    | _ -> go (token :: acc)
  in
  let tokens = go [] in
  (Cascade.Lexer.source lexer, tokens)

let is_function name (token : Cascade.Token.t) =
  match token.kind with
  | Function f -> String.equal (String.lowercase_ascii f) name
  | _ -> false

let is_whitespace (token : Cascade.Token.t) =
  match token.kind with Whitespace _ -> true | _ -> false

(* The index of the [)] closing the group the function token at [i] opens, or
   the index past the last token when the group runs to the end of the input,
   which a declaration value never lets it do. *)
let group_end tokens i =
  let n = Array.length tokens in
  let rec go j depth =
    if j >= n then n
    else
      match tokens.(j).Cascade.Token.kind with
      | Function _ | Open Paren -> go (j + 1) (depth + 1)
      | Close Paren when depth = 1 -> j
      | Close Paren -> go (j + 1) (depth - 1)
      | _ -> go (j + 1) depth
  in
  go i 0

(* The source text from the token at [i] through the token at [j]. *)
let source_slice source tokens i j =
  let start = tokens.(i).Cascade.Token.loc.start_pos in
  let stop =
    if j >= Array.length tokens then String.length source
    else tokens.(j).Cascade.Token.loc.end_pos
  in
  String.sub source start (stop - start)

(* The first token at or after [i] that is not whitespace, if [i] is inside the
   group ending at [last]. *)
let rec skip_whitespace tokens ~last i =
  if i >= last then None
  else if is_whitespace tokens.(i) then skip_whitespace tokens ~last (i + 1)
  else Some i

(* The custom property the [var()] at [i] reads: its first argument. *)
let var_name tokens i =
  match skip_whitespace tokens ~last:(group_end tokens i) (i + 1) with
  | Some j -> (
      match tokens.(j).Cascade.Token.kind with
      | Ident name -> Some name
      | _ -> None)
  | None -> None

(* The custom property a theme value reads when it opens with a [var()]
   reference, which Tailwind follows rather than inlines, whatever follows the
   reference. *)
let leading_var_name value =
  let _, tokens = tokens_of value in
  if Array.length tokens > 0 && is_function "var" tokens.(0) then
    var_name tokens 0
  else None

(* The value the theme binds [name] to, followed through a chain of references.
   [None] is a link the theme does not bind, or a chain that loops, which
   Tailwind cannot inline. *)
let rec inline_var ~resolve seen name =
  if List.mem name seen then None
  else
    match Option.map String.trim (Option.bind (bare_name name) resolve) with
    | None -> None
    | Some value -> (
        match leading_var_name value with
        | Some next -> inline_var ~resolve (name :: seen) next
        | None -> Some value)

(* What Tailwind's polyfill makes of the [color-mix()] at [i], closed at [last].
   [replace] says the mix stands for its first colour: it holds [currentcolor],
   or a [var()] the theme does not bind or binds to [currentcolor]. [edits] are
   the [var()] references the theme binds, each inlined with its value, which a
   replaced mix's first colour reads as well. *)
type mix_verdict = { replace : bool; edits : (int * int * string) list }

let is_currentcolor s = String.equal (String.lowercase_ascii s) "currentcolor"

let mix_verdict ~resolve tokens i last =
  let rec scan j replace edits =
    if j >= last then { replace; edits = List.rev edits }
    else
      match tokens.(j).Cascade.Token.kind with
      | Ident id when is_currentcolor id -> scan (j + 1) true edits
      | Function _ when is_function "var" tokens.(j) -> (
          match var_name tokens j with
          | None -> scan (j + 1) replace edits
          | Some name -> (
              match inline_var ~resolve [] name with
              | None -> scan (j + 1) true edits
              | Some value when is_currentcolor value -> scan (j + 1) true edits
              | Some value ->
                  let stop = group_end tokens j in
                  scan (stop + 1) replace ((j, stop, value) :: edits)))
      | _ -> scan (j + 1) replace edits
  in
  scan (i + 1) false []

(* The first colour of the [color-mix()] at [i]: the one token, or the one
   function call, after the first comma at the mix's own level. *)
let first_colour source tokens i last =
  let rec comma j depth =
    if j >= last then None
    else
      match tokens.(j).Cascade.Token.kind with
      | Function _ | Open Paren -> comma (j + 1) (depth + 1)
      | Close Paren -> comma (j + 1) (depth - 1)
      | Comma when depth = 0 -> Some (j + 1)
      | _ -> comma (j + 1) depth
  in
  match Option.bind (comma (i + 1) 0) (skip_whitespace tokens ~last) with
  | None -> None
  | Some j ->
      let stop =
        match tokens.(j).Cascade.Token.kind with
        | Function _ -> group_end tokens j
        | _ -> j
      in
      Some (source_slice source tokens j stop)

(* The colour space the mix at [i] names, when [in <space>] opens it and the
   space is one a browser without [color-mix()] has no colour for: the token to
   respell as [srgb], as Tailwind does for the value in the open. *)
let wide_space tokens i last =
  let ident j =
    match tokens.(j).Cascade.Token.kind with Ident id -> Some id | _ -> None
  in
  match skip_whitespace tokens ~last (i + 1) with
  | Some j when ident j = Some "in" -> (
      match skip_whitespace tokens ~last (j + 1) with
      | Some k -> (
          match ident k with
          | Some ("oklab" | "oklch" | "lab" | "lch") -> Some k
          | Some _ | None -> None)
      | None -> None)
  | Some _ | None -> None

(* The respelling of every wide space the mix at [i] and the mixes nested in it
   name, as edits. *)
let space_edits tokens i last =
  let rec go j acc =
    if j >= last then List.rev acc
    else if not (is_function "color-mix" tokens.(j)) then go (j + 1) acc
    else
      match wide_space tokens j (group_end tokens j) with
      | Some k -> go (j + 1) ((k, k, "srgb") :: acc)
      | None -> go (j + 1) acc
  in
  go i []

(* The source from token [i] through token [last], with the token ranges in
   [edits] replaced by the text given for each. *)
let rewrite source tokens i last edits =
  let edits = List.sort (fun (a, _, _) (b, _, _) -> compare a b) edits in
  let buf = Buffer.create 64 in
  let rec go j = function
    | [] -> Buffer.add_string buf (source_slice source tokens j last)
    | (a, b, text) :: rest ->
        if a > j then
          Buffer.add_string buf (source_slice source tokens j (a - 1));
        Buffer.add_string buf text;
        go (b + 1) rest
  in
  go i edits;
  Buffer.contents buf

(* [color-mix()] calls at any depth, Tailwind's polyfill applied to each in
   turn. Whether the value needs the polyfill is decided once for the whole
   value, so a mix a browser resolves on its own still has its space respelled
   when an earlier one, or one enclosing it, needed the polyfill. *)
let rec color_mix_fallback ~resolve s =
  let source, tokens = tokens_of s in
  let n = Array.length tokens in
  let buf = Buffer.create (String.length s) in
  let rec go i copied polyfilled =
    if i >= n then (
      if copied < n then
        Buffer.add_string buf (source_slice source tokens copied (n - 1));
      polyfilled)
    else if not (is_function "color-mix" tokens.(i)) then
      go (i + 1) copied polyfilled
    else
      let last = group_end tokens i in
      let { replace; edits } = mix_verdict ~resolve tokens i last in
      let polyfilled = polyfilled || replace || edits <> [] in
      let replacement =
        if replace then
          (* The first colour is read off the mix with its references inlined,
             so a reference the theme binds gives the value it binds. *)
          let source, tokens = tokens_of (rewrite source tokens i last edits) in
          Option.map
            (fun colour ->
              Option.value (color_mix_fallback ~resolve colour) ~default:colour)
            (first_colour source tokens 0 (group_end tokens 0))
        else if polyfilled then
          Some
            (rewrite source tokens i last (space_edits tokens i last @ edits))
        else None
      in
      match replacement with
      | None -> go (last + 1) copied polyfilled
      | Some text ->
          if i > copied then
            Buffer.add_string buf (source_slice source tokens copied (i - 1));
          Buffer.add_string buf text;
          go (last + 1) (last + 1) polyfilled
  in
  if go 0 0 false then Some (Buffer.contents buf) else None

let alpha_mix ~alpha value =
  let alpha =
    Cascade.Css.Pp.to_string ~minify:true
      (Cascade.Css.Values.pp_percentage ~always:true)
      alpha
  in
  wrap_declaration_value ~before:"color-mix(in oklab, "
    ~after:(" " ^ alpha ^ ", transparent)")
    value

(* Split [s] on [sep], reading a [[...]] or a [(...)] group, nested brackets of
   its own kind included, as one atom: the separator inside one belongs to the
   piece around it. Always yields one piece more than the separators it split
   on, so joining the pieces back with [sep] reconstructs [s]. *)
let split_atomic sep s =
  let len = String.length s in
  let buf = Buffer.create 16 in
  let parts = ref [] in
  let i = ref 0 in
  let group opening closing =
    let depth = ref 1 in
    Buffer.add_char buf opening;
    incr i;
    while !i < len && !depth > 0 do
      let c = s.[!i] in
      Buffer.add_char buf c;
      if c = opening then incr depth else if c = closing then decr depth;
      incr i
    done
  in
  while !i < len do
    let c = s.[!i] in
    if c = '[' then group '[' ']'
    else if c = '(' then group '(' ')'
    else if c = sep then (
      parts := Buffer.contents buf :: !parts;
      Buffer.clear buf;
      incr i)
    else (
      Buffer.add_char buf c;
      incr i)
  done;
  parts := Buffer.contents buf :: !parts;
  List.rev !parts

(** Split a class name on '-' but treat '[...]' as atomic. E.g.
    "m-[var(--value)]" → ["m"; "[var(--value)]"] E.g. "-m-[var(--value)]" →
    [""; "m"; "[var(--value)]"] *)
let split_class_uncached = split_atomic '-'

(** Split a variant chain on ':', treating '[...]' and '(...)' as atomic so a
    colon inside an arbitrary value or a shorthand var reference (e.g.
    "hover:bg-[color:var(--x)]") is not read as a variant separator. Always
    yields (colon count + 1) tokens: joining the result back with ':'
    reconstructs the input. *)
let split_on_colon = split_atomic ':'

(* Utility.base_of_class offers one class name to every handler in turn until
   one accepts it, and most handlers open by splitting that same name, so a
   single class is split once per handler tried. The split is a pure function of
   its argument, so remembering the last one collapses that run to one. *)
let last_split = Domain.DLS.new_key (fun () -> None)

let split_class class_name =
  match Domain.DLS.get last_split with
  | Some (key, parts) when key == class_name || String.equal key class_name ->
      parts
  | _ ->
      let parts = split_class_uncached class_name in
      Domain.DLS.set last_split (Some (class_name, parts));
      parts

(* Tailwind reads a shadow by taking its lengths and leaving what is left as the
   colour, so a trailing [var()] the shadow grammar read as the next length slot
   is the colour: [0 1px 2px var(--c)] paints with [--c]. The reference keeps
   its name and a syntactic fallback; a length fallback is no colour. *)
let colour_of_length_var (v : Cascade.Css.length Cascade.Css.var) :
    Cascade.Css.color Cascade.Css.var =
  let fallback : Cascade.Css.color Cascade.Css.fallback =
    match v.Cascade.Values.fallback with
    | Fallback _ | None -> None
    | Empty -> Empty
    | Empty2 -> Empty2
    | Syntax_fallback cv -> Syntax_fallback cv
    | Var_fallback s -> Var_fallback s
  in
  Cascade.Css.var_ref ~fallback ?layer:v.Cascade.Values.layer
    ?meta:v.Cascade.Values.meta ~runtime:v.Cascade.Values.runtime
    v.Cascade.Values.name

let trailing_var_as_colour (body : Cascade.Css.shadow_body) :
    Cascade.Css.shadow_body =
  let colour v : Cascade.Css.color = Var (colour_of_length_var v) in
  match body with
  | { color = None; spread = Some (Var v); _ } ->
      { body with spread = None; color = Some (colour v) }
  | { color = None; spread = None; blur = Some (Var v); _ } ->
      { body with blur = None; color = Some (colour v) }
  | _ -> body

let shadow s : Cascade.Css.shadow option =
  let rec reslot : Cascade.Css.shadow -> Cascade.Css.shadow = function
    | Shadow body -> Shadow (trailing_var_as_colour body)
    | Inset (Body body) -> Inset (Body (trailing_var_as_colour body))
    | Inset (Toggle t) ->
        Inset (Toggle { t with body = trailing_var_as_colour t.body })
    | List layers -> List (List.map reslot layers)
    | other -> other
  in
  Option.map reslot (Cascade.Css.parse_shadow s)

type shadow_colour =
  | Hex_token of string
  | Var_token of string
  | Colour of Cascade.Css.color
  | No_colour

let shadow_layer s =
  let parts = String.split_on_char ' ' (decode_arbitrary_value s) in
  let rec split acc = function
    | [] -> (List.rev acc, No_colour)
    | x :: _ when String.length x > 0 && x.[0] = '#' ->
        (List.rev acc, Hex_token x)
    | x :: _ when is_var x -> (List.rev acc, Var_token x)
    | x :: rest when is_css_color_fn x -> (
        (* A colour function may carry spaces, so it runs to the end of the
           value. *)
        match Cascade.Css.parse_color (String.concat " " (x :: rest)) with
        | Some c -> (List.rev acc, Colour c)
        | None -> split (x :: acc) rest)
    | x :: rest -> split (x :: acc) rest
  in
  let length_strs, colour = split [] parts in
  let lengths = List.filter_map arbitrary_length length_strs in
  (* A token that is not a length makes the value not a shadow. Dropping it
     instead would slide the surviving lengths into the wrong slots. A [#] token
     is only the shadow's colour when it is a hex spelling. *)
  if List.compare_lengths lengths length_strs <> 0 then None
  else
    match colour with
    | Hex_token h when Option.is_none (Cascade.Css.hex_opt h) -> None
    | _ -> Some (lengths, colour)
