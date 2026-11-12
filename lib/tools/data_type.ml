module Css = Cascade.Css

(* Tailwind's [segment]: split [s] on [sep] at the top level, so a separator
   inside a bracket group or a quoted string belongs to the piece around it. *)
let segment sep s =
  let len = String.length s in
  let pieces = ref [] in
  let buf = Buffer.create len in
  let depth = ref 0 in
  let quote = ref None in
  let i = ref 0 in
  while !i < len do
    let c = s.[!i] in
    (match !quote with
    | Some q ->
        Buffer.add_char buf c;
        if c = '\\' && !i + 1 < len then (
          Buffer.add_char buf s.[!i + 1];
          incr i)
        else if c = q then quote := None
    | None -> (
        match c with
        | '\'' | '"' ->
            quote := Some c;
            Buffer.add_char buf c
        | '(' | '[' | '{' ->
            incr depth;
            Buffer.add_char buf c
        | ')' | ']' | '}' ->
            if !depth > 0 then decr depth;
            Buffer.add_char buf c
        | c when c = sep && !depth = 0 ->
            pieces := Buffer.contents buf :: !pieces;
            Buffer.clear buf
        | c -> Buffer.add_char buf c));
    incr i
  done;
  List.rev (Buffer.contents buf :: !pieces)

let is_digit c = c >= '0' && c <= '9'

(* A [<number>] as a candidate spells one: an optional sign, digits with at most
   one decimal point and at least one after it, and an optional exponent. *)
let number_re =
  let sign = Re.opt (Re.set "+-") in
  Re.compile
    (Re.whole_string
       (Re.seq
          [
            sign;
            Re.alt
              [
                Re.seq [ Re.rep Re.digit; Re.char '.'; Re.rep1 Re.digit ];
                Re.rep1 Re.digit;
              ];
            Re.opt (Re.seq [ Re.set "eE"; sign; Re.rep1 Re.digit ]);
          ]))

let is_number text = Re.execp number_re text

let is_percentage text =
  let n = String.length text in
  n > 1 && text.[n - 1] = '%' && is_number (String.sub text 0 (n - 1))

let is_fraction text =
  match segment '/' text with
  | [ left; right ] ->
      is_number (String.trim left) && is_number (String.trim right)
  | _ -> false

(* The spelling a round trip through a number leaves unchanged, which is what
   Tailwind's [String(Number(value)) === String(value)] asks for: no sign, no
   redundant leading zero, no trailing zero in the fraction, no bare [.]. *)
let is_canonical_number text =
  let whole, fraction =
    match String.index_opt text '.' with
    | None -> (text, None)
    | Some i ->
        ( String.sub text 0 i,
          Some (String.sub text (i + 1) (String.length text - i - 1)) )
  in
  let digits s = s <> "" && String.for_all is_digit s in
  digits whole
  && (String.length whole = 1 || whole.[0] <> '0')
  &&
  match fraction with
  | None -> true
  | Some f -> digits f && f.[String.length f - 1] <> '0'

let is_positive_integer text =
  is_canonical_number text && not (String.contains text '.')

(* Tailwind's spacing multiplier: a canonical non-negative number that is a
   whole multiple of [0.25]. *)
let is_spacing_multiplier text =
  is_canonical_number text
  &&
  match float_of_string_opt text with
  | None -> false
  | Some value -> Float.rem value 0.25 = 0.

(* MDN's list of CSS length units, the one Tailwind reads a bare length
   against. *)
let length_units =
  [
    "cm";
    "mm";
    "Q";
    "in";
    "pc";
    "pt";
    "px";
    "em";
    "ex";
    "ch";
    "rem";
    "lh";
    "rlh";
    "vw";
    "vh";
    "vmin";
    "vmax";
    "vb";
    "vi";
    "svw";
    "svh";
    "lvw";
    "lvh";
    "dvw";
    "dvh";
    "cqw";
    "cqh";
    "cqi";
    "cqb";
    "cqmin";
    "cqmax";
  ]

let math_functions =
  [
    "calc";
    "min";
    "max";
    "clamp";
    "round";
    "mod";
    "rem";
    "sin";
    "cos";
    "tan";
    "asin";
    "acos";
    "atan";
    "atan2";
    "pow";
    "sqrt";
    "hypot";
    "log";
    "exp";
    "abs";
    "sign";
  ]

(* A math function stands for the value it computes, so it reads as every
   numeric type. *)
let math_fn_re =
  Re.compile
    (Re.alt (List.map (fun name -> Re.str (name ^ "(")) math_functions))

let has_math_fn text = Re.execp math_fn_re text

let is_length text =
  has_math_fn text
  || List.exists
       (fun unit ->
         let n = String.length text and u = String.length unit in
         String.ends_with ~suffix:unit text
         && n > u
         && is_number (String.sub text 0 (n - u)))
       length_units

(* Whether [text] reads as the CSS data type [kind]. A [var()] is opaque, so it
   reads as nothing at all. The kinds beyond these are ones no [--value([kind])]
   here asks for; one that does resolves to nothing rather than to a guess. *)
let infer text kind =
  if String.starts_with ~prefix:"var(" text then false
  else
    match kind with
    | "color" -> Option.is_some (Css.parse_color text)
    | "length" -> is_length text
    | "percentage" -> is_percentage text || has_math_fn text
    | "ratio" -> is_fraction text || has_math_fn text
    | "number" -> is_number text || has_math_fn text
    | "integer" -> is_positive_integer text
    | _ -> false

(* Only these four data types are read from a bare candidate value, so no
   [--value(color)] can turn [example-red] into a utility. *)
let bare_kinds = [ "number"; "integer"; "ratio"; "percentage" ]
