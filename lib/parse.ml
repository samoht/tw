let int_any s =
  match int_of_string_opt s with
  | Some n -> Ok n
  | None -> Error (`Msg ("Invalid number: " ^ s))

let int_pos ~name s =
  match int_of_string_opt s with
  | Some n when n >= 0 -> Ok n
  | Some _ -> Error (`Msg (name ^ " must be non-negative: " ^ s))
  | None -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))

(* Parse decimal values like "0.5", "1.5" for spacing utilities *)
let decimal_pos ~name s =
  try
    let f = Float.of_string s in
    if f >= 0.0 then Ok f
    else Error (`Msg (name ^ " must be non-negative: " ^ s))
  with Failure _ -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))

(* Parse spacing values - handles both integers and decimals *)
let spacing_value ~name s =
  match int_of_string_opt s with
  | Some n when n >= 0 -> Ok (float_of_int n)
  | Some _ -> Error (`Msg (name ^ " must be non-negative: " ^ s))
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
let is_valid_theme_name s = not (String.contains s '/')
let ( >|= ) r f = Result.map f r

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
