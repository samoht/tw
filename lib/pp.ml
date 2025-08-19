(** Fmt - Lightweight formatting DSL for UI modules

    This module provides a lightweight string formatting DSL that avoids
    Printf/Format modules to keep js_of_ocaml bundle sizes small. The Format
    module is known to significantly increase JavaScript bundle sizes, so we use
    simple string concatenation instead. *)

type 'a t = 'a -> string

(** Format a string with arguments *)
let str segments = String.concat "" segments

(** Format with separator *)
let sep separator segments = String.concat separator segments

(** Format with newlines *)
let lines segments = String.concat "\n" segments

(** Format key-value pair *)
let kv key value = key ^ ": " ^ value

(** Format field with semicolon *)
let field key value = key ^ " = " ^ value ^ ";"

(** Format in braces *)
let braces content = "{" ^ content ^ "}"

(** Format in parentheses *)
let parens content = "(" ^ content ^ ")"

(** Format quoted *)
let quote s = "\"" ^ s ^ "\""

(** Indent with spaces *)
let indent n s = String.make n ' ' ^ s

(** Format option *)
let option f = function None -> "None" | Some x -> "Some " ^ parens (f x)

(** Format list *)
let list ?(sep = ", ") f lst = "[" ^ String.concat sep (List.map f lst) ^ "]"

(** Format record *)
let record fields =
  match fields with
  | [] -> "{ }"
  | _ ->
      let field_strs = List.map (fun (k, v) -> field k v) fields in
      "{ " ^ String.concat " " field_strs ^ " }"

(** String formatter *)
let string s = s

(** Format bool *)
let bool = string_of_bool

(** Format int *)
let int = string_of_int

(** Format float nicely without trailing dots *)
let float f =
  let s = string_of_float f in
  if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
  else s

(** Format float with n decimal places *)
let float_n decimals f =
  let multiplier = Float.pow 10.0 (float_of_int decimals) in
  let rounded = Float.round (f *. multiplier) /. multiplier in
  (* Check if it's a whole number *)
  if Float.is_integer rounded then int (int_of_float rounded)
  else
    (* Format without Printf *)
    let whole = int_of_float (Float.floor rounded) in
    let frac_value = (rounded -. Float.floor rounded) *. multiplier in
    let frac_int = int_of_float (Float.round frac_value) in
    if frac_int = 0 then int whole
    else
      (* Convert fraction to string and remove trailing zeros *)
      let frac_str = string_of_int frac_int in
      let rec remove_trailing_zeros s =
        let len = String.length s in
        if len > 0 && s.[len - 1] = '0' then
          remove_trailing_zeros (String.sub s 0 (len - 1))
        else s
      in
      let trimmed_frac = remove_trailing_zeros frac_str in
      str [ int whole; "."; trimmed_frac ]
