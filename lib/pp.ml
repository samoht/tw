type 'a t = 'a -> string

let str segments = String.concat "" segments
let sep separator segments = String.concat separator segments
let lines segments = String.concat "\n" segments
let kv key value = key ^ ": " ^ value
let field key value = key ^ " = " ^ value ^ ";"
let braces content = "{" ^ content ^ "}"
let parens content = "(" ^ content ^ ")"
let quote s = "\"" ^ s ^ "\""
let indent n s = String.make n ' ' ^ s
let option f = function None -> "None" | Some x -> "Some " ^ parens (f x)
let list ?(sep = ", ") f lst = "[" ^ String.concat sep (List.map f lst) ^ "]"

let record fields =
  match fields with
  | [] -> "{ }"
  | _ ->
      let field_strs = List.map (fun (k, v) -> field k v) fields in
      "{ " ^ String.concat " " field_strs ^ " }"

let string s = s
let bool = string_of_bool
let int = string_of_int

(* Two lowercase hex digits. *)
let hex_byte n =
  let hex = "0123456789abcdef" in
  String.make 1 hex.[n / 16] ^ String.make 1 hex.[n mod 16]

(* No trailing dot. *)
let float f =
  let s = string_of_float f in
  if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
  else s
