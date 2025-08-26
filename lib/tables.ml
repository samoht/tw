(** Table-related utilities *)

open Core
open Css

let border_collapse = style "border-collapse" [ Css.border_collapse Collapse ]
let border_separate = style "border-separate" [ Css.border_collapse Separate ]

let border_spacing n =
  let class_name = Pp.str [ "border-spacing-"; string_of_int n ] in
  (* Tailwind spacing scale uses 0.25rem increments *)
  style class_name [ Css.border_spacing (Rem (float_of_int n *. 0.25)) ]

let table_auto = style "table-auto" [ Css.table_layout Auto ]
let table_fixed = style "table-fixed" [ Css.table_layout Fixed ]

let of_string = function
  | [ "border"; "collapse" ] -> Ok border_collapse
  | [ "border"; "separate" ] -> Ok border_separate
  | [ "border"; "spacing"; n ] -> (
      match int_of_string_opt n with
      | Some i when i >= 0 -> Ok (border_spacing i)
      | _ -> Error (`Msg (Pp.str [ "Invalid border-spacing: "; n ])))
  | [ "table"; "auto" ] -> Ok table_auto
  | [ "table"; "fixed" ] -> Ok table_fixed
  | _ -> Error (`Msg "Not a table utility")
