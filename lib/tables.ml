(** Table-related utilities *)

open Style
open Css

(** {1 Utility Types} *)

type utility =
  | Border_collapse
  | Border_separate
  | Border_spacing of int
  | Table_auto
  | Table_fixed

let border_collapse = style "border-collapse" [ Css.border_collapse Collapse ]
let border_separate = style "border-separate" [ Css.border_collapse Separate ]

let border_spacing n =
  let class_name = "border-spacing-" ^ string_of_int n in
  (* Tailwind spacing scale uses 0.25rem increments *)
  style class_name [ Css.border_spacing (Rem (float_of_int n *. 0.25)) ]

let table_auto = style "table-auto" [ Css.table_layout Auto ]
let table_fixed = style "table-fixed" [ Css.table_layout Fixed ]

(** {1 Conversion Functions} *)

let to_style = function
  | Border_collapse -> border_collapse
  | Border_separate -> border_separate
  | Border_spacing n -> border_spacing n
  | Table_auto -> table_auto
  | Table_fixed -> table_fixed

let of_string = function
  | [ "border"; "collapse" ] -> Ok Border_collapse
  | [ "border"; "separate" ] -> Ok Border_separate
  | [ "border"; "spacing"; n ] -> (
      match int_of_string_opt n with
      | Some i when i >= 0 -> Ok (Border_spacing i)
      | _ -> Error (`Msg ("Invalid border-spacing: " ^ n)))
  | [ "table"; "auto" ] -> Ok Table_auto
  | [ "table"; "fixed" ] -> Ok Table_fixed
  | _ -> Error (`Msg "Not a table utility")

(** {1 Ordering Support} *)

let suborder = function
  | Border_collapse -> 0
  | Border_separate -> 1
  | Border_spacing n -> 100 + n
  | Table_auto -> 200
  | Table_fixed -> 201
