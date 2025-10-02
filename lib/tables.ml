(** Table-related utilities

    What's included:
    - Border collapse: `border-collapse`, `border-separate`.
    - Border spacing: `border-spacing-*` with numeric values.
    - Table layout: `table-auto`, `table-fixed`.

    What's not:
    - Other table-specific properties not exposed in the typed `Css` API.

    Parsing contract (`of_string`):
    - Accepts tokens like ["border"; "collapse"], ["border"; "separate"],
      ["border"; "spacing"; n], ["table"; "auto"], ["table"; "fixed"].
    - Unknown tokens yield `Error (`Msg "Not a table utility")`. *)

open Style
open Css

(** Local table utility type *)
type t =
  | Border_collapse
  | Border_separate
  | Border_spacing of int
  | Table_auto
  | Table_fixed

(** Extensible variant for table utilities *)
type Utility.base += Tables of t

(** Error helper *)
let err_not_utility = Error (`Msg "Not a table utility")

(** Conversion functions *)
let to_style = function
  | Border_collapse -> style "border-collapse" [ Css.border_collapse Collapse ]
  | Border_separate -> style "border-separate" [ Css.border_collapse Separate ]
  | Border_spacing n ->
      let class_name = "border-spacing-" ^ string_of_int n in
      style class_name [ Css.border_spacing (Rem (float_of_int n *. 0.25)) ]
  | Table_auto -> style "table-auto" [ Css.table_layout Auto ]
  | Table_fixed -> style "table-fixed" [ Css.table_layout Fixed ]

let suborder = function
  | Border_collapse -> 0
  | Border_separate -> 1
  | Border_spacing n -> 100 + n
  | Table_auto -> 200
  | Table_fixed -> 201

let of_string = function
  | [ "border"; "collapse" ] -> Ok Border_collapse
  | [ "border"; "separate" ] -> Ok Border_separate
  | [ "border"; "spacing"; n ] -> (
      match int_of_string_opt n with
      | Some i when i >= 0 -> Ok (Border_spacing i)
      | _ -> err_not_utility)
  | [ "table"; "auto" ] -> Ok Table_auto
  | [ "table"; "fixed" ] -> Ok Table_fixed
  | _ -> err_not_utility

(** Priority for table utilities *)
let priority = 10

(** Typed handler *)
let handler : t Utility.handler = { to_style; priority; suborder; of_string }

(** Wrapper functions for extensible variant *)
let wrap x = Tables x

let unwrap = function Tables x -> Some x | _ -> None

(** Public API *)
let utility x = Utility.base (Tables x)

let border_collapse = utility Border_collapse
let border_separate = utility Border_separate
let border_spacing n = utility (Border_spacing n)
let table_auto = utility Table_auto
let table_fixed = utility Table_fixed

(** Register handler with Utility system *)
let () = Utility.register ~wrap ~unwrap handler
