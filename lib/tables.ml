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

module Handler = struct
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
  type Utility.base += Self of t

  (** Priority for table utilities - comes before layout utilities *)
  let name = "tables"

  let priority = 4
  let err_not_utility = Error (`Msg "Not a table utility")

  let to_style = function
    | Border_collapse -> style [ Css.border_collapse Collapse ]
    | Border_separate -> style [ Css.border_collapse Separate ]
    | Border_spacing n ->
        style [ Css.border_spacing (Rem (float_of_int n *. 0.25)) ]
    | Table_auto -> style [ Css.table_layout Auto ]
    | Table_fixed -> style [ Css.table_layout Fixed ]

  let suborder = function
    | Border_collapse -> 0
    | Border_separate -> 1
    | Border_spacing n -> 2 + n
    | Table_auto -> 6
    | Table_fixed -> 7

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "border"; "collapse" ] -> Ok Border_collapse
    | [ "border"; "separate" ] -> Ok Border_separate
    | [ "border"; "spacing"; n ] -> (
        match int_of_string_opt n with
        | Some i when i >= 0 -> Ok (Border_spacing i)
        | _ -> err_not_utility)
    | [ "table"; "auto" ] -> Ok Table_auto
    | [ "table"; "fixed" ] -> Ok Table_fixed
    | _ -> err_not_utility

  let to_class = function
    | Border_collapse -> "border-collapse"
    | Border_separate -> "border-separate"
    | Border_spacing n -> "border-spacing-" ^ string_of_int n
    | Table_auto -> "table-auto"
    | Table_fixed -> "table-fixed"
end

open Handler

(** Register handler with Utility system *)
let () = Utility.register (module Handler)

(** Public API *)
let utility x = Utility.base (Self x)

let border_collapse = utility Border_collapse
let border_separate = utility Border_separate
let border_spacing n = utility (Border_spacing n)
let table_auto = utility Table_auto
let table_fixed = utility Table_fixed
