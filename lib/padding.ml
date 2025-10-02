(** Padding utilities

    What's included:
    - Padding utilities: `p-*`, `px-*`, `py-*`, `pt-*`, `pr-*`, `pb-*`, `pl-*`
      supporting spacing scale values (0-96), `px`, and `full`.

    What's not:
    - Negative padding values (not valid in CSS).
    - Arbitrary padding values outside the spacing scale. Use `style` with raw
      `Css.padding` if needed.

    Parsing contract (`of_string`):
    - Accepts tokens like ["p"; value], ["px"; value], ["py"; value],
      ["pt"; value], etc., where value is a spacing scale number, "px", or
      "full".
    - Unknown tokens yield `Error (`Msg "Not a padding utility")`. *)

open Style
open Css

type t = { axis : [ `All | `X | `Y | `T | `R | `B | `L ]; value : spacing }
(** Local padding utility type *)

(** Extensible variant for padding utilities *)
type Utility.base += Padding of t

(** Wrapper functions *)
let wrap x = Padding x

let unwrap = function Padding x -> Some x | _ -> None
let base x = Utility.base (wrap x)

(** Error helper *)
let err_not_utility = Error (`Msg "Not a padding utility")

(** Helper to create Utility.t from Padding *)
let padding_t axis value = base { axis; value }

(** {2 Typed Padding Utilities} *)

let padding_util prefix prop (s : spacing) =
  let class_name = prefix ^ Spacing.pp_spacing_suffix s in
  let spacing_decl, spacing_ref = Var.binding Spacing.spacing_var (Rem 0.25) in
  let len = Spacing.to_length spacing_ref s in
  match s with
  | `Rem _ -> style class_name [ spacing_decl; prop len ]
  | _ -> style class_name [ prop len ]

let padding_list_util prefix prop (s : spacing) =
  let class_name = prefix ^ Spacing.pp_spacing_suffix s in
  let spacing_decl, spacing_ref = Var.binding Spacing.spacing_var (Rem 0.25) in
  let len = Spacing.to_length spacing_ref s in
  match s with
  | `Rem _ -> style class_name [ spacing_decl; prop [ len ] ]
  | _ -> style class_name [ prop [ len ] ]

let p' = padding_list_util "p-" padding
let px' = padding_util "px-" padding_inline
let py' = padding_util "py-" padding_block
let pt' = padding_util "pt-" padding_top
let pr' = padding_util "pr-" padding_right
let pb' = padding_util "pb-" padding_bottom
let pl' = padding_util "pl-" padding_left

(** {2 Int-based Padding Utilities} *)

let p n = padding_t `All (Spacing.int n)
let px n = padding_t `X (Spacing.int n)
let py n = padding_t `Y (Spacing.int n)
let pt n = padding_t `T (Spacing.int n)
let pr n = padding_t `R (Spacing.int n)
let pb n = padding_t `B (Spacing.int n)
let pl n = padding_t `L (Spacing.int n)

(** {2 Special Values} *)

let p_px = padding_t `All `Px
let p_full = padding_t `All `Full
let px_px = padding_t `X `Px
let px_full = padding_t `X `Full
let py_px = padding_t `Y `Px
let py_full = padding_t `Y `Full
let pt_px = padding_t `T `Px
let pt_full = padding_t `T `Full
let pr_px = padding_t `R `Px
let pr_full = padding_t `R `Full
let pb_px = padding_t `B `Px
let pb_full = padding_t `B `Full
let pl_px = padding_t `L `Px
let pl_full = padding_t `L `Full

(** {1 Conversion Functions} *)

let spacing_value_order = function
  | `Px -> 1
  | `Full -> 10000
  | `Rem f ->
      let units = f /. 0.25 in
      int_of_float (units *. 10.)

(** Convert padding utility to style *)
let to_style { axis; value } =
  match axis with
  | `All -> p' value
  | `X -> px' value
  | `Y -> py' value
  | `T -> pt' value
  | `R -> pr' value
  | `B -> pb' value
  | `L -> pl' value

(** Suborder for padding utilities *)
let suborder { axis; value } =
  let side_offset =
    match axis with
    | `All -> 0
    | `X -> 100
    | `Y -> 200
    | `T -> 300
    | `R -> 400
    | `B -> 500
    | `L -> 600
  in
  1000 + side_offset + spacing_value_order value

(** Parse string parts to padding utility *)
let of_string parts =
  let parse_class = function
    | [ "p"; value ] -> (
        match Parse.spacing_value ~name:"padding" value with
        | Ok f -> Ok { axis = `All; value = `Rem (f *. 0.25) }
        | Error _ ->
            if value = "px" then Ok { axis = `All; value = `Px }
            else if value = "full" then Ok { axis = `All; value = `Full }
            else err_not_utility)
    | [ "px"; value ] -> (
        match Parse.spacing_value ~name:"padding-x" value with
        | Ok f -> Ok { axis = `X; value = `Rem (f *. 0.25) }
        | Error _ ->
            if value = "px" then Ok { axis = `X; value = `Px }
            else if value = "full" then Ok { axis = `X; value = `Full }
            else err_not_utility)
    | [ "py"; value ] -> (
        match Parse.spacing_value ~name:"padding-y" value with
        | Ok f -> Ok { axis = `Y; value = `Rem (f *. 0.25) }
        | Error _ ->
            if value = "px" then Ok { axis = `Y; value = `Px }
            else if value = "full" then Ok { axis = `Y; value = `Full }
            else err_not_utility)
    | [ "pt"; value ] -> (
        match Parse.spacing_value ~name:"padding-top" value with
        | Ok f -> Ok { axis = `T; value = `Rem (f *. 0.25) }
        | Error _ ->
            if value = "px" then Ok { axis = `T; value = `Px }
            else if value = "full" then Ok { axis = `T; value = `Full }
            else err_not_utility)
    | [ "pr"; value ] -> (
        match Parse.spacing_value ~name:"padding-right" value with
        | Ok f -> Ok { axis = `R; value = `Rem (f *. 0.25) }
        | Error _ ->
            if value = "px" then Ok { axis = `R; value = `Px }
            else if value = "full" then Ok { axis = `R; value = `Full }
            else err_not_utility)
    | [ "pb"; value ] -> (
        match Parse.spacing_value ~name:"padding-bottom" value with
        | Ok f -> Ok { axis = `B; value = `Rem (f *. 0.25) }
        | Error _ ->
            if value = "px" then Ok { axis = `B; value = `Px }
            else if value = "full" then Ok { axis = `B; value = `Full }
            else err_not_utility)
    | [ "pl"; value ] -> (
        match Parse.spacing_value ~name:"padding-left" value with
        | Ok f -> Ok { axis = `L; value = `Rem (f *. 0.25) }
        | Error _ ->
            if value = "px" then Ok { axis = `L; value = `Px }
            else if value = "full" then Ok { axis = `L; value = `Full }
            else err_not_utility)
    | _ -> err_not_utility
  in
  parse_class parts

(** Priority for padding utilities *)
let priority = 19

(** Typed handler for padding utilities *)
let handler : t Utility.handler = { to_style; priority; suborder; of_string }

(** Register handler with Utility system *)

let () = Utility.register ~wrap ~unwrap handler

module Handler = struct
  type nonrec t = t

  let of_string = of_string
  let suborder = suborder
  let to_style = to_style
  let order x = (priority, suborder x)
end
