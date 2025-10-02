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
module Parse = Parse

(** Error helpers *)
let err_not_utility = Error (`Msg "Not a padding utility")

type Utility.base += Padding of [ `All | `X | `Y | `T | `R | `B | `L ] * spacing

(** Helper to create Utility.t from Padding *)
let padding_t axis value = Utility.Utility (Padding (axis, value))

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

let to_style = function
  | Padding (side, value) -> (
      match side with
      | `All -> Some (p' value)
      | `X -> Some (px' value)
      | `Y -> Some (py' value)
      | `T -> Some (pt' value)
      | `R -> Some (pr' value)
      | `B -> Some (pb' value)
      | `L -> Some (pl' value))
  | _ -> None

let spacing_value_order = function
  | `Px -> 1
  | `Full -> 10000
  | `Rem f ->
      let units = f /. 0.25 in
      int_of_float (units *. 10.)

let of_string parts =
  let parse_class = function
    | [ "p"; value ] -> (
        match Parse.spacing_value ~name:"padding" value with
        | Ok f -> Some (Padding (`All, `Rem (f *. 0.25)))
        | Error _ ->
            if value = "px" then Some (Padding (`All, `Px))
            else if value = "full" then Some (Padding (`All, `Full))
            else None)
    | [ "px"; value ] -> (
        match Parse.spacing_value ~name:"padding-x" value with
        | Ok f -> Some (Padding (`X, `Rem (f *. 0.25)))
        | Error _ ->
            if value = "px" then Some (Padding (`X, `Px))
            else if value = "full" then Some (Padding (`X, `Full))
            else None)
    | [ "py"; value ] -> (
        match Parse.spacing_value ~name:"padding-y" value with
        | Ok f -> Some (Padding (`Y, `Rem (f *. 0.25)))
        | Error _ ->
            if value = "px" then Some (Padding (`Y, `Px))
            else if value = "full" then Some (Padding (`Y, `Full))
            else None)
    | [ "pt"; value ] -> (
        match Parse.spacing_value ~name:"padding-top" value with
        | Ok f -> Some (Padding (`T, `Rem (f *. 0.25)))
        | Error _ ->
            if value = "px" then Some (Padding (`T, `Px))
            else if value = "full" then Some (Padding (`T, `Full))
            else None)
    | [ "pr"; value ] -> (
        match Parse.spacing_value ~name:"padding-right" value with
        | Ok f -> Some (Padding (`R, `Rem (f *. 0.25)))
        | Error _ ->
            if value = "px" then Some (Padding (`R, `Px))
            else if value = "full" then Some (Padding (`R, `Full))
            else None)
    | [ "pb"; value ] -> (
        match Parse.spacing_value ~name:"padding-bottom" value with
        | Ok f -> Some (Padding (`B, `Rem (f *. 0.25)))
        | Error _ ->
            if value = "px" then Some (Padding (`B, `Px))
            else if value = "full" then Some (Padding (`B, `Full))
            else None)
    | [ "pl"; value ] -> (
        match Parse.spacing_value ~name:"padding-left" value with
        | Ok f -> Some (Padding (`L, `Rem (f *. 0.25)))
        | Error _ ->
            if value = "px" then Some (Padding (`L, `Px))
            else if value = "full" then Some (Padding (`L, `Full))
            else None)
    | _ -> None
  in
  match parse_class parts with Some u -> Ok u | None -> err_not_utility

let suborder = function
  | Padding (side, value) ->
      let side_offset =
        match side with
        | `All -> 0
        | `X -> 100
        | `Y -> 200
        | `T -> 300
        | `R -> 400
        | `B -> 500
        | `L -> 600
      in
      Some (1000 + side_offset + spacing_value_order value)
  | _ -> None

(** Priority for padding utilities *)
let priority = 19

let order u = Option.map (fun s -> (priority, s)) (suborder u)

(** Register padding handler with Utility system *)
let () =
  Utility.register
    {
      to_style;
      order;
      of_string =
        (fun parts ->
          match of_string parts with Ok u -> Some (Ok u) | Error _ -> None);
    }
