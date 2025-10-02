(** Gap and space-between utilities

    What's included:
    - `gap-*` - Gap between flex/grid items (all, x-axis, y-axis).
    - `space-x-*`, `space-y-*` - Space between children using margin.

    What's not:
    - Arbitrary gap values beyond the spacing scale.
    - Padding-based spacing (use padding utilities instead).

    Parsing contract (`of_string`):
    - Accepts ["gap"; n], ["gap"; "x" | "y"; n], ["space"; "x" | "y"; n],
      ["-space"; "x" | "y"; n] for negative space. Unknown tokens yield `Error
      (`Msg "Not a gap utility")`. *)

open Style
open Css

(** Local gap utility type *)
type t =
  | Gap of { axis : [ `All | `X | `Y ]; value : spacing }
  | Space of { negative : bool; axis : [ `X | `Y ]; value : spacing }

(** Extensible variant for gap utilities *)
type Utility.base += Gap_util of t

(** Wrapper functions *)
let wrap x = Gap_util x

let unwrap = function Gap_util x -> Some x | _ -> None
let base x = Utility.base (wrap x)

(** Error helper *)
let err_not_utility = Error (`Msg "Not a gap utility")

(** Helpers to create Utility.t from Gap/Space *)
let gap_util axis value = base (Gap { axis; value })

let space_util negative axis value = base (Space { negative; axis; value })

(** {2 Typed Gap Utilities} *)

let gap' (s : spacing) =
  let class_name = "gap-" ^ Spacing.pp_spacing_suffix s in
  let spacing_decl, spacing_ref = Var.binding Spacing.spacing_var (Rem 0.25) in
  let len = Spacing.to_length spacing_ref s in
  let gap_value = { row_gap = Some len; column_gap = Some len } in
  match s with
  | `Rem _ -> style class_name [ spacing_decl; gap gap_value ]
  | _ -> style class_name [ gap gap_value ]

let gap_x' (s : spacing) =
  let class_name = "gap-x-" ^ Spacing.pp_spacing_suffix s in
  let spacing_decl, spacing_ref = Var.binding Spacing.spacing_var (Rem 0.25) in
  let len = Spacing.to_length spacing_ref s in
  match s with
  | `Rem _ -> style class_name [ spacing_decl; column_gap len ]
  | _ -> style class_name [ column_gap len ]

let gap_y' (s : spacing) =
  let class_name = "gap-y-" ^ Spacing.pp_spacing_suffix s in
  let spacing_decl, spacing_ref = Var.binding Spacing.spacing_var (Rem 0.25) in
  let len = Spacing.to_length spacing_ref s in
  match s with
  | `Rem _ -> style class_name [ spacing_decl; row_gap len ]
  | _ -> style class_name [ row_gap len ]

(** {2 Int-based Gap Utilities} *)

let gap n = gap_util `All (Spacing.int n)
let gap_x n = gap_util `X (Spacing.int n)
let gap_y n = gap_util `Y (Spacing.int n)

(** {2 Special Gap Values} *)

let gap_px = gap_util `All `Px
let gap_full = gap_util `All `Full
let gap_x_px = gap_util `X `Px
let gap_x_full = gap_util `X `Full
let gap_y_px = gap_util `Y `Px
let gap_y_full = gap_util `Y `Full

(** {2 Space Between Utilities} *)

let space_x' n =
  let s = Spacing.int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "space-x-" ^ Spacing.pp_spacing_suffix s in
  match s with
  | `Rem _ ->
      let decl, spacing_ref = Var.binding Spacing.spacing_var (Rem 0.25) in
      let n_units =
        int_of_float ((match s with `Rem f -> f | _ -> 0.) /. 0.25)
      in
      let len : Css.length =
        Calc
          Calc.(mul (length (Var spacing_ref)) (float (float_of_int n_units)))
      in
      style class_name (decl :: [ margin_left len ])
  | `Px -> style class_name [ margin_left (Px 1.) ]
  | `Full -> style class_name [ margin_left (Pct 100.0) ]

let space_x n =
  let s = Spacing.int n in
  let neg = n < 0 in
  space_util neg `X s

let space_y' n =
  let s = Spacing.int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "space-y-" ^ Spacing.pp_spacing_suffix s in
  match s with
  | `Rem _ ->
      let decl, spacing_ref = Var.binding Spacing.spacing_var (Rem 0.25) in
      let n_units =
        int_of_float ((match s with `Rem f -> f | _ -> 0.) /. 0.25)
      in
      let len : Css.length =
        Calc
          Calc.(mul (length (Var spacing_ref)) (float (float_of_int n_units)))
      in
      style class_name (decl :: [ margin_top len ])
  | `Px -> style class_name [ margin_top (Px 1.) ]
  | `Full -> style class_name [ margin_top (Pct 100.0) ]

let space_y n =
  let s = Spacing.int n in
  let neg = n < 0 in
  space_util neg `Y s

(** {1 Conversion Functions} *)

let spacing_value_order = function
  | `Px -> 1
  | `Full -> 10000
  | `Rem f ->
      let units = f /. 0.25 in
      int_of_float (units *. 10.)

(** Convert gap utility to style *)
let to_style = function
  | Gap { axis; value } -> (
      match axis with
      | `All -> gap' value
      | `X -> gap_x' value
      | `Y -> gap_y' value)
  | Space { negative; axis; value } -> (
      let n =
        match value with
        | `Rem f -> int_of_float (f /. 0.25)
        | `Px -> 0
        | `Full -> 0
      in
      let n = if negative then -n else n in
      match axis with `X -> space_x' n | `Y -> space_y' n)

(** Suborder for gap utilities *)
let suborder = function
  | Gap { axis; value } ->
      let axis_offset =
        match axis with `All -> 0 | `X -> 20000 | `Y -> 40000
      in
      25000 + axis_offset + spacing_value_order value
  | Space { negative; axis; value } ->
      let neg_offset = if negative then 100000 else 0 in
      let axis_offset = match axis with `X -> 0 | `Y -> 10000 in
      20000 + neg_offset + axis_offset + spacing_value_order value

(** Parse string parts to gap utility *)
let of_string parts =
  let parse_class = function
    | [ "gap"; value ] -> (
        match Parse.spacing_value ~name:"gap" value with
        | Ok f -> Ok (Gap { axis = `All; value = `Rem (f *. 0.25) })
        | Error _ ->
            if value = "px" then Ok (Gap { axis = `All; value = `Px })
            else if value = "full" then Ok (Gap { axis = `All; value = `Full })
            else err_not_utility)
    | [ "gap"; "x"; value ] -> (
        match Parse.spacing_value ~name:"gap-x" value with
        | Ok f -> Ok (Gap { axis = `X; value = `Rem (f *. 0.25) })
        | Error _ ->
            if value = "px" then Ok (Gap { axis = `X; value = `Px })
            else if value = "full" then Ok (Gap { axis = `X; value = `Full })
            else err_not_utility)
    | [ "gap"; "y"; value ] -> (
        match Parse.spacing_value ~name:"gap-y" value with
        | Ok f -> Ok (Gap { axis = `Y; value = `Rem (f *. 0.25) })
        | Error _ ->
            if value = "px" then Ok (Gap { axis = `Y; value = `Px })
            else if value = "full" then Ok (Gap { axis = `Y; value = `Full })
            else err_not_utility)
    | [ "space"; "x"; value ] -> (
        match Parse.int_pos ~name:"space-x" value with
        | Ok n ->
            Ok
              (Space
                 {
                   negative = false;
                   axis = `X;
                   value = `Rem (float_of_int n *. 0.25);
                 })
        | Error _ -> err_not_utility)
    | [ "space"; "y"; value ] -> (
        match Parse.int_pos ~name:"space-y" value with
        | Ok n ->
            Ok
              (Space
                 {
                   negative = false;
                   axis = `Y;
                   value = `Rem (float_of_int n *. 0.25);
                 })
        | Error _ -> err_not_utility)
    | [ "-space"; "x"; value ] -> (
        match Parse.int_pos ~name:"space-x" value with
        | Ok n ->
            Ok
              (Space
                 {
                   negative = true;
                   axis = `X;
                   value = `Rem (float_of_int n *. 0.25);
                 })
        | Error _ -> err_not_utility)
    | [ "-space"; "y"; value ] -> (
        match Parse.int_pos ~name:"space-y" value with
        | Ok n ->
            Ok
              (Space
                 {
                   negative = true;
                   axis = `Y;
                   value = `Rem (float_of_int n *. 0.25);
                 })
        | Error _ -> err_not_utility)
    | _ -> err_not_utility
  in
  parse_class parts

(** Priority for gap utilities *)
let priority = 16

(** Typed handler for gap utilities *)
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
