(** Gap and space-between utilities *)

open Style
open Css

let spacing_var = Theme.spacing_var

let pp_spacing_suffix : spacing -> string = function
  | `Px -> "px"
  | `Full -> "full"
  | `Rem f ->
      let scale = f /. 0.25 in
      if Float.is_integer scale then string_of_int (abs (int_of_float scale))
      else
        let abs_scale = Float.abs scale in
        let s = string_of_float abs_scale in
        if String.contains s '.' then
          let len = String.length s in
          let rec find_end i =
            if i < 0 then 0
            else if s.[i] = '0' then find_end (i - 1)
            else if s.[i] = '.' then i
            else i + 1
          in
          String.sub s 0 (find_end (len - 1))
        else s

let to_length spacing_ref : spacing -> length = function
  | `Px -> Px 1.
  | `Full -> Pct 100.0
  | `Rem f ->
      let n = int_of_float (f /. 0.25) in
      Calc
        (Calc.mul (Calc.length (Var spacing_ref)) (Calc.float (float_of_int n)))

let int n = `Rem (float_of_int n *. 0.25)

type utility =
  | Gap of [ `All | `X | `Y ] * spacing
  | Space of bool (* negative *) * [ `X | `Y ] * spacing

(** {2 Typed Gap Utilities} *)

let gap' (s : spacing) =
  let class_name = "gap-" ^ pp_spacing_suffix s in
  let spacing_decl, spacing_ref = Var.binding spacing_var (Rem 0.25) in
  let len = to_length spacing_ref s in
  let gap_value = { row_gap = Some len; column_gap = Some len } in
  match s with
  | `Rem _ -> style class_name [ spacing_decl; gap gap_value ]
  | _ -> style class_name [ gap gap_value ]

let gap_x' (s : spacing) =
  let class_name = "gap-x-" ^ pp_spacing_suffix s in
  let spacing_decl, spacing_ref = Var.binding spacing_var (Rem 0.25) in
  let len = to_length spacing_ref s in
  match s with
  | `Rem _ -> style class_name [ spacing_decl; column_gap len ]
  | _ -> style class_name [ column_gap len ]

let gap_y' (s : spacing) =
  let class_name = "gap-y-" ^ pp_spacing_suffix s in
  let spacing_decl, spacing_ref = Var.binding spacing_var (Rem 0.25) in
  let len = to_length spacing_ref s in
  match s with
  | `Rem _ -> style class_name [ spacing_decl; row_gap len ]
  | _ -> style class_name [ row_gap len ]

(** {2 Int-based Gap Utilities} *)

let gap n = gap' (int n)
let gap_x n = gap_x' (int n)
let gap_y n = gap_y' (int n)

(** {2 Special Gap Values} *)

let gap_px = gap' `Px
let gap_full = gap' `Full
let gap_x_px = gap_x' `Px
let gap_x_full = gap_x' `Full
let gap_y_px = gap_y' `Px
let gap_y_full = gap_y' `Full

(** {2 Space Between Utilities} *)

let space_x n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "space-x-" ^ pp_spacing_suffix s in
  match s with
  | `Rem _ ->
      let decl, spacing_ref = Var.binding spacing_var (Rem 0.25) in
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

let space_y n =
  let s = int n in
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "space-y-" ^ pp_spacing_suffix s in
  match s with
  | `Rem _ ->
      let decl, spacing_ref = Var.binding spacing_var (Rem 0.25) in
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

(** {1 Conversion Functions} *)

let to_style = function
  | Gap (axis, value) -> (
      match axis with
      | `All -> gap' value
      | `X -> gap_x' value
      | `Y -> gap_y' value)
  | Space (neg, axis, value) -> (
      let n =
        match value with
        | `Rem f -> int_of_float (f /. 0.25)
        | `Px -> 0
        | `Full -> 0
      in
      let n = if neg then -n else n in
      match axis with `X -> space_x n | `Y -> space_y n)

let spacing_value_order = function
  | `Px -> 1
  | `Full -> 10000
  | `Rem f ->
      let units = f /. 0.25 in
      int_of_float (units *. 10.)

let of_string parts =
  let parse_class = function
    | [ "gap"; value ] -> (
        match Parse.spacing_value ~name:"gap" value with
        | Ok f -> Some (Gap (`All, `Rem (f *. 0.25)))
        | Error _ ->
            if value = "px" then Some (Gap (`All, `Px))
            else if value = "full" then Some (Gap (`All, `Full))
            else None)
    | [ "gap"; "x"; value ] -> (
        match Parse.spacing_value ~name:"gap-x" value with
        | Ok f -> Some (Gap (`X, `Rem (f *. 0.25)))
        | Error _ ->
            if value = "px" then Some (Gap (`X, `Px))
            else if value = "full" then Some (Gap (`X, `Full))
            else None)
    | [ "gap"; "y"; value ] -> (
        match Parse.spacing_value ~name:"gap-y" value with
        | Ok f -> Some (Gap (`Y, `Rem (f *. 0.25)))
        | Error _ ->
            if value = "px" then Some (Gap (`Y, `Px))
            else if value = "full" then Some (Gap (`Y, `Full))
            else None)
    | [ "space"; "x"; value ] -> (
        match Parse.int_pos ~name:"space-x" value with
        | Ok n -> Some (Space (false, `X, `Rem (float_of_int n *. 0.25)))
        | Error _ -> None)
    | [ "space"; "y"; value ] -> (
        match Parse.int_pos ~name:"space-y" value with
        | Ok n -> Some (Space (false, `Y, `Rem (float_of_int n *. 0.25)))
        | Error _ -> None)
    | [ "-space"; "x"; value ] -> (
        match Parse.int_pos ~name:"space-x" value with
        | Ok n -> Some (Space (true, `X, `Rem (float_of_int n *. 0.25)))
        | Error _ -> None)
    | [ "-space"; "y"; value ] -> (
        match Parse.int_pos ~name:"space-y" value with
        | Ok n -> Some (Space (true, `Y, `Rem (float_of_int n *. 0.25)))
        | Error _ -> None)
    | _ -> None
  in
  match parse_class parts with
  | Some u -> Ok u
  | None -> Error (`Msg "Not a gap utility")

let suborder = function
  | Gap (axis, value) ->
      let axis_offset =
        match axis with `All -> 0 | `X -> 20000 | `Y -> 40000
      in
      25000 + axis_offset + spacing_value_order value
  | Space (neg, axis, value) ->
      let neg_offset = if neg then 100000 else 0 in
      let axis_offset = match axis with `X -> 0 | `Y -> 10000 in
      20000 + neg_offset + axis_offset + spacing_value_order value
