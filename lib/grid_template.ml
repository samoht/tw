(** Grid template utilities for grid layout configuration

    What's included:
    - `grid-cols-*` - Grid template columns (1-12, none, subgrid).
    - `grid-rows-*` - Grid template rows (1-6, none, subgrid).
    - `grid-flow-*` - Grid auto flow direction and density.
    - `auto-cols-*`, `auto-rows-*` - Auto sizing for implicit tracks.

    What's not:
    - Arbitrary grid templates beyond the provided scale.
    - Named grid lines or areas.

    Parsing contract (`of_string`):
    - Accepts ["grid"; "cols" | "rows"; n], ["grid"; "flow"; ...],
      ["auto"; "cols" | "rows"; ...]. Unknown tokens yield `Error (`Msg "Not a
      grid template utility")`. *)

module Handler = struct
  open Style
  open Css

  let err_not_utility = Error (`Msg "Not a grid template utility")
  let err_invalid_cols = Error (`Msg "Invalid grid-cols value")
  let err_invalid_rows = Error (`Msg "Invalid grid-rows value")

  type t =
    | Grid_cols of int
    | Grid_cols_none
    | Grid_cols_subgrid
    | Grid_cols_arbitrary of string
    | Grid_rows of int
    | Grid_rows_none
    | Grid_rows_subgrid
    | Grid_rows_arbitrary of string
    | Grid_flow_row
    | Grid_flow_col
    | Grid_flow_dense
    | Grid_flow_row_dense
    | Grid_flow_col_dense
    | Auto_cols_auto
    | Auto_cols_min
    | Auto_cols_max
    | Auto_cols_fr
    | Auto_rows_auto
    | Auto_rows_min
    | Auto_rows_max
    | Auto_rows_fr

  type Utility.base += Self of t

  let name = "grid_template"

  (* Before flex_props (16) and alignment/gap (17) *)
  let priority = 15

  let grid_cols n =
    if n < 1 || n > 999 then
      invalid_arg
        (String.concat ""
           [ "grid_cols: "; string_of_int n; " is out of range (1-999)" ])
    else
      style
        [ Css.grid_template_columns (Repeat (n, [ Min_max (Zero, Fr 1.0) ])) ]

  let grid_cols_none = style [ Css.grid_template_columns None ]
  let grid_cols_subgrid = style [ Css.grid_template_columns Subgrid ]

  let parse_arbitrary_length s : Css.length option =
    let value = String.trim s in
    let len = String.length value in
    if len = 0 then None
    else if len >= 2 && String.sub value (len - 2) 2 = "px" then
      match float_of_string_opt (String.sub value 0 (len - 2)) with
      | Some n -> Some (Px n)
      | None -> None
    else if len >= 3 && String.sub value (len - 3) 3 = "rem" then
      match float_of_string_opt (String.sub value 0 (len - 3)) with
      | Some n -> Some (Rem n)
      | None -> None
    else if len >= 2 && String.sub value (len - 2) 2 = "em" then
      match float_of_string_opt (String.sub value 0 (len - 2)) with
      | Some n -> Some (Em n)
      | None -> None
    else if len >= 2 && String.sub value (len - 2) 2 = "fr" then
      (* Handle fr values *)
      None (* fr needs special handling as grid_template type *)
    else
      match int_of_string_opt value with
      | Some n -> Some (Px (float_of_int n))
      | None -> None

  let parse_arbitrary_grid_template s : Css.grid_template =
    let value = String.trim s in
    (* First try to parse as a simple length *)
    match parse_arbitrary_length value with
    | Some len ->
        Px (match len with Px n -> n | Rem n -> n *. 16.0 | _ -> 0.0)
    | None ->
        (* Try to parse fr values *)
        let len = String.length value in
        if len >= 2 && String.sub value (len - 2) 2 = "fr" then
          match float_of_string_opt (String.sub value 0 (len - 2)) with
          | Some n -> Fr n
          | None -> Auto
        else Auto

  let grid_cols_arbitrary s =
    let template = parse_arbitrary_grid_template s in
    style [ Css.grid_template_columns template ]

  let grid_rows_arbitrary s =
    let template = parse_arbitrary_grid_template s in
    style [ Css.grid_template_rows template ]

  let grid_rows n =
    if n < 1 || n > 999 then
      invalid_arg
        (String.concat ""
           [ "grid_rows: "; string_of_int n; " is out of range (1-999)" ])
    else
      style [ Css.grid_template_rows (Repeat (n, [ Min_max (Zero, Fr 1.0) ])) ]

  let grid_rows_none = style [ Css.grid_template_rows None ]
  let grid_rows_subgrid = style [ Css.grid_template_rows Subgrid ]
  let grid_flow_row = style [ Css.grid_auto_flow Row ]
  let grid_flow_col = style [ Css.grid_auto_flow Column ]
  let grid_flow_dense = style [ Css.grid_auto_flow Dense ]
  let grid_flow_row_dense = style [ Css.grid_auto_flow Row_dense ]
  let grid_flow_col_dense = style [ Css.grid_auto_flow Column_dense ]
  let auto_cols_auto = style [ Css.grid_auto_columns Auto ]
  let auto_cols_min = style [ Css.grid_auto_columns Min_content ]
  let auto_cols_max = style [ Css.grid_auto_columns Max_content ]
  let auto_cols_fr = style [ Css.grid_auto_columns (Min_max (Zero, Fr 1.0)) ]

  (** {1 Grid Auto Rows} *)

  let auto_rows_auto = style [ Css.grid_auto_rows Auto ]
  let auto_rows_min = style [ Css.grid_auto_rows Min_content ]
  let auto_rows_max = style [ Css.grid_auto_rows Max_content ]
  let auto_rows_fr = style [ Css.grid_auto_rows (Min_max (Zero, Fr 1.0)) ]

  (** Convert grid template utility to style *)
  let to_style = function
    | Grid_cols n -> grid_cols n
    | Grid_cols_none -> grid_cols_none
    | Grid_cols_subgrid -> grid_cols_subgrid
    | Grid_cols_arbitrary s -> grid_cols_arbitrary s
    | Grid_rows n -> grid_rows n
    | Grid_rows_none -> grid_rows_none
    | Grid_rows_subgrid -> grid_rows_subgrid
    | Grid_rows_arbitrary s -> grid_rows_arbitrary s
    | Grid_flow_row -> grid_flow_row
    | Grid_flow_col -> grid_flow_col
    | Grid_flow_dense -> grid_flow_dense
    | Grid_flow_row_dense -> grid_flow_row_dense
    | Grid_flow_col_dense -> grid_flow_col_dense
    | Auto_cols_auto -> auto_cols_auto
    | Auto_cols_min -> auto_cols_min
    | Auto_cols_max -> auto_cols_max
    | Auto_cols_fr -> auto_cols_fr
    | Auto_rows_auto -> auto_rows_auto
    | Auto_rows_min -> auto_rows_min
    | Auto_rows_max -> auto_rows_max
    | Auto_rows_fr -> auto_rows_fr

  let suborder = function
    (* Grid template columns (10000-10999) *)
    | Grid_cols n -> 10000 + n
    | Grid_cols_none -> 10900
    | Grid_cols_subgrid -> 10901
    | Grid_cols_arbitrary _ -> 10902
    (* Grid template rows (11000-11999) *)
    | Grid_rows n -> 11000 + n
    | Grid_rows_none -> 11900
    | Grid_rows_subgrid -> 11901
    | Grid_rows_arbitrary _ -> 11902
    (* Grid auto flow (14000-14099) - alphabetical order *)
    | Grid_flow_col -> 14000
    | Grid_flow_col_dense -> 14001
    | Grid_flow_dense -> 14002
    | Grid_flow_row -> 14003
    | Grid_flow_row_dense -> 14004
    (* Grid auto columns (15000-15099) *)
    | Auto_cols_auto -> 15000
    | Auto_cols_min -> 15001
    | Auto_cols_max -> 15002
    | Auto_cols_fr -> 15003
    (* Grid auto rows (15100-15199) *)
    | Auto_rows_auto -> 15100
    | Auto_rows_min -> 15101
    | Auto_rows_max -> 15102
    | Auto_rows_fr -> 15103

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "grid"; "cols"; "none" ] -> Ok Grid_cols_none
    | [ "grid"; "cols"; "subgrid" ] -> Ok Grid_cols_subgrid
    | [ "grid"; "cols"; n ] -> (
        let len = String.length n in
        if len > 2 && n.[0] = '[' && n.[len - 1] = ']' then
          let inner = String.sub n 1 (len - 2) in
          Ok (Grid_cols_arbitrary inner)
        else
          match int_of_string_opt n with
          | Some i -> Ok (Grid_cols i)
          | None -> err_invalid_cols)
    | [ "grid"; "rows"; "none" ] -> Ok Grid_rows_none
    | [ "grid"; "rows"; "subgrid" ] -> Ok Grid_rows_subgrid
    | [ "grid"; "rows"; n ] -> (
        let len = String.length n in
        if len > 2 && n.[0] = '[' && n.[len - 1] = ']' then
          let inner = String.sub n 1 (len - 2) in
          Ok (Grid_rows_arbitrary inner)
        else
          match int_of_string_opt n with
          | Some i -> Ok (Grid_rows i)
          | None -> err_invalid_rows)
    | [ "grid"; "flow"; "row" ] -> Ok Grid_flow_row
    | [ "grid"; "flow"; "col" ] -> Ok Grid_flow_col
    | [ "grid"; "flow"; "dense" ] -> Ok Grid_flow_dense
    | [ "grid"; "flow"; "row"; "dense" ] -> Ok Grid_flow_row_dense
    | [ "grid"; "flow"; "col"; "dense" ] -> Ok Grid_flow_col_dense
    | [ "auto"; "cols"; "auto" ] -> Ok Auto_cols_auto
    | [ "auto"; "cols"; "min" ] -> Ok Auto_cols_min
    | [ "auto"; "cols"; "max" ] -> Ok Auto_cols_max
    | [ "auto"; "cols"; "fr" ] -> Ok Auto_cols_fr
    | [ "auto"; "rows"; "auto" ] -> Ok Auto_rows_auto
    | [ "auto"; "rows"; "min" ] -> Ok Auto_rows_min
    | [ "auto"; "rows"; "max" ] -> Ok Auto_rows_max
    | [ "auto"; "rows"; "fr" ] -> Ok Auto_rows_fr
    | _ -> err_not_utility

  let to_class = function
    | Grid_cols n -> "grid-cols-" ^ string_of_int n
    | Grid_cols_none -> "grid-cols-none"
    | Grid_cols_subgrid -> "grid-cols-subgrid"
    | Grid_cols_arbitrary s -> "grid-cols-[" ^ s ^ "]"
    | Grid_rows n -> "grid-rows-" ^ string_of_int n
    | Grid_rows_none -> "grid-rows-none"
    | Grid_rows_subgrid -> "grid-rows-subgrid"
    | Grid_rows_arbitrary s -> "grid-rows-[" ^ s ^ "]"
    | Grid_flow_row -> "grid-flow-row"
    | Grid_flow_col -> "grid-flow-col"
    | Grid_flow_dense -> "grid-flow-dense"
    | Grid_flow_row_dense -> "grid-flow-row-dense"
    | Grid_flow_col_dense -> "grid-flow-col-dense"
    | Auto_cols_auto -> "auto-cols-auto"
    | Auto_cols_min -> "auto-cols-min"
    | Auto_cols_max -> "auto-cols-max"
    | Auto_cols_fr -> "auto-cols-fr"
    | Auto_rows_auto -> "auto-rows-auto"
    | Auto_rows_min -> "auto-rows-min"
    | Auto_rows_max -> "auto-rows-max"
    | Auto_rows_fr -> "auto-rows-fr"
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let grid_cols n = utility (Grid_cols n)
let grid_cols_none = utility Grid_cols_none
let grid_cols_subgrid = utility Grid_cols_subgrid
let grid_rows n = utility (Grid_rows n)
let grid_rows_none = utility Grid_rows_none
let grid_rows_subgrid = utility Grid_rows_subgrid
let grid_flow_row = utility Grid_flow_row
let grid_flow_col = utility Grid_flow_col
let grid_flow_dense = utility Grid_flow_dense
let grid_flow_row_dense = utility Grid_flow_row_dense
let grid_flow_col_dense = utility Grid_flow_col_dense
let auto_cols_auto = utility Auto_cols_auto
let auto_cols_min = utility Auto_cols_min
let auto_cols_max = utility Auto_cols_max
let auto_cols_fr = utility Auto_cols_fr
let auto_rows_auto = utility Auto_rows_auto
let auto_rows_min = utility Auto_rows_min
let auto_rows_max = utility Auto_rows_max
let auto_rows_fr = utility Auto_rows_fr
