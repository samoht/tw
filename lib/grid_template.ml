(** Grid template utilities for grid layout configuration

    What's included:
    - `grid-cols-*` - Grid template columns (1-12, none, subgrid).
    - `grid-rows-*` - Grid template rows (1-6, none, subgrid).
    - `grid-flow-*` - Grid auto flow direction and density.
    - `auto-cols-*`, `auto-rows-*` - Auto sizing for implicit tracks.
    - Arbitrary track lists accepted by the corresponding CSS property grammar.

    What's not:
    - Named grid areas.

    Parsing contract (`of_string`):
    - Accepts ["grid"; "cols" | "rows"; n], ["grid"; "flow"; ...],
      ["auto"; "cols" | "rows"; ...]. Unknown tokens yield `Error (`Msg "Not a
      grid template utility")`. *)

module Css = Cascade.Css

module Handler = struct
  open Style

  (* Bind tw's [Pp.float : float -> string] before [open Css] shadows [Pp] with
     cascade's context-based [Css.Pp]. *)
  let pp_float = Pp.float

  open Css

  let err_not_utility = Error (`Msg "Not a grid template utility")
  let err_invalid_cols = Error (`Msg "Invalid grid-cols value")
  let err_invalid_rows = Error (`Msg "Invalid grid-rows value")

  (* The four arbitrary forms carry the parse beside the text an author wrote:
     the text spells the class name back and the parse renders it. Keeping only
     the text left [to_style] re-parsing on every render, with nothing but a
     comment to say it could not fail. *)
  type t =
    | Grid_cols of int
    | Grid_cols_none
    | Grid_cols_subgrid
    | Grid_cols_arbitrary of string * Css.grid_template
    | Grid_rows of int
    | Grid_rows_none
    | Grid_rows_subgrid
    | Grid_rows_arbitrary of string * Css.grid_template
    | Grid_flow_row
    | Grid_flow_col
    | Grid_flow_dense
    | Grid_flow_row_dense
    | Grid_flow_col_dense
    | Auto_cols_auto
    | Auto_cols_min
    | Auto_cols_max
    | Auto_cols_fr
    | Auto_cols_spacing of float  (** [auto-cols-<n>]: spacing-scaled track. *)
    | Auto_cols_arbitrary of string * Css.grid_template
    | Auto_rows_auto
    | Auto_rows_min
    | Auto_rows_max
    | Auto_rows_fr
    | Auto_rows_spacing of float  (** [auto-rows-<n>]: spacing-scaled track. *)
    | Auto_rows_arbitrary of string * Css.grid_template

  let name = "grid_template"

  (* Before flex_props (16) and alignment/gap (17) *)
  let priority _ = 15

  let grid_cols n =
    if n < 1 || n > 999 then
      invalid_arg
        (String.concat ""
           [ "grid_cols: "; string_of_int n; " is out of range (1-999)" ])
    else
      style
        [
          Css.grid_template_columns
            (Repeat (Count n, [ Min_max (Zero, Fr 1.0) ]));
        ]

  let themed_property var_name property value =
    let theme_decl =
      Css.custom_property ~layer:"theme" ("--" ^ var_name) value
    in
    let ref : Css.grid_template Css.var = Var.bracket var_name in
    let value : Css.grid_template = Css.Var ref in
    let prop_decl = property value in
    style [ theme_decl; prop_decl ]

  let grid_cols_none ?theme () =
    let var_name = "grid-template-columns-none" in
    match Scheme.theme_value theme var_name with
    | Some value -> themed_property var_name Css.grid_template_columns value
    | None -> style [ Css.grid_template_columns None ]

  let grid_cols_subgrid = style [ Css.grid_template_columns Subgrid ]

  (* Parse the complete property grammar in Cascade. Keeping a second grid
     parser here made tw reject valid CSS as soon as Grid gained flex-valued
     math functions, and also made template and auto-track validation drift. *)
  let parse_arbitrary_grid_template read s : Css.grid_template option =
    let decoded = Parse.decode_arbitrary_value (String.trim s) in
    let cursor = Cascade.Cursor.of_string decoded in
    match Cascade.Cursor.try_parse_full_err read cursor with
    | Ok template -> Some template
    | Error _ -> (
        (* Tailwind's upstream fixture includes the invalid CSS value [[123]].
           Preserve tw's established interpretation as [123px] without weakening
           Cascade's property grammar for every other value. *)
        match int_of_string_opt decoded with
        | Some n -> Some (Css.Px (float_of_int n))
        | None -> None)

  (* A track written with the [--spacing()] shorthand reads the scale, so the
     token has to be declared alongside it. *)
  let spacing_decls s =
    let has_spacing_fn =
      let n = String.length "--spacing(" in
      let rec at i =
        i + n <= String.length s
        && (String.sub s i n = "--spacing(" || at (i + 1))
      in
      at 0
    in
    if has_spacing_fn then
      let decl, _ = Var.binding Theme.spacing_var Theme.spacing_base in
      [ decl ]
    else []

  let grid_cols_arbitrary s template =
    style (spacing_decls s @ [ Css.grid_template_columns template ])

  let grid_rows_arbitrary s template =
    style (spacing_decls s @ [ Css.grid_template_rows template ])

  let grid_rows n =
    if n < 1 || n > 999 then
      invalid_arg
        (String.concat ""
           [ "grid_rows: "; string_of_int n; " is out of range (1-999)" ])
    else
      style
        [
          Css.grid_template_rows (Repeat (Count n, [ Min_max (Zero, Fr 1.0) ]));
        ]

  let grid_rows_none ?theme () =
    let var_name = "grid-template-rows-none" in
    match Scheme.theme_value theme var_name with
    | Some value -> themed_property var_name Css.grid_template_rows value
    | None -> style [ Css.grid_template_rows None ]

  let grid_rows_subgrid = style [ Css.grid_template_rows Subgrid ]
  let grid_flow_row = style [ Css.grid_auto_flow Row ]
  let grid_flow_col = style [ Css.grid_auto_flow Column ]
  let grid_flow_dense = style [ Css.grid_auto_flow Dense ]
  let grid_flow_row_dense = style [ Css.grid_auto_flow Row_dense ]
  let grid_flow_col_dense = style [ Css.grid_auto_flow Column_dense ]

  let auto_cols_auto ?theme () =
    let var_name = "grid-auto-columns-auto" in
    match Scheme.theme_value theme var_name with
    | Some value -> themed_property var_name Css.grid_auto_columns value
    | None -> style [ Css.grid_auto_columns Auto ]

  let auto_cols_min = style [ Css.grid_auto_columns Min_content ]
  let auto_cols_max = style [ Css.grid_auto_columns Max_content ]
  let auto_cols_fr = style [ Css.grid_auto_columns (Min_max (Zero, Fr 1.0)) ]
  let auto_cols_arbitrary template = style [ Css.grid_auto_columns template ]

  (* [auto-cols-<n>] sizes implicit columns to a spacing-scaled track,
     [grid-auto-columns: calc(var(--spacing) * n)]. *)
  let auto_cols_spacing ?theme n =
    let decl, len = Theme.spacing_calc_float ?theme n in
    style [ decl; Css.grid_auto_columns (Css.Length len) ]

  (** {1 Grid Auto Rows} *)

  let auto_rows_auto ?theme () =
    let var_name = "grid-auto-rows-auto" in
    match Scheme.theme_value theme var_name with
    | Some value -> themed_property var_name Css.grid_auto_rows value
    | None -> style [ Css.grid_auto_rows Auto ]

  let auto_rows_min = style [ Css.grid_auto_rows Min_content ]
  let auto_rows_max = style [ Css.grid_auto_rows Max_content ]
  let auto_rows_fr = style [ Css.grid_auto_rows (Min_max (Zero, Fr 1.0)) ]
  let auto_rows_arbitrary template = style [ Css.grid_auto_rows template ]

  let auto_rows_spacing ?theme n =
    let decl, len = Theme.spacing_calc_float ?theme n in
    style [ decl; Css.grid_auto_rows (Css.Length len) ]

  (** Convert grid template utility to style *)
  let to_style theme =
    let auto_cols_spacing n = auto_cols_spacing ~theme n in
    let auto_rows_spacing n = auto_rows_spacing ~theme n in
    let grid_cols_none () = grid_cols_none ~theme () in
    let grid_rows_none () = grid_rows_none ~theme () in
    let auto_cols_auto () = auto_cols_auto ~theme () in
    let auto_rows_auto () = auto_rows_auto ~theme () in
    function
    | Grid_cols n -> grid_cols n
    | Grid_cols_none -> grid_cols_none ()
    | Grid_cols_subgrid -> grid_cols_subgrid
    | Grid_cols_arbitrary (s, template) -> grid_cols_arbitrary s template
    | Grid_rows n -> grid_rows n
    | Grid_rows_none -> grid_rows_none ()
    | Grid_rows_subgrid -> grid_rows_subgrid
    | Grid_rows_arbitrary (s, template) -> grid_rows_arbitrary s template
    | Grid_flow_row -> grid_flow_row
    | Grid_flow_col -> grid_flow_col
    | Grid_flow_dense -> grid_flow_dense
    | Grid_flow_row_dense -> grid_flow_row_dense
    | Grid_flow_col_dense -> grid_flow_col_dense
    | Auto_cols_auto -> auto_cols_auto ()
    | Auto_cols_min -> auto_cols_min
    | Auto_cols_max -> auto_cols_max
    | Auto_cols_fr -> auto_cols_fr
    | Auto_cols_spacing n -> auto_cols_spacing n
    | Auto_cols_arbitrary (_, template) -> auto_cols_arbitrary template
    | Auto_rows_auto -> auto_rows_auto ()
    | Auto_rows_min -> auto_rows_min
    | Auto_rows_max -> auto_rows_max
    | Auto_rows_fr -> auto_rows_fr
    | Auto_rows_spacing n -> auto_rows_spacing n
    | Auto_rows_arbitrary (_, template) -> auto_rows_arbitrary template

  (* Tailwind emits these five families in the alphabetical order of the CSS
     property each declares: grid-auto-columns, grid-auto-flow, grid-auto-rows,
     grid-template-columns, grid-template-rows. The bands below follow that, not
     the template-before-auto reading they used to have, which put
     [auto-rows-min] after [grid-cols-3] where Tailwind puts it before. *)
  let suborder = function
    (* Grid auto columns (10000-10099) *)
    (* Order: spacing (numeric) → arbitrary → keywords alphabetical *)
    | Auto_cols_spacing _ -> 10000
    | Auto_cols_arbitrary _ -> 10001
    | Auto_cols_auto -> 10002
    | Auto_cols_fr -> 10003
    | Auto_cols_max -> 10004
    | Auto_cols_min -> 10005
    (* Grid auto flow (10100-10199) - alphabetical order *)
    | Grid_flow_col -> 10100
    | Grid_flow_col_dense -> 10101
    | Grid_flow_dense -> 10102
    | Grid_flow_row -> 10103
    | Grid_flow_row_dense -> 10104
    (* Grid auto rows (10200-10299) *)
    | Auto_rows_spacing _ -> 10200
    | Auto_rows_arbitrary _ -> 10201
    | Auto_rows_auto -> 10202
    | Auto_rows_fr -> 10203
    | Auto_rows_max -> 10204
    | Auto_rows_min -> 10205
    (* Grid template columns (11000-11999) *)
    (* Order: numeric → arbitrary → keywords alphabetical *)
    | Grid_cols n -> 11000 + n
    | Grid_cols_arbitrary _ -> 11800
    | Grid_cols_none -> 11900
    | Grid_cols_subgrid -> 11901
    (* Grid template rows (12000-12999) *)
    | Grid_rows n -> 12000 + n
    | Grid_rows_arbitrary _ -> 12800
    | Grid_rows_none -> 12900
    | Grid_rows_subgrid -> 12901

  let of_class _theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "grid"; "cols"; "none" ] -> Ok Grid_cols_none
    | [ "grid"; "cols"; "subgrid" ] -> Ok Grid_cols_subgrid
    | [ "grid"; "cols"; n ] -> (
        let len = String.length n in
        if len > 2 && n.[0] = '[' && n.[len - 1] = ']' then
          let inner = String.sub n 1 (len - 2) in
          match
            parse_arbitrary_grid_template
              Css.Properties.read_grid_template_tracks inner
          with
          | Some template -> Ok (Grid_cols_arbitrary (inner, template))
          | None -> err_invalid_cols
        else
          match Parse.decimal_int n with
          | Some i when i >= 1 && i <= 999 -> Ok (Grid_cols i)
          | Some _ | None -> err_invalid_cols)
    | [ "grid"; "rows"; "none" ] -> Ok Grid_rows_none
    | [ "grid"; "rows"; "subgrid" ] -> Ok Grid_rows_subgrid
    | [ "grid"; "rows"; n ] -> (
        let len = String.length n in
        if len > 2 && n.[0] = '[' && n.[len - 1] = ']' then
          let inner = String.sub n 1 (len - 2) in
          match
            parse_arbitrary_grid_template
              Css.Properties.read_grid_template_tracks inner
          with
          | Some template -> Ok (Grid_rows_arbitrary (inner, template))
          | None -> err_invalid_rows
        else
          match Parse.decimal_int n with
          | Some i when i >= 1 && i <= 999 -> Ok (Grid_rows i)
          | Some _ | None -> err_invalid_rows)
    | [ "grid"; "flow"; "row" ] -> Ok Grid_flow_row
    | [ "grid"; "flow"; "col" ] -> Ok Grid_flow_col
    | [ "grid"; "flow"; "dense" ] -> Ok Grid_flow_dense
    | [ "grid"; "flow"; "row"; "dense" ] -> Ok Grid_flow_row_dense
    | [ "grid"; "flow"; "col"; "dense" ] -> Ok Grid_flow_col_dense
    | [ "auto"; "cols"; "auto" ] -> Ok Auto_cols_auto
    | [ "auto"; "cols"; "min" ] -> Ok Auto_cols_min
    | [ "auto"; "cols"; "max" ] -> Ok Auto_cols_max
    | [ "auto"; "cols"; "fr" ] -> Ok Auto_cols_fr
    | [ "auto"; "cols"; n ] -> (
        match float_of_string_opt n with
        | Some f when f >= 0.0 -> Ok (Auto_cols_spacing f)
        | _ ->
            let len = String.length n in
            if len > 2 && n.[0] = '[' && n.[len - 1] = ']' then
              let inner = String.sub n 1 (len - 2) in
              match
                parse_arbitrary_grid_template
                  Css.Properties.read_grid_auto_tracks inner
              with
              | Some template -> Ok (Auto_cols_arbitrary (inner, template))
              | None -> err_not_utility
            else err_not_utility)
    | [ "auto"; "rows"; "auto" ] -> Ok Auto_rows_auto
    | [ "auto"; "rows"; "min" ] -> Ok Auto_rows_min
    | [ "auto"; "rows"; "max" ] -> Ok Auto_rows_max
    | [ "auto"; "rows"; "fr" ] -> Ok Auto_rows_fr
    | [ "auto"; "rows"; n ] -> (
        match float_of_string_opt n with
        | Some f when f >= 0.0 -> Ok (Auto_rows_spacing f)
        | _ ->
            let len = String.length n in
            if len > 2 && n.[0] = '[' && n.[len - 1] = ']' then
              let inner = String.sub n 1 (len - 2) in
              match
                parse_arbitrary_grid_template
                  Css.Properties.read_grid_auto_tracks inner
              with
              | Some template -> Ok (Auto_rows_arbitrary (inner, template))
              | None -> err_not_utility
            else err_not_utility)
    | _ -> err_not_utility

  let to_class = function
    | Grid_cols n -> "grid-cols-" ^ string_of_int n
    | Grid_cols_none -> "grid-cols-none"
    | Grid_cols_subgrid -> "grid-cols-subgrid"
    | Grid_cols_arbitrary (s, _) -> "grid-cols-[" ^ s ^ "]"
    | Grid_rows n -> "grid-rows-" ^ string_of_int n
    | Grid_rows_none -> "grid-rows-none"
    | Grid_rows_subgrid -> "grid-rows-subgrid"
    | Grid_rows_arbitrary (s, _) -> "grid-rows-[" ^ s ^ "]"
    | Grid_flow_row -> "grid-flow-row"
    | Grid_flow_col -> "grid-flow-col"
    | Grid_flow_dense -> "grid-flow-dense"
    | Grid_flow_row_dense -> "grid-flow-row-dense"
    | Grid_flow_col_dense -> "grid-flow-col-dense"
    | Auto_cols_auto -> "auto-cols-auto"
    | Auto_cols_min -> "auto-cols-min"
    | Auto_cols_max -> "auto-cols-max"
    | Auto_cols_fr -> "auto-cols-fr"
    | Auto_cols_spacing n -> "auto-cols-" ^ pp_float n
    | Auto_cols_arbitrary (s, _) -> "auto-cols-[" ^ s ^ "]"
    | Auto_rows_auto -> "auto-rows-auto"
    | Auto_rows_min -> "auto-rows-min"
    | Auto_rows_max -> "auto-rows-max"
    | Auto_rows_fr -> "auto-rows-fr"
    | Auto_rows_spacing n -> "auto-rows-" ^ pp_float n
    | Auto_rows_arbitrary (s, _) -> "auto-rows-[" ^ s ^ "]"

  let examples =
    [
      Grid_cols_none;
      Grid_rows_none;
      Grid_flow_row;
      Auto_cols_auto;
      Auto_rows_auto;
    ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
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
