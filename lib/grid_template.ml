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
    | Auto_cols_spacing of float  (** [auto-cols-<n>]: spacing-scaled track. *)
    | Auto_cols_arbitrary of string
    | Auto_rows_auto
    | Auto_rows_min
    | Auto_rows_max
    | Auto_rows_fr
    | Auto_rows_spacing of float  (** [auto-rows-<n>]: spacing-scaled track. *)
    | Auto_rows_arbitrary of string

  type Utility.base += Self of t

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
    else if len >= 1 && value.[len - 1] = '%' then
      match float_of_string_opt (String.sub value 0 (len - 1)) with
      | Some n -> Some (Pct n)
      | None -> None
    else if len >= 2 && String.sub value (len - 2) 2 = "fr" then
      (* Handle fr values *)
      None (* fr needs special handling as grid_template type *)
    else
      match int_of_string_opt value with
      | Some n -> Some (Px (float_of_int n))
      | None -> None

  let rec all_some = function
    | [] -> Some []
    | x :: xs -> (
        match (x, all_some xs) with
        | Some v, Some vs -> Some (v :: vs)
        | _ -> None)

  (* Split [s] on [sep] only at the top nesting level, so separators inside
     repeat()/minmax()/fit-content() are preserved. *)
  let split_top_level sep s =
    let len = String.length s in
    let buf = Buffer.create len in
    let rec loop i depth acc =
      if i >= len then List.rev (Buffer.contents buf :: acc)
      else
        let c = s.[i] in
        if c = '(' then (
          Buffer.add_char buf c;
          loop (i + 1) (depth + 1) acc)
        else if c = ')' then (
          Buffer.add_char buf c;
          loop (i + 1) (max 0 (depth - 1)) acc)
        else if c = sep && depth = 0 then (
          let part = Buffer.contents buf in
          Buffer.clear buf;
          loop (i + 1) depth (part :: acc))
        else (
          Buffer.add_char buf c;
          loop (i + 1) depth acc)
    in
    loop 0 0 []

  (* If [value] is [name(...)], return the inner argument string. *)
  let fn_args name value =
    let nl = String.length name and vl = String.length value in
    if
      vl > nl + 1
      && String.sub value 0 nl = name
      && value.[nl] = '('
      && value.[vl - 1] = ')'
    then Some (String.sub value (nl + 1) (vl - nl - 2))
    else None

  let parse_track_keyword value : Css.grid_template option =
    (* Unitless zero stays bare (minmax(0,1fr)), not 0px. *)
    if value = "0" then Some Css.Zero
    else
      match parse_arbitrary_length value with
      | Some (Px n) -> Some (Px n)
      | Some (Rem n) -> Some (Rem n)
      | Some (Em n) -> Some (Em n)
      | Some (Pct n) -> Some (Pct n)
      | Some (Vw n) -> Some (Vw n)
      | Some (Vh n) -> Some (Vh n)
      | Some _ -> None
      | None ->
          let len = String.length value in
          if len >= 2 && String.sub value (len - 2) 2 = "fr" then
            match float_of_string_opt (String.sub value 0 (len - 2)) with
            | Some n -> Some (Fr n)
            | None -> None
          else if value = "auto" then Some Auto
          else if value = "min-content" then Some Min_content
          else if value = "max-content" then Some Max_content
          else None

  (* A single grid track: a length/keyword, or one of the grid functions
     minmax()/fit-content()/repeat() (which may nest). *)
  let rec parse_single_track value : Css.grid_template option =
    match fn_args "minmax" value with
    | Some inner -> (
        match List.map String.trim (split_top_level ',' inner) with
        | [ a; b ] -> (
            match (parse_single_track a, parse_single_track b) with
            | Some ta, Some tb -> Some (Css.Min_max (ta, tb))
            | _ -> None)
        | _ -> None)
    | None -> (
        match fn_args "fit-content" value with
        | Some inner -> (
            match parse_arbitrary_length (String.trim inner) with
            | Some l -> Some (Css.Fit_content l)
            | None -> None)
        | None -> (
            match fn_args "repeat" value with
            | Some inner -> parse_repeat inner
            | None -> parse_track_keyword value))

  and parse_repeat inner =
    match split_top_level ',' inner with
    | count_s :: rest when rest <> [] -> (
        let count =
          match String.trim count_s with
          | "auto-fill" -> Some Css.Auto_fill
          | "auto-fit" -> Some Css.Auto_fit
          | n -> (
              match int_of_string_opt n with
              | Some i -> Some (Css.Count i)
              | None -> None)
        in
        (* The track list after the count is space-separated (underscores in the
           bracket); commas only separated count from the list. *)
        let tracks =
          String.concat "," rest |> split_top_level '_'
          |> List.filter (fun s -> s <> "")
          |> List.map String.trim
        in
        match (count, all_some (List.map parse_single_track tracks)) with
        | Some c, Some ts -> Some (Css.Repeat (c, ts))
        | _ -> None)
    | _ -> None

  let parse_arbitrary_grid_template s : Css.grid_template option =
    let value = String.trim s in
    (* Underscores are spaces in bracket values; split tracks at the top level
       so functions like repeat(2,1fr_2fr) keep their inner underscores. *)
    let parts = split_top_level '_' value |> List.filter (fun s -> s <> "") in
    match parts with
    | [] -> None
    | [ single ] -> parse_single_track single
    | tracks -> (
        match all_some (List.map parse_single_track tracks) with
        | Some ts -> Some (Tracks ts)
        | None -> None)

  (* Should never fail: of_class validates arbitrary values before constructing
     [Grid_cols_arbitrary]/[Grid_rows_arbitrary]. *)
  let parse_arbitrary_grid_template_exn s =
    match parse_arbitrary_grid_template s with
    | Some v -> v
    | None -> invalid_arg ("Unparseable grid template: " ^ s)

  let grid_cols_arbitrary s =
    style [ Css.grid_template_columns (parse_arbitrary_grid_template_exn s) ]

  let grid_rows_arbitrary s =
    style [ Css.grid_template_rows (parse_arbitrary_grid_template_exn s) ]

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

  let auto_cols_arbitrary s =
    style [ Css.grid_auto_columns (parse_arbitrary_grid_template_exn s) ]

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

  let auto_rows_arbitrary s =
    style [ Css.grid_auto_rows (parse_arbitrary_grid_template_exn s) ]

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
    | Grid_cols_arbitrary s -> grid_cols_arbitrary s
    | Grid_rows n -> grid_rows n
    | Grid_rows_none -> grid_rows_none ()
    | Grid_rows_subgrid -> grid_rows_subgrid
    | Grid_rows_arbitrary s -> grid_rows_arbitrary s
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
    | Auto_cols_arbitrary s -> auto_cols_arbitrary s
    | Auto_rows_auto -> auto_rows_auto ()
    | Auto_rows_min -> auto_rows_min
    | Auto_rows_max -> auto_rows_max
    | Auto_rows_fr -> auto_rows_fr
    | Auto_rows_spacing n -> auto_rows_spacing n
    | Auto_rows_arbitrary s -> auto_rows_arbitrary s

  let suborder = function
    (* Grid template columns (10000-10999) *)
    (* Order: numeric → arbitrary → keywords alphabetical *)
    | Grid_cols n -> 10000 + n
    | Grid_cols_arbitrary _ -> 10800
    | Grid_cols_none -> 10900
    | Grid_cols_subgrid -> 10901
    (* Grid template rows (11000-11999) *)
    | Grid_rows n -> 11000 + n
    | Grid_rows_arbitrary _ -> 11800
    | Grid_rows_none -> 11900
    | Grid_rows_subgrid -> 11901
    (* Grid auto flow (14000-14099) - alphabetical order *)
    | Grid_flow_col -> 14000
    | Grid_flow_col_dense -> 14001
    | Grid_flow_dense -> 14002
    | Grid_flow_row -> 14003
    | Grid_flow_row_dense -> 14004
    (* Grid auto columns (15000-15099) *)
    (* Order: spacing (numeric) → arbitrary → keywords alphabetical *)
    | Auto_cols_spacing _ -> 15000
    | Auto_cols_arbitrary _ -> 15001
    | Auto_cols_auto -> 15002
    | Auto_cols_fr -> 15003
    | Auto_cols_max -> 15004
    | Auto_cols_min -> 15005
    (* Grid auto rows (15100-15199) *)
    | Auto_rows_spacing _ -> 15100
    | Auto_rows_arbitrary _ -> 15101
    | Auto_rows_auto -> 15102
    | Auto_rows_fr -> 15103
    | Auto_rows_max -> 15104
    | Auto_rows_min -> 15105

  let of_class _theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "grid"; "cols"; "none" ] -> Ok Grid_cols_none
    | [ "grid"; "cols"; "subgrid" ] -> Ok Grid_cols_subgrid
    | [ "grid"; "cols"; n ] -> (
        let len = String.length n in
        if len > 2 && n.[0] = '[' && n.[len - 1] = ']' then
          let inner = String.sub n 1 (len - 2) in
          match parse_arbitrary_grid_template inner with
          | Some _ -> Ok (Grid_cols_arbitrary inner)
          | None -> err_invalid_cols
        else
          match int_of_string_opt n with
          | Some i when i >= 1 && i <= 999 -> Ok (Grid_cols i)
          | Some _ | None -> err_invalid_cols)
    | [ "grid"; "rows"; "none" ] -> Ok Grid_rows_none
    | [ "grid"; "rows"; "subgrid" ] -> Ok Grid_rows_subgrid
    | [ "grid"; "rows"; n ] -> (
        let len = String.length n in
        if len > 2 && n.[0] = '[' && n.[len - 1] = ']' then
          let inner = String.sub n 1 (len - 2) in
          match parse_arbitrary_grid_template inner with
          | Some _ -> Ok (Grid_rows_arbitrary inner)
          | None -> err_invalid_rows
        else
          match int_of_string_opt n with
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
              match parse_arbitrary_grid_template inner with
              | Some _ -> Ok (Auto_cols_arbitrary inner)
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
              match parse_arbitrary_grid_template inner with
              | Some _ -> Ok (Auto_rows_arbitrary inner)
              | None -> err_not_utility
            else err_not_utility)
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
    | Auto_cols_spacing n -> "auto-cols-" ^ pp_float n
    | Auto_cols_arbitrary s -> "auto-cols-[" ^ s ^ "]"
    | Auto_rows_auto -> "auto-rows-auto"
    | Auto_rows_min -> "auto-rows-min"
    | Auto_rows_max -> "auto-rows-max"
    | Auto_rows_fr -> "auto-rows-fr"
    | Auto_rows_spacing n -> "auto-rows-" ^ pp_float n
    | Auto_rows_arbitrary s -> "auto-rows-[" ^ s ^ "]"
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
