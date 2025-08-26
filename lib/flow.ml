open Core
open Css
module Parse = Parse

let ( >|= ) = Parse.( >|= )

(** {1 Display Utilities} *)

let flex = style "flex" [ display Flex ]
let inline_flex = style "inline-flex" [ display Inline_flex ]
let grid = style "grid" [ display Grid ]
let inline_grid = style "inline-grid" [ display Inline_grid ]

(** {1 Flex Direction} *)

let flex_row = style "flex-row" [ Flex.direction Row ]
let flex_row_reverse = style "flex-row-reverse" [ Flex.direction Row_reverse ]
let flex_col = style "flex-col" [ Flex.direction Column ]

let flex_col_reverse =
  style "flex-col-reverse" [ Flex.direction Column_reverse ]

(** {1 Flex Wrap} *)

let flex_wrap = style "flex-wrap" [ Flex.wrap Wrap ]
let flex_wrap_reverse = style "flex-wrap-reverse" [ Flex.wrap Wrap_reverse ]
let flex_nowrap = style "flex-nowrap" [ Flex.wrap Nowrap ]

(** {1 Flex} *)

let flex_1 = style "flex-1" [ Flex.flex (Grow 1.0) ]
let flex_auto = style "flex-auto" [ Flex.flex Auto ]
let flex_initial = style "flex-initial" [ Flex.flex Initial ]
let flex_none = style "flex-none" [ Flex.flex None ]

(** {1 Flex Grow/Shrink} *)

let flex_grow = style "flex-grow" [ Flex.grow 1.0 ]
let flex_grow_0 = style "flex-grow-0" [ Flex.grow 0.0 ]
let flex_shrink = style "flex-shrink" [ Flex.shrink 1.0 ]
let flex_shrink_0 = style "flex-shrink-0" [ Flex.shrink 0.0 ]

(** {1 Flex Basis} *)

let basis_0 = style "basis-0" [ Flex.basis Zero ]
let basis_1 = style "basis-1" [ Flex.basis (Rem 0.25) ]
let basis_auto = style "basis-auto" [ Flex.basis Auto ]
let basis_full = style "basis-full" [ Flex.basis (Pct 100.0) ]

(** {1 Order} *)

let order_1 = style "order-1" [ Flex.order 1 ]
let order_2 = style "order-2" [ Flex.order 2 ]
let order_3 = style "order-3" [ Flex.order 3 ]
let order_4 = style "order-4" [ Flex.order 4 ]
let order_5 = style "order-5" [ Flex.order 5 ]
let order_6 = style "order-6" [ Flex.order 6 ]
let order_first = style "order-first" [ Flex.order min_int ]
let order_last = style "order-last" [ Flex.order max_int ]
let order_none = style "order-none" [ Flex.order 0 ]

(** {1 Align/Justify (Flex/Grid)} *)

let items_start = style "items-start" [ align_items Flex_start ]
let items_end = style "items-end" [ align_items Flex_end ]
let items_center = style "items-center" [ align_items Center ]
let items_baseline = style "items-baseline" [ align_items Baseline ]
let items_stretch = style "items-stretch" [ align_items Stretch ]
let justify_start = style "justify-start" [ justify_content Flex_start ]
let justify_end = style "justify-end" [ justify_content Flex_end ]
let justify_center = style "justify-center" [ justify_content Center ]
let justify_between = style "justify-between" [ justify_content Space_between ]
let justify_around = style "justify-around" [ justify_content Space_around ]
let justify_evenly = style "justify-evenly" [ justify_content Space_evenly ]
let content_start = style "content-start" [ align_content Flex_start ]
let content_end = style "content-end" [ align_content Flex_end ]
let content_center = style "content-center" [ align_content Center ]
let content_between = style "content-between" [ align_content Space_between ]
let content_around = style "content-around" [ align_content Space_around ]
let content_evenly = style "content-evenly" [ align_content Space_evenly ]
let content_stretch = style "content-stretch" [ align_content Stretch ]
let self_auto = style "self-auto" [ align_self Auto ]
let self_start = style "self-start" [ align_self Flex_start ]
let self_end = style "self-end" [ align_self Flex_end ]
let self_center = style "self-center" [ align_self Center ]
let self_baseline = style "self-baseline" [ align_self Baseline ]
let self_stretch = style "self-stretch" [ align_self Stretch ]

let justify_items_start =
  style "justify-items-start" [ Css.Flex.justify_items Start ]

let justify_items_end = style "justify-items-end" [ Css.Flex.justify_items End ]

let justify_items_center =
  style "justify-items-center" [ Css.Flex.justify_items Center ]

let justify_items_stretch =
  style "justify-items-stretch" [ Css.Flex.justify_items Stretch ]

let justify_self_auto = style "justify-self-auto" [ justify_self Auto ]
let justify_self_start = style "justify-self-start" [ justify_self Start ]
let justify_self_end = style "justify-self-end" [ justify_self End ]
let justify_self_center = style "justify-self-center" [ justify_self Center ]
let justify_self_stretch = style "justify-self-stretch" [ justify_self Stretch ]
let place_content_start = style "place-content-start" [ place_content Start ]
let place_content_end = style "place-content-end" [ place_content End ]
let place_content_center = style "place-content-center" [ place_content Center ]

let place_content_between =
  style "place-content-between" [ place_content Space_between ]

let place_content_around =
  style "place-content-around" [ place_content Space_around ]

let place_content_evenly =
  style "place-content-evenly" [ place_content Space_evenly ]

let place_content_stretch =
  style "place-content-stretch" [ place_content Stretch ]

let place_items_start = style "place-items-start" [ place_items Start ]
let place_items_end = style "place-items-end" [ place_items End ]
let place_items_center = style "place-items-center" [ place_items Center ]
let place_items_stretch = style "place-items-stretch" [ place_items Stretch ]
let place_self_auto = style "place-self-auto" [ place_self "auto" ]
let place_self_start = style "place-self-start" [ place_self "start" ]
let place_self_end = style "place-self-end" [ place_self "end" ]
let place_self_center = style "place-self-center" [ place_self "center" ]
let place_self_stretch = style "place-self-stretch" [ place_self "stretch" ]

(** {1 Gap Utilities} *)

(** {2 Gap Helpers} *)

(* Define spacing type for gap utilities *)
type spacing = [ `Px | `Full | `Rem of float ]

let pp_spacing_suffix : spacing -> string = function
  | `Px -> "px"
  | `Full -> "full"
  | `Rem f ->
      (* Convert rem values back to Tailwind scale *)
      let n = int_of_float (f /. 0.25) in
      string_of_int (abs n)

let spacing_to_length : spacing -> length = function
  | `Px -> Px 1
  | `Full -> Pct 100.0
  | `Rem f ->
      let n = int_of_float (f /. 0.25) in
      Calc (Calc.mul (Calc.var "spacing") (Calc.float (float_of_int n)))

let int n = `Rem (float_of_int n *. 0.25)

(** {2 Typed Gap Utilities} *)

let gap' (s : spacing) =
  let class_name = "gap-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ gap len ]

let gap_x' (s : spacing) =
  let class_name = "gap-x-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ column_gap len ]

let gap_y' (s : spacing) =
  let class_name = "gap-y-" ^ pp_spacing_suffix s in
  let len = spacing_to_length s in
  style class_name [ row_gap len ]

(** {2 Int-based Gap Utilities} *)

let gap n = gap' (int n)
let gap_x n = gap_x' (int n)
let gap_y n = gap_y' (int n)

(** {2 Special Gap Values} *)

let gap_px = gap' `Px
let gap_full = gap' `Full

(** {1 Grid Template Columns/Rows} - parameterized helpers plus special cases *)

let grid_cols n =
  if n < 1 || n > 12 then
    invalid_arg (Printf.sprintf "grid_cols: %d is out of range (1-12)" n)
  else
    style
      (Pp.str [ "grid-cols-"; string_of_int n ])
      [ Grid.template_columns (Repeat (n, Min_max (Px 0, Fr 1.0))) ]

let grid_cols_none = style "grid-cols-none" [ Grid.template_columns Grid_none ]

let grid_cols_subgrid =
  style "grid-cols-subgrid" [ Grid.template_columns (Tracks [ Grid_auto ]) ]

let grid_rows n =
  if n < 1 || n > 12 then
    invalid_arg (Printf.sprintf "grid_rows: %d is out of range (1-12)" n)
  else
    style
      (Pp.str [ "grid-rows-"; string_of_int n ])
      [ Grid.template_rows (Repeat (n, Min_max (Px 0, Fr 1.0))) ]

let grid_rows_none = style "grid-rows-none" [ Grid.template_rows Grid_none ]

let grid_rows_subgrid =
  style "grid-rows-subgrid" [ Grid.template_rows (Tracks [ Grid_auto ]) ]

(** {1 Grid Column/Row Spans} *)

let col_auto = style "col-auto" [ Grid.column "auto" ]

let col_span n =
  let span_str =
    Pp.str [ "span "; string_of_int n; " / span "; string_of_int n ]
  in
  style (Pp.str [ "col-span-"; string_of_int n ]) [ Grid.column span_str ]

let col_span_full = style "col-span-full" [ Grid.column "1 / -1" ]

let col_start n =
  style
    (Pp.str [ "col-start-"; string_of_int n ])
    [ Grid.column_start (Line_number n) ]

let col_start_auto = style "col-start-auto" [ Grid.column_start Auto ]

let col_end n =
  style
    (Pp.str [ "col-end-"; string_of_int n ])
    [ Grid.column_end (Line_number n) ]

let col_end_auto = style "col-end-auto" [ Grid.column_end Auto ]
let row_auto = style "row-auto" [ Grid.row "auto" ]

let row_span n =
  let span_str =
    Pp.str [ "span "; string_of_int n; " / span "; string_of_int n ]
  in
  style (Pp.str [ "row-span-"; string_of_int n ]) [ Grid.row span_str ]

let row_span_full = style "row-span-full" [ Grid.row "1 / -1" ]

let row_start n =
  style
    (Pp.str [ "row-start-"; string_of_int n ])
    [ Grid.row_start (Line_number n) ]

let row_start_auto = style "row-start-auto" [ Grid.row_start Auto ]

let row_end n =
  style
    (Pp.str [ "row-end-"; string_of_int n ])
    [ Grid.row_end (Line_number n) ]

let row_end_auto = style "row-end-auto" [ Grid.row_end Auto ]

(** {1 Grid Auto Flow/Auto Tracks} *)

let grid_flow_row = style "grid-flow-row" [ Grid.auto_flow `Row ]
let grid_flow_col = style "grid-flow-col" [ Grid.auto_flow `Column ]
let grid_flow_dense = style "grid-flow-dense" [ Grid.auto_flow `Row_dense ]

let grid_flow_row_dense =
  style "grid-flow-row-dense" [ Grid.auto_flow `Row_dense ]

let grid_flow_col_dense =
  style "grid-flow-col-dense" [ Grid.auto_flow `Column_dense ]

let auto_cols_auto = style "auto-cols-auto" [ Grid.auto_columns Grid_auto ]
let auto_cols_min = style "auto-cols-min" [ Grid.auto_columns Min_content ]
let auto_cols_max = style "auto-cols-max" [ Grid.auto_columns Max_content ]
let auto_cols_fr = style "auto-cols-fr" [ Grid.auto_columns (Fr 1.0) ]
let auto_rows_auto = style "auto-rows-auto" [ Grid.auto_rows Grid_auto ]
let auto_rows_min = style "auto-rows-min" [ Grid.auto_rows Min_content ]
let auto_rows_max = style "auto-rows-max" [ Grid.auto_rows Max_content ]
let auto_rows_fr = style "auto-rows-fr" [ Grid.auto_rows (Fr 1.0) ]

(** {1 Parsing} *)

let of_string parts =
  match parts with
  (* Display *)
  | [ "flex" ] -> Ok flex
  | [ "inline"; "flex" ] -> Ok inline_flex
  | [ "grid" ] -> Ok grid
  | [ "inline"; "grid" ] -> Ok inline_grid
  (* Flex direction *)
  | [ "flex"; "row" ] -> Ok flex_row
  | [ "flex"; "row"; "reverse" ] -> Ok flex_row_reverse
  | [ "flex"; "col" ] -> Ok flex_col
  | [ "flex"; "col"; "reverse" ] -> Ok flex_col_reverse
  (* Flex wrap *)
  | [ "flex"; "wrap" ] -> Ok flex_wrap
  | [ "flex"; "wrap"; "reverse" ] -> Ok flex_wrap_reverse
  | [ "flex"; "nowrap" ] -> Ok flex_nowrap
  (* Flex *)
  | [ "flex"; "1" ] -> Ok flex_1
  | [ "flex"; "auto" ] -> Ok flex_auto
  | [ "flex"; "initial" ] -> Ok flex_initial
  | [ "flex"; "none" ] -> Ok flex_none
  (* Flex grow/shrink *)
  | [ "flex"; "grow" ] -> Ok flex_grow
  | [ "flex"; "grow"; "0" ] -> Ok flex_grow_0
  | [ "flex"; "shrink" ] -> Ok flex_shrink
  | [ "flex"; "shrink"; "0" ] -> Ok flex_shrink_0
  (* Flex basis *)
  | [ "basis"; "0" ] -> Ok basis_0
  | [ "basis"; "1" ] -> Ok basis_1
  | [ "basis"; "auto" ] -> Ok basis_auto
  | [ "basis"; "full" ] -> Ok basis_full
  (* Order *)
  | [ "order"; "1" ] -> Ok order_1
  | [ "order"; "2" ] -> Ok order_2
  | [ "order"; "3" ] -> Ok order_3
  | [ "order"; "4" ] -> Ok order_4
  | [ "order"; "5" ] -> Ok order_5
  | [ "order"; "6" ] -> Ok order_6
  | [ "order"; "first" ] -> Ok order_first
  | [ "order"; "last" ] -> Ok order_last
  | [ "order"; "none" ] -> Ok order_none
  (* Grid template columns/rows *)
  | [ "grid"; "cols"; "none" ] -> Ok grid_cols_none
  | [ "grid"; "cols"; "subgrid" ] -> Ok grid_cols_subgrid
  | [ "grid"; "cols"; n ] -> (
      match int_of_string_opt n with
      | Some i -> Ok (grid_cols i)
      | None -> Error (`Msg ""))
  | [ "grid"; "rows"; "none" ] -> Ok grid_rows_none
  | [ "grid"; "rows"; "subgrid" ] -> Ok grid_rows_subgrid
  | [ "grid"; "rows"; n ] -> (
      match int_of_string_opt n with
      | Some i -> Ok (grid_rows i)
      | None -> Error (`Msg ""))
  (* Grid column/row spans *)
  | [ "col"; "auto" ] -> Ok col_auto
  | [ "col"; "span"; n ] -> (
      match int_of_string_opt n with
      | Some i when i >= 1 && i <= 12 -> Ok (col_span i)
      | _ when n = "full" -> Ok col_span_full
      | _ -> Error (`Msg ("Invalid col-span value: " ^ n)))
  | [ "col"; "start"; "auto" ] -> Ok col_start_auto
  | [ "col"; "start"; n ] -> (
      match int_of_string_opt n with
      | Some i -> Ok (col_start i)
      | None -> Error (`Msg ""))
  | [ "col"; "end"; "auto" ] -> Ok col_end_auto
  | [ "col"; "end"; n ] -> (
      match int_of_string_opt n with
      | Some i -> Ok (col_end i)
      | None -> Error (`Msg ""))
  | [ "row"; "auto" ] -> Ok row_auto
  | [ "row"; "span"; n ] -> (
      match int_of_string_opt n with
      | Some i when i >= 1 && i <= 12 -> Ok (row_span i)
      | _ when n = "full" -> Ok row_span_full
      | _ -> Error (`Msg ("Invalid row-span value: " ^ n)))
  | [ "row"; "start"; "auto" ] -> Ok row_start_auto
  | [ "row"; "start"; n ] -> (
      match int_of_string_opt n with
      | Some i -> Ok (row_start i)
      | None -> Error (`Msg ""))
  | [ "row"; "end"; "auto" ] -> Ok row_end_auto
  | [ "row"; "end"; n ] -> (
      match int_of_string_opt n with
      | Some i -> Ok (row_end i)
      | None -> Error (`Msg ""))
  (* Grid auto flow/auto tracks *)
  | [ "grid"; "flow"; "row" ] -> Ok grid_flow_row
  | [ "grid"; "flow"; "col" ] -> Ok grid_flow_col
  | [ "grid"; "flow"; "dense" ] -> Ok grid_flow_dense
  | [ "grid"; "flow"; "row"; "dense" ] -> Ok grid_flow_row_dense
  | [ "grid"; "flow"; "col"; "dense" ] -> Ok grid_flow_col_dense
  | [ "auto"; "cols"; "auto" ] -> Ok auto_cols_auto
  | [ "auto"; "cols"; "min" ] -> Ok auto_cols_min
  | [ "auto"; "cols"; "max" ] -> Ok auto_cols_max
  | [ "auto"; "cols"; "fr" ] -> Ok auto_cols_fr
  | [ "auto"; "rows"; "auto" ] -> Ok auto_rows_auto
  | [ "auto"; "rows"; "min" ] -> Ok auto_rows_min
  | [ "auto"; "rows"; "max" ] -> Ok auto_rows_max
  | [ "auto"; "rows"; "fr" ] -> Ok auto_rows_fr
  (* Alignments from Grid API *)
  | [ "justify"; "items"; "start" ] -> Ok justify_items_start
  | [ "justify"; "items"; "end" ] -> Ok justify_items_end
  | [ "justify"; "items"; "center" ] -> Ok justify_items_center
  | [ "justify"; "items"; "stretch" ] -> Ok justify_items_stretch
  | [ "justify"; "self"; "auto" ] -> Ok justify_self_auto
  | [ "justify"; "self"; "start" ] -> Ok justify_self_start
  | [ "justify"; "self"; "end" ] -> Ok justify_self_end
  | [ "justify"; "self"; "center" ] -> Ok justify_self_center
  | [ "justify"; "self"; "stretch" ] -> Ok justify_self_stretch
  | [ "place"; "content"; "start" ] -> Ok place_content_start
  | [ "place"; "content"; "end" ] -> Ok place_content_end
  | [ "place"; "content"; "center" ] -> Ok place_content_center
  | [ "place"; "content"; "between" ] -> Ok place_content_between
  | [ "place"; "content"; "around" ] -> Ok place_content_around
  | [ "place"; "content"; "evenly" ] -> Ok place_content_evenly
  | [ "place"; "content"; "stretch" ] -> Ok place_content_stretch
  | [ "place"; "items"; "start" ] -> Ok place_items_start
  | [ "place"; "items"; "end" ] -> Ok place_items_end
  | [ "place"; "items"; "center" ] -> Ok place_items_center
  | [ "place"; "items"; "stretch" ] -> Ok place_items_stretch
  | [ "place"; "self"; "auto" ] -> Ok place_self_auto
  | [ "place"; "self"; "start" ] -> Ok place_self_start
  | [ "place"; "self"; "end" ] -> Ok place_self_end
  | [ "place"; "self"; "center" ] -> Ok place_self_center
  | [ "place"; "self"; "stretch" ] -> Ok place_self_stretch
  (* Gap utilities *)
  | [ "gap"; "px" ] -> Ok (gap' `Px)
  | [ "gap"; "full" ] -> Ok (gap' `Full)
  | [ "gap"; n ] -> Parse.int_pos ~name:"gap" n >|= gap
  | [ "gap"; "x"; "px" ] -> Ok (gap_x' `Px)
  | [ "gap"; "x"; "full" ] -> Ok (gap_x' `Full)
  | [ "gap"; "x"; n ] -> Parse.int_pos ~name:"gap-x" n >|= gap_x
  | [ "gap"; "y"; "px" ] -> Ok (gap_y' `Px)
  | [ "gap"; "y"; "full" ] -> Ok (gap_y' `Full)
  | [ "gap"; "y"; n ] -> Parse.int_pos ~name:"gap-y" n >|= gap_y
  | _ -> Error (`Msg "Not a flow utility")
