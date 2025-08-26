open Core
open Css

(** {1 Grid Display} *)

let grid = style "grid" [ display Grid ]
let inline_grid = style "inline-grid" [ display Inline_grid ]

(** {1 Grid Template Columns} *)

let grid_cols_1 =
  style "grid-cols-1"
    [ Grid.template_columns (Repeat (1, Min_max (Px 0, Fr 1.0))) ]

let grid_cols_2 =
  style "grid-cols-2"
    [ Grid.template_columns (Repeat (2, Min_max (Px 0, Fr 1.0))) ]

let grid_cols_3 =
  style "grid-cols-3"
    [ Grid.template_columns (Repeat (3, Min_max (Px 0, Fr 1.0))) ]

let grid_cols_4 =
  style "grid-cols-4"
    [ Grid.template_columns (Repeat (4, Min_max (Px 0, Fr 1.0))) ]

let grid_cols_5 =
  style "grid-cols-5"
    [ Grid.template_columns (Repeat (5, Min_max (Px 0, Fr 1.0))) ]

let grid_cols_6 =
  style "grid-cols-6"
    [ Grid.template_columns (Repeat (6, Min_max (Px 0, Fr 1.0))) ]

let grid_cols_7 =
  style "grid-cols-7"
    [ Grid.template_columns (Repeat (7, Min_max (Px 0, Fr 1.0))) ]

let grid_cols_8 =
  style "grid-cols-8"
    [ Grid.template_columns (Repeat (8, Min_max (Px 0, Fr 1.0))) ]

let grid_cols_9 =
  style "grid-cols-9"
    [ Grid.template_columns (Repeat (9, Min_max (Px 0, Fr 1.0))) ]

let grid_cols_10 =
  style "grid-cols-10"
    [ Grid.template_columns (Repeat (10, Min_max (Px 0, Fr 1.0))) ]

let grid_cols_11 =
  style "grid-cols-11"
    [ Grid.template_columns (Repeat (11, Min_max (Px 0, Fr 1.0))) ]

let grid_cols_12 =
  style "grid-cols-12"
    [ Grid.template_columns (Repeat (12, Min_max (Px 0, Fr 1.0))) ]

let grid_cols_none = style "grid-cols-none" [ Grid.template_columns Grid_none ]

let grid_cols_subgrid =
  style "grid-cols-subgrid" [ Grid.template_columns (Tracks [ Grid_auto ]) ]

(** Parameterized grid_cols function *)
let grid_cols n =
  if n < 1 || n > 12 then
    invalid_arg (Printf.sprintf "grid_cols: %d is out of range (1-12)" n)
  else
    style
      (Pp.str [ "grid-cols-"; string_of_int n ])
      [ Grid.template_columns (Repeat (n, Min_max (Px 0, Fr 1.0))) ]

(** {1 Grid Template Rows} *)

let grid_rows_1 =
  style "grid-rows-1"
    [ Grid.template_rows (Repeat (1, Min_max (Px 0, Fr 1.0))) ]

let grid_rows_2 =
  style "grid-rows-2"
    [ Grid.template_rows (Repeat (2, Min_max (Px 0, Fr 1.0))) ]

let grid_rows_3 =
  style "grid-rows-3"
    [ Grid.template_rows (Repeat (3, Min_max (Px 0, Fr 1.0))) ]

let grid_rows_4 =
  style "grid-rows-4"
    [ Grid.template_rows (Repeat (4, Min_max (Px 0, Fr 1.0))) ]

let grid_rows_5 =
  style "grid-rows-5"
    [ Grid.template_rows (Repeat (5, Min_max (Px 0, Fr 1.0))) ]

let grid_rows_6 =
  style "grid-rows-6"
    [ Grid.template_rows (Repeat (6, Min_max (Px 0, Fr 1.0))) ]

let grid_rows_none = style "grid-rows-none" [ Grid.template_rows Grid_none ]

let grid_rows_subgrid =
  style "grid-rows-subgrid" [ Grid.template_rows (Tracks [ Grid_auto ]) ]

(** Parameterized grid_rows function *)
let grid_rows n =
  if n < 1 || n > 12 then
    invalid_arg (Printf.sprintf "grid_rows: %d is out of range (1-12)" n)
  else
    style
      (Pp.str [ "grid-rows-"; string_of_int n ])
      [ Grid.template_rows (Repeat (n, Min_max (Px 0, Fr 1.0))) ]

(** {1 Grid Column Spans} *)

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

(** {1 Grid Row Spans} *)

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

(** {1 Grid Auto Flow} *)

let grid_flow_row = style "grid-flow-row" [ Grid.auto_flow `Row ]
let grid_flow_col = style "grid-flow-col" [ Grid.auto_flow `Column ]
let grid_flow_dense = style "grid-flow-dense" [ Grid.auto_flow `Row_dense ]

let grid_flow_row_dense =
  style "grid-flow-row-dense" [ Grid.auto_flow `Row_dense ]

let grid_flow_col_dense =
  style "grid-flow-col-dense" [ Grid.auto_flow `Column_dense ]

(** {1 Grid Auto Columns} *)

let auto_cols_auto = style "auto-cols-auto" [ Grid.auto_columns Grid_auto ]
let auto_cols_min = style "auto-cols-min" [ Grid.auto_columns Min_content ]
let auto_cols_max = style "auto-cols-max" [ Grid.auto_columns Max_content ]
let auto_cols_fr = style "auto-cols-fr" [ Grid.auto_columns (Fr 1.0) ]

(** {1 Grid Auto Rows} *)

let auto_rows_auto = style "auto-rows-auto" [ Grid.auto_rows Grid_auto ]
let auto_rows_min = style "auto-rows-min" [ Grid.auto_rows Min_content ]
let auto_rows_max = style "auto-rows-max" [ Grid.auto_rows Max_content ]
let auto_rows_fr = style "auto-rows-fr" [ Grid.auto_rows (Fr 1.0) ]

(** {1 Gap Utilities} *)

let gap_0 = style "gap-0" [ Grid.gap Zero ]
let gap_x_0 = style "gap-x-0" [ Grid.column_gap Zero ]
let gap_y_0 = style "gap-y-0" [ Grid.row_gap Zero ]
let gap_px = style "gap-px" [ Grid.gap (Px 1) ]
let gap_x_px = style "gap-x-px" [ Grid.column_gap (Px 1) ]
let gap_y_px = style "gap-y-px" [ Grid.row_gap (Px 1) ]
let gap_0_5 = style "gap-0.5" [ Grid.gap (Rem 0.125) ]
let gap_x_0_5 = style "gap-x-0.5" [ Grid.column_gap (Rem 0.125) ]
let gap_y_0_5 = style "gap-y-0.5" [ Grid.row_gap (Rem 0.125) ]
let gap_1 = style "gap-1" [ Grid.gap (Rem 0.25) ]
let gap_x_1 = style "gap-x-1" [ Grid.column_gap (Rem 0.25) ]
let gap_y_1 = style "gap-y-1" [ Grid.row_gap (Rem 0.25) ]
let gap_1_5 = style "gap-1.5" [ Grid.gap (Rem 0.375) ]
let gap_x_1_5 = style "gap-x-1.5" [ Grid.column_gap (Rem 0.375) ]
let gap_y_1_5 = style "gap-y-1.5" [ Grid.row_gap (Rem 0.375) ]
let gap_2 = style "gap-2" [ Grid.gap (Rem 0.5) ]
let gap_x_2 = style "gap-x-2" [ Grid.column_gap (Rem 0.5) ]
let gap_y_2 = style "gap-y-2" [ Grid.row_gap (Rem 0.5) ]
let gap_2_5 = style "gap-2.5" [ Grid.gap (Rem 0.625) ]
let gap_x_2_5 = style "gap-x-2.5" [ Grid.column_gap (Rem 0.625) ]
let gap_y_2_5 = style "gap-y-2.5" [ Grid.row_gap (Rem 0.625) ]
let gap_3 = style "gap-3" [ Grid.gap (Rem 0.75) ]
let gap_x_3 = style "gap-x-3" [ Grid.column_gap (Rem 0.75) ]
let gap_y_3 = style "gap-y-3" [ Grid.row_gap (Rem 0.75) ]
let gap_3_5 = style "gap-3.5" [ Grid.gap (Rem 0.875) ]
let gap_x_3_5 = style "gap-x-3.5" [ Grid.column_gap (Rem 0.875) ]
let gap_y_3_5 = style "gap-y-3.5" [ Grid.row_gap (Rem 0.875) ]
let gap_4 = style "gap-4" [ Grid.gap (Rem 1.0) ]
let gap_x_4 = style "gap-x-4" [ Grid.column_gap (Rem 1.0) ]
let gap_y_4 = style "gap-y-4" [ Grid.row_gap (Rem 1.0) ]
let gap_5 = style "gap-5" [ Grid.gap (Rem 1.25) ]
let gap_x_5 = style "gap-x-5" [ Grid.column_gap (Rem 1.25) ]
let gap_y_5 = style "gap-y-5" [ Grid.row_gap (Rem 1.25) ]
let gap_6 = style "gap-6" [ Grid.gap (Rem 1.5) ]
let gap_x_6 = style "gap-x-6" [ Grid.column_gap (Rem 1.5) ]
let gap_y_6 = style "gap-y-6" [ Grid.row_gap (Rem 1.5) ]
let gap_7 = style "gap-7" [ Grid.gap (Rem 1.75) ]
let gap_x_7 = style "gap-x-7" [ Grid.column_gap (Rem 1.75) ]
let gap_y_7 = style "gap-y-7" [ Grid.row_gap (Rem 1.75) ]
let gap_8 = style "gap-8" [ Grid.gap (Rem 2.0) ]
let gap_x_8 = style "gap-x-8" [ Grid.column_gap (Rem 2.0) ]
let gap_y_8 = style "gap-y-8" [ Grid.row_gap (Rem 2.0) ]
let gap_9 = style "gap-9" [ Grid.gap (Rem 2.25) ]
let gap_x_9 = style "gap-x-9" [ Grid.column_gap (Rem 2.25) ]
let gap_y_9 = style "gap-y-9" [ Grid.row_gap (Rem 2.25) ]
let gap_10 = style "gap-10" [ Grid.gap (Rem 2.5) ]
let gap_x_10 = style "gap-x-10" [ Grid.column_gap (Rem 2.5) ]
let gap_y_10 = style "gap-y-10" [ Grid.row_gap (Rem 2.5) ]
let gap_11 = style "gap-11" [ Grid.gap (Rem 2.75) ]
let gap_x_11 = style "gap-x-11" [ Grid.column_gap (Rem 2.75) ]
let gap_y_11 = style "gap-y-11" [ Grid.row_gap (Rem 2.75) ]
let gap_12 = style "gap-12" [ Grid.gap (Rem 3.0) ]
let gap_x_12 = style "gap-x-12" [ Grid.column_gap (Rem 3.0) ]
let gap_y_12 = style "gap-y-12" [ Grid.row_gap (Rem 3.0) ]
let gap_14 = style "gap-14" [ Grid.gap (Rem 3.5) ]
let gap_x_14 = style "gap-x-14" [ Grid.column_gap (Rem 3.5) ]
let gap_y_14 = style "gap-y-14" [ Grid.row_gap (Rem 3.5) ]
let gap_16 = style "gap-16" [ Grid.gap (Rem 4.0) ]
let gap_x_16 = style "gap-x-16" [ Grid.column_gap (Rem 4.0) ]
let gap_y_16 = style "gap-y-16" [ Grid.row_gap (Rem 4.0) ]
let gap_20 = style "gap-20" [ Grid.gap (Rem 5.0) ]
let gap_x_20 = style "gap-x-20" [ Grid.column_gap (Rem 5.0) ]
let gap_y_20 = style "gap-y-20" [ Grid.row_gap (Rem 5.0) ]
let gap_24 = style "gap-24" [ Grid.gap (Rem 6.0) ]
let gap_x_24 = style "gap-x-24" [ Grid.column_gap (Rem 6.0) ]
let gap_y_24 = style "gap-y-24" [ Grid.row_gap (Rem 6.0) ]
let gap_28 = style "gap-28" [ Grid.gap (Rem 7.0) ]
let gap_x_28 = style "gap-x-28" [ Grid.column_gap (Rem 7.0) ]
let gap_y_28 = style "gap-y-28" [ Grid.row_gap (Rem 7.0) ]
let gap_32 = style "gap-32" [ Grid.gap (Rem 8.0) ]
let gap_x_32 = style "gap-x-32" [ Grid.column_gap (Rem 8.0) ]
let gap_y_32 = style "gap-y-32" [ Grid.row_gap (Rem 8.0) ]
let gap_36 = style "gap-36" [ Grid.gap (Rem 9.0) ]
let gap_x_36 = style "gap-x-36" [ Grid.column_gap (Rem 9.0) ]
let gap_y_36 = style "gap-y-36" [ Grid.row_gap (Rem 9.0) ]
let gap_40 = style "gap-40" [ Grid.gap (Rem 10.0) ]
let gap_x_40 = style "gap-x-40" [ Grid.column_gap (Rem 10.0) ]
let gap_y_40 = style "gap-y-40" [ Grid.row_gap (Rem 10.0) ]
let gap_44 = style "gap-44" [ Grid.gap (Rem 11.0) ]
let gap_x_44 = style "gap-x-44" [ Grid.column_gap (Rem 11.0) ]
let gap_y_44 = style "gap-y-44" [ Grid.row_gap (Rem 11.0) ]
let gap_48 = style "gap-48" [ Grid.gap (Rem 12.0) ]
let gap_x_48 = style "gap-x-48" [ Grid.column_gap (Rem 12.0) ]
let gap_y_48 = style "gap-y-48" [ Grid.row_gap (Rem 12.0) ]
let gap_52 = style "gap-52" [ Grid.gap (Rem 13.0) ]
let gap_x_52 = style "gap-x-52" [ Grid.column_gap (Rem 13.0) ]
let gap_y_52 = style "gap-y-52" [ Grid.row_gap (Rem 13.0) ]
let gap_56 = style "gap-56" [ Grid.gap (Rem 14.0) ]
let gap_x_56 = style "gap-x-56" [ Grid.column_gap (Rem 14.0) ]
let gap_y_56 = style "gap-y-56" [ Grid.row_gap (Rem 14.0) ]
let gap_60 = style "gap-60" [ Grid.gap (Rem 15.0) ]
let gap_x_60 = style "gap-x-60" [ Grid.column_gap (Rem 15.0) ]
let gap_y_60 = style "gap-y-60" [ Grid.row_gap (Rem 15.0) ]
let gap_64 = style "gap-64" [ Grid.gap (Rem 16.0) ]
let gap_x_64 = style "gap-x-64" [ Grid.column_gap (Rem 16.0) ]
let gap_y_64 = style "gap-y-64" [ Grid.row_gap (Rem 16.0) ]
let gap_72 = style "gap-72" [ Grid.gap (Rem 18.0) ]
let gap_x_72 = style "gap-x-72" [ Grid.column_gap (Rem 18.0) ]
let gap_y_72 = style "gap-y-72" [ Grid.row_gap (Rem 18.0) ]
let gap_80 = style "gap-80" [ Grid.gap (Rem 20.0) ]
let gap_x_80 = style "gap-x-80" [ Grid.column_gap (Rem 20.0) ]
let gap_y_80 = style "gap-y-80" [ Grid.row_gap (Rem 20.0) ]
let gap_96 = style "gap-96" [ Grid.gap (Rem 24.0) ]
let gap_x_96 = style "gap-x-96" [ Grid.column_gap (Rem 24.0) ]
let gap_y_96 = style "gap-y-96" [ Grid.row_gap (Rem 24.0) ]

(** {1 Justify Items Utilities} *)

let justify_items_start =
  style "justify-items-start" [ Flex.justify_items Start ]

let justify_items_end = style "justify-items-end" [ Flex.justify_items End ]

let justify_items_center =
  style "justify-items-center" [ Flex.justify_items Center ]

let justify_items_stretch =
  style "justify-items-stretch" [ Flex.justify_items Stretch ]

(** Justify Self *)
let justify_self_auto = style "justify-self-auto" [ Flex.justify_self Auto ]

let justify_self_start = style "justify-self-start" [ Flex.justify_self Start ]
let justify_self_end = style "justify-self-end" [ Flex.justify_self End ]

let justify_self_center =
  style "justify-self-center" [ Flex.justify_self Center ]

let justify_self_stretch =
  style "justify-self-stretch" [ Flex.justify_self Stretch ]

(** {1 Place Content Utilities} *)

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

(** {1 Place Items Utilities} *)

let place_items_start = style "place-items-start" [ place_items Start ]
let place_items_end = style "place-items-end" [ place_items End ]
let place_items_center = style "place-items-center" [ place_items Center ]
let place_items_stretch = style "place-items-stretch" [ place_items Stretch ]

(** {1 Place Self Utilities} *)

let place_self_auto = style "place-self-auto" [ place_self "auto" ]
let place_self_start = style "place-self-start" [ place_self "start" ]
let place_self_end = style "place-self-end" [ place_self "end" ]
let place_self_center = style "place-self-center" [ place_self "center" ]
let place_self_stretch = style "place-self-stretch" [ place_self "stretch" ]

(** {1 Parsing Functions} *)

let of_string = function
  | [ "grid" ] -> Ok grid
  | [ "inline"; "grid" ] -> Ok inline_grid
  | [ "grid"; "cols"; "1" ] -> Ok grid_cols_1
  | [ "grid"; "cols"; "2" ] -> Ok grid_cols_2
  | [ "grid"; "cols"; "3" ] -> Ok grid_cols_3
  | [ "grid"; "cols"; "4" ] -> Ok grid_cols_4
  | [ "grid"; "cols"; "5" ] -> Ok grid_cols_5
  | [ "grid"; "cols"; "6" ] -> Ok grid_cols_6
  | [ "grid"; "cols"; "7" ] -> Ok grid_cols_7
  | [ "grid"; "cols"; "8" ] -> Ok grid_cols_8
  | [ "grid"; "cols"; "9" ] -> Ok grid_cols_9
  | [ "grid"; "cols"; "10" ] -> Ok grid_cols_10
  | [ "grid"; "cols"; "11" ] -> Ok grid_cols_11
  | [ "grid"; "cols"; "12" ] -> Ok grid_cols_12
  | [ "grid"; "cols"; "none" ] -> Ok grid_cols_none
  | [ "grid"; "cols"; "subgrid" ] -> Ok grid_cols_subgrid
  | [ "grid"; "rows"; "1" ] -> Ok grid_rows_1
  | [ "grid"; "rows"; "2" ] -> Ok grid_rows_2
  | [ "grid"; "rows"; "3" ] -> Ok grid_rows_3
  | [ "grid"; "rows"; "4" ] -> Ok grid_rows_4
  | [ "grid"; "rows"; "5" ] -> Ok grid_rows_5
  | [ "grid"; "rows"; "6" ] -> Ok grid_rows_6
  | [ "grid"; "rows"; "none" ] -> Ok grid_rows_none
  | [ "grid"; "rows"; "subgrid" ] -> Ok grid_rows_subgrid
  | [ "col"; "auto" ] -> Ok col_auto
  | [ "col"; "span"; n ] -> (
      match int_of_string_opt n with
      | Some i when i >= 1 && i <= 12 -> Ok (col_span i)
      | _ when n = "full" -> Ok col_span_full
      | _ -> Error (`Msg ("Invalid col-span value: " ^ n)))
  | [ "gap"; "0" ] -> Ok gap_0
  | [ "gap"; "px" ] -> Ok gap_px
  | [ "gap"; "1" ] -> Ok gap_1
  | [ "gap"; "2" ] -> Ok gap_2
  | [ "gap"; "3" ] -> Ok gap_3
  | [ "gap"; "4" ] -> Ok gap_4
  | [ "gap"; "5" ] -> Ok gap_5
  | [ "gap"; "6" ] -> Ok gap_6
  | [ "gap"; "8" ] -> Ok gap_8
  | [ "gap"; "10" ] -> Ok gap_10
  | [ "gap"; "12" ] -> Ok gap_12
  | [ "gap"; "16" ] -> Ok gap_16
  | [ "gap"; "20" ] -> Ok gap_20
  | [ "gap"; "24" ] -> Ok gap_24
  | [ "gap"; "32" ] -> Ok gap_32
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
  | parts ->
      Error
        (`Msg (Pp.str [ "Unknown grid utility: "; String.concat "-" parts ]))
