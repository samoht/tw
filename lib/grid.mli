(** CSS Grid utilities *)

open Core

(** {1 Grid Display} *)

val grid : t
(** [grid] sets display to grid. *)

val inline_grid : t
(** [inline_grid] sets display to inline-grid. *)

(** {1 Grid Template Columns} *)

val grid_cols_1 : t
val grid_cols_2 : t
val grid_cols_3 : t
val grid_cols_4 : t
val grid_cols_5 : t
val grid_cols_6 : t
val grid_cols_7 : t
val grid_cols_8 : t
val grid_cols_9 : t
val grid_cols_10 : t
val grid_cols_11 : t

val grid_cols_12 : t
(** [grid_cols_n] creates a grid with n equal columns. *)

val grid_cols_none : t
(** [grid_cols_none] removes grid template columns. *)

val grid_cols_subgrid : t
(** [grid_cols_subgrid] uses subgrid for columns. *)

val grid_cols : int -> t
(** [grid_cols n] creates a grid with n equal columns. *)

(** {1 Grid Template Rows} *)

val grid_rows_1 : t
val grid_rows_2 : t
val grid_rows_3 : t
val grid_rows_4 : t
val grid_rows_5 : t

val grid_rows_6 : t
(** [grid_rows_n] creates a grid with n equal rows. *)

val grid_rows : int -> t
(** [grid_rows n] creates a grid with n equal rows. *)

val grid_rows_none : t
(** [grid_rows_none] removes grid template rows. *)

val grid_rows_subgrid : t
(** [grid_rows_subgrid] uses subgrid for rows. *)

(** {1 Grid Column Spans} *)

val col_auto : t
(** [col_auto] sets grid-column to auto. *)

val col_span : int -> t
(** [col_span n] makes element span n columns. *)

val col_span_full : t
(** [col_span_full] spans all columns. *)

val col_start : int -> t
(** [col_start n] starts at column line n. *)

val col_start_auto : t
(** [col_start_auto] auto-places column start. *)

val col_end : int -> t
(** [col_end n] ends at column line n. *)

val col_end_auto : t
(** [col_end_auto] auto-places column end. *)

(** {1 Grid Row Spans} *)

val row_auto : t
(** [row_auto] sets grid-row to auto. *)

val row_span : int -> t
(** [row_span n] makes element span n rows. *)

val row_span_full : t
(** [row_span_full] spans all rows. *)

val row_start : int -> t
(** [row_start n] starts at row line n. *)

val row_start_auto : t
(** [row_start_auto] auto-places row start. *)

val row_end : int -> t
(** [row_end n] ends at row line n. *)

val row_end_auto : t
(** [row_end_auto] auto-places row end. *)

(** {1 Grid Auto Flow} *)

val grid_flow_row : t
(** [grid_flow_row] sets auto-placement to fill rows. *)

val grid_flow_col : t
(** [grid_flow_col] sets auto-placement to fill columns. *)

val grid_flow_dense : t
(** [grid_flow_dense] uses dense packing algorithm. *)

val grid_flow_row_dense : t
(** [grid_flow_row_dense] fills rows with dense packing. *)

val grid_flow_col_dense : t
(** [grid_flow_col_dense] fills columns with dense packing. *)

(** {1 Grid Auto Columns} *)

val auto_cols_auto : t
(** [auto_cols_auto] sets implicit columns to auto. *)

val auto_cols_min : t
(** [auto_cols_min] sets implicit columns to min-content. *)

val auto_cols_max : t
(** [auto_cols_max] sets implicit columns to max-content. *)

val auto_cols_fr : t
(** [auto_cols_fr] sets implicit columns to 1fr. *)

(** {1 Grid Auto Rows} *)

val auto_rows_auto : t
(** [auto_rows_auto] sets implicit rows to auto. *)

val auto_rows_min : t
(** [auto_rows_min] sets implicit rows to min-content. *)

val auto_rows_max : t
(** [auto_rows_max] sets implicit rows to max-content. *)

val auto_rows_fr : t
(** [auto_rows_fr] sets implicit rows to 1fr. *)

(** {1 Gap Utilities} *)

val gap_0 : t
val gap_x_0 : t
val gap_y_0 : t
val gap_px : t
val gap_x_px : t
val gap_y_px : t
val gap_0_5 : t
val gap_x_0_5 : t
val gap_y_0_5 : t
val gap_1 : t
val gap_x_1 : t
val gap_y_1 : t
val gap_1_5 : t
val gap_x_1_5 : t
val gap_y_1_5 : t
val gap_2 : t
val gap_x_2 : t
val gap_y_2 : t
val gap_2_5 : t
val gap_x_2_5 : t
val gap_y_2_5 : t
val gap_3 : t
val gap_x_3 : t
val gap_y_3 : t
val gap_3_5 : t
val gap_x_3_5 : t
val gap_y_3_5 : t
val gap_4 : t
val gap_x_4 : t
val gap_y_4 : t
val gap_5 : t
val gap_x_5 : t
val gap_y_5 : t
val gap_6 : t
val gap_x_6 : t
val gap_y_6 : t
val gap_7 : t
val gap_x_7 : t
val gap_y_7 : t
val gap_8 : t
val gap_x_8 : t
val gap_y_8 : t
val gap_9 : t
val gap_x_9 : t
val gap_y_9 : t
val gap_10 : t
val gap_x_10 : t
val gap_y_10 : t
val gap_11 : t
val gap_x_11 : t
val gap_y_11 : t
val gap_12 : t
val gap_x_12 : t
val gap_y_12 : t
val gap_14 : t
val gap_x_14 : t
val gap_y_14 : t
val gap_16 : t
val gap_x_16 : t
val gap_y_16 : t
val gap_20 : t
val gap_x_20 : t
val gap_y_20 : t
val gap_24 : t
val gap_x_24 : t
val gap_y_24 : t
val gap_28 : t
val gap_x_28 : t
val gap_y_28 : t
val gap_32 : t
val gap_x_32 : t
val gap_y_32 : t
val gap_36 : t
val gap_x_36 : t
val gap_y_36 : t
val gap_40 : t
val gap_x_40 : t
val gap_y_40 : t
val gap_44 : t
val gap_x_44 : t
val gap_y_44 : t
val gap_48 : t
val gap_x_48 : t
val gap_y_48 : t
val gap_52 : t
val gap_x_52 : t
val gap_y_52 : t
val gap_56 : t
val gap_x_56 : t
val gap_y_56 : t
val gap_60 : t
val gap_x_60 : t
val gap_y_60 : t
val gap_64 : t
val gap_x_64 : t
val gap_y_64 : t
val gap_72 : t
val gap_x_72 : t
val gap_y_72 : t
val gap_80 : t
val gap_x_80 : t
val gap_y_80 : t
val gap_96 : t
val gap_x_96 : t

val gap_y_96 : t
(** [gap_{n}] sets gap between grid items. *)

(** {1 Justify Items Utilities} *)

val justify_items_start : t
(** [justify_items_start] justifies items to start. *)

val justify_items_end : t
(** [justify_items_end] justifies items to end. *)

val justify_items_center : t
(** [justify_items_center] centers items. *)

val justify_items_stretch : t
(** [justify_items_stretch] stretches items. *)

(** {1 Justify Self} *)

val justify_self_auto : t
(** [justify_self_auto] uses parent's justify-items value (default). *)

val justify_self_start : t
(** [justify_self_start] justifies item to the start. *)

val justify_self_end : t
(** [justify_self_end] justifies item to the end. *)

val justify_self_center : t
(** [justify_self_center] centers the item. *)

val justify_self_stretch : t
(** [justify_self_stretch] stretches the item. *)

(** {1 Place Content Utilities} *)

val place_content_start : t
(** [place_content_start] aligns content to start in both axes. *)

val place_content_end : t
(** [place_content_end] aligns content to end in both axes. *)

val place_content_center : t
(** [place_content_center] centers content in both axes. *)

val place_content_between : t
(** [place_content_between] distributes content with space between in both axes.
*)

val place_content_around : t
(** [place_content_around] distributes content with space around in both axes.
*)

val place_content_evenly : t
(** [place_content_evenly] distributes content evenly in both axes. *)

val place_content_stretch : t
(** [place_content_stretch] stretches content to fill both axes. *)

(** {1 Place Items Utilities} *)

val place_items_start : t
(** [place_items_start] aligns items to start in both axes. *)

val place_items_end : t
(** [place_items_end] aligns items to end in both axes. *)

val place_items_center : t
(** [place_items_center] centers items in both axes. *)

val place_items_stretch : t
(** [place_items_stretch] stretches items to fill both axes. *)

(** {1 Place Self Utilities} *)

val place_self_auto : t
(** [place_self_auto] uses parent's place-items value. *)

val place_self_start : t
(** [place_self_start] aligns to start in both axes. *)

val place_self_end : t
(** [place_self_end] aligns to end in both axes. *)

val place_self_center : t
(** [place_self_center] centers in both axes. *)

val place_self_stretch : t
(** [place_self_stretch] stretches to fill both axes. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a grid utility from string parts. *)
