(** Grid template utilities for grid layout configuration

    https://tailwindcss.com/docs/grid-template-columns
    https://tailwindcss.com/docs/grid-template-rows
    https://tailwindcss.com/docs/grid-auto-flow
    https://tailwindcss.com/docs/grid-auto-columns
    https://tailwindcss.com/docs/grid-auto-rows *)

open Utility

(** {1 Grid Template Columns} *)

val grid_cols : int -> t
(** [grid_cols n] sets the grid column template. *)

val grid_cols_none : t
(** [grid_cols_none] disables the grid column template. *)

val grid_cols_subgrid : t
(** [grid_cols_subgrid] sets the grid to use subgrid for columns. *)

(** {1 Grid Template Rows} *)

val grid_rows : int -> t
(** [grid_rows n] sets the grid row template. *)

val grid_rows_none : t
(** [grid_rows_none] disables the grid row template. *)

val grid_rows_subgrid : t
(** [grid_rows_subgrid] sets the grid to use subgrid for rows. *)

(** {1 Grid Auto Flow} *)

val grid_flow_row : t
(** [grid_flow_row] sets grid auto-flow to row. *)

val grid_flow_col : t
(** [grid_flow_col] sets grid auto-flow to column. *)

val grid_flow_dense : t
(** [grid_flow_dense] sets grid auto-flow to dense. *)

val grid_flow_row_dense : t
(** [grid_flow_row_dense] sets grid auto-flow to row dense. *)

val grid_flow_col_dense : t
(** [grid_flow_col_dense] sets grid auto-flow to column dense. *)

(** {1 Grid Auto Columns} *)

val auto_cols_auto : t
(** [auto_cols_auto] sets grid auto columns to auto. *)

val auto_cols_min : t
(** [auto_cols_min] sets grid auto columns to min-content. *)

val auto_cols_max : t
(** [auto_cols_max] sets grid auto columns to max-content. *)

val auto_cols_fr : t
(** [auto_cols_fr] sets grid auto columns to 1fr. *)

(** {1 Grid Auto Rows} *)

val auto_rows_auto : t
(** [auto_rows_auto] sets grid auto rows to auto. *)

val auto_rows_min : t
(** [auto_rows_min] sets grid auto rows to min-content. *)

val auto_rows_max : t
(** [auto_rows_max] sets grid auto rows to max-content. *)

val auto_rows_fr : t
(** [auto_rows_fr] sets grid auto rows to 1fr. *)

module Handler : Utility.Handler
