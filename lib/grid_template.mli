(** Grid template utilities for grid layout configuration

    https://tailwindcss.com/docs/grid-template-columns
    https://tailwindcss.com/docs/grid-template-rows
    https://tailwindcss.com/docs/grid-auto-flow
    https://tailwindcss.com/docs/grid-auto-columns
    https://tailwindcss.com/docs/grid-auto-rows *)

open Utility

(** {1 Grid Template Columns} *)

val grid_cols : int -> t
val grid_cols_none : t
val grid_cols_subgrid : t

(** {1 Grid Template Rows} *)

val grid_rows : int -> t
val grid_rows_none : t
val grid_rows_subgrid : t

(** {1 Grid Auto Flow} *)

val grid_flow_row : t
val grid_flow_col : t
val grid_flow_dense : t
val grid_flow_row_dense : t
val grid_flow_col_dense : t

(** {1 Grid Auto Columns} *)

val auto_cols_auto : t
val auto_cols_min : t
val auto_cols_max : t
val auto_cols_fr : t

(** {1 Grid Auto Rows} *)

val auto_rows_auto : t
val auto_rows_min : t
val auto_rows_max : t
val auto_rows_fr : t

module Handler : sig
  type t

  val of_string : string list -> (t, [ `Msg of string ]) result
  val suborder : t -> int
  val to_style : t -> Style.t
  val order : t -> int * int
end
