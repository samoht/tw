(** Layout utilities for display, positioning, and z-index

    https://tailwindcss.com/docs/display https://tailwindcss.com/docs/visibility
    https://tailwindcss.com/docs/z-index https://tailwindcss.com/docs/object-fit
    https://tailwindcss.com/docs/object-position *)

open Utility

(** {1 Display Utilities} *)

val block : t
(** [block] sets display to block. *)

val contents : t
(** [contents] sets display to contents. *)

val flow_root : t
(** [flow_root] sets display to flow-root. *)

val inline : t
(** [inline] sets display to inline. *)

val inline_block : t
(** [inline_block] sets display to inline-block. *)

val inline_table : t
(** [inline_table] sets display to inline-table. *)

val list_item : t
(** [list_item] sets display to list-item. *)

val table : t
(** [table] sets display to table. *)

val table_caption : t
(** [table_caption] sets display to table-caption. *)

val table_cell : t
(** [table_cell] sets display to table-cell. *)

val table_column : t
(** [table_column] sets display to table-column. *)

val table_column_group : t
(** [table_column_group] sets display to table-column-group. *)

val table_footer_group : t
(** [table_footer_group] sets display to table-footer-group. *)

val table_header_group : t
(** [table_header_group] sets display to table-header-group. *)

val table_row : t
(** [table_row] sets display to table-row. *)

val table_row_group : t
(** [table_row_group] sets display to table-row-group. *)

val hidden : t
(** [hidden] sets display to none. *)

val sr_only : t
(** [sr_only] hides element visually but keeps it accessible to screen readers.
*)

val not_sr_only : t
(** [not_sr_only] reverses sr_only. *)

val visible : t
(** [visible] sets visibility to visible. *)

val invisible : t
(** [invisible] sets visibility to hidden. *)

val collapse : t
(** [collapse] sets visibility to collapse. *)

val isolate : t
(** [isolate] creates a new stacking context. *)

val isolation_auto : t
(** [isolation_auto] does not create a new stacking context (auto). *)

val z_0 : t
(** [z_0] sets z-index to 0. *)

val z_10 : t
(** [z_10] sets z-index to 10. *)

val z_20 : t
(** [z_20] sets z-index to 20. *)

val z_30 : t
(** [z_30] sets z-index to 30. *)

val z_40 : t
(** [z_40] sets z-index to 40. *)

val z_50 : t
(** [z_50] sets z-index to 50. *)

val z_auto : t
(** [z_auto] sets z-index to auto. *)

val object_contain : t
(** [object_contain] sets object-fit to contain. *)

val object_cover : t
(** [object_cover] sets object-fit to cover. *)

val object_fill : t
(** [object_fill] sets object-fit to fill. *)

val object_none : t
(** [object_none] sets object-fit to none. *)

val object_scale_down : t
(** [object_scale_down] sets object-fit to scale-down. *)

val object_center : t
(** [object_center] sets object-position to center. *)

val object_top : t
(** [object_top] sets object-position to top. *)

val object_bottom : t
(** [object_bottom] sets object-position to bottom. *)

val object_left : t
(** [object_left] sets object-position to left. *)

val object_right : t
(** [object_right] sets object-position to right. *)

(** {1 Float Utilities} *)

val float_left : t
(** [float_left] sets float to left. *)

val float_right : t
(** [float_right] sets float to right. *)

val float_none : t
(** [float_none] sets float to none. *)

val float_start : t
(** [float_start] sets float to inline-start. *)

val float_end : t
(** [float_end] sets float to inline-end. *)

val box_decoration_clone : t
(** [box_decoration_clone] sets box-decoration-break to clone. *)

val box_decoration_slice : t
(** [box_decoration_slice] sets box-decoration-break to slice. *)

module Handler : Utility.Handler
