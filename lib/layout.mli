(** Layout utilities for display, flexbox, grid, and positioning *)

open Core

(** {1 Display Utilities} *)

val block : t
val inline : t
val inline_block : t
val hidden : t
val flex : t
val inline_flex : t
val grid : t
val inline_grid : t
(* flow_root not supported by Css module *)

(** {1 Flexbox Utilities} *)

val flex_row : t
val flex_row_reverse : t
val flex_col : t
val flex_col_reverse : t
val flex_wrap : t
val flex_wrap_reverse : t
val flex_nowrap : t
val flex_1 : t
val flex_auto : t
val flex_initial : t
val flex_none : t
val flex_grow : t
val flex_grow_0 : t
val flex_shrink : t
val flex_shrink_0 : t

(** {1 Alignment Utilities} *)

val items_start : t
val items_end : t
val items_center : t
val items_baseline : t
val items_stretch : t
val justify_start : t
val justify_end : t
val justify_center : t
val justify_between : t
val justify_around : t
val justify_evenly : t
val content_start : t
val content_end : t
val content_center : t
val content_between : t
val content_around : t
val content_evenly : t
val self_auto : t
val self_start : t
val self_end : t
val self_center : t
val self_baseline : t
val self_stretch : t
(* Justify Self utilities not supported by Css module *)
(* Place Content utilities not supported by Css module *)
(* Place Items utilities not supported by Css module *)
(* Place Self utilities not supported by Css module *)

(** {1 Positioning Utilities} *)

val static : t
val relative : t
val absolute : t
val fixed : t
val sticky : t

(** {1 Overflow Utilities} *)

val overflow_auto : t
val overflow_hidden : t
val overflow_visible : t
val overflow_scroll : t
val overflow_x_auto : t
val overflow_x_hidden : t
val overflow_x_visible : t
val overflow_x_scroll : t
val overflow_y_auto : t
val overflow_y_hidden : t
val overflow_y_visible : t
val overflow_y_scroll : t

(** {1 Z-Index Utilities} *)

val z_0 : t
val z_10 : t
val z_20 : t
val z_30 : t
val z_40 : t
val z_50 : t
(* z_auto not supported by Css module *)

(** {1 Object Fit Utilities} *)

val object_contain : t
val object_cover : t
val object_fill : t
val object_none : t
val object_scale_down : t

(** {1 Object Position Utilities} *)

val object_center : t
val object_top : t
val object_bottom : t
val object_left : t
val object_right : t
