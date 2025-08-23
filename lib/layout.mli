(** Layout utilities for display, flexbox, grid, and positioning *)

open Core

(** {1 Display Utilities} *)

val block : t
(** [block] sets display to block. *)

val inline : t
val inline_block : t
val hidden : t
val flex : t
val inline_flex : t
val grid : t
val inline_grid : t

(* flow_root not supported by Css module *)

val flex_row : t
(** [flex_row] sets flex direction to row. *)

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

val items_start : t
(** [items_start] aligns items to flex-start. *)

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
(** [static] sets position to static. *)

val relative : t
val absolute : t
val fixed : t
val sticky : t

val overflow_auto : t
(** [overflow_auto] sets overflow to auto. *)

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

val z_0 : t
(** [z_0] sets z-index to 0. *)

val z_10 : t
val z_20 : t
val z_30 : t
val z_40 : t
val z_50 : t

(* z_auto not supported by Css module *)

val object_contain : t
(** [object_contain] sets object-fit to contain. *)

val object_cover : t
val object_fill : t
val object_none : t
val object_scale_down : t

val object_center : t
(** [object_center] sets object-position to center. *)

val object_top : t
val object_bottom : t
val object_left : t
val object_right : t

(** {1 Grid Utilities} *)

val grid_cols : int -> t
(** [grid_cols n] sets the number of grid template columns. *)

val grid_rows : int -> t
(** [grid_rows n] sets the number of grid template rows. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a layout utility from string parts. *)
