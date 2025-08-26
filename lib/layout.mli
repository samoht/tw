(** Layout utilities for display, flexbox, grid, and positioning *)

open Core

(** {1 Display Utilities} *)

val block : t
(** [block] sets display to block. *)

val inline : t
(** [inline] sets display to inline. *)

val inline_block : t
(** [inline_block] sets display to inline-block. *)

val hidden : t
(** [hidden] sets display to none. *)

val flex : t
(** [flex] sets display to flex. *)

val inline_flex : t
(** [inline_flex] sets display to inline-flex. *)

val grid : t
(** [grid] sets display to grid. *)

val inline_grid : t
(** [inline_grid] sets display to inline-grid. *)

(* TODO: flow_root not supported by Css module *)

val flex_row : t
(** [flex_row] sets flex direction to row. *)

val flex_row_reverse : t
(** [flex_row_reverse] sets flex direction to row-reverse. *)

val flex_col : t
(** [flex_col] sets flex direction to column. *)

val flex_col_reverse : t
(** [flex_col_reverse] sets flex direction to column-reverse. *)

val flex_wrap : t
(** [flex_wrap] enables wrapping on flex items. *)

val flex_wrap_reverse : t
(** [flex_wrap_reverse] enables reverse wrapping on flex items. *)

val flex_nowrap : t
(** [flex_nowrap] disables wrapping on flex items. *)

val flex_1 : t
(** [flex_1] sets flex: 1 1 0%. *)

val flex_auto : t
(** [flex_auto] sets flex: 1 1 auto. *)

val flex_initial : t
(** [flex_initial] sets flex: 0 1 auto. *)

val flex_none : t
(** [flex_none] sets flex: none. *)

val flex_grow : t
(** [flex_grow] sets flex-grow: 1. *)

val flex_grow_0 : t
(** [flex_grow_0] sets flex-grow: 0. *)

val flex_shrink : t
(** [flex_shrink] sets flex-shrink: 1. *)

val flex_shrink_0 : t
(** [flex_shrink_0] sets flex-shrink: 0. *)

val items_start : t
(** [items_start] aligns items to flex-start. *)

val items_end : t
(** [items_end] aligns items to flex-end. *)

val items_center : t
(** [items_center] aligns items to center. *)

val items_baseline : t
(** [items_baseline] aligns items to baseline. *)

val items_stretch : t
(** [items_stretch] aligns items to stretch. *)

val justify_start : t
(** [justify_start] justifies content to start. *)

val justify_end : t
(** [justify_end] justifies content to end. *)

val justify_center : t
(** [justify_center] justifies content to center. *)

val justify_between : t
(** [justify_between] distributes space between items. *)

val justify_around : t
(** [justify_around] distributes space around items. *)

val justify_evenly : t
(** [justify_evenly] distributes space evenly. *)

val content_start : t
(** [content_start] aligns content to start. *)

val content_end : t
(** [content_end] aligns content to end. *)

val content_center : t
(** [content_center] aligns content to center. *)

val content_between : t
(** [content_between] distributes content with space between. *)

val content_around : t
(** [content_around] distributes content with space around. *)

val content_evenly : t
(** [content_evenly] distributes content evenly. *)

val self_auto : t
(** [self_auto] sets align-self to auto. *)

val self_start : t
(** [self_start] sets align-self to flex-start. *)

val self_end : t
(** [self_end] sets align-self to flex-end. *)

val self_center : t
(** [self_center] sets align-self to center. *)

val self_baseline : t
(** [self_baseline] sets align-self to baseline. *)

val self_stretch : t
(** [self_stretch] sets align-self to stretch. *)

val content_stretch : t
(** [content_stretch] sets align-content to stretch. *)

(** {2 Place Content (Grid Shorthand)} *)

val place_content_start : t
(** Sets place-content to start. *)

val place_content_end : t
(** Sets place-content to end. *)

val place_content_center : t
(** [place_content_center] sets place-content to center. *)

val place_content_between : t
(** [place_content_between] distributes place-content with space between. *)

val place_content_around : t
(** [place_content_around] distributes place-content with space around. *)

val place_content_evenly : t
(** [place_content_evenly] distributes place-content evenly. *)

val place_content_stretch : t
(** [place_content_stretch] sets place-content to stretch. *)

(** {2 Place Items (Grid Shorthand)} *)

val place_items_start : t
(** Sets place-items to start. *)

val place_items_end : t
(** Sets place-items to end. *)

val place_items_center : t
(** [place_items_center] sets place-items to center. *)

val place_items_stretch : t
(** [place_items_stretch] sets place-items to stretch. *)

(** {2 Justify Self (Grid Items)} *)

val justify_self_auto : t
(** Sets justify-self to auto. *)

val justify_self_start : t
(** Sets justify-self to start. *)

val justify_self_end : t
(** Sets justify-self to end. *)

val justify_self_center : t
(** Sets justify-self to center. *)

val justify_self_stretch : t
(** Sets justify-self to stretch. *)

(** {2 Place Self (Grid Shorthand)} *)

val place_self_auto : t
(** Sets place-self to auto. *)

val place_self_start : t
(** Sets place-self to start. *)

val place_self_end : t
(** Sets place-self to end. *)

val place_self_center : t
(** Sets place-self to center. *)

val place_self_stretch : t
(** Sets place-self to stretch. *)

(** {1 Positioning Utilities} *)

val static : t
(** [static] sets position to static. *)

val relative : t
(** [relative] sets position to relative. *)

val absolute : t
(** [absolute] sets position to absolute. *)

val fixed : t
(** [fixed] sets position to fixed. *)

val sticky : t
(** [sticky] sets position to sticky. *)

val overflow_auto : t
(** [overflow_auto] sets overflow to auto. *)

val overflow_hidden : t
(** [overflow_hidden] sets overflow to hidden. *)

val overflow_visible : t
(** [overflow_visible] sets overflow to visible. *)

val overflow_scroll : t
(** [overflow_scroll] sets overflow to scroll. *)

val overflow_x_auto : t
(** [overflow_x_auto] sets overflow-x to auto. *)

val overflow_x_hidden : t
(** [overflow_x_hidden] sets overflow-x to hidden. *)

val overflow_x_visible : t
(** [overflow_x_visible] sets overflow-x to visible. *)

val overflow_x_scroll : t
(** [overflow_x_scroll] sets overflow-x to scroll. *)

val overflow_y_auto : t
(** [overflow_y_auto] sets overflow-y to auto. *)

val overflow_y_hidden : t
(** [overflow_y_hidden] sets overflow-y to hidden. *)

val overflow_y_visible : t
(** [overflow_y_visible] sets overflow-y to visible. *)

val overflow_y_scroll : t
(** [overflow_y_scroll] sets overflow-y to scroll. *)

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

(* z_auto not supported by Css module *)

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
(** [object_top] positions replaced content to top. *)

val object_bottom : t
(** [object_bottom] positions replaced content to bottom. *)

val object_left : t
(** [object_left] positions replaced content to left. *)

val object_right : t
(** [object_right] positions replaced content to right. *)

(** {1 Table Utilities} *)

val border_collapse : t
(** Sets table border-collapse to collapse. *)

val border_separate : t
(** [border_separate] sets table border-collapse to separate. *)

val border_spacing : int -> t
(** [border_spacing n] sets uniform table border-spacing. *)

val table_auto : t
(** [table_auto] sets table-layout to auto. *)

val table_fixed : t
(** [table_fixed] sets table-layout to fixed. *)

(** {1 Grid Utilities} *)

val grid_cols : int -> t
(** [grid_cols n] sets the number of grid template columns. *)

val grid_rows : int -> t
(** [grid_rows n] sets the number of grid template rows. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a layout utility from string parts. *)
