(** Combined Flexbox and Grid utilities following Tailwind v4 “Flexbox & Grid”.
    Exposes display, flex properties, grid properties, gap, and placement
    utilities. Values mirror Tailwind class names and behavior. *)

open Core

(** {1 Display Utilities} *)

val flex : t
(** [flex] sets display to flex. *)

val inline_flex : t
(** [inline_flex] sets display to inline-flex. *)

val grid : t
(** [grid] sets display to grid. *)

val inline_grid : t
(** [inline_grid] sets display to inline-grid. *)

(** {1 Flex Direction} *)

val flex_row : t
(** [flex_row] sets flex direction to row (default). *)

val flex_row_reverse : t
(** [flex_row_reverse] sets flex direction to row-reverse. *)

val flex_col : t
(** [flex_col] sets flex direction to column. *)

val flex_col_reverse : t
(** [flex_col_reverse] sets flex direction to column-reverse. *)

(** {1 Flex Wrap} *)

val flex_wrap : t
(** [flex_wrap] allows flex items to wrap. *)

val flex_wrap_reverse : t
(** [flex_wrap_reverse] allows flex items to wrap in reverse order. *)

val flex_nowrap : t
(** [flex_nowrap] prevents flex items from wrapping (default). *)

(** {1 Flex} *)

val flex_1 : t
(** [flex_1] sets flex to 1 1 0% (grow and shrink). *)

val flex_auto : t
(** [flex_auto] sets flex to 1 1 auto. *)

val flex_initial : t
(** [flex_initial] sets flex to 0 1 auto (default). *)

val flex_none : t
(** [flex_none] sets flex to none (don't grow or shrink). *)

(** {1 Flex Grow} *)

val flex_grow : t
(** [flex_grow] allows a flex item to grow. *)

val flex_grow_0 : t
(** [flex_grow_0] prevents a flex item from growing (default). *)

(** {1 Flex Shrink} *)

val flex_shrink : t
(** [flex_shrink] allows a flex item to shrink. *)

val flex_shrink_0 : t
(** [flex_shrink_0] prevents a flex item from shrinking. *)

(** {1 Flex Basis} *)

val basis_0 : t
(** [basis_0] sets flex-basis to 0. *)

val basis_1 : t
(** [basis_1] sets flex-basis to 0.25rem. *)

val basis_auto : t
(** [basis_auto] sets flex-basis to auto. *)

val basis_full : t
(** [basis_full] sets flex-basis to 100%. *)

(** {1 Order} *)

val order_1 : t
(** [order_1] sets order to 1. *)

val order_2 : t
(** [order_2] sets order to 2. *)

val order_3 : t
(** [order_3] sets order to 3. *)

val order_4 : t
(** [order_4] sets order to 4. *)

val order_5 : t
(** [order_5] sets order to 5. *)

val order_6 : t
(** [order_6] sets order to 6. *)

val order_first : t
(** [order_first] sets order to -9999. *)

val order_last : t
(** [order_last] sets order to 9999. *)

val order_none : t
(** [order_none] sets order to 0. *)

(** {1 Align Items} *)

val items_start : t
(** [items_start] aligns items to the start of the cross axis. *)

val items_end : t
(** [items_end] aligns items to the end of the cross axis. *)

val items_center : t
(** [items_center] centers items along the cross axis. *)

val items_baseline : t
(** [items_baseline] aligns items along their baseline. *)

val items_stretch : t
(** [items_stretch] stretches items to fill the container (default). *)

(** {1 Justify Content} *)

val justify_start : t
(** [justify_start] justifies items to the start of the main axis. *)

val justify_end : t
(** [justify_end] justifies items to the end of the main axis. *)

val justify_center : t
(** [justify_center] centers items along the main axis. *)

val justify_between : t
(** [justify_between] distributes items with space between them. *)

val justify_around : t
(** [justify_around] distributes items with space around them. *)

val justify_evenly : t
(** [justify_evenly] distributes items with equal space around them. *)

(** {1 Align Content} *)

val content_start : t
(** [content_start] aligns content to the start. *)

val content_end : t
(** [content_end] aligns content to the end. *)

val content_center : t
(** [content_center] centers content. *)

val content_between : t
(** [content_between] distributes content with space between. *)

val content_around : t
(** [content_around] distributes content with space around. *)

val content_evenly : t
(** [content_evenly] distributes content evenly. *)

val content_stretch : t
(** [content_stretch] stretches content. *)

(** {1 Align Self} *)

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

(** {1 Gap Utilities} *)

val gap : int -> t
(** [gap n] sets gap to [n] × 0.25rem. *)

val gap_x : int -> t
(** [gap_x n] sets column-gap to [n] × 0.25rem. *)

val gap_y : int -> t
(** [gap_y n] sets row-gap to [n] × 0.25rem. *)

val gap_px : t
(** [gap_px] sets gap to 1px. *)

val gap_full : t
(** [gap_full] sets gap to 100%. *)

(** {1 Grid Template Columns} *)

val grid_cols : int -> t
(** [grid_cols n] creates a grid with n equal columns. *)

val grid_cols_none : t
(** [grid_cols_none] removes grid template columns. *)

val grid_cols_subgrid : t
(** [grid_cols_subgrid] uses subgrid for columns. *)

(** {1 Grid Template Rows} *)

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

(** {1 Place/Justify Utilities} *)

val justify_items_start : t
(** [justify_items_start] justifies items to the start. *)

val justify_items_end : t
(** [justify_items_end] justifies items to the end. *)

val justify_items_center : t
(** [justify_items_center] justifies items to the center. *)

val justify_items_stretch : t
(** [justify_items_stretch] stretches items to fill. *)

val justify_self_auto : t
(** [justify_self_auto] uses automatic self justification. *)

val justify_self_start : t
(** [justify_self_start] justifies self to the start. *)

val justify_self_end : t
(** [justify_self_end] justifies self to the end. *)

val justify_self_center : t
(** [justify_self_center] justifies self to the center. *)

val justify_self_stretch : t
(** [justify_self_stretch] stretches self to fill. *)

val place_content_start : t
(** [place_content_start] places content at the start. *)

val place_content_end : t
(** [place_content_end] places content at the end. *)

val place_content_center : t
(** [place_content_center] places content at the center. *)

val place_content_between : t
(** [place_content_between] places content with space between. *)

val place_content_around : t
(** [place_content_around] places content with space around. *)

val place_content_evenly : t
(** [place_content_evenly] places content with space evenly. *)

val place_content_stretch : t
(** [place_content_stretch] stretches content to fill. *)

val place_items_start : t
(** [place_items_start] places items at the start. *)

val place_items_end : t
(** [place_items_end] places items at the end. *)

val place_items_center : t
(** [place_items_center] places items at the center. *)

val place_items_stretch : t
(** [place_items_stretch] stretches items to fill. *)

val place_self_auto : t
(** [place_self_auto] uses automatic self placement. *)

val place_self_start : t
(** [place_self_start] places self at the start. *)

val place_self_end : t
(** [place_self_end] places self at the end. *)

val place_self_center : t
(** [place_self_center] places self at the center. *)

val place_self_stretch : t
(** [place_self_stretch] stretches self to fill. *)

(** {1 Parsing} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a Flow (flex/grid) utility from string parts. *)
