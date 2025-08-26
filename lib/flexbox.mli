(** Flexbox utilities for controlling flex layout *)

open Core

(** {1 Display Utilities} *)

val flex : t
(** [flex] sets display to flex. *)

val inline_flex : t
(** [inline_flex] sets display to inline-flex. *)

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

(** {1 Gap} *)

val gap : int -> t
(** [gap n] sets the gap between grid/flex items. *)

val gap_x : int -> t
(** [gap_x n] sets the horizontal gap between grid/flex items. *)

val gap_y : int -> t
(** [gap_y n] sets the vertical gap between grid/flex items. *)

(** {1 String Parsing} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a list of strings into a flexbox utility. *)
