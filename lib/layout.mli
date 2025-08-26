(** Layout utilities for basic display, positioning, and object properties *)

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

(* TODO: flow_root not supported by Css module *)

val sr_only : t
(** [sr_only] visually hides content but keeps it accessible to screen readers.
*)

val not_sr_only : t
(** [not_sr_only] reverses [sr_only], making content visible. *)

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

(** {1 Visibility Utilities} *)

val visible : t
(** [visible] sets visibility to visible. *)

val invisible : t
(** [invisible] sets visibility to hidden. *)

val collapse : t
(** [collapse] sets visibility to collapse. *)

(** {1 Isolation} *)

val isolate : t
(** [isolate] creates a new stacking context (isolation: isolate). *)

val overflow_auto : t
(** [overflow_auto] sets overflow to auto. *)

val overflow_hidden : t
(** [overflow_hidden] sets overflow to hidden. *)

val overflow_clip : t
(** [overflow_clip] sets overflow to clip. *)

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
(** [object_top] positions replaced content to top. *)

val object_bottom : t
(** [object_bottom] positions replaced content to bottom. *)

val object_left : t
(** [object_left] positions replaced content to left. *)

val object_right : t
(** [object_right] positions replaced content to right. *)

(** {1 Table Utilities} *)

val border_collapse : t
(** [border_collapse] sets table border-collapse to collapse. *)

val border_separate : t
(** [border_separate] sets table border-collapse to separate. *)

val border_spacing : int -> t
(** [border_spacing n] sets uniform table border-spacing. *)

val table_auto : t
(** [table_auto] sets table-layout to auto. *)

val table_fixed : t
(** [table_fixed] sets table-layout to fixed. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a layout utility from string parts. *)
