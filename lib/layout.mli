(** Layout utilities for basic display, positioning, and object properties *)

(** {1 Utility Types} *)

type utility

val of_string : string list -> (utility, [ `Msg of string ]) result
(** [of_string parts] parses a layout utility from string parts. Returns an
    internal structured representation. *)

(** {1 Internal Conversion Functions} *)

val to_style : utility -> Style.t
(** [to_style u] converts a structured layout utility to a style. For internal
    use by the Tw module. *)

val suborder : utility -> int
(** [suborder u] returns the ordering value for layout utility [u]. Used for
    deterministic CSS output ordering. *)

(** {1 Display Utilities} *)

val block : Style.t
val inline : Style.t
val inline_block : Style.t
val hidden : Style.t
val sr_only : Style.t
val not_sr_only : Style.t
val static : Style.t
val relative : Style.t
val absolute : Style.t
val fixed : Style.t
val sticky : Style.t
val visible : Style.t
val invisible : Style.t
val collapse : Style.t
val isolate : Style.t
val overflow_auto : Style.t
val overflow_hidden : Style.t
val overflow_clip : Style.t
val overflow_visible : Style.t
val overflow_scroll : Style.t
val overflow_x_auto : Style.t
val overflow_x_hidden : Style.t
val overflow_x_visible : Style.t
val overflow_x_scroll : Style.t
val overflow_y_auto : Style.t
val overflow_y_hidden : Style.t
val overflow_y_visible : Style.t
val overflow_y_scroll : Style.t
val z_0 : Style.t
val z_10 : Style.t
val z_20 : Style.t
val z_30 : Style.t
val z_40 : Style.t
val z_50 : Style.t
val z_auto : Style.t
val object_contain : Style.t
val object_cover : Style.t
val object_fill : Style.t
val object_none : Style.t
val object_scale_down : Style.t
val object_center : Style.t
val object_top : Style.t
val object_bottom : Style.t
val object_left : Style.t
val object_right : Style.t
val border_collapse : Style.t
val border_separate : Style.t
val border_spacing : int -> Style.t
val table_auto : Style.t
val table_fixed : Style.t

(** Legacy string-based functions for backward compatibility *)

val position_suborder : string -> int
(** [position_suborder class_name] returns the suborder for position utilities.
*)

val display_suborder : string -> int
(** [display_suborder class_name] returns the suborder for display utilities. *)
