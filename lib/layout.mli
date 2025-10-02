(** Layout utilities for basic display, positioning, and object properties

    https://tailwindcss.com/docs/display https://tailwindcss.com/docs/visibility
    https://tailwindcss.com/docs/overflow https://tailwindcss.com/docs/z-index
    https://tailwindcss.com/docs/object-fit
    https://tailwindcss.com/docs/object-position
    https://tailwindcss.com/docs/border-collapse
    https://tailwindcss.com/docs/table-layout *)

open Utility

(** {1 Display Utilities} *)

val block : t
val inline : t
val inline_block : t
val hidden : t
val sr_only : t
val not_sr_only : t
val visible : t
val invisible : t
val collapse : t
val isolate : t
val overflow_auto : t
val overflow_hidden : t
val overflow_clip : t
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
val z_10 : t
val z_20 : t
val z_30 : t
val z_40 : t
val z_50 : t
val z_auto : t
val object_contain : t
val object_cover : t
val object_fill : t
val object_none : t
val object_scale_down : t
val object_center : t
val object_top : t
val object_bottom : t
val object_left : t
val object_right : t
val border_collapse : t
val border_separate : t
val border_spacing : int -> t
val table_auto : t
val table_fixed : t

module Handler : sig
  type t

  val of_string : string list -> (t, [ `Msg of string ]) result
  (** [of_string parts] parses a layout utility from string parts. Returns an
      internal structured representation. *)

  val suborder : t -> int
  (** [suborder u] returns the ordering value for layout utility [u]. Used for
      deterministic CSS output ordering. *)

  val to_style : t -> Style.t
  val order : t -> int * int
end

module Private : sig
  type t

  val of_string : string list -> (t, [ `Msg of string ]) result
  val suborder : t -> int
end
