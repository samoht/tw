(** Flexbox utilities

    @see <https://tailwindcss.com/docs/flex> Tailwind CSS Flex documentation
    @see <https://tailwindcss.com/docs/flex-direction> Flex Direction
    @see <https://tailwindcss.com/docs/flex-wrap> Flex Wrap
    @see <https://tailwindcss.com/docs/flex-grow> Flex Grow
    @see <https://tailwindcss.com/docs/flex-shrink> Flex Shrink *)

open Utility

(** {1 Display} *)

val flex : t
(** [flex] creates a flex container. *)

val inline_flex : t
(** [inline_flex] creates an inline flex container. *)

(** {1 Direction} *)

val flex_row : t
(** [flex_row] arranges flex items horizontally (left to right). *)

val flex_row_reverse : t
(** [flex_row_reverse] arranges flex items horizontally but reversed. *)

val flex_col : t
(** [flex_col] arranges flex items vertically (top to bottom). *)

val flex_col_reverse : t
(** [flex_col_reverse] arranges flex items vertically but reversed. *)

(** {1 Wrap} *)

val flex_wrap : t
(** [flex_wrap] allows flex items to wrap onto multiple lines. *)

val flex_wrap_reverse : t
(** [flex_wrap_reverse] wraps flex items onto multiple lines in reverse order.
*)

val flex_nowrap : t
(** [flex_nowrap] prevents flex items from wrapping. *)

(** {1 Flex Shortcuts} *)

val flex_1 : t
(** [flex_1] sets flex: 1 1 0% (grow, shrink, basis). *)

val flex_auto : t
(** [flex_auto] sets flex: 1 1 auto. *)

val flex_initial : t
(** [flex_initial] sets flex: 0 1 auto. *)

val flex_none : t
(** [flex_none] sets flex: none. *)

(** {1 Grow} *)

val flex_grow : t
(** [flex_grow] allows a flex item to grow. *)

val flex_grow_0 : t
(** [flex_grow_0] prevents a flex item from growing. *)

(** {1 Shrink} *)

val flex_shrink : t
(** [flex_shrink] allows a flex item to shrink. *)

val flex_shrink_0 : t
(** [flex_shrink_0] prevents a flex item from shrinking. *)

(** {1 Basis} *)

val basis_0 : t
(** [basis_0] sets flex-basis to 0. *)

val basis_1 : t
(** [basis_1] sets flex-basis to 100%. *)

val basis_auto : t
(** [basis_auto] sets flex-basis to auto. *)

val basis_full : t
(** [basis_full] sets flex-basis to 100%. *)

val order : int -> t
(** [order n] sets order to n. *)

val order_first : t
(** [order_first] sets order to -9999. *)

val order_last : t
(** [order_last] sets order to 9999. *)

val order_none : t
(** [order_none] sets order to 0. *)

(** {1 Internal types} *)

module Handler : sig
  type t

  val of_string : string list -> (t, [ `Msg of string ]) result
  (** [of_string parts] parses string parts into a flex utility. For internal
      use by the Tw module. *)

  val suborder : t -> int
  (** [suborder u] returns the ordering value for flex utility [u]. Used for
      deterministic CSS output ordering. *)

  val to_style : t -> Style.t
  val order : t -> int * int
end
