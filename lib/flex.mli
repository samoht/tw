(** Flexbox display utilities (flex, inline-flex).

    For flexbox property utilities (direction, wrap, grow, shrink, basis,
    order), see {!Flex_props} module.

    @see <https://tailwindcss.com/docs/flex> Tailwind CSS Flex documentation *)

open Utility

(** {1 Display} *)

val flex : t
(** [flex] creates a flex container. *)

val inline_flex : t
(** [inline_flex] creates an inline flex container. *)

(** {1 Re-exported from Flex_props}

    These utilities are re-exported for backward compatibility. New code should
    use {!Flex_props} directly. *)

(** {2 Direction} *)

val flex_row : t
(** [flex_row] arranges flex items horizontally. See {!Flex_props.flex_row}. *)

val flex_row_reverse : t
(** [flex_row_reverse] arranges flex items horizontally but reversed. See
    {!Flex_props.flex_row_reverse}. *)

val flex_col : t
(** [flex_col] arranges flex items vertically. See {!Flex_props.flex_col}. *)

val flex_col_reverse : t
(** [flex_col_reverse] arranges flex items vertically but reversed. See
    {!Flex_props.flex_col_reverse}. *)

(** {2 Wrap} *)

val flex_wrap : t
(** [flex_wrap] allows flex items to wrap. See {!Flex_props.flex_wrap}. *)

val flex_wrap_reverse : t
(** [flex_wrap_reverse] wraps flex items in reverse. See
    {!Flex_props.flex_wrap_reverse}. *)

val flex_nowrap : t
(** [flex_nowrap] prevents flex items from wrapping. See
    {!Flex_props.flex_nowrap}. *)

(** {2 Flex Shortcuts} *)

val flex_1 : t
(** [flex_1] sets flex: 1 1 0%. See {!Flex_props.flex_1}. *)

val flex_auto : t
(** [flex_auto] sets flex: 1 1 auto. See {!Flex_props.flex_auto}. *)

val flex_initial : t
(** [flex_initial] sets flex: 0 1 auto. See {!Flex_props.flex_initial}. *)

val flex_none : t
(** [flex_none] sets flex: none. See {!Flex_props.flex_none}. *)

(** {2 Grow} *)

val flex_grow : t
(** [flex_grow] allows a flex item to grow. See {!Flex_props.flex_grow}. *)

val flex_grow_0 : t
(** [flex_grow_0] prevents a flex item from growing. See
    {!Flex_props.flex_grow_0}. *)

(** {2 Shrink} *)

val flex_shrink : t
(** [flex_shrink] allows a flex item to shrink. See {!Flex_props.flex_shrink}.
*)

val flex_shrink_0 : t
(** [flex_shrink_0] prevents a flex item from shrinking. See
    {!Flex_props.flex_shrink_0}. *)

(** {2 Basis} *)

val basis_0 : t
(** [basis_0] sets flex-basis to 0. See {!Flex_props.basis_0}. *)

val basis_1 : t
(** [basis_1] sets flex-basis to 100%. See {!Flex_props.basis_1}. *)

val basis_auto : t
(** [basis_auto] sets flex-basis to auto. See {!Flex_props.basis_auto}. *)

val basis_full : t
(** [basis_full] sets flex-basis to 100%. See {!Flex_props.basis_full}. *)

(** {2 Order} *)

val order : int -> t
(** [order n] sets order to n. See {!Flex_props.order}. *)

val order_first : t
(** [order_first] sets order to -9999. See {!Flex_props.order_first}. *)

val order_last : t
(** [order_last] sets order to 9999. See {!Flex_props.order_last}. *)

val order_none : t
(** [order_none] sets order to 0. See {!Flex_props.order_none}. *)

(** {1 Internal types} *)

module Handler : Utility.Handler
