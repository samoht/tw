(** Flexbox property utilities (grow, shrink, basis, order, flex shortcuts).

    These utilities control flexbox item behavior and come after sizing
    utilities in the cascade order. For flex display utilities (flex,
    inline-flex), see {!Flex} module. For direction/wrap utilities, see
    {!Flex_layout} module.

    @see <https://tailwindcss.com/docs/flex-grow> Flex Grow
    @see <https://tailwindcss.com/docs/flex-shrink> Flex Shrink *)

open Utility

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

(** {1 Order} *)

val order : int -> t
(** [order n] sets order to n. *)

val order_first : t
(** [order_first] sets order to -9999. *)

val order_last : t
(** [order_last] sets order to 9999. *)

val order_none : t
(** [order_none] sets order to 0. *)

(** {1 Internal types} *)

module Handler : Utility.Handler
