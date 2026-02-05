(** Overscroll behavior utilities *)

open Utility

val overscroll_auto : t
(** [overscroll_auto] uses default scroll chaining behavior. *)

val overscroll_contain : t
(** [overscroll_contain] prevents scroll chaining to parent elements. *)

val overscroll_none : t
(** [overscroll_none] prevents scroll chaining and overscroll effects. *)

val overscroll_x_auto : t
(** [overscroll_x_auto] uses default horizontal overscroll behavior. *)

val overscroll_x_contain : t
(** [overscroll_x_contain] prevents horizontal scroll chaining. *)

val overscroll_x_none : t
(** [overscroll_x_none] prevents horizontal scroll chaining and effects. *)

val overscroll_y_auto : t
(** [overscroll_y_auto] uses default vertical overscroll behavior. *)

val overscroll_y_contain : t
(** [overscroll_y_contain] prevents vertical scroll chaining. *)

val overscroll_y_none : t
(** [overscroll_y_none] prevents vertical scroll chaining and effects. *)

module Handler : Utility.Handler
