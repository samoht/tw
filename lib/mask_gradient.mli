(** Mask gradient utilities *)

open Utility

module Handler : sig
  include Utility.Handler

  type value = Spacing of float | Percent of float | Arbitrary of string
  type radial_at_position = At_keyword of string | At_arbitrary of string
end

val mask_t_from : Handler.value -> t
(** [mask_t_from v] is the [mask-t-from-{v}] utility. *)

val mask_t_to : Handler.value -> t
(** [mask_t_to v] is the [mask-t-to-{v}] utility. *)

val mask_r_from : Handler.value -> t
(** [mask_r_from v] is the [mask-r-from-{v}] utility. *)

val mask_r_to : Handler.value -> t
(** [mask_r_to v] is the [mask-r-to-{v}] utility. *)

val mask_b_from : Handler.value -> t
(** [mask_b_from v] is the [mask-b-from-{v}] utility. *)

val mask_b_to : Handler.value -> t
(** [mask_b_to v] is the [mask-b-to-{v}] utility. *)

val mask_l_from : Handler.value -> t
(** [mask_l_from v] is the [mask-l-from-{v}] utility. *)

val mask_l_to : Handler.value -> t
(** [mask_l_to v] is the [mask-l-to-{v}] utility. *)

val mask_x_from : Handler.value -> t
(** [mask_x_from v] is the [mask-x-from-{v}] utility. *)

val mask_x_to : Handler.value -> t
(** [mask_x_to v] is the [mask-x-to-{v}] utility. *)

val mask_y_from : Handler.value -> t
(** [mask_y_from v] is the [mask-y-from-{v}] utility. *)

val mask_y_to : Handler.value -> t
(** [mask_y_to v] is the [mask-y-to-{v}] utility. *)

val mask_linear_from : Handler.value -> t
(** [mask_linear_from v] is the [mask-linear-from-{v}] utility. *)

val mask_linear_to : Handler.value -> t
(** [mask_linear_to v] is the [mask-linear-to-{v}] utility. *)

val mask_radial : t
(** [mask_radial] is the [mask-radial] utility. *)

val mask_radial_at : Handler.radial_at_position -> t
(** [mask_radial_at pos] is the [mask-radial-at-{pos}] utility. *)

val mask_radial_from : Handler.value -> t
(** [mask_radial_from v] is the [mask-radial-from-{v}] utility. *)

val mask_radial_to : Handler.value -> t
(** [mask_radial_to v] is the [mask-radial-to-{v}] utility. *)

val mask_conic_from : Handler.value -> t
(** [mask_conic_from v] is the [mask-conic-from-{v}] utility. *)

val mask_conic_to : Handler.value -> t
(** [mask_conic_to v] is the [mask-conic-to-{v}] utility. *)
