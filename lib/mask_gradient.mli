(** Mask gradient utilities *)

open Utility

module Handler : sig
  include Utility.Handler

  type value = Spacing of float | Percent of float | Arbitrary of string
  type radial_at_position = At_keyword of string | At_arbitrary of string
end

val mask_t_from : Handler.value -> t
val mask_t_to : Handler.value -> t
val mask_r_from : Handler.value -> t
val mask_r_to : Handler.value -> t
val mask_b_from : Handler.value -> t
val mask_b_to : Handler.value -> t
val mask_l_from : Handler.value -> t
val mask_l_to : Handler.value -> t
val mask_x_from : Handler.value -> t
val mask_x_to : Handler.value -> t
val mask_y_from : Handler.value -> t
val mask_y_to : Handler.value -> t
val mask_linear_from : Handler.value -> t
val mask_linear_to : Handler.value -> t
val mask_radial : t
val mask_radial_at : Handler.radial_at_position -> t
val mask_radial_from : Handler.value -> t
val mask_radial_to : Handler.value -> t
val mask_conic_from : Handler.value -> t
val mask_conic_to : Handler.value -> t
