(** Background and gradient utilities *)

open Core

val bg_gradient_to_b : t
val bg_gradient_to_br : t
val bg_gradient_to_t : t
val bg_gradient_to_tr : t
val bg_gradient_to_r : t
val bg_gradient_to_bl : t
val bg_gradient_to_l : t
val bg_gradient_to_tl : t
val from_color : ?shade:int -> Color.t -> t
val via_color : ?shade:int -> Color.t -> t
val to_color : ?shade:int -> Color.t -> t
