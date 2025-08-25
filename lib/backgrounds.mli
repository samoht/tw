(** Background and gradient utilities *)

open Core

type direction =
  | Bottom
  | Bottom_right
  | Right
  | Top_right
  | Top
  | Top_left
  | Left
  | Bottom_left

val bg_gradient_to : direction -> t
(** [bg_gradient_to dir] sets gradient direction. Prefer this typed variant over
    the fixed functions when composing logic. *)

val from_color : ?shade:int -> Color.t -> t
val via_color : ?shade:int -> Color.t -> t
val to_color : ?shade:int -> Color.t -> t
