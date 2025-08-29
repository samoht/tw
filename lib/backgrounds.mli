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

val from_color : ?shade:int -> Color.color -> t
(** [from_color ?shade color] sets the gradient "from" color (start stop).
    [shade] selects a color shade (e.g., 50..900) when using Tailwind colors. *)

val via_color : ?shade:int -> Color.color -> t
(** [via_color ?shade color] sets the gradient "via" color (middle stop). *)

val to_color : ?shade:int -> Color.color -> t
(** [to_color ?shade color] sets the gradient "to" color (end stop). *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** Parse gradient and background utilities from string parts *)
