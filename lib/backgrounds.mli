(** Background and gradient utilities

    https://tailwindcss.com/docs/background-image
    https://tailwindcss.com/docs/gradient-color-stops *)

open Utility

(** {1 Utility Types} *)

type direction =
  | Bottom
  | Bottom_right
  | Right
  | Top_right
  | Top
  | Top_left
  | Left
  | Bottom_left

val bg : Color.color -> int -> t
(** [bg color shade] sets the background color. [shade] selects a color shade
    (e.g., 50..900) when using Tailwind colors. *)

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

module Handler : sig
  include Utility.Handler

  (** Gradient variables for use by other modules (e.g., transition-colors) *)

  val gradient_from_var : Css.color Var.property_default
  val gradient_via_var : Css.color Var.property_default
  val gradient_to_var : Css.color Var.property_default
end
