(** Gradient utilities: linear, radial and conic gradient images, and the
    from/via/to colour stops they read. The other background utilities are in
    {!Backgrounds}.

    https://tailwindcss.com/docs/background-image
    https://tailwindcss.com/docs/gradient-color-stops *)

open Cascade
open Utility

(** {1 Gradient Direction} *)

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

(** {1 Gradient Color Stops} *)

val from_color : ?shade:int -> Color.color -> t
(** [from_color ?shade color] sets the gradient "from" color (start stop).
    [shade] selects a color shade (e.g., 50..900) when using Tailwind colors. *)

val via_color : ?shade:int -> Color.color -> t
(** [via_color ?shade color] sets the gradient "via" color (middle stop). *)

val to_color : ?shade:int -> Color.color -> t
(** [to_color ?shade color] sets the gradient "to" color (end stop). *)

module Handler : sig
  include Utility.Handler

  (** Gradient variables for use by other modules (e.g., transition-colors). *)

  val gradient_from_var : Css.color Var.property_default
  (** [gradient_from_var] is the [--tw-gradient-from] variable. *)

  val gradient_via_var : Css.color Var.property_default
  (** [gradient_via_var] is the [--tw-gradient-via] variable. *)

  val gradient_to_var : Css.color Var.property_default
  (** [gradient_to_var] is the [--tw-gradient-to] variable. *)

  val owns_bg_class : string -> bool
  (** [owns_bg_class c] is [true] when [c] has the shape of a [bg-] gradient
      utility ([bg-linear-*], [bg-radial*], [bg-conic*], [bg-gradient-to-*]),
      whether or not its value reads. {!Backgrounds} refuses such a class, so a
      theme colour spelled the same way does not turn it into a background
      colour. *)
end
