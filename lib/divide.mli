(** Divide utilities for creating gaps between child elements

    @see <https://tailwindcss.com/docs/divide-width>
      Tailwind CSS Divide Width documentation *)

module Css = Cascade.Css
open Utility

(** {1 Divide Reverse Utilities} *)

val divide_x_reverse : t
(** [divide_x_reverse] reverses the direction of horizontal divide borders
    (useful for RTL layouts). *)

val divide_y_reverse : t
(** [divide_y_reverse] reverses the direction of vertical divide borders. *)

(** {1 Divide Width Utilities} *)

val divide_x : int -> t
(** [divide_x n] sets the border width between horizontal children, in pixels:
    [divide_x 2] is [divide-x-2]. *)

val divide_y : int -> t
(** [divide_y n] sets the border width between vertical children. *)

val divide_x_length : Css.border_width -> t
(** [divide_x_length w] is {!val-divide_x} with an arbitrary width, as
    [divide-x-[3px]]. *)

val divide_y_length : Css.border_width -> t
(** [divide_y_length w] is {!val-divide_y} with an arbitrary width. *)

(** {1 Divide Colour Utilities} *)

val divide_color : ?opacity:int -> ?shade:int -> Color.color -> t
(** [divide_color color] sets the colour of the dividing borders. [shade]
    defaults to 500; [opacity] is the alpha modifier (0-100). *)

val divide_transparent : t
(** [divide_transparent] makes the dividing borders transparent. *)

val divide_current : t
(** [divide_current] takes the dividing border colour from [currentColor]. *)

val divide_inherit : t
(** [divide_inherit] inherits the dividing border colour. *)

(** {1 Divide Style Utilities} *)

val divide_style : Css.border_style -> t
(** [divide_style s] sets the style of the dividing borders, as [divide-dashed].
*)

module Handler : Utility.Handler
