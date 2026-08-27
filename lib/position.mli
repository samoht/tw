(** Position utilities for controlling element placement

    https://tailwindcss.com/docs/position
    https://tailwindcss.com/docs/top-right-bottom-left *)

open Utility

(** {1 Position Utilities} *)

val static : t
(** [static] sets position to static. *)

val relative : t
(** [relative] sets position to relative. *)

val absolute : t
(** [absolute] sets position to absolute. *)

val fixed : t
(** [fixed] sets position to fixed. *)

val sticky : t
(** [sticky] sets position to sticky. *)

val inset : int -> t
(** [inset n] sets all inset values. *)

val inset' : float -> t
(** [inset' n] is {!inset} for a half-step scale value (e.g. [inset' 0.5] for
    [inset-0.5]). *)

val inset_0 : t
(** [inset_0] sets all inset values to 0. *)

val inset_x : int -> t
(** [inset_x n] sets horizontal inset (left and right). *)

val inset_x' : float -> t
(** [inset_x' n] is {!inset_x} for a half-step scale value. *)

val inset_x_0 : t
(** [inset_x_0] sets horizontal inset to 0. *)

val inset_y : int -> t
(** [inset_y n] sets vertical inset (top and bottom). *)

val inset_y' : float -> t
(** [inset_y' n] is {!inset_y} for a half-step scale value. *)

val inset_y_0 : t
(** [inset_y_0] sets vertical inset to 0. *)

val top : int -> t
(** [top n] sets top position. *)

val top' : float -> t
(** [top' n] is {!top} for a half-step scale value. *)

val right : int -> t
(** [right n] sets right position. *)

val right' : float -> t
(** [right' n] is {!right} for a half-step scale value. *)

val bottom : int -> t
(** [bottom n] sets bottom position. *)

val bottom' : float -> t
(** [bottom' n] is {!bottom} for a half-step scale value. *)

val left : int -> t
(** [left n] sets left position. *)

val left' : float -> t
(** [left' n] is {!left} for a half-step scale value. *)

val top_1_2 : t
(** [top_1_2] sets top to 50%. *)

val left_1_2 : t
(** [left_1_2] sets left to 50%. *)

module Handler : Utility.Handler
