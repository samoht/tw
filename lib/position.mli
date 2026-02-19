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

val inset_0 : t
(** [inset_0] sets all inset values to 0. *)

val inset_x_0 : t
(** [inset_x_0] sets horizontal inset to 0. *)

val inset_y_0 : t
(** [inset_y_0] sets vertical inset to 0. *)

val top : int -> t
(** [top n] sets top position. *)

val right : int -> t
(** [right n] sets right position. *)

val bottom : int -> t
(** [bottom n] sets bottom position. *)

val left : int -> t
(** [left n] sets left position. *)

val top_1_2 : t
(** [top_1_2] sets top to 50%. *)

val left_1_2 : t
(** [left_1_2] sets left to 50%. *)

module Handler : Utility.Handler
