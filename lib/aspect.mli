(** Aspect ratio utilities *)

open Core

val aspect_auto : t
val aspect_square : t
val aspect_video : t
val aspect_ratio : int -> int -> t
