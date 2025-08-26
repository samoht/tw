(** Aspect ratio utilities *)

open Core

val aspect_auto : t
(** Uses the browser's default aspect-ratio (auto). *)

val aspect_square : t
(** Constrains element to a 1:1 square ratio. *)

val aspect_video : t
(** Constrains element to a 16:9 video ratio. *)

val aspect_ratio : int -> int -> t
(** [aspect_ratio w h] constrains element to a custom [w:h] ratio. *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses an aspect-ratio utility from string parts. *)
