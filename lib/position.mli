(** Position utilities for controlling element placement

    https://tailwindcss.com/docs/position
    https://tailwindcss.com/docs/top-right-bottom-left *)

open Utility

(** {1 Position Utilities} *)

val static : t
val relative : t
val absolute : t
val fixed : t
val sticky : t
val inset : int -> t
val inset_0 : t
val inset_x_0 : t
val inset_y_0 : t
val top : int -> t
val right : int -> t
val bottom : int -> t
val left : int -> t
val top_1_2 : t
val left_1_2 : t
val z : int -> t

module Handler : sig
  type t

  val of_string : string list -> (t, [ `Msg of string ]) result
  (** [of_string parts] parses a positioning utility from string parts. *)

  val suborder : t -> int
  (** [suborder u] returns the ordering value for positioning utility [u]. *)

  val to_style : t -> Style.t
  val order : t -> int * int
end
