(** tab-size utilities. *)

open Utility

val tab : int -> t
(** [tab n] sets [tab-size] to [n]. *)

module Handler : Utility.Handler
