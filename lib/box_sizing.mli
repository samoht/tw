(** Box sizing utilities *)

open Utility

val box_border : t
(** [box_border] includes borders and padding in the element's size. *)

val box_content : t
(** [box_content] excludes borders and padding from the element's size. *)

module Handler : Utility.Handler
