(** Positioning utilities for controlling element placement *)

open Core

(** {1 Inset Utilities} *)

val inset : int -> t
(** [inset n] sets all four sides (top, right, bottom, left) to n * spacing. *)

val inset_0 : t
val inset_x_0 : t
val inset_y_0 : t

(** {1 Individual Side Positioning} *)

val top : int -> t
(** [top n] sets top position to n * spacing (can be negative). *)

val right : int -> t
(** [right n] sets right position to n * spacing (can be negative). *)

val bottom : int -> t
(** [bottom n] sets bottom position to n * spacing (can be negative). *)

val left : int -> t
(** [left n] sets left position to n * spacing (can be negative). *)

(** {1 Fractional Positioning} *)

val top_1_2 : t
(** Sets top to 50%. *)

val left_1_2 : t
(** Sets left to 50%. *)

(** {1 Z-Index} *)

val z : int -> t
(** [z n] sets z-index to n. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a positioning utility from string parts. *)
