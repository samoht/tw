(** Positioning utilities for controlling element placement *)

open Style

(** {1 Utility Types} *)

type utility

val of_string : string list -> (utility, [ `Msg of string ]) result
(** [of_string parts] parses a positioning utility from string parts. Returns an
    internal structured representation. *)

(** {1 Internal Conversion Functions} *)

val to_style : utility -> t
(** [to_style u] converts a structured positioning utility to a style. For
    internal use by the Tw module. *)

val suborder : utility -> int
(** [suborder u] returns the ordering value for positioning utility [u]. Used
    for deterministic CSS output ordering. *)

(** {1 Inset Utilities} *)

val inset : int -> t
(** [inset n] sets all four sides (top, right, bottom, left) to n * spacing. *)

val inset_0 : t
(** [inset_0] sets all sides to 0. *)

val inset_x_0 : t
(** [inset_x_0] sets left and right to 0. *)

val inset_y_0 : t
(** [inset_y_0] sets top and bottom to 0. *)

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
(** [top_1_2] sets top to 50%. *)

val left_1_2 : t
(** [left_1_2] sets left to 50%. *)

(** {1 Z-Index} *)

val z : int -> t
(** [z n] sets z-index to n. *)
