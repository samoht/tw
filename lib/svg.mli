(** SVG utilities for fill, stroke, and stroke-width

    https://tailwindcss.com/docs/fill https://tailwindcss.com/docs/stroke
    https://tailwindcss.com/docs/stroke-width *)

open Utility

(** {1 Fill Utilities} *)

val fill_none : t
(** [fill_none] removes fill from SVG elements. *)

val fill_current : t
(** [fill_current] sets fill to currentColor. *)

val fill : Color.color -> ?shade:int -> unit -> Style.t
(** [fill color ?shade ()] sets SVG fill color. *)

(** {1 Stroke Utilities} *)

val stroke_none : t
(** [stroke_none] removes stroke from SVG elements. *)

val stroke_current : t
(** [stroke_current] sets stroke to currentColor. *)

val stroke : Color.color -> ?shade:int -> unit -> Style.t
(** [stroke color ?shade ()] sets SVG stroke color. *)

(** {1 Stroke Width Utilities} *)

val stroke_0 : t
(** [stroke_0] sets stroke-width to 0. *)

val stroke_1 : t
(** [stroke_1] sets stroke-width to 1. *)

val stroke_2 : t
(** [stroke_2] sets stroke-width to 2. *)

val stroke_width : int -> t
(** [stroke_width n] sets stroke-width to n. *)
