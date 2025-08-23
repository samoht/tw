(** Border utilities for border styles, widths, and radii *)

open Core

(** {1 Border Width Utilities} *)

val border : t
val border_0 : t
val border_2 : t
val border_4 : t
val border_8 : t
val border_t : t
val border_r : t
val border_b : t
val border_l : t
val border_x : t
val border_y : t
val border_solid : t
val border_dashed : t
val border_dotted : t
val border_double : t
val border_none : t
val rounded_none : t
val rounded_sm : t
val rounded : t
val rounded_md : t
val rounded_lg : t
val rounded_xl : t
val rounded_2xl : t
val rounded_3xl : t
val rounded_full : t

(* Individual corner radius utilities not supported by Css module: rounded_t,
   rounded_r, rounded_b, rounded_l, rounded_tl, rounded_tr, rounded_br,
   rounded_bl *)

(* outline_none not supported by Css module *)

(* outline not supported by Css module *)

val outline_offset_0 : t
val outline_offset_1 : t
val outline_offset_2 : t
val outline_offset_4 : t
val outline_offset_8 : t
val ring : t
val ring_0 : t
val ring_1 : t
val ring_2 : t
val ring_4 : t
val ring_8 : t
val ring_inset : t

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a border/rounded utility from string parts. *)
