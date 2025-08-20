(** Border utilities for border styles, widths, and radii *)

open Core

(** {1 Border Width Utilities} *)

val border : t
(** [border] border width of 1px. *)

val border_0 : t
(** [border_0] border width of 0. *)

val border_2 : t
(** [border_2] border width of 2px. *)

val border_4 : t
(** [border_4] border width of 4px. *)

val border_8 : t
(** [border_8] border width of 8px. *)

val border_t : t
(** [border_t] top border width of 1px. *)

val border_r : t
(** [border_r] right border width of 1px. *)

val border_b : t
(** [border_b] bottom border width of 1px. *)

val border_l : t
(** [border_l] left border width of 1px. *)

val border_x : t
(** [border_x] horizontal border width of 1px. *)

val border_y : t
(** [border_y] vertical border width of 1px. *)

(** {1 Border Style Utilities} *)

val border_solid : t
(** [border_solid] solid border style. *)

val border_dashed : t
(** [border_dashed] dashed border style. *)

val border_dotted : t
(** [border_dotted] dotted border style. *)

val border_double : t
(** [border_double] double border style. *)

val border_none : t
(** [border_none] no border style. *)

(** {1 Border Radius Utilities} *)

val rounded_none : t
(** [rounded_none] no border radius. *)

val rounded_sm : t
(** [rounded_sm] small border radius (0.125rem). *)

val rounded : t
(** [rounded] default border radius (0.25rem). *)

val rounded_md : t
(** [rounded_md] medium border radius (0.375rem). *)

val rounded_lg : t
(** [rounded_lg] large border radius (0.5rem). *)

val rounded_xl : t
(** [rounded_xl] extra large border radius (0.75rem). *)

val rounded_2xl : t
(** [rounded_2xl] 2x large border radius (1rem). *)

val rounded_3xl : t
(** [rounded_3xl] 3x large border radius (1.5rem). *)

val rounded_full : t
(** [rounded_full] full border radius (9999px). *)

(* Individual corner radius utilities not supported by Css module: rounded_t,
   rounded_r, rounded_b, rounded_l, rounded_tl, rounded_tr, rounded_br,
   rounded_bl *)

(** {1 Outline Utilities} *)

(* outline_none not supported by Css module *)

(* outline not supported by Css module *)

val outline_offset_0 : t
(** [outline_offset_0] outline offset of 0. *)

val outline_offset_1 : t
(** [outline_offset_1] outline offset of 1px. *)

val outline_offset_2 : t
(** [outline_offset_2] outline offset of 2px. *)

val outline_offset_4 : t
(** [outline_offset_4] outline offset of 4px. *)

val outline_offset_8 : t
(** [outline_offset_8] outline offset of 8px. *)

(** {1 Ring Utilities} *)

val ring : t
(** [ring] default ring width (3px). *)

val ring_0 : t
(** [ring_0] no ring. *)

val ring_1 : t
(** [ring_1] ring width of 1px. *)

val ring_2 : t
(** [ring_2] ring width of 2px. *)

val ring_4 : t
(** [ring_4] ring width of 4px. *)

val ring_8 : t
(** [ring_8] ring width of 8px. *)

val ring_inset : t
(** [ring_inset] inset ring. *)
