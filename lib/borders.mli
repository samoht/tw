(** Border utilities for border styles, widths, and radii

    https://tailwindcss.com/docs/border-width
    https://tailwindcss.com/docs/border-style
    https://tailwindcss.com/docs/border-radius
    https://tailwindcss.com/docs/outline-width
    https://tailwindcss.com/docs/outline-offset *)

open Utility

(** {1 Border Width Utilities} *)

val border : t
(** [border] sets all sides to 1px border width (uses [--tw-border-style]). *)

val border_0 : t
(** [border_0] sets all sides to 0px border width. *)

val border_2 : t
(** [border_2] sets all sides to 2px border width. *)

val border_4 : t
(** [border_4] sets all sides to 4px border width. *)

val border_8 : t
(** [border_8] sets all sides to 8px border width. *)

val border_t : t
(** [border_t] sets top border width to 1px. *)

val border_r : t
(** [border_r] sets right border width to 1px. *)

val border_b : t
(** [border_b] sets bottom border width to 1px. *)

val border_l : t
(** [border_l] sets left border width to 1px. *)

val border_x : t
(** [border_x] sets horizontal borders (left/right) to 1px. *)

val border_y : t
(** [border_y] sets vertical borders (top/bottom) to 1px. *)

val border_solid : t
(** [border_solid] sets [--tw-border-style] to [solid] and applies
    [border-style: solid]. *)

val border_dashed : t
(** [border_dashed] sets [--tw-border-style] to [dashed] and applies
    [border-style: dashed]. *)

val border_dotted : t
(** [border_dotted] sets [--tw-border-style] to [dotted] and applies
    [border-style: dotted]. *)

val border_double : t
(** [border_double] sets [--tw-border-style] to [double] and applies
    [border-style: double]. *)

val border_none : t
(** [border_none] sets [--tw-border-style] to [none] and applies
    [border-style: none]. *)

val border_xs : t
(** [border_xs] is a semantic width alias – 1px. *)

val border_sm : t
(** [border_sm] is a semantic width alias – 2px. *)

val border_md : t
(** [border_md] is a semantic width alias – 4px. *)

val border_lg : t
(** [border_lg] is a semantic width alias – 4px. *)

val border_xl : t
(** [border_xl] is a semantic width alias – 8px. *)

val border_2xl : t
(** [border_2xl] is a semantic width alias – 8px. *)

val border_3xl : t
(** [border_3xl] is a semantic width alias – 8px. *)

val border_full : t
(** [border_full] is a semantic width alias – 8px. *)

val rounded_none : t
(** [rounded_none] sets no border radius. *)

val rounded_sm : t
(** [rounded_sm] sets a small border radius. *)

val rounded : t
(** [rounded] sets the default border radius. *)

val rounded_md : t
(** [rounded_md] sets a medium border radius. *)

val rounded_lg : t
(** [rounded_lg] sets a large border radius. *)

val rounded_xl : t
(** [rounded_xl] sets an extra-large border radius. *)

val rounded_2xl : t
(** [rounded_2xl] sets a 2× extra-large border radius. *)

val rounded_3xl : t
(** [rounded_3xl] sets a 3× extra-large border radius. *)

val rounded_full : t
(** [rounded_full] sets a fully rounded (max) border radius. *)

(* Individual corner radius utilities not supported by Css module: rounded_t,
   rounded_r, rounded_b, rounded_l, rounded_tl, rounded_tr, rounded_br,
   rounded_bl *)

(** {1 Outline Utilities} *)

val outline_none : t
(** [outline_none] removes the outline. *)

val outline_offset_0 : t
(** [outline_offset_0] sets outline offset to 0px. *)

val outline_offset_1 : t
(** [outline_offset_1] sets outline offset to 1px. *)

val outline_offset_2 : t
(** [outline_offset_2] sets outline offset to 2px. *)

val outline_offset_4 : t
(** [outline_offset_4] sets outline offset to 4px. *)

val outline_offset_8 : t
(** [outline_offset_8] sets outline offset to 8px. *)

module Handler : sig
  type t

  val of_string : string list -> (t, [ `Msg of string ]) result
  val suborder : t -> int
  val to_style : t -> Style.t
  val order : t -> int * int
end
