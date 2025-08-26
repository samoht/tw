(** Border utilities for border styles, widths, and radii *)

open Core

(** {1 Border Width Utilities} *)

val border : t
(** All sides, 1px border width (uses [--tw-border-style]). *)

val border_0 : t
(** All sides, 0px border width. *)

val border_2 : t
(** All sides, 2px border width. *)

val border_4 : t
(** All sides, 4px border width. *)

val border_8 : t
(** All sides, 8px border width. *)

val border_t : t
(** Top border width 1px. *)

val border_r : t
(** Right border width 1px. *)

val border_b : t
(** Bottom border width 1px. *)

val border_l : t
(** Left border width 1px. *)

val border_x : t
(** Horizontal borders (left/right) 1px. *)

val border_y : t
(** Vertical borders (top/bottom) 1px. *)

val border_solid : t
(** Sets [--tw-border-style] to [solid] and applies [border-style: solid]. *)

val border_dashed : t
(** Sets [--tw-border-style] to [dashed] and applies [border-style: dashed]. *)

val border_dotted : t
(** Sets [--tw-border-style] to [dotted] and applies [border-style: dotted]. *)

val border_double : t
(** Sets [--tw-border-style] to [double] and applies [border-style: double]. *)

val border_none : t
(** Sets [--tw-border-style] to [none] and applies [border-style: none]. *)

(** Border width utilities with semantic names. *)
val border_xs : t
(** Semantic width alias – 1px. *)

val border_sm : t
(** Semantic width alias – 2px. *)

val border_md : t
(** Semantic width alias – 4px. *)

val border_lg : t
(** Semantic width alias – 4px. *)

val border_xl : t
(** Semantic width alias – 8px. *)

val border_2xl : t
(** Semantic width alias – 8px. *)

val border_3xl : t
(** Semantic width alias – 8px. *)

val border_full : t
(** Semantic width alias – 8px. *)

val rounded_none : t
(** No border radius. *)

val rounded_sm : t
(** Small border radius. *)

val rounded : t
(** Default border radius. *)

val rounded_md : t
(** Medium border radius. *)

val rounded_lg : t
(** Large border radius. *)

val rounded_xl : t
(** Extra-large border radius. *)

val rounded_2xl : t
(** 2× extra-large border radius. *)

val rounded_3xl : t
(** 3× extra-large border radius. *)

val rounded_full : t
(** Fully rounded (max) border radius. *)

(* Individual corner radius utilities not supported by Css module: rounded_t,
   rounded_r, rounded_b, rounded_l, rounded_tl, rounded_tr, rounded_br,
   rounded_bl *)

(* outline_none not supported by Css module *)

(* outline not supported by Css module *)

val outline_offset_0 : t
(** Outline offset 0px. *)

val outline_offset_1 : t
(** Outline offset 1px. *)

val outline_offset_2 : t
(** Outline offset 2px. *)

val outline_offset_4 : t
(** Outline offset 4px. *)

val outline_offset_8 : t
(** Outline offset 8px. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a border/rounded utility from string parts. *)
