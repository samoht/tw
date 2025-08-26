(** Visual effects utilities for shadows, opacity, and filters *)

open Core

(** {1 Shadow Utilities} *)

val shadow_none : t
(** No shadow. *)

val shadow_sm : t
(** Small shadow. *)

val shadow : t
(** Default shadow. *)

val shadow_md : t
(** Medium shadow. *)

val shadow_lg : t
(** Large shadow. *)

val shadow_xl : t
(** Extra-large shadow. *)

val shadow_2xl : t
(** 2× extra-large shadow. *)

val shadow_inner : t
(** Inner inset shadow. *)

val opacity_0 : t
(** Opacity 0%. *)

val opacity_5 : t
(** Opacity 5%. *)

val opacity_10 : t
(** Opacity 10%. *)

val opacity_20 : t
(** Opacity 20%. *)

val opacity_25 : t
(** Opacity 25%. *)

val opacity_30 : t
(** Opacity 30%. *)

val opacity_40 : t
(** Opacity 40%. *)

val opacity_50 : t
(** Opacity 50%. *)

val opacity_60 : t
(** Opacity 60%. *)

val opacity_70 : t
(** Opacity 70%. *)

val opacity_75 : t
(** Opacity 75%. *)

val opacity_80 : t
(** Opacity 80%. *)

val opacity_90 : t
(** Opacity 90%. *)

val opacity_95 : t
(** Opacity 95%. *)

val opacity_100 : t
(** Opacity 100%. *)

(* Mix blend modes not supported by Css module *)

(** {1 Ring Utilities} *)

val ring : t
(** Default ring outline. *)

val ring_none : t
(** No ring outline. *)

val ring_xs : t
(** Extra-small ring. *)

val ring_sm : t
(** Small ring. *)

val ring_md : t
(** Medium ring. *)

val ring_lg : t
(** Large ring. *)

val ring_xl : t
(** Extra-large ring. *)

(** {1 Transition Utilities} *)

val transition_none : t
(** Disable all transitions. *)

val transition_all : t
(** Enable transitions on all properties. *)

val transition_colors : t
(** Transition only color-related properties. *)

val transition_opacity : t
(** Transition only [opacity]. *)

val transition_shadow : t
(** Transition only [box-shadow]. *)

val transition_transform : t
(** Transition only [transform]. *)

val duration : int -> t
(** [duration ms] sets animation/transition duration to [ms] milliseconds. *)

(** {1 Opacity Utility} *)

val opacity : int -> t
(** [opacity n] sets element opacity to [n]% (0–100). *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses an effects utility from string parts. *)
