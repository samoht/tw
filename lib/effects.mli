(** Visual effects utilities for shadows, opacity, and filters *)

open Style
open Color

(** {1 Utility Types} *)

type utility

val of_string : string list -> (utility, [ `Msg of string ]) result
(** [of_string parts] parses an effects utility from string parts.
    Returns an internal structured representation. *)

(** {1 Internal Conversion Functions} *)

val to_style : utility -> Style.t
(** [to_style u] converts a structured effects utility to a style.
    For internal use by the Tw module. *)

val suborder : utility -> int
(** [suborder u] returns the ordering value for effects utility [u].
    Used for deterministic CSS output ordering. *)

(** {1 Shadow Utilities} *)

val shadow_none : t
(** [shadow_none] applies no shadow. *)

val shadow_sm : t
(** [shadow_sm] applies a small shadow. *)

val shadow : t
(** [shadow] applies the default shadow. *)

val shadow_md : t
(** [shadow_md] applies a medium shadow. *)

val shadow_lg : t
(** [shadow_lg] applies a large shadow. *)

val shadow_xl : t
(** [shadow_xl] applies an extra-large shadow. *)

val shadow_2xl : t
(** [shadow_2xl] applies a 2× extra-large shadow. *)

val shadow_inner : t
(** [shadow_inner] applies an inner (inset) shadow. *)

val opacity_0 : t
(** [opacity_0] sets opacity to 0%. *)

val opacity_5 : t
(** [opacity_5] sets opacity to 5%. *)

val opacity_10 : t
(** [opacity_10] sets opacity to 10%. *)

val opacity_20 : t
(** [opacity_20] sets opacity to 20%. *)

val opacity_25 : t
(** [opacity_25] sets opacity to 25%. *)

val opacity_30 : t
(** [opacity_30] sets opacity to 30%. *)

val opacity_40 : t
(** [opacity_40] sets opacity to 40%. *)

val opacity_50 : t
(** [opacity_50] sets opacity to 50%. *)

val opacity_60 : t
(** [opacity_60] sets opacity to 60%. *)

val opacity_70 : t
(** [opacity_70] sets opacity to 70%. *)

val opacity_75 : t
(** [opacity_75] sets opacity to 75%. *)

val opacity_80 : t
(** [opacity_80] sets opacity to 80%. *)

val opacity_90 : t
(** [opacity_90] sets opacity to 90%. *)

val opacity_95 : t
(** [opacity_95] sets opacity to 95%. *)

val opacity_100 : t
(** [opacity_100] sets opacity to 100%. *)

(* Mix blend modes not supported by Css module *)

(** {1 Ring Utilities} *)

val ring : t
(** [ring] applies the default ring outline. *)

val ring_none : t
(** [ring_none] removes the ring outline. *)

val ring_xs : t
(** [ring_xs] applies an extra-small ring. *)

val ring_sm : t
(** [ring_sm] applies a small ring. *)

val ring_md : t
(** [ring_md] applies a medium ring. *)

val ring_lg : t
(** [ring_lg] applies a large ring. *)

val ring_xl : t
(** [ring_xl] applies an extra-large ring. *)

val ring_color : color -> int -> t
(** [ring_color color shade] sets the ring color class, e.g., [ring blue 500].
*)

(** {1 Transition Utilities} *)

val transition_none : t
(** [transition_none] disables all transitions. *)

val transition_all : t
(** [transition_all] enables transitions on all properties. *)

val transition_colors : t
(** [transition_colors] transitions only color-related properties. *)

val transition_opacity : t
(** [transition_opacity] transitions only [opacity]. *)

val transition_shadow : t
(** [transition_shadow] transitions only [box-shadow]. *)

val transition_transform : t
(** [transition_transform] transitions only [transform]. *)

val duration : int -> t
(** [duration ms] sets animation/transition duration to [ms] milliseconds. *)

(** {1 Opacity Utility} *)

val opacity : int -> t
(** [opacity n] sets element opacity to [n]% (0–100). *)
