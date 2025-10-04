(** Visual effects utilities for shadows, opacity, and filters

    https://tailwindcss.com/docs/box-shadow https://tailwindcss.com/docs/opacity
    https://tailwindcss.com/docs/ring-width
    https://tailwindcss.com/docs/ring-color *)

open Utility
open Color

val order : base -> (int * int) option

module Handler : Utility.Handler

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

val opacity : int -> t
(** [opacity n] sets opacity to n%. *)

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

val ring_inset : t
(** [ring_inset] applies an inset ring. *)

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

(** {1 Mix Blend Mode Utilities} *)

val mix_blend_normal : t
(** [mix_blend_normal] sets mix-blend-mode to normal. *)

val mix_blend_multiply : t
(** [mix_blend_multiply] sets mix-blend-mode to multiply. *)

val mix_blend_screen : t
(** [mix_blend_screen] sets mix-blend-mode to screen. *)

val mix_blend_overlay : t
(** [mix_blend_overlay] sets mix-blend-mode to overlay. *)

val mix_blend_darken : t
(** [mix_blend_darken] sets mix-blend-mode to darken. *)

val mix_blend_lighten : t
(** [mix_blend_lighten] sets mix-blend-mode to lighten. *)

val mix_blend_color_dodge : t
(** [mix_blend_color_dodge] sets mix-blend-mode to color-dodge. *)

val mix_blend_color_burn : t
(** [mix_blend_color_burn] sets mix-blend-mode to color-burn. *)

val mix_blend_hard_light : t
(** [mix_blend_hard_light] sets mix-blend-mode to hard-light. *)

val mix_blend_soft_light : t
(** [mix_blend_soft_light] sets mix-blend-mode to soft-light. *)

val mix_blend_difference : t
(** [mix_blend_difference] sets mix-blend-mode to difference. *)

val mix_blend_exclusion : t
(** [mix_blend_exclusion] sets mix-blend-mode to exclusion. *)

val mix_blend_hue : t
(** [mix_blend_hue] sets mix-blend-mode to hue. *)

val mix_blend_saturation : t
(** [mix_blend_saturation] sets mix-blend-mode to saturation. *)

val mix_blend_color : t
(** [mix_blend_color] sets mix-blend-mode to color. *)

val mix_blend_luminosity : t
(** [mix_blend_luminosity] sets mix-blend-mode to luminosity. *)
