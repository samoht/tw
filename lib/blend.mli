(** Blend mode utilities: [mix-blend-mode] and [background-blend-mode].

    See {{:https://tailwindcss.com/docs/mix-blend-mode}mix-blend-mode} and
    {{:https://tailwindcss.com/docs/background-blend-mode}background-blend-mode}.
*)

open Utility
module Handler : Utility.Handler

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

val mix_blend_plus_darker : t
(** [mix_blend_plus_darker] sets mix-blend-mode to plus-darker (Safari). *)

val mix_blend_plus_lighter : t
(** [mix_blend_plus_lighter] sets mix-blend-mode to plus-lighter (Safari). *)

(** {1 Background Blend Mode Utilities} *)

val bg_blend_normal : t
(** [bg_blend_normal] uses normal background blending. *)

val bg_blend_multiply : t
(** [bg_blend_multiply] uses multiply background blending. *)

val bg_blend_screen : t
(** [bg_blend_screen] uses screen background blending. *)

val bg_blend_overlay : t
(** [bg_blend_overlay] uses overlay background blending. *)

val bg_blend_darken : t
(** [bg_blend_darken] uses darken background blending. *)

val bg_blend_lighten : t
(** [bg_blend_lighten] uses lighten background blending. *)

val bg_blend_color_dodge : t
(** [bg_blend_color_dodge] uses color-dodge background blending. *)

val bg_blend_color_burn : t
(** [bg_blend_color_burn] uses color-burn background blending. *)

val bg_blend_hard_light : t
(** [bg_blend_hard_light] uses hard-light background blending. *)

val bg_blend_soft_light : t
(** [bg_blend_soft_light] uses soft-light background blending. *)

val bg_blend_difference : t
(** [bg_blend_difference] uses difference background blending. *)

val bg_blend_exclusion : t
(** [bg_blend_exclusion] uses exclusion background blending. *)

val bg_blend_hue : t
(** [bg_blend_hue] uses hue background blending. *)

val bg_blend_saturation : t
(** [bg_blend_saturation] uses saturation background blending. *)

val bg_blend_color : t
(** [bg_blend_color] uses color background blending. *)

val bg_blend_luminosity : t
(** [bg_blend_luminosity] uses luminosity background blending. *)
