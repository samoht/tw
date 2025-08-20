(** Visual effects utilities for shadows, opacity, and filters *)

open Core

(** {1 Shadow Utilities} *)

val shadow_none : t
(** [shadow_none] no box shadow. *)

val shadow_sm : t
(** [shadow_sm] small box shadow. *)

val shadow : t
(** [shadow] default box shadow. *)

val shadow_md : t
(** [shadow_md] medium box shadow. *)

val shadow_lg : t
(** [shadow_lg] large box shadow. *)

val shadow_xl : t
(** [shadow_xl] extra large box shadow. *)

val shadow_2xl : t
(** [shadow_2xl] 2x large box shadow. *)

val shadow_inner : t
(** [shadow_inner] inner box shadow. *)

(** {1 Opacity Utilities} *)

val opacity_0 : t
(** [opacity_0] opacity of 0. *)

val opacity_5 : t
(** [opacity_5] opacity of 0.05. *)

val opacity_10 : t
(** [opacity_10] opacity of 0.1. *)

val opacity_20 : t
(** [opacity_20] opacity of 0.2. *)

val opacity_25 : t
(** [opacity_25] opacity of 0.25. *)

val opacity_30 : t
(** [opacity_30] opacity of 0.3. *)

val opacity_40 : t
(** [opacity_40] opacity of 0.4. *)

val opacity_50 : t
(** [opacity_50] opacity of 0.5. *)

val opacity_60 : t
(** [opacity_60] opacity of 0.6. *)

val opacity_70 : t
(** [opacity_70] opacity of 0.7. *)

val opacity_75 : t
(** [opacity_75] opacity of 0.75. *)

val opacity_80 : t
(** [opacity_80] opacity of 0.8. *)

val opacity_90 : t
(** [opacity_90] opacity of 0.9. *)

val opacity_95 : t
(** [opacity_95] opacity of 0.95. *)

val opacity_100 : t
(** [opacity_100] opacity of 1. *)

(** {1 Mix Blend Mode Utilities} *)

(* Mix blend modes not supported by Css module *)

(** {1 Filter Utilities} *)

val blur_none : t
(** [blur_none] no blur filter. *)

val blur_sm : t
(** [blur_sm] small blur filter (4px). *)

val blur : t
(** [blur] default blur filter (8px). *)

val blur_md : t
(** [blur_md] medium blur filter (12px). *)

val blur_lg : t
(** [blur_lg] large blur filter (16px). *)

val blur_xl : t
(** [blur_xl] extra large blur filter (24px). *)

val blur_2xl : t
(** [blur_2xl] 2x large blur filter (40px). *)

val blur_3xl : t
(** [blur_3xl] 3x large blur filter (64px). *)

val brightness : int -> t
(** [brightness n] brightness filter with value n%. *)

val contrast : int -> t
(** [contrast n] contrast filter with value n%. *)

val grayscale : int -> t
(** [grayscale n] grayscale filter with value n%. *)

val hue_rotate : int -> t
(** [hue_rotate n] hue rotate filter with value n degrees. *)

val invert : int -> t
(** [invert n] invert filter with value n%. *)

val saturate : int -> t
(** [saturate n] saturate filter with value n%. *)

val sepia : int -> t
(** [sepia n] sepia filter with value n%. *)

(** {1 Backdrop Filter Utilities} *)

val backdrop_blur_none : t
(** [backdrop_blur_none] no backdrop blur filter. *)

val backdrop_blur_sm : t
(** [backdrop_blur_sm] small backdrop blur filter (4px). *)

val backdrop_blur : t
(** [backdrop_blur] default backdrop blur filter (8px). *)

val backdrop_blur_md : t
(** [backdrop_blur_md] medium backdrop blur filter (12px). *)

val backdrop_blur_lg : t
(** [backdrop_blur_lg] large backdrop blur filter (16px). *)

val backdrop_blur_xl : t
(** [backdrop_blur_xl] extra large backdrop blur filter (24px). *)

val backdrop_blur_2xl : t
(** [backdrop_blur_2xl] 2x large backdrop blur filter (40px). *)

val backdrop_blur_3xl : t
(** [backdrop_blur_3xl] 3x large backdrop blur filter (64px). *)
