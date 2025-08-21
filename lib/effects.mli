(** Visual effects utilities for shadows, opacity, and filters *)

open Core

(** {1 Shadow Utilities} *)

val shadow_none : t
val shadow_sm : t
val shadow : t
val shadow_md : t
val shadow_lg : t
val shadow_xl : t
val shadow_2xl : t
val shadow_inner : t
val opacity_0 : t
val opacity_5 : t
val opacity_10 : t
val opacity_20 : t
val opacity_25 : t
val opacity_30 : t
val opacity_40 : t
val opacity_50 : t
val opacity_60 : t
val opacity_70 : t
val opacity_75 : t
val opacity_80 : t
val opacity_90 : t
val opacity_95 : t
val opacity_100 : t

(* Mix blend modes not supported by Css module *)

(** {1 Filter Utilities} *)

val blur_none : t
val blur_sm : t
val blur : t
val blur_md : t
val blur_lg : t
val blur_xl : t
val blur_2xl : t
val blur_3xl : t
val brightness : int -> t
val contrast : int -> t
val grayscale : int -> t
val hue_rotate : int -> t
val invert : int -> t
val saturate : int -> t
val sepia : int -> t

(** {1 Backdrop Filter Utilities} *)

val backdrop_blur_none : t
val backdrop_blur_sm : t
val backdrop_blur : t
val backdrop_blur_md : t
val backdrop_blur_lg : t
val backdrop_blur_xl : t
val backdrop_blur_2xl : t
val backdrop_blur_3xl : t
