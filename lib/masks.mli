(** Mask utilities for CSS masking *)

open Utility

val mask_none : t
(** [mask_none] is the [mask-composite: none] utility. *)

val mask_add : t
(** [mask_add] is the [mask-composite: add] utility. *)

val mask_exclude : t
(** [mask_exclude] is the [mask-composite: exclude] utility. *)

val mask_intersect : t
(** [mask_intersect] is the [mask-composite: intersect] utility. *)

val mask_subtract : t
(** [mask_subtract] is the [mask-composite: subtract] utility. *)

val mask_alpha : t
(** [mask_alpha] is the [mask-mode: alpha] utility. *)

val mask_luminance : t
(** [mask_luminance] is the [mask-mode: luminance] utility. *)

val mask_match : t
(** [mask_match] is the [mask-mode: match-source] utility. *)

val mask_type_alpha : t
(** [mask_type_alpha] is the [mask-type: alpha] utility. *)

val mask_type_luminance : t
(** [mask_type_luminance] is the [mask-type: luminance] utility. *)

val mask_auto : t
(** [mask_auto] is the [mask-size: auto] utility. *)

val mask_clip_border : t
(** [mask_clip_border] is the [mask-clip: border-box] utility. *)

val mask_clip_padding : t
(** [mask_clip_padding] is the [mask-clip: padding-box] utility. *)

val mask_clip_content : t
(** [mask_clip_content] is the [mask-clip: content-box] utility. *)

val mask_clip_fill : t
(** [mask_clip_fill] is the [mask-clip: fill-box] utility. *)

val mask_clip_stroke : t
(** [mask_clip_stroke] is the [mask-clip: stroke-box] utility. *)

val mask_clip_view : t
(** [mask_clip_view] is the [mask-clip: view-box] utility. *)

val mask_no_clip : t
(** [mask_no_clip] is the [mask-clip: no-clip] utility. *)

val mask_origin_border : t
(** [mask_origin_border] is the [mask-origin: border-box] utility. *)

val mask_origin_padding : t
(** [mask_origin_padding] is the [mask-origin: padding-box] utility. *)

val mask_origin_content : t
(** [mask_origin_content] is the [mask-origin: content-box] utility. *)

val mask_origin_fill : t
(** [mask_origin_fill] is the [mask-origin: fill-box] utility. *)

val mask_origin_stroke : t
(** [mask_origin_stroke] is the [mask-origin: stroke-box] utility. *)

val mask_origin_view : t
(** [mask_origin_view] is the [mask-origin: view-box] utility. *)

module Handler : Utility.Handler
