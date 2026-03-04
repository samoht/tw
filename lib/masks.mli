(** Mask utilities for CSS masking *)

open Utility

val mask_none : t
val mask_add : t
val mask_exclude : t
val mask_intersect : t
val mask_subtract : t
val mask_alpha : t
val mask_luminance : t
val mask_match : t
val mask_type_alpha : t
val mask_type_luminance : t
val mask_auto : t
val mask_clip_border : t
val mask_clip_padding : t
val mask_clip_content : t
val mask_clip_fill : t
val mask_clip_stroke : t
val mask_clip_view : t
val mask_no_clip : t
val mask_origin_border : t
val mask_origin_padding : t
val mask_origin_content : t
val mask_origin_fill : t
val mask_origin_stroke : t
val mask_origin_view : t

module Handler : Utility.Handler
