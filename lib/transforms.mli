(** Transform utilities for 2D and 3D transformations *)

open Style

(** {1 Utility Types} *)

type utility

val of_string : string list -> (utility, [ `Msg of string ]) result
(** [of_string parts] parses a transform utility from string parts.
    Returns an internal structured representation. *)

(** {1 Internal Conversion Functions} *)

val to_style : utility -> Style.t
(** [to_style u] converts a structured transform utility to a style.
    For internal use by the Tw module. *)

val suborder : utility -> int
(** [suborder u] returns the ordering value for transform utility [u].
    Used for deterministic CSS output ordering. *)

(** {1 2D Transform Utilities} *)

val rotate : int -> t
(** [rotate deg] rotates element by deg degrees. *)

val translate_x : int -> t
(** [translate_x n] translates element horizontally by n * 0.25rem. *)

val translate_y : int -> t
(** [translate_y n] translates element vertically by n * 0.25rem. *)

val scale : int -> t
(** [scale n] scales element by n%. *)

val scale_x : int -> t
(** [scale_x n] scales element horizontally by n%. *)

val scale_y : int -> t
(** [scale_y n] scales element vertically by n%. *)

val skew_x : int -> t
(** [skew_x deg] skews element horizontally by deg degrees. *)

val skew_y : int -> t
(** [skew_y deg] skews element vertically by deg degrees. *)

val neg_translate_x_1_2 : t
(** [neg_translate_x_1_2] translates element -50% along the X axis (useful for
    centering). *)

val neg_translate_y_1_2 : t
(** [neg_translate_y_1_2] translates element -50% along the Y axis (useful for
    centering). *)

(** {1 3D Transform Utilities} *)

val rotate_x : int -> t
(** [rotate_x deg] rotates element around X axis by deg degrees. *)

val rotate_y : int -> t
(** [rotate_y deg] rotates element around Y axis by deg degrees. *)

val rotate_z : int -> t
(** [rotate_z deg] rotates element around Z axis by deg degrees. *)

val translate_z : int -> t
(** [translate_z n] translates element along Z axis by n pixels. *)

val scale_z : int -> t
(** [scale_z n] scales element along Z axis by n%. *)

val perspective : int -> t
(** [perspective n] sets perspective distance in pixels. *)

val perspective_origin_center : t
(** [perspective_origin_center] sets perspective origin to center. *)

val perspective_origin_top : t
(** [perspective_origin_top] sets perspective origin to top. *)

val perspective_origin_bottom : t
(** [perspective_origin_bottom] sets perspective origin to bottom. *)

val perspective_origin_left : t
(** [perspective_origin_left] sets perspective origin to left. *)

val perspective_origin_right : t
(** [perspective_origin_right] sets perspective origin to right. *)

val transform_style_3d : t
(** [transform_style_3d] preserves 3D positioning of child elements. *)

val transform_style_flat : t
(** [transform_style_flat] flattens 3D positioning of child elements. *)

val backface_visible : t
(** [backface_visible] makes the back face visible when rotated. *)

val backface_hidden : t
(** [backface_hidden] hides the back face when rotated. *)

(** {1 Transform Control Utilities} *)

val transform : t
(** [transform] is the base transform utility that sets CSS custom properties
    for transforms. *)

val transform_none : t
(** [transform_none] removes all transforms. *)

val transform_gpu : t
(** [transform_gpu] forces GPU acceleration for transforms. *)
