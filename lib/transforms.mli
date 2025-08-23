(** Transform utilities for 2D and 3D transformations *)

open Core

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
(** Sets perspective origin to center. *)

val perspective_origin_top : t
(** Sets perspective origin to top. *)

val perspective_origin_bottom : t
(** Sets perspective origin to bottom. *)

val perspective_origin_left : t
(** Sets perspective origin to left. *)

val perspective_origin_right : t
(** Sets perspective origin to right. *)

val transform_style_3d : t
(** Preserves 3D positioning of child elements. *)

val transform_style_flat : t
(** Flattens 3D positioning of child elements. *)

val backface_visible : t
(** Makes backface visible when rotated. *)

val backface_hidden : t
(** Hides backface when rotated. *)

(** {1 Transform Control Utilities} *)

val transform : t
(** Base transform utility that sets CSS custom properties for transforms. *)

val transform_none : t
(** Removes all transforms. *)

val transform_gpu : t
(** Forces GPU acceleration for transforms. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a transform utility from string parts. *)
