(** Filter utilities for visual effects like blur, brightness, and backdrop
    filters *)

open Core

(** {1 Filter Utilities} *)

val blur_none : t
(** No blur. *)

val blur_xs : t
(** Extra-small blur. *)

val blur_sm : t
(** Small blur. *)

val blur : t
(** Default blur. *)

val blur_md : t
(** Medium blur. *)

val blur_lg : t
(** Large blur. *)

val blur_xl : t
(** Extra-large blur. *)

val blur_2xl : t
(** 2× extra-large blur. *)

val blur_3xl : t
(** 3× extra-large blur. *)

val brightness : int -> t
(** [brightness n] sets brightness to n% (0-200, 100 is normal). *)

val contrast : int -> t
(** [contrast n] sets contrast to n% (0-200, 100 is normal). *)

val grayscale : int -> t
(** [grayscale n] applies n% grayscale (0-100). *)

val saturate : int -> t
(** [saturate n] sets saturation to n% (0-200, 100 is normal). *)

val sepia : int -> t
(** [sepia n] applies n% sepia effect (0-100). *)

val invert : int -> t
(** [invert n] inverts colors by n% (0-100). *)

val hue_rotate : int -> t
(** [hue_rotate deg] rotates hue by deg degrees. *)

(** {1 Backdrop Filter Utilities} *)

val backdrop_blur_none : t
(** No backdrop blur. *)

val backdrop_blur_xs : t
(** Extra-small backdrop blur. *)

val backdrop_blur_sm : t
(** Small backdrop blur. *)

val backdrop_blur : t
(** Default backdrop blur. *)

val backdrop_blur_md : t
(** Medium backdrop blur. *)

val backdrop_blur_lg : t
(** Large backdrop blur. *)

val backdrop_blur_xl : t
(** Extra-large backdrop blur. *)

val backdrop_blur_2xl : t
(** 2× extra-large backdrop blur. *)

val backdrop_blur_3xl : t
(** 3× extra-large backdrop blur. *)

val backdrop_brightness : int -> t
(** [backdrop_brightness n] sets backdrop brightness to n% (0-200, 100 is
    normal). *)

val backdrop_contrast : int -> t
(** [backdrop_contrast n] sets backdrop contrast to n% (0-200, 100 is normal).
*)

val backdrop_opacity : int -> t
(** [backdrop_opacity n] sets backdrop opacity to n% (0-100). *)

val backdrop_saturate : int -> t
(** [backdrop_saturate n] sets backdrop saturation to n% (0-200, 100 is normal).
*)

val backdrop_grayscale : int -> t
(** [backdrop_grayscale n] applies n% grayscale to backdrop (0-100). *)

val backdrop_invert : int -> t
(** [backdrop_invert n] inverts backdrop colors by n% (0-100). *)

val backdrop_sepia : int -> t
(** [backdrop_sepia n] applies n% sepia effect to backdrop (0-100). *)

val backdrop_hue_rotate : int -> t
(** [backdrop_hue_rotate deg] rotates backdrop hue by deg degrees. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a filter utility from string parts. *)
