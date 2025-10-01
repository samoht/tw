(** Filter utilities for visual effects like blur, brightness, and backdrop
    filters *)

open Style

(** {1 Utility Type} *)

type utility =
  | Blur_none
  | Blur_xs
  | Blur_sm
  | Blur
  | Blur_md
  | Blur_lg
  | Blur_xl
  | Blur_2xl
  | Blur_3xl
  | Brightness of int
  | Contrast of int
  | Grayscale of int
  | Saturate of int
  | Sepia of int
  | Invert of int
  | Hue_rotate of int
  | Backdrop_blur_none
  | Backdrop_blur_xs
  | Backdrop_blur_sm
  | Backdrop_blur
  | Backdrop_blur_md
  | Backdrop_blur_lg
  | Backdrop_blur_xl
  | Backdrop_blur_2xl
  | Backdrop_blur_3xl
  | Backdrop_brightness of int
  | Backdrop_contrast of int
  | Backdrop_opacity of int
  | Backdrop_saturate of int
  | Backdrop_grayscale of int
  | Backdrop_invert of int
  | Backdrop_sepia of int
  | Backdrop_hue_rotate of int

val to_style : utility -> t
(** [to_style utility] converts a filter utility to a style. *)

val suborder : utility -> int
(** [suborder utility] returns the suborder for utility ordering. *)

(** {1 Filter Utilities} *)

val blur_none : t
(** [blur_none] applies no blur. *)

val blur_xs : t
(** [blur_xs] applies an extra-small blur. *)

val blur_sm : t
(** [blur_sm] applies a small blur. *)

val blur : t
(** [blur] applies the default blur. *)

val blur_md : t
(** [blur_md] applies a medium blur. *)

val blur_lg : t
(** [blur_lg] applies a large blur. *)

val blur_xl : t
(** [blur_xl] applies an extra-large blur. *)

val blur_2xl : t
(** [blur_2xl] applies a 2× extra-large blur. *)

val blur_3xl : t
(** [blur_3xl] applies a 3× extra-large blur. *)

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
(** [backdrop_blur_none] applies no backdrop blur. *)

val backdrop_blur_xs : t
(** [backdrop_blur_xs] applies an extra-small backdrop blur. *)

val backdrop_blur_sm : t
(** [backdrop_blur_sm] applies a small backdrop blur. *)

val backdrop_blur : t
(** [backdrop_blur] applies the default backdrop blur. *)

val backdrop_blur_md : t
(** [backdrop_blur_md] applies a medium backdrop blur. *)

val backdrop_blur_lg : t
(** [backdrop_blur_lg] applies a large backdrop blur. *)

val backdrop_blur_xl : t
(** [backdrop_blur_xl] applies an extra-large backdrop blur. *)

val backdrop_blur_2xl : t
(** [backdrop_blur_2xl] applies a 2× extra-large backdrop blur. *)

val backdrop_blur_3xl : t
(** [backdrop_blur_3xl] applies a 3× extra-large backdrop blur. *)

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

val backdrop_grayscale_default : t
(** [backdrop_grayscale_default] applies 100% grayscale to backdrop. *)

val backdrop_grayscale : int -> t
(** [backdrop_grayscale n] applies n% grayscale to backdrop (0-100). *)

val backdrop_invert_default : t
(** [backdrop_invert_default] inverts backdrop colors 100%. *)

val backdrop_invert : int -> t
(** [backdrop_invert n] inverts backdrop colors by n% (0-100). *)

val backdrop_sepia_default : t
(** [backdrop_sepia_default] applies 100% sepia effect to backdrop. *)

val backdrop_sepia : int -> t
(** [backdrop_sepia n] applies n% sepia effect to backdrop (0-100). *)

val backdrop_hue_rotate : int -> t
(** [backdrop_hue_rotate deg] rotates backdrop hue by deg degrees. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (utility, [ `Msg of string ]) result
(** [of_string parts] parses a filter utility from string parts. *)
