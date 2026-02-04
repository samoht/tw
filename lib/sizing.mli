(** Sizing utilities for width and height

    https://tailwindcss.com/docs/width https://tailwindcss.com/docs/height
    https://tailwindcss.com/docs/min-width
    https://tailwindcss.com/docs/max-width
    https://tailwindcss.com/docs/min-height
    https://tailwindcss.com/docs/max-height
    https://tailwindcss.com/docs/aspect-ratio *)

open Utility

type size =
  [ `None | `Xs | `Sm | `Md | `Lg | `Xl | `Xl_2 | `Xl_3 | `Full | `Rem of float ]

val order : base -> (int * int) option
(** [order u] returns the priority and suborder for sizing utilities. *)

module Handler : Utility.Handler

(** {1 Width Utilities} *)

val w' : size -> t
(** [w' sz] sets width to predefined size [sz] (e.g., [`Sm], [`Full]). *)

val w : int -> t
(** [w n] sets width using spacing scale (n * 0.25rem). *)

val w_auto : t
(** [w_auto] sets width to auto. *)

val w_full : t
(** [w_full] sets width to 100%. *)

val w_screen : t
(** [w_screen] sets width to 100vw. *)

val w_min : t
(** [w_min] sets width to min-content. *)

val w_max : t
(** [w_max] sets width to max-content. *)

val w_fit : t
(** [w_fit] sets width to fit-content. *)

val w_1_2 : t
(** [w_1_2] sets width to 1/2. *)

val w_1_3 : t
(** [w_1_3] sets width to 1/3. *)

val w_2_3 : t
(** [w_2_3] sets width to 2/3. *)

val w_1_4 : t
(** [w_1_4] sets width to 1/4. *)

val w_3_4 : t
(** [w_3_4] sets width to 3/4. *)

val w_1_5 : t
(** [w_1_5] sets width to 1/5. *)

val w_2_5 : t
(** [w_2_5] sets width to 2/5. *)

val w_3_5 : t
(** [w_3_5] sets width to 3/5. *)

val w_4_5 : t
(** [w_4_5] sets width to 4/5. *)

val h' : size -> t
(** [h' sz] sets height to predefined size [sz]. *)

val h : int -> t
(** [h n] sets height using spacing scale (n * 0.25rem). *)

val h_auto : t
(** [h_auto] sets height to auto. *)

val h_full : t
(** [h_full] sets height to 100%. *)

val h_screen : t
(** [h_screen] sets height to 100vh. *)

val h_min : t
(** [h_min] sets height to min-content. *)

val h_max : t
(** [h_max] sets height to max-content. *)

val h_fit : t
(** [h_fit] sets height to fit-content. *)

val h_1_2 : t
(** [h_1_2] sets height to 1/2. *)

val h_1_3 : t
(** [h_1_3] sets height to 1/3. *)

val h_2_3 : t
(** [h_2_3] sets height to 2/3. *)

val h_1_4 : t
(** [h_1_4] sets height to 1/4. *)

val h_3_4 : t
(** [h_3_4] sets height to 3/4. *)

val h_1_5 : t
(** [h_1_5] sets height to 1/5. *)

val h_2_5 : t
(** [h_2_5] sets height to 2/5. *)

val h_3_5 : t
(** [h_3_5] sets height to 3/5. *)

val h_4_5 : t
(** [h_4_5] sets height to 4/5. *)

val min_w' : size -> t
(** [min_w' sz] sets min-width to predefined size [sz]. *)

val min_w : int -> t
(** [min_w n] sets min-width using spacing scale. *)

val min_w_0 : t
(** [min_w_0] sets min-width to 0. *)

val min_w_full : t
(** [min_w_full] sets min-width to 100%. *)

val min_w_min : t
(** [min_w_min] sets min-width to min-content. *)

val min_w_max : t
(** [min_w_max] sets min-width to max-content. *)

val min_w_fit : t
(** [min_w_fit] sets min-width to fit-content. *)

val max_w' : size -> t
(** [max_w' sz] sets max-width to predefined size [sz]. *)

val max_w : int -> t
(** [max_w n] sets max-width using spacing scale. *)

val max_w_none : t
(** [max_w_none] removes the max-width constraint. *)

val max_w_xs : t
(** [max_w_xs] sets max-width to xs container width. *)

val max_w_sm : t
(** [max_w_sm] sets max-width to sm container width. *)

val max_w_md : t
(** [max_w_md] sets max-width to md container width. *)

val max_w_lg : t
(** [max_w_lg] sets max-width to lg container width. *)

val max_w_xl : t
(** [max_w_xl] sets max-width to xl container width. *)

val max_w_2xl : t
(** [max_w_2xl] sets max-width to 2xl container width. *)

val max_w_3xl : t
(** [max_w_3xl] sets max-width to 3xl container width. *)

val max_w_4xl : t
(** [max_w_4xl] sets max-width to 4xl container width. *)

val max_w_5xl : t
(** [max_w_5xl] sets max-width to 5xl container width. *)

val max_w_6xl : t
(** [max_w_6xl] sets max-width to 6xl container width. *)

val max_w_7xl : t
(** [max_w_7xl] sets max-width to 7xl container width. *)

val max_w_full : t
(** [max_w_full] sets max-width to 100%. *)

val max_w_min : t
(** [max_w_min] sets max-width to min-content. *)

val max_w_max : t
(** [max_w_max] sets max-width to max-content. *)

val max_w_fit : t
(** [max_w_fit] sets max-width to fit-content. *)

val max_w_prose : t
(** [max_w_prose] sets max-width to a readable measure for prose. *)

val max_w_screen_sm : t
(** [max_w_screen_sm] sets max-width to small breakpoint width. *)

val max_w_screen_md : t
(** [max_w_screen_md] sets max-width to medium breakpoint width. *)

val max_w_screen_lg : t
(** [max_w_screen_lg] sets max-width to large breakpoint width. *)

val max_w_screen_xl : t
(** [max_w_screen_xl] sets max-width to extra-large breakpoint width. *)

val max_w_screen_2xl : t
(** [max_w_screen_2xl] sets max-width to 2× extra-large breakpoint width. *)

val min_h' : size -> t
(** [min_h' sz] sets min-height to predefined size [sz]. *)

val min_h : int -> t
(** [min_h n] sets min-height using spacing scale. *)

val min_h_0 : t
(** [min_h_0] sets min-height to 0. *)

val min_h_full : t
(** [min_h_full] sets min-height to 100%. *)

val min_h_screen : t
(** [min_h_screen] sets min-height to 100vh. *)

val min_h_min : t
(** [min_h_min] sets min-height to min-content. *)

val min_h_max : t
(** [min_h_max] sets min-height to max-content. *)

val min_h_fit : t
(** [min_h_fit] sets min-height to fit-content. *)

val max_h' : size -> t
(** [max_h' sz] sets max-height to predefined size [sz]. *)

val max_h : int -> t
(** [max_h n] sets max-height using spacing scale. *)

val max_h_none : t
(** [max_h_none] removes the max-height constraint. *)

val max_h_full : t
(** [max_h_full] sets max-height to 100%. *)

val max_h_screen : t
(** [max_h_screen] sets max-height to 100vh. *)

val max_h_min : t
(** [max_h_min] sets max-height to min-content. *)

val max_h_max : t
(** [max_h_max] sets max-height to max-content. *)

val max_h_fit : t
(** [max_h_fit] sets max-height to fit-content. *)

(** {1 Size Utilities (Width and Height Combined)} *)

val size : int -> t
(** [size n] sets both width and height using spacing scale (n * 0.25rem).
    Equivalent to combining [w n] and [h n]. *)

val size_auto : t
(** [size_auto] sets both width and height to auto. *)

val size_full : t
(** [size_full] sets both width and height to 100%. *)

val size_min : t
(** [size_min] sets both width and height to min-content. *)

val size_max : t
(** [size_max] sets both width and height to max-content. *)

val size_fit : t
(** [size_fit] sets both width and height to fit-content. *)

(** {1 Aspect Ratio Utilities} *)

val aspect_auto : t
(** [aspect_auto] removes any aspect ratio constraint. *)

val aspect_square : t
(** [aspect_square] sets aspect ratio to 1:1. *)

val aspect_video : t
(** [aspect_video] sets aspect ratio to 16:9. *)

val aspect_ratio : int -> int -> t
(** [aspect_ratio w h] creates a custom aspect ratio with width [w] and height
    [h]. *)

(** [suborder core] returns the suborder for a sizing utility class name.
    Heights come before widths, with numeric values sorted numerically. *)
