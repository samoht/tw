(** Sizing utilities for width and height *)

open Core

(** {1 Width Utilities} *)

val w : size -> t
(** [w size] width with given size. *)

val w_auto : t
(** [w_auto] width auto. *)

val w_full : t
(** [w_full] width 100%. *)

val w_screen : t
(** [w_screen] width 100vw. *)

val w_min : t
(** [w_min] min-content width. *)

val w_max : t
(** [w_max] max-content width. *)

val w_fit : t
(** [w_fit] fit-content width. *)

val w_1_2 : t
(** [w_1_2] width 50%. *)

val w_1_3 : t
(** [w_1_3] width 33.333333%. *)

val w_2_3 : t
(** [w_2_3] width 66.666667%. *)

val w_1_4 : t
(** [w_1_4] width 25%. *)

val w_3_4 : t
(** [w_3_4] width 75%. *)

val w_1_5 : t
(** [w_1_5] width 20%. *)

val w_2_5 : t
(** [w_2_5] width 40%. *)

val w_3_5 : t
(** [w_3_5] width 60%. *)

val w_4_5 : t
(** [w_4_5] width 80%. *)

(** {1 Height Utilities} *)

val h : size -> t
(** [h size] height with given size. *)

val h_auto : t
(** [h_auto] height auto. *)

val h_full : t
(** [h_full] height 100%. *)

val h_screen : t
(** [h_screen] height 100vh. *)

val h_min : t
(** [h_min] min-content height. *)

val h_max : t
(** [h_max] max-content height. *)

val h_fit : t
(** [h_fit] fit-content height. *)

val h_1_2 : t
(** [h_1_2] height 50%. *)

val h_1_3 : t
(** [h_1_3] height 33.333333%. *)

val h_2_3 : t
(** [h_2_3] height 66.666667%. *)

val h_1_4 : t
(** [h_1_4] height 25%. *)

val h_3_4 : t
(** [h_3_4] height 75%. *)

val h_1_5 : t
(** [h_1_5] height 20%. *)

val h_2_5 : t
(** [h_2_5] height 40%. *)

val h_3_5 : t
(** [h_3_5] height 60%. *)

val h_4_5 : t
(** [h_4_5] height 80%. *)

(** {1 Min Width Utilities} *)

val min_w : size -> t
(** [min_w size] min-width with given size. *)

val min_w_0 : t
(** [min_w_0] min-width 0. *)

val min_w_full : t
(** [min_w_full] min-width 100%. *)

val min_w_min : t
(** [min_w_min] min-width min-content. *)

val min_w_max : t
(** [min_w_max] min-width max-content. *)

val min_w_fit : t
(** [min_w_fit] min-width fit-content. *)

(** {1 Max Width Utilities} *)

val max_w : size -> t
(** [max_w size] max-width with given size. *)

val max_w_none : t
(** [max_w_none] max-width none. *)

val max_w_xs : t
(** [max_w_xs] max-width 20rem. *)

val max_w_sm : t
(** [max_w_sm] max-width 24rem. *)

val max_w_md : t
(** [max_w_md] max-width 28rem. *)

val max_w_lg : t
(** [max_w_lg] max-width 32rem. *)

val max_w_xl : t
(** [max_w_xl] max-width 36rem. *)

val max_w_2xl : t
(** [max_w_2xl] max-width 42rem. *)

val max_w_3xl : t
(** [max_w_3xl] max-width 48rem. *)

val max_w_4xl : t
(** [max_w_4xl] max-width 56rem. *)

val max_w_5xl : t
(** [max_w_5xl] max-width 64rem. *)

val max_w_6xl : t
(** [max_w_6xl] max-width 72rem. *)

val max_w_7xl : t
(** [max_w_7xl] max-width 80rem. *)

val max_w_full : t
(** [max_w_full] max-width 100%. *)

val max_w_min : t
(** [max_w_min] max-width min-content. *)

val max_w_max : t
(** [max_w_max] max-width max-content. *)

val max_w_fit : t
(** [max_w_fit] max-width fit-content. *)

val max_w_prose : t
(** [max_w_prose] max-width 65ch. *)

val max_w_screen_sm : t
(** [max_w_screen_sm] max-width 640px. *)

val max_w_screen_md : t
(** [max_w_screen_md] max-width 768px. *)

val max_w_screen_lg : t
(** [max_w_screen_lg] max-width 1024px. *)

val max_w_screen_xl : t
(** [max_w_screen_xl] max-width 1280px. *)

val max_w_screen_2xl : t
(** [max_w_screen_2xl] max-width 1536px. *)

(** {1 Min Height Utilities} *)

val min_h : size -> t
(** [min_h size] min-height with given size. *)

val min_h_0 : t
(** [min_h_0] min-height 0. *)

val min_h_full : t
(** [min_h_full] min-height 100%. *)

val min_h_screen : t
(** [min_h_screen] min-height 100vh. *)

val min_h_min : t
(** [min_h_min] min-height min-content. *)

val min_h_max : t
(** [min_h_max] min-height max-content. *)

val min_h_fit : t
(** [min_h_fit] min-height fit-content. *)

(** {1 Max Height Utilities} *)

val max_h : size -> t
(** [max_h size] max-height with given size. *)

val max_h_none : t
(** [max_h_none] max-height none. *)

val max_h_full : t
(** [max_h_full] max-height 100%. *)

val max_h_screen : t
(** [max_h_screen] max-height 100vh. *)

val max_h_min : t
(** [max_h_min] max-height min-content. *)

val max_h_max : t
(** [max_h_max] max-height max-content. *)

val max_h_fit : t
(** [max_h_fit] max-height fit-content. *)
