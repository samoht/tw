(** Sizing utilities for width and height *)

open Core

(** {1 Width Utilities} *)

val w : size -> t
val w_auto : t
val w_full : t
val w_screen : t
val w_min : t
val w_max : t
val w_fit : t
val w_1_2 : t
val w_1_3 : t
val w_2_3 : t
val w_1_4 : t
val w_3_4 : t
val w_1_5 : t
val w_2_5 : t
val w_3_5 : t
val w_4_5 : t
val h : size -> t
val h_auto : t
val h_full : t
val h_screen : t
val h_min : t
val h_max : t
val h_fit : t
val h_1_2 : t
val h_1_3 : t
val h_2_3 : t
val h_1_4 : t
val h_3_4 : t
val h_1_5 : t
val h_2_5 : t
val h_3_5 : t
val h_4_5 : t
val min_w : size -> t
val min_w_0 : t
val min_w_full : t
val min_w_min : t
val min_w_max : t
val min_w_fit : t
val max_w : size -> t
val max_w_none : t
val max_w_xs : t
val max_w_sm : t
val max_w_md : t
val max_w_lg : t
val max_w_xl : t
val max_w_2xl : t
val max_w_3xl : t
val max_w_4xl : t
val max_w_5xl : t
val max_w_6xl : t
val max_w_7xl : t
val max_w_full : t
val max_w_min : t
val max_w_max : t
val max_w_fit : t
val max_w_prose : t
val max_w_screen_sm : t
val max_w_screen_md : t
val max_w_screen_lg : t
val max_w_screen_xl : t
val max_w_screen_2xl : t
val min_h : size -> t
val min_h_0 : t
val min_h_full : t
val min_h_screen : t
val min_h_min : t
val min_h_max : t
val min_h_fit : t
val max_h : size -> t
val max_h_none : t
val max_h_full : t
val max_h_screen : t
val max_h_min : t
val max_h_max : t
val max_h_fit : t
