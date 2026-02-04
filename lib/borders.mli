(** Border utilities for border styles, widths, and radii

    https://tailwindcss.com/docs/border-width
    https://tailwindcss.com/docs/border-style
    https://tailwindcss.com/docs/border-radius
    https://tailwindcss.com/docs/outline-width
    https://tailwindcss.com/docs/outline-offset *)

open Utility

(** {1 Border Width Utilities} *)

val border : t
(** [border] sets all sides to 1px border width (uses [--tw-border-style]). *)

val border_0 : t
(** [border_0] sets all sides to 0px border width. *)

val border_2 : t
(** [border_2] sets all sides to 2px border width. *)

val border_4 : t
(** [border_4] sets all sides to 4px border width. *)

val border_8 : t
(** [border_8] sets all sides to 8px border width. *)

val border_t : t
(** [border_t] sets top border width to 1px. *)

val border_r : t
(** [border_r] sets right border width to 1px. *)

val border_b : t
(** [border_b] sets bottom border width to 1px. *)

val border_l : t
(** [border_l] sets left border width to 1px. *)

val border_x : t
(** [border_x] sets horizontal borders (left/right) to 1px. *)

val border_y : t
(** [border_y] sets vertical borders (top/bottom) to 1px. *)

val border_solid : t
(** [border_solid] sets [--tw-border-style] to [solid] and applies
    [border-style: solid]. *)

val border_dashed : t
(** [border_dashed] sets [--tw-border-style] to [dashed] and applies
    [border-style: dashed]. *)

val border_dotted : t
(** [border_dotted] sets [--tw-border-style] to [dotted] and applies
    [border-style: dotted]. *)

val border_double : t
(** [border_double] sets [--tw-border-style] to [double] and applies
    [border-style: double]. *)

val border_hidden : t
(** [border_hidden] sets [--tw-border-style] to [hidden] and applies
    [border-style: hidden]. *)

val border_none : t
(** [border_none] sets [--tw-border-style] to [none] and applies
    [border-style: none]. *)

(** {1 Border Color Utilities} *)

val border_color : Color.color -> int -> t
(** [border_color color shade] sets the border color to the specified color and
    shade. *)

val border_transparent : t
(** [border_transparent] sets the border color to transparent. *)

val border_current : t
(** [border_current] sets the border color to currentColor. *)

val border_xs : t
(** [border_xs] is a semantic width alias – 1px. *)

val border_sm : t
(** [border_sm] is a semantic width alias – 2px. *)

val border_md : t
(** [border_md] is a semantic width alias – 4px. *)

val border_lg : t
(** [border_lg] is a semantic width alias – 4px. *)

val border_xl : t
(** [border_xl] is a semantic width alias – 8px. *)

val border_2xl : t
(** [border_2xl] is a semantic width alias – 8px. *)

val border_3xl : t
(** [border_3xl] is a semantic width alias – 8px. *)

val border_full : t
(** [border_full] is a semantic width alias – 8px. *)

val rounded_none : t
(** [rounded_none] sets no border radius. *)

val rounded_sm : t
(** [rounded_sm] sets a small border radius. *)

val rounded : t
(** [rounded] sets the default border radius. *)

val rounded_md : t
(** [rounded_md] sets a medium border radius. *)

val rounded_lg : t
(** [rounded_lg] sets a large border radius. *)

val rounded_xl : t
(** [rounded_xl] sets an extra-large border radius. *)

val rounded_2xl : t
(** [rounded_2xl] sets a 2× extra-large border radius. *)

val rounded_3xl : t
(** [rounded_3xl] sets a 3× extra-large border radius. *)

val rounded_full : t
(** [rounded_full] sets a fully rounded (max) border radius. *)

(** {2 Side-specific border radius} *)

val rounded_t : t
(** [rounded_t] rounds top corners with default radius. *)

val rounded_t_none : t
(** [rounded_t_none] sets no rounding on top corners. *)

val rounded_t_sm : t
(** [rounded_t_sm] sets small rounding on top corners. *)

val rounded_t_md : t
(** [rounded_t_md] sets medium rounding on top corners. *)

val rounded_t_lg : t
(** [rounded_t_lg] sets large rounding on top corners. *)

val rounded_t_xl : t
(** [rounded_t_xl] sets extra large rounding on top corners. *)

val rounded_t_2xl : t
(** [rounded_t_2xl] sets 2× rounding on top corners. *)

val rounded_t_3xl : t
(** [rounded_t_3xl] sets 3× rounding on top corners. *)

val rounded_t_full : t
(** [rounded_t_full] sets full rounding on top corners. *)

val rounded_r : t
(** [rounded_r] rounds right corners with default radius. *)

val rounded_r_none : t
(** [rounded_r_none] sets no rounding on right corners. *)

val rounded_r_sm : t
(** [rounded_r_sm] sets small rounding on right corners. *)

val rounded_r_md : t
(** [rounded_r_md] sets medium rounding on right corners. *)

val rounded_r_lg : t
(** [rounded_r_lg] sets large rounding on right corners. *)

val rounded_r_xl : t
(** [rounded_r_xl] sets extra large rounding on right corners. *)

val rounded_r_2xl : t
(** [rounded_r_2xl] sets 2× rounding on right corners. *)

val rounded_r_3xl : t
(** [rounded_r_3xl] sets 3× rounding on right corners. *)

val rounded_r_full : t
(** [rounded_r_full] sets full rounding on right corners. *)

val rounded_b : t
(** [rounded_b] rounds bottom corners with default radius. *)

val rounded_b_none : t
(** [rounded_b_none] sets no rounding on bottom corners. *)

val rounded_b_sm : t
(** [rounded_b_sm] sets small rounding on bottom corners. *)

val rounded_b_md : t
(** [rounded_b_md] sets medium rounding on bottom corners. *)

val rounded_b_lg : t
(** [rounded_b_lg] sets large rounding on bottom corners. *)

val rounded_b_xl : t
(** [rounded_b_xl] sets extra large rounding on bottom corners. *)

val rounded_b_2xl : t
(** [rounded_b_2xl] sets 2× rounding on bottom corners. *)

val rounded_b_3xl : t
(** [rounded_b_3xl] sets 3× rounding on bottom corners. *)

val rounded_b_full : t
(** [rounded_b_full] sets full rounding on bottom corners. *)

val rounded_l : t
(** [rounded_l] rounds left corners with default radius. *)

val rounded_l_none : t
(** [rounded_l_none] sets no rounding on left corners. *)

val rounded_l_sm : t
(** [rounded_l_sm] sets small rounding on left corners. *)

val rounded_l_md : t
(** [rounded_l_md] sets medium rounding on left corners. *)

val rounded_l_lg : t
(** [rounded_l_lg] sets large rounding on left corners. *)

val rounded_l_xl : t
(** [rounded_l_xl] sets extra large rounding on left corners. *)

val rounded_l_2xl : t
(** [rounded_l_2xl] sets 2× rounding on left corners. *)

val rounded_l_3xl : t
(** [rounded_l_3xl] sets 3× rounding on left corners. *)

val rounded_l_full : t
(** [rounded_l_full] sets full rounding on left corners. *)

(** {2 Corner-specific border radius} *)

val rounded_tl : t
(** [rounded_tl] rounds the top-left corner with default radius. *)

val rounded_tl_none : t
(** [rounded_tl_none] sets no rounding on top-left corner. *)

val rounded_tl_sm : t
(** [rounded_tl_sm] sets small rounding on top-left corner. *)

val rounded_tl_md : t
(** [rounded_tl_md] sets medium rounding on top-left corner. *)

val rounded_tl_lg : t
(** [rounded_tl_lg] sets large rounding on top-left corner. *)

val rounded_tl_xl : t
(** [rounded_tl_xl] sets extra large rounding on top-left corner. *)

val rounded_tl_2xl : t
(** [rounded_tl_2xl] sets 2× rounding on top-left corner. *)

val rounded_tl_3xl : t
(** [rounded_tl_3xl] sets 3× rounding on top-left corner. *)

val rounded_tl_full : t
(** [rounded_tl_full] sets full rounding on top-left corner. *)

val rounded_tr : t
(** [rounded_tr] rounds the top-right corner with default radius. *)

val rounded_tr_none : t
(** [rounded_tr_none] sets no rounding on top-right corner. *)

val rounded_tr_sm : t
(** [rounded_tr_sm] sets small rounding on top-right corner. *)

val rounded_tr_md : t
(** [rounded_tr_md] sets medium rounding on top-right corner. *)

val rounded_tr_lg : t
(** [rounded_tr_lg] sets large rounding on top-right corner. *)

val rounded_tr_xl : t
(** [rounded_tr_xl] sets extra large rounding on top-right corner. *)

val rounded_tr_2xl : t
(** [rounded_tr_2xl] sets 2× rounding on top-right corner. *)

val rounded_tr_3xl : t
(** [rounded_tr_3xl] sets 3× rounding on top-right corner. *)

val rounded_tr_full : t
(** [rounded_tr_full] sets full rounding on top-right corner. *)

val rounded_br : t
(** [rounded_br] rounds the bottom-right corner with default radius. *)

val rounded_br_none : t
(** [rounded_br_none] sets no rounding on bottom-right corner. *)

val rounded_br_sm : t
(** [rounded_br_sm] sets small rounding on bottom-right corner. *)

val rounded_br_md : t
(** [rounded_br_md] sets medium rounding on bottom-right corner. *)

val rounded_br_lg : t
(** [rounded_br_lg] sets large rounding on bottom-right corner. *)

val rounded_br_xl : t
(** [rounded_br_xl] sets extra large rounding on bottom-right corner. *)

val rounded_br_2xl : t
(** [rounded_br_2xl] sets 2× rounding on bottom-right corner. *)

val rounded_br_3xl : t
(** [rounded_br_3xl] sets 3× rounding on bottom-right corner. *)

val rounded_br_full : t
(** [rounded_br_full] sets full rounding on bottom-right corner. *)

val rounded_bl : t
(** [rounded_bl] rounds the bottom-left corner with default radius. *)

val rounded_bl_none : t
(** [rounded_bl_none] sets no rounding on bottom-left corner. *)

val rounded_bl_sm : t
(** [rounded_bl_sm] sets small rounding on bottom-left corner. *)

val rounded_bl_md : t
(** [rounded_bl_md] sets medium rounding on bottom-left corner. *)

val rounded_bl_lg : t
(** [rounded_bl_lg] sets large rounding on bottom-left corner. *)

val rounded_bl_xl : t
(** [rounded_bl_xl] sets extra large rounding on bottom-left corner. *)

val rounded_bl_2xl : t
(** [rounded_bl_2xl] sets 2× rounding on bottom-left corner. *)

val rounded_bl_3xl : t
(** [rounded_bl_3xl] sets 3× rounding on bottom-left corner. *)

val rounded_bl_full : t
(** [rounded_bl_full] sets full rounding on bottom-left corner. *)

(** {1 Outline Utilities} *)

val outline_none : t
(** [outline_none] removes the outline. *)

val outline_offset_0 : t
(** [outline_offset_0] sets outline offset to 0px. *)

val outline_offset_1 : t
(** [outline_offset_1] sets outline offset to 1px. *)

val outline_offset_2 : t
(** [outline_offset_2] sets outline offset to 2px. *)

val outline_offset_4 : t
(** [outline_offset_4] sets outline offset to 4px. *)

val outline_offset_8 : t
(** [outline_offset_8] sets outline offset to 8px. *)

module Handler : Utility.Handler
