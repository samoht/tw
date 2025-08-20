(** Spacing utilities for padding, margin, and gap *)

open Core

(** {1 Value Constructors} *)

val rem : float -> spacing
val int : int -> spacing
val one_px : spacing
val full : spacing
val int_to_length : int -> Css.length

(** {1 Padding Utilities} *)

val p' : spacing -> t
val px' : spacing -> t
val py' : spacing -> t
val pt' : spacing -> t
val pr' : spacing -> t
val pb' : spacing -> t
val pl' : spacing -> t
val p : int -> t
val px : int -> t
val py : int -> t
val pt : int -> t
val pr : int -> t
val pb : int -> t
val pl : int -> t
val p_px : t
val p_full : t
val px_px : t
val px_full : t
val py_px : t
val py_full : t
val pt_px : t
val pt_full : t
val pr_px : t
val pr_full : t
val pb_px : t
val pb_full : t
val pl_px : t
val pl_full : t

(** {1 Margin Utilities} *)

val m' : margin -> t
val mx' : margin -> t
val my' : margin -> t
val mt' : margin -> t
val mr' : margin -> t
val mb' : margin -> t
val ml' : margin -> t
val m : int -> t
val mx : int -> t
val my : int -> t
val mt : int -> t
val mr : int -> t
val mb : int -> t
val ml : int -> t
val m_auto : t
val mx_auto : t
val my_auto : t
val mt_auto : t
val mr_auto : t
val mb_auto : t
val ml_auto : t

(** {1 Gap Utilities} *)

val gap' : spacing -> t
val gap_x' : spacing -> t
val gap_y' : spacing -> t
val gap : int -> t
val gap_x : int -> t
val gap_y : int -> t
val gap_px : t
val gap_full : t

(** {1 Space Between Utilities} *)

val space_x : int -> t
val space_y : int -> t
