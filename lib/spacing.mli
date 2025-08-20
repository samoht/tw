(** Spacing utilities for padding, margin, and gap *)

open Core

(** {1 Value Constructors} *)

val rem : float -> spacing
(** [rem f] creates a rem-based spacing value. *)

val int : int -> spacing
(** [int n] creates spacing based on Tailwind's 0.25rem scale. *)

val one_px : spacing
(** [one_px] represents 1px spacing. *)

val full : spacing
(** [full] represents 100% spacing. *)

val int_to_length : int -> Css.length
(** [int_to_length n] converts an integer to CSS length. *)

(** {1 Padding Utilities} *)

val p' : spacing -> t
(** [p' s] creates padding on all sides. *)

val px' : spacing -> t
(** [px' s] creates horizontal padding. *)

val py' : spacing -> t
(** [py' s] creates vertical padding. *)

val pt' : spacing -> t
(** [pt' s] creates top padding. *)

val pr' : spacing -> t
(** [pr' s] creates right padding. *)

val pb' : spacing -> t
(** [pb' s] creates bottom padding. *)

val pl' : spacing -> t
(** [pl' s] creates left padding. *)

val p : int -> t
(** [p n] creates padding on all sides using integer scale. *)

val px : int -> t
(** [px n] creates horizontal padding using integer scale. *)

val py : int -> t
(** [py n] creates vertical padding using integer scale. *)

val pt : int -> t
(** [pt n] creates top padding using integer scale. *)

val pr : int -> t
(** [pr n] creates right padding using integer scale. *)

val pb : int -> t
(** [pb n] creates bottom padding using integer scale. *)

val pl : int -> t
(** [pl n] creates left padding using integer scale. *)

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
(** [m' m] creates margin on all sides. *)

val mx' : margin -> t
(** [mx' m] creates horizontal margin. *)

val my' : margin -> t
(** [my' m] creates vertical margin. *)

val mt' : margin -> t
(** [mt' m] creates top margin. *)

val mr' : margin -> t
(** [mr' m] creates right margin. *)

val mb' : margin -> t
(** [mb' m] creates bottom margin. *)

val ml' : margin -> t
(** [ml' m] creates left margin. *)

val m : int -> t
(** [m n] creates margin on all sides using integer scale. *)

val mx : int -> t
(** [mx n] creates horizontal margin using integer scale. *)

val my : int -> t
(** [my n] creates vertical margin using integer scale. *)

val mt : int -> t
(** [mt n] creates top margin using integer scale. *)

val mr : int -> t
(** [mr n] creates right margin using integer scale. *)

val mb : int -> t
(** [mb n] creates bottom margin using integer scale. *)

val ml : int -> t
(** [ml n] creates left margin using integer scale. *)

val m_auto : t
val mx_auto : t
val my_auto : t
val mt_auto : t
val mr_auto : t
val mb_auto : t
val ml_auto : t

(** {1 Gap Utilities} *)

val gap' : spacing -> t
(** [gap' s] creates gap between grid/flex items. *)

val gap_x' : spacing -> t
(** [gap_x' s] creates horizontal gap between grid/flex items. *)

val gap_y' : spacing -> t
(** [gap_y' s] creates vertical gap between grid/flex items. *)

val gap : int -> t
(** [gap n] creates gap using integer scale. *)

val gap_x : int -> t
(** [gap_x n] creates horizontal gap using integer scale. *)

val gap_y : int -> t
(** [gap_y n] creates vertical gap using integer scale. *)

val gap_px : t
(** [gap_px] creates 1px gap. *)

val gap_full : t
(** [gap_full] creates 100% gap. *)

(** {1 Space Between Utilities} *)

val space_x : int -> t
(** [space_x n] creates horizontal space between child elements. *)

val space_y : int -> t
(** [space_y n] creates vertical space between child elements. *)
