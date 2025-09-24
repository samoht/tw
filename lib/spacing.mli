(** Spacing utilities for padding and margin. *)

open Core

(** {1 Helper Functions} *)

val to_length : Css.length Css.var -> spacing -> Css.length
(** [to_length spacing_ref s] converts spacing to CSS length using the provided
    spacing variable reference. *)

val margin_to_length : Css.length Css.var -> margin -> Css.length
(** [margin_to_length spacing_ref m] converts margin to CSS length using the
    provided spacing variable reference. *)

(** {1 Padding Utilities} *)

val p : int -> t
(** [p n] creates padding on all sides. *)

val px : int -> t
(** [px n] creates horizontal padding. *)

val py : int -> t
(** [py n] creates vertical padding. *)

val pt : int -> t
(** [pt n] creates top padding. *)

val pr : int -> t
(** [pr n] creates right padding. *)

val pb : int -> t
(** [pb n] creates bottom padding. *)

val pl : int -> t
(** [pl n] creates left padding. *)

val p' : spacing -> t
(** [p' s] sets padding on all sides using typed spacing [s]. *)

val px' : spacing -> t
(** [px' s] sets horizontal padding using typed spacing [s]. *)

val py' : spacing -> t
(** [py' s] sets vertical padding using typed spacing [s]. *)

val pt' : spacing -> t
(** [pt' s] sets top padding using typed spacing [s]. *)

val pr' : spacing -> t
(** [pr' s] sets right padding using typed spacing [s]. *)

val pb' : spacing -> t
(** [pb' s] sets bottom padding using typed spacing [s]. *)

val pl' : spacing -> t
(** [pl' s] sets left padding using typed spacing [s]. *)

(** {1 Margin Utilities} *)

val m : int -> t
(** [m n] creates margin on all sides (supports negative values). *)

val mx : int -> t
(** [mx n] creates horizontal margin (supports negative values). *)

val my : int -> t
(** [my n] creates vertical margin (supports negative values). *)

val mt : int -> t
(** [mt n] creates top margin (supports negative values). *)

val mr : int -> t
(** [mr n] creates right margin (supports negative values). *)

val mb : int -> t
(** [mb n] creates bottom margin (supports negative values). *)

val ml : int -> t
(** [ml n] creates left margin (supports negative values). *)

val m' : margin -> t
(** [m' m] sets margin on all sides using typed margin [m]. *)

val mx' : margin -> t
(** [mx' m] sets horizontal margin using typed margin [m]. *)

val my' : margin -> t
(** [my' m] sets vertical margin using typed margin [m]. *)

val mt' : margin -> t
(** [mt' m] sets top margin using typed margin [m]. *)

val mr' : margin -> t
(** [mr' m] sets right margin using typed margin [m]. *)

val mb' : margin -> t
(** [mb' m] sets bottom margin using typed margin [m]. *)

val ml' : margin -> t
(** [ml' m] sets left margin using typed margin [m]. *)

(** {1 Space Between Utilities} *)

val space_x : int -> t
(** [space_x n] creates horizontal space between child elements. *)

val space_y : int -> t
(** [space_y n] creates vertical space between child elements. *)

(** {1 Special Values} *)

val p_px : t
(** [p_px] sets padding to 1px on all sides. *)

val p_full : t
(** [p_full] sets padding to 100% on all sides. *)

val px_px : t
(** [px_px] sets horizontal padding to 1px. *)

val px_full : t
(** [px_full] sets horizontal padding to 100%. *)

val py_px : t
(** [py_px] sets vertical padding to 1px. *)

val py_full : t
(** [py_full] sets vertical padding to 100%. *)

val pt_px : t
(** [pt_px] sets top padding to 1px. *)

val pt_full : t
(** [pt_full] sets top padding to 100%. *)

val pr_px : t
(** [pr_px] sets right padding to 1px. *)

val pr_full : t
(** [pr_full] sets right padding to 100%. *)

val pb_px : t
(** [pb_px] sets bottom padding to 1px. *)

val pb_full : t
(** [pb_full] sets bottom padding to 100%. *)

val pl_px : t
(** [pl_px] sets left padding to 1px. *)

val pl_full : t
(** [pl_full] sets left padding to 100%. *)

val m_auto : t
(** [m_auto] sets margin to auto on all sides. *)

val mx_auto : t
(** [mx_auto] sets horizontal margin to auto. *)

val my_auto : t
(** [my_auto] sets vertical margin to auto. *)

val mt_auto : t
(** [mt_auto] sets top margin to auto. *)

val mr_auto : t
(** [mr_auto] sets right margin to auto. *)

val mb_auto : t
(** [mb_auto] sets bottom margin to auto. *)

val ml_auto : t
(** [ml_auto] sets left margin to auto. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a spacing/margin utility from string parts. *)
