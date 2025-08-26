(** Spacing utilities for padding and margin. *)

open Core

(** {1 Helper Functions} *)

val spacing_to_length : spacing -> Css.length
(** [spacing_to_length s] converts spacing to CSS length. *)

val margin_to_length : margin -> Css.length
(** [margin_to_length m] converts margin to CSS length. *)

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
val m_auto : t
val mx_auto : t
val my_auto : t
val mt_auto : t
val mr_auto : t
val mb_auto : t
val ml_auto : t

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a spacing/margin utility from string parts. *)
