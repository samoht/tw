(** Padding utilities

    https://tailwindcss.com/docs/padding *)

open Utility

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

module Handler : Utility.Handler
