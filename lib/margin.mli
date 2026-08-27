(** Margin utilities with negative value support

    https://tailwindcss.com/docs/margin *)

open Utility

(** {1 Margin Utilities} *)

val m : int -> t
(** [m n] creates margin on all sides (supports negative values). *)

val m' : float -> t
(** [m' n] is {!m} for a half-step scale value (e.g. [m' 0.5] for [m-0.5]). *)

val mx : int -> t
(** [mx n] creates horizontal margin (supports negative values). *)

val mx' : float -> t
(** [mx' n] is {!mx} for a half-step scale value. *)

val my : int -> t
(** [my n] creates vertical margin (supports negative values). *)

val my' : float -> t
(** [my' n] is {!my} for a half-step scale value. *)

val mt : int -> t
(** [mt n] creates top margin (supports negative values). *)

val mt' : float -> t
(** [mt' n] is {!mt} for a half-step scale value. *)

val mr : int -> t
(** [mr n] creates right margin (supports negative values). *)

val mr' : float -> t
(** [mr' n] is {!mr} for a half-step scale value. *)

val mb : int -> t
(** [mb n] creates bottom margin (supports negative values). *)

val mb' : float -> t
(** [mb' n] is {!mb} for a half-step scale value. *)

val ml : int -> t
(** [ml n] creates left margin (supports negative values). *)

val ml' : float -> t
(** [ml' n] is {!ml} for a half-step scale value. *)

(** {1 Special Values} *)

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

module Handler : Utility.Handler
