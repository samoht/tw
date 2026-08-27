(** Scroll margin and padding utilities *)

open Utility

val scroll_m : int -> t
(** [scroll_m n] is the [scroll-m-{n}] utility. *)

val scroll_m' : float -> t
(** [scroll_m' n] is {!scroll_m} for a half-step scale value (e.g.
    [scroll_m' 0.5] for [scroll-m-0.5]). *)

val scroll_mx : int -> t
(** [scroll_mx n] is the [scroll-mx-{n}] utility. *)

val scroll_mx' : float -> t
(** [scroll_mx' n] is {!scroll_mx} for a half-step scale value. *)

val scroll_my : int -> t
(** [scroll_my n] is the [scroll-my-{n}] utility. *)

val scroll_my' : float -> t
(** [scroll_my' n] is {!scroll_my} for a half-step scale value. *)

val scroll_mt : int -> t
(** [scroll_mt n] is the [scroll-mt-{n}] utility. *)

val scroll_mt' : float -> t
(** [scroll_mt' n] is {!scroll_mt} for a half-step scale value. *)

val scroll_mr : int -> t
(** [scroll_mr n] is the [scroll-mr-{n}] utility. *)

val scroll_mr' : float -> t
(** [scroll_mr' n] is {!scroll_mr} for a half-step scale value. *)

val scroll_mb : int -> t
(** [scroll_mb n] is the [scroll-mb-{n}] utility. *)

val scroll_mb' : float -> t
(** [scroll_mb' n] is {!scroll_mb} for a half-step scale value. *)

val scroll_ml : int -> t
(** [scroll_ml n] is the [scroll-ml-{n}] utility. *)

val scroll_ml' : float -> t
(** [scroll_ml' n] is {!scroll_ml} for a half-step scale value. *)

val scroll_ms : int -> t
(** [scroll_ms n] is the [scroll-ms-{n}] utility. *)

val scroll_ms' : float -> t
(** [scroll_ms' n] is {!scroll_ms} for a half-step scale value. *)

val scroll_me : int -> t
(** [scroll_me n] is the [scroll-me-{n}] utility. *)

val scroll_me' : float -> t
(** [scroll_me' n] is {!scroll_me} for a half-step scale value. *)

val scroll_mbs : int -> t
(** [scroll_mbs n] is the [scroll-mbs-{n}] utility. *)

val scroll_mbs' : float -> t
(** [scroll_mbs' n] is {!scroll_mbs} for a half-step scale value. *)

val scroll_mbe : int -> t
(** [scroll_mbe n] is the [scroll-mbe-{n}] utility. *)

val scroll_mbe' : float -> t
(** [scroll_mbe' n] is {!scroll_mbe} for a half-step scale value. *)

val scroll_p : int -> t
(** [scroll_p n] is the [scroll-p-{n}] utility. *)

val scroll_p' : float -> t
(** [scroll_p' n] is {!scroll_p} for a half-step scale value. *)

val scroll_px : int -> t
(** [scroll_px n] is the [scroll-px-{n}] utility. *)

val scroll_px' : float -> t
(** [scroll_px' n] is {!scroll_px} for a half-step scale value. *)

val scroll_py : int -> t
(** [scroll_py n] is the [scroll-py-{n}] utility. *)

val scroll_py' : float -> t
(** [scroll_py' n] is {!scroll_py} for a half-step scale value. *)

val scroll_pt : int -> t
(** [scroll_pt n] is the [scroll-pt-{n}] utility. *)

val scroll_pt' : float -> t
(** [scroll_pt' n] is {!scroll_pt} for a half-step scale value. *)

val scroll_pr : int -> t
(** [scroll_pr n] is the [scroll-pr-{n}] utility. *)

val scroll_pr' : float -> t
(** [scroll_pr' n] is {!scroll_pr} for a half-step scale value. *)

val scroll_pb : int -> t
(** [scroll_pb n] is the [scroll-pb-{n}] utility. *)

val scroll_pb' : float -> t
(** [scroll_pb' n] is {!scroll_pb} for a half-step scale value. *)

val scroll_pl : int -> t
(** [scroll_pl n] is the [scroll-pl-{n}] utility. *)

val scroll_pl' : float -> t
(** [scroll_pl' n] is {!scroll_pl} for a half-step scale value. *)

val scroll_ps : int -> t
(** [scroll_ps n] is the [scroll-ps-{n}] utility. *)

val scroll_ps' : float -> t
(** [scroll_ps' n] is {!scroll_ps} for a half-step scale value. *)

val scroll_pe : int -> t
(** [scroll_pe n] is the [scroll-pe-{n}] utility. *)

val scroll_pe' : float -> t
(** [scroll_pe' n] is {!scroll_pe} for a half-step scale value. *)

val scroll_pbs : int -> t
(** [scroll_pbs n] is the [scroll-pbs-{n}] utility. *)

val scroll_pbs' : float -> t
(** [scroll_pbs' n] is {!scroll_pbs} for a half-step scale value. *)

val scroll_pbe : int -> t
(** [scroll_pbe n] is the [scroll-pbe-{n}] utility. *)

val scroll_pbe' : float -> t
(** [scroll_pbe' n] is {!scroll_pbe} for a half-step scale value. *)

module Handler : Utility.Handler
