(** Scroll margin and padding utilities *)

open Utility

val scroll_m : int -> t
val scroll_mx : int -> t
val scroll_my : int -> t
val scroll_mt : int -> t
val scroll_mr : int -> t
val scroll_mb : int -> t
val scroll_ml : int -> t
val scroll_ms : int -> t
val scroll_me : int -> t
val scroll_mbs : int -> t
val scroll_mbe : int -> t
val scroll_p : int -> t
val scroll_px : int -> t
val scroll_py : int -> t
val scroll_pt : int -> t
val scroll_pr : int -> t
val scroll_pb : int -> t
val scroll_pl : int -> t
val scroll_ps : int -> t
val scroll_pe : int -> t
val scroll_pbs : int -> t
val scroll_pbe : int -> t

module Handler : Utility.Handler
