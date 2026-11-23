(** {{:https://tailwindcss.com/docs/margin}Margin} utilities, negative values
    included. *)

open Utility

(** {1 Margin Utilities} *)

val m : int -> t
(** [m n] sets the margin on all sides to [n] steps of the spacing scale; a
    negative [n] gives a negative margin. *)

val m' : float -> t
(** [m' n] is {!m} for a half-step scale value (e.g. [m' 0.5] for [m-0.5]). *)

val mx : int -> t
(** [mx n] sets the margin on the left and right to [n] steps of the spacing
    scale; a negative [n] gives a negative margin. *)

val mx' : float -> t
(** [mx' n] is {!mx} for a half-step scale value. *)

val my : int -> t
(** [my n] sets the margin on the top and bottom to [n] steps of the spacing
    scale; a negative [n] gives a negative margin. *)

val my' : float -> t
(** [my' n] is {!my} for a half-step scale value. *)

val mt : int -> t
(** [mt n] sets the margin on the top to [n] steps of the spacing scale; a
    negative [n] gives a negative margin. *)

val mt' : float -> t
(** [mt' n] is {!mt} for a half-step scale value. *)

val mr : int -> t
(** [mr n] sets the margin on the right to [n] steps of the spacing scale; a
    negative [n] gives a negative margin. *)

val mr' : float -> t
(** [mr' n] is {!mr} for a half-step scale value. *)

val mb : int -> t
(** [mb n] sets the margin on the bottom to [n] steps of the spacing scale; a
    negative [n] gives a negative margin. *)

val mb' : float -> t
(** [mb' n] is {!mb} for a half-step scale value. *)

val ml : int -> t
(** [ml n] sets the margin on the left to [n] steps of the spacing scale; a
    negative [n] gives a negative margin. *)

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
