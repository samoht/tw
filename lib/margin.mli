(** Margin utilities with negative value support *)

open Style
open Utility

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

val m' : margin -> Style.t
(** [m' m] sets margin on all sides using typed margin [m]. *)

val mx' : margin -> Style.t
(** [mx' m] sets horizontal margin using typed margin [m]. *)

val my' : margin -> Style.t
(** [my' m] sets vertical margin using typed margin [m]. *)

val mt' : margin -> Style.t
(** [mt' m] sets top margin using typed margin [m]. *)

val mr' : margin -> Style.t
(** [mr' m] sets right margin using typed margin [m]. *)

val mb' : margin -> Style.t
(** [mb' m] sets bottom margin using typed margin [m]. *)

val ml' : margin -> Style.t
(** [ml' m] sets left margin using typed margin [m]. *)

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

(** {1 Internal Functions} *)

val to_style : base -> Style.t option
(** [to_style u] converts a structured margin utility to a style. For internal
    use by the Tw module. *)

val suborder : base -> int option
(** [suborder u] returns the ordering value for margin utility [u]. Used for
    deterministic CSS output ordering. *)

val order : base -> (int * int) option
(** [order u] returns the (priority, suborder) pair for margin utility [u]. *)

val of_string : string list -> (base, [ `Msg of string ]) result
(** [of_string parts] parses a margin utility from string parts. Returns an
    internal structured representation. *)
