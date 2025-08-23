(** Spacing utilities for padding, margin, and gap *)

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
(** Typed versions with ' suffix *)

val px' : spacing -> t
val py' : spacing -> t
val pt' : spacing -> t
val pr' : spacing -> t
val pb' : spacing -> t
val pl' : spacing -> t

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
(** Typed versions with ' suffix *)

val mx' : margin -> t
val my' : margin -> t
val mt' : margin -> t
val mr' : margin -> t
val mb' : margin -> t
val ml' : margin -> t

(** {1 Gap Utilities} *)

val gap : int -> t
(** [gap n] creates gap between grid/flex items. *)

val gap_x : int -> t
(** [gap_x n] creates horizontal gap between grid/flex items. *)

val gap_y : int -> t
(** [gap_y n] creates vertical gap between grid/flex items. *)

val gap' : spacing -> t
(** Typed versions with ' suffix *)

val gap_x' : spacing -> t
val gap_y' : spacing -> t

(** {1 Space Between Utilities} *)

val space_x : int -> t
(** [space_x n] creates horizontal space between child elements. *)

val space_y : int -> t
(** [space_y n] creates vertical space between child elements. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a spacing/margin utility from string parts. *)
