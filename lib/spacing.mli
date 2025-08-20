(** Spacing utilities for padding, margin, and gap *)

open Core

(** {1 Helper Functions} *)

val spacing_to_length : spacing -> Css.length
(** [spacing_to_length s] converts spacing to CSS length. *)

val margin_to_length : margin -> Css.length
(** [margin_to_length m] converts margin to CSS length. *)

(** {1 Padding Utilities} *)

val p : spacing -> t
(** [p s] creates padding on all sides. *)

val px : spacing -> t
(** [px s] creates horizontal padding. *)

val py : spacing -> t
(** [py s] creates vertical padding. *)

val pt : spacing -> t
(** [pt s] creates top padding. *)

val pr : spacing -> t
(** [pr s] creates right padding. *)

val pb : spacing -> t
(** [pb s] creates bottom padding. *)

val pl : spacing -> t
(** [pl s] creates left padding. *)

(** {1 Margin Utilities} *)

val m : margin -> t
(** [m m] creates margin on all sides. *)

val mx : margin -> t
(** [mx m] creates horizontal margin. *)

val my : margin -> t
(** [my m] creates vertical margin. *)

val mt : margin -> t
(** [mt m] creates top margin. *)

val mr : margin -> t
(** [mr m] creates right margin. *)

val mb : margin -> t
(** [mb m] creates bottom margin. *)

val ml : margin -> t
(** [ml m] creates left margin. *)

(** {1 Gap Utilities} *)

val gap : spacing -> t
(** [gap s] creates gap between grid/flex items. *)

val gap_x : spacing -> t
(** [gap_x s] creates horizontal gap between grid/flex items. *)

val gap_y : spacing -> t
(** [gap_y s] creates vertical gap between grid/flex items. *)

(** {1 Space Between Utilities} *)

val space_x : spacing -> t
(** [space_x s] creates horizontal space between child elements. *)

val space_y : spacing -> t
(** [space_y s] creates vertical space between child elements. *)
