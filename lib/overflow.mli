(** Overflow utilities *)

open Utility

val overflow_auto : t
(** [overflow_auto] sets overflow to auto. *)

val overflow_hidden : t
(** [overflow_hidden] sets overflow to hidden. *)

val overflow_clip : t
(** [overflow_clip] sets overflow to clip. *)

val overflow_visible : t
(** [overflow_visible] sets overflow to visible. *)

val overflow_scroll : t
(** [overflow_scroll] sets overflow to scroll. *)

val overflow_x_auto : t
(** [overflow_x_auto] sets overflow-x to auto. *)

val overflow_x_hidden : t
(** [overflow_x_hidden] sets overflow-x to hidden. *)

val overflow_x_visible : t
(** [overflow_x_visible] sets overflow-x to visible. *)

val overflow_x_scroll : t
(** [overflow_x_scroll] sets overflow-x to scroll. *)

val overflow_y_auto : t
(** [overflow_y_auto] sets overflow-y to auto. *)

val overflow_y_hidden : t
(** [overflow_y_hidden] sets overflow-y to hidden. *)

val overflow_y_visible : t
(** [overflow_y_visible] sets overflow-y to visible. *)

val overflow_y_scroll : t
(** [overflow_y_scroll] sets overflow-y to scroll. *)

module Handler : Utility.Handler
