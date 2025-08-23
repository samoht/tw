(** Interactivity utilities for cursor, selection, and scroll behavior *)

open Core

(** {1 Cursor Utilities} *)

val cursor_auto : t
(** Browser determines cursor based on context. *)

val cursor_default : t
(** Default cursor (usually an arrow). *)

val cursor_pointer : t
(** Pointing hand cursor for clickable elements. *)

val cursor_wait : t
(** Wait cursor indicating processing. *)

val cursor_move : t
(** Move cursor for draggable elements. *)

val cursor_not_allowed : t
(** Not-allowed cursor for disabled elements. *)

val cursor_text : t
(** Text selection cursor. *)

val cursor_crosshair : t
(** Crosshair cursor. *)

val cursor_help : t
(** Help cursor. *)

val cursor_grab : t
(** Open hand cursor for grabbable elements. *)

val cursor_grabbing : t
(** Closed hand cursor when grabbing. *)

(** {1 User Select Utilities} *)

val select_none : t
(** Prevents text selection. *)

val select_text : t
(** Allows text selection. *)

val select_all : t
(** Selects all text on click. *)

val select_auto : t
(** Default browser text selection behavior. *)

(** {1 Scroll Behavior Utilities} *)

val scroll_auto : t
(** Default instant scrolling behavior. *)

val scroll_smooth : t
(** Smooth scrolling animation. *)

(** {1 Scroll Snap Utilities} *)

val snap_start : t
(** Snap to start of container. *)

val snap_end : t
(** Snap to end of container. *)

val snap_center : t
(** Snap to center of container. *)

val snap_none : t
(** No scroll snapping. *)

val snap_mandatory : t
(** Mandatory scroll snapping. *)

val snap_proximity : t
(** Proximity-based scroll snapping. *)

(** {1 Resize Utilities} *)

val resize_none : t
(** Prevents resizing. *)

val resize : t
(** Allows both horizontal and vertical resizing. *)

val resize_x : t
(** Allows horizontal resizing only. *)

val resize_y : t
(** Allows vertical resizing only. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses an interactivity utility from string parts. *)
