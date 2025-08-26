(** Interactivity utilities for cursor, selection, and scroll behavior *)

open Core

(** {1 Cursor Utilities} *)

val cursor_auto : t
(** [cursor_auto] lets the browser determine the cursor based on context. *)

val cursor_default : t
(** [cursor_default] uses the default cursor (usually an arrow). *)

val cursor_pointer : t
(** [cursor_pointer] uses the pointing hand cursor for clickable elements. *)

val cursor_wait : t
(** [cursor_wait] uses the wait cursor to indicate processing. *)

val cursor_move : t
(** [cursor_move] uses the move cursor for draggable elements. *)

val cursor_not_allowed : t
(** [cursor_not_allowed] uses the not-allowed cursor for disabled elements. *)

val cursor_text : t
(** [cursor_text] uses the text selection cursor. *)

val cursor_crosshair : t
(** [cursor_crosshair] uses the crosshair cursor. *)

val cursor_help : t
(** [cursor_help] uses the help cursor. *)

val cursor_grab : t
(** [cursor_grab] uses the open hand cursor for grabbable elements. *)

val cursor_grabbing : t
(** [cursor_grabbing] uses the closed hand cursor when grabbing. *)

(** {1 User Select Utilities} *)

val select_none : t
(** [select_none] prevents text selection. *)

val select_text : t
(** [select_text] allows text selection. *)

val select_all : t
(** [select_all] selects all text on click. *)

val select_auto : t
(** [select_auto] uses the default browser text selection behavior. *)

(** {1 Scroll Behavior Utilities} *)

val scroll_auto : t
(** [scroll_auto] uses the default instant scrolling behavior. *)

val scroll_smooth : t
(** [scroll_smooth] enables smooth scrolling animation. *)

(** {1 Scroll Snap Utilities} *)

val snap_start : t
(** [snap_start] snaps to the start of the container. *)

val snap_end : t
(** [snap_end] snaps to the end of the container. *)

val snap_center : t
(** [snap_center] snaps to the center of the container. *)

val snap_none : t
(** [snap_none] disables scroll snapping. *)

val snap_mandatory : t
(** [snap_mandatory] enforces mandatory scroll snapping. *)

val snap_proximity : t
(** [snap_proximity] enables proximity-based scroll snapping. *)

val snap_x : t
(** [snap_x] enables horizontal scroll snapping. *)

val snap_y : t
(** [snap_y] enables vertical scroll snapping. *)

val snap_both : t
(** [snap_both] enables both-axis scroll snapping. *)

val snap_align_none : t
(** [snap_align_none] disables snap alignment. *)

val snap_normal : t
(** [snap_normal] uses normal snap stop behavior. *)

val snap_always : t
(** [snap_always] always stops at snap positions. *)

(** {1 Resize Utilities} *)

val resize_none : t
(** [resize_none] prevents resizing. *)

val resize : t
(** [resize] allows both horizontal and vertical resizing. *)

val resize_x : t
(** [resize_x] allows horizontal resizing only. *)

val resize_y : t
(** [resize_y] allows vertical resizing only. *)

(** {1 Parsing Functions} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses an interactivity utility from string parts. *)

(** {1 Pointer Events} *)

val pointer_events_none : t
(** [pointer_events_none] disables pointer interactions. *)

val pointer_events_auto : t
(** [pointer_events_auto] enables default pointer interactions. *)

(** {1 Appearance} *)

val appearance_none : t
(** [appearance_none] removes native OS/browser control styling. *)

(** {1 Will-Change} *)

val will_change_auto : t
(** [will_change_auto] sets will-change to auto. *)

val will_change_scroll : t
(** [will_change_scroll] optimizes for scroll position changes. *)

val will_change_contents : t
(** [will_change_contents] optimizes for content changes. *)

val will_change_transform : t
(** [will_change_transform] optimizes for transform changes. *)
