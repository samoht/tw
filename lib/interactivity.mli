(** Interactivity utilities for selection, scroll behavior, and more *)

open Style

(** {1 Utility Type} *)

type utility =
  | Select_none
  | Select_text
  | Select_all
  | Select_auto
  | Scroll_auto
  | Scroll_smooth
  | Snap_start
  | Snap_end
  | Snap_center
  | Snap_none
  | Snap_x
  | Snap_y
  | Snap_both
  | Snap_mandatory
  | Snap_proximity
  | Snap_align_none
  | Snap_normal
  | Snap_always
  | Resize_none
  | Resize
  | Resize_x
  | Resize_y
  | Pointer_events_none
  | Pointer_events_auto
  | Appearance_none
  | Will_change_auto
  | Will_change_scroll
  | Will_change_contents
  | Will_change_transform

val to_style : utility -> t
(** [to_style utility] converts an interactivity utility to a style. *)

val suborder : utility -> int
(** [suborder utility] returns the suborder for utility ordering. *)

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

val of_string : string list -> (utility, [ `Msg of string ]) result
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
