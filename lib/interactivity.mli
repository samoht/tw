(** Interactivity utilities for selection, scroll behavior, and more

    https://tailwindcss.com/docs/user-select
    https://tailwindcss.com/docs/scroll-behavior
    https://tailwindcss.com/docs/scroll-snap-align
    https://tailwindcss.com/docs/resize
    https://tailwindcss.com/docs/pointer-events
    https://tailwindcss.com/docs/appearance
    https://tailwindcss.com/docs/will-change *)

(** {1 User Select Utilities} *)

val select_none : Utility.t
(** [select_none] prevents text selection. *)

val select_text : Utility.t
(** [select_text] allows text selection. *)

val select_all : Utility.t
(** [select_all] selects all text on click. *)

val select_auto : Utility.t
(** [select_auto] uses the default browser text selection behavior. *)

(** {1 Scroll Behavior Utilities} *)

val scroll_auto : Utility.t
(** [scroll_auto] uses the default instant scrolling behavior. *)

val scroll_smooth : Utility.t
(** [scroll_smooth] enables smooth scrolling animation. *)

(** {1 Scroll Snap Utilities} *)

val snap_start : Utility.t
(** [snap_start] snaps to the start of the container. *)

val snap_end : Utility.t
(** [snap_end] snaps to the end of the container. *)

val snap_center : Utility.t
(** [snap_center] snaps to the center of the container. *)

val snap_none : Utility.t
(** [snap_none] disables scroll snapping. *)

val snap_mandatory : Utility.t
(** [snap_mandatory] enforces mandatory scroll snapping. *)

val snap_proximity : Utility.t
(** [snap_proximity] enables proximity-based scroll snapping. *)

val snap_x : Utility.t
(** [snap_x] enables horizontal scroll snapping. *)

val snap_y : Utility.t
(** [snap_y] enables vertical scroll snapping. *)

val snap_both : Utility.t
(** [snap_both] enables both-axis scroll snapping. *)

val snap_align_none : Utility.t
(** [snap_align_none] disables snap alignment. *)

val snap_normal : Utility.t
(** [snap_normal] uses normal snap stop behavior. *)

val snap_always : Utility.t
(** [snap_always] always stops at snap positions. *)

(** {1 Resize Utilities} *)

val resize_none : Utility.t
(** [resize_none] prevents resizing. *)

val resize : Utility.t
(** [resize] allows both horizontal and vertical resizing. *)

val resize_x : Utility.t
(** [resize_x] allows horizontal resizing only. *)

val resize_y : Utility.t
(** [resize_y] allows vertical resizing only. *)

(** {1 Pointer Events} *)

val pointer_events_none : Utility.t
(** [pointer_events_none] disables pointer interactions. *)

val pointer_events_auto : Utility.t
(** [pointer_events_auto] enables default pointer interactions. *)

(** {1 Appearance} *)

val appearance_none : Utility.t
(** [appearance_none] removes native OS/browser control styling. *)

(** {1 Will-Change} *)

val will_change_auto : Utility.t
(** [will_change_auto] sets will-change to auto. *)

val will_change_scroll : Utility.t
(** [will_change_scroll] optimizes for scroll position changes. *)

val will_change_contents : Utility.t
(** [will_change_contents] optimizes for content changes. *)

val will_change_transform : Utility.t
(** [will_change_transform] optimizes for transform changes. *)

(** {1 Marker Classes} *)

val group : Utility.t
(** [group] marks an element as a group parent for group-hover, group-focus,
    etc. *)

val peer : Utility.t
(** [peer] marks an element as a peer for peer-hover, peer-focus, etc. *)

module Handler : sig
  type t

  val of_class : string -> (t, [ `Msg of string ]) result
  val to_class : t -> string
  val suborder : t -> int
  val to_style : t -> Style.t
end
