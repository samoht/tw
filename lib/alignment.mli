(** Alignment utilities for flexbox and grid layouts

    https://tailwindcss.com/docs/justify-content
    https://tailwindcss.com/docs/align-items
    https://tailwindcss.com/docs/align-content
    https://tailwindcss.com/docs/align-self
    https://tailwindcss.com/docs/place-content
    https://tailwindcss.com/docs/place-items
    https://tailwindcss.com/docs/place-self *)

open Utility

(** {1 Justify Content Utilities} *)

val justify_start : t
(** [justify_start] justifies items to the start of the main axis. *)

val justify_end : t
(** [justify_end] justifies items to the end of the main axis. *)

val justify_center : t
(** [justify_center] centers items along the main axis. *)

val justify_between : t
(** [justify_between] distributes items with space between them. *)

val justify_around : t
(** [justify_around] distributes items with space around them. *)

val justify_evenly : t
(** [justify_evenly] distributes items with equal space around them. *)

(** {1 Align Items Utilities} *)

val items_start : t
(** [items_start] aligns items to the start of the cross axis. *)

val items_end : t
(** [items_end] aligns items to the end of the cross axis. *)

val items_center : t
(** [items_center] centers items along the cross axis. *)

val items_baseline : t
(** [items_baseline] aligns items along their baseline. *)

val items_stretch : t
(** [items_stretch] stretches items to fill the container (default). *)

(** {1 Align Content Utilities} *)

val content_start : t
(** [content_start] aligns content to the start. *)

val content_end : t
(** [content_end] aligns content to the end. *)

val content_center : t
(** [content_center] centers content. *)

val content_between : t
(** [content_between] distributes content with space between. *)

val content_around : t
(** [content_around] distributes content with space around. *)

val content_evenly : t
(** [content_evenly] distributes content evenly. *)

val content_stretch : t
(** [content_stretch] stretches content. *)

(** {1 Align Self Utilities} *)

val self_auto : t
(** [self_auto] sets align-self to auto. *)

val self_start : t
(** [self_start] sets align-self to flex-start. *)

val self_end : t
(** [self_end] sets align-self to flex-end. *)

val self_center : t
(** [self_center] sets align-self to center. *)

val self_baseline : t
(** [self_baseline] sets align-self to baseline. *)

val self_stretch : t
(** [self_stretch] sets align-self to stretch. *)

(** {1 Justify Items Utilities} *)

val justify_items_start : t
(** [justify_items_start] justifies items to the start. *)

val justify_items_end : t
(** [justify_items_end] justifies items to the end. *)

val justify_items_center : t
(** [justify_items_center] justifies items to the center. *)

val justify_items_stretch : t
(** [justify_items_stretch] stretches items to fill. *)

(** {1 Justify Self Utilities} *)

val justify_self_auto : t
(** [justify_self_auto] uses automatic self justification. *)

val justify_self_start : t
(** [justify_self_start] justifies self to the start. *)

val justify_self_end : t
(** [justify_self_end] justifies self to the end. *)

val justify_self_center : t
(** [justify_self_center] justifies self to the center. *)

val justify_self_stretch : t
(** [justify_self_stretch] stretches self to fill. *)

(** {1 Place Content Utilities} *)

val place_content_start : t
(** [place_content_start] places content at the start. *)

val place_content_end : t
(** [place_content_end] places content at the end. *)

val place_content_center : t
(** [place_content_center] places content at the center. *)

val place_content_between : t
(** [place_content_between] places content with space between. *)

val place_content_around : t
(** [place_content_around] places content with space around. *)

val place_content_evenly : t
(** [place_content_evenly] places content with space evenly. *)

val place_content_stretch : t
(** [place_content_stretch] stretches content to fill. *)

(** {1 Place Items Utilities} *)

val place_items_start : t
(** [place_items_start] places items at the start. *)

val place_items_end : t
(** [place_items_end] places items at the end. *)

val place_items_center : t
(** [place_items_center] places items at the center. *)

val place_items_stretch : t
(** [place_items_stretch] stretches items to fill. *)

(** {1 Place Self Utilities} *)

val place_self_auto : t
(** [place_self_auto] uses automatic self placement. *)

val place_self_start : t
(** [place_self_start] places self at the start. *)

val place_self_end : t
(** [place_self_end] places self at the end. *)

val place_self_center : t
(** [place_self_center] places self at the center. *)

val place_self_stretch : t
(** [place_self_stretch] stretches self to fill. *)

module Handler : Utility.Handler
