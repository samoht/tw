(** Text overflow utilities.

    https://tailwindcss.com/docs/text-overflow *)

open Utility

val text_ellipsis : t
(** [text_ellipsis] sets text-overflow to ellipsis. *)

val text_clip : t
(** [text_clip] sets text-overflow to clip. *)

module Handler : Utility.Handler
