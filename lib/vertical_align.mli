(** Vertical align utilities.

    https://tailwindcss.com/docs/vertical-align *)

open Utility

val align_baseline : t
(** [align_baseline] sets vertical-align to baseline. *)

val align_top : t
(** [align_top] sets vertical-align to top. *)

val align_middle : t
(** [align_middle] sets vertical-align to middle. *)

val align_bottom : t
(** [align_bottom] sets vertical-align to bottom. *)

val align_text_top : t
(** [align_text_top] sets vertical-align to text-top. *)

val align_text_bottom : t
(** [align_text_bottom] sets vertical-align to text-bottom. *)

val align_sub : t
(** [align_sub] sets vertical-align to sub. *)

val align_super : t
(** [align_super] sets vertical-align to super. *)

module Handler : Utility.Handler
