(** Text shadow utilities *)

open Utility

val text_shadow_none : t
(** [text_shadow_none] is the [text-shadow: none] utility. *)

val text_shadow_2xs : t
(** [text_shadow_2xs] is the [text-shadow-2xs] utility. *)

val text_shadow_xs : t
(** [text_shadow_xs] is the [text-shadow-xs] utility. *)

val text_shadow_sm : t
(** [text_shadow_sm] is the [text-shadow-sm] utility. *)

val text_shadow : t
(** [text_shadow] is the default [text-shadow] utility. *)

val text_shadow_md : t
(** [text_shadow_md] is the [text-shadow-md] utility. *)

val text_shadow_lg : t
(** [text_shadow_lg] is the [text-shadow-lg] utility. *)

val text_shadow_arbitrary : string -> t
(** [text_shadow_arbitrary v] is an arbitrary [text-shadow] utility. *)

module Handler : Utility.Handler
