(** Text shadow utilities *)

open Utility

val text_shadow_none : t
val text_shadow_2xs : t
val text_shadow_xs : t
val text_shadow_sm : t
val text_shadow : t
val text_shadow_md : t
val text_shadow_lg : t
val text_shadow_arbitrary : string -> t

module Handler : Utility.Handler
