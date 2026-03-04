(** Field sizing utilities *)

open Utility

val field_sizing_content : t
(** [field_sizing_content] is the [field-sizing: content] utility. *)

val field_sizing_fixed : t
(** [field_sizing_fixed] is the [field-sizing: fixed] utility. *)

module Handler : Utility.Handler
