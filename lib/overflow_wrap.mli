(** Overflow wrap utilities *)

open Utility

val wrap_normal : t
(** [wrap_normal] uses default word wrapping behavior. *)

val wrap_break_word : t
(** [wrap_break_word] breaks words to prevent overflow. *)

val wrap_anywhere : t
(** [wrap_anywhere] breaks at any character to prevent overflow. *)

module Handler : Utility.Handler
