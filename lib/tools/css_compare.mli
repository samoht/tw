(** CSS comparison utilities for testing *)

(** CSS token type *)
type token =
  | Selector of string
  | Property of string * string
  | OpenBrace
  | CloseBrace
  | AtRule of string
  | Semicolon
  | Comma

val token_to_string : token -> string
(** [token_to_string token] converts a token to a string representation for
    debugging. *)

val strip_header : string -> string
(** [strip_header css] strips header comments from CSS. *)

val compare_css : string -> string -> bool
(** [compare_css css1 css2] compares two CSS strings structurally. *)

val format_diff : string -> string -> string
(** [format_diff css1 css2] formats differences between two CSS strings as
    human-readable text. *)
