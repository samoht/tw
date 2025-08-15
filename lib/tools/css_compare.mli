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
(** Convert a token to a string representation for debugging *)

val strip_header : string -> string
(** Strip header comments from CSS *)

val compare_css : string -> string -> bool
(** Compare two CSS strings structurally *)

val format_diff : string -> string -> string
(** Format differences between two CSS strings as human-readable text *)
