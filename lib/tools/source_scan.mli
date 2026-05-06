(** Tailwind-like source scanning. *)

val split_whitespace : string -> string list
(** Split a class list on HTML/CSS whitespace. *)

val candidates : string -> string list
(** Extract candidate class tokens from UTF-8 source text.

    This follows Tailwind's broad source-scanning model: scan plain text for
    plausible class candidates and leave utility validation to the caller. *)

val candidates_from_file : string -> string list
(** Read a UTF-8 source file and extract candidate class tokens. *)
