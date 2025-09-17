(** Concise diff formatting utilities. *)

val format_diff : original:string -> actual:string -> string
(** [format_diff ~original ~actual] returns a concise diff showing the first
    difference between two strings. Returns a formatted string with:
    - Original string snippet (up to 80 chars around difference)
    - New string snippet (same range)
    - Caret pointing to the difference position.

    @param original the original/expected string
    @param actual the actual/generated string
    @return formatted diff string with newlines. *)

val eprintf_diff : original:string -> actual:string -> unit
(** [eprintf_diff ~original ~actual] prints a concise diff to stderr.
    @param original the original/expected string.
    @param actual the actual/generated string. *)
