(** Tailwind-like source scanning. *)

val split_whitespace : string -> string list
(** Split a class list on HTML/CSS whitespace. *)

val candidates : string -> string list
(** Extract candidate class tokens from UTF-8 source text.

    This follows Tailwind's broad source-scanning model: scan plain text for
    plausible class candidates and leave utility validation to the caller. *)

val candidates_from_file : string -> string list
(** Read a UTF-8 source file and extract candidate class tokens. *)

(** {1 Source globs} *)

val is_glob : string -> bool
(** [is_glob path] is [true] when [path] holds a [*], [?] or [{], so it names
    files by pattern rather than one file or directory. *)

val glob_root : string -> string * string
(** [glob_root pattern] splits [pattern] before its first segment holding a
    glob: the directory every matching file sits under, and the pattern the rest
    of a file's path has to match. [glob_root "../src/**/*.html"] is
    [("../src", "**/*.html")]. *)

val glob_matches : pattern:string -> string -> bool
(** [glob_matches ~pattern path] is [true] when the [/]-separated [path] matches
    [pattern]: [**] is any number of segments, [*] any run of characters within
    one, [?] any one character, and [{a,b}] either alternative. *)
