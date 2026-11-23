(** Source scanning for class candidates, as Tailwind scans sources. *)

val split_whitespace : string -> string list
(** [split_whitespace s] is {!Tw.split_whitespace}: a class list on HTML/CSS
    whitespace. *)

val candidates : string -> string list
(** [candidates s] are the plausible class candidates in the UTF-8 text [s].
    Like Tailwind's scanner, it reads [s] as plain text and leaves deciding
    which candidates are utilities to the caller. *)

val candidates_from_file : string -> string list
(** [candidates_from_file file] is {!candidates} of the contents of [file]. *)

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
