(** Tailwind CSS generation utilities for testing *)

val generate :
  ?minify:bool ->
  ?optimize:bool ->
  ?forms:bool ->
  ?input_css:string ->
  string list ->
  string
(** [generate ?minify ?optimize ?forms ?input_css classnames] generates Tailwind
    CSS for given class names.

    Each class name is handed to the CLI as an [@source inline] candidate, which
    reaches the engine without passing Tailwind's source extractor. The
    extractor declines spellings the engine compiles, and it drops a candidate
    rather than compiling it differently, so a sheet built by scanning is short
    whole rules and a comparison against it reports them as tw's invention.
    {!scanned_candidates} names the few class names that cannot take this route.
    @param minify Whether to minify the output (default: false)
    @param optimize Whether to optimize the output (default: true)
    @param forms
      Whether to include [@tailwindcss/forms] plugin (default: auto-detect)
    @param input_css
      The project's CSS entrypoint content, used verbatim as the Tailwind input
      so its [@theme]/[@plugin]/[@config] apply (default: synthesised import)
    @param classnames List of Tailwind class names
    @return The generated CSS as a string
    @raise Failure if Tailwind CSS generation fails. *)

val scanned_candidates : string list -> string list
(** [scanned_candidates classnames] are the candidates {!generate} writes into a
    scanned file because an [@source inline] string cannot hold them: [{] and
    [}] are its expansion syntax, a backslash opens an escape, and a name
    carrying both quote characters fits inside neither string form. An entry of
    [classnames] holding several whitespace-separated candidates is split first,
    the way both routes split it. These reach the CLI through its source
    extractor, which drops what it cannot read, so a sheet compared over one of
    them may be missing a rule Tailwind would emit. It is empty for every class
    name Tailwind can produce; a caller comparing sheets should name what it
    returns rather than leave the two routes mixed unremarked. *)

val check_tailwindcss_available : unit -> unit
(** [check_tailwindcss_available ()] checks if Tailwind CSS v4 is available.
    @raise Failure if Tailwind CSS is not available or not v4. *)

val tailwindcss_command : unit -> string
(** [tailwindcss_command ()] is the shell command for the pinned Tailwind CLI.
    @raise Failure if Tailwind CSS is unavailable or has the wrong version. *)

val availability : unit -> (unit, string) result
(** [availability ()] is [Ok ()] iff the required tailwindcss CLI is installed
    and could be identified. It is [Error reason] when the CLI is missing,
    answers with another version, or cannot be probed at all, and it never
    raises; [reason] names what each candidate answered and the version wanted,
    so a caller can report which of the three it hit. A CLI that is present and
    then fails to produce CSS is a separate matter: {!generate} raises on that.
*)

val with_stats : (unit -> 'a) -> 'a
(** [with_stats f] runs function [f] and prints Tailwind CSS generation
    statistics afterward. *)

val generate_entrypoint : ?minify:bool -> string -> string
(** [generate_entrypoint ?minify entry] is the CSS the pinned CLI produces from
    the project entrypoint at path [entry], minified by default. The entrypoint
    is read where it sits, so its [@import]s, its [@source] and its [@plugin]s
    resolve the way they would in the project: an entry pinning [source(none)]
    plus an explicit [@source] scans exactly what it names and nothing else,
    which is what keeps a comparison against tw from reading tw's own output
    back in.
    @raise Failure if Tailwind CSS is unavailable or the run fails. *)
