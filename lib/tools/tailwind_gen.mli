(** Tailwind CSS generation utilities for testing *)

val generate :
  ?minify:bool -> ?optimize:bool -> ?forms:bool -> string list -> string
(** [generate ?minify ?optimize ?forms classnames] generates Tailwind CSS for
    given class names.
    @param minify Whether to minify the output (default: false)
    @param optimize Whether to optimize the output (default: true)
    @param forms Whether to include @tailwindcss/forms plugin (default: auto-detect)
    @param classnames List of Tailwind class names
    @return The generated CSS as a string
    @raise Failure if Tailwind CSS generation fails. *)

val check_tailwindcss_available : unit -> unit
(** [check_tailwindcss_available ()] checks if Tailwind CSS v4 is available.
    @raise Failure if Tailwind CSS is not available or not v4. *)

val with_stats : (unit -> 'a) -> 'a
(** [with_stats f] runs function [f] and prints Tailwind CSS generation
    statistics afterward. *)
