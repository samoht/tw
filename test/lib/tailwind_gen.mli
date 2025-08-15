(** Tailwind CSS generation utilities for testing *)

val generate : ?minify:bool -> string list -> string
(** Generate Tailwind CSS for given class names
    @param minify Whether to minify the output (default: false)
    @param classnames List of Tailwind class names
    @return The generated CSS as a string
    @raise Failure if Tailwind CSS generation fails *)

val check_tailwindcss_available : unit -> unit
(** Check if Tailwind CSS v4 is available
    @raise Failure if Tailwind CSS is not available or not v4 *)
