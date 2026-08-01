(** Tailwind's own CSS for each theme token.

    A token's default is the same for every stylesheet, so it lives in one
    process-wide table rather than in each {!Theme.t}: the family that owns a
    token publishes its default at module-init, and a theme that overrides the
    token shadows it. [theme(static)] emits every published default rather than
    only the tokens a utility read. *)

val register : string -> string -> unit
(** [register name css] publishes [css] as the default of the theme token
    [name], written without its leading [--]. A second registration of [name]
    replaces the first. *)

val find : string -> string option
(** [find name] is the published default of [name], and [None] when no family
    owns it. *)

val all : unit -> (string * string) list
(** [all ()] is every published [(name, css)] pair, in no particular order. *)
