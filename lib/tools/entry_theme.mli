(** The theme an entrypoint asks for.

    Tailwind's [\@theme] blocks are not CSS: cascade keeps each one whole, and
    its declarations, the [--ns-*: initial] namespace resets among them, are
    read from its body. The import's options decide the rest. *)

val of_css : string -> Tw.Scheme.t
(** [of_css css] is the theme the entrypoint [css] asks for: its [\@theme]
    overrides, [theme(static)] and [prefix()]. The CLI builds its theme with
    this, so a measurement against the CLI can build the same one. *)

val overrides_of_css : string -> (string * string) list * string list
(** [overrides_of_css css] is the [(bare-name, value)] pairs the [@theme] blocks
    of [css] declare, together with the names among them that came from an
    [@theme inline] block. Both feed {!Tw.Scheme.with_overrides}, so tw renders
    with the tokens Tailwind reads from the same file. *)

val hoist_keyframes : string -> string
(** [hoist_keyframes css] lifts a [@keyframes] written inside a [@theme] block,
    beside the [--animate-*] token that names it, to the top level. The theme
    block itself does not reach the output, so a nested one would go with it. *)
