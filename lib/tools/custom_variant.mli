(** A project's own [\@custom-variant] declarations, and the [\@variant] blocks
    that apply them.

    [\@custom-variant NAME { ... @slot; ... }] and its selector shorthand
    [\@custom-variant NAME (...);] declare a variant as a template, and
    [\@variant NAME { decls }] applies one inside author CSS. Neither is CSS, so
    a parser would drop them and the declarations they guard with them. *)

val builtins : (string * string) list
(** [builtins] is the variant templates known without a declaration: the
    built-in [dark]. *)

val take : string -> string * (string * string) list
(** [take css] is [css] without its [@custom-variant] declarations, and those
    declarations as [(name, template)] pairs. A template is the variant's body,
    with [@slot] where the utility goes. *)

val fill_slots : string -> string -> string
(** [fill_slots template body] is [template] with each [@slot] replaced by
    [body], which is how a [@custom-variant] wraps the utility it decorates. *)

val expand : (string * string) list -> string -> string
(** [expand defs css] is [css] with each [\@variant NAME { decls }] whose [NAME]
    [defs] declares replaced by its template, the declarations at each [@slot].
    Nested blocks expand outermost-first, eight levels deep at most. *)

val segments : string -> string list
(** [segments name] splits a class name on its variant separators. A [:] inside
    [[&>*]] or [(--x)] belongs to the segment, not between two. *)

val split_declared : (string * string) list -> string -> string list * string
(** [split_declared defs name] separates the variants [defs] declares from the
    rest of [name]. A declared variant is picked out wherever it sits in the
    chain, and the built-in prefixes stay attached to the utility so their media
    queries keep wrapping the declared variant's selector. *)
