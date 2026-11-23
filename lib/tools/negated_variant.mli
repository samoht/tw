(** The [not-] form of a project's declared variants.

    Tailwind's [not-] negates what a variant's body builds, not the variant's
    name, so a project that declares [dark] itself gets the negation of its own
    body: a style rule goes under [:not()] with [&] read as [*], and a condition
    takes a [not]. *)

val add : (string * string) list -> (string * string) list * string list
(** [add defs] is [defs] with a [not-NAME] template for every declared [NAME]
    whose body Tailwind negates, and the [not-NAME]s it refuses. Tailwind
    negates a body in place, one node at a time, so a body of sibling branches
    is refused, as are a pseudo-element, a compound condition and an at-rule
    other than [\@media], [\@supports] and [\@container]. A [not-NAME] the
    project declared itself is left as declared. *)
