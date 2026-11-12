(** The order Tailwind v4 gives variants in the cascade.

    Every function below reads one table of cascade positions, so none can put a
    variant in one place as a [not-*] inner and another place as a class-name
    token. Each returns a key on the same scale, comparable with the others, and
    [0] for something that names no variant, which {!Sort.compare_indexed_rules}
    reads as "this rule carries no variant". *)

open Cascade

val of_prefix : ?theme:Scheme.t -> string -> int
(** [of_prefix ?theme prefix] returns the position of a modifier prefix string
    in the Tailwind v4 variant cascade. The prefix is one modifier of a class
    name, the part between two ":" (e.g. ["hover"], ["group-has-checked"],
    ["@min-[64rem]"]). When [theme] is supplied, its exact custom-variant names
    take precedence over the built-in prefix grammar; re-registering ["dark"]
    retains its pre-existing built-in position. Returns 0 for a token that names
    no variant. *)

val inner_order : string -> int
(** [inner_order token] returns what separates [token] from another token in the
    same cascade position: the state a [group-]/[peer-] or [in-] token names on
    another element, and the variant a [not-] token negates, all on the scale
    {!of_prefix} returns. [0] for every other token. *)

val inner_token : string -> string option
(** [inner_token token] is the variant immediately wrapped by a [group-],
    [peer-], [not-], [in-], or [has-] compound token. *)

val inner_order_path : ?theme:Scheme.t -> string -> int list
(** [inner_order_path ?theme token] recursively returns every wrapped variant
    position, outermost first. For example, [group-has-focus] yields the [has-]
    and [focus] positions. [theme] resolves wrapped custom variants. *)

val of_media_cond : Css.Media.t -> int
(** [of_media_cond cond] returns the same sort key as {!of_prefix} for the
    corresponding CSS media condition. Used to derive the cascade position of
    rules whose ordering comes from a nested media query rather than from the
    class-name prefix alone. *)
