(** Reading one variant token, the part of a class name between two [":"], into
    a typed {!Style.modifier}.

    The reader also decides which compositions name a variant at all: a [not-],
    [group-not-], [has-] or [in-] over an inner that has nothing for it to
    negate or hold reads as no variant, the way Tailwind compiles nothing for
    it. *)

open Cascade

(** {1 Reading} *)

val modifier : theme:Scheme.t -> string -> Style.modifier option
(** [modifier ~theme token] is the variant [token] names, [hover],
    [group-has-[.x]/name] or [\@min-md], reading the custom breakpoints, the
    [\@custom-variant]s and the container variants from [theme]. [None] when
    [token] names no variant. *)

val involves_hover : Style.modifier -> bool
(** [involves_hover m] is whether [m]'s rendering gates on
    [\@media (hover: hover)]: the hover state itself and its group, peer, named,
    ancestor and [has-] forms. *)

val is_custom_variant : Scheme.t -> string -> bool
(** [is_custom_variant theme token] is whether [token] names one of the
    [\@custom-variant]s [theme] registers. *)

val split_name : string -> string * string option
(** [split_name s] splits a [/name] suffix off [s]: ["checked/name"] is
    [("checked", Some "name")]. An empty name leaves [s] whole. *)

(** {1 Arguments} *)

val nth_selector : string -> Css.Selector.nth * Css.Selector.t list option
(** [nth_selector expr] reads an [nth-*] bracket argument, an An+B expression
    with an optional [of S] clause, with underscores read as spaces:
    ["2n+1_of_.foo"] is [2n+1] of [.foo]. Raises [Cascade.Cursor.Parse_error] or
    [Invalid_argument] on anything else; the reader validates the argument
    before a modifier holding it is built. *)

val bracket_media_condition : string -> Css.Media.t option
(** [bracket_media_condition inner] is the media query a bracket at-rule [inner]
    names when it is an [@media] one, [@media_print] or [@media(width>=600px)],
    with underscores read as spaces, and [None] when it is not or the query does
    not parse. *)

val bracket_supports_condition : string -> Css.Supports.t option
(** [bracket_supports_condition content] is the condition a bracket [@supports]
    at-rule names under [not-], [@supports(display:grid)] or
    [@supports_not_(display:grid)], with underscores read as spaces, and [None]
    when it is not one, when it does not parse, or when it is an [and]/[or]
    compound with no single negation. *)

val normalize_supports_condition : string -> Css.Supports.t
(** [normalize_supports_condition cond] builds the [supports-[...]] bracket
    content as a typed CSS [@supports] condition: underscores become spaces, and
    a bare property or custom property expands to a [(prop: var(--tw))] test,
    built directly through {!Css.Supports.property} rather than assembled as a
    string and re-parsed. An already-parenthesised condition or a function call
    is the text the author wrote, so it is parsed with {!Css.Supports.of_string}
    instead. The modifier reader validates the result, so only conditions that
    parse reach a rule. *)

(** {1 Container Queries} *)

val negate_container : Css.Container.t -> Css.Container.t
(** [negate_container c] is [c] negated, a double negation cancelled and the
    negation pushed inside a named container: [name (C)] is [name (not C)]. *)

val container_query_of_token :
  ?theme:Scheme.t -> string -> Style.container_query option
(** [container_query_of_token ?theme token] reads the container query one
    modifier token spells, [\@sm], [\@max-md], [\@min-[theme(--breakpoint-lg)]]
    or [\@lg/main]; [None] for any other token. A size the project's [\@theme]
    declared as [--container-<name>] is read through [theme] (default
    {!Scheme.default}). *)

val container_query_theme_decls : Style.container_query -> Css.declaration list
(** [container_query_theme_decls q] is the theme-layer bindings [q] read through
    [theme()], e.g. [--breakpoint-lg: 64rem] for
    [\@min-[theme(--breakpoint-lg)]]. Resolving the reference into the query
    does not consume the token: Tailwind writes its binding into [\@layer theme]
    as well, so a consumer reading the token off the sheet still finds it. Empty
    for a query naming no token. *)

(** {1 ARIA and Data Attributes} *)

val is_aria_shorthand : string -> bool
(** [is_aria_shorthand name] returns [true] if [name] is a known ARIA shorthand
    attribute (e.g., "checked", "expanded", "hidden") that maps to
    [aria-X=true]. *)

val parse_data_expr :
  string ->
  string * Css.Selector.attribute_match * Css.Selector.attr_flag option
(** [parse_data_expr expr] reads a [data-[expr]] bracket body (the interior
    text, no brackets) into the attribute name (["data-" ^ name]), the match
    operator, and an optional case-sensitivity flag. Handles [$=], [^=], [*=],
    [~=], [|=] and bare [=]; underscores in the attribute name and value stand
    for spaces, per Tailwind's arbitrary-value convention. Reads the decoded
    expression through {!Css.Selector.of_string}, so it raises
    [Invalid_argument] or [Cascade.Cursor.Parse_error] on a malformed
    expression; callers parsing untrusted class strings validate first and only
    call this once that validation has already succeeded. Used both to build the
    selector at render time and, by that earlier validation, to reject a
    malformed expression at parse time. *)
