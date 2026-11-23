(** Tailwind's value functions in author CSS.

    [--spacing(N)], [--alpha(colour / alpha)], [theme(--token)] and
    [--theme(--token)] are not CSS: a parser rejects the declaration that holds
    one, and the rule loses it. Each is written out as the CSS it stands for, in
    the author's text. *)

val expand_spacing : theme:Tw.Scheme.t -> string -> string
(** [expand_spacing ~theme css] writes each [--spacing(N)] of [css] as
    [calc(var(--spacing) * N)], or as the length it works out to when [theme]
    declares [--spacing] in an [\@theme inline] block. *)

val expand_alpha : string -> string
(** [expand_alpha css] writes each [--alpha(colour / alpha)] of [css] as the
    [color-mix(in oklab, colour alpha, transparent)] it is shorthand for. *)

val resolve_theme : theme:Tw.Scheme.t -> string -> string
(** [resolve_theme ~theme css] writes each [theme(--token)] of [css], or its v3
    dotted path [theme(fontSize.sm)], as the value [theme] gives the token. An
    unknown token is left as written. *)

val resolve_dashed_theme : theme:Tw.Scheme.t -> string -> string
(** [resolve_dashed_theme ~theme css] writes each [--theme(--token, fallback)]
    of [css] as a reference to the token, its fallback threaded in; as the
    token's value where the call says [inline] or stands in an at-rule prelude;
    and as the fallback alone when [theme] has no such token. A call naming no
    token is left as written. *)

val theme_token_value : Tw.Scheme.t -> string -> string option
(** [theme_token_value theme name] is the value [theme] gives the token [name],
    the palette's colours included. *)
