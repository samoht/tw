(** Tailwind's dialect in author CSS, expanded to CSS.

    [\@apply] pulls a utility's declarations into an author rule, [\@variant]
    and the declared variants wrap them, the value functions resolve against the
    theme, and the directives themselves are dropped, since Tailwind emits none
    of them. A utility is rendered by tw and nested under the [&] of the rule
    that applied it, so its variants survive without being reimplemented. *)

val expand :
  ?extra_defs:(string * string) list ->
  ?udefs:(string * string) list ->
  theme:Tw.Scheme.t ->
  string ->
  string
(** [expand ?extra_defs ?udefs ~theme css] runs the whole expansion over author
    CSS: [@apply] pulls in utilities, [@variant] and the declared variants wrap
    them, [--spacing()], [theme()] and [--theme()] resolve against [theme], and
    the directives themselves are dropped. [extra_defs] adds variant
    declarations from outside [css], [udefs] the [@utility] ones. *)

val drop_directives : string -> string
(** [drop_directives css] removes Tailwind's own at-rules. A build-time
    directive has no meaning in a browser, and Tailwind emits none of them;
    anything else is the author's CSS and passes through. *)

val nest_on_ampersand :
  classes:string list -> Cascade.Selector.t -> Cascade.Selector.t
(** [nest_on_ampersand ~classes sel] swaps the utility's own class in [sel] -
    the one named in [classes] - for the nesting selector [&], so the rule can
    be hosted by whatever selector applied it. A selector naming none of
    [classes] gives up its leftmost class instead. *)

val nested_utilities :
  theme:Tw.Scheme.t -> string list -> string * Cascade.Css.statement list
(** [nested_utilities ~theme names] is the declarations of the utilities [names]
    as text nested under [&], in one rule where they share a selector, and the
    statements that stand at the top of the sheet instead: the theme block they
    read, [\@layer properties], [\@property] and [\@keyframes]. A name tw does
    not know is left out. *)

val add_once :
  Buffer.t ->
  (int, Cascade.Css.statement) Hashtbl.t ->
  Cascade.Css.statement list ->
  unit
(** [add_once buf seen stmts] appends to [buf] each statement of [stmts] that
    {!Statements.seen} has not yet recorded in [seen]. *)

val variant_names_in : string -> string list
(** [variant_names_in css] is the names the [\@variant NAME {] headers of [css]
    use, in source order. *)

val builtin_variant_template : theme:Tw.Scheme.t -> string -> string option
(** [builtin_variant_template ~theme name] is the template the built-in variant
    [name] stands for, derived from tw's own render of a probe utility under it:
    each probe declaration becomes the [@slot]. [None] when tw renders nothing
    for it. *)
