(** Tailwind's CSS dialect, compiled to CSS.

    A project's CSS entrypoint is not plain CSS. It carries Tailwind's own
    at-rules - [@theme], [@custom-variant], [@utility], [@apply], [@variant] -
    and value shorthands - [theme()], [--spacing()] - none of which mean
    anything to a browser. This module reads them, expands them against a
    {!Tw.Scheme.t}, and splices the generated stylesheet in at the
    [@import "tailwindcss"] that asked for it.

    Most of it is text passes over the source. A directive is input for the
    generator, not CSS, so it is read and taken out before the file reaches
    cascade's parser. *)

val read_file : string -> string
(** [read_file path] is the whole contents of [path]. Raises [Sys_error] when
    the file cannot be read. *)

(** {1 The project theme} *)

val imports_static_theme : string -> bool
(** [imports_static_theme css] is [true] when [css] asks for the whole theme
    with [@import "tailwindcss" theme(static)], rather than only the variables
    its utilities read. *)

val theme_overrides_of_css : string -> (string * string) list * string list
(** [theme_overrides_of_css css] is the [(bare-name, value)] pairs the [@theme]
    blocks of [css] declare, together with the names among them that came from
    an [@theme inline] block. Both feed {!Tw.Scheme.with_overrides}, so tw
    renders with the tokens Tailwind reads from the same file. *)

(** {1 Declared variants and utilities} *)

val strip_tailwind_import_options : string -> string
(** [strip_tailwind_import_options css] drops the option functions Tailwind adds
    to [@import] - [theme(static)], [source(none)], [prefix(tw)] - which CSS has
    no grammar for. *)

val take_custom_variants : string -> string * (string * string) list
(** [take_custom_variants css] is [css] without its [@custom-variant]
    declarations, and those declarations as [(name, template)] pairs. A template
    is the variant's body, with [@slot] where the utility goes. *)

val take_custom_utilities : string -> string * (string * string) list
(** [take_custom_utilities css] is [css] without its [@utility] declarations,
    and those declarations as [(name, body)] pairs. Both forms are read: the
    static [@utility NAME], and the functional [@utility NAME-*] whose body
    reads the candidate's own value with [--value()] and its modifier with
    [--modifier()]. A functional declaration keeps the [-*] in its name. *)

val entry_variant_defs : string option -> (string * string) list
(** [entry_variant_defs path] is the [@custom-variant] declarations of the
    entrypoint at [path], or none when there is no entrypoint or it cannot be
    read. *)

val entry_utility_defs : string option -> (string * string) list
(** [entry_utility_defs path] is the [@utility] declarations of the entrypoint
    at [path], the same way. *)

val variant_segments : string -> string list
(** [variant_segments name] splits a class name on its variant separators. A [:]
    inside [[&>*]] or [(--x)] belongs to the segment, not between two. *)

val split_declared_variants :
  (string * string) list -> string -> string list * string
(** [split_declared_variants defs name] separates the variants [defs] declares
    from the rest of [name]. A declared variant is picked out wherever it sits
    in the chain, and the built-in prefixes stay attached to the utility so
    their media queries keep wrapping the declared variant's selector. *)

val is_custom_routed :
  defs:(string * string) list -> udefs:(string * string) list -> string -> bool
(** [is_custom_routed ~defs ~udefs cls] is [true] when [cls] names a variant or
    a utility the project declared - a functional one included, whose root [cls]
    carries a value for - which {!Tw.of_string} cannot produce and
    {!custom_routed_utilities} generates instead. *)

val custom_routed_utilities :
  theme:Tw.Scheme.t ->
  defs:(string * string) list ->
  udefs:(string * string) list ->
  string list ->
  int
  * (string * (int * int) * Cascade.Css.statement list) list
  * Cascade.Css.statement list
(** [custom_routed_utilities ~theme ~defs ~udefs candidates] generates the
    classes {!is_custom_routed} claimed: how many it produced, the rules to hand
    {!Tw.to_css} as [~extra] so they sort among the built-in utilities, and the
    statements that belong beside the utilities layer rather than in it. *)

val place_routed : Cascade.Css.statement list -> Cascade.Css.t -> Cascade.Css.t
(** [place_routed stmts sheet] puts the statements {!custom_routed_utilities}
    left beside the utilities layer around [sheet]: the [@layer properties]
    fallback block a declared utility hoists leads, where the generated sheet
    puts its own, and the rest follows. *)

(** {1 Text passes} *)

val hoist_theme_keyframes : string -> string
(** [hoist_theme_keyframes css] lifts a [@keyframes] written inside a [@theme]
    block, beside the [--animate-*] token that names it, to the top level. The
    theme block itself does not reach the output, so a nested one would go with
    it. *)

val drop_directives : string -> string
(** [drop_directives css] removes Tailwind's own at-rules. A build-time
    directive has no meaning in a browser, and Tailwind emits none of them;
    anything else is the author's CSS and passes through. *)

val fill_slots : string -> string -> string
(** [fill_slots template body] is [template] with each [@slot] replaced by
    [body], which is how a [@custom-variant] wraps the utility it decorates. *)

val apply_variants :
  ?extra_defs:(string * string) list ->
  ?udefs:(string * string) list ->
  theme:Tw.Scheme.t ->
  string ->
  string
(** [apply_variants ?extra_defs ?udefs ~theme css] runs the whole expansion over
    author CSS: [@apply] pulls in utilities, [@variant] and the declared
    variants wrap them, [--spacing()] and [theme()] resolve against [theme], and
    the directives themselves are dropped. [extra_defs] adds variant
    declarations from outside [css], [udefs] the [@utility] ones. *)

val nest_on_ampersand :
  classes:string list -> Cascade.Selector.t -> Cascade.Selector.t
(** [nest_on_ampersand ~classes sel] swaps the utility's own class in [sel] -
    the one named in [classes] - for the nesting selector [&], so the rule can
    be hosted by whatever selector applied it. A selector naming none of
    [classes] gives up its leftmost class instead. *)

(** {1 Splicing} *)

val splice_into_entrypoint :
  theme:Tw.Scheme.t -> path:string -> Cascade.Css.t -> Cascade.Css.t
(** [splice_into_entrypoint ~theme ~path generated] compiles the entrypoint at
    [path] and puts [generated] where its [@import "tailwindcss"] is. The
    entrypoint's own rules, its relative [@import]s and its directives are all
    part of the result; when [path] cannot be read, [generated] is. *)
