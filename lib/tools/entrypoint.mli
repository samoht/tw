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

(** {1 Declared variants and utilities} *)

val entry_variant_defs : string option -> (string * string) list
(** [entry_variant_defs path] is the [@custom-variant] declarations of the
    entrypoint at [path], or none when there is no entrypoint or it cannot be
    read. *)

val entry_utility_defs : string option -> (string * string) list
(** [entry_utility_defs path] is the [@utility] declarations of the entrypoint
    at [path], the same way. *)

val is_custom_routed :
  theme:Tw.Scheme.t ->
  defs:(string * string) list ->
  udefs:(string * string) list ->
  string ->
  bool
(** [is_custom_routed ~theme ~defs ~udefs cls] is [true] when [cls] names a
    variant or a utility the project declared - a functional one included, whose
    value reads resolve for [cls] - which {!Tw.of_string} cannot produce and
    {!custom_routed_utilities} generates instead. A candidate a functional
    declaration declines falls to the built-in utility of its root, as it does
    in Tailwind. *)

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

(** {1 Splicing} *)

val splice_into_entrypoint :
  theme:Tw.Scheme.t -> path:string -> Cascade.Css.t -> Cascade.Css.t
(** [splice_into_entrypoint ~theme ~path generated] compiles the entrypoint at
    [path] and puts [generated] where its [@import "tailwindcss"] is. The
    entrypoint's own rules, its relative [@import]s and its directives are all
    part of the result; when [path] cannot be read, [generated] is. *)
