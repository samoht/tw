(** The candidates a project's own declarations govern.

    A variant the project redefined with [\@custom-variant] - a class-based
    [dark:], say - and a class it declared with [\@utility] are not what
    {!Tw.of_string} produces. Such a candidate goes through the expansion author
    CSS uses instead: its declarations land under the escaped class, wrapped in
    its variants, and the rules sort among the built-in utilities. *)

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
