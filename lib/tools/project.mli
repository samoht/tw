(** The stylesheet [tw] generates for a whole project.

    One function, so the CLI and anything measuring the CLI against Tailwind
    compile a project the same way rather than two ways that drift. *)

val utilities :
  theme:Tw.Scheme.t ->
  ?entrypoint:string ->
  base:bool ->
  string list ->
  int * Cascade.Css.t
(** [utilities ~theme ?entrypoint ~base classes] is the generated sheet for
    [classes] alone, with how many of them produced a rule: the built-in
    utilities and the classes the entrypoint's own [@utility] and
    [@custom-variant] declarations route. Nothing else the entrypoint holds
    reaches it, neither its CSS nor its [@source inline] safelist. The base
    layer is included when [base]. *)

val stylesheet :
  theme:Tw.Scheme.t ->
  ?entrypoint:string ->
  base:bool ->
  string list ->
  int * Cascade.Css.t
(** [stylesheet ~theme ?entrypoint ~base classes] is the sheet for a project
    whose markup carries [classes], with how many of them produced a rule: the
    built-in utilities, the classes the entrypoint's own [@utility] and
    [@custom-variant] declarations route, and the entrypoint at path
    [entrypoint] all of it is spliced into. The base layer is included when
    [base]. A class no handler reads is left out rather than raising. *)
