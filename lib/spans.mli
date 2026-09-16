(** Probe spans over the stages of {!Build.to_css}, for [obs run]. *)

val span : string -> unit Probe.span
(** [span name] declares the payload-free span [name]. *)

val with_ : unit Probe.span -> (unit -> 'a) -> 'a
(** [with_ span f] runs [f] inside [span]. *)

val outputs : unit Probe.span
val sort : unit Probe.span
val layers : unit Probe.span
val theme_layer : unit Probe.span
val base_layer : unit Probe.span
val theme_extract : unit Probe.span
val theme_static : unit Probe.span
val theme_finish : unit Probe.span
