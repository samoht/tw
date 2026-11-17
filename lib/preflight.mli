(** Preflight, Tailwind's base reset rules. *)

open Cascade

val stylesheet :
  ?theme:Scheme.t -> ?placeholder_supports:Css.t -> ?forms:bool -> unit -> Css.t
(** [stylesheet ~theme ~placeholder_supports ~forms ()] is Tailwind's base
    reset. The [--default-*] font tokens are read through [theme] (default
    {!Scheme.default}): a reference carrying its fallback while the theme holds
    the token, the fallback itself once a [\@theme] block took it away.
    [placeholder_supports], when given, goes after the [::placeholder] rule.
    [forms] has no effect: the forms plugin with [strategy: 'class'] leaves the
    WebKit datetime rules to preflight, so they are always written. *)
