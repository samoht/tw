(** Preflight and reset rules *)

open Cascade

val stylesheet :
  ?theme:Scheme.t -> ?placeholder_supports:Css.t -> ?forms:bool -> unit -> Css.t
(** [stylesheet ?theme ?placeholder_supports ?forms ()] generates Tailwind-like
    base reset rules. The [--default-*] font tokens are read through [theme]
    (default {!Scheme.default}): a reference carrying its fallback while the
    theme holds the token, the fallback itself once a [\@theme] block took it
    away. If [placeholder_supports] is provided, it will be inserted after the
    ::placeholder rule. If [forms] is [true], omits webkit datetime rules that
    the forms plugin provides (display:inline-flex and fields-wrapper padding)
    to avoid duplicates. Returns a stylesheet that can be directly used or
    wrapped in a layer. *)
