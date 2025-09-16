(** Preflight and reset rules *)

val stylesheet : ?placeholder_supports:Css.t -> unit -> Css.t
(** [stylesheet ?placeholder_supports ()] generates Tailwind-like base reset
    rules. If [placeholder_supports] is provided, it will be inserted after the
    ::placeholder rule. Returns a stylesheet that can be directly used or
    wrapped in a layer. *)
