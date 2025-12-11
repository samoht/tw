(** Preflight and reset rules *)

val stylesheet : ?placeholder_supports:Css.t -> ?forms:bool -> unit -> Css.t
(** [stylesheet ?placeholder_supports ?forms ()] generates Tailwind-like base
    reset rules. If [placeholder_supports] is provided, it will be inserted
    after the ::placeholder rule. If [forms] is [true], omits webkit datetime
    rules that the forms plugin provides (display:inline-flex and fields-wrapper
    padding) to avoid duplicates. Returns a stylesheet that can be directly used
    or wrapped in a layer. *)
