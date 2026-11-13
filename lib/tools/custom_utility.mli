(** A project's own [\@utility] declarations.

    [\@utility NAME { ... }] declares a utility class.
    [\@utility NAME-* { ... }] declares a functional one, a template rather than
    a rule: its body reads the candidate's value back with [--value(...)] and
    the [/half] after it with [--modifier(...)], each argument a quoted literal,
    a bare data type, a [[data-type]] for an arbitrary value, a [--namespace] to
    look up in the theme, or [--default(...)] for a candidate that spelled no
    value. This is Tailwind's [createCssUtility]. *)

val take : string -> string * (string * string) list
(** [take css] is [css] without its [@utility] declarations, and those
    declarations as [(name, body)] pairs. Both forms are read; a functional
    declaration keeps the [-*] in its name. *)

val functional_body :
  theme:Tw.Scheme.t -> udefs:(string * string) list -> string -> string option
(** [functional_body ~theme ~udefs cls] is the body the functional declarations
    of [udefs] give [cls]: every declaration whose root [cls] names, its reads
    resolved against [cls] and [theme], in the order they were written. A
    declaration whose reads do not all resolve is dropped, and [cls] is no
    utility of a declaration when it resolves no [--value(...)] at all. [None]
    when no declaration takes [cls]. *)

val is_functional_candidate : (string * string) list -> string -> bool
(** [is_functional_candidate udefs cls] is [true] when [cls] parses as a
    candidate of a functional declaration of [udefs], whether or not its reads
    then resolve. *)
