(** The CSS data types a candidate's value reads as.

    A functional [\@utility]'s [--value(number)] or [--value([length])] takes
    the candidate's value only when it reads as that type, the way Tailwind's
    [inferDataType] decides it. *)

val segment : char -> string -> string list
(** [segment sep s] is Tailwind's [segment]: [s] split on [sep] at the top
    level, so a separator inside a bracket group or a quoted string belongs to
    the piece around it. *)

val is_digit : char -> bool
(** [is_digit c] is [true] when [c] is an ASCII digit. *)

val is_positive_integer : string -> bool
(** [is_positive_integer text] is [true] when [text] spells a whole number the
    way a round trip through a number leaves it: no sign, no redundant leading
    zero. *)

val is_spacing_multiplier : string -> bool
(** [is_spacing_multiplier text] is [true] when [text] is Tailwind's spacing
    multiplier: a canonical non-negative number that is a whole multiple of
    [0.25]. *)

val infer : string -> string -> bool
(** [infer text kind] is [true] when [text] reads as the CSS data type [kind]:
    [color], [length], [percentage], [ratio], [number] or [integer]. A math
    function reads as a length, a percentage, a ratio or a number, and a [var()]
    as nothing; any other [kind] reads as nothing rather than as a guess. *)

val bare_kinds : string list
(** [bare_kinds] is the data types a bare candidate value is read as: [number],
    [integer], [ratio] and [percentage]. No [--value(color)] can turn
    [example-red] into a utility. *)
