(** Tailwind's brace expansion.

    An [\@source inline("...")] pattern names several candidates at once:
    [{hover:,}bg-red-{500,600}] is four of them and [p-{0..8..4}] three. *)

val expand : string -> string list
(** [expand pattern] is every candidate [pattern] names, in order. The first
    [{...}] is a comma list, nested braces included, or a range
    [{from..to..step}] counting from [from] to [to] inclusive with a step of one
    by default. A pattern with no brace names itself; one with an unbalanced
    brace, or a range with a zero step, names nothing. *)
