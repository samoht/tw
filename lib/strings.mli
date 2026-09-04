(** String scanning shared across the library, the dev tools and the command
    line.

    [tw_tools] and the [tw] binary both depend on [tw], so the substring scan
    written here is the only one any of them needs. *)

val index : sub:string -> string -> int option
(** [index ~sub s] is the offset of the first occurrence of [sub] in [s], and
    [None] when [sub] does not occur. The empty pattern occurs at [0]. *)

val contains : sub:string -> string -> bool
(** [contains ~sub s] is [true] when [sub] occurs in [s]. The empty string
    occurs in every string, so [contains ~sub:"" s] is [true] for any [s]. *)

val is_digit : char -> bool
(** [is_digit c] is [true] for the ASCII digits ['0'] to ['9']. *)
