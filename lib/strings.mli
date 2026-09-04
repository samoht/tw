(** String scanning predicates shared across the library and the dev tools.

    Both were hand-rolled in several modules before landing here, and the
    substring test had already drifted on the empty pattern. [tw_tools] depends
    on [tw], so this module is the single answer for both. *)

val contains : sub:string -> string -> bool
(** [contains ~sub s] is [true] when [sub] occurs in [s]. The empty string
    occurs in every string, so [contains ~sub:"" s] is [true] for any [s]. *)

val is_digit : char -> bool
(** [is_digit c] is [true] for the ASCII digits ['0'] to ['9']. *)
