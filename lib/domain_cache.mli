(** Lock-free caches whose mutable tables are local to each OCaml Domain. *)

type ('key, 'value) t
(** A cache key that can be shared between Domains. Each Domain observes its own
    mutable table, so lookups need no process-wide mutex. *)

val v : int -> ('key, 'value) t
(** [v size] creates a cache whose table in each Domain starts with capacity
    [size]. No table is allocated for a Domain until that Domain first uses the
    cache. *)

val or_add : ('key, 'value) t -> 'key -> (unit -> 'value) -> 'value
(** [or_add cache key make] returns the value already cached for [key] in the
    calling Domain. If none exists, it evaluates [make] and caches the result.
    An exception raised by [make] is propagated and is not cached. *)
