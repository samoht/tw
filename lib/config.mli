(** What a stylesheet contains besides its utilities.

    A configuration says which of Tailwind's surrounding layers to emit around
    the utility rules. It says nothing about how the result is rendered:
    minification, variable inlining and optimisation are choices made when the
    stylesheet is turned into text. *)

type t
(** The type for stylesheet configurations. *)

val default : t
(** [default] emits every layer and decides the forms styles from the utilities
    in the stylesheet. *)

val v : ?base:bool -> ?forms:bool -> ?layers:bool -> unit -> t
(** [v ()] is {!default} with the given fields replaced. [base] emits
    [@layer base], the Preflight reset and the form element defaults. [forms]
    forces the forms plugin's base styles in or out, where the default decides
    from the utilities in the stylesheet. [layers] wraps the output in [@layer]
    blocks; when [false] the same rules are emitted bare, except
    [@layer properties], which is always kept because [@property] registrations
    have no unwrapped spelling. *)

val base : t -> bool
(** [base c] is whether [c] emits [@layer base]. *)

val forms : t -> bool option
(** [forms c] is [c]'s answer on the forms plugin's base styles, and [None] when
    it leaves the answer to the utilities in the stylesheet. *)

val layers : t -> bool
(** [layers c] is whether [c] wraps the output in [@layer] blocks. *)

val to_string : t -> string
(** [to_string c] is a one-line summary of [c], for diagnostics. *)
