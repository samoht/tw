(** What a stylesheet contains besides its utilities.

    A configuration says which of Tailwind's surrounding layers to emit around
    the utility rules. It says nothing about how the result is rendered:
    minification, variable inlining and optimisation are choices made when the
    stylesheet is turned into text. *)

type t = {
  base : bool;
      (** Emit [@layer base], the Preflight reset and the form element defaults.
      *)
  forms : bool option;
      (** Emit the forms plugin's base styles: [None] emits them when a form
          utility is used, [Some b] forces the answer. *)
  layers : bool;
      (** Wrap the output in [@layer] blocks. When [false] the same rules are
          emitted bare, except [@layer properties], which is always kept because
          [@property] registrations have no unwrapped spelling. *)
}
(** The type for stylesheet configurations. *)

val default : t
(** [default] emits every layer and decides the forms styles from the utilities
    in the stylesheet. *)

val v : ?base:bool -> ?forms:bool -> ?layers:bool -> unit -> t
(** [v ()] is {!default} with the given fields replaced. *)
