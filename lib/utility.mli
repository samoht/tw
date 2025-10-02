(** Utility module for common utility types and functions *)

type base = ..
(** Base utility type without modifiers - extensible variant *)

(** Unified utility type with modifiers support *)
type t = Base of base | Modified of Style.modifier * t | Group of t list

val base : base -> t
(** [base u] wraps a base utility into a Utility.t *)

type 'a handler = {
  to_style : 'a -> Style.t;
  priority : int;
  suborder : 'a -> int;
  of_string : string list -> ('a, [ `Msg of string ]) result;
}
(** Generic handler for a specific utility type 'a *)

val register :
  wrap:('a -> base) -> unwrap:(base -> 'a option) -> 'a handler -> unit
(** [register ~wrap ~unwrap handler] registers a typed utility handler.
    - [wrap] converts from the local type to the extensible variant
    - [unwrap] attempts to extract the local type from the extensible variant
    - [handler] is the typed handler for the local utility type *)

(** Parse CSS string into AST *)
val css_of_string :
  ?filename:string -> string -> (Css.t, Css.parse_error) result
(** [css_of_string ?filename css_str] parses a CSS string into an AST. Returns
    [Ok ast] on success or [Error err] on parse failure. *)

(** Parse a class string into a base utility (without modifiers) *)
val base_of_string : string list -> (base, [ `Msg of string ]) result
(** [base_of_string parts] parses a list of string parts into a base utility.
    For internal use by the Tw module. *)

val base_to_style : base -> Style.t
(** Convert a base utility (without modifiers) to Style.t *)

val to_style : t -> Style.t
(** Convert Utility.t (with modifiers) to Style.t *)

val order : base -> int * int
(** Get the ordering information (priority, suborder) for a base utility *)

val deduplicate : t list -> t list
(** Deduplicate utilities while preserving order (last occurrence wins) *)
