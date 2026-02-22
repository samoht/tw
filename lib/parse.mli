(** Parsing helpers for small integers and results.

    This module provides lightweight parsers used across the library for
    interpreting small integers from strings and a convenient infix map on
    results. All functions return [result] with [`Msg] error messages instead of
    raising. *)

val int_pos : name:string -> string -> (int, [> `Msg of string ]) result
(** [int_pos ~name s] parses a non-negative integer from [s]. Returns [Ok n] if
    [s] is a decimal integer >= 0, otherwise [Error (`Msg msg)]. [name] is used
    to produce helpful error messages. *)

val int_bounded :
  name:string ->
  min:int ->
  max:int ->
  string ->
  (int, [> `Msg of string ]) result
(** [int_bounded ~name ~min ~max s] parses a bounded integer from [s]. Returns
    [Ok n] if [n] is within [min..max], otherwise [Error (`Msg msg)]. *)

val int_any : string -> (int, [> `Msg of string ]) result
(** [int_any s] parses any signed integer from [s]. *)

val spacing_value : name:string -> string -> (float, [> `Msg of string ]) result
(** [spacing_value ~name s] parses spacing values, handling both integers and
    decimals like "0.5", "1.5". *)

val is_valid_theme_name : string -> bool
(** [is_valid_theme_name s] returns [true] if [s] is a valid theme variable
    name. Names containing ['/'] are rejected — such values are invalid class
    suffixes, not theme references. *)

val ( >|= ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
(** Infix map over [result]: [r >|= f] maps [Ok x] to [Ok (f x)] and leaves
    [Error e] unchanged. *)

val extract_var_name : string -> string
(** [extract_var_name s] extracts the bare variable name from ["var(--name)"],
    returning ["name"]. If [s] is not a var() reference, returns [s] unchanged.
*)

val is_bracket_value : string -> bool
(** [is_bracket_value s] returns [true] if [s] is a bracket-wrapped value like
    ["[...]"]. *)

val bracket_inner : string -> string
(** [bracket_inner s] extracts the inner content from ["[foo]"], returning
    ["foo"]. If [s] is not bracket-wrapped, returns [s] unchanged. *)

val is_bracket_var : string -> bool
(** [is_bracket_var s] returns [true] if [s] is a bracket-wrapped var()
    reference like ["[var(--value)]"]. *)

val is_bare_var : string -> bool
(** [is_bare_var s] returns [true] if [s] is a bare var reference like
    ["(--name)"]. *)

val bare_var_inner : string -> string
(** [bare_var_inner s] extracts the inner content from ["(--name)"], returning
    ["--name"]. *)

val split_class : string -> string list
(** [split_class class_name] splits a class name on ['-'] but treats ['[...]']
    and ['(...)'] as atomic, so brackets and parentheses containing dashes are
    preserved. E.g. ["m-[var(--value)]"] becomes [["m"; "[var(--value)]"]]. *)
