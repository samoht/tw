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
