(** Parsing helpers for small integers and results.

    This module provides lightweight parsers used across the library for
    interpreting small integers from strings and a convenient infix map on
    results. All functions return [result] with [`Msg] error messages instead of
    raising. *)

val has_prefix : prefix:string -> string -> bool
(** [has_prefix ~prefix s] is [true] when [s] starts with [prefix]. Like
    [String.starts_with] but allocation-free (no per-call closure), for hot
    prefix tests in ordering. *)

val decimal_int : string -> int option
(** [decimal_int s] reads the canonical plain-decimal spelling of an integer.
    OCaml-only forms such as [0x10] and [1_0], and redundant leading zeroes, are
    rejected. *)

val decimal_float : string -> float option
(** [decimal_float s] reads a canonical plain-decimal integer or fraction.
    Exponents, digit separators, redundant leading or trailing zeroes, and
    missing digits around the decimal point are rejected. *)

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
(** [is_bracket_value s] returns [true] if [s] is one bracket-wrapped value. The
    closing bracket has to be the last character, so a suffix carrying a second
    bracket - one bracket with a bracket modifier, or two brackets in a row - is
    not one bracket value. *)

val bracket_inner : string -> string
(** [bracket_inner s] extracts the inner content from ["[foo]"], returning
    ["foo"]. If [s] is not bracket-wrapped, returns [s] unchanged. *)

val decode_underscores : string -> string
(** [decode_underscores s] turns the [_] of an arbitrary value into a space, and
    [\_] into a literal underscore. *)

val decode_arbitrary_value : string -> string
(** [decode_arbitrary_value s] decodes Tailwind arbitrary-value syntax into a
    CSS value string suitable for Cascade readers. This converts underscores to
    spaces and normalizes omitted whitespace around binary [+] and [-] operators
    inside CSS math functions such as [calc()]. *)

val normalize_css_math_operators : string -> string
(** [normalize_css_math_operators s] inserts the spaces CSS math functions
    (calc/min/max/...) require around binary [+] and [-], e.g.
    [calc(var(--a)-var(--b))] becomes [calc(var(--a) - var(--b))]. *)

val arbitrary_length : string -> Cascade.Css.length option
(** [arbitrary_length s] reads the inside of an arbitrary value as a CSS length.
    [s] goes through {!decode_arbitrary_value} first, so underscores, [calc()]
    and [--spacing()] all read; the whole CSS length grammar is accepted, not a
    hand-picked subset of units. Returns [None] when [s] is not a length. *)

val arbitrary_length_percentage : string -> Cascade.Css.length_percentage option
(** [arbitrary_length_percentage s] reads the inside of an arbitrary value as a
    CSS [<length-percentage>]. It is {!arbitrary_length} narrowed to the values
    that spelling admits: the keywords the length reader also accepts ([auto],
    [none], [max-content], ...) and a unitless number are [None], because
    neither is a length-percentage. *)

val is_ident : string -> bool
(** [is_ident s] is [true] when [s] is a CSS identifier, as a custom-ident or a
    property name written in an arbitrary value has to be. *)

val is_var : string -> bool
(** [is_var s] returns [true] if [s] starts with ["var("]. Works on inner
    bracket content (without surrounding brackets). *)

val is_bracket_var : string -> bool
(** [is_bracket_var s] returns [true] if [s] is a bracket-wrapped var()
    reference like ["[var(--value)]"]. *)

val is_css_color_fn : string -> bool
(** [is_css_color_fn s] returns [true] if [s] looks like a CSS color function
    call such as ["rgba(...)"], ["hsl(...)"], or ["oklch(...)"]. Recognizes all
    standard CSS color functions: rgb, rgba, hsl, hsla, hwb, oklch, oklab, lch,
    lab, color, and color-mix. *)

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
