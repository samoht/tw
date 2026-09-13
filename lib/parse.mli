(** Parsing helpers for small integers and results.

    This module provides lightweight parsers used across the library for
    interpreting small integers from strings and a convenient infix map on
    results. All functions return [result] with [`Msg] error messages instead of
    raising. *)

val decimal_int : string -> int option
(** [decimal_int s] reads the canonical plain-decimal spelling of an integer.
    OCaml-only forms such as [0x10] and [1_0], and redundant leading zeroes, are
    rejected. *)

val decimal_float : string -> float option
(** [decimal_float s] reads a canonical plain-decimal integer or fraction.
    Exponents, digit separators, redundant leading or trailing zeroes, and
    missing digits around the decimal point are rejected. *)

val fraction : string -> (int * int) option
(** [fraction s] reads a fraction suffix such as ["1/2"] or ["13/17"] as its
    numerator and denominator. Both are plain decimals with no sign and no
    redundant leading zero; the denominator is drawn from no fixed list, and
    zero is a value on either side. *)

val fraction_percent : int -> int -> float option
(** [fraction_percent n m] is the percentage Tailwind's [calc(n / m * 100%)]
    computes, folded to six significant figures the way Tailwind's printer folds
    it. A zero denominator is [None]: it has no percentage, and Tailwind writes
    the division out instead. *)

val fraction_pct : string -> float option
(** [fraction_pct s] is {!fraction} put through {!fraction_percent}. *)

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
(** [extract_var_name s] parses one complete [var()] reference and returns its
    name without the [--] prefix, retaining an optional fallback after a comma.
    If [s] is not exactly one valid reference, it returns [s] unchanged. *)

val bracket_close : string -> int option
(** [bracket_close s] is the index of the [\]] closing the [\[] that [s] starts
    with, and [None] when [s] does not start with one or leaves it open. Nested
    brackets are matched.

    A [\]] the value quotes or escapes belongs to the value: strings and the
    backslash escape are read as CSS Syntax 3 sec. 4.3 reads them, so
    [[background-image:url('a]b')\]] closes on its last bracket. A string left
    open runs to the end of [s], so nothing after it closes the bracket. *)

val is_bracket_value : string -> bool
(** [is_bracket_value s] returns [true] if [s] is one bracket-wrapped value. The
    closing bracket has to be the last character, so a suffix carrying a second
    bracket - one bracket with a bracket modifier, or two brackets in a row - is
    not one bracket value. It is {!bracket_close} landing on the last character,
    and reads quotes and escapes the same way. *)

val bracket_inner : string -> string
(** [bracket_inner s] extracts the inner content from ["[foo]"], returning
    ["foo"]. If [s] is not bracket-wrapped, returns [s] unchanged. *)

val decode_underscores : string -> string
(** [decode_underscores s] turns the [_] of an arbitrary value into a space, and
    [\_] into a literal underscore.

    The argument list of a [url()] is left as written, because a [_] there is
    part of a file name: [image-set(url('a_b.png')_1x)] keeps the first
    underscore and gives the second a space.

    The first argument of a [var()] or a [theme()] keeps its bare [_] too,
    because it names a custom property, while [\_] there still unescapes. The
    rest of the call reads normally, so [var(--a_b,_c_d)] is [var(--a_b, c d)].

    A name is matched on the word itself or on one ending in [_url], [_var] or
    [_theme], since a class writes a space as [_]: the whole of [0_0_0_var] is
    one function name, and [shadow-[0_0_0_var(--my_var)]] keeps the property it
    references. So [myurl(a_b)] and [a-url(a_b)] decode their arguments. *)

val unescape_underscores : string -> string
(** [unescape_underscores s] turns the [\_] of an arbitrary value into a literal
    underscore and leaves a bare [_] alone. This is the reading a property name
    takes: [[--my\_var:red]] and [[--my_var:red]] both declare [--my_var], where
    {!decode_underscores} would give the first a space. *)

val encode_underscores : string -> string
(** [encode_underscores s] writes a decoded value back into the arbitrary
    spelling a class name carries, turning a space into [_] and an underscore
    into [\_]. It is the inverse of {!decode_underscores}, which is what a
    utility holding its value decoded needs to name itself. *)

val decode_arbitrary_value : string -> string
(** [decode_arbitrary_value s] decodes Tailwind arbitrary-value syntax into a
    CSS value string suitable for Cascade readers. This converts underscores to
    spaces and normalizes omitted whitespace around binary [+] and [-] operators
    inside CSS math functions such as [calc()]. *)

val url_token : string -> string option
(** [url_token s] reads [s] as one whole CSS [url()] token and returns the URL
    it names, with quotes and escapes resolved: [url(a\]b)], [url(a\\\]b)] and
    [url('a\]b')] all name [a\]b]. [None] when [s] is not one whole token.

    A utility holding a [url()] as it was written reads it back through this
    rather than slicing the file name out of the text, which would carry the
    backslash of an escape into the value. *)

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
    property name written in an arbitrary value has to be. Delegates to
    {!Cascade.Syntax.is_ident}, which is the CSS Syntax 3 grammar: a bare [-], a
    [-] before a digit, and a leading digit all open no ident. *)

val is_declaration_value : string -> bool
(** [is_declaration_value s] is [true] when [s] can be the whole value of one
    declaration: no top-level [;], no unmatched closing bracket, and no
    unterminated function, block or string (CSS Syntax 3 (ED) sec. 7.2).

    A bracket value that fails this ends the declaration it is written into, or
    swallows the rest of the rule, so the class carrying it is refused rather
    than emitted. {!Cascade.Css.custom_property} takes the same values and
    raises on the rest. *)

val data_type_hint : string -> (string * string) option
(** [data_type_hint inner] splits a bracket's inner text at a data-type hint:
    [Some (hint, value)] when [inner] opens with a run of [a-z] and [-] closed
    by a [:], and [None] when it does not, so the whole of [inner] is the value.

    The hint chooses which longhand the class lands in and says nothing about
    the value, so a family that does not know the hint still writes what follows
    it into its last resort. Only the leading run is the hint's name:
    [color:red:blue] leaves [red:blue] whole, an upper-case letter or a digit
    ends the run without a hint, so [FOO:5] and [a1:5] are values, and a [:]
    inside a function call belongs to the value, so [url(http://x/a.png)] is a
    URL.

    A [hint] of [""], which is a bracket opening with [:], names no utility at
    all: {!arbitrary_declaration_value} is where that is refused. *)

val value_after_hint : string -> string option
(** [value_after_hint inner] is what a family's own reader sees: [inner] with
    any {!data_type_hint} taken off, and [None] for a bracket whose hint is
    empty, which names no utility.

    A family whose bracket reader is typed calls this before reading, so the
    hint chooses the longhand and the reader is handed only the value. A family
    that falls through to a token stream gets the same peel from
    {!arbitrary_declaration_value}; calling both peels twice. *)

val arbitrary_declaration_value : string -> string option
(** [arbitrary_declaration_value s] decodes the inside of a Tailwind bracket and
    returns its CSS declaration value: the text after any {!data_type_hint},
    which has to hold something other than blank space. Values that can
    terminate or swallow the declaration are [None], as is a bracket whose hint
    is empty. *)

val wrap_declaration_value :
  before:string -> after:string -> string -> string option
(** [wrap_declaration_value ~before ~after value] safely embeds a declaration
    value between generated tokens. In particular, it closes a comment that was
    implicitly closed by the end of [value], so that comment cannot swallow
    [after]. *)

val opaque_declaration : string -> string -> Cascade.Css.declaration option
(** [opaque_declaration property value] preserves one non-empty,
    declaration-safe value verbatim. It implements Tailwind's token-stream
    contract for arbitrary utilities, which deliberately emits some values that
    are invalid for [property]. *)

val starts_with_math_function : string -> bool
(** [starts_with_math_function s] is [true] when [s] opens with a CSS math
    function ([calc], [min], [max], [clamp], ...). A utility whose bracket takes
    either a length or a colour reads this to classify one that starts with a
    function call: a math function stands for the value it computes, so it is on
    the numeric side. *)

val is_var : string -> bool
(** [is_var s] returns [true] if [s] starts with ["var("]. Works on inner
    bracket content (without surrounding brackets). *)

val is_bracket_var : string -> bool
(** [is_bracket_var s] returns [true] if [s] is a bracket-wrapped var()
    reference like ["[var(--value)]"]. *)

val is_css_color_fn : string -> bool
(** [is_css_color_fn s] returns [true] if [s] looks like a CSS color function
    call such as ["rgba(...)"], ["hsl(...)"], or ["oklch(...)"]: the part of [s]
    before its first ['('] names a function
    {!Cascade.Css.Properties.is_color_function} recognises - a colour syntax
    fixes, case-insensitively. *)

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

val split_on_colon : string -> string list
(** [split_on_colon s] splits a variant chain on [':'] but treats ['[...]'] and
    ['(...)'] as atomic, so a colon inside an arbitrary value or a shorthand var
    reference (e.g. ["hover:bg-[color:var(--x)]"]) is not read as a variant
    separator. Always yields (colon count + 1) tokens: e.g. ["hover:focus:p-4"]
    becomes [["hover"; "focus"; "p-4"]], and a string with no unbracketed colon
    becomes a single-element list. *)
