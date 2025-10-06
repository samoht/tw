(** Shared spacing utilities for padding, margin, and gap *)

(** {1 Spacing Variable} *)

val spacing_var : Css.length Var.theme
(** Shared spacing variable from theme *)

(** {1 Class Name Formatting} *)

val pp_spacing_suffix : Style.spacing -> string
(** Format a spacing value to a class name suffix.

    Examples:
    - [`Px] becomes ["px"]
    - [`Full] becomes ["full"]
    - [`Rem 1.0] becomes ["4"] (because 1.0 / 0.25 = 4)
    - [`Rem 0.375] becomes ["1.5"] (because 0.375 / 0.25 = 1.5) *)

val pp_margin_suffix : Style.margin -> string
(** Format a margin value to a class name suffix.

    Extends {!pp_spacing_suffix} with auto support:
    - [`Auto] becomes ["auto"]
    - All spacing values use {!pp_spacing_suffix} *)

(** {1 CSS Conversion} *)

val to_length : Css.length Css.var -> Style.spacing -> Css.length
(** Convert a spacing value to a CSS length using the spacing variable
    reference.

    @param spacing_ref
      Reference to the spacing theme variable (from Var.binding)
    @param spacing The spacing value to convert
    @return CSS length with proper calc() expression for `Rem values *)

val margin_to_length : Css.length Css.var -> Style.margin -> Css.length
(** Convert a margin value to a CSS length using the spacing variable reference.

    Extends {!to_length} with auto support:
    - [`Auto] becomes [Css.Auto]
    - All spacing values use {!to_length}

    @param spacing_ref
      Reference to the spacing theme variable (from Var.binding)
    @param margin The margin value to convert
    @return CSS length (or Auto) *)

val margin_to_length_neg : Css.length Css.var -> Style.spacing -> Css.length
(** Convert a spacing value to a negated CSS length for negative margins.

    Similar to {!to_length} but negates the value:
    - [`Px] becomes [Css.Px (-1.)]
    - [`Full] becomes [Css.Pct (-100.0)]
    - [`Rem f] creates [calc(var(--spacing) * -n)] where n = f / 0.25

    @param spacing_ref
      Reference to the spacing theme variable (from Var.binding)
    @param spacing The spacing value to convert (will be negated)
    @return CSS length with negated value *)

(** {1 Spacing Constructors} *)

val int : int -> Style.spacing
(** Convert an integer to a spacing value in rem units.

    Uses Tailwind's standard 0.25rem scale:
    - [int 0] = [`Rem 0.0]
    - [int 1] = [`Rem 0.25] (equivalent to 0.25rem)
    - [int 4] = [`Rem 1.0] (equivalent to 1rem)
    - etc.

    @param n The scale factor (will be multiplied by 0.25 to get rem value)
    @return A spacing value in rem units *)

(** {1 Shared Parsing Logic} *)

type axis = [ `All | `X | `Y | `T | `R | `B | `L ]
(** Axis for spacing utilities (all, x, y, top, right, bottom, left) *)

val parse_value_string : allow_auto:bool -> string -> Style.margin option
(** Parse a spacing value string (px, full, auto, or numeric).

    @param allow_auto Whether to accept "auto" as a valid value
    @param value The string to parse ("px", "full", "auto", or a number)
    @return Parsed margin value or None if invalid *)

val axis_of_prefix : string -> axis option
(** Map a class prefix to its axis.

    Examples:
    - "p" or "m" -> [`All]
    - "px" or "mx" -> [`X]
    - "pt" or "mt" -> [`T]

    @param prefix The class prefix (e.g., "p", "px", "m", "ml")
    @return The corresponding axis, or None if invalid *)

val is_margin_prefix : string -> bool
(** Check if a prefix is for margin (vs padding).

    @param prefix The class prefix
    @return [true] if it's a margin prefix ("m", "mx", etc.), [false] otherwise
*)

val parse_class_parts : string list -> (bool * string * string) option
(** Parse class name parts into (is_negative, prefix, value).

    Handles both positive and negative values:
    - [["p"; "4"]] -> [Some (false, "p", "4")]
    - [[""; "m"; "4"]] -> [Some (true, "m", "4")] (negative margin)
    - Other patterns -> [None]

    @param parts List of strings from [String.split_on_char '-']
    @return [(is_negative, prefix, value)] tuple or None if invalid *)
