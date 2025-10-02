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
