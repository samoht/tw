(** Spacing values shared by padding, margin and gap. *)

open Cascade

(** {1 Spacing Variable} *)

val var : Css.length Var.theme
(** [var] is the [--spacing] theme variable. *)

(** {1 Class Name Formatting} *)

val pp_spacing_suffix : Style.spacing -> string
(** [pp_spacing_suffix s] is the class-name suffix of [s]. *)

val pp_margin_suffix : Style.margin -> string
(** [pp_margin_suffix m] is the class-name suffix of [m]: ["auto"] for [`Auto],
    and {!pp_spacing_suffix} for every spacing value. *)

(** {1 Spacing Constructors} *)

val int : int -> Style.spacing
(** [int n] is [`Rem (0.25 *. float_of_int n)], step [n] of Tailwind's 0.25rem
    scale: [int 1] is [`Rem 0.25] and [int 4] is [`Rem 1.0]. *)

val float : float -> Style.spacing
(** [float n] is like {!int} but takes a fractional scale factor, for the
    half-step classes (e.g. [p-0.5]): [float 0.5] = [`Rem 0.125]. *)

(** {1 Shared Parsing Logic} *)

val named_spacing_ref : string -> Css.length
(** [named_spacing_ref name] is the reference [var(--spacing-<name>)] to a named
    spacing value: ["big"] gives [var(--spacing-big)]. *)

val to_decl_len :
  ?theme:Scheme.t ->
  ?negative:bool ->
  Style.spacing ->
  Css.declaration option * Css.length
(** [to_decl_len ?theme ?negative s] is the declaration [s] needs and the length
    that reads it. Padding, margin and gap share it: each emits
    [var(--spacing-<name>)] for a named value, and the theme binding that
    declares the variable has to travel with the reference. [negative] flips the
    sign, for the margin utilities that allow it. *)

val is_named_spacing : string -> bool
(** [is_named_spacing value] is [true] iff [value] can name a spacing token: a
    lowercase letter followed by ASCII letters, digits and [-], other than
    ["px"], ["full"] and ["auto"]. *)

type axis = [ `All | `X | `Y | `T | `R | `B | `L | `S | `E | `Bs | `Be ]
(** The type for spacing axes: all, x, y, top, right, bottom, left,
    inline-start, inline-end, block-start and block-end. *)

val parse_value_string :
  ?theme:Scheme.t -> allow_auto:bool -> string -> Style.margin option
(** [parse_value_string ~theme ~allow_auto value] is the spacing [value] names:
    ["px"], ["full"], ["auto"] when [allow_auto] is [true], a step of the
    spacing scale, or a named token. A name is accepted only when [theme]
    defines [--spacing-<name>], so a stray source token ([mx-big]) does not
    parse as a utility. This is [None] for anything else. *)

val axis_of_prefix : string -> axis option
(** [axis_of_prefix prefix] is the axis of the padding or margin class prefix
    [prefix]: [`All] for ["p"] and ["m"], [`X] for ["px"] and ["mx"], [`T] for
    ["pt"] and ["mt"], and so on. This is [None] if [prefix] is not one. *)

val is_margin_prefix : string -> bool
(** [is_margin_prefix prefix] is [true] iff [prefix] is a margin class prefix
    (["m"], ["mx"], ...) rather than a padding one. *)

val parse_class_parts : string list -> (bool * string * string) option
(** [parse_class_parts parts] is [Some (negative, prefix, value)] for the parts
    of a class split on ['-']: [["p"; "4"]] gives [Some (false, "p", "4")] and
    [[""; "m"; "4"]] gives [Some (true, "m", "4")]. This is [None] for any other
    shape. *)
