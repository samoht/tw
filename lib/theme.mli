(** Shared theme variables for consistent ordering and avoiding conflicts *)

module Css = Cascade.Css

val spacing_var : Css.length Var.theme
(** [spacing_var] is the shared [--spacing] variable used across padding,
    margin, positioning, etc. *)

val spacing_times : float -> string option
(** [spacing_times n] is the spacing step times [n], rendered, or
    {!constructor-None} when the step is in a unit this cannot scale. Tailwind's
    v3 [spacing] and [lineHeight] scales are both that product, and v4 keeps no
    token per step, so [theme(spacing.4)] is computed rather than looked up. *)

val spacing_base : Css.length
(** [spacing_base] is the base spacing value ([0.25rem]). *)

val spacing_n_var : int -> Css.length Var.theme
(** [spacing_n_var n] creates the [--spacing-n] variable for explicit spacing
    values. *)

val has_spacing_step : ?theme:Scheme.t -> float -> bool
(** [has_spacing_step ?theme n] is whether step [n] of the spacing scale still
    resolves. Tailwind reads a bare step off [--spacing-<n>] when the theme
    binds one and off the [--spacing] multiplier otherwise, so
    [\@theme \{ --spacing: initial \}] leaves the multiplied steps with nothing
    to read and they stop being utilities. *)

val spacing_calc : ?theme:Scheme.t -> int -> Css.declaration * Css.length
(** [spacing_calc ?theme n] returns the theme declaration and a length for [n].

    When the scheme ([theme] when given, else the global) has an explicit
    spacing for [|n|], returns [var(--spacing-|n|)] (or
    [calc(var(--spacing-|n|) * -1)] for negatives). Otherwise returns
    [calc(var(--spacing) * n)]. *)

val spacing_calc_float :
  ?theme:Scheme.t -> float -> Css.declaration * Css.length
(** [spacing_calc_float ?theme n] is like {!spacing_calc} but accepts float
    multipliers such as [2.5] for classes like [my-2.5]. *)

val spacing_product : ?theme:Scheme.t -> float -> Css.declaration * Css.length
(** [spacing_product ?theme n] is {!spacing_calc_float} without the folding:
    [calc(var(--spacing) * n)] for every multiplier, the zero and unit ones
    included. A scheme that binds the step outright still wins, as it does for
    {!spacing_calc}. Tailwind's [start-*] and [end-*] utilities resolve the step
    themselves and write the product out in full. *)
