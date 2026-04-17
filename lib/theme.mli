(** Shared theme variables for consistent ordering and avoiding conflicts *)

module Css = Cascade.Css

val set_scheme : Scheme.t -> unit
(** [set_scheme s] sets the current scheme used by spacing calculations. *)

val spacing_var : Css.length Var.theme
(** [spacing_var] is the shared [--spacing] variable used across padding,
    margin, positioning, etc. *)

val spacing_base : Css.length
(** [spacing_base] is the base spacing value ([0.25rem]). *)

val spacing_n_var : int -> Css.length Var.theme
(** [spacing_n_var n] creates the [--spacing-n] variable for explicit spacing
    values. *)

val spacing_calc : int -> Css.declaration * Css.length
(** [spacing_calc n] returns the theme declaration and a length for [n].

    When the current scheme has an explicit spacing for [|n|], returns
    [var(--spacing-|n|)] (or [calc(var(--spacing-|n|) * -1)] for negatives).
    Otherwise returns [calc(var(--spacing) * n)]. *)

val spacing_calc_float : float -> Css.declaration * Css.length
(** [spacing_calc_float n] is like {!spacing_calc} but accepts float multipliers
    such as [2.5] for classes like [my-2.5]. *)
