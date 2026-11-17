(** CSS rule extraction — converts Tailwind utility classes into {!Output.t}
    values.

    This is the bridge between the high-level utility DSL ({!Utility.t}) and the
    low-level CSS output type ({!Output.t}). For every utility class, possibly
    wrapped in modifiers (responsive breakpoints, pseudo-classes, dark mode, …),
    {!outputs} walks the modifier stack and emits the appropriate CSS rule
    variant.

    Typical call chain:
    {v
      Utility.t list
        |> List.concat_map Rule.outputs   (* Rule *)

        |> Build.rule_sets               (* Build *)
    v} *)

open Cascade

(** {1 Rule extraction} *)

val outputs : ?theme:Scheme.t -> Utility.t -> Output.t list
(** [outputs ?theme u] extracts the CSS rules for utility [u], reading any theme
    values it needs from [theme] (default {!Scheme.default}).

    Returns a list because a single utility can produce more than one rule — for
    example a container utility emits one plain rule plus one [@media] rule per
    breakpoint. *)

val ordered_outputs :
  ?theme:Scheme.t -> Utility.t -> ((int * int) * Output.t) list
(** [ordered_outputs ?theme u] is {!outputs} with each rule paired with the
    [(priority, suborder)] of the base utility it was built for:
    {!Utility.order} on that utility, whatever modifiers [u] wraps it in. Each
    item of a group pairs its rules with its own order.

    @raise Invalid_argument
      if a mark wraps a group, as [Utility.Important (false, Group us)] does: a
      rule it writes has no single utility to take its order from. *)

(** {1 Modifier dispatch}

    Lower-level entry point used when constructing rules from an already-parsed
    modifier/selector/props triple. Prefer {!outputs} for normal use. *)

val compute_variant_order :
  ?theme:Scheme.t -> selector:Css.Selector.t -> string option -> int
(** [compute_variant_order ?theme ~selector base_class] computes the sort key
    for modifier-prefixed rules. Returns 0 for plain rules; non-zero for rules
    carrying a [not-*] or variant prefix. [selector] supplies the modifiers that
    [base_class] no longer carries, and [theme] the variants the project
    declared, its breakpoints among them. Used internally by {!Build}. *)

val modifier_to_rule :
  ?inner_has_hover:bool ->
  Style.modifier ->
  string ->
  Css.Selector.t ->
  Css.declaration list ->
  Output.t
(** [modifier_to_rule ?inner_has_hover modifier base_class selector props] wraps
    [selector]/[props] in the CSS construct appropriate for [modifier].
    [inner_has_hover] is [true] when an inner modifier already carries hover
    semantics and the outer wrapper must therefore emit [@media (hover:hover)]
    nesting. *)

(** {1 Class name escaping} *)

val escape_class_name : string -> string
(** [escape_class_name name] CSS-escapes all special characters in [name], e.g.
    [escape_class_name "hover:p-4"] returns ["hover\\:p-4"]. *)
