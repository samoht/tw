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

(** {1 Scheme}

    The scheme determines whether responsive breakpoints are expressed as
    [rem]-based media queries (default) or [px]-based ones (when a custom scheme
    with pixel breakpoints is active). *)

val set_scheme : Scheme.t -> unit
(** [set_scheme s] installs [s] as the active breakpoint scheme. Call once at
    application start if the default rem breakpoints are not appropriate. *)

(** {1 Rule extraction} *)

val outputs : Utility.t -> Output.t list
(** [outputs u] extracts the CSS rules for utility [u].

    Returns a list because a single utility can produce more than one rule — for
    example a container utility emits one plain rule plus one [@media] rule per
    breakpoint. *)

(** {1 Modifier dispatch}

    Lower-level entry point used when constructing rules from an already-parsed
    modifier/selector/props triple. Prefer {!outputs} for normal use. *)

val compute_variant_order : string option -> Css.Selector.t -> int
(** [compute_variant_order base_class selector] computes the sort key for
    modifier-prefixed rules. Returns 0 for plain rules; non-zero for rules
    carrying a [not-*] or variant prefix. Used internally by {!Build}. *)

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
