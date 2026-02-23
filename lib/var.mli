(** Typed CSS variable definitions and CSS generation architecture.

    This module provides a type-safe system for CSS custom properties that
    generates CSS across multiple layers following Tailwind v4's architecture.

    {1 CSS Output Architecture}

    The variable system generates CSS across multiple layers following Tailwind
    v4's architecture. Each layer has its own deterministic ordering rules:

    {b Layer Ordering Rules (Tailwind v4, mirrored by tw):}

    - {b Theme layer}: Order is stable and intentional, not raw alphabetical.
      Tokens appear in a canonical sequence (e.g., default font families, then
      color palette in palette order, then scales like radius/shadow/spacing,
      etc.). In this repo, theme order is explicitly sorted by metadata
      (priority, subindex) attached to each variable; lower priority first, then
      subindex.

    - {b Properties layer}: Split into two parts for parity with Tailwind's
      output shape:
    - Properties layer (near top): Emits initial values for custom properties
      inside a guarded {i \@supports} block (applies to "*, ::before, ::after,
      ::backdrop"). Ordering follows the order of corresponding {i \@property}
      rules.
    - {i \@property} rules (at end): Appended at the very end of stylesheet
      (after all layers), in first-seen order: explicit rules from utilities
      first, then auto-generated ones for variables flagged as "needs
      {i \@property}".

    - {b Utilities layer}: Ordered by "conflict resolution" groups to ensure
      predictable cascade. Utilities are grouped and sorted by group priority,
      then by per-group suborder. Examples: display → position → margin →
      background → padding → typography → border → sizing → effects →
      interactivity → flexbox/grid → gap → container/prose. Within background
      colors, sort uses canonical palette order; for margins/padding, "all"
      precedes axis which precedes side-specific.

    {2 [\@layer properties]}
    Contains initial values for utility variables that need [\@property]
    registration:
    {[
      @layer properties {
        *, :before, :after, ::backdrop {
          --tw-shadow: 0 0 #0000;
          --tw-border-style: initial;
          --tw-shadow-alpha: 100%;
        }
      }
    ]}

    {2 [@layer theme]}
    Contains theme design tokens - the actual values:
    {[
      @layer theme {
        :root, :host {
          --font-weight-thin: 100;
          --font-weight-bold: 700;
          --text-xl: 1.25rem;
        }
      }
    ]}

    {2 [@layer utilities]}
    Contains utility class definitions that set variables and CSS properties:
    {[
      @layer utilities {
        .font-thin {
          --tw-font-weight: var(--font-weight-thin);
          font-weight: var(--font-weight-thin);
        }
        .border-solid {
          --tw-border-style: solid;
          border-style: solid;
        }
        .border {
          border-style: var(--tw-border-style);
          border-width: 1px;
        }
      }
    ]}

    {2 [@property declarations]}
    Type registrations for animated/transitionable variables (at the end):
    {[
      \@property --tw-shadow {
        syntax: "*";
        inherits: false;
        initial-value: 0 0 #0000;
      }
      \@property --tw-shadow-alpha {
        syntax: "<percentage>";
        inherits: false;
        initial-value: 100%;
      }
    ]}

    {1 Variable Types and [@property] Rules}

    The variable system has 4 constructors that map to clear, distinct
    behaviors:

    {2 Constructor Rules}

    {b 1. [theme]} - Design tokens in theme layer
    - Layer: [@layer theme]
    - [@property]: {b Never generated}
    - Usage: Utilities reference these values but never modify them
    - Example: [--text-xl: 1.25rem] referenced by [.text-xl]

    {b 2. [property_default]} - Variables with required initial values
    - Layer: [@layer utilities]
    - [@property]: {b Always generated with initial-value}
    - Usage: Some utilities set it, others rely on the default
    - Example: [--tw-border-style] with initial [solid]

    {b 3. [channel ~needs_property:false]} - Composition variables
    - Layer: [@layer utilities]
    - [@property]: {b Never generated}
    - Usage: Multiple utilities set portions, aggregator combines them
    - Example: [--tw-translate-x], [--tw-rotate] combined by [.transform]

    {b 4. [channel ~needs_property:true]} - Animated composition variables
    - Layer: [@layer utilities]
    - [@property]: {b Always generated without initial-value}
    - Usage: Same as channel but needs [@property] for animations
    - Example: [--tw-font-weight], [--tw-shadow-color]

    {b 5. [ref_only]} - Reference-only variables
    - Layer: None (no declaration)
    - [@property]: {b Never generated}
    - Usage: Only referenced with explicit fallback, never set by our utilities
    - Example: Variables from other libraries we reference but don't control

    {2 Simple Decision Tree}

    To choose the right constructor:

    1. Is it a design token shared across utilities? → [theme] 2. Does it need a
    default value for referencing utilities? → [property_default] 3. Is it only
    referenced, never set by us? → [ref_only] 4. Otherwise it's a utility
    variable → [channel]
    - Add [~needs_property:true] only if it needs animation support

    {1 Inline Mode Requirements}

    The variable system must support an inline mode that generates CSS without
    any custom properties, suitable for embedding in HTML style attributes or
    environments that don't support CSS variables.

    {2 Inline Mode Constraint}
    Every variable must always have a concrete default value available for
    inline rendering:
    - Theme variables: use the stored theme value
    - Property_default variables: use the [@property] initial value
    - Channel variables: use the identity/zero value (0px, 0deg, 1.0, etc.)
    - Ref_only variables: use the fallback value
    - Always-set variables: use the bound value

    {2 Example: Variables vs Inline Mode}
    {[
      (* Variables mode *)
      .border { border-style: var(--tw-border-style); border-width: 1px; }
      .border-solid { --tw-border-style: solid; border-style: solid; }

      (* Inline mode - no variables *)
      .border { border-style: solid; border-width: 1px; }  (* uses [@property] initial *)
      .border-solid { border-style: solid; }
    ]}

    This constraint ensures the same CSS classes work identically whether
    variables are supported or not, enabling progressive enhancement and broader
    compatibility.

    {1 Variable Usage Policy}

    The variable system follows a simple, strict policy:

    {2 The Three Rules}

    1. {b When you need both declaration and variable reference}: Use
    [Var.binding] 2.
    {b When you need only declaration OR only variable reference}: Pass it as
    function parameter, let the parent function call [Var.binding] 3.
    {b No other ways are allowed}: No direct [Css.var_ref], no ignoring
    declarations, no workarounds

    {2 Examples}

    {b Rule 1: Need both declaration and variable}
    {[
      (* Variable owned by this module *)
      let my_var = Var.theme Type "css-var-name" ~order:N

      (* Utility that sets the variable *)
      let my_utility =
        let var_d, var_v = Var.binding my_var value in
        style "utility-name" [ var_d; css_property (Css.Var var_v) ]
    ]}

    {b Rule 2: Need only declaration OR only variable reference}
    {[
      (* Function receives variable reference as 'a Css.var parameter *)
      let my_utility var_ref =
        style "utility-name" [ css_property (Var var_ref) ]

      (* Or receives declaration as Css.declaration parameter *)
      let my_utility var_decl =
        style "utility-name" [ var_decl; css_property some_other_value ]

      (* Parent function calls Var.binding and passes the needed part *)
      let parent_utility =
        let var_d, var_v = Var.binding my_var value in
        my_utility var_v (* or: my_utility var_d *)
    ]}

    {1 Advanced Patterns}

    {2 Theme Record Pattern}
    For complex utilities involving multiple variables, use a record to manage
    all bindings centrally:
    {[
      type font_theme = {
        weight : declaration * font_weight var;
        size : declaration * font_size var;
        leading : declaration * line_height var;
      }

      let default_font_theme =
        let weight_d, weight_v = Var.binding weight_var default_weight in
        let size_d, size_v = Var.binding size_var default_size in
        let leading_d, leading_v = Var.binding leading_var default_leading in
        {
          weight = (weight_d, weight_v);
          size = (size_d, size_v);
          leading = (leading_d, leading_v);
        }

      let text_utility size_value =
        let theme = default_font_theme in
        let new_size_d, new_size_v = Var.binding size_var size_value in
        let updated_theme = { theme with size = (new_size_d, new_size_v) } in
        style "text-xl"
          [
            fst updated_theme.size;
            (* only the active declaration *)
            font_size (Var (snd updated_theme.size));
            line_height (Var (snd theme.leading)) (* reference default *);
          ]
    ]}

    This pattern allows selective variable updates while maintaining consistent
    defaults for other variables in the group.

    {2 Inline Mode Semantics}
    The goal is to make inline mode work properly by ensuring every variable has
    a clear, single point where it's defined and set. This is achieved by always
    following the three rules unless no declaration exists in the Tailwind case.

    The [binding] function's value parameter serves as the default for inline
    mode:
    {[
      (* Variables mode: generates --tw-color: red; color: var(--tw-color) *)
      (* Inline mode: generates color: red directly *)
      let color_decl, color_ref = Var.binding color_var red in
      style "text-red-500" [ color_decl; color (Var color_ref) ]
    ]}

    This guarantees consistent behavior between Variables and Inline modes with
    a single source of truth for each variable's value.

    {2 [@Property] Registration Strategy}
    Use [@property] registration for variables that need type safety, animation
    support, or fallback defaults for referencing utilities:
    {[
      (* Variable with [@property] initial value *)
      let shadow_var =
        Var.property_default Css.Shadow "tw-shadow"
          ~property:(Some (Shadow "0 0 #0000"), false)

      (* Setting utility - follows three rules *)
      let shadow_lg_utility =
        let decl, var_ref = Var.binding shadow_var lg_shadow in
        style "shadow-lg" [ decl; box_shadow (Var var_ref) ]

      (* Referencing utility - needs [@property] defaults and property_rules *)
      let shadow_utility =
        let var_ref = Var.reference shadow_var in
        let property_rule =
          match Var.property_rule shadow_var with
          | Some rule -> rule
          | None -> Css.empty
        in
        style "shadow" ~property_rules:property_rule
          [ box_shadow (Var var_ref) ]
    ]}

    {2 The Border Pattern: Setting vs Referencing Variables}

    For override variables like border-style, utilities fall into two
    categories:

    {3 Setting Utilities}
    Set the variable to a specific value (use [Var.binding]):
    {[
      (* No [@property] rule generation needed *)
      let border_solid =
        let decl, var_ref = Var.binding border_style_var Solid in
        style "border-solid" [ decl; border_style (Var var_ref) ]
    ]}

    {3 Referencing Utilities}
    Reference the variable with [@property] default (use [Var.reference] +
    [~property_rules]):
    {[
      (* Generates [@property] rule and properties layer *)
      let border =
        let var_ref = Var.reference border_style_var in
        let property_rule =
          match Var.property_rule border_style_var with
          | Some rule -> rule
          | None -> Css.empty
        in
        style "border" ~property_rules:property_rule
          [ border_style (Var var_ref); border_width (Px 1.) ]
    ]}

    This pattern ensures [@property] rules are only generated when utilities
    actually need the fallback defaults, not when they set the variable.

    {2 Module Organization}

    - Variables are declared at module top
    - Each variable is owned by one module
    - Cross-module usage only via function parameters
    - Parent functions call [Var.binding] and pass results to children *)

(** Layer classification for CSS variables *)
type layer = Theme | Utility

type 'a property_info = {
  initial : 'a option;
  inherits : bool;
  universal : bool;
}
(** Property metadata for [@property] registration.
    - [initial]: Optional initial value. If [None], properties layer uses
      "initial"
    - [inherits]: Whether the property inherits from parent elements
    - [universal]: Force universal syntax "*" instead of typed syntax *)

val property_info :
  ?initial:'a -> ?inherits:bool -> ?universal:bool -> unit -> 'a property_info
(** [property_info ?initial ?inherits ?universal ()] creates property metadata
    with defaults: inherits=false, universal=false. *)

type ('a, 'r) t
(** The type for CSS variables with phantom type for role *)

val pp : Format.formatter -> ('a, 'r) t -> unit
(** [pp ppf v] pretty-prints a variable. *)

(** {2 Type shortcuts for common patterns} *)

(* Family classification for ordering without string-prefix checks *)
type family =
  [ `Border
  | `Rotate
  | `Skew
  | `Scale
  | `Translate
  | `Gradient
  | `Shadow
  | `Inset_shadow
  | `Ring
  | `Inset_ring
  | `Leading
  | `Font_weight
  | `Duration
  | `Tracking
  | `Text_shadow ]

type 'a theme = ('a, [ `Theme ]) t
(** Theme variables (Pattern 1) - design tokens set in theme layer *)

type 'a property_default = ('a, [ `Property_default ]) t
(** Property default variables (Pattern 2) - variables with [@property] defaults
*)

type 'a channel = ('a, [ `Channel ]) t
(** Channel variables (Pattern 3) - composition variables *)

type 'a ref_only = ('a, [ `Ref_only ]) t
(** Reference-only variables (Pattern 4) - variables only referenced, never set
*)

(** {1 Core API} *)

val theme : 'a Css.kind -> string -> order:int * int -> 'a theme
(** [theme kind name ~order] creates a Theme-layer variable (design token).
    Values are set via [Var.binding] at use sites that own the declaration.
    Enforces explicit ordering for deterministic theme output. *)

val property_default :
  'a Css.kind ->
  initial:'a ->
  ?inherits:bool ->
  ?universal:bool ->
  ?initial_css:string ->
  ?property_order:int ->
  ?family:family ->
  string ->
  'a property_default
(** [property_default kind ~initial name ?inherits ?universal ?property_order]
    creates a Utility variable with a typed [\@property] registration and an
    initial value used for referencing utilities and inline mode. The initial
    value is required for proper [\@property] registration and reference
    fallbacks.

    [property_order] specifies the ordering of this variable in the
    {i \@layer properties \@supports} block. Lower values appear first.

    {b IMPORTANT}: Due to current architecture limitations, any style that uses
    a [property_default] variable MUST include its [property_rule] explicitly:
    {[
      let my_var = Var.property_default Content ~initial:(String "") "tw-content"

      let my_style =
        let decl, ref = Var.binding my_var value in
        let property_rules =
          match Var.property_rule my_var with
          | None -> Css.empty
          | Some r -> r
        in
        style "my-class" ~property_rules [ decl; ... ]
    ]}

    This ensures the [\@property] rule with the correct initial value flows
    through the system. Without this, a generic [\@property] rule without
    initial value may be generated, breaking the CSS output.

    TODO: Fix this architecture limitation to automatically include property
    rules for property_default variables in rules.ml without requiring explicit
    inclusion. *)

val channel :
  ?needs_property:bool ->
  ?property_order:int ->
  ?family:family ->
  'a Css.kind ->
  string ->
  'a channel
(** [channel ?needs_property ?property_order kind name] creates a Utility
    variable. When [needs_property] is true, generates an [\@property] rule for
    animation support. [property_order] specifies ordering in the {i \@supports}
    block. Ideal for composition patterns where contributing utilities set
    declarations and aggregators reference values. *)

val get_property_order : string -> int option
(** [get_property_order name] returns the property order for a variable name,
    used for sorting properties in the {i \@layer properties \@supports} block.
    Returns [None] if no order was registered. *)

val get_order : string -> (int * int) option
(** [get_order name] returns the theme layer order for a variable name. None if
    no order was set (i.e., not a theme variable). *)

val get_family : string -> family option
(** [get_family name] returns the family for a variable name. None if the
    variable is not registered. *)

val get_needs_property : string -> bool
(** [get_needs_property name] returns whether a variable needs an [\@property]
    rule. Returns [false] if the variable is not registered or doesn't need
    [\@property]. *)

val ref_only : 'a Css.kind -> string -> fallback:'a -> 'a ref_only
(** [ref_only kind name ~fallback] creates a reference-only handle to a Utility
    variable with a concrete fallback for inline mode. No declaration is
    produced. This implements Pattern 4 - variables that are only referenced,
    never set. *)

val theme_ref : string -> default:'a -> default_css:string -> 'a Css.var
(** [theme_ref name ~default ~default_css] creates a bare var reference to an
    optional theme variable. In variables mode, emits [var(--name)]. When the
    var is resolved (e.g., for [\@config run] tests where the theme doesn't
    define it), emits the concrete [default_css] string instead.

    [default] is the typed value used for inline mode (when [pp_var] inlines the
    default). [default_css] is the CSS string registered in the theme ref
    registry for [resolve_theme_refs].

    Used for keyword utilities like [z-auto] where:
    - Without custom theme: output is [z-index: auto] (concrete)
    - With [\@theme --z-index-auto: 42]: output is
      [z-index: var(--z-index-auto)] *)

val resolve_theme_refs : string -> string option
(** [resolve_theme_refs name] returns the default CSS string value for a
    [theme_ref] variable. Used as [theme_defaults] in [Pp.ctx] when theme
    variables don't exist and should be replaced by their concrete defaults. *)

val set_theme_value : string -> string -> unit
(** [set_theme_value name value] registers a theme value override. When set,
    utilities using [theme_ref] for this name will produce custom declarations
    in the theme layer (e.g., [--z-index-auto: 42] in [:root, :host]). *)

val get_theme_value : string -> string option
(** [get_theme_value name] returns the overridden theme value, if any. *)

val clear_theme_values : unit -> unit
(** [clear_theme_values ()] removes all theme value overrides. *)

val binding :
  ('a, [< `Theme | `Property_default | `Channel ]) t ->
  ?fallback:'a Css.fallback ->
  'a ->
  Css.declaration * 'a Css.var
(** [binding var ?fallback value] creates both a CSS declaration and a var()
    reference with the value as default for inline mode. This is the primary way
    to use variables.

    - [fallback] if provided, the var reference will use this fallback instead
      of the default value. Can be [Empty] for var(--name,), [None] for
      var(--name), or [Fallback value] for var(--name, value). This is useful
      for utilities that want to reference a variable with a different fallback
      (e.g., text-xs references --tw-leading with --text-xs--line-height as
      fallback). *)

val reference : ('a, [< `Ref_only | `Property_default ]) t -> 'a Css.var
(** [reference var] creates a reference to a variable. For ref_only and
    property_default variables only. *)

val reference_with_fallback : ('a, [< `Theme | `Channel ]) t -> 'a -> 'a Css.var
(** [reference_with_fallback var fallback_value] creates a variable reference
    with an explicit fallback value. Required for theme and channel variables.
*)

val reference_with_empty_fallback : ('a, [< `Channel ]) t -> 'a Css.var
(** [reference_with_empty_fallback var] creates a variable reference with an
    empty fallback, producing [var(--name,)]. Used for optional transform
    components where unset variables should contribute nothing. *)

val reference_with_var_fallback :
  ('a, [< `Channel ]) t -> ('a, [< `Theme ]) t -> 'a -> 'a Css.var
(** [reference_with_var_fallback channel_var theme_var dummy_value] creates a
    variable reference to [channel_var] with a nested var fallback to
    [theme_var]. Produces: [var(--channel, var(--theme-fallback))]. The
    [dummy_value] is used for type inference but not in the output. *)

val property_rule : ('a, [< `Property_default | `Channel ]) t -> Css.t option
(** [property_rule var] generates the [@property] rule if metadata is present.
    Returns [None] if the variable has no property metadata.

    Used with [Var.reference] to provide explicit property rules:
    {[
      (* Generate [@property] rule for variables that need it *)
      let property_rules =
        match Var.property_rule my_var with
        | Some rule -> rule
        | None -> Css.empty
    ]}

    Example output:
    {[
      \@property --tw-shadow {
        syntax: "*";
        inherits: false;
        initial-value: 0 0 #0000;
      }
    ]} *)

val property_rules : ('a, [< `Property_default ]) t -> Css.t
(** [property_rules var] is a convenience function for property_default
    variables that returns either the property rule or [Css.empty] if there is
    none. This simplifies the common pattern of:
    {[
      let property_rules =
        match Var.property_rule var with None -> Css.empty | Some r -> r
    ]}
    to just:
    {[
      let property_rules = Var.property_rules var
    ]}
    Only use this for property_default variables where you expect a property
    rule. *)

(** {1 Heterogeneous Collections} *)

type any_var =
  | Any : ('a, 'r) t -> any_var
      (** Existential type for heterogeneous collections of variables *)

val properties : any_var list -> Css.t
(** [properties vars] generates deduplicated [@property] rules for all variables
    that need them, sorted deterministically by (name, kind). *)

(** {1 Helper Types and Functions} *)

val css_name : ('a, _) t -> string
(** [css_name var] returns the full CSS property name with [--] prefix. For
    example, [css_name gradient_from_var] returns ["--tw-gradient-from"]. Use
    this when you need the property name (e.g., for [transition-property]). *)

val var_needs_property : 'a Css.var -> bool
(** [var_needs_property v] is [true] if [v]'s underlying Var.t has property
    metadata. *)

val order_of_declaration : Css.declaration -> (int * int) option
(** [order_of_declaration d] returns theme ordering information for a custom
    declaration. *)

val property_initial_string : Css.property_info -> string
(** [property_initial_string info] converts the typed initial value of a
    [@property] declaration into a string suitable for [initial-value:]. *)
