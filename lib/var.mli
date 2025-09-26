(** Typed CSS variable definitions and CSS generation architecture.

    This module provides a type-safe system for CSS custom properties that
    generates CSS across multiple layers following Tailwind v4's architecture.

    {1 CSS Output Architecture}

    The variable system generates CSS across four layers in this order:

    {2 [@layer properties]}
    Contains initial values for utility variables that need [@property]
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
      @property --tw-shadow {
        syntax: "*";
        inherits: false;
        initial-value: 0 0 #0000;
      }
      @property --tw-shadow-alpha {
        syntax: "<percentage>";
        inherits: false;
        initial-value: 100%;
      }
    ]}

    {1 The Five Variable Patterns}

    This module recognizes exactly 5 distinct variable patterns, each with specific
    constraints and behaviors:

    {2 Pattern 1: Theme Variables}
    Design tokens set once in theme layer, referenced by utilities:
    {[
      (* Theme layer *)
      :root { --text-xl: 1.25rem; --font-weight-bold: 700; }

      (* Utility layer *)
      .text-xl { font-size: var(--text-xl); }
      .font-bold { font-weight: var(--font-weight-bold); }
    ]}

    {2 Pattern 2: Property_default Variables}
    Variables with @property defaults, allowing utilities to either SET or REFERENCE:
    {[
      (* @property provides fallback *)
      @property --tw-border-style { syntax: "*"; inherits: false; initial-value: solid; }

      (* Setting utilities *)
      .border-solid { --tw-border-style: solid; border-style: solid; }
      .border-dashed { --tw-border-style: dashed; border-style: dashed; }

      (* Referencing utilities rely on @property default *)
      .border { border-style: var(--tw-border-style); border-width: 1px; }
    ]}

    {2 Pattern 3: Channel Variables}
    Composition variables where multiple utilities contribute to a single CSS property:
    {[
      .translate-x-4 { --tw-translate-x: 1rem; }
      .rotate-45 { --tw-rotate: 45deg; }
      .scale-110 { --tw-scale: 1.1; }

      (* Aggregator combines all channels *)
      .transform { transform: translateX(var(--tw-translate-x)) rotate(var(--tw-rotate)) scale(var(--tw-scale)); }
    ]}

    {2 Pattern 4: Ref_only Variables}
    Variables that utilities only reference (with fallback), never set:
    {[
      (* Shadow utilities reference color but don't set it *)
      .shadow-sm { box-shadow: 0 1px 3px var(--tw-shadow-color, #0000001a); }
      .shadow-lg { box-shadow: 0 10px 25px var(--tw-shadow-color, #00000019); }

      (* Color utilities set the shadow color *)
      .shadow-red-500 { --tw-shadow-color: #ef4444; }
    ]}

    {2 Pattern 5: Always-set Variables}
    Variables that are always set when used (no separate reference):
    {[
      .font-bold { --tw-font-weight: 700; font-weight: var(--tw-font-weight); }
      .font-thin { --tw-font-weight: 100; font-weight: var(--tw-font-weight); }
    ]}

    {1 Inline Mode Requirements}

    The variable system must support an inline mode that generates CSS without any
    custom properties, suitable for embedding in HTML style attributes or environments
    that don't support CSS variables.

    {2 Inline Mode Constraint}
    Every variable must always have a concrete default value available for inline
    rendering:
    - Theme variables: use the stored theme value
    - Property_default variables: use the @property initial value
    - Channel variables: use the identity/zero value (0px, 0deg, 1.0, etc.)
    - Ref_only variables: use the fallback value
    - Always-set variables: use the bound value

    {2 Example: Variables vs Inline Mode}
    {[
      (* Variables mode *)
      .border { border-style: var(--tw-border-style); border-width: 1px; }
      .border-solid { --tw-border-style: solid; border-style: solid; }

      (* Inline mode - no variables *)
      .border { border-style: solid; border-width: 1px; }  (* uses @property initial *)
      .border-solid { border-style: solid; }
    ]}

    This constraint ensures the same CSS classes work identically whether variables
    are supported or not, enabling progressive enhancement and broader compatibility.

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
      let my_var = Var.create Type "css-var-name" ~layer:Layer ~order:N

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
        { weight = (weight_d, weight_v);
          size = (size_d, size_v);
          leading = (leading_d, leading_v) }

      let text_utility size_value =
        let theme = default_font_theme in
        let new_size_d, new_size_v = Var.binding size_var size_value in
        let updated_theme = { theme with size = (new_size_d, new_size_v) } in
        style "text-xl" [
          fst updated_theme.size;  (* only the active declaration *)
          font_size (Var (snd updated_theme.size));
          line_height (Var (snd theme.leading))  (* reference default *)
        ]
    ]}

    This pattern allows selective variable updates while maintaining consistent
    defaults for other variables in the group.

    {2 Inline Mode Semantics}
    The goal is to make inline mode work properly by ensuring every variable has
    a clear, single point where it's defined and set. This is achieved by always
    following the three rules unless no declaration exists in the Tailwind case.

    The [binding] function's value parameter serves as the default for inline mode:
    {[
      (* Variables mode: generates --tw-color: red; color: var(--tw-color) *)
      (* Inline mode: generates color: red directly *)
      let color_decl, color_ref = Var.binding color_var red in
      style "text-red-500" [ color_decl; color (Var color_ref) ]
    ]}

    This guarantees consistent behavior between Variables and Inline modes with
    a single source of truth for each variable's value.

    {2 @Property Registration Strategy}
    Use [@property] registration for variables that need type safety, animation
    support, or fallback defaults for referencing utilities:
    {[
      (* Variable with @property initial value *)
      let shadow_var = Var.create Css.Shadow "tw-shadow" ~layer:Utility
        ~property:(Some (Shadow "0 0 #0000"), false)

      (* Setting utility - follows three rules *)
      let shadow_lg_utility =
        let decl, var_ref = Var.binding shadow_var lg_shadow in
        style "shadow-lg" [ decl; box_shadow (Var var_ref) ]

      (* Referencing utility - needs @property defaults and property_rules *)
      let shadow_utility =
        let var_ref = Var.reference shadow_var in
        let property_rule = match Var.property_rule shadow_var with
          | Some rule -> rule | None -> Css.empty in
        style "shadow" ~property_rules:property_rule [ box_shadow (Var var_ref) ]
    ]}

    {2 The Border Pattern: Setting vs Referencing Variables}

    For override variables like border-style, utilities fall into two categories:

    {3 Setting Utilities}
    Set the variable to a specific value (use [Var.binding]):
    {[
      (* No @property rule generation needed *)
      let border_solid =
        let decl, var_ref = Var.binding border_style_var Solid in
        style "border-solid" [ decl; border_style (Var var_ref) ]
    ]}

    {3 Referencing Utilities}
    Reference the variable with @property default (use [Var.reference] + [~property_rules]):
    {[
      (* Generates @property rule and properties layer *)
      let border =
        let var_ref = Var.reference border_style_var in
        let property_rule = match Var.property_rule border_style_var with
          | Some rule -> rule | None -> Css.empty in
        style "border" ~property_rules:property_rule [
          border_style (Var var_ref); border_width (Px 1.)
        ]
    ]}

    This pattern ensures @property rules are only generated when utilities actually
    need the fallback defaults, not when they set the variable.

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
(** Property metadata for @property registration.
    - [initial]: Optional initial value. If [None], properties layer uses "initial"
    - [inherits]: Whether the property inherits from parent elements
    - [universal]: Force universal syntax "*" instead of typed syntax *)

val property_info :
  ?initial:'a -> ?inherits:bool -> ?universal:bool -> unit -> 'a property_info
(** Create property metadata with defaults: inherits=false, universal=false *)

type 'a t = {
  kind : 'a Css.kind;  (** CSS type witness ensuring type safety *)
  name : string;  (** CSS variable name without the [--] prefix *)
  layer : layer;  (** Whether this is a theme token or utility variable *)
  binding : ?fallback:'a Css.fallback -> 'a -> Css.declaration * 'a Css.var;
  property : 'a property_info option;  (** Optional [@property] metadata *)
  order : int option;  (** Explicit ordering for theme layer *)
}
(** The type for CSS variables *)

(** {1 Core API} *)

val theme : 'a Css.kind -> string -> order:int -> 'a t
(** [theme kind name ~order] creates a Theme-layer variable (design token).
    Values are set via [Var.binding] at use sites that own the declaration.
    Enforces explicit ordering for deterministic theme output. *)

val property_default :
  'a Css.kind ->
  ?initial:'a ->
  ?inherits:bool ->
  ?universal:bool ->
  string ->
  'a t
(** [property_default kind name ~initial ?inherits ?universal] creates a Utility
    variable with a typed [@property] registration and an initial value used for
    referencing utilities and inline mode. *)

val channel : 'a Css.kind -> string -> 'a t
(** [channel kind name] creates a Utility variable without [@property]. Ideal
    for composition patterns where contributing utilities set declarations and
    aggregators reference values. *)

val ref_only : 'a Css.kind -> string -> fallback:'a -> 'a Css.var
(** [ref_only kind name ~fallback] creates a reference-only handle to a Utility
    variable with a concrete fallback for inline mode. No declaration is
    produced. *)

val binding :
  'a t -> ?fallback:'a Css.fallback -> 'a -> Css.declaration * 'a Css.var
(** [binding var ?fallback value] creates both a CSS declaration and a var()
    reference with the value as default for inline mode. This is the primary way
    to use variables.

    - [fallback] if provided, the var reference will use this fallback instead
      of the default value. Can be [Empty] for var(--name,), [None] for
      var(--name), or [Fallback value] for var(--name, value). This is useful
      for utilities that want to reference a variable with a different fallback
      (e.g., text-xs references --tw-leading with --text-xs--line-height as
      fallback). *)

val reference : ?fallback:'a Css.fallback -> 'a t -> 'a Css.var
(** [reference var ?fallback] creates a variable reference without a declaration.

    Requirements:
    - EITHER the variable has ~property metadata with an initial value
    - OR a fallback is provided

    This allows referencing variables in two scenarios:
    1. Variables with @property defaults (e.g., border utilities)
    2. Variables without setting them when you provide a fallback (e.g., shadow utilities)

    Use this for utilities that REFERENCE a variable but don't SET it:
    - Variables mode: generates [var(--name)] without fallback
    - Inline mode: uses the @property initial value directly

    {b Must be paired with ~property_rules:}
    {[
      let utility =
        let var_ref = Var.reference my_var in
        let props = match Var.property_rule my_var with
          | Some rule -> rule | None -> Css.empty in
        style "my-util" ~property_rules:props [ css_prop (Var var_ref) ]
    ]}

    This ensures @property rules are generated only when utilities need them,
    following the border/border-none pattern where setting utilities use
    [Var.binding] and referencing utilities use [Var.reference]. *)

val property_rule : 'a t -> Css.t option
(** [property_rule var] generates the [@property] rule if metadata is present.
    Returns [None] if the variable has no property metadata.

    Used with [Var.reference] to provide explicit property rules:
    {[
      (* Generate @property rule for variables that need it *)
      let property_rules =
        match Var.property_rule my_var with
        | Some rule -> rule
        | None -> Css.empty
    ]}

    Example output:
    {[
      @property --tw-shadow {
        syntax: "*";
        inherits: false;
        initial-value: 0 0 #0000;
      }
    ]} *)

(** {1 Helper Types and Functions} *)

val var_needs_property : 'a Css.var -> bool
(** [var_needs_property v] is [true] if [v]'s underlying Var.t has property
    metadata. *)

val order_of_declaration : Css.declaration -> int option
(** [order_of_declaration d] returns theme ordering information for a custom
    declaration. *)

val property_initial_string : Css.property_info -> string
(** [property_initial_string info] converts the typed initial value of a
    [@property] declaration into a string suitable for [initial-value:]. *)
