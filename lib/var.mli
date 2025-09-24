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

    {1 Variable Types and Patterns}

    {2 Theme Variables}
    Design tokens that hold actual values, generated in [@layer theme]:
    - [--font-weight-thin: 100]
    - [--text-xl: 1.25rem]
    - [--color-blue-500: #3b82f6]

    {2 Utility Variables}
    "Property channels" that track utility state. Two main patterns:

    {3 Composition Variables (e.g., transforms, shadows)}
    Multiple utilities contribute to a single CSS property:
    {[
      .translate-x-4 { --tw-translate-x: 1rem; }
      .rotate-45 { --tw-rotate: 45deg; }
      .transform {
        transform: translateX(var(--tw-translate-x)) rotate(var(--tw-rotate));
      }
    ]}

    {3 Override Variables (e.g., border-style)}
    Each utility sets the variable to control shared behavior:
    {[
      .border-solid { --tw-border-style: solid; border-style: solid; }
      .border-dashed { --tw-border-style: dashed; border-style: dashed; }
      .border { border-style: var(--tw-border-style); border-width: 1px; }
    ]}

    {1 Variable Usage Policy}

    The variable system follows a simple, strict policy:

    {2 The Three Rules}

    1. {b When you need both declaration and variable reference}: Use [Var.binding]
    2. {b When you need only declaration OR only variable reference}: Pass it as function parameter, let the parent function call [Var.binding]
    3. {b No other ways are allowed}: No direct [Css.var_ref], no ignoring declarations, no workarounds

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
        my_utility var_v  (* or: my_utility var_d *)
    ]}

    {2 Module Organization}

    - Variables are declared at module top
    - Each variable is owned by one module
    - Cross-module usage only via function parameters
    - Parent functions call [Var.binding] and pass results to children *)

(** Layer classification for CSS variables *)
type layer = Theme | Utility

type 'a property_info = { initial : 'a; inherits : bool }
(** Property metadata for @property registration. *)

type 'a t = {
  kind : 'a Css.kind;  (** CSS type witness ensuring type safety *)
  name : string;  (** CSS variable name without the [--] prefix *)
  layer : layer;  (** Whether this is a theme token or utility variable *)
  binding : ?fallback:'a -> 'a -> Css.declaration * 'a Css.var;
  property : 'a property_info option;  (** Optional [@property] metadata *)
  order : int option;  (** Explicit ordering for theme layer *)
}
(** The type for CSS variables *)

(** {1 Core API} *)

val create :
  'a Css.kind ->
  ?property:'a * bool ->
  ?order:int ->
  string ->
  layer:layer ->
  'a t
(** [create css_kind ?fallback ?order name ~layer] creates a variable
    definition.

    - [css_kind]: The CSS type witness (e.g., [Css.Font_weight])
    - [name]: CSS variable name without [--] (e.g., ["tw-font-weight"])
    - [layer]: [Theme] for design tokens, [Utility] for property channels.
    - [property]: is TODO.
    - [fallback] sets the fallback value for [var()] references. This value is
      used when the variable is not set: [var(--name, fallback)]
    - [order] sets the order for sorting variables in the theme layer (defaults
      to 9999). Only relevant for Theme layer variables that appear in [:root]
*)

val binding : 'a t -> ?fallback:'a -> 'a -> Css.declaration * 'a Css.var
(** [binding var ?fallback value] creates both a CSS declaration and a var()
    reference with the value as default for inline mode. This is the primary way
    to use variables.

    - [fallback] if provided, the var reference will use this as a fallback
      instead of the value. This is useful for utilities that want to reference
      a variable with a different fallback (e.g., text-xs references
      --tw-leading with --text-xs--line-height as fallback). *)


val property_rule : 'a t -> Css.t option
(** [property_rule var] generates the [@property] rule if metadata is present.
    Returns [None] if the variable has no property metadata.

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
