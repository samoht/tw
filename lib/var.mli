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

    {1 The Variable API}

    The API centers around the {!type:t} record type, which represents a
    complete variable definition. Variables are created with {!create}, enhanced
    with {!with_property} or {!with_fallback}, and used in utilities via the
    {!binding} type.

    {2 Basic Usage}

    {[
      (* Create theme variable *)
      let font_weight_bold =
        Var.create Css.Font_weight "font-weight-bold" ~layer:Theme ~order:206

      (* Create utility variable with @property metadata *)
      let font_weight =
        Var.create Css.Font_weight "tw-font-weight" ~layer:Utility
        |> Var.with_property ~initial:(Weight 400)

      (* Use in utilities - ALWAYS use Binding to set variables *)
      let font_bold =
        style "font-bold"
          ~vars:
            [
              Binding (font_weight_bold, Weight 700);
              Binding (font_weight, Var (Var.use font_weight_bold));
            ]
          [ Css.font_weight (Var.use font_weight) ]
      (* The @property rule is automatically extracted from font_weight *)
    ]}

    This generates:
    - In [@layer theme]: [--font-weight-bold: 700]
    - In [@layer properties]: [--tw-font-weight: 400]
    - In [@property]: [@property --tw-font-weight { syntax: "*"; ... }]
      (automatically extracted)
    - In [@layer utilities]:
      [.font-bold { --tw-font-weight: 700; font-weight: var(--tw-font-weight,
       400) }]

    {2 Note on Property Rules}

    The [style] function automatically collects [@property] rules from variables
    in the [~vars] parameter. You only need to use [~property_rules] for
    additional property rules not tied to the variables being set. *)

(** Layer classification for CSS variables *)
type layer = Theme | Utility

type 'a property_info = { initial : 'a; inherits : bool }
(** Property metadata for @property registration. *)

type 'a t = {
  kind : 'a Css.kind;  (** CSS type witness ensuring type safety *)
  name : string;  (** CSS variable name without the [--] prefix *)
  layer : layer;  (** Whether this is a theme token or utility variable *)
  binding : 'a -> Css.declaration * 'a Css.var;
  property : 'a property_info option;  (** Optional [@property] metadata *)
  order : int option;  (** Explicit ordering for theme layer *)
}
(** The type for CSS variables *)

(** {1 Core API} *)

val create :
  'a Css.kind ->
  ?fallback:'a ->
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

val binding : 'a t -> 'a -> Css.declaration * 'a Css.var
(** [binding var value] creates both a CSS declaration and a var() reference
    with the value as default for inline mode. This is the primary way to use
    variables. *)

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
