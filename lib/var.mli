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
      (* Define variable kinds - modules extend this *)
      type _ Var.kind +=
        | Font_weight : Css.font_weight Var.kind
        | Font_weight_bold : Css.font_weight Var.kind

      (* Create theme variable *)
      let font_weight_bold =
        Var.create Font_weight_bold ~name:"font-weight-bold" ~layer:Theme

      (* Create utility variable with @property metadata *)
      let font_weight =
        Var.create Font_weight ~name:"tw-font-weight" ~layer:Utility
        |> Var.with_property ~syntax:Css.Universal ~initial:(Weight 400)
        |> Var.with_fallback (Weight 400)

      (* Use in utilities with the binding pattern *)
      let font_bold =
        style "font-bold"
          ~vars:[ Binding (font_weight, Weight 700) ]
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

type _ kind = ..
(** CSS variable kinds as extensible GADT *)

(** Core CSS variable kinds - extended by individual modules *)
type _ kind +=
  | (* Core shared variables *)
      Color :
      string * int option
      -> Css.color kind (* e.g., Color ("blue", Some 500) *)
  | Spacing : Css.length kind
  | Font_family_list : Css.font_family kind
  | Scroll_snap_strictness : Css.scroll_snap_strictness kind
  | Duration : Css.duration kind

(** Property metadata. *)
type 'a property_info = Info : 'b Css.syntax * 'b * bool -> 'a property_info

type 'a t = {
  kind : 'a kind;  (** Type witness ensuring type safety *)
  name : string;  (** CSS variable name without the [--] prefix *)
  layer : layer;  (** Whether this is a theme token or utility variable *)
  fallback : 'a option;  (** Default value for [var()] references *)
  property : 'a property_info option;  (** Optional [@property] metadata *)
  order : int;  (** Explicit ordering for theme layer *)
}
(** Variable definition record - the main type for working with CSS variables *)

(** Var binding pairs a variable with its value for use in utilities. When
    passed to [style], both the declaration and any [@property] rule are
    automatically extracted. *)
type binding = Binding : 'a t * 'a -> binding

(** {1 Core API} *)

val create :
  'a kind -> ?fallback:'a -> order:int -> string -> layer:layer -> 'a t
(** [create kind ?fallback ~order name ~layer] creates a variable definition.

    - [kind]: The type witness (e.g., [Font_weight])
    - [name]: CSS variable name without [--] (e.g., ["tw-font-weight"])
    - [layer]: [Theme] for design tokens, [Utility] for property channels.
    - [fallback] sets the fallback value for [var()] references. This value is
      used when the variable is not set: [var(--name, fallback)]
    - [order] sets the order for sorting variables in the theme layer *)

val with_property :
  syntax:'b Css.syntax -> initial:'b -> ?inherits:bool -> 'a t -> 'a t
(** [with_property ~syntax ~initial ?inherits var] adds [@property] metadata.
    This enables CSS transitions, animations, and proper cascade behavior.

    - [syntax]: CSS syntax descriptor (e.g., [Css.Length], [Css.Color])
    - [initial]: Initial value for [@layer properties] and [@property]
    - [inherits]: Whether the property inherits (defaults to [false]) *)

val declaration : 'a t -> 'a -> Css.declaration
(** [declaration var value] creates a CSS custom property declaration with the
    given value. This is used in utility classes to set the variable:
    [--tw-font-weight: 700] *)

val needs_property : 'a t -> bool
(** [needs_property var] returns true if variable has [@property] metadata *)

val use : 'a t -> 'a Css.var
(** [use var] creates a [var()] reference for use in CSS properties. Includes
    fallback if set: [var(--tw-font-weight, 400)] *)

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

val declaration_of_binding : binding -> Css.declaration
(** [declaration_of_binding (Binding (var, value))] extracts the CSS declaration
    from a variable binding. Used internally by the [style] function to
    automatically generate variable declarations from the [~vars] parameter. *)

val property_rule_of_binding : binding -> Css.t option
(** [property_rule_of_binding (Binding (var, _))] extracts the [@property] rule
    from a variable binding if the variable has property metadata. Used
    internally by the [style] function to automatically collect property rules
    from the [~vars] parameter. *)

(** {1 Helper Types and Functions} *)

(** Existential wrapper for heterogeneous collections *)
type any = Any : _ kind -> any

val compare : any -> any -> int
(** [compare a b] compares two variables for canonical ordering in the theme. *)

val compare_declarations : layer -> Css.declaration -> Css.declaration -> int
(** [compare_declarations layer d1 d2] compares two custom declarations via Var
    metadata for the given layer. *)

val property_info_to_declaration_value : Css.property_info -> string
(** [property_info_to_declaration_value info] converts [@property] initial
    values to declaration values following CSS spec requirements. This is the
    canonical implementation for the 0/0px conversion used across the codebase.
*)

val of_meta : Css.meta -> any option
(** [of_meta meta] extracts the variable from CSS declaration metadata if
    present *)

type meta_info = { var : any; needs_property : bool; order : int }
(** Metadata stored in CSS declarations *)

val meta_of_info : meta_info -> Css.meta
(** [meta_of_info info] converts variable metadata to CSS metadata *)

val needs_property_of_meta : Css.meta -> bool option
(** [needs_property_of_meta meta] extracts the needs_property flag from CSS
    declaration metadata if present *)

val order_of_meta : Css.meta -> int option
(** [order_of_meta meta] extracts the ordering value from CSS declaration
    metadata if present *)

val layer_name : layer -> string
(** [layer_name layer] returns the string name of the layer ("theme" or
    "utilities") *)

val layer : 'a t -> layer
(** [layer var] returns the layer of the variable *)

module Map : Map.S with type key = any
(** Map with Var.any keys *)

module Set : Set.S with type elt = any
(** Set with Var.any elements *)
