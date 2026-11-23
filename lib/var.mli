(** Typed CSS variables and the layers they are written to.

    {1 CSS Output Architecture}

    The variable system generates CSS custom properties across the layers of
    Tailwind v4's architecture. Each layer has its own deterministic ordering
    rules:

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
      then by per-group suborder. Examples: display -> position -> margin ->
      background -> padding -> typography -> border -> sizing -> effects ->
      interactivity -> flexbox/grid -> gap -> container/prose. Within background
      colors, sort uses canonical palette order; for margins/padding, "all"
      precedes axis which precedes side-specific.

    {2 [\@layer properties]}
    Contains initial values for utility variables that need [\@property]
    registration:
    {v
      @layer properties {
        *, :before, :after, ::backdrop {
          --tw-shadow: 0 0 #0000;
          --tw-border-style: initial;
          --tw-shadow-alpha: 100%;
        }
      }
    v}

    {2 [@layer theme]}
    Contains theme design tokens - the actual values:
    {v
      @layer theme {
        :root, :host {
          --font-weight-thin: 100;
          --font-weight-bold: 700;
          --text-xl: 1.25rem;
        }
      }
    v}

    {2 [@layer utilities]}
    Contains utility class definitions that set variables and CSS properties:
    {v
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
    v}

    {2 [@property declarations]}
    Type registrations for animated/transitionable variables (at the end):
    {v
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
    v}

    {1 Variable Types and [@property] Rules}

    The variable system has 3 constructors: [theme], [property_default] and
    [channel].

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

    {b 3. [channel]} - Composition variables
    - Layer: [@layer utilities]
    - [@property]: {b Never generated} by default; with [~needs_property:true],
      {b always generated without initial-value}
    - Usage: Multiple utilities set portions, aggregator combines them
    - Example: [--tw-translate-x], [--tw-rotate] combined by [.transform];
      [--tw-font-weight], [--tw-shadow-color] with [~needs_property:true]

    {2 Simple Decision Tree}

    To choose the right constructor:

    + Is it a design token shared across utilities? Use [theme].
    + Does it need a default value for referencing utilities? Use
      [property_default].
    + Otherwise it is a utility variable: use [channel], with
      [~needs_property:true] only if it needs animation support.

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
    - Always-set variables: use the bound value

    {2 Example: Variables vs Inline Mode}
    {v
      (* Variables mode *)

      .border { border-style: var(--tw-border-style); border-width: 1px; }
      .border-solid { --tw-border-style: solid; border-style: solid; }

      (* Inline mode - no variables *)
      .border { border-style: solid; border-width: 1px; }  (* uses [@property] initial *)
      .border-solid { border-style: solid; }
    v}

    This constraint ensures the same CSS classes work identically whether
    variables are supported or not, enabling progressive enhancement and broader
    compatibility.

    {1 Variable Usage Policy}

    The variable system follows a simple, strict policy:

    {2 The Four Rules}

    1. {b When you need both declaration and variable reference}: Use
    [Var.binding] 2. {b When you need only the declaration}: Use [Var.set],
    which is the declaration of the same binding 3.
    {b When you need only the variable reference}: Pass it as function
    parameter, let the parent function call [Var.binding] 4.
    {b No other ways are allowed}: No direct [Css.var_ref], no ignoring
    declarations, no workarounds

    {2 Examples}

    {b Rule 1: Need both declaration and variable}
    {[
    let my_var = Var.theme Css.Color "example-color" ~order:(999, 0)

    let my_utility =
      let var_d, var_v = Var.binding my_var (Css.Named Css.Red) in
      Style.style [ var_d; Css.color (Css.Var var_v) ]
    ]}

    {b Rule 2: Need only declaration OR only variable reference}
    {[
    let color_utility var_ref = Style.style [ Css.color (Css.Var var_ref) ]

    let declaration_utility var_decl =
      Style.style [ var_decl; Css.color (Css.Named Css.Black) ]

    let my_var = Var.theme Css.Color "example-parent-color" ~order:(999, 1)
    let value : Css.color = Css.Named Css.Blue

    let parent_utility =
      let var_d, var_v = Var.binding my_var value in
      let _set_only = declaration_utility var_d in
      color_utility var_v
    ]}

    {1 Advanced Patterns}

    {2 Theme Record Pattern}
    For complex utilities involving multiple variables, use a record to manage
    all bindings centrally:
    {[
    type text_theme = {
      color : Css.declaration * Css.color Css.var;
      size : Css.declaration * Css.length Css.var;
    }

    let color_var = Var.theme Css.Color "example-text-color" ~order:(999, 2)
    let size_var = Var.theme Css.Length "example-text-size" ~order:(999, 3)

    let default_text_theme =
      let color_d, color_v = Var.binding color_var (Css.Named Css.Black) in
      let size_d, size_v = Var.binding size_var (Css.Rem 1.) in
      { color = (color_d, color_v); size = (size_d, size_v) }

    let text_utility size_value =
      let theme = default_text_theme in
      let new_size_d, new_size_v = Var.binding size_var size_value in
      let updated_theme = { theme with size = (new_size_d, new_size_v) } in
      Style.style
        [
          fst updated_theme.size;
          Css.font_size (Css.Var (snd updated_theme.size));
          Css.color (Css.Var (snd theme.color));
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
    let color_var = Var.theme Css.Color "example-inline-color" ~order:(999, 4)
    let color_decl, color_ref = Var.binding color_var (Css.Named Css.Red)
    let text_red = Style.style [ color_decl; Css.color (Css.Var color_ref) ]
    ]}

    This guarantees consistent behavior between Variables and Inline modes with
    a single source of truth for each variable's value.

    {2 [@Property] Registration Strategy}
    Use [@property] registration for variables that need type safety, animation
    support, or fallback defaults for referencing utilities:
    {[
    let example_color =
      Var.property_default Css.Color ~initial:(Css.Named Css.Black)
        "tw-example-property-color"

    let set_color =
      let decl, var_ref = Var.binding example_color (Css.Named Css.Red) in
      Style.style [ decl; Css.color (Css.Var var_ref) ]

    let reference_color =
      let var_ref = Var.reference example_color in
      Style.style
        ~property_rules:(Var.property_rules example_color)
        [ Css.color (Css.Var var_ref) ]
    ]}

    {2 The Border Pattern: Setting vs Referencing Variables}

    For override variables like border-style, utilities fall into two
    categories:

    {3 Setting Utilities}
    Set the variable to a specific value (use [Var.binding]):
    {[
    let border_style_var =
      Var.property_default Css.Border_style ~initial:Css.Solid
        "tw-example-border-style"

    let border_solid =
      let decl, var_ref = Var.binding border_style_var Css.Solid in
      Style.style [ decl; Css.border_style (Css.Var var_ref) ]
    ]}

    {3 Referencing Utilities}
    Reference the variable with [@property] default (use [Var.reference] +
    [~property_rules]):
    {[
    let border_style_var =
      Var.property_default Css.Border_style ~initial:Css.Solid
        "tw-example-border-style-reference"

    let border =
      let var_ref = Var.reference border_style_var in
      Style.style
        ~property_rules:(Var.property_rules border_style_var)
        [ Css.border_style (Css.Var var_ref); Css.border_width (Css.Px 1.) ]
    ]}

    This pattern ensures [@property] rules are only generated when utilities
    actually need the fallback defaults, not when they set the variable.

    {2 Module Organization}

    - Variables are declared at module top
    - Each variable is owned by one module
    - Cross-module usage only via function parameters
    - Parent functions call [Var.binding] and pass results to children *)

open Cascade

(** The type for the layer a CSS variable belongs to. *)
type layer = Theme | Utility

type 'a property_info = {
  initial : 'a option;
  inherits : bool;
  universal : bool;
}
(** Property metadata for [@property] registration.
    - {!field-initial}: Optional initial value. If [None], properties layer uses
      "initial"
    - {!field-inherits}: Whether the property inherits from parent elements
    - {!field-universal}: Force universal syntax "*" instead of typed syntax *)

type ('a, 'r) t
(** The type for CSS variables, with a phantom type for their role. *)

val pp : ('a, 'r) t -> string
(** [pp v] is ["Var(--<name>)"] for the variable [v]. *)

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
  | `Content
  | `Text_shadow
  | `Filter
  | `Drop_shadow
  | `Backdrop_filter ]

type 'a theme = ('a, [ `Theme ]) t
(** The type for theme variables (pattern 1), the design tokens of the theme
    layer. *)

type 'a property_default = ('a, [ `Property_default ]) t
(** The type for property-default variables (pattern 2), whose default is their
    [@property] initial value. *)

type 'a channel = ('a, [ `Channel ]) t
(** The type for channel variables (pattern 3), which several utilities compose.
*)

(** {1 Core API} *)

val theme :
  'a Css.kind -> ?runtime:bool -> string -> order:int * int -> 'a theme
(** [theme kind ~runtime name ~order] is the theme-layer variable (design token)
    [--name], placed at [order] in the theme layer. A utility that owns the
    declaration sets its value with {!val-binding}.

    With [~runtime:true] the optimizer keeps [var(name)] references unfolded
    instead of inlining the theme value, so the token stays overridable at
    runtime (matching Tailwind, which leaves e.g. [calc(var(--spacing)*4)]
    intact rather than baking in the resolved length).

    Several variables may share an [order]: token families are numbered from
    their own base, and the families a project [\@theme] names funnel every
    member into one slot. The theme layer breaks such a tie on the variable
    name. Ordering metadata travels with the variable and its declarations;
    construction does not mutate a process-global registry. *)

val property_default :
  'a Css.kind ->
  initial:'a ->
  ?inherits:bool ->
  ?universal:bool ->
  ?property_order:int ->
  ?family:family ->
  string ->
  'a property_default
(** [property_default kind ~initial ~property_order name] is the utility
    variable [--name] with a typed [\@property] registration whose initial value
    is [initial]. Referencing utilities and inline mode read [initial] too.
    [property_order] places the variable in the
    {i \@layer properties \@supports} block, lower values first.

    The build reads the typed registration metadata from declarations and
    references, so a style using {!val-binding} automatically carries the
    matching [\@property] rule. Passing [~property_rules] explicitly remains
    supported for hand-written rules. *)

val channel :
  ?needs_property:bool ->
  ?property_order:int ->
  ?family:family ->
  'a Css.kind ->
  string ->
  'a channel
(** [channel ~needs_property ~property_order kind name] is the utility variable
    [--name] that several utilities compose: contributing utilities set it and
    an aggregating one references it. When [needs_property] is [true] it gets an
    [\@property] rule, so it can be animated. [property_order] places it in the
    {i \@supports} block. *)

val theme_ref : ?default:'a -> ?default_css:string -> string -> 'a Css.var
(** [theme_ref ~default ~default_css name] is a bare reference to the theme
    variable [--name], written [var(--name)] in variables mode.

    When [default] and [default_css] are provided, both defaults are carried by
    the returned reference. [default] is the typed value used for inline mode;
    [default_css] is available from its metadata to theme-resolution passes.

    When [default] and [default_css] are omitted, the var reference is created
    without resolution registration. Use this for dynamically-constructed theme
    variable names (e.g., [blur-xl], [spacing-4]) whose values come from the
    active theme. *)

val binding :
  ('a, [< `Theme | `Property_default | `Channel ]) t ->
  ?fallback:'a Css.fallback ->
  'a ->
  Css.declaration * 'a Css.var
(** [binding var ~fallback value] is the declaration that sets [var] to [value]
    and a [var()] reference to it whose inline value is [value]. This is the
    primary way to use variables.

    [fallback], when given, replaces [value] as the reference's fallback:
    [Empty] writes [var(--name,)], [None] writes [var(--name)] and [Fallback v]
    writes [var(--name, v)]. A utility uses it to reference a variable with
    another fallback: [text-xs] references [--tw-leading] with
    [--text-xs--line-height] as the fallback. *)

val set :
  ('a, [< `Theme | `Property_default | `Channel ]) t -> 'a -> Css.declaration
(** [set var value] is the declaration of [binding var value], for a utility
    that writes [var] without reading it back. *)

val binding_initial :
  ('a, [< `Property_default | `Channel ]) t -> Css.declaration
(** [binding_initial var] resets the channel to the CSS-wide initial keyword
    ([--name: initial]). Used by the [*-initial] and [via-none] utilities, which
    clear a channel var instead of setting it to a typed value. *)

val reference : ('a, [< `Property_default ]) t -> 'a Css.var
(** [reference var] is a reference to [var] whose inline value is its
    [\@property] initial value. *)

val reference_with_fallback : ('a, [< `Theme | `Channel ]) t -> 'a -> 'a Css.var
(** [reference_with_fallback var fallback_value] is a reference to [var] with
    the fallback [fallback_value]. Theme and channel variables are referenced
    this way. *)

val reference_with_empty_fallback : ('a, [< `Channel ]) t -> 'a Css.var
(** [reference_with_empty_fallback var] is the reference [var(--name,)] to
    [var], with an empty fallback, so an unset variable contributes nothing. The
    optional transform components use it. *)

val reference_with_var_fallback :
  ('a, [< `Channel ]) t -> ('a, [< `Theme ]) t -> 'a -> 'a Css.var
(** [reference_with_var_fallback channel_var theme_var dummy_value] is the
    reference [var(--channel, var(--theme))] to [channel_var], falling back to
    [theme_var]. [dummy_value] fixes the type and is not written. *)

val property_rule : ('a, [< `Property_default | `Channel ]) t -> Css.t option
(** [property_rule var] is the [@property] rule of [var], and [None] if [var]
    has no property metadata.

    A utility that uses {!reference} passes it as an explicit property rule:
    {[
    let my_var =
      Var.property_default Css.Color ~initial:(Css.Named Css.Black)
        "tw-example-rule-color"

    let property_rules =
      match Var.property_rule my_var with
      | Some rule -> rule
      | None -> Css.empty
    ]}

    Example output:
    {v
      \@property --tw-shadow {
        syntax: "*";
        inherits: false;
        initial-value: 0 0 #0000;
      }
    v} *)

val property_rules : ('a, [< `Property_default ]) t -> Css.t
(** [property_rules var] is the property rule of [var], or [Css.empty] if there
    is none. It shortens the common pattern
    {[
    let var =
      Var.property_default Css.Color ~initial:(Css.Named Css.Black)
        "tw-example-rules-color"

    let property_rules =
      match Var.property_rule var with None -> Css.empty | Some r -> r
    ]}
    to just:
    {[
    let var =
      Var.property_default Css.Color ~initial:(Css.Named Css.Black)
        "tw-example-rules-short-color"

    let property_rules = Var.property_rules var
    ]}
    Use it only for a property-default variable that has a property rule. *)

(** {1 Helper Types and Functions} *)

type metadata
(** Build metadata carried by variable references and declarations. *)

val metadata : ('a, _) t -> metadata
(** [metadata var] is the build metadata attached to [var]. *)

val metadata_of_var : 'a Css.var -> metadata option
(** [metadata_of_var var] is the metadata the reference [var] carries, if this
    module made it. *)

val metadata_of_declaration : Css.declaration -> metadata option
(** [metadata_of_declaration declaration] is the metadata the custom-property
    [declaration] carries, if this module made it. *)

val metadata_name : metadata -> string
(** [metadata_name metadata] is the custom-property name without its leading
    ["--"]. *)

val metadata_order : metadata -> (int * int) option
(** [metadata_order metadata] is its Theme-layer ordering slot, if any. *)

val metadata_property_order : metadata -> int option
(** [metadata_property_order metadata] is its ordering slot in the properties
    layer, if any. *)

val metadata_family : metadata -> family option
(** [metadata_family metadata] is its related-property family, if any. *)

val metadata_needs_property : metadata -> bool
(** [metadata_needs_property metadata] is [true] iff the variable needs an
    [@property] registration. *)

val metadata_default_css : metadata -> string option
(** [metadata_default_css metadata] is the serialized typed initial value used
    for an automatic [@property] registration, if any. *)

val property_rule_of_var : 'a Css.var -> Css.t option
(** [property_rule_of_var var] is the exact typed [@property] rule carried by
    [var], if any. *)

val name : ('a, _) t -> string
(** [name var] is the custom property's name without the leading ["--"], the
    spelling {!theme_ref} and {!bracket} take. Read it off the handle rather
    than writing the name again beside it: the two cannot then drift apart. *)

val css_name : ('a, _) t -> string
(** [css_name var] is ["--" ^ name var], the custom property's full name, as
    [transition-property] needs it: [css_name gradient_from_var] is
    ["--tw-gradient-from"]. *)

val needs_property_rule : 'a Css.var -> bool
(** [needs_property_rule v] is [true] if [v]'s underlying Var.t has property
    metadata. *)

val order_of_declaration : Css.declaration -> (int * int) option
(** [order_of_declaration d] is the theme-layer slot of the custom declaration
    [d], if it has one. *)

val is_runtime_declaration : Css.declaration -> bool
(** [is_runtime_declaration d] is [true] when [d] belongs to a theme variable
    created with [~runtime:true]. *)

val property_initial_declaration : Css.property_info -> Css.declaration
(** [property_initial_declaration info] is the declaration that sets a parsed
    [@property] statement's typed initial value in the properties layer's bulk
    rule (e.g. [--tw-ring-offset-width: 0px]). *)

(** {1 Bracket Variable References}

    For user-supplied CSS variable names from bracket notation like
    [bg-[var(--my-color)]]. These are variables whose names come from user
    input, not from our typed variable system. *)

val bracket : ?fallback:'a Css.fallback -> string -> 'a Css.var
(** [bracket ~fallback name] is a reference to the CSS variable a bracket names.
    [name] may be the bare variable name without [--], the inner arguments
    retained by {!Parse.extract_var_name}, or a complete [var()] expression. A
    fallback in the expression is preserved. *)
