(** CSS rule generation and management

    This module handles the generation of CSS rules across different layers
    following Tailwind v4's architecture with deterministic ordering.

    {1 Layer-Specific Ordering Rules}

    {2 Theme Layer}
    Order is stable and intentional, not alphabetical. Tokens appear in
    canonical sequence: default fonts → color palette (in specific order) →
    scales. Sorted by metadata (priority, subindex) attached to each variable.

    {2 Properties Layer}
    Two-part structure:
    - Initial values in [@supports] block at layer start
    - [@property] rules appended at stylesheet end (after all layers)

    {2 Utilities Layer}
    Ordered by conflict resolution groups for predictable cascade: display →
    position → margin → background → padding → typography → border → sizing →
    effects → interactivity → flexbox/grid → gap → container/prose

    Within groups, specific suborders apply (e.g., colors by palette order,
    spacing with all → axis → side-specific). *)

open Utility

(** {1 Types} *)

type output =
  | Regular of {
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
      has_hover : bool;
      nested : Css.statement list;
    }
  | Media_query of {
      condition : Css.Media.t;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
      nested : Css.statement list;
    }
  | Container_query of {
      condition : Css.Container.t;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
    }
  | Starting_style of {
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
    }

type by_type = {
  regular : output list;
  media : output list;
  container : output list;
  starting : output list;
}

val regular :
  selector:Css.Selector.t ->
  props:Css.declaration list ->
  ?base_class:string ->
  ?has_hover:bool ->
  ?nested:Css.statement list ->
  unit ->
  output
(** [regular ~selector ~props ?base_class ?has_hover ?nested ()] constructs a
    regular rule with optional nested statements (e.g., {i \@media} queries). *)

val media_query :
  condition:Css.Media.t ->
  selector:Css.Selector.t ->
  props:Css.declaration list ->
  ?base_class:string ->
  ?nested:Css.statement list ->
  unit ->
  output
(** [media_query ~condition ~selector ~props ?base_class ?nested ()] constructs
    a media query rule with optional nested statements (e.g., inner
    {i \@media}). *)

val container_query :
  condition:Css.Container.t ->
  selector:Css.Selector.t ->
  props:Css.declaration list ->
  ?base_class:string ->
  unit ->
  output
(** [container_query ~condition ~selector ~props ?base_class ()] constructs a
    container query rule. *)

val starting_style :
  selector:Css.Selector.t ->
  props:Css.declaration list ->
  ?base_class:string ->
  unit ->
  output
(** [starting_style ~selector ~props ?base_class ()] constructs a starting-style
    rule. *)

(** {1 CSS Generation} *)

type config = {
  base : bool;
      (** Include base styles (Tailwind's Preflight reset and semantic
          defaults). In [Variables] mode this controls whether the [@layer base]
          is emitted. In [Inline] mode this flag has no effect on layering
          (inline mode never emits layers). *)
  mode : Css.mode;
      (** CSS generation mode:
          - [Variables]: Generates CSS with layers (Theme, Base when
            [reset=true], Components, Utilities) and CSS custom properties for
            dynamic theming. CSS variables are emitted as [var(--name)]
            references.
          - [Inline]: Generates raw CSS rules without layers, suitable for
            inline styles or environments without CSS variable support.
            Variables are resolved to their default values when available. *)
  optimize : bool;
      (** Enable CSS optimizations. When true, applies various optimizations:
          - Merges adjacent rules with identical selectors
          - Combines consecutive rules with identical declarations
          - Deduplicates declarations within rules (last wins) Optimizations
            preserve CSS cascade semantics. Default: true *)
}
(** Configuration for CSS generation *)

val default_config : config
(** [default_config] is [{ base = true; mode = Variables }]. Provides layered
    output with Theme, Base, Components, Utilities and CSS variables. *)

val to_css : ?config:config -> t list -> Css.t
(** [to_css ?config classes] generates a CSS stylesheet from Tailwind classes.

    Behavior matrix:
    - [mode = Variables, base = true]: Layers with Theme, Base, Components,
      Utilities; emits [@property] registrations as needed.
    - [mode = Variables, base = false]: Layers with Theme, Components, Utilities
      (no Base layer); emits [@property] registrations as needed.
    - [mode = Inline, _]: No layers; emits raw CSS rules with values resolved.

    @param config Configuration for CSS generation (default: [default_config])
*)

val to_inline_style : t list -> string
(** [to_inline_style styles] generates inline CSS string from Tailwind classes.
*)

(** {2 Tailwind v4 Layer Model}

    The generator follows Tailwind v4's cascade layering model with a pragmatic
    properties layer strategy that mirrors Tailwind's output where applicable.

    - Order: [@layer properties] → [@layer theme] → if [base=true] then
      [@layer base] → [@layer components] → [@layer utilities].
    - Properties layer: Emitted only when at least one class registers a custom
      property via [Var.property_default] (or explicit rules). The layer
      contains a single [@supports] block with Tailwind v4's vendor-targeted
      condition, wrapping a universal selector applying the initial values for
      each registered property as custom declarations (e.g.,
      [--tw-...: initial]). The exact condition used is:

    {v
      (((-webkit-hyphens:none)) and (not (margin-trim:inline))) or
      ((-moz-orient:inline) and (not (color:rgb(from red r g b))))
    v}

    Notes:
    - Property defaults are derived from the collected [@property] rules'
      [initial] values; if empty, "initial" is used.
    - If no properties are registered, the properties layer is omitted.
    - Theme layer: Contains design-token variables extracted from utilities,
      ordered by the canonical variable order from [Var]. Classification uses
      Var metadata only (the declaration's [layer] = "theme" | "utilities").
      Custom declarations without Var metadata are ignored for theme hoisting.
    - Base layer: When [base=true], emits Preflight reset from
      [Preflight.stylesheet ()]. Placeholder opacity handling is gated under a
      [@supports] block to match browser compatibility behavior.
    - Components layer: Emitted as an empty layer when no components are present
      (renders as [@layer components;]).
    - Utilities layer: Contains sorted utility rules (and any responsive or
      container queries) with conflict resolution to match Tailwind's cascade
      expectations. Hover utilities are gated under [(hover:hover)].
    - [@property] emission: In [Variables] mode, collected [@property]
      registrations (from utilities via [Var.property_rule]) are still emitted
      at the top level after layers. In [Inline] mode, no layers are used and
      raw rules are emitted without [@property] registration changes.

    This behavior ensures compatibility with Tailwind v4's layering and browser
    targeting while keeping output minimal: the properties layer appears only
    when needed, and theme variables are centralized in the theme layer. *)

(** {1 Helper Functions} *)

val set_debug_compare : bool -> unit
(** [set_debug_compare true] enables debug output for rule comparison. *)

val outputs : t -> output list
(** [outputs tw] extracts CSS rules from a Tailwind class. *)

val selector_props_pairs :
  output list -> (Css.Selector.t * Css.declaration list * (int * int)) list
(** [selector_props_pairs outputs] converts output list to selector-props-order
    triples. The order tuple is (priority, suborder). Used internally for rule
    processing. Exposed for testing cascade ordering. *)

(** {1 Rule Extraction and Processing} *)

val modifier_to_rule :
  ?inner_has_hover:bool ->
  Style.modifier ->
  string ->
  Css.Selector.t ->
  Css.declaration list ->
  output
(** [modifier_to_rule ?inner_has_hover modifier base_class selector props]
    converts a modifier into appropriate CSS rule output. When [inner_has_hover]
    is true and the modifier is a media-like modifier (dark, motion-safe, etc.),
    the output will use CSS nesting to properly wrap the hover state with
    [@media (hover:hover)]. *)

val is_hover_rule : output -> bool
(** [is_hover_rule output] checks if an output is a hover rule. *)

val rule_sets : t list -> Css.statement list
(** [rule_sets tw_classes] processes Tailwind classes into CSS statements with
    media queries interleaved in the correct order. Consecutive media queries
    with the same condition will be merged by {!build_utilities_layer}. *)

val classify_by_type : output list -> by_type
(** [classify_by_type rules] classifies rules by their type (regular, media,
    container, starting). *)

(** {1 Variable Resolution} *)

(** {1 Conflict Resolution} *)

val conflict_order : string -> int * int
(** [conflict_order selector] returns the (priority, suborder) tuple for utility
    ordering by parsing the selector and delegating to Utility.order. *)

(** {1 Layer Generation} *)

val theme_layer_of : ?default_decls:Css.declaration list -> t list -> Css.t
(** [theme_layer_of ?default_decls tw_classes] generates the theme layer with
    CSS variables referenced in the classes plus any [default_decls] provided
    (e.g., baseline theme tokens like default font families). *)

val build_utilities_layer : statements:Css.statement list -> Css.t
(** [build_utilities_layer ~statements] builds the utilities layer. The
    statements should already be in the correct order with media queries
    interleaved. Consecutive media queries with the same condition will be
    merged to reduce output size while preserving cascade order. *)

(** {1 Utility Functions} *)

val escape_class_name : string -> string
(** [escape_class_name name] escapes special characters in CSS class names. *)

val of_grouped :
  ?filter_custom_props:bool ->
  (Css.Selector.t * Css.declaration list * (int * int)) list ->
  Css.statement list
(** [of_grouped grouped_triples] converts selector/properties/order triples to
    CSS rules. Used for testing the rule generation pipeline. *)
