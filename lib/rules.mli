(** CSS rule generation and management *)

open Core

(** {1 Types} *)

type output =
  | Regular of {
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
      has_hover : bool;
    }
  | Media_query of {
      condition : string;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
    }
  | Container_query of {
      condition : string;
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
  unit ->
  output
(** [regular ~selector ~props ?base_class ?has_hover ()] constructs a regular
    rule. *)

val media_query :
  condition:string ->
  selector:Css.Selector.t ->
  props:Css.declaration list ->
  ?base_class:string ->
  unit ->
  output
(** [media_query ~condition ~selector ~props ?base_class ()] constructs a media
    query rule. *)

val container_query :
  condition:string ->
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

    - Order: [\@layer properties] → [\@layer theme] → if [base=true] then
      [\@layer base] → [\@layer components] → [\@layer utilities].
    - Properties layer: Emitted only when at least one class registers a custom
      property via [Var.property]. The layer contains a single [\@supports]
      block with Tailwind v4's vendor-targeted condition, wrapping a universal
      selector applying the initial values for each registered property as
      custom declarations (e.g., [--tw-...: initial]). The exact condition used
      is:

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
      [\@supports] block to match browser compatibility behavior.
    - Components layer: Emitted as an empty layer when no components are present
      (renders as [@layer components;]).
    - Utilities layer: Contains sorted utility rules (and any responsive or
      container queries) with conflict resolution to match Tailwind's cascade
      expectations. Hover utilities are gated under [(hover:hover)].
    - [@property] emission: In [Variables] mode, collected [@property]
      registrations (from utilities via [Var.property]) are still emitted at the
      top level after layers. In [Inline] mode, no layers are used and raw rules
      are emitted without [@property] registration changes.

    This behavior ensures compatibility with Tailwind v4's layering and browser
    targeting while keeping output minimal: the properties layer appears only
    when needed, and theme variables are centralized in the theme layer. *)

(** {1 Helper Functions} *)

val extract_selector_props : t -> output list
(** [extract_selector_props tw] extracts CSS rules from a Tailwind class. *)

(** {1 Rule Extraction and Processing} *)

val modifier_to_rule :
  Core.modifier -> string -> Css.Selector.t -> Css.declaration list -> output
(** [modifier_to_rule modifier base_class selector props] converts a modifier
    into appropriate CSS rule output. *)

val extract_selector_props_pairs :
  output list -> (Css.Selector.t * Css.declaration list) list
(** [extract_selector_props_pairs rules] extracts selector/props pairs from
    Regular rules. *)

val is_hover_rule : output -> bool
(** [is_hover_rule output] checks if an output is a hover rule. *)

val rule_sets :
  t list -> Css.rule list * Css.media_rule list * Css.container_rule list
(** [rule_sets tw_classes] processes Tailwind classes into CSS rule sets. *)

val classify : output list -> by_type
(** [classify rules] classifies rules by their type. *)

(** {1 Variable Resolution} *)

(** {1 Conflict Resolution} *)

val conflict_group : string -> int * int
(** [conflict_group selector] returns the conflict group priority for utility
    ordering. *)

val color_order : string -> int
(** [color_order color] returns the ordering priority for color names. *)

(** {1 Layer Generation} *)

val compute_theme_layer : t list -> Css.layer_rule
(** [compute_theme_layer tw_classes] generates the theme layer with CSS
    variables. *)

val build_utilities_layer :
  rules:Css.rule list ->
  media_queries:Css.media_rule list ->
  container_queries:Css.container_rule list ->
  Css.layer_rule
(** [build_utilities_layer ~rules ~media_queries ~container_queries] builds the
    utilities layer with proper conflict ordering. *)

(** {1 Utility Functions} *)

val escape_class_name : string -> string
(** [escape_class_name name] escapes special characters in CSS class names. *)

val string_of_breakpoint : breakpoint -> string
(** [string_of_breakpoint bp] converts a breakpoint to its string
    representation. *)

val responsive_breakpoint : string -> string
(** [responsive_breakpoint prefix] returns the CSS breakpoint value for a
    prefix. *)

val rules_of_grouped :
  ?filter_custom_props:bool ->
  (Css.Selector.t * Css.declaration list) list ->
  Css.rule list
(** [rules_of_grouped grouped_pairs] converts selector/properties pairs to CSS
    rules. Used for testing the rule generation pipeline. *)
