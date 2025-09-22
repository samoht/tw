(** Typed CSS variable definitions and CSS generation architecture.

    This module defines the typed set of CSS custom properties used by the
    library and their corresponding CSS output shapes. Variables are emitted
    across multiple CSS layers following Tailwind v4's architecture.

    {1 CSS Output Architecture}

    The variable system generates CSS across four layers in this order:

    {2 [@layer properties]}
    Contains initial values for utility variables when [Var.property] is used:
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
    Contains theme design tokens from [Var.theme]:
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
    Type registrations for utility variables (at the end):
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

    {2 Theme Variables ([Var.theme])}
    Design tokens that hold actual values. Generated in [@layer theme]:
    - [--font-weight-thin: 100]
    - [--text-xl: 1.25rem]
    - [--color-blue-500: #3b82f6]

    {2 Utility Variables ([Var.utility] and [Var.property])}
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

    {1 Property Registration}

    When [Var.property] is used with initial values: 1. Creates
    [[@layer properties]] default: [--tw-shadow: 0 0 #0000] 2. Creates
    [[@property]] registration with proper syntax and initial-value 3. Enables
    CSS transitions, animations, and proper cascade behavior

    The [syntax] parameter defaults to ["*"] (Universal) when not specified.

    {1 API Functions}

    - [Var.theme]: Creates theme variables ([[@layer theme]])
    - [Var.utility]: Creates utility variables (inline in utility classes)
    - [Var.property]: Creates property registration + [[@layer properties]]
      defaults
    - [Var.handle]: References variables without defining them *)

(** Layer classification for CSS variables. In v4, variables live in [\@layer]
    theme or inline within utilities; base/properties are not used for
    Var-defined variables. *)
type layer = Theme | Utility

(** CSS variable type as a GADT for type safety *)
type _ t =
  (* Design tokens first *)
  | Font_sans : Css.font_family list t
  | Font_mono : Css.font_family list t
  (* Colors *)
  | Color :
      string * int option
      -> Css.color t (* e.g., Color ("blue", Some 500) *)
  | Spacing : Css.length t
  | Default_font_family : Css.font_family list t
  | Default_mono_font_family : Css.font_family list t
  | Default_font_feature_settings : Css.font_feature_settings t
  | Default_font_variation_settings : Css.font_variation_settings t
  | Default_mono_font_feature_settings : Css.font_feature_settings t
  | Default_mono_font_variation_settings : Css.font_variation_settings t
  | Font_serif : Css.font_family list t
  (* Typography scale *)
  | Text_xs : Css.length t
  | Text_xs_line_height : Css.line_height t
  | Text_sm : Css.length t
  | Text_sm_line_height : Css.line_height t
  | Text_base : Css.length t
  | Text_base_line_height : Css.line_height t
  | Text_lg : Css.length t
  | Text_lg_line_height : Css.line_height t
  | Text_xl : Css.length t
  | Text_xl_line_height : Css.line_height t
  | Text_2xl : Css.length t
  | Text_2xl_line_height : Css.line_height t
  | Text_3xl : Css.length t
  | Text_3xl_line_height : Css.line_height t
  | Text_4xl : Css.length t
  | Text_4xl_line_height : Css.line_height t
  | Text_5xl : Css.length t
  | Text_5xl_line_height : Css.line_height t
  | Text_6xl : Css.length t
  | Text_6xl_line_height : Css.line_height t
  | Text_7xl : Css.length t
  | Text_7xl_line_height : Css.line_height t
  | Text_8xl : Css.length t
  | Text_8xl_line_height : Css.line_height t
  | Text_9xl : Css.length t
  | Text_9xl_line_height : Css.line_height t
  (* Font weights *)
  | Font_weight_thin : Css.font_weight t
  | Font_weight_extralight : Css.font_weight t
  | Font_weight_light : Css.font_weight t
  | Font_weight_normal : Css.font_weight t
  | Font_weight_medium : Css.font_weight t
  | Font_weight_semibold : Css.font_weight t
  | Font_weight_bold : Css.font_weight t
  | Font_weight_extrabold : Css.font_weight t
  | Font_weight_black : Css.font_weight t
  | Font_weight : Css.font_weight t
  | Leading : Css.line_height t
  (* Border radius *)
  | Radius_none : Css.length t
  | Radius_sm : Css.length t
  | Radius_default : Css.length t
  | Radius_md : Css.length t
  | Radius_lg : Css.length t
  | Radius_xl : Css.length t
  | Radius_2xl : Css.length t
  | Radius_3xl : Css.length t
  (* Transform variables *)
  | Translate_x : Css.length t
  | Translate_y : Css.length t
  | Translate_z : Css.length t
  | Rotate : Css.angle t
  | Skew_x : Css.angle t
  | Skew_y : Css.angle t
  | Scale_x : float t
  | Scale_y : float t
  | Scale_z : float t
  (* Filter variables *)
  | Blur : Css.length t
  | Brightness : float t
  | Contrast : float t
  | Grayscale : float t
  | Hue_rotate : Css.angle t
  | Invert : float t
  | Saturate : float t
  | Sepia : float t
  | Drop_shadow : string t
  | Drop_shadow_alpha : float t
  (* Box shadow variable *)
  | Box_shadow : Css.shadow list t
  (* Backdrop filter variables *)
  | Backdrop_blur : Css.length t
  | Backdrop_brightness : float t
  | Backdrop_contrast : float t
  | Backdrop_grayscale : float t
  | Backdrop_hue_rotate : Css.angle t
  | Backdrop_invert : float t
  | Backdrop_saturate : float t
  | Backdrop_sepia : float t
  | Backdrop_opacity : float t
  (* Shadow and ring variables *)
  | Single_shadow : Css.shadow t
  | Shadow : Css.box_shadow t
  | Shadow_color : Css.color t
  | Shadow_alpha : Css.percentage t
  | Inset_shadow : Css.shadow t
  | Inset_shadow_color : Css.color t
  | Inset_shadow_alpha : Css.percentage t
  | Ring_color : Css.color t
  | Ring_shadow : Css.shadow t
  | Inset_ring_color : Css.color t
  | Inset_ring_shadow : Css.shadow t
  | Ring_inset : string t
  | Ring_offset_width : Css.length t
  | Ring_offset_color : Css.color t
  | Ring_offset_shadow : Css.shadow t
  | Ring_width : Css.length t
  (* Prose theming variables *)
  | Prose_body : Css.color t
  | Prose_headings : Css.color t
  | Prose_code : Css.color t
  (* Content variable for pseudo-elements *)
  | Content : Css.content t
  | Prose_pre_code : Css.color t
  | Prose_pre_bg : Css.color t
  | Prose_th_borders : Css.color t
  | Prose_td_borders : Css.color t
  | Prose_links : Css.color t
  | Prose_quotes : Css.color t
  | Prose_quote_borders : Css.color t
  | Prose_hr : Css.color t
  | Prose_bold : Css.color t
  | Prose_lead : Css.color t
  | Prose_counters : Css.color t
  | Prose_bullets : Css.color t
  | Prose_captions : Css.color t
  | Prose_kbd : Css.color t
  | Prose_kbd_shadows : string t (* RGB values like "17 24 39" *)
  (* Prose invert variants for dark mode *)
  | Prose_invert_body : Css.color t
  | Prose_invert_headings : Css.color t
  | Prose_invert_lead : Css.color t
  | Prose_invert_links : Css.color t
  | Prose_invert_bold : Css.color t
  | Prose_invert_counters : Css.color t
  | Prose_invert_bullets : Css.color t
  | Prose_invert_hr : Css.color t
  | Prose_invert_quotes : Css.color t
  | Prose_invert_quote_borders : Css.color t
  | Prose_invert_captions : Css.color t
  | Prose_invert_kbd : Css.color t
  | Prose_invert_kbd_shadows : string t
  | Prose_invert_code : Css.color t
  | Prose_invert_pre_code : Css.color t
  | Prose_invert_pre_bg : Css.color t
  | Prose_invert_th_borders : Css.color t
  | Prose_invert_td_borders : Css.color t
  (* Gradient variables *)
  | Gradient_from : Css.color t
  | Gradient_via : Css.color t
  | Gradient_to : Css.color t
  | Gradient_stops : string t
  | Gradient_via_stops : string t
  | Gradient_position : string t
  | Gradient_from_position : Css.percentage t
  | Gradient_via_position : Css.percentage t
  | Gradient_to_position : Css.percentage t
  (* Font variant numeric *)
  | Font_variant_ordinal : Css.font_variant_numeric_token t
  | Font_variant_slashed_zero : Css.font_variant_numeric_token t
  | Font_variant_numeric_figure : Css.font_variant_numeric_token t
  | Font_variant_numeric_spacing : Css.font_variant_numeric_token t
  | Font_variant_numeric_fraction : Css.font_variant_numeric_token t
  | Font_variant_numeric : Css.font_variant_numeric t
  (* Other *)
  | Border_style : Css.border_style t
  | Scroll_snap_strictness : Css.scroll_snap_strictness t
  | Duration : Css.duration t

val name : _ t -> string
(** [name v] returns the variable name without the leading "--". *)

val theme :
  'a t -> ?fallback:'a -> ?property:bool -> 'a -> Css.declaration * 'a Css.var
(** [theme v ?fallback ?property value] creates a theme-layer variable
    declaration and handle. When property is true, indicates this variable needs
    [@property] registration. *)

val utility :
  'a t -> ?fallback:'a -> ?property:bool -> 'a -> Css.declaration * 'a Css.var
(** [utility v ?fallback ?property value] creates a utility-layer variable
    declaration and handle.

    When [property:true], this variable becomes a "property channel" that:
    - Gets initialized in [@layer properties] with [initial] value
    - Gets [@property] registration for proper CSS behavior
    - Is set by each utility class to track which utility was applied

    Example:
    [Var.utility Var.Font_weight ~property:true (Var font_weight_thin_var)]
    creates [--tw-font-weight: var(--font-weight-thin)] which tracks that
    font-thin was applied, even though the actual [font-weight] property uses
    the theme variable directly. *)

val handle_only : 'a t -> unit -> 'a Css.var
(** [handle_only v] creates a variable handle with empty fallback without a
    definition. Useful for referencing variables that may be defined elsewhere.
    Creates var(--name,) format. *)

val handle : 'a t -> ?fallback:'a -> unit -> 'a Css.var
(** [handle v ?fallback] creates a variable handle with optional fallback
    without a definition. Useful for referencing variables that may be defined
    elsewhere. *)

val property :
  'a t -> ?syntax:'a Css.syntax -> ?inherits:bool -> 'a option -> Css.t
(** [property var_t ?initial ?syntax ?inherits] creates a typed [[@property]]
    registration with the appropriate syntax inferred from the variable type.

    - [initial]: Typed initial value (used for both [[@layer properties]] and
      [[@property]])
    - [syntax]: CSS syntax (defaults to Universal when not provided)
    - [inherits]: Whether the property inherits (defaults to false)

    Examples:
    - [property Shadow ~initial:[shadow_value]]
    - [property Shadow_alpha ~initial:(Css.Pct 100.0)]
    - [property Border_style ~initial:Solid] *)

val layer : _ Css.var -> layer option
(** [layer var] returns the layer recorded in the variable metadata, if any. *)

val needs_property : _ Css.var -> bool
(** [needs_property var] returns true if this variable needs property
    declaration. *)

val to_string : _ t -> string
(** [to_string v] returns the full "--var-name" representation. *)

val pp : _ t -> string
(** [pp] is {!to_string}. *)

(** Existential wrapper for variables of any type *)
type any = Any : _ t -> any

(* Utilities own their variable registrations and any @property rules. *)

val compare : any -> any -> int
(** [compare a b] compares two variables for canonical ordering in the theme. *)

val compare_declarations : layer -> Css.declaration -> Css.declaration -> int
(** [compare_declarations layer d1 d2] compares two custom declarations via Var
    metadata for the given layer. *)

val of_meta : Css.meta -> any option
(** [of_meta meta] extracts a variable from CSS metadata if present. *)

module Map : Map.S with type key = any
(** Map with Var.any keys *)

module Set : Set.S with type elt = any
(** Set with Var.any elements *)

module Def : sig
  type 'a property_def = {
    syntax : 'a Css.syntax;
    initial_value : 'a;
    inherits : bool;
  }

  type 'a def = {
    name : 'a t;
    layer : layer;
    property : 'a property_def option;
    initial : 'a option;
    inherits : bool;
  }

  val theme :
    'a t -> ?initial:'a -> ?syntax:'a Css.syntax -> 'a -> 'a def * 'a Css.var
  (** [theme var ?initial ?syntax value] builds a structured variable definition
      for the theme layer and returns it with a typed CSS variable handle. If
      [~initial] or [~syntax] is provided, a corresponding [@property]
      registration can be produced via [to_property_rule]. *)

  val utility :
    'a t -> ?initial:'a -> ?syntax:'a Css.syntax -> 'a -> 'a def * 'a Css.var
  (** [utility var ?initial ?syntax value] builds a structured variable
      definition for the utilities layer and returns it with a typed CSS
      variable handle. If [~initial] or [~syntax] is provided, a corresponding
      [@property] registration can be produced via [to_property_rule]. *)

  val to_property_rule : 'a def -> Css.t option
  (** [to_property_rule def] converts a structured definition to a typed
      [@property] rule if property metadata is present. The rule renders the
      initial-value per spec (e.g., zero lengths as [0]). *)

  val to_initial_declaration : 'a def -> Css.declaration option
  (** [to_initial_declaration def] converts a structured definition to a
      properties-layer default declaration when an initial is present. Rendering
      follows CSS expectations (e.g., zero lengths as [0px]). *)
end
