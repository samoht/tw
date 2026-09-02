(** Color conversion utilities for Tailwind v4 compatibility *)

open Cascade

type rgb = {
  r : int;  (** Red channel (0-255) *)
  g : int;  (** Green channel (0-255) *)
  b : int;  (** Blue channel (0-255) *)
}

type oklch = {
  l : float;  (** Lightness (0-100) *)
  c : float;  (** Chroma (0-0.4+) *)
  h : float;  (** Hue (0-360) *)
}

type color =
  | Black
  | White
  | Gray
  | Slate
  | Zinc
  | Neutral
  | Stone
  | Mauve
  | Olive
  | Mist
  | Taupe
  | Red
  | Orange
  | Amber
  | Yellow
  | Lime
  | Green
  | Emerald
  | Teal
  | Cyan
  | Sky
  | Blue
  | Indigo
  | Violet
  | Purple
  | Fuchsia
  | Pink
  | Rose
  | Hex of string
  | Rgb of { red : int; green : int; blue : int }
  | Oklch of oklch
  | Css of Css.color
      (** Arbitrary CSS color function (rgba, hsl, oklch, ...) *)
  | Theme_named of string

open Utility

val pp : color -> string
(** [pp color] pretty-prints a color. *)

(** {1 Conversion Functions} *)

val rgb_to_oklch : rgb -> oklch
(** [rgb_to_oklch rgb] converts RGB color to OKLCH color space. *)

val oklch_to_rgb : oklch -> rgb
(** [oklch_to_rgb oklch] converts OKLCH color to RGB color space. *)

val hex_to_rgb : string -> rgb option
(** [hex_to_rgb hex] parses hex color string to RGB. *)

val rgb_to_hex : rgb -> string
(** [rgb_to_hex rgb] converts RGB to hex string. *)

val oklch_to_css : oklch -> string
(** [oklch_to_css oklch] formats OKLCH for CSS. *)

val to_css : ?theme:Scheme.t -> color -> int -> Css.color
(** [to_css ?theme color shade] converts a color to CSS color value. A project
    token declared in an [\@theme] block has no palette entry, so [theme]
    supplies its value; without one such a colour reads as transparent. *)

(** {1 Tailwind Colors} *)

(** {1 Color Constructors} *)

val black : color
(** [black] is the black color (0, 0, 0). *)

val white : color
(** [white] is the white color (255, 255, 255). *)

val gray : color
(** [gray] is the base gray color. *)

val slate : color
(** [slate] is the base slate color. *)

val zinc : color
(** [zinc] is the base zinc color. *)

val neutral : color
(** [neutral] is the base neutral color. *)

val stone : color
(** [stone] is the base stone color. *)

val mauve : color
(** [mauve] is the base mauve color. *)

val olive : color
(** [olive] is the base olive color. *)

val mist : color
(** [mist] is the base mist color. *)

val taupe : color
(** [taupe] is the base taupe color. *)

val red : color
(** [red] is the base red color. *)

val orange : color
(** [orange] is the base orange color. *)

val amber : color
(** [amber] is the base amber color. *)

val yellow : color
(** [yellow] is the base yellow color. *)

val lime : color
(** [lime] is the base lime color. *)

val green : color
(** [green] is the base green color. *)

val emerald : color
(** [emerald] is the base emerald color. *)

val teal : color
(** [teal] is the base teal color. *)

val cyan : color
(** [cyan] is the base cyan color. *)

val sky : color
(** [sky] is the base sky color. *)

val blue : color
(** [blue] is the base blue color. *)

val indigo : color
(** [indigo] is the base indigo color. *)

val violet : color
(** [violet] is the base violet color. *)

val purple : color
(** [purple] is the base purple color. *)

val fuchsia : color
(** [fuchsia] is the base fuchsia color. *)

val pink : color
(** [pink] is the base pink color. *)

val rose : color
(** [rose] is the base rose color. *)

val hex : string -> color
(** [hex s] creates color from hex string. *)

val rgb : int -> int -> int -> color
(** [rgb r g b] creates color from RGB values. *)

val of_string_exn : string -> color
(** [of_string_exn name] converts a color name string to a color type. Raises
    Failure if unknown color. *)

val of_string : string -> (color, [ `Msg of string ]) result
(** [of_string name] converts a color name string to a color type, returning a
    Result. *)

(** {1 Color Conversion} *)

val to_oklch : color -> int -> oklch
(** [to_oklch color shade] converts color to OKLCH data for a given shade. A
    colour the palette does not define reads as black; use {!to_oklch_opt} to
    tell that case apart. *)

val to_oklch_opt : color -> int -> oklch option
(** [to_oklch_opt color shade] is the OKLCH data for [color] at [shade], or
    [None] for a colour the palette does not define - a project token, whose
    value only the theme knows. *)

val to_oklch_css : color -> int -> string
(** [to_oklch_css color shade] converts color to OKLCH CSS string for a given
    shade. *)

val to_name : color -> string
(** [to_name color] gets the name of a color as a string. *)

val color_to_string : color -> string
(** [color_to_string color] converts a color to its string representation for
    class names. For hex colors, includes the # prefix in the arbitrary value
    syntax (e.g., "[#0f0]"). *)

val is_base_color : color -> bool
(** [is_base_color color] checks if a color is black or white (doesn't need a
    shade). *)

val is_custom_color : color -> bool
(** [is_custom_color color] checks if a color is a custom color (hex or rgb). *)

val opacity_keyword : color -> Css.color option
(** [opacity_keyword color] returns [transparent] or [inherit] when [color] is
    one of the CSS-wide keywords accepted by opacity-bearing colour utilities.
*)

val is_shadeless : color -> bool
(** [is_shadeless color] checks if a color should NOT have a shade suffix in
    class names (base colors, custom colors, or theme-named colors). *)

val is_valid_shade : color -> int -> bool
(** [is_valid_shade color shade] checks that [shade] is one the Tailwind palette
    defines for [color]. Shadeless colors accept any shade (it is ignored). *)

val check_shade : utility:string -> color -> int -> unit
(** [check_shade ~utility color shade] raises [Invalid_argument] if
    [is_valid_shade color shade] is false. [utility] names the constructor in
    the error message. *)

val color_var : color -> int -> Css.color Var.theme
(** [color_var color shade] gets or creates a memoized color variable for the
    given color and shade. *)

val property_color_var :
  ?theme:Scheme.t ->
  property_prefix:string ->
  color ->
  int ->
  Css.color Var.theme
(** [property_color_var ?theme ~property_prefix color shade] gets or creates a
    property-scoped color variable (e.g., [--border-color-blue-500]), checking
    [theme] for a property-scoped token override when given. *)

val property_color_value :
  ?theme:Scheme.t -> property_prefix:string -> color -> int -> Css.color
(** [property_color_value ?theme ~property_prefix color shade] returns the CSS
    color value for a property-scoped color variable, reading scheme colors from
    [theme] when given (default: the current global scheme). *)

val scheme_color_name : color -> int -> string
(** [scheme_color_name color shade] returns the scheme color name (e.g.,
    "red-500") for a color and shade. *)

val hex_with_alpha : string -> float -> string
(** [hex_with_alpha hex_str opacity_percent] adds alpha to a hex color string.
    Returns #RRGGBBAA format. The opacity is a percentage (0-100). *)

val hex_to_oklab_alpha : string -> float -> Css.color
(** [hex_to_oklab_alpha hex alpha] converts a hex color to an oklab CSS color
    with the given alpha (0.0-1.0). Used for bracket hex colors with opacity
    where the color is known at compile time. *)

val color_mix_supports_condition : Css.Supports.t
(** [color_mix_supports_condition] is the CSS supports condition for color-mix:
    [(color: color-mix(in lab, red, red))]. *)

val opacity_fallback_for_theme_value :
  ?theme:Scheme.t -> string -> string -> Css.percentage Css.fallback
(** [opacity_fallback_for_theme_value ?theme var_name bare] determines the
    appropriate fallback for an opacity theme variable, reading token overrides
    from [theme] when given. *)

(** {1 Tailwind Colors} *)

(** Predefined Tailwind v4 color values *)
module Tailwind : sig
  val get_color : string -> int -> string option
  (** [get_color name shade] gets a Tailwind color value in OKLCH format. *)
end

(** {1 Color Application Utilities} *)

val text : ?opacity:int -> ?shade:int -> color -> t
(** [text color] sets the text color. [shade] defaults to 500. [opacity] sets
    the alpha modifier (0-100), e.g. [text ~opacity:50 red]. *)

val text_transparent : t
(** [text_transparent] makes text fully transparent. *)

val text_current : t
(** [text_current] uses [currentColor] for text. *)

val text_inherit : t
(** [text_inherit] inherits text color from parent. *)

val border_color : ?opacity:int -> ?shade:int -> color -> t
(** [border_color color] sets the border color. [shade] defaults to 500.
    [opacity] sets the alpha modifier (0-100), e.g.
    [border_color ~opacity:5 white]. *)

val border_transparent : t
(** [border_transparent] makes the border fully transparent. *)

val border_current : t
(** [border_current] uses [currentColor] for border color. *)

val outline_color : ?opacity:int -> ?shade:int -> color -> t
(** [outline_color color] sets the outline color. [shade] defaults to 500 and
    [opacity] sets the alpha modifier (0-100). *)

val outline_transparent : t
(** [outline_transparent] makes the outline fully transparent. *)

val outline_current : t
(** [outline_current] uses [currentColor] for the outline. *)

val outline_inherit : t
(** [outline_inherit] inherits the outline color. *)

val accent : ?opacity:int -> ?shade:int -> color -> t
(** [accent color] sets the accent color for form controls. [shade] defaults to
    500. [opacity] sets the alpha modifier (0-100). *)

val accent_current : t
(** [accent_current] sets accent color to currentColor. *)

val accent_inherit : t
(** [accent_inherit] sets accent color to inherit. *)

val caret : ?opacity:int -> ?shade:int -> color -> t
(** [caret color] sets the caret color for text input elements. [shade] defaults
    to 500. [opacity] sets the alpha modifier (0-100). *)

val caret_current : t
(** [caret_current] sets caret color to currentColor. *)

val caret_inherit : t
(** [caret_inherit] sets caret color to inherit. *)

val caret_transparent : t
(** [caret_transparent] sets caret color to transparent. *)

(** {1 Opacity Modifiers} *)

type opacity_number = {
  value : float;  (** the number the modifier denotes *)
  text : string;  (** the digits the author wrote, for the class name *)
}
(** A number in an opacity modifier. The class name is a selector, so it repeats
    [text] rather than re-printing [value]: [/[25]] and [/[25.0]] denote one
    alpha and are two different classes. *)

type opacity_modifier =
  | No_opacity
  | Opacity_percent of opacity_number  (** e.g., /50 means 50% *)
  | Opacity_arbitrary of opacity_number  (** e.g., /[0.5] means 0.5 *)
  | Opacity_bracket_percent of opacity_number
      (** e.g., /[50%] means 50% but preserves bracket form in class name *)
  | Opacity_named of string  (** e.g., /half, /custom - theme-defined names *)
  | Opacity_var of string
      (** e.g., /[var(--x)] - var ref used directly as percentage *)

val opacity_of_int : int -> opacity_modifier
(** [opacity_of_int pct] is the modifier a class spells [/pct]. *)

val opacity_var_bare : string -> string
(** [opacity_var_bare v] is the bare custom-property name inside an opacity
    modifier, written either as ["[var(--x)]"] or as the ["(--x)"] shorthand. *)

val opacity_var_bare_of : opacity_modifier -> string option
(** [opacity_var_bare_of opacity] is the custom property the modifier reads its
    percentage from, when it names one rather than giving a number. *)

val mix_alpha :
  ?in_space:Css.color_space -> opacity_modifier -> Css.color -> Css.color
(** [mix_alpha ?in_space opacity color] is [color] with the modifier's alpha
    applied: a percentage folds into the [color-mix], an alpha read from a var
    is referenced by name. *)

val apply_alpha :
  ?in_space:Css.color_space -> opacity_modifier -> Css.color -> Css.color
(** [apply_alpha ?in_space opacity color] applies the modifier while preserving
    the identity that every alpha of [transparent] is still [transparent]. *)

val opacity_fallback :
  ?theme:Scheme.t -> percent:float -> color -> int -> Css.color -> Css.color
(** [opacity_fallback ?theme ~percent c shade value] is what a browser without
    [color-mix()] reads for [c] at [percent] opacity. A palette colour folds the
    alpha into a plain hex; a project token, whose [value] the theme supplies
    and which may name a colour space no hex can spell, takes an sRGB mix
    instead. *)

val opacity_of_string : ?theme:Scheme.t -> string -> opacity_modifier option
(** [opacity_of_string ?theme s] parses the modifier that follows the [/] in a
    colour class: a percentage, a bracket value, the [(--x)] shorthand, or a
    theme-defined name. *)

val parse_opacity_modifier :
  ?theme:Scheme.t -> string -> string * opacity_modifier
(** [parse_opacity_modifier ?theme s] parses an opacity modifier from a string.
    Returns the base string and the opacity modifier. Example: "500/50" ->
    ("500", Opacity_percent 50.0). Named opacities are validated at parse time
    against the [@theme] tokens in [theme]. *)

val shade_of_strings :
  ?theme:Scheme.t -> string list -> (color * int, [ `Msg of string ]) result
(** [shade_of_strings ?theme parts] parses a color and shade from a list of
    strings. Example: ["blue"; "500"] -> Ok (Blue, 500). A name the palette does
    not know resolves against [theme]: a [--color-<name>] token the project
    declared is a shadeless colour of its own. *)

val shade_and_opacity_of_strings :
  ?theme:Scheme.t ->
  string list ->
  (color * int * opacity_modifier, [ `Msg of string ]) result
(** [shade_and_opacity_of_strings ?theme parts] parses a color, shade, and
    optional opacity modifier from a list of strings. Example:
    ["blue"; "500/50"] -> Ok (Blue, 500, Opacity_percent 50.0). *)

val theme_order : string -> int * int
(** [theme_order c] returns the theme layer order for a color variable. *)

val theme_order_with_shade : string -> int -> int * int
(** [theme_order_with_shade c s] returns the theme layer order for a color with
    shade. *)

val utilities_order : string -> int * int
(** [utilities_order c] returns the utilities layer order for conflict
    resolution. *)

val suborder_with_shade : string -> int
(** [suborder_with_shade color_part] extracts the numeric suborder for a color
    utility with shade (e.g., "blue-500" returns 500 + color order offset). Used
    for sorting color utilities within their priority group. *)

module Handler : sig
  include Utility.Handler

  val all_palette_declarations : ?theme:Scheme.t -> unit -> Css.declaration list
  (** [all_palette_declarations ?theme ()] is the [\@layer theme] declaration
      for every colour the palette defines, in theme order. [theme(static)] on
      the package import emits the whole theme, not only the tokens a utility
      used. *)

  val color_binding :
    ?theme:Scheme.t -> color -> int -> Css.declaration * Css.color Css.var
  (** [color_binding ?theme color shade] is the [\@layer theme] declaration for
      the palette token and the typed reference to it, for utilities that set a
      colour var of their own from a palette entry. *)

  val colors_with_opacity_style :
    ?theme:Scheme.t ->
    properties:(Css.color -> Css.declaration) list ->
    ?property_prefix:string ->
    ?merge_key:string ->
    color ->
    int ->
    opacity_modifier ->
    Style.t
  (** [colors_with_opacity_style ~properties color shade opacity] sets every
      declaration in [properties] to [color] at [opacity]: the srgb fallback
      plus the [@supports] colour-mix override Tailwind emits. *)

  val theme_color_of_name : string -> (color * int) option
  (** [theme_color_of_name name] is the palette colour and shade a [color-*]
      token names, or {!constructor-None} when it names none. *)

  val theme_color_decl : ?theme:Scheme.t -> string -> Css.declaration option
  (** [theme_color_decl ?theme name] is the [\@layer theme] declaration for the
      colour token [name] (e.g. ["color-red-500"]), or [None] when [name] is not
      a catalogued colour token. Lets the build emit colour tokens that are only
      referenced via [var()] (e.g. by an arbitrary [color:var(--color-red-500)])
      rather than set by a colour utility. *)
end

(** {1 Color with Opacity Helpers}

    These functions generate scheme-aware color styles with progressive
    enhancement. They produce a fallback declaration plus a [\@supports] block
    for color-mix. *)

val fill_with_opacity :
  ?theme:Scheme.t -> color -> int -> opacity_modifier -> Style.t
(** [fill_with_opacity ?theme color shade opacity] generates fill style with
    opacity, reading scheme colours from [theme] when given. *)

val stroke_with_opacity :
  ?theme:Scheme.t -> color -> int -> opacity_modifier -> Style.t
(** [stroke_with_opacity ?theme color shade opacity] generates stroke style with
    opacity, reading scheme colours from [theme] when given. *)

val fill_current_with_opacity : opacity_modifier -> Style.t
(** [fill_current_with_opacity opacity] generates fill currentColor with
    opacity. *)

val stroke_current_with_opacity : opacity_modifier -> Style.t
(** [stroke_current_with_opacity opacity] generates stroke currentColor with
    opacity. *)

val divide_with_opacity :
  ?theme:Scheme.t ->
  color ->
  int ->
  opacity_modifier ->
  Css.Selector.t ->
  Style.t
(** [divide_with_opacity ?theme color shade opacity selector] generates divide
    border-color with opacity using the given selector. *)

val divide_current_with_opacity : opacity_modifier -> Css.Selector.t -> Style.t
(** [divide_current_with_opacity opacity selector] generates divide currentColor
    with opacity using the given selector. *)

val opacity_to_percent : opacity_modifier -> float
(** [opacity_to_percent modifier] returns the opacity as a float percentage. *)

val pp_opacity : opacity_modifier -> string
(** [pp_opacity modifier] is the class-name spelling of [modifier], without the
    leading ["/"]: ["50"] for [/50], ["[0.5]"] for [/[0.5]], [""] for
    {!No_opacity}. *)

val opacity_suffix : opacity_modifier -> string
(** [opacity_suffix modifier] is {!pp_opacity} behind the ["/"] that separates
    it from the colour, and [""] for {!No_opacity}. *)

val hex_alpha_color :
  ?theme:Scheme.t -> color -> int -> opacity_modifier -> string option
(** [hex_alpha_color ?theme color shade opacity] returns a hex color with alpha
    if the color is defined in the scheme, otherwise None. This is useful for
    properties where Tailwind outputs simple hex+alpha without [@supports]. *)

val bg_with_opacity :
  ?theme:Scheme.t -> color -> int -> opacity_modifier -> Style.t
(** [bg_with_opacity ?theme color shade opacity] generates background-color
    style with opacity. Scheme-aware: uses hex+alpha fallback with theme
    variable in [\@supports] block. *)

val bg_current_with_opacity : ?theme:Scheme.t -> opacity_modifier -> Style.t
(** [bg_current_with_opacity ?theme opacity] generates background-color
    currentColor with opacity using color-mix progressive enhancement. *)

val rgb_to_oklab : rgb -> float * float * float
(** [rgb_to_oklab rgb] converts RGB to OKLab (L, a, b) components. *)

val shorten_hex_str : string -> string
(** [shorten_hex_str hex] shortens a hex color string if possible. *)

val bracket_color_opacity_style :
  ?theme:Scheme.t ->
  ?merge_key:string ->
  property:(Css.color -> Css.declaration) ->
  Css.color ->
  opacity_modifier ->
  Style.t
(** [bracket_color_opacity_style ~property c opacity] sets [property] to the
    bracket colour [c] with [opacity] mixed into it. [c] is the colour the
    bracket was parsed into, so a CSS keyword or colour function keeps its own
    value rather than being read back through the palette. *)

type bracket_opacity =
  | Folded of Css.color
      (** the single value a colour with a hex spelling and a literal alpha
          resolves to *)
  | Guarded of { fallback : Css.color; mixed : Css.color }
      (** the colour itself, for a browser with no [color-mix()], and the mix
          that goes behind an [@supports] guard *)

val bracket_color_opacity :
  ?theme:Scheme.t -> Css.color -> opacity_modifier -> bracket_opacity
(** [bracket_color_opacity c opacity] is what [opacity] makes of the bracket
    colour [c]. It answers values rather than declarations because the
    properties they land on differ by family: a decoration colour writes a
    vendor prefix alongside, and a divide colour hangs both on its child
    selector. *)

val parse_bracket_color : string -> Css.color option
(** [parse_bracket_color inner] parses a bracket color value into a typed
    {!Css.color}. Handles hex, CSS color functions (rgba, hsl, oklch, ...), and
    Tailwind named colors. Returns [None] if not a recognized color. *)

(** What a bracket value names, once the [color:]/[var(] spellings are told
    apart from a plain color. Every color-bearing utility (text, outline, ring,
    shadow, fill, stroke, ...) classifies its bracket content this way; only the
    variant it stores the result in differs. *)
type bracket_hint =
  | Typed_var of string  (** [color:<value>], the part after [color:] *)
  | Bare_var of string  (** [var(--x)], the full [var(...)] text *)
  | Plain_color of Css.color  (** any other color spelling *)

val parse_bracket_hint : string -> bracket_hint option
(** [parse_bracket_hint inner] classifies a bracket's inner text as a typed var,
    a bare var, or a plain color parsed via {!parse_bracket_color}. Returns
    [None] when [inner] is none of these. *)

val css_color_to_hex : Css.color -> Css.color option
(** [css_color_to_hex c] converts a typed CSS color (Rgb, Rgba, Hsl) to a hex
    color for Tailwind parity. Returns [None] for color types that cannot be
    easily converted (oklch, oklab, etc.). *)

val resolve_bracket_css_color : Css.color -> Css.color
(** [resolve_bracket_css_color c] returns the compact emission form of an
    arbitrary colour, folding static RGB/HSL spellings to hex when possible. *)

val pre_color_mix_fallback : Scheme.t -> Css.color -> Css.color option
(** [pre_color_mix_fallback theme c] is Tailwind's legacy value for an arbitrary
    [color-mix()] containing dynamic colour operands. Theme variables are
    resolved into an sRGB mix. If an operand cannot be resolved at compile time,
    the first colour operand is used. [None] means no guard is needed. *)

val bracket_color_style :
  ?merge_key:string ->
  theme:Scheme.t ->
  property:(Css.color -> Css.declaration) ->
  Css.color ->
  Style.t
(** [bracket_color_style ~theme ~property c] emits [c], preceded by its
    pre-[color-mix()] fallback and followed by a guarded enhancement when the
    arbitrary colour needs progressive enhancement. *)

val round_n : int -> float -> float
(** [round_n n f] rounds [f] to [n] decimal places. *)
