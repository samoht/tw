(** Color conversion utilities for Tailwind v4 compatibility *)

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

val to_css : color -> int -> Css.color
(** [to_css color shade] converts a color to CSS color value. *)

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
(** [to_oklch color shade] converts color to OKLCH data for a given shade. *)

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

val is_shadeless : color -> bool
(** [is_shadeless color] checks if a color should NOT have a shade suffix in
    class names (base colors, custom colors, or theme-named colors). *)

val color_var : color -> int -> Css.color Var.theme
(** [color_var color shade] gets or creates a memoized color variable for the
    given color and shade. *)

val property_color_var :
  property_prefix:string -> color -> int -> Css.color Var.theme
(** [property_color_var ~property_prefix color shade] gets or creates a
    property-scoped color variable (e.g., [--border-color-blue-500]). *)

val property_color_value : property_prefix:string -> color -> int -> Css.color
(** [property_color_value ~property_prefix color shade] returns the CSS color
    value for a property-scoped color variable. *)

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

val current_scheme : unit -> Scheme.t
(** [current_scheme ()] returns the current color scheme. *)

val color_mix_supports_condition : Css.Supports.t
(** [color_mix_supports_condition] is the CSS supports condition for color-mix:
    [(color: color-mix(in lab, red, red))]. *)

val opacity_fallback_for_theme_value :
  string -> string -> Css.percentage Css.fallback
(** [opacity_fallback_for_theme_value var_name bare] determines the appropriate
    fallback for an opacity theme variable. *)

(** {1 Tailwind Colors} *)

(** Predefined Tailwind v4 color values *)
module Tailwind : sig
  val get_color : string -> int -> string option
  (** [get_color name shade] gets a Tailwind color value in OKLCH format. *)
end

(** {1 Color Application Utilities} *)

val bg : ?opacity:int -> ?shade:int -> color -> t
(** [bg color] sets the background color. [shade] defaults to 500. [opacity]
    sets the alpha modifier (0-100), e.g. [bg ~opacity:50 white]. *)

val bg_transparent : t
(** [bg_transparent] makes the background fully transparent. *)

val bg_current : t
(** [bg_current] uses [currentColor] for the background. *)

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

type opacity_modifier =
  | No_opacity
  | Opacity_percent of float  (** e.g., /50 means 50% *)
  | Opacity_arbitrary of float  (** e.g., /[0.5] means 0.5 *)
  | Opacity_bracket_percent of float
      (** e.g., /[50%] means 50% but preserves bracket form in class name *)
  | Opacity_named of string  (** e.g., /half, /custom - theme-defined names *)
  | Opacity_var of string
      (** e.g., /[var(--x)] - var ref used directly as percentage *)

val parse_opacity_modifier : string -> string * opacity_modifier
(** [parse_opacity_modifier s] parses an opacity modifier from a string. Returns
    the base string and the opacity modifier. Example: "500/50" -> ("500",
    Opacity_percent 50.0). *)

val shade_of_strings : string list -> (color * int, [ `Msg of string ]) result
(** [shade_of_strings parts] parses a color and shade from a list of strings.
    Example: ["blue"; "500"] -> Ok (Blue, 500). *)

val shade_and_opacity_of_strings :
  string list -> (color * int * opacity_modifier, [ `Msg of string ]) result
(** [shade_and_opacity_of_strings parts] parses a color, shade, and optional
    opacity modifier from a list of strings. Example: ["blue"; "500/50"] -> Ok
    (Blue, 500, Opacity_percent 50.0). *)

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

  val set_scheme : Scheme.t -> unit
  (** Set the current scheme for color generation. When a color is defined as
      hex in the scheme, opacity modifiers will use hex+alpha fallback instead
      of color-mix. *)
end

(** {1 Color with Opacity Helpers}

    These functions generate scheme-aware color styles with progressive
    enhancement. They produce a fallback declaration plus a [\@supports] block
    for color-mix. *)

val fill_with_opacity : color -> int -> opacity_modifier -> Style.t
(** [fill_with_opacity color shade opacity] generates fill style with opacity.
*)

val stroke_with_opacity : color -> int -> opacity_modifier -> Style.t
(** [stroke_with_opacity color shade opacity] generates stroke style with
    opacity. *)

val fill_current_with_opacity : opacity_modifier -> Style.t
(** [fill_current_with_opacity opacity] generates fill currentColor with
    opacity. *)

val stroke_current_with_opacity : opacity_modifier -> Style.t
(** [stroke_current_with_opacity opacity] generates stroke currentColor with
    opacity. *)

val divide_with_opacity :
  color -> int -> opacity_modifier -> Css.Selector.t -> Style.t
(** [divide_with_opacity color shade opacity selector] generates divide
    border-color with opacity using the given selector. *)

val divide_current_with_opacity : opacity_modifier -> Css.Selector.t -> Style.t
(** [divide_current_with_opacity opacity selector] generates divide currentColor
    with opacity using the given selector. *)

val opacity_to_percent : opacity_modifier -> float
(** [opacity_to_percent modifier] returns the opacity as a float percentage. *)

val pp_opacity : opacity_modifier -> string
(** [pp_opacity modifier] returns a string representation of the opacity
    modifier for use in class names. E.g., Opacity_percent 50. -> "50",
    Opacity_arbitrary 0.5 -> "[0.5]". *)

val hex_alpha_color : color -> int -> opacity_modifier -> string option
(** [hex_alpha_color color shade opacity] returns a hex color with alpha if the
    color is defined in the scheme, otherwise None. This is useful for
    properties where Tailwind outputs simple hex+alpha without [@supports]. *)

val bg_with_opacity : color -> int -> opacity_modifier -> Style.t
(** [bg_with_opacity color shade opacity] generates background-color style with
    opacity. Scheme-aware: uses hex+alpha fallback with theme variable in
    [\@supports] block. *)

val bg_current_with_opacity : opacity_modifier -> Style.t
(** [bg_current_with_opacity opacity] generates background-color currentColor
    with opacity using color-mix progressive enhancement. *)

val rgb_to_oklab : rgb -> float * float * float
(** [rgb_to_oklab rgb] converts RGB to OKLab (L, a, b) components. *)

val shorten_hex_str : string -> string
(** [shorten_hex_str hex] shortens a hex color string if possible. *)

val bracket_color_to_custom : string -> color
(** [bracket_color_to_custom inner] converts a bracket color string to a custom
    color for opacity handling. *)

val parse_bracket_color : string -> Css.color option
(** [parse_bracket_color inner] parses a bracket color value into a typed
    {!Css.color}. Handles hex, CSS color functions (rgba, hsl, oklch, ...), and
    Tailwind named colors. Returns [None] if not a recognized color. *)

val css_color_to_hex : Css.color -> Css.color option
(** [css_color_to_hex c] converts a typed CSS color (Rgb, Rgba, Hsl) to a hex
    color for Tailwind parity. Returns [None] for color types that cannot be
    easily converted (oklch, oklab, etc.). *)

val round_n : int -> float -> float
(** [round_n n f] rounds [f] to [n] decimal places. *)

val scheme : unit -> Scheme.t
(** [scheme ()] returns the current color scheme reference. *)
