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

(** {1 Utility Types} *)

type utility

(** {1 Internal Conversion Functions} *)

val to_style : utility -> Style.t
(** [to_style u] converts a structured color utility to a style. For internal
    use by the Tw module. *)

val utility_of_string : string list -> (utility, [ `Msg of string ]) result
(** [utility_of_string parts] parses a color utility from string parts. Returns
    an internal structured representation. *)

val suborder : utility -> int
(** [suborder u] returns the ordering value for color utility [u]. Used for
    deterministic CSS output ordering. *)

val is_background_color : utility -> bool
(** [is_background_color u] returns true if the utility is a background color
    (bg-), false otherwise. Used for priority calculation. *)

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

val is_base_color : color -> bool
(** [is_base_color color] checks if a color is black or white (doesn't need a
    shade). *)

val is_custom_color : color -> bool
(** [is_custom_color color] checks if a color is a custom color (hex or rgb). *)

val get_color_var : color -> int -> Css.color Var.theme
(** [get_color_var color shade] gets or creates a memoized color variable for
    the given color and shade. *)

(** {1 Tailwind Colors} *)

(** Predefined Tailwind v4 color values *)
module Tailwind : sig
  val get_color : string -> int -> string option
  (** [get_color name shade] gets a Tailwind color value in OKLCH format. *)
end

(** {1 Color Application Utilities} *)

val bg : color -> int -> Style.t
(** [bg color shade] sets the background color using a palette [color] and
    [shade]. *)

val bg_transparent : Style.t
(** [bg_transparent] makes the background fully transparent. *)

val bg_current : Style.t
(** [bg_current] uses [currentColor] for the background. *)

val bg_black : Style.t
(** [bg_black] uses the black palette for background. *)

val bg_white : Style.t
(** [bg_white] uses the white palette for background. *)

val bg_gray : Style.t
(** [bg_gray] uses the gray palette for background. *)

val bg_slate : Style.t
(** [bg_slate] uses the slate palette for background. *)

val bg_zinc : Style.t
(** [bg_zinc] uses the zinc palette for background. *)

val bg_neutral : Style.t
(** [bg_neutral] uses the neutral palette for background. *)

val bg_stone : Style.t
(** [bg_stone] uses the stone palette for background. *)

val bg_red : Style.t
(** [bg_red] uses the red palette for background. *)

val bg_orange : Style.t
(** [bg_orange] uses the orange palette for background. *)

val bg_amber : Style.t
(** [bg_amber] uses the amber palette for background. *)

val bg_yellow : Style.t
(** [bg_yellow] uses the yellow palette for background. *)

val bg_lime : Style.t
(** [bg_lime] uses the lime palette for background. *)

val bg_green : Style.t
(** [bg_green] uses the green palette for background. *)

val bg_emerald : Style.t
(** [bg_emerald] uses the emerald palette for background. *)

val bg_teal : Style.t
(** [bg_teal] uses the teal palette for background. *)

val bg_cyan : Style.t
(** [bg_cyan] uses the cyan palette for background. *)

val bg_sky : Style.t
(** [bg_sky] uses the sky palette for background. *)

val bg_blue : Style.t
(** [bg_blue] uses the blue palette for background. *)

val bg_indigo : Style.t
(** [bg_indigo] uses the indigo palette for background. *)

val bg_violet : Style.t
(** [bg_violet] uses the violet palette for background. *)

val bg_purple : Style.t
(** [bg_purple] uses the purple palette for background. *)

val bg_fuchsia : Style.t
(** [bg_fuchsia] uses the fuchsia palette for background. *)

val bg_pink : Style.t
(** [bg_pink] uses the pink palette for background. *)

val bg_rose : Style.t
(** [bg_rose] uses the rose palette for background. *)

val text : color -> int -> Style.t
(** [text color shade] sets the text color using a palette [color] and [shade].
*)

val text_transparent : Style.t
(** [text_transparent] makes text fully transparent. *)

val text_current : Style.t
(** [text_current] uses [currentColor] for text. *)

val text_inherit : Style.t
(** [text_inherit] inherits text color from parent. *)

val text_black : Style.t
(** [text_black] uses the black palette for text. *)

val text_white : Style.t
(** [text_white] uses the white palette for text. *)

val text_gray : Style.t
(** [text_gray] uses the gray palette for text. *)

val text_slate : Style.t
(** [text_slate] uses the slate palette for text. *)

val text_zinc : Style.t
(** [text_zinc] uses the zinc palette for text. *)

val text_neutral : Style.t
(** [text_neutral] uses the neutral palette for text. *)

val text_stone : Style.t
(** [text_stone] uses the stone palette for text. *)

val text_red : Style.t
(** [text_red] uses the red palette for text. *)

val text_orange : Style.t
(** [text_orange] uses the orange palette for text. *)

val text_amber : Style.t
(** [text_amber] uses the amber palette for text. *)

val text_yellow : Style.t
(** [text_yellow] uses the yellow palette for text. *)

val text_lime : Style.t
(** [text_lime] uses the lime palette for text. *)

val text_green : Style.t
(** [text_green] uses the green palette for text. *)

val text_emerald : Style.t
(** [text_emerald] uses the emerald palette for text. *)

val text_teal : Style.t
(** [text_teal] uses the teal palette for text. *)

val text_cyan : Style.t
(** [text_cyan] uses the cyan palette for text. *)

val text_sky : Style.t
(** [text_sky] uses the sky palette for text. *)

val text_blue : Style.t
(** [text_blue] uses the blue palette for text. *)

val text_indigo : Style.t
(** [text_indigo] uses the indigo palette for text. *)

val text_violet : Style.t
(** [text_violet] uses the violet palette for text. *)

val text_purple : Style.t
(** [text_purple] uses the purple palette for text. *)

val text_fuchsia : Style.t
(** [text_fuchsia] uses the fuchsia palette for text. *)

val text_pink : Style.t
(** [text_pink] uses the pink palette for text. *)

val text_rose : Style.t
(** [text_rose] uses the rose palette for text. *)

val border_color : color -> int -> Style.t
(** [border_color color shade] sets the border color using a palette [color] and
    [shade]. *)

val border_transparent : Style.t
(** [border_transparent] makes the border fully transparent. *)

val border_current : Style.t
(** [border_current] uses [currentColor] for border color. *)

val border_black : Style.t
(** [border_black] uses the black palette for border color. *)

val border_white : Style.t
(** [border_white] uses the white palette for border color. *)

val border_gray : Style.t
(** [border_gray] uses the gray palette for border color. *)

val border_slate : Style.t
(** [border_slate] uses the slate palette for border color. *)

val border_zinc : Style.t
(** [border_zinc] uses the zinc palette for border color. *)

val border_neutral : Style.t
(** [border_neutral] uses the neutral palette for border color. *)

val border_stone : Style.t
(** [border_stone] uses the stone palette for border color. *)

val border_red : Style.t
(** [border_red] uses the red palette for border color. *)

val border_orange : Style.t
(** [border_orange] uses the orange palette for border color. *)

val border_amber : Style.t
(** [border_amber] uses the amber palette for border color. *)

val border_yellow : Style.t
(** [border_yellow] uses the yellow palette for border color. *)

val border_lime : Style.t
(** [border_lime] uses the lime palette for border color. *)

val border_green : Style.t
(** [border_green] uses the green palette for border color. *)

val border_emerald : Style.t
(** [border_emerald] uses the emerald palette for border color. *)

val border_teal : Style.t
(** [border_teal] uses the teal palette for border color. *)

val border_cyan : Style.t
(** [border_cyan] uses the cyan palette for border color. *)

val border_sky : Style.t
(** [border_sky] uses the sky palette for border color. *)

val border_blue : Style.t
(** [border_blue] uses the blue palette for border color. *)

val border_indigo : Style.t
(** [border_indigo] uses the indigo palette for border color. *)

val border_violet : Style.t
(** [border_violet] uses the violet palette for border color. *)

val border_purple : Style.t
(** [border_purple] uses the purple palette for border color. *)

val border_fuchsia : Style.t
(** [border_fuchsia] uses the fuchsia palette for border color. *)

val border_pink : Style.t
(** [border_pink] uses the pink palette for border color. *)

val border_rose : Style.t
(** [border_rose] uses the rose palette for border color. *)

val shade_of_strings : string list -> (color * int, [ `Msg of string ]) result
(** [shade_of_strings parts] parses a color and shade from a list of strings.
    Example: ["blue"; "500"] -> Ok (Blue, 500). *)

val theme_order : string -> int * int
(** Get theme layer order for a color variable. See {!Rules} for detailed layer
    ordering rules. *)

val theme_order_with_shade : string -> int -> int * int
(** Get theme layer order for a color variable with shade *)

val utilities_order : string -> int * int
(** Get utilities layer order for conflict resolution. See {!Rules} for detailed
    layer ordering rules. *)

val suborder_with_shade : string -> int
(** [suborder_with_shade color_part] extracts the numeric suborder for a color
    utility with shade (e.g., "blue-500" returns 500 + color order offset). Used
    for sorting color utilities within their priority group. *)
