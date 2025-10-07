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

val bg : color -> int -> t
(** [bg color shade] sets the background color using a palette [color] and
    [shade]. *)

val bg_transparent : t
(** [bg_transparent] makes the background fully transparent. *)

val bg_current : t
(** [bg_current] uses [currentColor] for the background. *)

val bg_black : t
(** [bg_black] uses the black palette for background. *)

val bg_white : t
(** [bg_white] uses the white palette for background. *)

val bg_gray : t
(** [bg_gray] uses the gray palette for background. *)

val bg_slate : t
(** [bg_slate] uses the slate palette for background. *)

val bg_zinc : t
(** [bg_zinc] uses the zinc palette for background. *)

val bg_neutral : t
(** [bg_neutral] uses the neutral palette for background. *)

val bg_stone : t
(** [bg_stone] uses the stone palette for background. *)

val bg_red : t
(** [bg_red] uses the red palette for background. *)

val bg_orange : t
(** [bg_orange] uses the orange palette for background. *)

val bg_amber : t
(** [bg_amber] uses the amber palette for background. *)

val bg_yellow : t
(** [bg_yellow] uses the yellow palette for background. *)

val bg_lime : t
(** [bg_lime] uses the lime palette for background. *)

val bg_green : t
(** [bg_green] uses the green palette for background. *)

val bg_emerald : t
(** [bg_emerald] uses the emerald palette for background. *)

val bg_teal : t
(** [bg_teal] uses the teal palette for background. *)

val bg_cyan : t
(** [bg_cyan] uses the cyan palette for background. *)

val bg_sky : t
(** [bg_sky] uses the sky palette for background. *)

val bg_blue : t
(** [bg_blue] uses the blue palette for background. *)

val bg_indigo : t
(** [bg_indigo] uses the indigo palette for background. *)

val bg_violet : t
(** [bg_violet] uses the violet palette for background. *)

val bg_purple : t
(** [bg_purple] uses the purple palette for background. *)

val bg_fuchsia : t
(** [bg_fuchsia] uses the fuchsia palette for background. *)

val bg_pink : t
(** [bg_pink] uses the pink palette for background. *)

val bg_rose : t
(** [bg_rose] uses the rose palette for background. *)

val text : color -> int -> t
(** [text color shade] sets the text color using a palette [color] and [shade].
*)

val text_transparent : t
(** [text_transparent] makes text fully transparent. *)

val text_current : t
(** [text_current] uses [currentColor] for text. *)

val text_inherit : t
(** [text_inherit] inherits text color from parent. *)

val text_black : t
(** [text_black] uses the black palette for text. *)

val text_white : t
(** [text_white] uses the white palette for text. *)

val text_gray : t
(** [text_gray] uses the gray palette for text. *)

val text_slate : t
(** [text_slate] uses the slate palette for text. *)

val text_zinc : t
(** [text_zinc] uses the zinc palette for text. *)

val text_neutral : t
(** [text_neutral] uses the neutral palette for text. *)

val text_stone : t
(** [text_stone] uses the stone palette for text. *)

val text_red : t
(** [text_red] uses the red palette for text. *)

val text_orange : t
(** [text_orange] uses the orange palette for text. *)

val text_amber : t
(** [text_amber] uses the amber palette for text. *)

val text_yellow : t
(** [text_yellow] uses the yellow palette for text. *)

val text_lime : t
(** [text_lime] uses the lime palette for text. *)

val text_green : t
(** [text_green] uses the green palette for text. *)

val text_emerald : t
(** [text_emerald] uses the emerald palette for text. *)

val text_teal : t
(** [text_teal] uses the teal palette for text. *)

val text_cyan : t
(** [text_cyan] uses the cyan palette for text. *)

val text_sky : t
(** [text_sky] uses the sky palette for text. *)

val text_blue : t
(** [text_blue] uses the blue palette for text. *)

val text_indigo : t
(** [text_indigo] uses the indigo palette for text. *)

val text_violet : t
(** [text_violet] uses the violet palette for text. *)

val text_purple : t
(** [text_purple] uses the purple palette for text. *)

val text_fuchsia : t
(** [text_fuchsia] uses the fuchsia palette for text. *)

val text_pink : t
(** [text_pink] uses the pink palette for text. *)

val text_rose : t
(** [text_rose] uses the rose palette for text. *)

val border_color : color -> int -> t
(** [border_color color shade] sets the border color using a palette [color] and
    [shade]. *)

val border_transparent : t
(** [border_transparent] makes the border fully transparent. *)

val border_current : t
(** [border_current] uses [currentColor] for border color. *)

val border_black : t
(** [border_black] uses the black palette for border color. *)

val border_white : t
(** [border_white] uses the white palette for border color. *)

val border_gray : t
(** [border_gray] uses the gray palette for border color. *)

val border_slate : t
(** [border_slate] uses the slate palette for border color. *)

val border_zinc : t
(** [border_zinc] uses the zinc palette for border color. *)

val border_neutral : t
(** [border_neutral] uses the neutral palette for border color. *)

val border_stone : t
(** [border_stone] uses the stone palette for border color. *)

val border_red : t
(** [border_red] uses the red palette for border color. *)

val border_orange : t
(** [border_orange] uses the orange palette for border color. *)

val border_amber : t
(** [border_amber] uses the amber palette for border color. *)

val border_yellow : t
(** [border_yellow] uses the yellow palette for border color. *)

val border_lime : t
(** [border_lime] uses the lime palette for border color. *)

val border_green : t
(** [border_green] uses the green palette for border color. *)

val border_emerald : t
(** [border_emerald] uses the emerald palette for border color. *)

val border_teal : t
(** [border_teal] uses the teal palette for border color. *)

val border_cyan : t
(** [border_cyan] uses the cyan palette for border color. *)

val border_sky : t
(** [border_sky] uses the sky palette for border color. *)

val border_blue : t
(** [border_blue] uses the blue palette for border color. *)

val border_indigo : t
(** [border_indigo] uses the indigo palette for border color. *)

val border_violet : t
(** [border_violet] uses the violet palette for border color. *)

val border_purple : t
(** [border_purple] uses the purple palette for border color. *)

val border_fuchsia : t
(** [border_fuchsia] uses the fuchsia palette for border color. *)

val border_pink : t
(** [border_pink] uses the pink palette for border color. *)

val border_rose : t
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

module Handler : Utility.Handler
