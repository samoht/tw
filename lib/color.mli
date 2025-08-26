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

type t =
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

val pp : t -> string
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

val to_css : t -> int -> Css.color
(** [to_css color shade] converts a color to CSS color value. *)

(** {1 Tailwind Colors} *)

(** {1 Color Constructors} *)

val black : t
(** [black] is the black color (0, 0, 0). *)

val white : t
(** [white] is the white color (255, 255, 255). *)

val gray : t
(** [gray] is the base gray color. *)

val slate : t
(** [slate] is the base slate color. *)

val zinc : t
(** [zinc] is the base zinc color. *)

val neutral : t
(** [neutral] is the base neutral color. *)

val stone : t
(** [stone] is the base stone color. *)

val red : t
(** [red] is the base red color. *)

val orange : t
(** [orange] is the base orange color. *)

val amber : t
(** [amber] is the base amber color. *)

val yellow : t
(** [yellow] is the base yellow color. *)

val lime : t
(** [lime] is the base lime color. *)

val green : t
(** [green] is the base green color. *)

val emerald : t
(** [emerald] is the base emerald color. *)

val teal : t
(** [teal] is the base teal color. *)

val cyan : t
(** [cyan] is the base cyan color. *)

val sky : t
(** [sky] is the base sky color. *)

val blue : t
(** [blue] is the base blue color. *)

val indigo : t
(** [indigo] is the base indigo color. *)

val violet : t
(** [violet] is the base violet color. *)

val purple : t
(** [purple] is the base purple color. *)

val fuchsia : t
(** [fuchsia] is the base fuchsia color. *)

val pink : t
(** [pink] is the base pink color. *)

val rose : t
(** [rose] is the base rose color. *)

val hex : string -> t
(** [hex s] creates color from hex string. *)

val rgb : int -> int -> int -> t
(** [rgb r g b] creates color from RGB values. *)

val of_string_exn : string -> t
(** [of_string_exn name] converts a color name string to a color type. Raises
    Failure if unknown color. *)

val of_string : string -> (t, [ `Msg of string ]) result
(** [of_string name] converts a color name string to a color type, returning a
    Result. *)

(** {1 Color Conversion} *)

val to_oklch : t -> int -> oklch
(** [to_oklch color shade] converts color to OKLCH data for a given shade. *)

val to_oklch_css : t -> int -> string
(** [to_oklch_css color shade] converts color to OKLCH CSS string for a given
    shade. *)

val to_name : t -> string
(** [to_name color] gets the name of a color as a string. *)

val is_base_color : t -> bool
(** [is_base_color color] checks if a color is black or white (doesn't need a
    shade). *)

val is_custom_color : t -> bool
(** [is_custom_color color] checks if a color is a custom color (hex or rgb). *)

(** {1 Tailwind Colors} *)

(** Predefined Tailwind v4 color values *)
module TailwindColors : sig
  val get_color : string -> int -> string option
  (** Get a Tailwind color value in OKLCH format *)
end

(** {1 Color Application Utilities} *)

val bg : t -> int -> Core.t
(** [bg color shade] sets the background color using a palette [color] and
    [shade]. *)

val bg_transparent : Core.t
(** Transparent background. *)

val bg_current : Core.t
(** Background using [currentColor]. *)

val bg_black : Core.t
(** Background using the black palette. *)

val bg_white : Core.t
(** Background using the white palette. *)

val bg_gray : Core.t
(** Background using the gray palette. *)

val bg_slate : Core.t
(** Background using the slate palette. *)

val bg_zinc : Core.t
(** Background using the zinc palette. *)

val bg_neutral : Core.t
(** Background using the neutral palette. *)

val bg_stone : Core.t
(** Background using the stone palette. *)

val bg_red : Core.t
(** Background using the red palette. *)

val bg_orange : Core.t
(** Background using the orange palette. *)

val bg_amber : Core.t
(** Background using the amber palette. *)

val bg_yellow : Core.t
(** Background using the yellow palette. *)

val bg_lime : Core.t
(** Background using the lime palette. *)

val bg_green : Core.t
(** Background using the green palette. *)

val bg_emerald : Core.t
(** Background using the emerald palette. *)

val bg_teal : Core.t
(** Background using the teal palette. *)

val bg_cyan : Core.t
(** Background using the cyan palette. *)

val bg_sky : Core.t
(** Background using the sky palette. *)

val bg_blue : Core.t
(** Background using the blue palette. *)

val bg_indigo : Core.t
(** Background using the indigo palette. *)

val bg_violet : Core.t
(** Background using the violet palette. *)

val bg_purple : Core.t
(** Background using the purple palette. *)

val bg_fuchsia : Core.t
(** Background using the fuchsia palette. *)

val bg_pink : Core.t
(** Background using the pink palette. *)

val bg_rose : Core.t
(** Background using the rose palette. *)

val text : t -> int -> Core.t
(** [text color shade] sets the text color using a palette [color] and [shade].
*)

val text_transparent : Core.t
(** Transparent text color. *)

val text_current : Core.t
(** Text using [currentColor]. *)

val text_black : Core.t
(** Text using the black palette. *)

val text_white : Core.t
(** Text using the white palette. *)

val text_gray : Core.t
(** Text using the gray palette. *)

val text_slate : Core.t
(** Text using the slate palette. *)

val text_zinc : Core.t
(** Text using the zinc palette. *)

val text_neutral : Core.t
(** Text using the neutral palette. *)

val text_stone : Core.t
(** Text using the stone palette. *)

val text_red : Core.t
(** Text using the red palette. *)

val text_orange : Core.t
(** Text using the orange palette. *)

val text_amber : Core.t
(** Text using the amber palette. *)

val text_yellow : Core.t
(** Text using the yellow palette. *)

val text_lime : Core.t
(** Text using the lime palette. *)

val text_green : Core.t
(** Text using the green palette. *)

val text_emerald : Core.t
(** Text using the emerald palette. *)

val text_teal : Core.t
(** Text using the teal palette. *)

val text_cyan : Core.t
(** Text using the cyan palette. *)

val text_sky : Core.t
(** Text using the sky palette. *)

val text_blue : Core.t
(** Text using the blue palette. *)

val text_indigo : Core.t
(** Text using the indigo palette. *)

val text_violet : Core.t
(** Text using the violet palette. *)

val text_purple : Core.t
(** Text using the purple palette. *)

val text_fuchsia : Core.t
(** Text using the fuchsia palette. *)

val text_pink : Core.t
(** Text using the pink palette. *)

val text_rose : Core.t
(** Text using the rose palette. *)

val border_color : t -> int -> Core.t
(** [border_color color shade] sets the border color using a palette [color] and
    [shade]. *)

val border_transparent : Core.t
(** Transparent border color. *)

val border_current : Core.t
(** Border color using [currentColor]. *)

val border_black : Core.t
(** Border color using the black palette. *)

val border_white : Core.t
(** Border color using the white palette. *)

val border_gray : Core.t
(** Border color using the gray palette. *)

val border_slate : Core.t
(** Border color using the slate palette. *)

val border_zinc : Core.t
(** Border color using the zinc palette. *)

val border_neutral : Core.t
(** Border color using the neutral palette. *)

val border_stone : Core.t
(** Border color using the stone palette. *)

val border_red : Core.t
(** Border color using the red palette. *)

val border_orange : Core.t
(** Border color using the orange palette. *)

val border_amber : Core.t
(** Border color using the amber palette. *)

val border_yellow : Core.t
(** Border color using the yellow palette. *)

val border_lime : Core.t
(** Border color using the lime palette. *)

val border_green : Core.t
(** Border color using the green palette. *)

val border_emerald : Core.t
(** Border color using the emerald palette. *)

val border_teal : Core.t
(** Border color using the teal palette. *)

val border_cyan : Core.t
(** Border color using the cyan palette. *)

val border_sky : Core.t
(** Border color using the sky palette. *)

val border_blue : Core.t
(** Border color using the blue palette. *)

val border_indigo : Core.t
(** Border color using the indigo palette. *)

val border_violet : Core.t
(** Border color using the violet palette. *)

val border_purple : Core.t
(** Border color using the purple palette. *)

val border_fuchsia : Core.t
(** Border color using the fuchsia palette. *)

val border_pink : Core.t
(** Border color using the pink palette. *)

val border_rose : Core.t
(** Border color using the rose palette. *)

val color_classes_of_string : string list -> (Core.t, [ `Msg of string ]) result
(** [color_classes_of_string parts] parses color utilities from string [parts].
*)
