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
(** Background color utilities *)

val bg_transparent : Core.t
val bg_current : Core.t
val bg_black : Core.t
val bg_white : Core.t
val bg_gray : Core.t
val bg_slate : Core.t
val bg_zinc : Core.t
val bg_neutral : Core.t
val bg_stone : Core.t
val bg_red : Core.t
val bg_orange : Core.t
val bg_amber : Core.t
val bg_yellow : Core.t
val bg_lime : Core.t
val bg_green : Core.t
val bg_emerald : Core.t
val bg_teal : Core.t
val bg_cyan : Core.t
val bg_sky : Core.t
val bg_blue : Core.t
val bg_indigo : Core.t
val bg_violet : Core.t
val bg_purple : Core.t
val bg_fuchsia : Core.t
val bg_pink : Core.t
val bg_rose : Core.t

val text : t -> int -> Core.t
(** Text color utilities *)

val text_transparent : Core.t
val text_current : Core.t
val text_black : Core.t
val text_white : Core.t
val text_gray : Core.t
val text_slate : Core.t
val text_zinc : Core.t
val text_neutral : Core.t
val text_stone : Core.t
val text_red : Core.t
val text_orange : Core.t
val text_amber : Core.t
val text_yellow : Core.t
val text_lime : Core.t
val text_green : Core.t
val text_emerald : Core.t
val text_teal : Core.t
val text_cyan : Core.t
val text_sky : Core.t
val text_blue : Core.t
val text_indigo : Core.t
val text_violet : Core.t
val text_purple : Core.t
val text_fuchsia : Core.t
val text_pink : Core.t
val text_rose : Core.t

val border_color : t -> int -> Core.t
(** Border color utilities *)

val border_transparent : Core.t
val border_current : Core.t
val border_black : Core.t
val border_white : Core.t
val border_gray : Core.t
val border_slate : Core.t
val border_zinc : Core.t
val border_neutral : Core.t
val border_stone : Core.t
val border_red : Core.t
val border_orange : Core.t
val border_amber : Core.t
val border_yellow : Core.t
val border_lime : Core.t
val border_green : Core.t
val border_emerald : Core.t
val border_teal : Core.t
val border_cyan : Core.t
val border_sky : Core.t
val border_blue : Core.t
val border_indigo : Core.t
val border_violet : Core.t
val border_purple : Core.t
val border_fuchsia : Core.t
val border_pink : Core.t
val border_rose : Core.t

val color_classes_of_string : string list -> (Core.t, [ `Msg of string ]) result
(** Color parsing utilities *)
