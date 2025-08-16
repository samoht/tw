(** Color conversion utilities for Tailwind v4 compatibility *)

type rgb = {
  r : int;  (** Red channel (0-255) *)
  g : int;  (** Green channel (0-255) *)
  b : int;  (** Blue channel (0-255) *)
}
(** RGB color representation *)

type oklch = {
  l : float;  (** Lightness (0-100) *)
  c : float;  (** Chroma (0-0.4+) *)
  h : float;  (** Hue (0-360) *)
}
(** OKLCH color representation *)

(** Color type *)
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
  | Oklch of oklch (* OKLCH as a primary color type *)

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

val of_string : string -> t
(** [of_string name] converts a color name string to a color type. *)

(** {1 Color Conversion} *)

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
