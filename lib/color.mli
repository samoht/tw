(** Color conversion utilities for Tailwind v4 compatibility *)

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

val hex_to_oklch_css : string -> string
(** [hex_to_oklch_css hex] converts hex color to OKLCH CSS string. *)

(** {1 Tailwind Colors} *)

(** {1 Color Constructors} *)

val of_string : string -> t
(** [of_string s] converts string name to color type. *)

val black : t
val white : t
val gray : t
val slate : t
val zinc : t
val neutral : t
val stone : t
val red : t
val orange : t
val amber : t
val yellow : t
val lime : t
val green : t
val emerald : t
val teal : t
val cyan : t
val sky : t
val blue : t
val indigo : t
val violet : t
val purple : t
val fuchsia : t
val pink : t
val rose : t

val hex : string -> t
(** [hex s] creates color from hex string. *)

val rgb : int -> int -> int -> t
(** [rgb r g b] creates color from RGB values. *)

(** {1 Color Conversion} *)

val to_hex : t -> int -> string
(** [to_hex color shade] converts color to hex string for a given shade. *)

val to_oklch_css : t -> int -> string
(** [to_oklch_css color shade] converts color to OKLCH CSS string for a given
    shade. *)

val to_rgb_string : t -> int -> string
(** [to_rgb_string color shade] converts color to RGB string for a given shade.
*)

val to_name : t -> string
(** [to_name color] gets the name of a color as a string. *)

val is_base_color : t -> bool
(** [is_base_color color] checks if a color is black or white (doesn't need a
    shade). *)

val is_custom_color : t -> bool
(** Check if a color is a custom color (hex or rgb) *)

(** {1 Tailwind Colors} *)

(** Predefined Tailwind v4 color values *)
module TailwindColors : sig
  val get_color : string -> int -> string option
  (** Get a Tailwind color value in OKLCH format *)
end
