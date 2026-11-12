(** sRGB colours by the byte.

    The hex spellings a class or a theme writes, the CSS colours that fold to
    one, and the conversions between sRGB bytes and the OKLab and OKLCh forms
    Tailwind's palette is written in. The float arithmetic behind the
    conversions is cascade's {!Cascade.Color_space}; this module owns the byte
    and hex sides of it. *)

open Cascade

type rgb = {
  r : int;  (** Red channel (0-255) *)
  g : int;  (** Green channel (0-255) *)
  b : int;  (** Blue channel (0-255) *)
}
(** An sRGB colour by its bytes. *)

type oklch = {
  l : float;  (** Lightness (0-100) *)
  c : float;  (** Chroma (0-0.4+) *)
  h : float;  (** Hue (0-360) *)
}
(** An OKLCh colour, its lightness a percentage. *)

(** {1 Conversions} *)

val rgb_to_oklch : rgb -> oklch
(** [rgb_to_oklch rgb] converts RGB color to OKLCH color space. *)

val oklch_to_rgb : oklch -> rgb
(** [oklch_to_rgb oklch] converts OKLCH color to RGB color space. A colour
    outside the sRGB gamut loses chroma at constant lightness and hue until it
    is within a just noticeable difference of one sRGB can show, taking the
    first such chroma as Tailwind's published fallbacks do. *)

val rgb_to_oklab : rgb -> float * float * float
(** [rgb_to_oklab rgb] converts RGB to OKLab (L, a, b) components. *)

val oklch_to_css : oklch -> string
(** [oklch_to_css oklch] formats OKLCH for CSS. *)

val round_n : int -> float -> float
(** [round_n n f] rounds [f] to [n] decimal places. *)

(** {1 Hex spellings} *)

val hex_to_rgb : string -> rgb option
(** [hex_to_rgb hex] parses hex color string to RGB. The leading [#] is
    optional, and an alpha digit pair, [#rgba] or [#rrggbbaa], is dropped. *)

val rgb_to_hex : rgb -> string
(** [rgb_to_hex rgb] converts RGB to hex string. *)

val rgba_to_hex : int -> int -> int -> int -> string
(** [rgba_to_hex r g b a] is the hex spelling of the bytes [r], [g], [b] with
    the alpha byte [a], which a fully opaque colour ([a = 255]) leaves off. *)

val hex_string_of_rgb : int * int * int -> string
(** [hex_string_of_rgb (r, g, b)] is the six lower-case hex digits of the bytes,
    without a [#]. *)

val hex_with_alpha : string -> float -> string
(** [hex_with_alpha hex_str opacity_percent] adds alpha to a hex color string.
    Returns #RRGGBBAA format. The opacity is a percentage (0-100). *)

val hex_to_oklch_css : string -> string
(** [hex_to_oklch_css hex] is the [oklch()] spelling of [hex], or [hex] itself
    when it does not parse. *)

val hex_to_oklab_alpha : string -> float -> Css.color
(** [hex_to_oklab_alpha hex alpha] converts a hex color to an oklab CSS color
    with the given alpha (0.0-1.0). Used for bracket hex colors with opacity
    where the color is known at compile time. *)

val shorten_hex_str : string -> string
(** [shorten_hex_str hex] shortens a hex color string if possible. *)

val authored_hex : string -> Css.color
(** [authored_hex hex] is the colour [hex] spells, keeping that spelling for
    output: [#f00] stays three digits and [#F00] keeps its case, which is what
    Tailwind writes back for an arbitrary colour. Leading [#] optional. Raises
    [Invalid_argument] on a string that is not a hex colour, as {!Css.hex} does.
*)

val is_rgb_call : string -> bool
(** [is_rgb_call s] is whether [s] is spelled as an [rgb(...)] call. *)

val parse_rgb_string : string -> (int * int * int) option
(** [parse_rgb_string s] is the three channels of the comma-separated
    [rgb(r, g, b)] spelling [s], or [None] when [s] is not one. *)

(** {1 CSS colours} *)

val oklab_alpha : Css.color -> float -> Css.color option
(** [oklab_alpha c alpha] is [c] in oklab carrying [alpha] (0.0-1.0), which is
    what [oklab(from c l a b / alpha)] resolves to: the alpha replaces the
    colour's own rather than multiplying it, where a [color-mix()] multiplies.
    [None] for a colour whose channels are not named where it is written -
    [currentcolor], a [var()] reference, a colour function - leaving those to a
    [color-mix()] or to the browser. *)

val css_color_to_hex : Css.color -> Css.color option
(** [css_color_to_hex c] converts a typed CSS color (Rgb, Rgba, Hsl) to a hex
    color for Tailwind parity. Returns [None] for color types that cannot be
    easily converted (oklch, oklab, etc.). *)

val resolve_bracket_css_color : Css.color -> Css.color
(** [resolve_bracket_css_color c] returns the compact emission form of an
    arbitrary colour, folding static RGB/HSL spellings to hex when possible. *)
