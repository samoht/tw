(** Colour utilities: the text, border, outline, accent, caret and placeholder
    colours, and the colour model every colour family shares.

    The model lives in four modules this one re-exports: {!Srgb} for the hex
    spellings and colour-space conversions, {!Color_value} for the colours a
    class names and the theme tokens they read, {!Color_opacity} for colours
    under an opacity modifier, and {!Palette} for Tailwind's default palette. *)

open Cascade

(** {1 Colour model} *)

include module type of struct
  include Srgb
end

include module type of struct
  include Color_value
end

include module type of struct
  include Color_opacity
end

(** Predefined Tailwind v4 color values, from {!Palette}. *)
module Tailwind : sig
  val get_color : string -> int -> string option
  (** [get_color name shade] gets a Tailwind color value in OKLCH format. *)
end

open Utility

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

val border_inherit : t
(** [border_inherit] inherits the border color. *)

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

(** The colour utility handler, with the colour helpers other families reach
    through it. *)
module Handler : sig
  include Utility.Handler

  val all_palette_declarations : ?theme:Scheme.t -> unit -> Css.declaration list
  (** [all_palette_declarations] is {!Color_value.all_palette_declarations}. *)

  val color_binding :
    ?theme:Scheme.t -> color -> int -> Css.declaration * Css.color Css.var
  (** [color_binding] is {!Color_value.color_binding}. *)

  val colors_with_opacity_style :
    ?theme:Scheme.t ->
    properties:(Css.color -> Css.declaration) list ->
    ?property_prefix:string ->
    ?merge_key:string ->
    color ->
    int ->
    opacity_modifier ->
    Style.t
  (** [colors_with_opacity_style] is {!Color_opacity.colors_with_opacity_style}.
  *)

  val theme_color_of_name : string -> (color * int) option
  (** [theme_color_of_name] is {!Color_value.theme_color_of_name}. *)

  val theme_color_decl : ?theme:Scheme.t -> string -> Css.declaration option
  (** [theme_color_decl] is {!Color_value.theme_color_decl}. *)
end
