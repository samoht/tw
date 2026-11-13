(** The colours a class names, and the values and theme tokens they resolve to.

    A colour is a palette family ({!Palette}), [black] or [white], a token the
    project's [\@theme] block declares, or an arbitrary value written in a
    bracket. This module reads each from a class name, says which [--color-*]
    token it reads and where that token sorts, and resolves it to the CSS value
    a utility writes. *)

open Cascade

(** A colour a class can name: the palette families and the shadeless [Black]
    and [White], an arbitrary value, or a [Theme_named] token the project
    declared. *)
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
  | Oklch of Srgb.oklch
  | Css of Css.color
      (** Arbitrary CSS color function (rgba, hsl, oklch, ...) *)
  | Theme_named of string

val pp : color -> string
(** [pp color] pretty-prints a color. *)

val to_css : ?theme:Scheme.t -> color -> int -> Css.color
(** [to_css ?theme color shade] converts a color to CSS color value. A colour
    [theme.colors] binds takes that value instead of the palette's. A project
    token declared in an [\@theme] block has no palette entry, so [theme]
    supplies its value; without one such a colour reads as transparent. *)

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

val to_oklch : color -> int -> Srgb.oklch
(** [to_oklch color shade] converts color to OKLCH data for a given shade. A
    colour the palette does not define reads as black; use {!to_oklch_opt} to
    tell that case apart. *)

val to_oklch_opt : color -> int -> Srgb.oklch option
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

val is_theme_named : color -> bool
(** [is_theme_named c] is whether [c] names a token the project's [\@theme]
    declared rather than a palette colour; such a colour carries no shade. *)

val is_valid_shade : color -> int -> bool
(** [is_valid_shade color shade] checks that [shade] is one the Tailwind palette
    defines for [color]. Shadeless colors accept any shade (it is ignored). *)

val check_shade : utility:string -> color -> int -> unit
(** [check_shade ~utility color shade] raises [Invalid_argument] if
    [is_valid_shade color shade] is false. [utility] names the constructor in
    the error message. *)

(** {1 Theme tokens} *)

val color_var : color -> int -> Css.color Var.theme
(** [color_var color shade] gets or creates a memoized color variable for the
    given color and shade. *)

val bound :
  ?theme:Scheme.t ->
  Css.color Var.theme ->
  Css.color ->
  Css.declaration list * Css.color
(** [bound ?theme var value] is the theme declaration a colour token needs and
    the value a utility writes for it: the reference, with the token declared
    beside it, or for a token [theme]'s [\@theme inline] block declared, [value]
    itself and no declaration. *)

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

val token_value : ?theme:Scheme.t -> color -> int -> Css.color
(** [token_value ?theme color shade] is the value the [--color-*] token of
    [color] at [shade] binds: {!to_css} at [shade], or at 500 for a colour with
    no shade of its own, {!val-black} or {!val-white}. *)

val palette_token : color -> int -> string option
(** [palette_token color shade] is the [color-*] token a palette colour reads,
    shaded or not, or [None] for a colour that carries its own value or names a
    project token. *)

val palette_is_declared : Scheme.t option -> color -> int -> bool
(** [palette_is_declared theme color shade] is [false] when [theme]'s [\@theme]
    block removed the token the palette colour reads. *)

val color_binding :
  ?theme:Scheme.t -> color -> int -> Css.declaration * Css.color Css.var
(** [color_binding ?theme color shade] is the [\@layer theme] declaration for
    the palette token and the typed reference to it, for utilities that set a
    colour var of their own from a palette entry. *)

val bound_color :
  ?theme:Scheme.t -> shade:int -> color -> Css.declaration list * Css.color
(** [bound_color ?theme ~shade color] is {!bound} for the palette token of
    [color] at [shade]: the theme declaration and the value a utility writes. A
    custom colour has no token, so it is its own value with no declaration. *)

val all_palette_declarations : ?theme:Scheme.t -> unit -> Css.declaration list
(** [all_palette_declarations ?theme ()] is the [\@layer theme] declaration for
    every colour the palette defines, in theme order. [theme(static)] on the
    package import emits the whole theme, not only the tokens a utility used. *)

val theme_color_of_name : string -> (color * int) option
(** [theme_color_of_name name] is the palette colour and shade a [color-*] token
    names, or {!constructor-None} when it names none. *)

val theme_color_decl : ?theme:Scheme.t -> string -> Css.declaration option
(** [theme_color_decl ?theme name] is the [\@layer theme] declaration for the
    colour token [name] (e.g. ["color-red-500"]), or [None] when [name] is not a
    catalogued colour token. Lets the build emit colour tokens that are only
    referenced via [var()] (e.g. by an arbitrary [color:var(--color-red-500)])
    rather than set by a colour utility. *)

val theme_token : Scheme.t -> string -> string option
(** [theme_token theme name] is the value [theme] binds the custom property
    [--name] to, as text: a token the theme declares, or a palette colour
    spelled out. [None] for a name the theme does not bind, a removed token
    included. It is what a walk over a value's text inlines for a [var()]
    reading [name], as Tailwind's polyfills do. *)

(** {1 Ordering} *)

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

(** {1 Parsing} *)

val shade_of_strings :
  ?theme:Scheme.t -> string list -> (color * int, [ `Msg of string ]) result
(** [shade_of_strings ?theme parts] parses a color and shade from a list of
    strings. Example: ["blue"; "500"] -> Ok (Blue, 500). A name the palette does
    not know resolves against [theme]: a [--color-<name>] token the project
    declared is a shadeless colour of its own. *)

val parse_bracket_color : string -> Css.color option
(** [parse_bracket_color inner] parses a bracket color value into a typed
    {!Css.color}. Handles hex, CSS color functions (rgba, hsl, oklch, ...), and
    Tailwind named colors. Returns [None] if not a recognized color. *)

(** What a bracket value names, once the [color:]/[var(] spellings are told
    apart from a plain color. Every color-bearing utility (text, outline, ring,
    shadow, fill, stroke, ...) classifies its bracket content this way; only the
    variant it stores the result in differs. *)
type bracket_hint =
  | Typed_var of string  (** [color:var(--x)], the [var(...)] text *)
  | Bare_var of string  (** [var(--x)], the full [var(...)] text *)
  | Plain_color of Css.color  (** any other color spelling *)

val parse_bracket_hint : string -> bracket_hint option
(** [parse_bracket_hint inner] classifies a bracket's inner text as a typed var,
    a bare var, or a plain color parsed via {!parse_bracket_color}. Returns
    [None] when [inner] is none of these.

    A [color:] hint says how to read the value written after it, so
    [color:var(--x)] is a typed var while [color:red] is the color red. Only a
    [var()] reference names a custom property. *)

val is_bracket_color : string -> bool
(** [is_bracket_color v] is [true] when [v] is a [var()] reference or a colour
    {!parse_bracket_color} reads. *)

val bracket_color_hint : string -> string option
(** [bracket_color_hint inner] is the value after a [color:] hint in [inner],
    when there is one and {!is_bracket_color} accepts it. *)
