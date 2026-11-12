(** Colours under an opacity modifier.

    The modifier a colour class takes after its [/] ([/50], [/[0.3]], [/(--o)],
    [/half]), and what it makes of a colour: an alpha folded in where the colour
    is known at build time, else a [color-mix()] behind an [\@supports] guard
    with a fallback in the open for a browser without it. Every colour family
    writes its declarations through these builders. *)

open Cascade

(** {1 Opacity Modifiers} *)

type opacity_number = {
  value : float;  (** the number the modifier denotes *)
  text : string;  (** the digits the author wrote, for the class name *)
}
(** A number in an opacity modifier. The class name is a selector, so it repeats
    [text] rather than re-printing [value]: [/[25]] and [/[25.0]] denote one
    alpha and are two different classes. *)

(** The modifier a colour class spells after its [/]. *)
type opacity_modifier =
  | No_opacity
  | Opacity_percent of opacity_number  (** e.g., /50 means 50% *)
  | Opacity_arbitrary of opacity_number  (** e.g., /[0.5] means 0.5 *)
  | Opacity_bracket_percent of opacity_number
      (** e.g., /[50%] means 50% but preserves bracket form in class name *)
  | Opacity_named of string  (** e.g., /half, /custom - theme-defined names *)
  | Opacity_var of string
      (** e.g., /[var(--x)] - var ref used directly as percentage *)

val opacity_of_int : int -> opacity_modifier
(** [opacity_of_int pct] is the modifier a class spells [/pct]. *)

val opacity_of_string : ?theme:Scheme.t -> string -> opacity_modifier option
(** [opacity_of_string ?theme s] parses the modifier that follows the [/] in a
    colour class: a percentage, a bracket value, the [(--x)] shorthand, or a
    theme-defined name. *)

val has_opacity : string -> bool
(** [has_opacity s] is [true] when [s] carries a [/] opacity modifier. *)

val parse_opacity_modifier :
  ?theme:Scheme.t -> string -> string * opacity_modifier
(** [parse_opacity_modifier ?theme s] parses an opacity modifier from a string.
    Returns the base string and the opacity modifier. Example: "500/50" ->
    ("500", Opacity_percent 50.0). Named opacities are validated at parse time
    against the [@theme] tokens in [theme]. *)

val shade_and_opacity_of_strings :
  ?theme:Scheme.t ->
  string list ->
  (Color_value.color * int * opacity_modifier, [ `Msg of string ]) result
(** [shade_and_opacity_of_strings ?theme parts] parses a color, shade, and
    optional opacity modifier from a list of strings. Example:
    ["blue"; "500/50"] -> Ok (Blue, 500, Opacity_percent 50.0). *)

val pp_opacity : opacity_modifier -> string
(** [pp_opacity modifier] is the class-name spelling of [modifier], without the
    leading ["/"]: ["50"] for [/50], ["[0.5]"] for [/[0.5]], [""] for
    {!No_opacity}. *)

val opacity_suffix : opacity_modifier -> string
(** [opacity_suffix modifier] is {!pp_opacity} behind the ["/"] that separates
    it from the colour, and [""] for {!No_opacity}. *)

val opacity_to_percent : opacity_modifier -> float
(** [opacity_to_percent modifier] returns the opacity as a float percentage. *)

val opacity_var_bare : string -> string
(** [opacity_var_bare v] is the bare custom-property name inside an opacity
    modifier, written either as ["[var(--x)]"] or as the ["(--x)"] shorthand. *)

val opacity_var_bare_of : opacity_modifier -> string option
(** [opacity_var_bare_of opacity] is the custom property the modifier reads its
    percentage from, when it names one rather than giving a number. *)

val is_fully_opaque : opacity_modifier -> bool
(** [is_fully_opaque opacity] is whether [opacity] is a number of at least 100%,
    a modifier the colour takes as a no-op. One read from a custom property or a
    theme name never is: its value is not known here. *)

val opacity_percentage : opacity_modifier -> Css.percentage
(** [opacity_percentage opacity] is the alpha a modifier writes into an alpha
    channel: a percentage, or the custom property a bracket alpha reads. *)

val opacity_alpha_value : opacity_modifier -> Css.percentage
(** [opacity_alpha_value opacity] is what a family's [--tw-<family>-alpha]
    channel is set to: the percentage, the custom property a bracket alpha
    reads, or the author's own unscaled number for a bracket alpha with no [%],
    which is what Tailwind writes for [shadow-lg/[25]]. *)

(** {1 Applying an alpha} *)

val mix_alpha :
  ?in_space:Css.color_space -> opacity_modifier -> Css.color -> Css.color
(** [mix_alpha ?in_space opacity color] is [color] with the modifier's alpha
    applied: a percentage folds into the [color-mix], an alpha read from a var
    is referenced by name. *)

val apply_alpha :
  ?in_space:Css.color_space -> opacity_modifier -> Css.color -> Css.color
(** [apply_alpha ?in_space opacity color] applies the modifier while preserving
    the identity that every alpha of [transparent] is still [transparent]. *)

val color_mix_supports_condition : Css.Supports.t
(** [color_mix_supports_condition] is the CSS supports condition for color-mix:
    [(color: color-mix(in lab, red, red))]. *)

val color_mix_supports : Css.declaration list -> Css.statement
(** [color_mix_supports decls] is [decls] on the utility's own class behind
    {!color_mix_supports_condition}: the [\@supports] block a colour utility
    puts its [color-mix()] value in, with the fallback left in the open. *)

val opacity_fallback :
  ?theme:Scheme.t ->
  percent:float ->
  Color_value.color ->
  int ->
  Css.color ->
  Css.color
(** [opacity_fallback ?theme ~percent c shade value] is what a browser without
    [color-mix()] reads for [c] at [percent] opacity. A palette colour folds the
    alpha into a plain hex; a project token, whose [value] the theme supplies
    and which may name a colour space no hex can spell, takes an sRGB mix
    instead. *)

val hex_alpha_color :
  ?theme:Scheme.t ->
  Color_value.color ->
  int ->
  opacity_modifier ->
  string option
(** [hex_alpha_color ?theme color shade opacity] returns a hex color with alpha
    if the color is defined in the scheme, otherwise None. This is useful for
    properties where Tailwind outputs simple hex+alpha without [@supports]. *)

val relative_color_supports : Css.Supports.t
(** [relative_color_supports] is the guard for relative colour syntax:
    [(color: lab(from red l a b))]. *)

val relative_alpha : opacity_modifier -> Css.color -> Css.color
(** [relative_alpha opacity c] is Tailwind's [oklab(from c l a b / alpha)]: [c]
    with the modifier's alpha in place of its own. *)

val shadow_alpha_decls :
  opacity_modifier ->
  colours:Css.color list ->
  rebuild:(Css.color list -> Css.declaration) ->
  Css.declaration * Css.statement option
(** [shadow_alpha_decls opacity ~colours ~rebuild] is a shadow list under an
    opacity modifier, one colour per layer, as Tailwind writes it: the
    declaration [rebuild] makes of the colours every browser reads, and, when a
    layer needs one, the [\@supports] block holding the declaration made of the
    alpha-carrying forms. A colour whose channels the class names folds the
    alpha in at build time; [currentcolor] takes it through a [color-mix()]
    behind that guard; a [var()] colour, or an alpha read from a custom
    property, keeps every layer's authored colour in the open and takes the
    alpha behind the relative colour guard, with the [color-mix()] guard nested
    inside it for a [currentcolor] layer. *)

(** {1 Colour styles}

    These functions generate scheme-aware color styles with progressive
    enhancement. They produce a fallback declaration plus a [\@supports] block
    for color-mix. *)

val colors_with_opacity_style :
  ?theme:Scheme.t ->
  properties:(Css.color -> Css.declaration) list ->
  ?property_prefix:string ->
  ?merge_key:string ->
  Color_value.color ->
  int ->
  opacity_modifier ->
  Style.t
(** [colors_with_opacity_style ~properties color shade opacity] sets every
    declaration in [properties] to [color] at [opacity]: the srgb fallback plus
    the [@supports] colour-mix override Tailwind emits. *)

val color_with_opacity_style :
  ?theme:Scheme.t ->
  property:(Css.color -> Css.declaration) ->
  ?property_prefix:string ->
  ?merge_key:string ->
  Color_value.color ->
  int ->
  opacity_modifier ->
  Style.t
(** [color_with_opacity_style ~property color shade opacity] is
    {!colors_with_opacity_style} for the one declaration [property]. *)

val fill_with_opacity :
  ?theme:Scheme.t -> Color_value.color -> int -> opacity_modifier -> Style.t
(** [fill_with_opacity ?theme color shade opacity] generates fill style with
    opacity, reading scheme colours from [theme] when given. *)

val stroke_with_opacity :
  ?theme:Scheme.t -> Color_value.color -> int -> opacity_modifier -> Style.t
(** [stroke_with_opacity ?theme color shade opacity] generates stroke style with
    opacity, reading scheme colours from [theme] when given. *)

val fill_current_with_opacity : opacity_modifier -> Style.t
(** [fill_current_with_opacity opacity] generates fill currentColor with
    opacity. *)

val stroke_current_with_opacity : opacity_modifier -> Style.t
(** [stroke_current_with_opacity opacity] generates stroke currentColor with
    opacity. *)

val divide_with_opacity :
  ?theme:Scheme.t ->
  Color_value.color ->
  int ->
  opacity_modifier ->
  Css.Selector.t ->
  Style.t
(** [divide_with_opacity ?theme color shade opacity selector] generates divide
    border-color with opacity using the given selector. *)

val divide_current_with_opacity : opacity_modifier -> Css.Selector.t -> Style.t
(** [divide_current_with_opacity opacity selector] generates divide currentColor
    with opacity using the given selector. *)

val bg_with_opacity :
  ?theme:Scheme.t -> Color_value.color -> int -> opacity_modifier -> Style.t
(** [bg_with_opacity ?theme color shade opacity] generates background-color
    style with opacity. Scheme-aware: uses hex+alpha fallback with theme
    variable in [\@supports] block. *)

val bg_current_with_opacity : opacity_modifier -> Style.t
(** [bg_current_with_opacity opacity] generates background-color currentColor
    with opacity using color-mix progressive enhancement. *)

(** {1 Bracket colours} *)

val pre_color_mix_fallback : Scheme.t -> Css.color -> Css.color option
(** [pre_color_mix_fallback theme c] is Tailwind's legacy value for an arbitrary
    [color-mix()] containing dynamic colour operands. Theme variables are
    resolved into an sRGB mix. If an operand cannot be resolved at compile time,
    the first colour operand is used. [None] means no guard is needed. *)

val bracket_colors_style :
  ?merge_key:string ->
  theme:Scheme.t ->
  properties:(Css.color -> Css.declaration) list ->
  Css.color ->
  Style.t
(** [bracket_colors_style ~theme ~properties c] is {!bracket_color_style} for
    every declaration in [properties], which a per-side border colour on the
    [x]/[y] axes needs. *)

val bracket_color_style :
  ?merge_key:string ->
  theme:Scheme.t ->
  property:(Css.color -> Css.declaration) ->
  Css.color ->
  Style.t
(** [bracket_color_style ~theme ~property c] emits [c], preceded by its
    pre-[color-mix()] fallback and followed by a guarded enhancement when the
    arbitrary colour needs progressive enhancement. *)

(** What an opacity modifier makes of a bracket colour. *)
type bracket_opacity =
  | Folded of Css.color
      (** the single value a colour with a hex spelling and a literal alpha
          resolves to *)
  | Guarded of { fallback : Css.color; mixed : Css.color }
      (** the colour itself, for a browser with no [color-mix()], and the mix
          that goes behind an [@supports] guard *)

val bracket_color_opacity :
  ?theme:Scheme.t -> Css.color -> opacity_modifier -> bracket_opacity
(** [bracket_color_opacity c opacity] is what [opacity] makes of the bracket
    colour [c]. It answers values rather than declarations because the
    properties they land on differ by family: a decoration colour writes a
    vendor prefix alongside, and a divide colour hangs both on its child
    selector. *)

val bracket_colors_opacity_style :
  ?theme:Scheme.t ->
  ?merge_key:string ->
  properties:(Css.color -> Css.declaration) list ->
  Css.color ->
  opacity_modifier ->
  Style.t
(** [bracket_colors_opacity_style ~properties c opacity] is
    {!bracket_color_opacity_style} for every declaration in [properties]. *)

val bracket_color_opacity_style :
  ?theme:Scheme.t ->
  ?merge_key:string ->
  property:(Css.color -> Css.declaration) ->
  Css.color ->
  opacity_modifier ->
  Style.t
(** [bracket_color_opacity_style ~property c opacity] sets [property] to the
    bracket colour [c] with [opacity] mixed into it. [c] is the colour the
    bracket was parsed into, so a CSS keyword or colour function keeps its own
    value rather than being read back through the palette. *)

(** {1 Colour channels}

    A family that paints through a [--tw-<family>-color] custom property with a
    [--tw-<family>-alpha] companion sets the pair the same way whatever the
    family: the box shadow, inset shadow and text shadow utilities share these
    builders. Each writes the colour every browser reads in the open and, behind
    {!color_mix_supports_condition}, the colour with the alpha channel mixed in.
*)

type channel = {
  color : Css.color Var.channel;  (** the colour custom property *)
  alpha : Css.percentage Var.property_default;  (** its alpha companion *)
  property_prefix : string;
      (** the prefix of a property-scoped palette token, [box-shadow-color] *)
  metadata : Var.metadata list;  (** what {!Style.style} gets as [metadata] *)
  property_rules : Css.t;  (** and as [property_rules] *)
}
(** A family's colour custom property and its alpha companion. *)

val channel_style :
  ?decls:Css.declaration list ->
  channel ->
  fallback:Css.color ->
  Css.color ->
  Style.t
(** [channel_style ?decls ch ~fallback c] sets [ch] to [fallback] and, behind
    the guard, to [c] at the channel's alpha, with [decls] declared beside it.
*)

val channel_inherit : channel -> Style.t
(** [channel_inherit ch] sets [ch] to [inherit], which no alpha applies to. *)

val palette_hex :
  ?theme:Scheme.t ->
  ?property_prefix:string ->
  Color_value.color ->
  int ->
  string
(** [palette_hex ?theme ?property_prefix c shade] is the hex a browser without
    [color-mix()] reads for [c] at [shade]: the scheme's own hex, else the
    [property_prefix]-scoped token [theme] declares, else its [--color-*] token,
    else the palette colour converted. *)

val channel_color :
  ?theme:Scheme.t -> channel -> Color_value.color -> int -> Style.t
(** [channel_color ?theme ch c shade] sets [ch] to a palette colour. *)

val channel_color_opacity :
  ?theme:Scheme.t ->
  channel ->
  Color_value.color ->
  int ->
  opacity_modifier ->
  Style.t
(** [channel_color_opacity ?theme ch c shade opacity] sets [ch] to a palette
    colour at [opacity]: a hex carrying the alpha byte in the open, the mix
    behind the guard. *)

val channel_current : channel -> Style.t
(** [channel_current ch] sets [ch] to [currentcolor]. *)

val channel_current_opacity : channel -> opacity_modifier -> Style.t
(** [channel_current_opacity ch opacity] sets [ch] to [currentcolor] at
    [opacity]. *)

val channel_transparent : channel -> Style.t
(** [channel_transparent ch] sets [ch] to [transparent]. *)

val channel_transparent_opacity : channel -> opacity_modifier -> Style.t
(** [channel_transparent_opacity ch opacity] sets [ch] to [transparent] at
    [opacity]. *)

val channel_bracket_color : theme:Scheme.t -> channel -> Css.color -> Style.t
(** [channel_bracket_color ~theme ch c] sets [ch] to the bracket colour [c],
    with its {!pre_color_mix_fallback} in the open when it needs one. *)

val channel_bracket_color_opacity :
  theme:Scheme.t -> channel -> Css.color -> opacity_modifier -> Style.t
(** [channel_bracket_color_opacity ~theme ch c opacity] sets [ch] to the bracket
    colour [c] at [opacity]: a colour with a hex spelling carries the alpha byte
    in the open, one without takes an sRGB mix there. *)

val bracket_var_ref : string -> Css.color
(** [bracket_var_ref v] is the reference the [var()] expression [v] of a bracket
    names, as a colour. *)

val bracket_var_color : string -> Css.color
(** [bracket_var_color v] is the colour a [var()] token in an arbitrary value
    paints with: the colour reader's reading of [v] when it has one, else
    {!bracket_var_ref}. *)

val shadow_token_colour : Parse.shadow_colour -> Css.color
(** [shadow_token_colour t] is the colour a bracket shadow layer wrote, as the
    class spelled it: a hex keeps its case, a [var()] its reference, a colour
    function folds to hex where one spells it, and no colour is the current
    colour. *)

val channel_bracket_var : channel -> string -> Style.t
(** [channel_bracket_var ch v] sets [ch] to the [var()] reference [v]. *)

val channel_bracket_var_opacity :
  channel -> string -> opacity_modifier -> Style.t
(** [channel_bracket_var_opacity ch v opacity] sets [ch] to the [var()]
    reference [v] at [opacity]. *)
