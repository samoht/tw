(** Typed CSS variable definitions and ordering.

    This module defines the typed set of CSS custom properties used by the
    library. Theme variables (design tokens) are emitted in the theme layer and
    ordered to match Tailwind v4 expectations. Utility variables are ordered
    lexicographically. *)

(** Layer classification for CSS variables. In v4, variables live in [\@layer]
    theme or inline within utilities; base/properties are not used for
    Var-defined variables. *)
type layer = Theme | Utility

(** CSS variable type as a GADT for type safety *)
type _ t =
  (* Design tokens first *)
  | Font_sans : Css.font_family list t
  | Font_mono : Css.font_family list t
  (* Colors *)
  | Color :
      string * int option
      -> Css.color t (* e.g., Color ("blue", Some 500) *)
  | Spacing : Css.length t
  | Default_font_family : Css.font_family list t
  | Default_mono_font_family : Css.font_family list t
  | Default_font_feature_settings : Css.font_feature_settings t
  | Default_font_variation_settings : Css.font_variation_settings t
  | Default_mono_font_feature_settings : Css.font_feature_settings t
  | Default_mono_font_variation_settings : Css.font_variation_settings t
  | Font_serif : Css.font_family list t
  (* Typography scale *)
  | Text_xs : Css.length t
  | Text_xs_line_height : Css.line_height t
  | Text_sm : Css.length t
  | Text_sm_line_height : Css.line_height t
  | Text_base : Css.length t
  | Text_base_line_height : Css.line_height t
  | Text_lg : Css.length t
  | Text_lg_line_height : Css.line_height t
  | Text_xl : Css.length t
  | Text_xl_line_height : Css.line_height t
  | Text_2xl : Css.length t
  | Text_2xl_line_height : Css.line_height t
  | Text_3xl : Css.length t
  | Text_3xl_line_height : Css.line_height t
  | Text_4xl : Css.length t
  | Text_4xl_line_height : Css.line_height t
  | Text_5xl : Css.length t
  | Text_5xl_line_height : Css.line_height t
  | Text_6xl : Css.length t
  | Text_6xl_line_height : Css.line_height t
  | Text_7xl : Css.length t
  | Text_7xl_line_height : Css.line_height t
  | Text_8xl : Css.length t
  | Text_8xl_line_height : Css.line_height t
  | Text_9xl : Css.length t
  | Text_9xl_line_height : Css.line_height t
  (* Font weights *)
  | Font_weight_thin : Css.font_weight t
  | Font_weight_extralight : Css.font_weight t
  | Font_weight_light : Css.font_weight t
  | Font_weight_normal : Css.font_weight t
  | Font_weight_medium : Css.font_weight t
  | Font_weight_semibold : Css.font_weight t
  | Font_weight_bold : Css.font_weight t
  | Font_weight_extrabold : Css.font_weight t
  | Font_weight_black : Css.font_weight t
  | Font_weight : Css.font_weight t
  | Leading : Css.line_height t
  (* Border radius *)
  | Radius_none : Css.length t
  | Radius_sm : Css.length t
  | Radius_default : Css.length t
  | Radius_md : Css.length t
  | Radius_lg : Css.length t
  | Radius_xl : Css.length t
  | Radius_2xl : Css.length t
  | Radius_3xl : Css.length t
  (* Transform variables *)
  | Translate_x : Css.length t
  | Translate_y : Css.length t
  | Translate_z : Css.length t
  | Rotate : Css.angle t
  | Skew_x : Css.angle t
  | Skew_y : Css.angle t
  | Scale_x : float t
  | Scale_y : float t
  | Scale_z : float t
  (* Filter variables *)
  | Blur : Css.length t
  | Brightness : float t
  | Contrast : float t
  | Grayscale : float t
  | Hue_rotate : Css.angle t
  | Invert : float t
  | Saturate : float t
  | Sepia : float t
  | Drop_shadow : string t
  | Drop_shadow_alpha : float t
  (* Box shadow variable *)
  | Box_shadow : Css.shadow list t
  (* Backdrop filter variables *)
  | Backdrop_blur : Css.length t
  | Backdrop_brightness : float t
  | Backdrop_contrast : float t
  | Backdrop_grayscale : float t
  | Backdrop_hue_rotate : Css.angle t
  | Backdrop_invert : float t
  | Backdrop_saturate : float t
  | Backdrop_sepia : float t
  | Backdrop_opacity : float t
  (* Shadow and ring variables *)
  | Shadow : Css.shadow t
  | Shadow_color : Css.color t
  | Shadow_alpha : float t
  | Inset_shadow : Css.shadow t
  | Inset_shadow_color : Css.color t
  | Inset_shadow_alpha : float t
  | Ring_color : Css.color t
  | Ring_shadow : Css.shadow t
  | Inset_ring_color : Css.color t
  | Inset_ring_shadow : Css.shadow t
  | Ring_inset : string t
  | Ring_offset_width : Css.length t
  | Ring_offset_color : Css.color t
  | Ring_offset_shadow : Css.shadow t
  | Ring_width : Css.length t
  (* Prose theming variables *)
  | Prose_body : Css.color t
  | Prose_headings : Css.color t
  | Prose_code : Css.color t
  (* Content variable for pseudo-elements *)
  | Content : Css.content t
  | Prose_pre_code : Css.color t
  | Prose_pre_bg : Css.color t
  | Prose_th_borders : Css.color t
  | Prose_td_borders : Css.color t
  | Prose_links : Css.color t
  | Prose_quotes : Css.color t
  | Prose_quote_borders : Css.color t
  | Prose_hr : Css.color t
  | Prose_bold : Css.color t
  | Prose_lead : Css.color t
  | Prose_counters : Css.color t
  | Prose_bullets : Css.color t
  | Prose_captions : Css.color t
  | Prose_kbd : Css.color t
  | Prose_kbd_shadows : string t (* RGB values like "17 24 39" *)
  (* Prose invert variants for dark mode *)
  | Prose_invert_body : Css.color t
  | Prose_invert_headings : Css.color t
  | Prose_invert_lead : Css.color t
  | Prose_invert_links : Css.color t
  | Prose_invert_bold : Css.color t
  | Prose_invert_counters : Css.color t
  | Prose_invert_bullets : Css.color t
  | Prose_invert_hr : Css.color t
  | Prose_invert_quotes : Css.color t
  | Prose_invert_quote_borders : Css.color t
  | Prose_invert_captions : Css.color t
  | Prose_invert_kbd : Css.color t
  | Prose_invert_kbd_shadows : string t
  | Prose_invert_code : Css.color t
  | Prose_invert_pre_code : Css.color t
  | Prose_invert_pre_bg : Css.color t
  | Prose_invert_th_borders : Css.color t
  | Prose_invert_td_borders : Css.color t
  (* Gradient variables *)
  | Gradient_from : Css.color t
  | Gradient_via : Css.color t
  | Gradient_to : Css.color t
  | Gradient_stops : string t
  | Gradient_via_stops : string t
  | Gradient_position : string t
  | Gradient_from_position : float t
  | Gradient_via_position : float t
  | Gradient_to_position : float t
  (* Font variant numeric *)
  | Font_variant_ordinal : Css.font_variant_numeric_token t
  | Font_variant_slashed_zero : Css.font_variant_numeric_token t
  | Font_variant_numeric_figure : Css.font_variant_numeric_token t
  | Font_variant_numeric_spacing : Css.font_variant_numeric_token t
  | Font_variant_numeric_fraction : Css.font_variant_numeric_token t
  | Font_variant_numeric : Css.font_variant_numeric t
  (* Other *)
  | Border_style : Css.border_style t
  | Scroll_snap_strictness : Css.scroll_snap_strictness t
  | Duration : Css.duration t

val name : _ t -> string
(** [name v] returns the variable name without the leading "--". *)

val theme : 'a t -> ?fallback:'a -> 'a -> Css.declaration * 'a Css.var
(** [theme v ?fallback value] creates a theme-layer variable declaration and
    handle. *)

val utility : 'a t -> ?fallback:'a -> 'a -> Css.declaration * 'a Css.var
(** [utility v ?fallback value] creates a utility-layer variable declaration and
    handle. *)

val handle_only : 'a t -> unit -> 'a Css.var
(** [handle_only v] creates a variable handle with empty fallback without a
    definition. Useful for referencing variables that may be defined elsewhere.
    Creates var(--name,) format. *)

val handle : 'a t -> ?fallback:'a -> unit -> 'a Css.var
(** [handle v ?fallback] creates a variable handle with optional fallback
    without a definition. Useful for referencing variables that may be defined
    elsewhere. *)

val property : inherits:bool -> ?initial:'a -> 'a t -> Css.t
(** [property  ~inherits ?initial t v] creates a typed [@property] registration
    as a stylesheet. Examples:
    - [property ~inherits:false ~initial:(Css.hex "#000") My_color]
    - [property ~inherits:false ~initial:"solid" Border_style]

    @param inherits Whether the property inherits.
    @param initial The initial value (optional). *)

val layer : _ Css.var -> layer option
(** [layer var] returns the layer recorded in the variable metadata, if any. *)

val to_string : _ t -> string
(** [to_string v] returns the full "--var-name" representation. *)

val pp : _ t -> string
(** [pp] is {!to_string}. *)

(** Existential wrapper for variables of any type *)
type any = Any : _ t -> any

(* Utilities own their variable registrations and any @property rules. *)

val compare : any -> any -> int
(** [compare a b] compares two variables for canonical ordering in the theme. *)

val compare_declarations : layer -> Css.declaration -> Css.declaration -> int
(** [compare_declarations layer d1 d2] compares two custom declarations via Var
    metadata for the given layer. *)

val var_of_meta : Css.meta -> any option
(** [var_of_meta meta] extracts a variable from CSS metadata if present. *)

module Map : Map.S with type key = any
(** Map with Var.any keys *)

module Set : Set.S with type elt = any
(** Set with Var.any elements *)
