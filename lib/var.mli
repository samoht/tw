(* Variable tracking for CSS composition groups and layer assignment *)

(** Layer classification for CSS variables. In v4, variables live in @layer
    theme or inline within utilities; base/properties are not used for
    Var-defined variables. *)
type layer = Theme | Utility

(** CSS variable type as a GADT for type safety *)
type _ t =
  (* Spacing and sizing *)
  | Spacing : Css.length t
  (* Typography *)
  | Font_sans : Css.font_family list t
  | Font_serif : Css.font_family list t
  | Font_mono : Css.font_family list t
  | Font_weight : string t
  | Leading : string t
  | Text_xs : Css.length t
  | Text_xs_line_height : Css.length t
  | Text_sm : Css.length t
  | Text_sm_line_height : Css.length t
  | Text_base : Css.length t
  | Text_base_line_height : Css.length t
  | Text_lg : Css.length t
  | Text_lg_line_height : Css.length t
  | Text_xl : Css.length t
  | Text_xl_line_height : Css.length t
  | Text_2xl : Css.length t
  | Text_2xl_line_height : Css.length t
  | Text_3xl : Css.length t
  | Text_3xl_line_height : Css.length t
  | Text_4xl : Css.length t
  | Text_4xl_line_height : Css.length t
  | Text_5xl : Css.length t
  | Text_5xl_line_height : Css.length t
  | Text_6xl : Css.length t
  | Text_6xl_line_height : Css.length t
  | Text_7xl : Css.length t
  | Text_7xl_line_height : Css.length t
  | Text_8xl : Css.length t
  | Text_8xl_line_height : Css.length t
  | Text_9xl : Css.length t
  | Text_9xl_line_height : Css.length t
  | Font_weight_thin : int t
  | Font_weight_extralight : int t
  | Font_weight_light : int t
  | Font_weight_normal : int t
  | Font_weight_medium : int t
  | Font_weight_semibold : int t
  | Font_weight_bold : int t
  | Font_weight_extrabold : int t
  | Font_weight_black : int t
  (* Border radius *)
  | Radius_none : Css.length t
  | Radius_sm : Css.length t
  | Radius_default : Css.length t
  | Radius_md : Css.length t
  | Radius_lg : Css.length t
  | Radius_xl : Css.length t
  | Radius_2xl : Css.length t
  | Radius_3xl : Css.length t
  (* Colors - using color name and optional shade *)
  | Color :
      string * int option
      -> Css.color t (* e.g., Color ("blue", Some 500) *)
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
  | Shadow : string t
  | Shadow_color : Css.color t
  | Shadow_alpha : float t
  | Inset_shadow : string t
  | Inset_shadow_color : Css.color t
  | Inset_shadow_alpha : float t
  | Ring_color : Css.color t
  | Ring_width : Css.length t
  | Ring_shadow : string t
  | Inset_ring_color : Css.color t
  | Inset_ring_shadow : string t
  | Ring_inset : string t
  | Ring_offset_width : Css.length t
  | Ring_offset_color : Css.color t
  | Ring_offset_shadow : string t
  (* Prose theming variables (colors) *)
  | Prose_body : Css.color t
  | Prose_headings : Css.color t
  | Prose_code : Css.color t
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
  (* Border variables *)
  | Border_style : Css.border_style t
  (* Scroll snap variables *)
  | Scroll_snap_strictness : Css.scroll_snap_strictness t
  (* Transition variables *)
  | Duration : Css.duration t
  (* Default font variables *)
  | Default_font_family : Css.font_family list t
  | Default_mono_font_family : Css.font_family list t
  (* Font feature/variation settings - undefined in Tailwind v4, used with
     fallbacks *)
  | Default_font_feature_settings : Css.font_feature_settings t
  | Default_font_variation_settings : Css.font_variation_settings t
  | Default_mono_font_feature_settings : Css.font_feature_settings t
  | Default_mono_font_variation_settings : Css.font_variation_settings t

val name : _ t -> string
(** Get the name of a variable (without --) *)

val theme : 'a t -> ?fallback:'a -> 'a -> Css.declaration * 'a Css.var
(** Create a theme layer variable (colors, spacing, fonts, radius) *)

val utility : 'a t -> ?fallback:'a -> 'a -> Css.declaration * 'a Css.var
(** Create a utility layer variable *)

val property :
  'a t -> syntax:string -> inherits:bool -> initial:string -> Css.property_rule
(** Create an @property rule for a variable.
    @param syntax The property syntax (e.g., "*", "<color>", "<length>")
    @param inherits Whether the property inherits
    @param initial The initial value *)

val layer : _ Css.var -> layer option
(** Get the layer from a CSS variable *)

val to_string : _ t -> string
(** Convert a CSS variable to its string representation (with --) *)

(** Existential wrapper for variables of any type *)
type any = Any : _ t -> any

(* Utilities own their variable registrations and any @property rules. *)

val canonical_theme_order : any list
(** List of all theme variables in their canonical order *)

val compare_for : layer -> any -> any -> int
(** Compare two variables for ordering within a specific layer.
    - Theme: Design tokens first (fonts), then spacing, type scales, colors,
      radii
    - Utility: Fallback to alphabetical ordering *)

val compare_declarations : layer -> Css.declaration -> Css.declaration -> int
(** Compare two CSS declarations by extracting and comparing their typed
    variable metadata *)
