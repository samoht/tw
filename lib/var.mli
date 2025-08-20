(** Variable tracking for CSS composition groups *)

(** CSS variable type covering all Tailwind CSS variables *)
type t =
  (* Spacing and sizing *)
  | Spacing
  (* Typography *)
  | Font_sans
  | Font_serif
  | Font_mono
  | Font_weight
  | Leading
  | Text_xs
  | Text_xs_line_height
  | Text_sm
  | Text_sm_line_height
  | Text_base
  | Text_base_line_height
  | Text_lg
  | Text_lg_line_height
  | Text_xl
  | Text_xl_line_height
  | Text_2xl
  | Text_2xl_line_height
  | Text_3xl
  | Text_3xl_line_height
  | Text_4xl
  | Text_4xl_line_height
  | Text_5xl
  | Text_5xl_line_height
  | Text_6xl
  | Text_6xl_line_height
  | Text_7xl
  | Text_7xl_line_height
  | Text_8xl
  | Text_8xl_line_height
  | Text_9xl
  | Text_9xl_line_height
  | Font_weight_thin
  | Font_weight_extralight
  | Font_weight_light
  | Font_weight_normal
  | Font_weight_medium
  | Font_weight_semibold
  | Font_weight_bold
  | Font_weight_extrabold
  | Font_weight_black
  (* Border radius *)
  | Radius_none
  | Radius_sm
  | Radius_default
  | Radius_md
  | Radius_lg
  | Radius_xl
  | Radius_2xl
  | Radius_3xl
  (* Colors - using color name and optional shade *)
  | Color of string * int option (* e.g., Color ("blue", Some 500) *)
  (* Transform variables *)
  | Translate_x
  | Translate_y
  | Translate_z
  | Rotate
  | Skew_x
  | Skew_y
  | Scale_x
  | Scale_y
  | Scale_z
  (* Filter variables *)
  | Blur
  | Brightness
  | Contrast
  | Grayscale
  | Hue_rotate
  | Invert
  | Saturate
  | Sepia
  | Drop_shadow
  | Drop_shadow_alpha
  (* Backdrop filter variables *)
  | Backdrop_blur
  | Backdrop_brightness
  | Backdrop_contrast
  | Backdrop_grayscale
  | Backdrop_hue_rotate
  | Backdrop_invert
  | Backdrop_saturate
  | Backdrop_sepia
  | Backdrop_opacity
  (* Shadow and ring variables *)
  | Shadow
  | Shadow_color
  | Shadow_alpha
  | Inset_shadow
  | Inset_shadow_color
  | Inset_shadow_alpha
  | Ring_color
  | Ring_shadow
  | Inset_ring_color
  | Inset_ring_shadow
  | Ring_inset
  | Ring_offset_width
  | Ring_offset_color
  | Ring_offset_shadow
  (* Gradient variables *)
  | Gradient_from
  | Gradient_via
  | Gradient_to
  | Gradient_stops
  | Gradient_via_stops
  | Gradient_position
  | Gradient_from_position
  | Gradient_via_position
  | Gradient_to_position
  (* Border variables *)
  | Border_style
  (* Scroll snap variables *)
  | Scroll_snap_strictness
  (* Default font family helpers *)
  | Default_font_family
  | Default_mono_font_family

val to_string : t -> string
(** Convert a CSS variable to its string representation *)

val of_string : string -> t option
(** Parse a CSS variable from its string representation *)

val compare : t -> t -> int
(** Compare two variables for canonical ordering *)

val to_css_properties : t -> Css.declaration list
(** Generate CSS property declarations for a variable *)

type tally
(** The tally of variable usage *)

val empty : tally
(** Empty tally *)

val analyze_properties : Css.declaration list -> tally
(** Analyze CSS properties to track variable assignments and references *)

val generate_properties_layer : tally -> (string * string) list
(** Generate properties layer initializers for groups that need them *)

val needs_at_property : tally -> string list
(** Get list of variables that need [@property] rules *)
