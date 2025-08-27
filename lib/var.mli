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
  (* Transition variables *)
  | Duration
  (* Default font family helpers *)
  | Default_font_family
  | Default_mono_font_family

val to_string : t -> string
(** [to_string v] converts variable [v] to its string representation. *)

val of_string : string -> t option
(** [of_string s] parses a CSS variable from string [s]. *)

val compare : t -> t -> int
(** [compare a b] compares variables for canonical ordering. *)

type tally
(** The tally of variable usage. *)

val empty : tally
(** [empty] is the empty tally. *)

val tally_of_vars : string list -> tally
(** [tally_of_vars var_names] creates a tally from CSS variable names (with or
    without '--' prefix). All variables are marked as assigned (not just
    fallback references). *)

val pp : t -> string
(** [pp v] pretty-prints variable [v] as a string. *)

val generate_properties_layer : tally -> (string * string) list
(** [generate_properties_layer t] generates properties layer initializers for
    groups that need them. *)

val needs_at_property : tally -> t list
(** [needs_at_property t] lists variables that need [@property] rules. *)

val at_property_config : t -> (string * string * bool * string) option
(** [at_property_config v] returns the @property configuration
    (name, syntax, inherits, initial_value) for variable [v]. *)

val is_composition_var : string -> bool
(** [is_composition_var name] returns true if the variable name (with or without '--' prefix)
    is a composition variable that should be tracked for @property rules. 
    This includes --tw-* variables and other composition variables. *)

val extract_composition_vars : (string * string) list -> string list
(** [extract_composition_vars custom_props] extracts composition variable names
    from a list of custom property declarations (name, value) pairs. Returns the
    list of composition variables that are being assigned. *)
