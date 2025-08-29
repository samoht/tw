(** Visual effects utilities for shadows, opacity, and filters

    What's included:
    - Shadows: `shadow-none/sm/(default)/md/lg/xl/2xl/inner`.
    - Opacity: `opacity-{0..100}`.
    - Filter & backdrop-filter helpers: see Filters module for full coverage.
    - Ring and transition helpers.

    What's not:
    - Full mix-blend and backdrop-blend sets are not exposed where the typed
      `Css` API lacks variants. You can extend with
      `style "mix-blend-multiply" [Css.property "mix-blend-mode" "multiply"]`.

    Parsing contract (`of_string`):
    - Accepts `opacity-<n>` and common transition aliases. Unknown tokens yield
      `Error (`Msg "Not an effects utility")`. *)

open Core
open Css

(* Note: Ring utilities create their own width/color variables as needed. Each
   ring size has its own width, and colors are set per-utility. *)

(* Note: blend modes are emitted directly as properties; no custom property is
   used for mix-blend-mode to avoid redundant defaults. *)

module Parse = Parse

(** {1 Shadow Utilities} *)

(* Shadow property rules for @property registration to trigger @layer properties *)
(* Only include the shadow and ring variables that Tailwind v4 actually includes for shadow utilities *)
let shadow_property_rules =
  [
    (* Shadow and ring variables - ordered as in Tailwind v4 *)
    Var.property Var.Shadow ~syntax:"*" ~inherits:false ~initial:"0 0 #0000";
    Var.property Var.Shadow_color ~syntax:"*" ~inherits:false ~initial:"initial";
    Var.property Var.Shadow_alpha ~syntax:"<percentage>" ~inherits:false
      ~initial:"100%";
    Var.property Var.Inset_shadow ~syntax:"*" ~inherits:false
      ~initial:"0 0 #0000";
    Var.property Var.Inset_shadow_color ~syntax:"*" ~inherits:false
      ~initial:"initial";
    Var.property Var.Inset_shadow_alpha ~syntax:"<percentage>" ~inherits:false
      ~initial:"100%";
    Var.property Var.Ring_color ~syntax:"*" ~inherits:false ~initial:"initial";
    Var.property Var.Ring_shadow ~syntax:"*" ~inherits:false
      ~initial:"0 0 #0000";
    Var.property Var.Inset_ring_color ~syntax:"*" ~inherits:false
      ~initial:"initial";
    Var.property Var.Inset_ring_shadow ~syntax:"*" ~inherits:false
      ~initial:"0 0 #0000";
    Var.property Var.Ring_inset ~syntax:"*" ~inherits:false ~initial:"initial";
    Var.property Var.Ring_offset_width ~syntax:"<length>" ~inherits:false
      ~initial:"0";
    Var.property Var.Ring_offset_color ~syntax:"*" ~inherits:false
      ~initial:"#fff";
    Var.property Var.Ring_offset_shadow ~syntax:"*" ~inherits:false
      ~initial:"0 0 #0000";
    (* Note: Ring_width is not included here as it's set by ring utilities, not
       shadow utilities *)
  ]

(* Helper function to create shadow utilities with the Tailwind v4 variable
   pattern *)
let make_shadow_utility name shadow_value =
  let shadow_def, _ = Var.utility Var.Shadow shadow_value in
  style name ~property_rules:shadow_property_rules
    [
      shadow_def;
      box_shadow
        (Raw
           "var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)");
    ]

let shadow_none = make_shadow_utility "shadow-none" "0 0 #0000"

let shadow_sm =
  make_shadow_utility "shadow-sm"
    "0 1px 3px 0 var(--tw-shadow-color,#0000001a),0 1px 2px -1px \
     var(--tw-shadow-color,#0000001a)"

let shadow =
  make_shadow_utility "shadow"
    "0 1px 3px 0 var(--tw-shadow-color,#0000001a),0 1px 2px -1px \
     var(--tw-shadow-color,#0000001a)"

let shadow_md =
  make_shadow_utility "shadow-md"
    "0 4px 6px -1px var(--tw-shadow-color,#0000001a),0 2px 4px -2px \
     var(--tw-shadow-color,#0000001a)"

let shadow_lg =
  make_shadow_utility "shadow-lg"
    "0 10px 15px -3px var(--tw-shadow-color,#0000001a),0 4px 6px -4px \
     var(--tw-shadow-color,#0000001a)"

let shadow_xl =
  make_shadow_utility "shadow-xl"
    "0 20px 25px -5px var(--tw-shadow-color,#0000001a),0 8px 10px -6px \
     var(--tw-shadow-color,#0000001a)"

let shadow_2xl =
  make_shadow_utility "shadow-2xl"
    "0 25px 50px -12px var(--tw-shadow-color,#00000040)"

let shadow_inner =
  let inset_shadow_def, _ =
    Var.utility Var.Inset_shadow
      "inset 0 2px 4px 0 var(--tw-shadow-color,#0000000f)"
  in
  style "shadow-inner" ~property_rules:shadow_property_rules
    [
      inset_shadow_def;
      box_shadow
        (Raw
           "var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)");
    ]

(** {1 Opacity Utilities} *)

let opacity_0 = style "opacity-0" [ opacity 0.0 ]
let opacity_5 = style "opacity-5" [ opacity 0.05 ]
let opacity_10 = style "opacity-10" [ opacity 0.1 ]
let opacity_20 = style "opacity-20" [ opacity 0.2 ]
let opacity_25 = style "opacity-25" [ opacity 0.25 ]
let opacity_30 = style "opacity-30" [ opacity 0.3 ]
let opacity_40 = style "opacity-40" [ opacity 0.4 ]
let opacity_50 = style "opacity-50" [ opacity 0.5 ]
let opacity_60 = style "opacity-60" [ opacity 0.6 ]
let opacity_70 = style "opacity-70" [ opacity 0.7 ]
let opacity_75 = style "opacity-75" [ opacity 0.75 ]
let opacity_80 = style "opacity-80" [ opacity 0.8 ]
let opacity_90 = style "opacity-90" [ opacity 0.9 ]
let opacity_95 = style "opacity-95" [ opacity 0.95 ]
let opacity_100 = style "opacity-100" [ opacity 1.0 ]

(** {1 Mix Blend Mode Utilities} *)

(* Mix blend modes not supported by Css module - would need raw property support *)
(* mix_blend_normal, mix_blend_multiply, mix_blend_screen, mix_blend_overlay,
   mix_blend_darken, mix_blend_lighten, mix_blend_color_dodge, mix_blend_color_burn,
   mix_blend_hard_light, mix_blend_soft_light, mix_blend_difference, mix_blend_exclusion,
   mix_blend_hue, mix_blend_saturation, mix_blend_color, mix_blend_luminosity *)

(** {1 Ring Utilities} *)

type ring_width = [ `None | `Xs | `Sm | `Md | `Lg | `Xl ]

let ring_internal (w : ring_width) =
  let width, class_suffix =
    match w with
    | `None -> ("0", "0")
    | `Xs -> ("1px", "1")
    | `Sm -> ("2px", "2")
    | `Md -> ("3px", "")
    | `Lg -> ("4px", "4")
    | `Xl -> ("8px", "8")
  in
  let class_name =
    if class_suffix = "" then "ring" else "ring-" ^ class_suffix
  in
  let width_len =
    match width with
    | "1px" -> Px 1
    | "2px" -> Px 2
    | "4px" -> Px 4
    | "8px" -> Px 8
    | _ -> Px 3
  in
  let width_def, width_var = Var.utility Var.Ring_width width_len in
  let color_def, color_var =
    Var.utility Var.Ring_color (Rgba { r = 59; g = 130; b = 246; a = 0.5 })
  in
  style class_name
    [
      width_def;
      color_def;
      box_shadow
        (Shadow
           {
             inset = false;
             h_offset = Px 0;
             v_offset = Px 0;
             blur = Px 0;
             spread = Var width_var;
             color = Var color_var;
           });
    ]

let ring_none = ring_internal `None
let ring_xs = ring_internal `Xs
let ring_sm = ring_internal `Sm
let ring = ring_internal `Md (* Default ring *)
let ring_md = ring_internal `Md
let ring_lg = ring_internal `Lg
let ring_xl = ring_internal `Xl

let ring_inset =
  (* Ring inset needs special handling - it's a string value "inset" Since we
     don't have a String kind, we'll handle this differently *)
  style "ring-inset"
    [ (* TODO: Add support for string-valued CSS variables or handle ring-inset
         specially *) ]

let ring_color color shade =
  let class_name =
    if Color.is_base_color color then Pp.str [ "ring-"; Color.pp color ]
    else Pp.str [ "ring-"; Color.pp color; "-"; string_of_int shade ]
  in
  style class_name []

(** {1 Transition Utilities} *)

let transition_none =
  style "transition-none" [ transition (Simple (None, S 0.0)) ]

let transition_all =
  style "transition-all"
    [
      transition (With_timing (All, Ms 150, Cubic_bezier (0.4, 0.0, 0.2, 1.0)));
    ]

let transition_colors =
  style "transition-colors"
    [
      transition
        (Multiple
           [
             With_timing
               ( Property "background-color",
                 Ms 150,
                 Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
             With_timing
               ( Property "border-color",
                 Ms 150,
                 Cubic_bezier (0.4, 0.0, 0.2, 1.0) );
             With_timing
               (Property "color", Ms 150, Cubic_bezier (0.4, 0.0, 0.2, 1.0));
             With_timing
               (Property "fill", Ms 150, Cubic_bezier (0.4, 0.0, 0.2, 1.0));
             With_timing
               (Property "stroke", Ms 150, Cubic_bezier (0.4, 0.0, 0.2, 1.0));
           ]);
    ]

let transition_opacity =
  style "transition-opacity"
    [
      transition
        (With_timing
           (Property "opacity", Ms 150, Cubic_bezier (0.4, 0.0, 0.2, 1.0)));
    ]

let transition_shadow =
  style "transition-shadow"
    [
      transition
        (With_timing
           (Property "box-shadow", Ms 150, Cubic_bezier (0.4, 0.0, 0.2, 1.0)));
    ]

let transition_transform =
  style "transition-transform"
    [
      transition
        (With_timing
           (Property "transform", Ms 150, Cubic_bezier (0.4, 0.0, 0.2, 1.0)));
    ]

let duration n =
  let class_name = "duration-" ^ string_of_int n in
  style class_name [ transition_duration (Ms n) ]

(** {1 Opacity Utility} *)

let opacity n =
  let class_name = "opacity-" ^ string_of_int n in
  let value = float_of_int n /. 100.0 in
  style class_name [ opacity value ]

(** {1 Mix Blend Mode Utilities} *)

let mix_blend_normal = style "mix-blend-normal" [ mix_blend_mode Normal ]
let mix_blend_multiply = style "mix-blend-multiply" [ mix_blend_mode Multiply ]
let mix_blend_screen = style "mix-blend-screen" [ mix_blend_mode Screen ]
let mix_blend_overlay = style "mix-blend-overlay" [ mix_blend_mode Overlay ]
let mix_blend_darken = style "mix-blend-darken" [ mix_blend_mode Darken ]
let mix_blend_lighten = style "mix-blend-lighten" [ mix_blend_mode Lighten ]

let mix_blend_color_dodge =
  style "mix-blend-color-dodge" [ mix_blend_mode Color_dodge ]

let mix_blend_color_burn =
  style "mix-blend-color-burn" [ mix_blend_mode Color_burn ]

let mix_blend_hard_light =
  style "mix-blend-hard-light" [ mix_blend_mode Hard_light ]

let mix_blend_soft_light =
  style "mix-blend-soft-light" [ mix_blend_mode Soft_light ]

let mix_blend_difference =
  style "mix-blend-difference" [ mix_blend_mode Difference ]

let mix_blend_exclusion =
  style "mix-blend-exclusion" [ mix_blend_mode Exclusion ]

let mix_blend_hue = style "mix-blend-hue" [ mix_blend_mode Hue ]

let mix_blend_saturation =
  style "mix-blend-saturation" [ mix_blend_mode Saturation ]

let mix_blend_color = style "mix-blend-color" [ mix_blend_mode Color ]

let mix_blend_luminosity =
  style "mix-blend-luminosity" [ mix_blend_mode Luminosity ]

(** {1 Parsing Functions} *)

let ( >|= ) = Parse.( >|= )

let of_string = function
  | [ "shadow"; "none" ] -> Ok shadow_none
  | [ "shadow"; "sm" ] -> Ok shadow_sm
  | [ "shadow" ] -> Ok shadow
  | [ "shadow"; "md" ] -> Ok shadow_md
  | [ "shadow"; "lg" ] -> Ok shadow_lg
  | [ "shadow"; "xl" ] -> Ok shadow_xl
  | [ "shadow"; "2xl" ] -> Ok shadow_2xl
  | [ "shadow"; "inner" ] -> Ok shadow_inner
  | [ "opacity"; n ] ->
      Parse.int_bounded ~name:"opacity" ~min:0 ~max:100 n >|= opacity
  | [ "ring" ] -> Ok ring
  | [ "ring"; "0" ] -> Ok ring_none
  | [ "ring"; "1" ] -> Ok ring_xs
  | [ "ring"; "2" ] -> Ok ring_sm
  | [ "ring"; "3" ] -> Ok ring_md
  | [ "ring"; "4" ] -> Ok ring_lg
  | [ "ring"; "8" ] -> Ok ring_xl
  | [ "ring"; "inset" ] -> Ok ring_inset
  | [ "transition" ] -> Ok transition_all
  | [ "transition"; "none" ] -> Ok transition_none
  | [ "transition"; "all" ] -> Ok transition_all
  | [ "transition"; "colors" ] -> Ok transition_colors
  | [ "transition"; "opacity" ] -> Ok transition_opacity
  | [ "transition"; "shadow" ] -> Ok transition_shadow
  | [ "transition"; "transform" ] -> Ok transition_transform
  | [ "duration"; n ] -> Parse.int_pos ~name:"duration" n >|= duration
  | [ "mix"; "blend"; "normal" ] -> Ok mix_blend_normal
  | [ "mix"; "blend"; "multiply" ] -> Ok mix_blend_multiply
  | [ "mix"; "blend"; "screen" ] -> Ok mix_blend_screen
  | [ "mix"; "blend"; "overlay" ] -> Ok mix_blend_overlay
  | [ "mix"; "blend"; "darken" ] -> Ok mix_blend_darken
  | [ "mix"; "blend"; "lighten" ] -> Ok mix_blend_lighten
  | [ "mix"; "blend"; "color-dodge" ] -> Ok mix_blend_color_dodge
  | [ "mix"; "blend"; "color-burn" ] -> Ok mix_blend_color_burn
  | [ "mix"; "blend"; "hard-light" ] -> Ok mix_blend_hard_light
  | [ "mix"; "blend"; "soft-light" ] -> Ok mix_blend_soft_light
  | [ "mix"; "blend"; "difference" ] -> Ok mix_blend_difference
  | [ "mix"; "blend"; "exclusion" ] -> Ok mix_blend_exclusion
  | [ "mix"; "blend"; "hue" ] -> Ok mix_blend_hue
  | [ "mix"; "blend"; "saturation" ] -> Ok mix_blend_saturation
  | [ "mix"; "blend"; "color" ] -> Ok mix_blend_color
  | [ "mix"; "blend"; "luminosity" ] -> Ok mix_blend_luminosity
  | _ -> Error (`Msg "Not an effects utility")
