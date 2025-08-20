(** Visual effects utilities for shadows, opacity, and filters *)

open Core
open Css

(** {1 Shadow Utilities} *)

let shadow_none = style "shadow-none" [ box_shadow "none" ]

let shadow_sm =
  style "shadow-sm" [ box_shadow "0 1px 2px 0 rgba(0, 0, 0, 0.05)" ]

let shadow =
  style "shadow"
    [
      box_shadow
        "0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.06)";
    ]

let shadow_md =
  style "shadow-md"
    [
      box_shadow
        "0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06)";
    ]

let shadow_lg =
  style "shadow-lg"
    [
      box_shadow
        "0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, \
         0.05)";
    ]

let shadow_xl =
  style "shadow-xl"
    [
      box_shadow
        "0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, \
         0.04)";
    ]

let shadow_2xl =
  style "shadow-2xl" [ box_shadow "0 25px 50px -12px rgba(0, 0, 0, 0.25)" ]

let shadow_inner =
  style "shadow-inner" [ box_shadow "inset 0 2px 4px 0 rgba(0, 0, 0, 0.06)" ]

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

(** {1 Filter Utilities} *)

let blur_none = style "blur-none" [ filter "blur(0)" ]
let blur_sm = style "blur-sm" [ filter "blur(4px)" ]
let blur = style "blur" [ filter "blur(8px)" ]
let blur_md = style "blur-md" [ filter "blur(12px)" ]
let blur_lg = style "blur-lg" [ filter "blur(16px)" ]
let blur_xl = style "blur-xl" [ filter "blur(24px)" ]
let blur_2xl = style "blur-2xl" [ filter "blur(40px)" ]
let blur_3xl = style "blur-3xl" [ filter "blur(64px)" ]

let brightness n =
  style
    (Printf.sprintf "brightness-%d" n)
    [ filter (Printf.sprintf "brightness(%d%%)" n) ]

let contrast n =
  style
    (Printf.sprintf "contrast-%d" n)
    [ filter (Printf.sprintf "contrast(%d%%)" n) ]

let grayscale n =
  style
    (Printf.sprintf "grayscale-%d" n)
    [ filter (Printf.sprintf "grayscale(%d%%)" n) ]

let hue_rotate n =
  style
    (Printf.sprintf "hue-rotate-%d" n)
    [ filter (Printf.sprintf "hue-rotate(%ddeg)" n) ]

let invert n =
  style
    (Printf.sprintf "invert-%d" n)
    [ filter (Printf.sprintf "invert(%d%%)" n) ]

let saturate n =
  style
    (Printf.sprintf "saturate-%d" n)
    [ filter (Printf.sprintf "saturate(%d%%)" n) ]

let sepia n =
  style
    (Printf.sprintf "sepia-%d" n)
    [ filter (Printf.sprintf "sepia(%d%%)" n) ]

(** {1 Backdrop Filter Utilities} *)

let backdrop_blur_none =
  style "backdrop-blur-none" [ backdrop_filter "blur(0)" ]

let backdrop_blur_sm = style "backdrop-blur-sm" [ backdrop_filter "blur(4px)" ]
let backdrop_blur = style "backdrop-blur" [ backdrop_filter "blur(8px)" ]
let backdrop_blur_md = style "backdrop-blur-md" [ backdrop_filter "blur(12px)" ]
let backdrop_blur_lg = style "backdrop-blur-lg" [ backdrop_filter "blur(16px)" ]
let backdrop_blur_xl = style "backdrop-blur-xl" [ backdrop_filter "blur(24px)" ]

let backdrop_blur_2xl =
  style "backdrop-blur-2xl" [ backdrop_filter "blur(40px)" ]

let backdrop_blur_3xl =
  style "backdrop-blur-3xl" [ backdrop_filter "blur(64px)" ]
