(** Border utilities for border styles, widths, and radii *)

open Core
open Css

(** {1 Border Width Utilities} *)

let border = style "border" [ border_width (Px 1) ]
let border_0 = style "border-0" [ border_width (Px 0) ]
let border_2 = style "border-2" [ border_width (Px 2) ]
let border_4 = style "border-4" [ border_width (Px 4) ]
let border_8 = style "border-8" [ border_width (Px 8) ]
let border_t = style "border-t" [ border_top_width (Px 1) ]
let border_r = style "border-r" [ border_right_width (Px 1) ]
let border_b = style "border-b" [ border_bottom_width (Px 1) ]
let border_l = style "border-l" [ border_left_width (Px 1) ]

let border_x =
  style "border-x" [ border_left_width (Px 1); border_right_width (Px 1) ]

let border_y =
  style "border-y" [ border_top_width (Px 1); border_bottom_width (Px 1) ]

(** {1 Border Style Utilities} *)

let border_solid = style "border-solid" [ border_style Solid ]
let border_dashed = style "border-dashed" [ border_style Dashed ]
let border_dotted = style "border-dotted" [ border_style Dotted ]
let border_double = style "border-double" [ border_style Double ]
let border_none = style "border-none" [ border_style None ]

(** {1 Border Radius Utilities} *)

let rounded_none = style "rounded-none" [ border_radius (Px 0) ]
let rounded_sm = style "rounded-sm" [ border_radius (Rem 0.125) ]
let rounded = style "rounded" [ border_radius (Rem 0.25) ]
let rounded_md = style "rounded-md" [ border_radius (Rem 0.375) ]
let rounded_lg = style "rounded-lg" [ border_radius (Rem 0.5) ]
let rounded_xl = style "rounded-xl" [ border_radius (Rem 0.75) ]
let rounded_2xl = style "rounded-2xl" [ border_radius (Rem 1.0) ]
let rounded_3xl = style "rounded-3xl" [ border_radius (Rem 1.5) ]
let rounded_full = style "rounded-full" [ border_radius (Px 9999) ]

(* Individual corner radius utilities not supported by Css module: rounded_t,
   rounded_r, rounded_b, rounded_l rounded_tl, rounded_tr, rounded_br,
   rounded_bl These would require border_top_left_radius, etc. properties *)

(** {1 Outline Utilities} *)

(* outline_none not directly supported - would need outline_style property *)
(* outline not directly supported - would need outline_width property *)

let outline_offset_0 = style "outline-offset-0" [ outline_offset (Px 0) ]
let outline_offset_1 = style "outline-offset-1" [ outline_offset (Px 1) ]
let outline_offset_2 = style "outline-offset-2" [ outline_offset (Px 2) ]
let outline_offset_4 = style "outline-offset-4" [ outline_offset (Px 4) ]
let outline_offset_8 = style "outline-offset-8" [ outline_offset (Px 8) ]

(** {1 Ring Utilities} *)

let ring = style "ring" [ box_shadow "0 0 0 3px rgba(59, 130, 246, 0.5)" ]
let ring_0 = style "ring-0" [ box_shadow "0 0 0 0px rgba(59, 130, 246, 0.5)" ]
let ring_1 = style "ring-1" [ box_shadow "0 0 0 1px rgba(59, 130, 246, 0.5)" ]
let ring_2 = style "ring-2" [ box_shadow "0 0 0 2px rgba(59, 130, 246, 0.5)" ]
let ring_4 = style "ring-4" [ box_shadow "0 0 0 4px rgba(59, 130, 246, 0.5)" ]
let ring_8 = style "ring-8" [ box_shadow "0 0 0 8px rgba(59, 130, 246, 0.5)" ]

let ring_inset =
  style "ring-inset" [ box_shadow "inset 0 0 0 3px rgba(59, 130, 246, 0.5)" ]
