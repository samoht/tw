(** Border utilities for border styles, widths, and radii *)

open Core
open Css

(** {1 Border Width Utilities} *)

let border =
  style "border"
    [ border_style (Var (var "tw-border-style")); border_width (Px 1) ]

let border_0 =
  style "border-0"
    [ border_style (Var (var "tw-border-style")); border_width (Px 0) ]

let border_2 =
  style "border-2"
    [ border_style (Var (var "tw-border-style")); border_width (Px 2) ]

let border_4 =
  style "border-4"
    [ border_style (Var (var "tw-border-style")); border_width (Px 4) ]

let border_8 =
  style "border-8"
    [ border_style (Var (var "tw-border-style")); border_width (Px 8) ]

let border_t =
  style "border-t"
    [ border_top_style (Var (var "tw-border-style")); border_top_width (Px 1) ]

let border_r =
  style "border-r"
    [
      border_right_style (Var (var "tw-border-style"));
      border_right_width (Px 1);
    ]

let border_b =
  style "border-b"
    [
      border_bottom_style (Var (var "tw-border-style"));
      border_bottom_width (Px 1);
    ]

let border_l =
  style "border-l"
    [
      border_left_style (Var (var "tw-border-style")); border_left_width (Px 1);
    ]

let border_x =
  style "border-x"
    [
      border_left_style (Var (var "tw-border-style"));
      border_left_width (Px 1);
      border_right_style (Var (var "tw-border-style"));
      border_right_width (Px 1);
    ]

let border_y =
  style "border-y"
    [
      border_top_style (Var (var "tw-border-style"));
      border_top_width (Px 1);
      border_bottom_style (Var (var "tw-border-style"));
      border_bottom_width (Px 1);
    ]

(** {1 Border Style Utilities} *)

let border_solid =
  style_with_vars "border-solid"
    [ custom_property "--tw-border-style" "solid"; border_style Solid ]
    []

let border_dashed =
  style_with_vars "border-dashed"
    [ custom_property "--tw-border-style" "dashed"; border_style Dashed ]
    []

let border_dotted =
  style_with_vars "border-dotted"
    [ custom_property "--tw-border-style" "dotted"; border_style Dotted ]
    []

let border_double =
  style_with_vars "border-double"
    [ custom_property "--tw-border-style" "double"; border_style Double ]
    []

let border_none =
  style_with_vars "border-none"
    [ custom_property "--tw-border-style" "none"; border_style None ]
    []

(** {1 Border Radius Utilities} *)

let rounded_none =
  style_with_vars "rounded-none"
    [ border_radius (Var { name = "radius-none"; fallback = None }) ]
    [ Radius { name = "none"; value = "0" } ]

let rounded_sm =
  style_with_vars "rounded-sm"
    [ border_radius (Var { name = "radius-sm"; fallback = None }) ]
    [ Radius { name = "sm"; value = ".125rem" } ]

let rounded =
  style_with_vars "rounded"
    [ border_radius (Var { name = "radius-default"; fallback = None }) ]
    [ Radius { name = "default"; value = ".25rem" } ]

let rounded_md =
  style_with_vars "rounded-md"
    [ border_radius (Var { name = "radius-md"; fallback = None }) ]
    [ Radius { name = "md"; value = ".375rem" } ]

let rounded_lg =
  style_with_vars "rounded-lg"
    [ border_radius (Var { name = "radius-lg"; fallback = None }) ]
    [ Radius { name = "lg"; value = ".5rem" } ]

let rounded_xl =
  style_with_vars "rounded-xl"
    [ border_radius (Var { name = "radius-xl"; fallback = None }) ]
    [ Radius { name = "xl"; value = ".75rem" } ]

let rounded_2xl =
  style_with_vars "rounded-2xl"
    [ border_radius (Var { name = "radius-2xl"; fallback = None }) ]
    [ Radius { name = "2xl"; value = "1rem" } ]

let rounded_3xl =
  style_with_vars "rounded-3xl"
    [ border_radius (Var { name = "radius-3xl"; fallback = None }) ]
    [ Radius { name = "3xl"; value = "1.5rem" } ]

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

(** {1 Parsing Functions} *)

let of_string = function
  | [ "border" ] -> Ok border
  | [ "border"; "0" ] -> Ok border_0
  | [ "border"; "2" ] -> Ok border_2
  | [ "border"; "4" ] -> Ok border_4
  | [ "border"; "8" ] -> Ok border_8
  | [ "border"; "t" ] -> Ok border_t
  | [ "border"; "r" ] -> Ok border_r
  | [ "border"; "b" ] -> Ok border_b
  | [ "border"; "l" ] -> Ok border_l
  | [ "border"; "x" ] -> Ok border_x
  | [ "border"; "y" ] -> Ok border_y
  | [ "border"; "solid" ] -> Ok border_solid
  | [ "border"; "dashed" ] -> Ok border_dashed
  | [ "border"; "dotted" ] -> Ok border_dotted
  | [ "border"; "double" ] -> Ok border_double
  | [ "border"; "none" ] -> Ok border_none
  | [ "rounded" ] -> Ok rounded
  | [ "rounded"; "none" ] -> Ok rounded_none
  | [ "rounded"; "sm" ] -> Ok rounded_sm
  | [ "rounded"; "md" ] -> Ok rounded_md
  | [ "rounded"; "lg" ] -> Ok rounded_lg
  | [ "rounded"; "xl" ] -> Ok rounded_xl
  | [ "rounded"; "2xl" ] -> Ok rounded_2xl
  | [ "rounded"; "3xl" ] -> Ok rounded_3xl
  | [ "rounded"; "full" ] -> Ok rounded_full
  | [ "outline"; "offset"; "0" ] -> Ok outline_offset_0
  | [ "outline"; "offset"; "1" ] -> Ok outline_offset_1
  | [ "outline"; "offset"; "2" ] -> Ok outline_offset_2
  | [ "outline"; "offset"; "4" ] -> Ok outline_offset_4
  | [ "outline"; "offset"; "8" ] -> Ok outline_offset_8
  | [ "ring" ] -> Ok ring
  | [ "ring"; "0" ] -> Ok ring_0
  | [ "ring"; "1" ] -> Ok ring_1
  | [ "ring"; "2" ] -> Ok ring_2
  | [ "ring"; "4" ] -> Ok ring_4
  | [ "ring"; "8" ] -> Ok ring_8
  | [ "ring"; "inset" ] -> Ok ring_inset
  | _ -> Error (`Msg "Not a border utility")
