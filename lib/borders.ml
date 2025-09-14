(** Border utilities for border styles, widths, and radii

    What's included:
    - Border widths: `border`, `border-0/2/4/8`, and side/axis variants.
    - Border styles: `border-solid/dashed/dotted/double/none` via a CSS var.
    - Border radius: `rounded-*` and `rounded-full`.
    - Outline offsets and simple ring helpers.

    What's not:
    - Utility shorthands for per-corner radii (e.g., `rounded-tl`) are not
      provided here, but you can use typed helpers in `Css`:
      `border_top_left_radius`, `border_top_right_radius`,
      `border_bottom_right_radius`, `border_bottom_left_radius`.

    Parsing contract (`of_string`):
    - Accepts tokens like ["border"], ["border"; "t"], ["rounded"; "md"],
      ["ring"; "2"], ["outline"; "offset"; "4"]. Unknown tokens yield `Error
      (`Msg "Not a border utility")`. *)

open Core
open Css

(** {1 Border Width Utilities} *)

(* Create shared variable for border style *)
let _, border_style_var = Var.utility Var.Border_style Solid

(* Shared @property registration for border style variable *)
let property_rule =
  Css.stylesheet
    [
      Css.Property
        (Var.property ~inherits:false ~initial:Css.Solid Var.Border_style);
    ]

let border =
  style "border" ~property_rules:property_rule
    [ border_style (Var border_style_var); border_width (Px 1.) ]

let border_0 =
  style "border-0" ~property_rules:property_rule
    [ border_style (Var border_style_var); border_width (Px 0.) ]

let border_2 =
  style "border-2" ~property_rules:property_rule
    [ border_style (Var border_style_var); border_width (Px 2.) ]

let border_4 =
  style "border-4" ~property_rules:property_rule
    [ border_style (Var border_style_var); border_width (Px 4.) ]

let border_8 =
  style "border-8" ~property_rules:property_rule
    [ border_style (Var border_style_var); border_width (Px 8.) ]

let border_t =
  style "border-t" ~property_rules:property_rule
    [ border_top_style (Var border_style_var); border_top_width (Px 1.) ]

let border_r =
  style "border-r" ~property_rules:property_rule
    [ border_right_style (Var border_style_var); border_right_width (Px 1.) ]

let border_b =
  style "border-b" ~property_rules:property_rule
    [ border_bottom_style (Var border_style_var); border_bottom_width (Px 1.) ]

let border_l =
  style "border-l" ~property_rules:property_rule
    [ border_left_style (Var border_style_var); border_left_width (Px 1.) ]

let border_x =
  style "border-x" ~property_rules:property_rule
    [
      border_left_style (Var border_style_var);
      border_left_width (Px 1.);
      border_right_style (Var border_style_var);
      border_right_width (Px 1.);
    ]

let border_y =
  style "border-y" ~property_rules:property_rule
    [
      border_top_style (Var border_style_var);
      border_top_width (Px 1.);
      border_bottom_style (Var border_style_var);
      border_bottom_width (Px 1.);
    ]

(** Border side utilities with specific widths *)
let border_t_0 =
  style "border-t-0" ~property_rules:property_rule
    [ border_top_style (Var border_style_var); border_top_width (Px 0.) ]

let border_t_2 =
  style "border-t-2" ~property_rules:property_rule
    [ border_top_style (Var border_style_var); border_top_width (Px 2.) ]

let border_t_4 =
  style "border-t-4" ~property_rules:property_rule
    [ border_top_style (Var border_style_var); border_top_width (Px 4.) ]

let border_t_8 =
  style "border-t-8" ~property_rules:property_rule
    [ border_top_style (Var border_style_var); border_top_width (Px 8.) ]

let border_r_0 =
  style "border-r-0" ~property_rules:property_rule
    [ border_right_style (Var border_style_var); border_right_width (Px 0.) ]

let border_r_2 =
  style "border-r-2" ~property_rules:property_rule
    [ border_right_style (Var border_style_var); border_right_width (Px 2.) ]

let border_r_4 =
  style "border-r-4" ~property_rules:property_rule
    [ border_right_style (Var border_style_var); border_right_width (Px 4.) ]

let border_r_8 =
  style "border-r-8" ~property_rules:property_rule
    [ border_right_style (Var border_style_var); border_right_width (Px 8.) ]

let border_b_0 =
  style "border-b-0" ~property_rules:property_rule
    [ border_bottom_style (Var border_style_var); border_bottom_width (Px 0.) ]

let border_b_2 =
  style "border-b-2" ~property_rules:property_rule
    [ border_bottom_style (Var border_style_var); border_bottom_width (Px 2.) ]

let border_b_4 =
  style "border-b-4" ~property_rules:property_rule
    [ border_bottom_style (Var border_style_var); border_bottom_width (Px 4.) ]

let border_b_8 =
  style "border-b-8" ~property_rules:property_rule
    [ border_bottom_style (Var border_style_var); border_bottom_width (Px 8.) ]

let border_l_0 =
  style "border-l-0" ~property_rules:property_rule
    [ border_left_style (Var border_style_var); border_left_width (Px 0.) ]

let border_l_2 =
  style "border-l-2" ~property_rules:property_rule
    [ border_left_style (Var border_style_var); border_left_width (Px 2.) ]

let border_l_4 =
  style "border-l-4" ~property_rules:property_rule
    [ border_left_style (Var border_style_var); border_left_width (Px 4.) ]

let border_l_8 =
  style "border-l-8" ~property_rules:property_rule
    [ border_left_style (Var border_style_var); border_left_width (Px 8.) ]

(** {1 Border Style Utilities} *)

let border_solid =
  let def, _ = Var.utility Var.Border_style Solid in
  style "border-solid" [ def; border_style Solid ]

let border_dashed =
  let def, _ = Var.utility Var.Border_style Dashed in
  style "border-dashed" [ def; border_style Dashed ]

let border_dotted =
  let def, _ = Var.utility Var.Border_style Dotted in
  style "border-dotted" [ def; border_style Dotted ]

let border_double =
  let def, _ = Var.utility Var.Border_style Double in
  style "border-double" [ def; border_style Double ]

let border_none =
  let def, _ = Var.utility Var.Border_style None in
  style "border-none" [ def; border_style None ]

(* Border width utilities with semantic names matching tw.mli *)
let border_xs = border (* 1px *)
let border_sm = border_2 (* 2px *)
let border_md = border_4 (* 4px *)
let border_lg = border_4 (* 4px *)
let border_xl = border_8 (* 8px *)
let border_2xl = border_8 (* 8px *)
let border_3xl = border_8 (* 8px *)
let border_full = border_8 (* 8px *)

(** {1 Border Radius Utilities} *)

let rounded_none = style "rounded-none" [ border_radius Zero ]

let rounded_sm =
  let def, var_ref = Var.theme Var.Radius_sm (Rem 0.25) in
  style "rounded-sm" [ def; border_radius (Var var_ref) ]

let rounded = style "rounded" [ border_radius (Rem 0.25) ]

let rounded_md =
  let def, var_ref = Var.theme Var.Radius_md (Rem 0.375) in
  style "rounded-md" [ def; border_radius (Var var_ref) ]

let rounded_lg =
  let def, var_ref = Var.theme Var.Radius_lg (Rem 0.5) in
  style "rounded-lg" [ def; border_radius (Var var_ref) ]

let rounded_xl =
  let def, var_ref = Var.theme Var.Radius_xl (Rem 0.75) in
  style "rounded-xl" [ def; border_radius (Var var_ref) ]

let rounded_2xl =
  let def, var_ref = Var.theme Var.Radius_2xl (Rem 1.0) in
  style "rounded-2xl" [ def; border_radius (Var var_ref) ]

let rounded_3xl =
  let def, var_ref = Var.theme Var.Radius_3xl (Rem 1.5) in
  style "rounded-3xl" [ def; border_radius (Var var_ref) ]

let rounded_full =
  (* Tailwind v4 uses calc(infinity * 1px) which gets optimized to
     3.40282e38px *)
  style "rounded-full" [ border_radius (Calc Calc.(infinity * px 1.)) ]

(** Corner-specific rounded utilities *)
let rounded_t =
  style "rounded-t"
    [
      border_radius (Rem 0.25);
      (* TODO: Use individual corner properties when available *)
    ]

let rounded_r =
  style "rounded-r"
    [
      border_radius (Rem 0.25);
      (* TODO: Use individual corner properties when available *)
    ]

let rounded_b =
  style "rounded-b"
    [
      border_radius (Rem 0.25);
      (* TODO: Use individual corner properties when available *)
    ]

let rounded_l =
  style "rounded-l"
    [
      border_radius (Rem 0.25);
      (* TODO: Use individual corner properties when available *)
    ]

let rounded_tl = style "rounded-tl" [ border_radius (Rem 0.25) ]
let rounded_tr = style "rounded-tr" [ border_radius (Rem 0.25) ]
let rounded_br = style "rounded-br" [ border_radius (Rem 0.25) ]
let rounded_bl = style "rounded-bl" [ border_radius (Rem 0.25) ]

(** Corner-specific rounded utilities with sizes *)
let rounded_t_lg =
  style "rounded-t-lg" [ border_radius (Rem 0.5); border_radius (Rem 0.5) ]

let rounded_tl_2xl = style "rounded-tl-2xl" [ border_radius (Rem 1.0) ]

(** {1 Outline Utilities} *)

(* Outline style *)
let outline_none = style "outline-none" [ Css.outline_style Css.None ]

(* Outline offset *)
let outline_offset_0 = style "outline-offset-0" [ outline_offset (Px 0.) ]
let outline_offset_1 = style "outline-offset-1" [ outline_offset (Px 1.) ]
let outline_offset_2 = style "outline-offset-2" [ outline_offset (Px 2.) ]
let outline_offset_4 = style "outline-offset-4" [ outline_offset (Px 4.) ]
let outline_offset_8 = style "outline-offset-8" [ outline_offset (Px 8.) ]

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
  | [ "border"; "t"; "0" ] -> Ok border_t_0
  | [ "border"; "t"; "2" ] -> Ok border_t_2
  | [ "border"; "t"; "4" ] -> Ok border_t_4
  | [ "border"; "t"; "8" ] -> Ok border_t_8
  | [ "border"; "r"; "0" ] -> Ok border_r_0
  | [ "border"; "r"; "2" ] -> Ok border_r_2
  | [ "border"; "r"; "4" ] -> Ok border_r_4
  | [ "border"; "r"; "8" ] -> Ok border_r_8
  | [ "border"; "b"; "0" ] -> Ok border_b_0
  | [ "border"; "b"; "2" ] -> Ok border_b_2
  | [ "border"; "b"; "4" ] -> Ok border_b_4
  | [ "border"; "b"; "8" ] -> Ok border_b_8
  | [ "border"; "l"; "0" ] -> Ok border_l_0
  | [ "border"; "l"; "2" ] -> Ok border_l_2
  | [ "border"; "l"; "4" ] -> Ok border_l_4
  | [ "border"; "l"; "8" ] -> Ok border_l_8
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
  | [ "rounded"; "t" ] -> Ok rounded_t
  | [ "rounded"; "r" ] -> Ok rounded_r
  | [ "rounded"; "b" ] -> Ok rounded_b
  | [ "rounded"; "l" ] -> Ok rounded_l
  | [ "rounded"; "tl" ] -> Ok rounded_tl
  | [ "rounded"; "tr" ] -> Ok rounded_tr
  | [ "rounded"; "br" ] -> Ok rounded_br
  | [ "rounded"; "bl" ] -> Ok rounded_bl
  | [ "rounded"; "t"; "lg" ] -> Ok rounded_t_lg
  | [ "rounded"; "tl"; "2xl" ] -> Ok rounded_tl_2xl
  | [ "outline"; "offset"; "0" ] -> Ok outline_offset_0
  | [ "outline"; "offset"; "1" ] -> Ok outline_offset_1
  | [ "outline"; "offset"; "2" ] -> Ok outline_offset_2
  | [ "outline"; "offset"; "4" ] -> Ok outline_offset_4
  | [ "outline"; "offset"; "8" ] -> Ok outline_offset_8
  (* ring* handled in Effects *)
  | _ -> Error (`Msg "Not a border utility")
