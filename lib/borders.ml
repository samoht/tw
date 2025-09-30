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

(* Create border style variable with @property for utilities that reference
   it *)
let border_style_var =
  Var.property_default Css.Border_style
    ~initial:(Solid : Css.border_style)
    "tw-border-style"

(* Helper for border utilities that reference the variable with @property
   default *)
let make_border_util class_name additional_props =
  let border_ref = Var.reference border_style_var in
  let property_rule =
    match Var.property_rule border_style_var with
    | Some rule -> rule
    | None -> Css.empty
  in
  style class_name ~property_rules:property_rule
    (border_style (Var border_ref) :: additional_props)

(* Helper for border style utilities that set the variable *)
let border_style_util class_name border_style_value =
  let decl, _ = Var.binding border_style_var border_style_value in
  style class_name [ decl; border_style border_style_value ]

let border = make_border_util "border" [ border_width (Px 1.) ]
let border_0 = make_border_util "border-0" [ border_width (Px 0.) ]
let border_2 = make_border_util "border-2" [ border_width (Px 2.) ]
let border_4 = make_border_util "border-4" [ border_width (Px 4.) ]
let border_8 = make_border_util "border-8" [ border_width (Px 8.) ]

(* Helper for border side utilities that reference the variable with @property
   default *)
let make_side_util class_name side_props_fn =
  let border_ref = Var.reference border_style_var in
  let property_rule =
    match Var.property_rule border_style_var with
    | Some rule -> rule
    | None -> Css.empty
  in
  style class_name ~property_rules:property_rule (side_props_fn border_ref)

let border_t =
  make_side_util "border-t" (fun border_var ->
      [ border_top_style (Var border_var); border_top_width (Px 1.) ])

let border_r =
  make_side_util "border-r" (fun border_var ->
      [ border_right_style (Var border_var); border_right_width (Px 1.) ])

let border_b =
  make_side_util "border-b" (fun border_var ->
      [ border_bottom_style (Var border_var); border_bottom_width (Px 1.) ])

let border_l =
  make_side_util "border-l" (fun border_var ->
      [ border_left_style (Var border_var); border_left_width (Px 1.) ])

let border_x =
  make_side_util "border-x" (fun border_var ->
      [
        border_left_style (Var border_var);
        border_left_width (Px 1.);
        border_right_style (Var border_var);
        border_right_width (Px 1.);
      ])

let border_y =
  make_side_util "border-y" (fun border_var ->
      [
        border_top_style (Var border_var);
        border_top_width (Px 1.);
        border_bottom_style (Var border_var);
        border_bottom_width (Px 1.);
      ])

(** Border side utilities with specific widths *)
let border_t_0 =
  make_side_util "border-t-0" (fun border_var ->
      [ border_top_style (Var border_var); border_top_width (Px 0.) ])

let border_t_2 =
  make_side_util "border-t-2" (fun border_var ->
      [ border_top_style (Var border_var); border_top_width (Px 2.) ])

let border_t_4 =
  make_side_util "border-t-4" (fun border_var ->
      [ border_top_style (Var border_var); border_top_width (Px 4.) ])

let border_t_8 =
  make_side_util "border-t-8" (fun border_var ->
      [ border_top_style (Var border_var); border_top_width (Px 8.) ])

let border_r_0 =
  make_side_util "border-r-0" (fun border_var ->
      [ border_right_style (Var border_var); border_right_width (Px 0.) ])

let border_r_2 =
  make_side_util "border-r-2" (fun border_var ->
      [ border_right_style (Var border_var); border_right_width (Px 2.) ])

let border_r_4 =
  make_side_util "border-r-4" (fun border_var ->
      [ border_right_style (Var border_var); border_right_width (Px 4.) ])

let border_r_8 =
  make_side_util "border-r-8" (fun border_var ->
      [ border_right_style (Var border_var); border_right_width (Px 8.) ])

let border_b_0 =
  make_side_util "border-b-0" (fun border_var ->
      [ border_bottom_style (Var border_var); border_bottom_width (Px 0.) ])

let border_b_2 =
  make_side_util "border-b-2" (fun border_var ->
      [ border_bottom_style (Var border_var); border_bottom_width (Px 2.) ])

let border_b_4 =
  make_side_util "border-b-4" (fun border_var ->
      [ border_bottom_style (Var border_var); border_bottom_width (Px 4.) ])

let border_b_8 =
  make_side_util "border-b-8" (fun border_var ->
      [ border_bottom_style (Var border_var); border_bottom_width (Px 8.) ])

let border_l_0 =
  make_side_util "border-l-0" (fun border_var ->
      [ border_left_style (Var border_var); border_left_width (Px 0.) ])

let border_l_2 =
  make_side_util "border-l-2" (fun border_var ->
      [ border_left_style (Var border_var); border_left_width (Px 2.) ])

let border_l_4 =
  make_side_util "border-l-4" (fun border_var ->
      [ border_left_style (Var border_var); border_left_width (Px 4.) ])

let border_l_8 =
  make_side_util "border-l-8" (fun border_var ->
      [ border_left_style (Var border_var); border_left_width (Px 8.) ])

(** {1 Border Style Utilities} *)

let border_solid = border_style_util "border-solid" Solid
let border_dashed = border_style_util "border-dashed" Dashed
let border_dotted = border_style_util "border-dotted" Dotted
let border_double = border_style_util "border-double" Double
let border_none = border_style_util "border-none" None

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

(* Create radius theme variables with fallback values for inline mode *)
let radius_sm_var = Var.theme Css.Length "radius-sm" ~order:(7, 0)
let radius_md_var = Var.theme Css.Length "radius-md" ~order:(7, 1)
let radius_lg_var = Var.theme Css.Length "radius-lg" ~order:(7, 2)
let radius_xl_var = Var.theme Css.Length "radius-xl" ~order:(7, 3)
let radius_2xl_var = Var.theme Css.Length "radius-2xl" ~order:(7, 4)
let radius_3xl_var = Var.theme Css.Length "radius-3xl" ~order:(7, 5)
let rounded_none = style "rounded-none" [ border_radius Zero ]

let rounded_sm =
  let decl, r = Var.binding radius_sm_var (Rem 0.25) in
  style "rounded-sm" (decl :: [ border_radius (Var r) ])

let rounded = style "rounded" [ border_radius (Rem 0.25) ]

let rounded_md =
  let decl, r = Var.binding radius_md_var (Rem 0.375) in
  style "rounded-md" (decl :: [ border_radius (Var r) ])

let rounded_lg =
  let decl, r = Var.binding radius_lg_var (Rem 0.5) in
  style "rounded-lg" (decl :: [ border_radius (Var r) ])

let rounded_xl =
  let decl, r = Var.binding radius_xl_var (Rem 0.75) in
  style "rounded-xl" (decl :: [ border_radius (Var r) ])

let rounded_2xl =
  let decl, r = Var.binding radius_2xl_var (Rem 1.0) in
  style "rounded-2xl" (decl :: [ border_radius (Var r) ])

let rounded_3xl =
  let decl, r = Var.binding radius_3xl_var (Rem 1.5) in
  style "rounded-3xl" (decl :: [ border_radius (Var r) ])

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

(** Suborder function for sorting border utilities within their priority group.
    Borders come before rounded, then outlines. *)
let suborder core =
  if
    String.starts_with ~prefix:"border-" core
    && not (String.starts_with ~prefix:"rounded" core)
  then 0
  else if String.starts_with ~prefix:"rounded" core then 1
  else if String.starts_with ~prefix:"outline" core then 2
  else 3
