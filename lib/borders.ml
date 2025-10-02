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

open Style
open Css

(** Error helper *)
let err_not_utility = Error (`Msg "Not a border utility")

(** {1 Utility Types} *)

(** Local border utility type *)
type t =
  | (* Border width utilities *)
    Border
  | Border_0
  | Border_2
  | Border_4
  | Border_8
  | (* Border side/axis utilities *)
    Border_t
  | Border_r
  | Border_b
  | Border_l
  | Border_x
  | Border_y
  | (* Border side utilities with widths *)
    Border_t_0
  | Border_t_2
  | Border_t_4
  | Border_t_8
  | Border_r_0
  | Border_r_2
  | Border_r_4
  | Border_r_8
  | Border_b_0
  | Border_b_2
  | Border_b_4
  | Border_b_8
  | Border_l_0
  | Border_l_2
  | Border_l_4
  | Border_l_8
  | (* Border style utilities *)
    Border_solid
  | Border_dashed
  | Border_dotted
  | Border_double
  | Border_none
  | (* Border radius utilities *)
    Rounded
  | Rounded_none
  | Rounded_sm
  | Rounded_md
  | Rounded_lg
  | Rounded_xl
  | Rounded_2xl
  | Rounded_3xl
  | Rounded_full
  | (* Side-specific rounded utilities *)
    Rounded_t
  | Rounded_r
  | Rounded_b
  | Rounded_l
  | (* Corner-specific rounded utilities *)
    Rounded_tl
  | Rounded_tr
  | Rounded_br
  | Rounded_bl
  | (* Corner-specific rounded utilities with sizes *)
    Rounded_t_lg
  | Rounded_tl_2xl
  | (* Outline utilities *)
    Outline_none
  | Outline_offset_0
  | Outline_offset_1
  | Outline_offset_2
  | Outline_offset_4
  | Outline_offset_8

(** Extensible variant for border utilities *)
type Utility.base += Borders of t

let wrap x = Borders x
let base x = Utility.base (wrap x)

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

let border' = make_border_util "border" [ border_width (Px 1.) ]
let border_0' = make_border_util "border-0" [ border_width (Px 0.) ]
let border_2' = make_border_util "border-2" [ border_width (Px 2.) ]
let border_4' = make_border_util "border-4" [ border_width (Px 4.) ]
let border_8' = make_border_util "border-8" [ border_width (Px 8.) ]
let border = base Border
let border_0 = base Border_0
let border_2 = base Border_2
let border_4 = base Border_4
let border_8 = base Border_8

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

let border_t' =
  make_side_util "border-t" (fun border_var ->
      [ border_top_style (Var border_var); border_top_width (Px 1.) ])

let border_t = Utility.base (Borders Border_t)

let border_r' =
  make_side_util "border-r" (fun border_var ->
      [ border_right_style (Var border_var); border_right_width (Px 1.) ])

let border_r = Utility.base (Borders Border_r)

let border_b' =
  make_side_util "border-b" (fun border_var ->
      [ border_bottom_style (Var border_var); border_bottom_width (Px 1.) ])

let border_b = Utility.base (Borders Border_b)

let border_l' =
  make_side_util "border-l" (fun border_var ->
      [ border_left_style (Var border_var); border_left_width (Px 1.) ])

let border_l = Utility.base (Borders Border_l)

let border_x' =
  make_side_util "border-x" (fun border_var ->
      [
        border_left_style (Var border_var);
        border_left_width (Px 1.);
        border_right_style (Var border_var);
        border_right_width (Px 1.);
      ])

let border_x = Utility.base (Borders Border_x)

let border_y' =
  make_side_util "border-y" (fun border_var ->
      [
        border_top_style (Var border_var);
        border_top_width (Px 1.);
        border_bottom_style (Var border_var);
        border_bottom_width (Px 1.);
      ])

let border_y = Utility.base (Borders Border_y)

(** Border side utilities with specific widths *)
let border_t_0' =
  make_side_util "border-t-0" (fun border_var ->
      [ border_top_style (Var border_var); border_top_width (Px 0.) ])

let border_t_2' =
  make_side_util "border-t-2" (fun border_var ->
      [ border_top_style (Var border_var); border_top_width (Px 2.) ])

let border_t_4' =
  make_side_util "border-t-4" (fun border_var ->
      [ border_top_style (Var border_var); border_top_width (Px 4.) ])

let border_t_8' =
  make_side_util "border-t-8" (fun border_var ->
      [ border_top_style (Var border_var); border_top_width (Px 8.) ])

let border_r_0' =
  make_side_util "border-r-0" (fun border_var ->
      [ border_right_style (Var border_var); border_right_width (Px 0.) ])

let border_r_2' =
  make_side_util "border-r-2" (fun border_var ->
      [ border_right_style (Var border_var); border_right_width (Px 2.) ])

let border_r_4' =
  make_side_util "border-r-4" (fun border_var ->
      [ border_right_style (Var border_var); border_right_width (Px 4.) ])

let border_r_8' =
  make_side_util "border-r-8" (fun border_var ->
      [ border_right_style (Var border_var); border_right_width (Px 8.) ])

let border_b_0' =
  make_side_util "border-b-0" (fun border_var ->
      [ border_bottom_style (Var border_var); border_bottom_width (Px 0.) ])

let border_b_2' =
  make_side_util "border-b-2" (fun border_var ->
      [ border_bottom_style (Var border_var); border_bottom_width (Px 2.) ])

let border_b_4' =
  make_side_util "border-b-4" (fun border_var ->
      [ border_bottom_style (Var border_var); border_bottom_width (Px 4.) ])

let border_b_8' =
  make_side_util "border-b-8" (fun border_var ->
      [ border_bottom_style (Var border_var); border_bottom_width (Px 8.) ])

let border_l_0' =
  make_side_util "border-l-0" (fun border_var ->
      [ border_left_style (Var border_var); border_left_width (Px 0.) ])

let border_l_2' =
  make_side_util "border-l-2" (fun border_var ->
      [ border_left_style (Var border_var); border_left_width (Px 2.) ])

let border_l_4' =
  make_side_util "border-l-4" (fun border_var ->
      [ border_left_style (Var border_var); border_left_width (Px 4.) ])

let border_l_8' =
  make_side_util "border-l-8" (fun border_var ->
      [ border_left_style (Var border_var); border_left_width (Px 8.) ])

(** {1 Border Style Utilities} *)

let border_solid' = border_style_util "border-solid" Solid
let border_solid = Utility.base (Borders Border_solid)
let border_dashed' = border_style_util "border-dashed" Dashed
let border_dashed = Utility.base (Borders Border_dashed)
let border_dotted' = border_style_util "border-dotted" Dotted
let border_dotted = Utility.base (Borders Border_dotted)
let border_double' = border_style_util "border-double" Double
let border_double = Utility.base (Borders Border_double)
let border_none' = border_style_util "border-none" None
let border_none = Utility.base (Borders Border_none)

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
let rounded_none' = style "rounded-none" [ border_radius Zero ]
let rounded_none = Utility.base (Borders Rounded_none)

let rounded_sm' =
  let decl, r = Var.binding radius_sm_var (Rem 0.25) in
  style "rounded-sm" (decl :: [ border_radius (Var r) ])

let rounded_sm = Utility.base (Borders Rounded_sm)
let rounded' = style "rounded" [ border_radius (Rem 0.25) ]
let rounded = Utility.base (Borders Rounded)

let rounded_md' =
  let decl, r = Var.binding radius_md_var (Rem 0.375) in
  style "rounded-md" (decl :: [ border_radius (Var r) ])

let rounded_md = Utility.base (Borders Rounded_md)

let rounded_lg' =
  let decl, r = Var.binding radius_lg_var (Rem 0.5) in
  style "rounded-lg" (decl :: [ border_radius (Var r) ])

let rounded_lg = Utility.base (Borders Rounded_lg)

let rounded_xl' =
  let decl, r = Var.binding radius_xl_var (Rem 0.75) in
  style "rounded-xl" (decl :: [ border_radius (Var r) ])

let rounded_xl = Utility.base (Borders Rounded_xl)

let rounded_2xl' =
  let decl, r = Var.binding radius_2xl_var (Rem 1.0) in
  style "rounded-2xl" (decl :: [ border_radius (Var r) ])

let rounded_2xl = Utility.base (Borders Rounded_2xl)

let rounded_3xl' =
  let decl, r = Var.binding radius_3xl_var (Rem 1.5) in
  style "rounded-3xl" (decl :: [ border_radius (Var r) ])

let rounded_3xl = Utility.base (Borders Rounded_3xl)

let rounded_full' =
  (* Tailwind v4 uses calc(infinity * 1px) which gets optimized to
     3.40282e38px *)
  style "rounded-full" [ border_radius (Calc Calc.(infinity * px 1.)) ]

let rounded_full = Utility.base (Borders Rounded_full)

(** Corner-specific rounded utilities *)
let rounded_t' =
  style "rounded-t"
    [
      border_radius (Rem 0.25);
      (* TODO: Use individual corner properties when available *)
    ]

let rounded_r' =
  style "rounded-r"
    [
      border_radius (Rem 0.25);
      (* TODO: Use individual corner properties when available *)
    ]

let rounded_b' =
  style "rounded-b"
    [
      border_radius (Rem 0.25);
      (* TODO: Use individual corner properties when available *)
    ]

let rounded_l' =
  style "rounded-l"
    [
      border_radius (Rem 0.25);
      (* TODO: Use individual corner properties when available *)
    ]

let rounded_tl' = style "rounded-tl" [ border_radius (Rem 0.25) ]
let rounded_tr' = style "rounded-tr" [ border_radius (Rem 0.25) ]
let rounded_br' = style "rounded-br" [ border_radius (Rem 0.25) ]
let rounded_bl' = style "rounded-bl" [ border_radius (Rem 0.25) ]

(** Corner-specific rounded utilities with sizes *)

let rounded_t_lg' =
  style "rounded-t-lg" [ border_radius (Rem 0.5); border_radius (Rem 0.5) ]

let rounded_tl_2xl' = style "rounded-tl-2xl" [ border_radius (Rem 1.0) ]

(** {1 Outline Utilities} *)

(* Outline style *)

let outline_none' = style "outline-none" [ Css.outline_style Css.None ]

(* Outline offset *)
let outline_none = Utility.base (Borders Outline_none)
let outline_offset_0' = style "outline-offset-0" [ outline_offset (Px 0.) ]
let outline_offset_0 = Utility.base (Borders Outline_offset_0)
let outline_offset_1' = style "outline-offset-1" [ outline_offset (Px 1.) ]
let outline_offset_1 = Utility.base (Borders Outline_offset_1)
let outline_offset_2' = style "outline-offset-2" [ outline_offset (Px 2.) ]
let outline_offset_2 = Utility.base (Borders Outline_offset_2)
let outline_offset_4' = style "outline-offset-4" [ outline_offset (Px 4.) ]
let outline_offset_4 = Utility.base (Borders Outline_offset_4)
let outline_offset_8' = style "outline-offset-8" [ outline_offset (Px 8.) ]

(** {1 Conversion Functions} *)

let outline_offset_8 = Utility.base (Borders Outline_offset_8)

let to_style = function
  (* Border width utilities *)
  | Border -> border'
  | Border_0 -> border_0'
  | Border_2 -> border_2'
  | Border_4 -> border_4'
  | Border_8 -> border_8'
  (* Border side/axis utilities *)
  | Border_t -> border_t'
  | Border_r -> border_r'
  | Border_b -> border_b'
  | Border_l -> border_l'
  | Border_x -> border_x'
  | Border_y -> border_y'
  (* Border side utilities with widths *)
  | Border_t_0 -> border_t_0'
  | Border_t_2 -> border_t_2'
  | Border_t_4 -> border_t_4'
  | Border_t_8 -> border_t_8'
  | Border_r_0 -> border_r_0'
  | Border_r_2 -> border_r_2'
  | Border_r_4 -> border_r_4'
  | Border_r_8 -> border_r_8'
  | Border_b_0 -> border_b_0'
  | Border_b_2 -> border_b_2'
  | Border_b_4 -> border_b_4'
  | Border_b_8 -> border_b_8'
  | Border_l_0 -> border_l_0'
  | Border_l_2 -> border_l_2'
  | Border_l_4 -> border_l_4'
  | Border_l_8 -> border_l_8'
  (* Border style utilities *)
  | Border_solid -> border_solid'
  | Border_dashed -> border_dashed'
  | Border_dotted -> border_dotted'
  | Border_double -> border_double'
  | Border_none -> border_none'
  (* Border radius utilities *)
  | Rounded -> rounded'
  | Rounded_none -> rounded_none'
  | Rounded_sm -> rounded_sm'
  | Rounded_md -> rounded_md'
  | Rounded_lg -> rounded_lg'
  | Rounded_xl -> rounded_xl'
  | Rounded_2xl -> rounded_2xl'
  | Rounded_3xl -> rounded_3xl'
  | Rounded_full -> rounded_full'
  (* Side-specific rounded' utilities *)
  | Rounded_t -> rounded_t'
  | Rounded_r -> rounded_r'
  | Rounded_b -> rounded_b'
  | Rounded_l -> rounded_l'
  (* Corner-specific rounded' utilities *)
  | Rounded_tl -> rounded_tl'
  | Rounded_tr -> rounded_tr'
  | Rounded_br -> rounded_br'
  | Rounded_bl -> rounded_bl'
  (* Corner-specific rounded' utilities with sizes *)
  | Rounded_t_lg -> rounded_t_lg'
  | Rounded_tl_2xl -> rounded_tl_2xl'
  (* Outline utilities *)
  | Outline_none -> outline_none'
  | Outline_offset_0 -> outline_offset_0'
  | Outline_offset_1 -> outline_offset_1'
  | Outline_offset_2 -> outline_offset_2'
  | Outline_offset_4 -> outline_offset_4'
  | Outline_offset_8 -> outline_offset_8'

(** {1 Parsing Functions} *)

let of_string = function
  | [ "border" ] -> Ok Border
  | [ "border"; "0" ] -> Ok Border_0
  | [ "border"; "2" ] -> Ok Border_2
  | [ "border"; "4" ] -> Ok Border_4
  | [ "border"; "8" ] -> Ok Border_8
  | [ "border"; "t" ] -> Ok Border_t
  | [ "border"; "r" ] -> Ok Border_r
  | [ "border"; "b" ] -> Ok Border_b
  | [ "border"; "l" ] -> Ok Border_l
  | [ "border"; "x" ] -> Ok Border_x
  | [ "border"; "y" ] -> Ok Border_y
  | [ "border"; "t"; "0" ] -> Ok Border_t_0
  | [ "border"; "t"; "2" ] -> Ok Border_t_2
  | [ "border"; "t"; "4" ] -> Ok Border_t_4
  | [ "border"; "t"; "8" ] -> Ok Border_t_8
  | [ "border"; "r"; "0" ] -> Ok Border_r_0
  | [ "border"; "r"; "2" ] -> Ok Border_r_2
  | [ "border"; "r"; "4" ] -> Ok Border_r_4
  | [ "border"; "r"; "8" ] -> Ok Border_r_8
  | [ "border"; "b"; "0" ] -> Ok Border_b_0
  | [ "border"; "b"; "2" ] -> Ok Border_b_2
  | [ "border"; "b"; "4" ] -> Ok Border_b_4
  | [ "border"; "b"; "8" ] -> Ok Border_b_8
  | [ "border"; "l"; "0" ] -> Ok Border_l_0
  | [ "border"; "l"; "2" ] -> Ok Border_l_2
  | [ "border"; "l"; "4" ] -> Ok Border_l_4
  | [ "border"; "l"; "8" ] -> Ok Border_l_8
  | [ "border"; "solid" ] -> Ok Border_solid
  | [ "border"; "dashed" ] -> Ok Border_dashed
  | [ "border"; "dotted" ] -> Ok Border_dotted
  | [ "border"; "double" ] -> Ok Border_double
  | [ "border"; "none" ] -> Ok Border_none
  | [ "rounded" ] -> Ok Rounded
  | [ "rounded"; "none" ] -> Ok Rounded_none
  | [ "rounded"; "sm" ] -> Ok Rounded_sm
  | [ "rounded"; "md" ] -> Ok Rounded_md
  | [ "rounded"; "lg" ] -> Ok Rounded_lg
  | [ "rounded"; "xl" ] -> Ok Rounded_xl
  | [ "rounded"; "2xl" ] -> Ok Rounded_2xl
  | [ "rounded"; "3xl" ] -> Ok Rounded_3xl
  | [ "rounded"; "full" ] -> Ok Rounded_full
  | [ "rounded"; "t" ] -> Ok Rounded_t
  | [ "rounded"; "r" ] -> Ok Rounded_r
  | [ "rounded"; "b" ] -> Ok Rounded_b
  | [ "rounded"; "l" ] -> Ok Rounded_l
  | [ "rounded"; "tl" ] -> Ok Rounded_tl
  | [ "rounded"; "tr" ] -> Ok Rounded_tr
  | [ "rounded"; "br" ] -> Ok Rounded_br
  | [ "rounded"; "bl" ] -> Ok Rounded_bl
  | [ "rounded"; "t"; "lg" ] -> Ok Rounded_t_lg
  | [ "rounded"; "tl"; "2xl" ] -> Ok Rounded_tl_2xl
  | [ "outline"; "offset"; "0" ] -> Ok Outline_offset_0
  | [ "outline"; "offset"; "1" ] -> Ok Outline_offset_1
  | [ "outline"; "offset"; "2" ] -> Ok Outline_offset_2
  | [ "outline"; "offset"; "4" ] -> Ok Outline_offset_4
  | [ "outline"; "offset"; "8" ] -> Ok Outline_offset_8
  (* ring* handled in Effects *)
  | _ -> err_not_utility

(** Suborder function for sorting border utilities within their priority group.
    Borders come before rounded, then outlines. *)
let suborder = function
  (* Border width utilities (0-99) *)
  | Border -> 0
  | Border_0 -> 1
  | Border_2 -> 2
  | Border_4 -> 3
  | Border_8 -> 4
  (* Border side/axis utilities (100-199) *)
  | Border_t -> 100
  | Border_r -> 101
  | Border_b -> 102
  | Border_l -> 103
  | Border_x -> 104
  | Border_y -> 105
  (* Border side utilities with widths (200-399) *)
  | Border_t_0 -> 200
  | Border_t_2 -> 201
  | Border_t_4 -> 202
  | Border_t_8 -> 203
  | Border_r_0 -> 210
  | Border_r_2 -> 211
  | Border_r_4 -> 212
  | Border_r_8 -> 213
  | Border_b_0 -> 220
  | Border_b_2 -> 221
  | Border_b_4 -> 222
  | Border_b_8 -> 223
  | Border_l_0 -> 230
  | Border_l_2 -> 231
  | Border_l_4 -> 232
  | Border_l_8 -> 233
  (* Border style utilities (400-499) *)
  | Border_solid -> 400
  | Border_dashed -> 401
  | Border_dotted -> 402
  | Border_double -> 403
  | Border_none -> 404
  (* Border radius utilities (1000-1099) *)
  | Rounded -> 1000
  | Rounded_none -> 1001
  | Rounded_sm -> 1002
  | Rounded_md -> 1003
  | Rounded_lg -> 1004
  | Rounded_xl -> 1005
  | Rounded_2xl -> 1006
  | Rounded_3xl -> 1007
  | Rounded_full -> 1008
  (* Side-specific rounded utilities (1100-1199) *)
  | Rounded_t -> 1100
  | Rounded_r -> 1101
  | Rounded_b -> 1102
  | Rounded_l -> 1103
  (* Corner-specific rounded utilities (1200-1299) *)
  | Rounded_tl -> 1200
  | Rounded_tr -> 1201
  | Rounded_br -> 1202
  | Rounded_bl -> 1203
  (* Corner-specific rounded utilities with sizes (1300-1399) *)
  | Rounded_t_lg -> 1300
  | Rounded_tl_2xl -> 1301
  (* Outline utilities (2000-2099) *)
  | Outline_none -> 1999
  | Outline_offset_0 -> 2000
  | Outline_offset_1 -> 2001
  | Outline_offset_2 -> 2002
  | Outline_offset_4 -> 2003
  | Outline_offset_8 -> 2004

(** Priority for border utilities *)
let priority = 30

(** Typed handler for border utilities *)
let handler : t Utility.handler = { to_style; priority; suborder; of_string }

(** Wrapper functions for extensible variant *)
let unwrap = function Borders x -> Some x | _ -> None

(** Register border handler with Utility system *)

let () = Utility.register ~wrap ~unwrap handler

module Handler = struct
  type nonrec t = t

  let of_string = of_string
  let suborder = suborder
  let to_style = to_style
  let order x = (priority, suborder x)
end
