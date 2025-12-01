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

module Handler = struct
  open Style
  open Css

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
    | (* Border color utilities *)
      Border_color of Color.color * int
    | Border_transparent
    | Border_current
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

  type Utility.base += Self of t

  let name = "borders"
  let priority = 16

  (* Create border style variable with @property for utilities that reference
     it *)
  let border_style_var =
    Var.property_default Css.Border_style
      ~initial:(Solid : Css.border_style)
      "tw-border-style"

  (* Helper for border utilities that reference the variable with @property
     default *)
  let make_border_util additional_props =
    let border_ref = Var.reference border_style_var in
    let property_rule =
      match Var.property_rule border_style_var with
      | Some rule -> rule
      | None -> Css.empty
    in
    style ~property_rules:property_rule
      (border_style (Var border_ref) :: additional_props)

  (* Helper for border style utilities that set the variable *)
  let border_style_util border_style_value =
    let decl, _ = Var.binding border_style_var border_style_value in
    style [ decl; border_style border_style_value ]

  let border = make_border_util [ Css.border_width (Px 1.) ]
  let border_0 = make_border_util [ Css.border_width (Px 0.) ]
  let border_2 = make_border_util [ Css.border_width (Px 2.) ]
  let border_4 = make_border_util [ Css.border_width (Px 4.) ]
  let border_8 = make_border_util [ Css.border_width (Px 8.) ]

  (* Helper for border side utilities that reference the variable with @property
     default *)
  let make_side_util side_props_fn =
    let border_ref = Var.reference border_style_var in
    let property_rule =
      match Var.property_rule border_style_var with
      | Some rule -> rule
      | None -> Css.empty
    in
    style ~property_rules:property_rule (side_props_fn border_ref)

  let border_t =
    make_side_util (fun border_var ->
        [ border_top_style (Var border_var); border_top_width (Px 1.) ])

  let border_r =
    make_side_util (fun border_var ->
        [ border_right_style (Var border_var); border_right_width (Px 1.) ])

  let border_b =
    make_side_util (fun border_var ->
        [ border_bottom_style (Var border_var); border_bottom_width (Px 1.) ])

  let border_l =
    make_side_util (fun border_var ->
        [ border_left_style (Var border_var); border_left_width (Px 1.) ])

  let border_x =
    make_side_util (fun border_var ->
        [
          border_left_style (Var border_var);
          border_left_width (Px 1.);
          border_right_style (Var border_var);
          border_right_width (Px 1.);
        ])

  let border_y =
    make_side_util (fun border_var ->
        [
          border_top_style (Var border_var);
          border_top_width (Px 1.);
          border_bottom_style (Var border_var);
          border_bottom_width (Px 1.);
        ])

  (** Border side utilities with specific widths *)
  let border_t_0 =
    make_side_util (fun border_var ->
        [ border_top_style (Var border_var); border_top_width (Px 0.) ])

  let border_t_2 =
    make_side_util (fun border_var ->
        [ border_top_style (Var border_var); border_top_width (Px 2.) ])

  let border_t_4 =
    make_side_util (fun border_var ->
        [ border_top_style (Var border_var); border_top_width (Px 4.) ])

  let border_t_8 =
    make_side_util (fun border_var ->
        [ border_top_style (Var border_var); border_top_width (Px 8.) ])

  let border_r_0 =
    make_side_util (fun border_var ->
        [ border_right_style (Var border_var); border_right_width (Px 0.) ])

  let border_r_2 =
    make_side_util (fun border_var ->
        [ border_right_style (Var border_var); border_right_width (Px 2.) ])

  let border_r_4 =
    make_side_util (fun border_var ->
        [ border_right_style (Var border_var); border_right_width (Px 4.) ])

  let border_r_8 =
    make_side_util (fun border_var ->
        [ border_right_style (Var border_var); border_right_width (Px 8.) ])

  let border_b_0 =
    make_side_util (fun border_var ->
        [ border_bottom_style (Var border_var); border_bottom_width (Px 0.) ])

  let border_b_2 =
    make_side_util (fun border_var ->
        [ border_bottom_style (Var border_var); border_bottom_width (Px 2.) ])

  let border_b_4 =
    make_side_util (fun border_var ->
        [ border_bottom_style (Var border_var); border_bottom_width (Px 4.) ])

  let border_b_8 =
    make_side_util (fun border_var ->
        [ border_bottom_style (Var border_var); border_bottom_width (Px 8.) ])

  let border_l_0 =
    make_side_util (fun border_var ->
        [ border_left_style (Var border_var); border_left_width (Px 0.) ])

  let border_l_2 =
    make_side_util (fun border_var ->
        [ border_left_style (Var border_var); border_left_width (Px 2.) ])

  let border_l_4 =
    make_side_util (fun border_var ->
        [ border_left_style (Var border_var); border_left_width (Px 4.) ])

  let border_l_8 =
    make_side_util (fun border_var ->
        [ border_left_style (Var border_var); border_left_width (Px 8.) ])

  let border_solid = border_style_util Solid
  let border_dashed = border_style_util Dashed
  let border_dotted = border_style_util Dotted
  let border_double = border_style_util Double
  let border_none = border_style_util None

  (* Border color utilities *)
  let border_color' color shade =
    if Color.is_custom_color color then
      let css_color = Color.to_css color shade in
      style [ Css.border_color css_color ]
    else
      let color_var = Color.get_color_var color shade in
      let color_value =
        Color.to_css color (if Color.is_base_color color then 500 else shade)
      in
      let decl, color_ref = Var.binding color_var color_value in
      style (decl :: [ Css.border_color (Var color_ref) ])

  let border_transparent' = style [ Css.border_color Transparent ]
  let border_current' = style [ Css.border_color Current ]

  (* Create radius theme variables with fallback values for inline mode *)
  let radius_sm_var = Var.theme Css.Length "radius-sm" ~order:(7, 0)
  let radius_md_var = Var.theme Css.Length "radius-md" ~order:(7, 1)
  let radius_lg_var = Var.theme Css.Length "radius-lg" ~order:(7, 2)
  let radius_xl_var = Var.theme Css.Length "radius-xl" ~order:(7, 3)
  let radius_2xl_var = Var.theme Css.Length "radius-2xl" ~order:(7, 4)
  let radius_3xl_var = Var.theme Css.Length "radius-3xl" ~order:(7, 5)
  let rounded_none = style [ Css.border_radius Zero ]

  let rounded_sm =
    let decl, r = Var.binding radius_sm_var (Rem 0.25) in
    style (decl :: [ Css.border_radius (Var r) ])

  let rounded = style [ Css.border_radius (Rem 0.25) ]

  let rounded_md =
    let decl, r = Var.binding radius_md_var (Rem 0.375) in
    style (decl :: [ Css.border_radius (Var r) ])

  let rounded_lg =
    let decl, r = Var.binding radius_lg_var (Rem 0.5) in
    style (decl :: [ Css.border_radius (Var r) ])

  let rounded_xl =
    let decl, r = Var.binding radius_xl_var (Rem 0.75) in
    style (decl :: [ Css.border_radius (Var r) ])

  let rounded_2xl =
    let decl, r = Var.binding radius_2xl_var (Rem 1.0) in
    style (decl :: [ Css.border_radius (Var r) ])

  let rounded_3xl =
    let decl, r = Var.binding radius_3xl_var (Rem 1.5) in
    style (decl :: [ Css.border_radius (Var r) ])

  let rounded_full =
    (* Tailwind v4 uses calc(infinity * 1px) which gets optimized to
       3.40282e38px *)
    style [ Css.border_radius (Calc Calc.(infinity * px 1.)) ]

  (** Corner-specific rounded utilities *)
  let rounded_t =
    style
      [
        Css.border_radius (Rem 0.25);
        (* TODO: Use individual corner properties when available *)
      ]

  let rounded_r =
    style
      [
        Css.border_radius (Rem 0.25);
        (* TODO: Use individual corner properties when available *)
      ]

  let rounded_b =
    style
      [
        Css.border_radius (Rem 0.25);
        (* TODO: Use individual corner properties when available *)
      ]

  let rounded_l =
    style
      [
        Css.border_radius (Rem 0.25);
        (* TODO: Use individual corner properties when available *)
      ]

  let rounded_tl = style [ Css.border_radius (Rem 0.25) ]
  let rounded_tr = style [ Css.border_radius (Rem 0.25) ]
  let rounded_br = style [ Css.border_radius (Rem 0.25) ]
  let rounded_bl = style [ Css.border_radius (Rem 0.25) ]

  (** Corner-specific rounded utilities with sizes *)

  let rounded_t_lg =
    style [ Css.border_radius (Rem 0.5); Css.border_radius (Rem 0.5) ]

  let rounded_tl_2xl = style [ Css.border_radius (Rem 1.0) ]

  (* Outline style *)

  let outline_none = style [ Css.outline_style Css.None ]

  (* Outline offset *)
  let outline_offset_0 = style [ Css.outline_offset (Px 0.) ]
  let outline_offset_1 = style [ Css.outline_offset (Px 1.) ]
  let outline_offset_2 = style [ Css.outline_offset (Px 2.) ]
  let outline_offset_4 = style [ Css.outline_offset (Px 4.) ]
  let outline_offset_8 = style [ Css.outline_offset (Px 8.) ]

  let to_style : t -> Style.t = function
    (* Border width utilities *)
    | Border -> border
    | Border_0 -> border_0
    | Border_2 -> border_2
    | Border_4 -> border_4
    | Border_8 -> border_8
    (* Border side/axis utilities *)
    | Border_t -> border_t
    | Border_r -> border_r
    | Border_b -> border_b
    | Border_l -> border_l
    | Border_x -> border_x
    | Border_y -> border_y
    (* Border side utilities with widths *)
    | Border_t_0 -> border_t_0
    | Border_t_2 -> border_t_2
    | Border_t_4 -> border_t_4
    | Border_t_8 -> border_t_8
    | Border_r_0 -> border_r_0
    | Border_r_2 -> border_r_2
    | Border_r_4 -> border_r_4
    | Border_r_8 -> border_r_8
    | Border_b_0 -> border_b_0
    | Border_b_2 -> border_b_2
    | Border_b_4 -> border_b_4
    | Border_b_8 -> border_b_8
    | Border_l_0 -> border_l_0
    | Border_l_2 -> border_l_2
    | Border_l_4 -> border_l_4
    | Border_l_8 -> border_l_8
    (* Border style utilities *)
    | Border_solid -> border_solid
    | Border_dashed -> border_dashed
    | Border_dotted -> border_dotted
    | Border_double -> border_double
    | Border_none -> border_none
    (* Border color utilities *)
    | Border_color (color, shade) -> border_color' color shade
    | Border_transparent -> border_transparent'
    | Border_current -> border_current'
    (* Border radius utilities *)
    | Rounded -> rounded
    | Rounded_none -> rounded_none
    | Rounded_sm -> rounded_sm
    | Rounded_md -> rounded_md
    | Rounded_lg -> rounded_lg
    | Rounded_xl -> rounded_xl
    | Rounded_2xl -> rounded_2xl
    | Rounded_3xl -> rounded_3xl
    | Rounded_full -> rounded_full
    (* Side-specific rounded utilities *)
    | Rounded_t -> rounded_t
    | Rounded_r -> rounded_r
    | Rounded_b -> rounded_b
    | Rounded_l -> rounded_l
    (* Corner-specific rounded utilities *)
    | Rounded_tl -> rounded_tl
    | Rounded_tr -> rounded_tr
    | Rounded_br -> rounded_br
    | Rounded_bl -> rounded_bl
    (* Corner-specific rounded utilities with sizes *)
    | Rounded_t_lg -> rounded_t_lg
    | Rounded_tl_2xl -> rounded_tl_2xl
    (* Outline utilities *)
    | Outline_none -> outline_none
    | Outline_offset_0 -> outline_offset_0
    | Outline_offset_1 -> outline_offset_1
    | Outline_offset_2 -> outline_offset_2
    | Outline_offset_4 -> outline_offset_4
    | Outline_offset_8 -> outline_offset_8

  let err_not_utility = Error (`Msg "Not a border utility")

  let suborder = function
    (* Border radius utilities (0-99) - alphabetical, comes before border *)
    | Rounded -> 0
    | Rounded_2xl -> 1
    | Rounded_3xl -> 2
    | Rounded_full -> 3
    | Rounded_lg -> 4
    | Rounded_md -> 5
    | Rounded_none -> 6
    | Rounded_sm -> 7
    | Rounded_xl -> 8
    (* Side-specific rounded utilities (100-199) *)
    | Rounded_t -> 100
    | Rounded_r -> 101
    | Rounded_b -> 102
    | Rounded_l -> 103
    (* Corner-specific rounded utilities (200-299) *)
    | Rounded_tl -> 200
    | Rounded_tr -> 201
    | Rounded_br -> 202
    | Rounded_bl -> 203
    (* Corner-specific rounded utilities with sizes (300-399) *)
    | Rounded_t_lg -> 300
    | Rounded_tl_2xl -> 301
    (* Border width utilities (1000-1099) *)
    | Border -> 1000
    | Border_0 -> 1001
    | Border_2 -> 1002
    | Border_4 -> 1003
    | Border_8 -> 1004
    (* Border side/axis utilities (1100-1199) - alphabetical *)
    | Border_b -> 1100
    | Border_l -> 1101
    | Border_r -> 1102
    | Border_t -> 1103
    | Border_x -> 1104
    | Border_y -> 1105
    (* Border side utilities with widths (1200-1399) *)
    | Border_t_0 -> 1200
    | Border_t_2 -> 1201
    | Border_t_4 -> 1202
    | Border_t_8 -> 1203
    | Border_r_0 -> 1210
    | Border_r_2 -> 1211
    | Border_r_4 -> 1212
    | Border_r_8 -> 1213
    | Border_b_0 -> 1220
    | Border_b_2 -> 1221
    | Border_b_4 -> 1222
    | Border_b_8 -> 1223
    | Border_l_0 -> 1230
    | Border_l_2 -> 1231
    | Border_l_4 -> 1232
    | Border_l_8 -> 1233
    (* Border style utilities (1400-1499) - alphabetical *)
    | Border_dashed -> 1400
    | Border_dotted -> 1401
    | Border_double -> 1402
    | Border_none -> 1403
    | Border_solid -> 1404
    (* Border color utilities (1500-1999) *)
    | Border_color (color, shade) ->
        let base =
          if Color.is_base_color color then
            Color.suborder_with_shade (Color.to_name color)
          else
            Color.suborder_with_shade
              (Color.to_name color ^ "-" ^ string_of_int shade)
        in
        1500 + base
    | Border_transparent -> 1500
    | Border_current -> 1501
    (* Outline utilities (2000-2099) *)
    | Outline_none -> 2000
    | Outline_offset_0 -> 2001
    | Outline_offset_1 -> 2002
    | Outline_offset_2 -> 2003
    | Outline_offset_4 -> 2004
    | Outline_offset_8 -> 2005

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
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
    | [ "border"; "transparent" ] -> Ok Border_transparent
    | [ "border"; "current" ] -> Ok Border_current
    | "border" :: color_parts -> (
        match Color.shade_of_strings color_parts with
        | Ok (color, shade) -> Ok (Border_color (color, shade))
        | Error _ -> err_not_utility)
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

  let to_class = function
    | Border -> "border"
    | Border_0 -> "border-0"
    | Border_2 -> "border-2"
    | Border_4 -> "border-4"
    | Border_8 -> "border-8"
    | Border_t -> "border-t"
    | Border_r -> "border-r"
    | Border_b -> "border-b"
    | Border_l -> "border-l"
    | Border_x -> "border-x"
    | Border_y -> "border-y"
    | Border_t_0 -> "border-t-0"
    | Border_t_2 -> "border-t-2"
    | Border_t_4 -> "border-t-4"
    | Border_t_8 -> "border-t-8"
    | Border_r_0 -> "border-r-0"
    | Border_r_2 -> "border-r-2"
    | Border_r_4 -> "border-r-4"
    | Border_r_8 -> "border-r-8"
    | Border_b_0 -> "border-b-0"
    | Border_b_2 -> "border-b-2"
    | Border_b_4 -> "border-b-4"
    | Border_b_8 -> "border-b-8"
    | Border_l_0 -> "border-l-0"
    | Border_l_2 -> "border-l-2"
    | Border_l_4 -> "border-l-4"
    | Border_l_8 -> "border-l-8"
    | Border_solid -> "border-solid"
    | Border_dashed -> "border-dashed"
    | Border_dotted -> "border-dotted"
    | Border_double -> "border-double"
    | Border_none -> "border-none"
    | Border_color (c, shade) ->
        if Color.is_base_color c || Color.is_custom_color c then
          "border-" ^ Color.color_to_string c
        else "border-" ^ Color.color_to_string c ^ "-" ^ string_of_int shade
    | Border_transparent -> "border-transparent"
    | Border_current -> "border-current"
    | Rounded -> "rounded"
    | Rounded_none -> "rounded-none"
    | Rounded_sm -> "rounded-sm"
    | Rounded_md -> "rounded-md"
    | Rounded_lg -> "rounded-lg"
    | Rounded_xl -> "rounded-xl"
    | Rounded_2xl -> "rounded-2xl"
    | Rounded_3xl -> "rounded-3xl"
    | Rounded_full -> "rounded-full"
    | Rounded_t -> "rounded-t"
    | Rounded_r -> "rounded-r"
    | Rounded_b -> "rounded-b"
    | Rounded_l -> "rounded-l"
    | Rounded_tl -> "rounded-tl"
    | Rounded_tr -> "rounded-tr"
    | Rounded_br -> "rounded-br"
    | Rounded_bl -> "rounded-bl"
    | Rounded_t_lg -> "rounded-t-lg"
    | Rounded_tl_2xl -> "rounded-tl-2xl"
    | Outline_none -> "outline-none"
    | Outline_offset_0 -> "outline-offset-0"
    | Outline_offset_1 -> "outline-offset-1"
    | Outline_offset_2 -> "outline-offset-2"
    | Outline_offset_4 -> "outline-offset-4"
    | Outline_offset_8 -> "outline-offset-8"
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)

(** {1 Border Width Utilities} *)

let border = utility Border
let border_0 = utility Border_0
let border_2 = utility Border_2
let border_4 = utility Border_4
let border_8 = utility Border_8
let border_t = utility Border_t
let border_r = utility Border_r
let border_b = utility Border_b
let border_l = utility Border_l
let border_x = utility Border_x
let border_y = utility Border_y

(** {1 Border Style Utilities} *)

let border_solid = utility Border_solid
let border_dashed = utility Border_dashed
let border_dotted = utility Border_dotted
let border_double = utility Border_double
let border_none = utility Border_none

(** {1 Border Color Utilities} *)

let border_color color shade = utility (Border_color (color, shade))
let border_transparent = utility Border_transparent
let border_current = utility Border_current

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

let rounded_none = utility Rounded_none
let rounded_sm = utility Rounded_sm
let rounded = utility Rounded
let rounded_md = utility Rounded_md
let rounded_lg = utility Rounded_lg
let rounded_xl = utility Rounded_xl
let rounded_2xl = utility Rounded_2xl
let rounded_3xl = utility Rounded_3xl
let rounded_full = utility Rounded_full

(** {1 Outline Utilities} *)

let outline_none = utility Outline_none
let outline_offset_0 = utility Outline_offset_0
let outline_offset_1 = utility Outline_offset_1
let outline_offset_2 = utility Outline_offset_2
let outline_offset_4 = utility Outline_offset_4
let outline_offset_8 = utility Outline_offset_8
