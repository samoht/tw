(** Border utilities for border styles, widths, and radii

    What's included:
    - Border widths: `border`, `border-0/2/4/8`, and side/axis variants.
    - Border styles: `border-solid/dashed/dotted/double/none` via a CSS var.
    - Border radius: `rounded-*` and `rounded-full`.
    - Side-specific border radius: `rounded-t-*`, `rounded-r-*`, `rounded-b-*`,
      `rounded-l-*` for top/right/bottom/left sides.
    - Corner-specific border radius: `rounded-tl-*`, `rounded-tr-*`,
      `rounded-br-*`, `rounded-bl-*` for individual corners.
    - Outline offsets and simple ring helpers.

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
    | (* Side-specific rounded utilities - top *)
      Rounded_t
    | Rounded_t_none
    | Rounded_t_sm
    | Rounded_t_md
    | Rounded_t_lg
    | Rounded_t_xl
    | Rounded_t_2xl
    | Rounded_t_3xl
    | Rounded_t_full
    | (* Side-specific rounded utilities - right *)
      Rounded_r
    | Rounded_r_none
    | Rounded_r_sm
    | Rounded_r_md
    | Rounded_r_lg
    | Rounded_r_xl
    | Rounded_r_2xl
    | Rounded_r_3xl
    | Rounded_r_full
    | (* Side-specific rounded utilities - bottom *)
      Rounded_b
    | Rounded_b_none
    | Rounded_b_sm
    | Rounded_b_md
    | Rounded_b_lg
    | Rounded_b_xl
    | Rounded_b_2xl
    | Rounded_b_3xl
    | Rounded_b_full
    | (* Side-specific rounded utilities - left *)
      Rounded_l
    | Rounded_l_none
    | Rounded_l_sm
    | Rounded_l_md
    | Rounded_l_lg
    | Rounded_l_xl
    | Rounded_l_2xl
    | Rounded_l_3xl
    | Rounded_l_full
    | (* Corner-specific rounded utilities - top-left *)
      Rounded_tl
    | Rounded_tl_none
    | Rounded_tl_sm
    | Rounded_tl_md
    | Rounded_tl_lg
    | Rounded_tl_xl
    | Rounded_tl_2xl
    | Rounded_tl_3xl
    | Rounded_tl_full
    | (* Corner-specific rounded utilities - top-right *)
      Rounded_tr
    | Rounded_tr_none
    | Rounded_tr_sm
    | Rounded_tr_md
    | Rounded_tr_lg
    | Rounded_tr_xl
    | Rounded_tr_2xl
    | Rounded_tr_3xl
    | Rounded_tr_full
    | (* Corner-specific rounded utilities - bottom-right *)
      Rounded_br
    | Rounded_br_none
    | Rounded_br_sm
    | Rounded_br_md
    | Rounded_br_lg
    | Rounded_br_xl
    | Rounded_br_2xl
    | Rounded_br_3xl
    | Rounded_br_full
    | (* Corner-specific rounded utilities - bottom-left *)
      Rounded_bl
    | Rounded_bl_none
    | Rounded_bl_sm
    | Rounded_bl_md
    | Rounded_bl_lg
    | Rounded_bl_xl
    | Rounded_bl_2xl
    | Rounded_bl_3xl
    | Rounded_bl_full
    | (* Outline utilities *)
      Outline_none
    | Outline_offset_0
    | Outline_offset_1
    | Outline_offset_2
    | Outline_offset_4
    | Outline_offset_8

  type Utility.base += Self of t

  let name = "borders"
  let priority = 19

  (* Create border style variable with @property for utilities that reference
     it *)
  let border_style_var =
    Var.property_default Css.Border_style
      ~initial:(Solid : Css.border_style)
      ~property_order:0 ~family:`Border "tw-border-style"

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

  let border_transparent' = style [ Css.border_color (Css.hex "#0000") ]
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

  (* Helper for creating infinity radius for corner-specific utilities *)
  let infinity_radius () : Css.length = Calc Calc.(infinity * px 1.)

  (** Side-specific rounded utilities - top *)
  let rounded_t =
    style
      [
        Css.border_top_left_radius (Rem 0.25);
        Css.border_top_right_radius (Rem 0.25);
      ]

  let rounded_t_none =
    style [ Css.border_top_left_radius Zero; Css.border_top_right_radius Zero ]

  let rounded_t_sm =
    let decl, r = Var.binding radius_sm_var (Rem 0.25) in
    style
      (decl
      :: [
           Css.border_top_left_radius (Var r);
           Css.border_top_right_radius (Var r);
         ])

  let rounded_t_md =
    let decl, r = Var.binding radius_md_var (Rem 0.375) in
    style
      (decl
      :: [
           Css.border_top_left_radius (Var r);
           Css.border_top_right_radius (Var r);
         ])

  let rounded_t_lg =
    let decl, r = Var.binding radius_lg_var (Rem 0.5) in
    style
      (decl
      :: [
           Css.border_top_left_radius (Var r);
           Css.border_top_right_radius (Var r);
         ])

  let rounded_t_xl =
    let decl, r = Var.binding radius_xl_var (Rem 0.75) in
    style
      (decl
      :: [
           Css.border_top_left_radius (Var r);
           Css.border_top_right_radius (Var r);
         ])

  let rounded_t_2xl =
    let decl, r = Var.binding radius_2xl_var (Rem 1.0) in
    style
      (decl
      :: [
           Css.border_top_left_radius (Var r);
           Css.border_top_right_radius (Var r);
         ])

  let rounded_t_3xl =
    let decl, r = Var.binding radius_3xl_var (Rem 1.5) in
    style
      (decl
      :: [
           Css.border_top_left_radius (Var r);
           Css.border_top_right_radius (Var r);
         ])

  let rounded_t_full =
    style
      [
        Css.border_top_left_radius (infinity_radius ());
        Css.border_top_right_radius (infinity_radius ());
      ]

  (** Side-specific rounded utilities - right *)
  let rounded_r =
    style
      [
        Css.border_top_right_radius (Rem 0.25);
        Css.border_bottom_right_radius (Rem 0.25);
      ]

  let rounded_r_none =
    style
      [ Css.border_top_right_radius Zero; Css.border_bottom_right_radius Zero ]

  let rounded_r_sm =
    let decl, r = Var.binding radius_sm_var (Rem 0.25) in
    style
      (decl
      :: [
           Css.border_top_right_radius (Var r);
           Css.border_bottom_right_radius (Var r);
         ])

  let rounded_r_md =
    let decl, r = Var.binding radius_md_var (Rem 0.375) in
    style
      (decl
      :: [
           Css.border_top_right_radius (Var r);
           Css.border_bottom_right_radius (Var r);
         ])

  let rounded_r_lg =
    let decl, r = Var.binding radius_lg_var (Rem 0.5) in
    style
      (decl
      :: [
           Css.border_top_right_radius (Var r);
           Css.border_bottom_right_radius (Var r);
         ])

  let rounded_r_xl =
    let decl, r = Var.binding radius_xl_var (Rem 0.75) in
    style
      (decl
      :: [
           Css.border_top_right_radius (Var r);
           Css.border_bottom_right_radius (Var r);
         ])

  let rounded_r_2xl =
    let decl, r = Var.binding radius_2xl_var (Rem 1.0) in
    style
      (decl
      :: [
           Css.border_top_right_radius (Var r);
           Css.border_bottom_right_radius (Var r);
         ])

  let rounded_r_3xl =
    let decl, r = Var.binding radius_3xl_var (Rem 1.5) in
    style
      (decl
      :: [
           Css.border_top_right_radius (Var r);
           Css.border_bottom_right_radius (Var r);
         ])

  let rounded_r_full =
    style
      [
        Css.border_top_right_radius (infinity_radius ());
        Css.border_bottom_right_radius (infinity_radius ());
      ]

  (** Side-specific rounded utilities - bottom *)
  let rounded_b =
    style
      [
        Css.border_bottom_left_radius (Rem 0.25);
        Css.border_bottom_right_radius (Rem 0.25);
      ]

  let rounded_b_none =
    style
      [
        Css.border_bottom_left_radius Zero; Css.border_bottom_right_radius Zero;
      ]

  let rounded_b_sm =
    let decl, r = Var.binding radius_sm_var (Rem 0.25) in
    style
      (decl
      :: [
           Css.border_bottom_left_radius (Var r);
           Css.border_bottom_right_radius (Var r);
         ])

  let rounded_b_md =
    let decl, r = Var.binding radius_md_var (Rem 0.375) in
    style
      (decl
      :: [
           Css.border_bottom_left_radius (Var r);
           Css.border_bottom_right_radius (Var r);
         ])

  let rounded_b_lg =
    let decl, r = Var.binding radius_lg_var (Rem 0.5) in
    style
      (decl
      :: [
           Css.border_bottom_left_radius (Var r);
           Css.border_bottom_right_radius (Var r);
         ])

  let rounded_b_xl =
    let decl, r = Var.binding radius_xl_var (Rem 0.75) in
    style
      (decl
      :: [
           Css.border_bottom_left_radius (Var r);
           Css.border_bottom_right_radius (Var r);
         ])

  let rounded_b_2xl =
    let decl, r = Var.binding radius_2xl_var (Rem 1.0) in
    style
      (decl
      :: [
           Css.border_bottom_left_radius (Var r);
           Css.border_bottom_right_radius (Var r);
         ])

  let rounded_b_3xl =
    let decl, r = Var.binding radius_3xl_var (Rem 1.5) in
    style
      (decl
      :: [
           Css.border_bottom_left_radius (Var r);
           Css.border_bottom_right_radius (Var r);
         ])

  let rounded_b_full =
    style
      [
        Css.border_bottom_left_radius (infinity_radius ());
        Css.border_bottom_right_radius (infinity_radius ());
      ]

  (** Side-specific rounded utilities - left *)
  let rounded_l =
    style
      [
        Css.border_top_left_radius (Rem 0.25);
        Css.border_bottom_left_radius (Rem 0.25);
      ]

  let rounded_l_none =
    style
      [ Css.border_top_left_radius Zero; Css.border_bottom_left_radius Zero ]

  let rounded_l_sm =
    let decl, r = Var.binding radius_sm_var (Rem 0.25) in
    style
      (decl
      :: [
           Css.border_top_left_radius (Var r);
           Css.border_bottom_left_radius (Var r);
         ])

  let rounded_l_md =
    let decl, r = Var.binding radius_md_var (Rem 0.375) in
    style
      (decl
      :: [
           Css.border_top_left_radius (Var r);
           Css.border_bottom_left_radius (Var r);
         ])

  let rounded_l_lg =
    let decl, r = Var.binding radius_lg_var (Rem 0.5) in
    style
      (decl
      :: [
           Css.border_top_left_radius (Var r);
           Css.border_bottom_left_radius (Var r);
         ])

  let rounded_l_xl =
    let decl, r = Var.binding radius_xl_var (Rem 0.75) in
    style
      (decl
      :: [
           Css.border_top_left_radius (Var r);
           Css.border_bottom_left_radius (Var r);
         ])

  let rounded_l_2xl =
    let decl, r = Var.binding radius_2xl_var (Rem 1.0) in
    style
      (decl
      :: [
           Css.border_top_left_radius (Var r);
           Css.border_bottom_left_radius (Var r);
         ])

  let rounded_l_3xl =
    let decl, r = Var.binding radius_3xl_var (Rem 1.5) in
    style
      (decl
      :: [
           Css.border_top_left_radius (Var r);
           Css.border_bottom_left_radius (Var r);
         ])

  let rounded_l_full =
    style
      [
        Css.border_top_left_radius (infinity_radius ());
        Css.border_bottom_left_radius (infinity_radius ());
      ]

  (** Corner-specific rounded utilities - top-left *)
  let rounded_tl = style [ Css.border_top_left_radius (Rem 0.25) ]

  let rounded_tl_none = style [ Css.border_top_left_radius Zero ]

  let rounded_tl_sm =
    let decl, r = Var.binding radius_sm_var (Rem 0.25) in
    style (decl :: [ Css.border_top_left_radius (Var r) ])

  let rounded_tl_md =
    let decl, r = Var.binding radius_md_var (Rem 0.375) in
    style (decl :: [ Css.border_top_left_radius (Var r) ])

  let rounded_tl_lg =
    let decl, r = Var.binding radius_lg_var (Rem 0.5) in
    style (decl :: [ Css.border_top_left_radius (Var r) ])

  let rounded_tl_xl =
    let decl, r = Var.binding radius_xl_var (Rem 0.75) in
    style (decl :: [ Css.border_top_left_radius (Var r) ])

  let rounded_tl_2xl =
    let decl, r = Var.binding radius_2xl_var (Rem 1.0) in
    style (decl :: [ Css.border_top_left_radius (Var r) ])

  let rounded_tl_3xl =
    let decl, r = Var.binding radius_3xl_var (Rem 1.5) in
    style (decl :: [ Css.border_top_left_radius (Var r) ])

  let rounded_tl_full =
    style [ Css.border_top_left_radius (infinity_radius ()) ]

  (** Corner-specific rounded utilities - top-right *)
  let rounded_tr = style [ Css.border_top_right_radius (Rem 0.25) ]

  let rounded_tr_none = style [ Css.border_top_right_radius Zero ]

  let rounded_tr_sm =
    let decl, r = Var.binding radius_sm_var (Rem 0.25) in
    style (decl :: [ Css.border_top_right_radius (Var r) ])

  let rounded_tr_md =
    let decl, r = Var.binding radius_md_var (Rem 0.375) in
    style (decl :: [ Css.border_top_right_radius (Var r) ])

  let rounded_tr_lg =
    let decl, r = Var.binding radius_lg_var (Rem 0.5) in
    style (decl :: [ Css.border_top_right_radius (Var r) ])

  let rounded_tr_xl =
    let decl, r = Var.binding radius_xl_var (Rem 0.75) in
    style (decl :: [ Css.border_top_right_radius (Var r) ])

  let rounded_tr_2xl =
    let decl, r = Var.binding radius_2xl_var (Rem 1.0) in
    style (decl :: [ Css.border_top_right_radius (Var r) ])

  let rounded_tr_3xl =
    let decl, r = Var.binding radius_3xl_var (Rem 1.5) in
    style (decl :: [ Css.border_top_right_radius (Var r) ])

  let rounded_tr_full =
    style [ Css.border_top_right_radius (infinity_radius ()) ]

  (** Corner-specific rounded utilities - bottom-right *)
  let rounded_br = style [ Css.border_bottom_right_radius (Rem 0.25) ]

  let rounded_br_none = style [ Css.border_bottom_right_radius Zero ]

  let rounded_br_sm =
    let decl, r = Var.binding radius_sm_var (Rem 0.25) in
    style (decl :: [ Css.border_bottom_right_radius (Var r) ])

  let rounded_br_md =
    let decl, r = Var.binding radius_md_var (Rem 0.375) in
    style (decl :: [ Css.border_bottom_right_radius (Var r) ])

  let rounded_br_lg =
    let decl, r = Var.binding radius_lg_var (Rem 0.5) in
    style (decl :: [ Css.border_bottom_right_radius (Var r) ])

  let rounded_br_xl =
    let decl, r = Var.binding radius_xl_var (Rem 0.75) in
    style (decl :: [ Css.border_bottom_right_radius (Var r) ])

  let rounded_br_2xl =
    let decl, r = Var.binding radius_2xl_var (Rem 1.0) in
    style (decl :: [ Css.border_bottom_right_radius (Var r) ])

  let rounded_br_3xl =
    let decl, r = Var.binding radius_3xl_var (Rem 1.5) in
    style (decl :: [ Css.border_bottom_right_radius (Var r) ])

  let rounded_br_full =
    style [ Css.border_bottom_right_radius (infinity_radius ()) ]

  (** Corner-specific rounded utilities - bottom-left *)
  let rounded_bl = style [ Css.border_bottom_left_radius (Rem 0.25) ]

  let rounded_bl_none = style [ Css.border_bottom_left_radius Zero ]

  let rounded_bl_sm =
    let decl, r = Var.binding radius_sm_var (Rem 0.25) in
    style (decl :: [ Css.border_bottom_left_radius (Var r) ])

  let rounded_bl_md =
    let decl, r = Var.binding radius_md_var (Rem 0.375) in
    style (decl :: [ Css.border_bottom_left_radius (Var r) ])

  let rounded_bl_lg =
    let decl, r = Var.binding radius_lg_var (Rem 0.5) in
    style (decl :: [ Css.border_bottom_left_radius (Var r) ])

  let rounded_bl_xl =
    let decl, r = Var.binding radius_xl_var (Rem 0.75) in
    style (decl :: [ Css.border_bottom_left_radius (Var r) ])

  let rounded_bl_2xl =
    let decl, r = Var.binding radius_2xl_var (Rem 1.0) in
    style (decl :: [ Css.border_bottom_left_radius (Var r) ])

  let rounded_bl_3xl =
    let decl, r = Var.binding radius_3xl_var (Rem 1.5) in
    style (decl :: [ Css.border_bottom_left_radius (Var r) ])

  let rounded_bl_full =
    style [ Css.border_bottom_left_radius (infinity_radius ()) ]

  (* Outline style *)

  let outline_none =
    style
      [
        Css.custom_property ~layer:"utilities" "--tw-outline-style" "none";
        Css.outline_style Css.None;
      ]

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
    (* Side-specific rounded utilities - top *)
    | Rounded_t -> rounded_t
    | Rounded_t_none -> rounded_t_none
    | Rounded_t_sm -> rounded_t_sm
    | Rounded_t_md -> rounded_t_md
    | Rounded_t_lg -> rounded_t_lg
    | Rounded_t_xl -> rounded_t_xl
    | Rounded_t_2xl -> rounded_t_2xl
    | Rounded_t_3xl -> rounded_t_3xl
    | Rounded_t_full -> rounded_t_full
    (* Side-specific rounded utilities - right *)
    | Rounded_r -> rounded_r
    | Rounded_r_none -> rounded_r_none
    | Rounded_r_sm -> rounded_r_sm
    | Rounded_r_md -> rounded_r_md
    | Rounded_r_lg -> rounded_r_lg
    | Rounded_r_xl -> rounded_r_xl
    | Rounded_r_2xl -> rounded_r_2xl
    | Rounded_r_3xl -> rounded_r_3xl
    | Rounded_r_full -> rounded_r_full
    (* Side-specific rounded utilities - bottom *)
    | Rounded_b -> rounded_b
    | Rounded_b_none -> rounded_b_none
    | Rounded_b_sm -> rounded_b_sm
    | Rounded_b_md -> rounded_b_md
    | Rounded_b_lg -> rounded_b_lg
    | Rounded_b_xl -> rounded_b_xl
    | Rounded_b_2xl -> rounded_b_2xl
    | Rounded_b_3xl -> rounded_b_3xl
    | Rounded_b_full -> rounded_b_full
    (* Side-specific rounded utilities - left *)
    | Rounded_l -> rounded_l
    | Rounded_l_none -> rounded_l_none
    | Rounded_l_sm -> rounded_l_sm
    | Rounded_l_md -> rounded_l_md
    | Rounded_l_lg -> rounded_l_lg
    | Rounded_l_xl -> rounded_l_xl
    | Rounded_l_2xl -> rounded_l_2xl
    | Rounded_l_3xl -> rounded_l_3xl
    | Rounded_l_full -> rounded_l_full
    (* Corner-specific rounded utilities - top-left *)
    | Rounded_tl -> rounded_tl
    | Rounded_tl_none -> rounded_tl_none
    | Rounded_tl_sm -> rounded_tl_sm
    | Rounded_tl_md -> rounded_tl_md
    | Rounded_tl_lg -> rounded_tl_lg
    | Rounded_tl_xl -> rounded_tl_xl
    | Rounded_tl_2xl -> rounded_tl_2xl
    | Rounded_tl_3xl -> rounded_tl_3xl
    | Rounded_tl_full -> rounded_tl_full
    (* Corner-specific rounded utilities - top-right *)
    | Rounded_tr -> rounded_tr
    | Rounded_tr_none -> rounded_tr_none
    | Rounded_tr_sm -> rounded_tr_sm
    | Rounded_tr_md -> rounded_tr_md
    | Rounded_tr_lg -> rounded_tr_lg
    | Rounded_tr_xl -> rounded_tr_xl
    | Rounded_tr_2xl -> rounded_tr_2xl
    | Rounded_tr_3xl -> rounded_tr_3xl
    | Rounded_tr_full -> rounded_tr_full
    (* Corner-specific rounded utilities - bottom-right *)
    | Rounded_br -> rounded_br
    | Rounded_br_none -> rounded_br_none
    | Rounded_br_sm -> rounded_br_sm
    | Rounded_br_md -> rounded_br_md
    | Rounded_br_lg -> rounded_br_lg
    | Rounded_br_xl -> rounded_br_xl
    | Rounded_br_2xl -> rounded_br_2xl
    | Rounded_br_3xl -> rounded_br_3xl
    | Rounded_br_full -> rounded_br_full
    (* Corner-specific rounded utilities - bottom-left *)
    | Rounded_bl -> rounded_bl
    | Rounded_bl_none -> rounded_bl_none
    | Rounded_bl_sm -> rounded_bl_sm
    | Rounded_bl_md -> rounded_bl_md
    | Rounded_bl_lg -> rounded_bl_lg
    | Rounded_bl_xl -> rounded_bl_xl
    | Rounded_bl_2xl -> rounded_bl_2xl
    | Rounded_bl_3xl -> rounded_bl_3xl
    | Rounded_bl_full -> rounded_bl_full
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
    (* Side-specific rounded utilities - top (100-109) *)
    | Rounded_t -> 100
    | Rounded_t_none -> 101
    | Rounded_t_sm -> 102
    | Rounded_t_md -> 103
    | Rounded_t_lg -> 104
    | Rounded_t_xl -> 105
    | Rounded_t_2xl -> 106
    | Rounded_t_3xl -> 107
    | Rounded_t_full -> 108
    (* Side-specific rounded utilities - right (110-119) *)
    | Rounded_r -> 110
    | Rounded_r_none -> 111
    | Rounded_r_sm -> 112
    | Rounded_r_md -> 113
    | Rounded_r_lg -> 114
    | Rounded_r_xl -> 115
    | Rounded_r_2xl -> 116
    | Rounded_r_3xl -> 117
    | Rounded_r_full -> 118
    (* Side-specific rounded utilities - bottom (120-129) *)
    | Rounded_b -> 120
    | Rounded_b_none -> 121
    | Rounded_b_sm -> 122
    | Rounded_b_md -> 123
    | Rounded_b_lg -> 124
    | Rounded_b_xl -> 125
    | Rounded_b_2xl -> 126
    | Rounded_b_3xl -> 127
    | Rounded_b_full -> 128
    (* Side-specific rounded utilities - left (130-139) *)
    | Rounded_l -> 130
    | Rounded_l_none -> 131
    | Rounded_l_sm -> 132
    | Rounded_l_md -> 133
    | Rounded_l_lg -> 134
    | Rounded_l_xl -> 135
    | Rounded_l_2xl -> 136
    | Rounded_l_3xl -> 137
    | Rounded_l_full -> 138
    (* Corner-specific rounded utilities - top-left (200-209) *)
    | Rounded_tl -> 200
    | Rounded_tl_none -> 201
    | Rounded_tl_sm -> 202
    | Rounded_tl_md -> 203
    | Rounded_tl_lg -> 204
    | Rounded_tl_xl -> 205
    | Rounded_tl_2xl -> 206
    | Rounded_tl_3xl -> 207
    | Rounded_tl_full -> 208
    (* Corner-specific rounded utilities - top-right (210-219) *)
    | Rounded_tr -> 210
    | Rounded_tr_none -> 211
    | Rounded_tr_sm -> 212
    | Rounded_tr_md -> 213
    | Rounded_tr_lg -> 214
    | Rounded_tr_xl -> 215
    | Rounded_tr_2xl -> 216
    | Rounded_tr_3xl -> 217
    | Rounded_tr_full -> 218
    (* Corner-specific rounded utilities - bottom-right (220-229) *)
    | Rounded_br -> 220
    | Rounded_br_none -> 221
    | Rounded_br_sm -> 222
    | Rounded_br_md -> 223
    | Rounded_br_lg -> 224
    | Rounded_br_xl -> 225
    | Rounded_br_2xl -> 226
    | Rounded_br_3xl -> 227
    | Rounded_br_full -> 228
    (* Corner-specific rounded utilities - bottom-left (230-239) *)
    | Rounded_bl -> 230
    | Rounded_bl_none -> 231
    | Rounded_bl_sm -> 232
    | Rounded_bl_md -> 233
    | Rounded_bl_lg -> 234
    | Rounded_bl_xl -> 235
    | Rounded_bl_2xl -> 236
    | Rounded_bl_3xl -> 237
    | Rounded_bl_full -> 238
    (* Border width utilities (1000-1099) *)
    | Border -> 1000
    | Border_0 -> 1001
    | Border_2 -> 1002
    | Border_4 -> 1003
    | Border_8 -> 1004
    (* Border side/axis utilities (1100-1199) - clockwise from top: t, r, b,
       l *)
    | Border_t -> 1100
    | Border_r -> 1101
    | Border_b -> 1102
    | Border_l -> 1103
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
    (* Side-specific rounded utilities - top *)
    | [ "rounded"; "t" ] -> Ok Rounded_t
    | [ "rounded"; "t"; "none" ] -> Ok Rounded_t_none
    | [ "rounded"; "t"; "sm" ] -> Ok Rounded_t_sm
    | [ "rounded"; "t"; "md" ] -> Ok Rounded_t_md
    | [ "rounded"; "t"; "lg" ] -> Ok Rounded_t_lg
    | [ "rounded"; "t"; "xl" ] -> Ok Rounded_t_xl
    | [ "rounded"; "t"; "2xl" ] -> Ok Rounded_t_2xl
    | [ "rounded"; "t"; "3xl" ] -> Ok Rounded_t_3xl
    | [ "rounded"; "t"; "full" ] -> Ok Rounded_t_full
    (* Side-specific rounded utilities - right *)
    | [ "rounded"; "r" ] -> Ok Rounded_r
    | [ "rounded"; "r"; "none" ] -> Ok Rounded_r_none
    | [ "rounded"; "r"; "sm" ] -> Ok Rounded_r_sm
    | [ "rounded"; "r"; "md" ] -> Ok Rounded_r_md
    | [ "rounded"; "r"; "lg" ] -> Ok Rounded_r_lg
    | [ "rounded"; "r"; "xl" ] -> Ok Rounded_r_xl
    | [ "rounded"; "r"; "2xl" ] -> Ok Rounded_r_2xl
    | [ "rounded"; "r"; "3xl" ] -> Ok Rounded_r_3xl
    | [ "rounded"; "r"; "full" ] -> Ok Rounded_r_full
    (* Side-specific rounded utilities - bottom *)
    | [ "rounded"; "b" ] -> Ok Rounded_b
    | [ "rounded"; "b"; "none" ] -> Ok Rounded_b_none
    | [ "rounded"; "b"; "sm" ] -> Ok Rounded_b_sm
    | [ "rounded"; "b"; "md" ] -> Ok Rounded_b_md
    | [ "rounded"; "b"; "lg" ] -> Ok Rounded_b_lg
    | [ "rounded"; "b"; "xl" ] -> Ok Rounded_b_xl
    | [ "rounded"; "b"; "2xl" ] -> Ok Rounded_b_2xl
    | [ "rounded"; "b"; "3xl" ] -> Ok Rounded_b_3xl
    | [ "rounded"; "b"; "full" ] -> Ok Rounded_b_full
    (* Side-specific rounded utilities - left *)
    | [ "rounded"; "l" ] -> Ok Rounded_l
    | [ "rounded"; "l"; "none" ] -> Ok Rounded_l_none
    | [ "rounded"; "l"; "sm" ] -> Ok Rounded_l_sm
    | [ "rounded"; "l"; "md" ] -> Ok Rounded_l_md
    | [ "rounded"; "l"; "lg" ] -> Ok Rounded_l_lg
    | [ "rounded"; "l"; "xl" ] -> Ok Rounded_l_xl
    | [ "rounded"; "l"; "2xl" ] -> Ok Rounded_l_2xl
    | [ "rounded"; "l"; "3xl" ] -> Ok Rounded_l_3xl
    | [ "rounded"; "l"; "full" ] -> Ok Rounded_l_full
    (* Corner-specific rounded utilities - top-left *)
    | [ "rounded"; "tl" ] -> Ok Rounded_tl
    | [ "rounded"; "tl"; "none" ] -> Ok Rounded_tl_none
    | [ "rounded"; "tl"; "sm" ] -> Ok Rounded_tl_sm
    | [ "rounded"; "tl"; "md" ] -> Ok Rounded_tl_md
    | [ "rounded"; "tl"; "lg" ] -> Ok Rounded_tl_lg
    | [ "rounded"; "tl"; "xl" ] -> Ok Rounded_tl_xl
    | [ "rounded"; "tl"; "2xl" ] -> Ok Rounded_tl_2xl
    | [ "rounded"; "tl"; "3xl" ] -> Ok Rounded_tl_3xl
    | [ "rounded"; "tl"; "full" ] -> Ok Rounded_tl_full
    (* Corner-specific rounded utilities - top-right *)
    | [ "rounded"; "tr" ] -> Ok Rounded_tr
    | [ "rounded"; "tr"; "none" ] -> Ok Rounded_tr_none
    | [ "rounded"; "tr"; "sm" ] -> Ok Rounded_tr_sm
    | [ "rounded"; "tr"; "md" ] -> Ok Rounded_tr_md
    | [ "rounded"; "tr"; "lg" ] -> Ok Rounded_tr_lg
    | [ "rounded"; "tr"; "xl" ] -> Ok Rounded_tr_xl
    | [ "rounded"; "tr"; "2xl" ] -> Ok Rounded_tr_2xl
    | [ "rounded"; "tr"; "3xl" ] -> Ok Rounded_tr_3xl
    | [ "rounded"; "tr"; "full" ] -> Ok Rounded_tr_full
    (* Corner-specific rounded utilities - bottom-right *)
    | [ "rounded"; "br" ] -> Ok Rounded_br
    | [ "rounded"; "br"; "none" ] -> Ok Rounded_br_none
    | [ "rounded"; "br"; "sm" ] -> Ok Rounded_br_sm
    | [ "rounded"; "br"; "md" ] -> Ok Rounded_br_md
    | [ "rounded"; "br"; "lg" ] -> Ok Rounded_br_lg
    | [ "rounded"; "br"; "xl" ] -> Ok Rounded_br_xl
    | [ "rounded"; "br"; "2xl" ] -> Ok Rounded_br_2xl
    | [ "rounded"; "br"; "3xl" ] -> Ok Rounded_br_3xl
    | [ "rounded"; "br"; "full" ] -> Ok Rounded_br_full
    (* Corner-specific rounded utilities - bottom-left *)
    | [ "rounded"; "bl" ] -> Ok Rounded_bl
    | [ "rounded"; "bl"; "none" ] -> Ok Rounded_bl_none
    | [ "rounded"; "bl"; "sm" ] -> Ok Rounded_bl_sm
    | [ "rounded"; "bl"; "md" ] -> Ok Rounded_bl_md
    | [ "rounded"; "bl"; "lg" ] -> Ok Rounded_bl_lg
    | [ "rounded"; "bl"; "xl" ] -> Ok Rounded_bl_xl
    | [ "rounded"; "bl"; "2xl" ] -> Ok Rounded_bl_2xl
    | [ "rounded"; "bl"; "3xl" ] -> Ok Rounded_bl_3xl
    | [ "rounded"; "bl"; "full" ] -> Ok Rounded_bl_full
    | [ "outline"; "none" ] -> Ok Outline_none
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
    (* Side-specific rounded utilities - top *)
    | Rounded_t -> "rounded-t"
    | Rounded_t_none -> "rounded-t-none"
    | Rounded_t_sm -> "rounded-t-sm"
    | Rounded_t_md -> "rounded-t-md"
    | Rounded_t_lg -> "rounded-t-lg"
    | Rounded_t_xl -> "rounded-t-xl"
    | Rounded_t_2xl -> "rounded-t-2xl"
    | Rounded_t_3xl -> "rounded-t-3xl"
    | Rounded_t_full -> "rounded-t-full"
    (* Side-specific rounded utilities - right *)
    | Rounded_r -> "rounded-r"
    | Rounded_r_none -> "rounded-r-none"
    | Rounded_r_sm -> "rounded-r-sm"
    | Rounded_r_md -> "rounded-r-md"
    | Rounded_r_lg -> "rounded-r-lg"
    | Rounded_r_xl -> "rounded-r-xl"
    | Rounded_r_2xl -> "rounded-r-2xl"
    | Rounded_r_3xl -> "rounded-r-3xl"
    | Rounded_r_full -> "rounded-r-full"
    (* Side-specific rounded utilities - bottom *)
    | Rounded_b -> "rounded-b"
    | Rounded_b_none -> "rounded-b-none"
    | Rounded_b_sm -> "rounded-b-sm"
    | Rounded_b_md -> "rounded-b-md"
    | Rounded_b_lg -> "rounded-b-lg"
    | Rounded_b_xl -> "rounded-b-xl"
    | Rounded_b_2xl -> "rounded-b-2xl"
    | Rounded_b_3xl -> "rounded-b-3xl"
    | Rounded_b_full -> "rounded-b-full"
    (* Side-specific rounded utilities - left *)
    | Rounded_l -> "rounded-l"
    | Rounded_l_none -> "rounded-l-none"
    | Rounded_l_sm -> "rounded-l-sm"
    | Rounded_l_md -> "rounded-l-md"
    | Rounded_l_lg -> "rounded-l-lg"
    | Rounded_l_xl -> "rounded-l-xl"
    | Rounded_l_2xl -> "rounded-l-2xl"
    | Rounded_l_3xl -> "rounded-l-3xl"
    | Rounded_l_full -> "rounded-l-full"
    (* Corner-specific rounded utilities - top-left *)
    | Rounded_tl -> "rounded-tl"
    | Rounded_tl_none -> "rounded-tl-none"
    | Rounded_tl_sm -> "rounded-tl-sm"
    | Rounded_tl_md -> "rounded-tl-md"
    | Rounded_tl_lg -> "rounded-tl-lg"
    | Rounded_tl_xl -> "rounded-tl-xl"
    | Rounded_tl_2xl -> "rounded-tl-2xl"
    | Rounded_tl_3xl -> "rounded-tl-3xl"
    | Rounded_tl_full -> "rounded-tl-full"
    (* Corner-specific rounded utilities - top-right *)
    | Rounded_tr -> "rounded-tr"
    | Rounded_tr_none -> "rounded-tr-none"
    | Rounded_tr_sm -> "rounded-tr-sm"
    | Rounded_tr_md -> "rounded-tr-md"
    | Rounded_tr_lg -> "rounded-tr-lg"
    | Rounded_tr_xl -> "rounded-tr-xl"
    | Rounded_tr_2xl -> "rounded-tr-2xl"
    | Rounded_tr_3xl -> "rounded-tr-3xl"
    | Rounded_tr_full -> "rounded-tr-full"
    (* Corner-specific rounded utilities - bottom-right *)
    | Rounded_br -> "rounded-br"
    | Rounded_br_none -> "rounded-br-none"
    | Rounded_br_sm -> "rounded-br-sm"
    | Rounded_br_md -> "rounded-br-md"
    | Rounded_br_lg -> "rounded-br-lg"
    | Rounded_br_xl -> "rounded-br-xl"
    | Rounded_br_2xl -> "rounded-br-2xl"
    | Rounded_br_3xl -> "rounded-br-3xl"
    | Rounded_br_full -> "rounded-br-full"
    (* Corner-specific rounded utilities - bottom-left *)
    | Rounded_bl -> "rounded-bl"
    | Rounded_bl_none -> "rounded-bl-none"
    | Rounded_bl_sm -> "rounded-bl-sm"
    | Rounded_bl_md -> "rounded-bl-md"
    | Rounded_bl_lg -> "rounded-bl-lg"
    | Rounded_bl_xl -> "rounded-bl-xl"
    | Rounded_bl_2xl -> "rounded-bl-2xl"
    | Rounded_bl_3xl -> "rounded-bl-3xl"
    | Rounded_bl_full -> "rounded-bl-full"
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

(** {2 Side-specific rounded utilities - top} *)

let rounded_t = utility Rounded_t
let rounded_t_none = utility Rounded_t_none
let rounded_t_sm = utility Rounded_t_sm
let rounded_t_md = utility Rounded_t_md
let rounded_t_lg = utility Rounded_t_lg
let rounded_t_xl = utility Rounded_t_xl
let rounded_t_2xl = utility Rounded_t_2xl
let rounded_t_3xl = utility Rounded_t_3xl
let rounded_t_full = utility Rounded_t_full

(** {2 Side-specific rounded utilities - right} *)

let rounded_r = utility Rounded_r
let rounded_r_none = utility Rounded_r_none
let rounded_r_sm = utility Rounded_r_sm
let rounded_r_md = utility Rounded_r_md
let rounded_r_lg = utility Rounded_r_lg
let rounded_r_xl = utility Rounded_r_xl
let rounded_r_2xl = utility Rounded_r_2xl
let rounded_r_3xl = utility Rounded_r_3xl
let rounded_r_full = utility Rounded_r_full

(** {2 Side-specific rounded utilities - bottom} *)

let rounded_b = utility Rounded_b
let rounded_b_none = utility Rounded_b_none
let rounded_b_sm = utility Rounded_b_sm
let rounded_b_md = utility Rounded_b_md
let rounded_b_lg = utility Rounded_b_lg
let rounded_b_xl = utility Rounded_b_xl
let rounded_b_2xl = utility Rounded_b_2xl
let rounded_b_3xl = utility Rounded_b_3xl
let rounded_b_full = utility Rounded_b_full

(** {2 Side-specific rounded utilities - left} *)

let rounded_l = utility Rounded_l
let rounded_l_none = utility Rounded_l_none
let rounded_l_sm = utility Rounded_l_sm
let rounded_l_md = utility Rounded_l_md
let rounded_l_lg = utility Rounded_l_lg
let rounded_l_xl = utility Rounded_l_xl
let rounded_l_2xl = utility Rounded_l_2xl
let rounded_l_3xl = utility Rounded_l_3xl
let rounded_l_full = utility Rounded_l_full

(** {2 Corner-specific rounded utilities - top-left} *)

let rounded_tl = utility Rounded_tl
let rounded_tl_none = utility Rounded_tl_none
let rounded_tl_sm = utility Rounded_tl_sm
let rounded_tl_md = utility Rounded_tl_md
let rounded_tl_lg = utility Rounded_tl_lg
let rounded_tl_xl = utility Rounded_tl_xl
let rounded_tl_2xl = utility Rounded_tl_2xl
let rounded_tl_3xl = utility Rounded_tl_3xl
let rounded_tl_full = utility Rounded_tl_full

(** {2 Corner-specific rounded utilities - top-right} *)

let rounded_tr = utility Rounded_tr
let rounded_tr_none = utility Rounded_tr_none
let rounded_tr_sm = utility Rounded_tr_sm
let rounded_tr_md = utility Rounded_tr_md
let rounded_tr_lg = utility Rounded_tr_lg
let rounded_tr_xl = utility Rounded_tr_xl
let rounded_tr_2xl = utility Rounded_tr_2xl
let rounded_tr_3xl = utility Rounded_tr_3xl
let rounded_tr_full = utility Rounded_tr_full

(** {2 Corner-specific rounded utilities - bottom-right} *)

let rounded_br = utility Rounded_br
let rounded_br_none = utility Rounded_br_none
let rounded_br_sm = utility Rounded_br_sm
let rounded_br_md = utility Rounded_br_md
let rounded_br_lg = utility Rounded_br_lg
let rounded_br_xl = utility Rounded_br_xl
let rounded_br_2xl = utility Rounded_br_2xl
let rounded_br_3xl = utility Rounded_br_3xl
let rounded_br_full = utility Rounded_br_full

(** {2 Corner-specific rounded utilities - bottom-left} *)

let rounded_bl = utility Rounded_bl
let rounded_bl_none = utility Rounded_bl_none
let rounded_bl_sm = utility Rounded_bl_sm
let rounded_bl_md = utility Rounded_bl_md
let rounded_bl_lg = utility Rounded_bl_lg
let rounded_bl_xl = utility Rounded_bl_xl
let rounded_bl_2xl = utility Rounded_bl_2xl
let rounded_bl_3xl = utility Rounded_bl_3xl
let rounded_bl_full = utility Rounded_bl_full

(** {1 Outline Utilities} *)

let outline_none = utility Outline_none
let outline_offset_0 = utility Outline_offset_0
let outline_offset_1 = utility Outline_offset_1
let outline_offset_2 = utility Outline_offset_2
let outline_offset_4 = utility Outline_offset_4
let outline_offset_8 = utility Outline_offset_8
