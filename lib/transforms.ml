(** Transform utilities for 2D and 3D transformations. *)

(** Error helpers *)
let err_not_utility = Error (`Msg "Not a transform utility")

module Handler = struct
  open Style
  open Css

  type t =
    | (* 2D Transforms *)
      Rotate of int
    | Rotate_arbitrary of Css.angle
    | Translate_x of int
    | Translate_x_full
    | Translate_x_px
    | Translate_x_arbitrary of Css.length
    | Translate_y of int
    | Translate_y_full
    | Translate_y_px
    | Translate_y_arbitrary of Css.length
    | Scale of int
    | Scale_arbitrary of float
    | Scale_x of int
    | Scale_x_arbitrary of float
    | Scale_y of int
    | Scale_y_arbitrary of float
    | Skew_x of int
    | Skew_x_arbitrary of Css.angle
    | Skew_y of int
    | Skew_y_arbitrary of Css.angle
    | Skew of int
    | (* Combined translate utilities *)
      Translate_full
    | Translate_1_2
    | Translate_arbitrary of Css.length
    | (* Negative translate utilities *)
      Neg_translate_arbitrary of string
    | Neg_translate_full
    | Neg_translate_x_arbitrary of string
    | Neg_translate_x_full
    | Neg_translate_x_1_2
    | Neg_translate_y_arbitrary of string
    | Neg_translate_y_full
    | Neg_translate_y_1_2
    | (* 3D Transforms *)
      Translate_z of int
    | Translate_z_px
    | Neg_translate_z_arbitrary of string
    | Neg_translate_z_px
    | Translate_3d
    | Rotate_x of int
    | Rotate_x_arbitrary of Css.angle
    | Rotate_y of int
    | Rotate_y_arbitrary of Css.angle
    | Rotate_z of int
    | Rotate_z_arbitrary of Css.angle
    | Scale_z of int
    | Scale_3d
    | Perspective_none
    | Perspective_dramatic
    | Perspective_normal
    | Perspective_arbitrary of Css.length
    | (* Perspective origin *)
      Perspective_origin_center
    | Perspective_origin_top
    | Perspective_origin_bottom
    | Perspective_origin_left
    | Perspective_origin_right
    | Perspective_origin_top_left
    | Perspective_origin_top_right
    | Perspective_origin_bottom_left
    | Perspective_origin_bottom_right
    | Perspective_origin_arbitrary of string
    | (* Transform style *)
      Transform_style_3d
    | Transform_style_flat
    | (* Transform box *)
      Transform_box_border
    | Transform_box_content
    | Transform_box_fill
    | Transform_box_stroke
    | Transform_box_view
    | (* Backface visibility *)
      Backface_visible
    | Backface_hidden
    | (* Transform control *)
      Transform
    | Transform_cpu
    | Transform_none
    | Transform_gpu
    | (* Transform origin *)
      Origin_center
    | Origin_top
    | Origin_bottom
    | Origin_left
    | Origin_right
    | Origin_top_left
    | Origin_top_right
    | Origin_bottom_left
    | Origin_bottom_right
    | Origin_arbitrary of string

  type Utility.base += Self of t

  (** Priority for transform utilities *)
  let name = "transforms"

  (* Match Tailwind ordering: transforms before animations and cursor *)
  let priority = 9

  (* Tailwind v4 uses rotate-x/y/z and skew-x/y variables for the transform
     utility. These variables contain the full transform function values, e.g.:
     --tw-rotate-x: rotateX(45deg) --tw-skew-x: skewX(10deg) *)
  (* Property ordering to match Tailwind v4:
     translate (0-2), scale (3-5), then other properties follow *)

  (* Translate variables - use property_default pattern with initial 0 *)
  (* Translate properties don't appear in @layer properties in standard Tailwind - only used locally *)
  let tw_translate_x_var =
    Var.property_default Css.Length ~initial:Css.Zero ~universal:true
      ~property_order:0 ~family:`Translate "tw-translate-x"

  let tw_translate_y_var =
    Var.property_default Css.Length ~initial:Css.Zero ~universal:true
      ~property_order:1 ~family:`Translate "tw-translate-y"

  let tw_translate_z_var =
    Var.property_default Css.Length ~initial:Css.Zero ~universal:true
      ~property_order:2 ~family:`Translate "tw-translate-z"

  (* Scale variables - property_order (6-8) comes after duration (5) *)
  let tw_scale_x_var =
    Var.property_default Css.Number_percentage ~initial:(Num 1.0)
      ~universal:true ~property_order:6 ~family:`Scale "tw-scale-x"

  let tw_scale_y_var =
    Var.property_default Css.Number_percentage ~initial:(Num 1.0)
      ~universal:true ~property_order:7 ~family:`Scale "tw-scale-y"

  let tw_scale_z_var =
    Var.property_default Css.Number_percentage ~initial:(Num 1.0)
      ~universal:true ~property_order:8 ~family:`Scale "tw-scale-z"

  (* Rotate and skew properties appear FIRST in @layer properties (0-4) *)
  let tw_rotate_x_var =
    Var.channel ~needs_property:true ~property_order:0 ~family:`Rotate
      Css.Transform "tw-rotate-x"

  let tw_rotate_y_var =
    Var.channel ~needs_property:true ~property_order:1 ~family:`Rotate
      Css.Transform "tw-rotate-y"

  let tw_rotate_z_var =
    Var.channel ~needs_property:true ~property_order:2 ~family:`Rotate
      Css.Transform "tw-rotate-z"

  let tw_skew_x_var =
    Var.channel ~needs_property:true ~property_order:3 ~family:`Skew
      Css.Transform "tw-skew-x"

  let tw_skew_y_var =
    Var.channel ~needs_property:true ~property_order:4 ~family:`Skew
      Css.Transform "tw-skew-y"

  (* Perspective theme variables *)
  let perspective_dramatic_var =
    Var.theme Css.Length "perspective-dramatic" ~order:(7, 13)

  let perspective_normal_var =
    Var.theme Css.Length "perspective-normal" ~order:(7, 14)

  (** {1 Helpers} *)

  let collect_property_rules vars =
    List.filter_map Var.property_rule vars |> concat

  (* Property rules for rotate/skew transform chain *)
  let rotate_skew_props =
    collect_property_rules
      [
        tw_rotate_x_var;
        tw_rotate_y_var;
        tw_rotate_z_var;
        tw_skew_x_var;
        tw_skew_y_var;
      ]

  (* Helper to create transform with variable chain: Sets one variable and
     outputs transform with all rotate/skew vars *)
  let transform_with_var var transform_val =
    let d, _ = Var.binding var transform_val in
    let rotate_x_ref = Var.reference_with_empty_fallback tw_rotate_x_var in
    let rotate_y_ref = Var.reference_with_empty_fallback tw_rotate_y_var in
    let rotate_z_ref = Var.reference_with_empty_fallback tw_rotate_z_var in
    let skew_x_ref = Var.reference_with_empty_fallback tw_skew_x_var in
    let skew_y_ref = Var.reference_with_empty_fallback tw_skew_y_var in
    style ~property_rules:rotate_skew_props
      [
        d;
        transforms
          [
            Var rotate_x_ref;
            Var rotate_y_ref;
            Var rotate_z_ref;
            Var skew_x_ref;
            Var skew_y_ref;
          ];
      ]

  (* Helper to create an angle with calc for negative values *)
  let make_angle deg =
    if deg >= 0 then Deg (float_of_int deg)
    else Calc (Expr (Val (Deg (float_of_int (abs deg))), Mul, Num (-1.)))

  (* Helper to create a percentage with calc for negative values *)
  let make_pct n : Css.number_percentage =
    if n >= 0 then Pct (float_of_int n)
    else Calc (Expr (Val (Pct (float_of_int (abs n))), Mul, Num (-1.)))

  let transform_with_both_skew deg =
    let angle = make_angle deg in
    let skew_x_decl, _ = Var.binding tw_skew_x_var (Skew_x angle) in
    let skew_y_decl, _ = Var.binding tw_skew_y_var (Skew_y angle) in
    let rotate_x_ref = Var.reference_with_empty_fallback tw_rotate_x_var in
    let rotate_y_ref = Var.reference_with_empty_fallback tw_rotate_y_var in
    let rotate_z_ref = Var.reference_with_empty_fallback tw_rotate_z_var in
    let skew_x_ref = Var.reference_with_empty_fallback tw_skew_x_var in
    let skew_y_ref = Var.reference_with_empty_fallback tw_skew_y_var in
    style ~property_rules:rotate_skew_props
      [
        skew_x_decl;
        skew_y_decl;
        transforms
          [
            Var rotate_x_ref;
            Var rotate_y_ref;
            Var rotate_z_ref;
            Var skew_x_ref;
            Var skew_y_ref;
          ];
      ]

  let neg_class name n =
    let prefix = if n < 0 then "-" else "" in
    prefix ^ name ^ string_of_int (abs n)

  let parse_bracket_length s : (Css.length, _) result =
    if String.length s >= 3 && s.[0] = '[' && s.[String.length s - 1] = ']' then (
      let inner = String.sub s 1 (String.length s - 2) in
      let slen = String.length inner in
      let i = ref 0 in
      while
        !i < slen
        && ((inner.[!i] >= '0' && inner.[!i] <= '9') || inner.[!i] = '.')
      do
        incr i
      done;
      if !i = 0 || !i = slen then
        Error (`Msg ("Invalid length value: " ^ inner))
      else
        let num_s = String.sub inner 0 !i in
        let unit_s = String.sub inner !i (slen - !i) in
        match Float.of_string_opt num_s with
        | Option.None -> Error (`Msg ("Invalid length value: " ^ inner))
        | Option.Some n -> (
            match unit_s with
            | "px" -> Ok (Px n : Css.length)
            | "rem" -> Ok (Rem n)
            | "em" -> Ok (Em n)
            | "vw" -> Ok (Vw n)
            | "vh" -> Ok (Vh n)
            | "cm" -> Ok (Cm n)
            | "mm" -> Ok (Mm n)
            | "pt" -> Ok (Pt n)
            | "%" -> Ok (Pct n)
            | _ -> Error (`Msg ("Invalid length unit: " ^ unit_s))))
    else Error (`Msg ("Not a bracket value: " ^ s))

  let parse_bracket_angle s : (Css.angle, _) result =
    if String.length s >= 3 && s.[0] = '[' && s.[String.length s - 1] = ']' then (
      let inner = String.sub s 1 (String.length s - 2) in
      let slen = String.length inner in
      let i = ref 0 in
      while
        !i < slen
        && ((inner.[!i] >= '0' && inner.[!i] <= '9')
           || inner.[!i] = '.'
           || inner.[!i] = '-')
      do
        incr i
      done;
      if !i = 0 || !i = slen then Error (`Msg ("Invalid angle value: " ^ inner))
      else
        let num_s = String.sub inner 0 !i in
        let unit_s = String.sub inner !i (slen - !i) in
        match Float.of_string_opt num_s with
        | Option.None -> Error (`Msg ("Invalid angle value: " ^ inner))
        | Option.Some n -> (
            match unit_s with
            | "deg" -> Ok (Css.Deg n : Css.angle)
            | "rad" -> Ok (Rad n)
            | "turn" -> Ok (Turn n)
            | "grad" -> Ok (Grad n)
            | _ -> Error (`Msg ("Invalid angle unit: " ^ unit_s))))
    else Error (`Msg ("Not a bracket value: " ^ s))

  let parse_bracket_number s : (float, _) result =
    if String.length s >= 3 && s.[0] = '[' && s.[String.length s - 1] = ']' then
      let inner = String.sub s 1 (String.length s - 2) in
      match Float.of_string_opt inner with
      | Some f -> Ok f
      | None -> Error (`Msg ("Invalid number: " ^ inner))
    else Error (`Msg ("Not a bracket value: " ^ s))

  (** {1 2D Transform Utilities} *)

  let rotate n = style [ Css.rotate (Angle (Deg (float_of_int n))) ]
  let rotate_arbitrary angle = style [ Css.rotate (Angle angle) ]

  let translate_props =
    collect_property_rules
      [ tw_translate_x_var; tw_translate_y_var; tw_translate_z_var ]

  let translate_axis axis_var n =
    let spacing_decl, spacing_ref =
      Var.binding Theme.spacing_var (Css.Rem 0.25)
    in
    let spacing_value : Css.length =
      Css.Calc
        (Css.Calc.mul
           (Css.Calc.length (Css.Var spacing_ref))
           (Css.Calc.float (float_of_int n)))
    in
    let axis_decl, _ = Var.binding axis_var spacing_value in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    style ~property_rules:translate_props
      (spacing_decl :: axis_decl
      :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  let translate_x n = translate_axis tw_translate_x_var n
  let translate_y n = translate_axis tw_translate_y_var n

  let translate_x_full =
    let axis_decl, _ = Var.binding tw_translate_x_var (Pct 100.0) in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    style ~property_rules:translate_props
      (axis_decl :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  let translate_x_px =
    let axis_decl, _ = Var.binding tw_translate_x_var (Px 1.0) in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    style ~property_rules:translate_props
      (axis_decl :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  let translate_x_arbitrary len =
    let axis_decl, _ = Var.binding tw_translate_x_var len in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    style ~property_rules:translate_props
      (axis_decl :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  let translate_y_full =
    let axis_decl, _ = Var.binding tw_translate_y_var (Pct 100.0) in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    style ~property_rules:translate_props
      (axis_decl :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  let translate_y_px =
    let axis_decl, _ = Var.binding tw_translate_y_var (Px 1.0) in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    style ~property_rules:translate_props
      (axis_decl :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  let translate_y_arbitrary len =
    let axis_decl, _ = Var.binding tw_translate_y_var len in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    style ~property_rules:translate_props
      (axis_decl :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  let neg_translate_x_arbitrary_style s =
    let bare_name = Parse.extract_var_name s in
    let neg_len : Css.length =
      Calc (Calc.mul (Calc.var bare_name) (Calc.float (-1.)))
    in
    let axis_decl, _ = Var.binding tw_translate_x_var neg_len in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    style ~property_rules:translate_props
      (axis_decl :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  let neg_translate_y_arbitrary_style s =
    let bare_name = Parse.extract_var_name s in
    let neg_len : Css.length =
      Calc (Calc.mul (Calc.var bare_name) (Calc.float (-1.)))
    in
    let axis_decl, _ = Var.binding tw_translate_y_var neg_len in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    style ~property_rules:translate_props
      (axis_decl :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  let neg_translate_x_full =
    let axis_decl, _ = Var.binding tw_translate_x_var (Pct (-100.0)) in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    style ~property_rules:translate_props
      (axis_decl :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  let neg_translate_y_full =
    let axis_decl, _ = Var.binding tw_translate_y_var (Pct (-100.0)) in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    style ~property_rules:translate_props
      (axis_decl :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  let scale n =
    let value = make_pct n in
    let dx, _ = Var.binding tw_scale_x_var value in
    let dy, _ = Var.binding tw_scale_y_var value in
    let dz, _ = Var.binding tw_scale_z_var value in
    let props =
      collect_property_rules [ tw_scale_x_var; tw_scale_y_var; tw_scale_z_var ]
    in
    (* Also set the scale property with variable references *)
    let scale_x_ref = Var.reference tw_scale_x_var in
    let scale_y_ref = Var.reference tw_scale_y_var in
    style ~property_rules:props
      (dx :: dy :: dz :: [ Css.scale (XY (Var scale_x_ref, Var scale_y_ref)) ])

  let scale_x n =
    let value = make_pct n in
    let d, _ = Var.binding tw_scale_x_var value in
    let props =
      collect_property_rules [ tw_scale_x_var; tw_scale_y_var; tw_scale_z_var ]
    in
    let scale_x_ref = Var.reference tw_scale_x_var in
    let scale_y_ref = Var.reference tw_scale_y_var in
    style ~property_rules:props
      (d :: [ Css.scale (XY (Var scale_x_ref, Var scale_y_ref)) ])

  let scale_y n =
    let value = make_pct n in
    let d, _ = Var.binding tw_scale_y_var value in
    let props =
      collect_property_rules [ tw_scale_x_var; tw_scale_y_var; tw_scale_z_var ]
    in
    let scale_x_ref = Var.reference tw_scale_x_var in
    let scale_y_ref = Var.reference tw_scale_y_var in
    style ~property_rules:props
      (d :: [ Css.scale (XY (Var scale_x_ref, Var scale_y_ref)) ])

  let scale_arbitrary f =
    let value : Css.number_percentage = Css.Num f in
    let dx, _ = Var.binding tw_scale_x_var value in
    let dy, _ = Var.binding tw_scale_y_var value in
    let dz, _ = Var.binding tw_scale_z_var value in
    let props =
      collect_property_rules [ tw_scale_x_var; tw_scale_y_var; tw_scale_z_var ]
    in
    let scale_x_ref = Var.reference tw_scale_x_var in
    let scale_y_ref = Var.reference tw_scale_y_var in
    style ~property_rules:props
      (dx :: dy :: dz :: [ Css.scale (XY (Var scale_x_ref, Var scale_y_ref)) ])

  let scale_x_arbitrary f =
    let value : Css.number_percentage = Css.Num f in
    let d, _ = Var.binding tw_scale_x_var value in
    style (d :: [ Css.transform (Scale_x f) ])

  let scale_y_arbitrary f =
    let value : Css.number_percentage = Css.Num f in
    let d, _ = Var.binding tw_scale_y_var value in
    style (d :: [ Css.transform (Scale_y f) ])

  let skew_x deg = transform_with_var tw_skew_x_var (Skew_x (make_angle deg))
  let skew_y deg = transform_with_var tw_skew_y_var (Skew_y (make_angle deg))
  let skew_x_arbitrary angle = transform_with_var tw_skew_x_var (Skew_x angle)
  let skew_y_arbitrary angle = transform_with_var tw_skew_y_var (Skew_y angle)

  (* Combined translate utilities *)
  let translate_full =
    let dx, _ = Var.binding tw_translate_x_var (Pct 100.0) in
    let dy, _ = Var.binding tw_translate_y_var (Pct 100.0) in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    let props =
      collect_property_rules
        [ tw_translate_x_var; tw_translate_y_var; tw_translate_z_var ]
    in
    style ~property_rules:props
      (dx :: dy :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  let translate_1_2 =
    (* Tailwind outputs calc(1 / 2 * 100%) rather than 50% *)
    let half_pct : Css.length =
      Css.Calc
        (Css.Calc.mul
           (Css.Calc.div (Css.Calc.float 1.) (Css.Calc.float 2.))
           (Css.Calc.length (Css.Pct 100.)))
    in
    let dx, _ = Var.binding tw_translate_x_var half_pct in
    let dy, _ = Var.binding tw_translate_y_var half_pct in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    let props =
      collect_property_rules
        [ tw_translate_x_var; tw_translate_y_var; tw_translate_z_var ]
    in
    style ~property_rules:props
      (dx :: dy :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  let translate_arbitrary len =
    let dx, _ = Var.binding tw_translate_x_var len in
    let dy, _ = Var.binding tw_translate_y_var len in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    let props =
      collect_property_rules
        [ tw_translate_x_var; tw_translate_y_var; tw_translate_z_var ]
    in
    style ~property_rules:props
      (dx :: dy :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  let neg_translate_arbitrary_style s =
    (* Parse var name and negate: calc(var(--value) * -1) *)
    let bare_name = Parse.extract_var_name s in
    let neg_len : Css.length =
      Calc (Calc.mul (Calc.var bare_name) (Calc.float (-1.)))
    in
    let dx, _ = Var.binding tw_translate_x_var neg_len in
    let dy, _ = Var.binding tw_translate_y_var neg_len in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    let props =
      collect_property_rules
        [ tw_translate_x_var; tw_translate_y_var; tw_translate_z_var ]
    in
    style ~property_rules:props
      (dx :: dy :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  let neg_translate_full =
    let dx, _ = Var.binding tw_translate_x_var (Pct (-100.0)) in
    let dy, _ = Var.binding tw_translate_y_var (Pct (-100.0)) in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    let props =
      collect_property_rules
        [ tw_translate_x_var; tw_translate_y_var; tw_translate_z_var ]
    in
    style ~property_rules:props
      (dx :: dy :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  (* Negative translate utilities for centering *)
  let neg_translate_x_1_2 =
    style [ Css.transform (Css.Translate_x (Css.Pct (-50.0))) ]

  let neg_translate_y_1_2 =
    style [ Css.transform (Css.Translate_y (Css.Pct (-50.0))) ]

  (** {1 3D Transform Utilities} *)

  let rotate_x n = transform_with_var tw_rotate_x_var (Rotate_x (make_angle n))

  let rotate_x_arbitrary angle =
    transform_with_var tw_rotate_x_var (Rotate_x angle)

  let rotate_y n = transform_with_var tw_rotate_y_var (Rotate_y (make_angle n))

  let rotate_y_arbitrary angle =
    transform_with_var tw_rotate_y_var (Rotate_y angle)

  let rotate_z n = transform_with_var tw_rotate_z_var (Rotate_z (make_angle n))

  let rotate_z_arbitrary angle =
    transform_with_var tw_rotate_z_var (Rotate_z angle)

  let translate_z n =
    let spacing_decl, spacing_ref =
      Var.binding Theme.spacing_var (Css.Rem 0.25)
    in
    let spacing_value : Css.length =
      Css.Calc
        (Css.Calc.mul
           (Css.Calc.length (Css.Var spacing_ref))
           (Css.Calc.float (float_of_int n)))
    in
    let axis_decl, _ = Var.binding tw_translate_z_var spacing_value in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    let tz_ref = Var.reference tw_translate_z_var in
    style ~property_rules:translate_props
      (spacing_decl :: axis_decl
      :: [ Css.translate (XYZ (Var tx_ref, Var ty_ref, Var tz_ref)) ])

  let translate_z_px =
    let axis_decl, _ = Var.binding tw_translate_z_var (Px 1.0) in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    let tz_ref = Var.reference tw_translate_z_var in
    style ~property_rules:translate_props
      (axis_decl :: [ Css.translate (XYZ (Var tx_ref, Var ty_ref, Var tz_ref)) ])

  let neg_translate_z_arbitrary_style s =
    let bare_name = Parse.extract_var_name s in
    let neg_len : Css.length =
      Calc (Calc.mul (Calc.var bare_name) (Calc.float (-1.)))
    in
    let axis_decl, _ = Var.binding tw_translate_z_var neg_len in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    let tz_ref = Var.reference tw_translate_z_var in
    style ~property_rules:translate_props
      (axis_decl :: [ Css.translate (XYZ (Var tx_ref, Var ty_ref, Var tz_ref)) ])

  let neg_translate_z_px =
    let axis_decl, _ = Var.binding tw_translate_z_var (Px (-1.0)) in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    let tz_ref = Var.reference tw_translate_z_var in
    style ~property_rules:translate_props
      (axis_decl :: [ Css.translate (XYZ (Var tx_ref, Var ty_ref, Var tz_ref)) ])

  let translate_3d =
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    let tz_ref = Var.reference tw_translate_z_var in
    style ~property_rules:translate_props
      [ Css.translate (XYZ (Var tx_ref, Var ty_ref, Var tz_ref)) ]

  let scale_z n =
    let value = make_pct n in
    let d, _ = Var.binding tw_scale_z_var value in
    let props =
      collect_property_rules [ tw_scale_x_var; tw_scale_y_var; tw_scale_z_var ]
    in
    let scale_x_ref = Var.reference tw_scale_x_var in
    let scale_y_ref = Var.reference tw_scale_y_var in
    let scale_z_ref = Var.reference tw_scale_z_var in
    style ~property_rules:props
      (d
      :: [ Css.scale (XYZ (Var scale_x_ref, Var scale_y_ref, Var scale_z_ref)) ]
      )

  let scale_3d =
    let props =
      collect_property_rules [ tw_scale_x_var; tw_scale_y_var; tw_scale_z_var ]
    in
    let scale_x_ref = Var.reference tw_scale_x_var in
    let scale_y_ref = Var.reference tw_scale_y_var in
    let scale_z_ref = Var.reference tw_scale_z_var in
    style ~property_rules:props
      [ Css.scale (XYZ (Var scale_x_ref, Var scale_y_ref, Var scale_z_ref)) ]

  let perspective_none =
    let v : Css.length =
      Var
        (Var.theme_ref "perspective-none"
           ~default:(None : Css.length)
           ~default_css:"none")
    in
    style [ Css.perspective v ]

  let perspective_dramatic =
    let decl, r = Var.binding perspective_dramatic_var (Px 100.0) in
    style (decl :: [ Css.perspective (Var r) ])

  let perspective_normal =
    let decl, r = Var.binding perspective_normal_var (Px 500.0) in
    style (decl :: [ Css.perspective (Var r) ])

  let perspective_arbitrary len = style [ Css.perspective len ]

  let po_with_ref name (default : Css.perspective_origin) default_css () =
    match Var.get_theme_value name with
    | Some value_str ->
        let decl =
          Css.custom_declaration ~layer:"theme" ("--" ^ name) Css.String
            value_str
        in
        let ref : Css.perspective_origin =
          Perspective_var (Css.var_ref ~layer:"theme" name)
        in
        let perspective_ref : Css.length =
          Css.Var (Css.var_ref ~layer:"theme" name)
        in
        style [ decl; perspective_origin ref; Css.perspective perspective_ref ]
    | None ->
        let v : Css.perspective_origin =
          Perspective_var (Var.theme_ref name ~default ~default_css)
        in
        let perspective_ref : Css.length =
          Css.Var (Css.var_ref ~layer:"theme" name)
        in
        style
          [
            perspective_origin v;
            Css.theme_guarded ~var_name:name (Css.perspective perspective_ref);
          ]

  let perspective_origin_center () =
    po_with_ref "perspective-origin-center" Perspective_center "center" ()

  let perspective_origin_top () =
    po_with_ref "perspective-origin-top" Perspective_top "top" ()

  let perspective_origin_bottom () =
    po_with_ref "perspective-origin-bottom" Perspective_bottom "bottom" ()

  let perspective_origin_left () =
    po_with_ref "perspective-origin-left" (Perspective_x Zero) "0" ()

  let perspective_origin_right () =
    po_with_ref "perspective-origin-right" (Perspective_x (Pct 100.)) "100%" ()

  let perspective_origin_top_left () =
    po_with_ref "perspective-origin-top-left"
      (Perspective_xy (Zero, Zero))
      "0 0" ()

  let perspective_origin_top_right () =
    po_with_ref "perspective-origin-top-right"
      (Perspective_xy (Pct 100., Zero))
      "100% 0" ()

  let perspective_origin_bottom_left () =
    po_with_ref "perspective-origin-bottom-left"
      (Perspective_xy (Zero, Pct 100.))
      "0 100%" ()

  let perspective_origin_bottom_right () =
    po_with_ref "perspective-origin-bottom-right"
      (Perspective_xy (Pct 100., Pct 100.))
      "100% 100%" ()

  let perspective_origin_arbitrary s =
    (* Convert underscore to space for arbitrary values like 50px_100px *)
    let value = String.map (fun c -> if c = '_' then ' ' else c) s in
    style [ perspective_origin (Css.Perspective_arbitrary value) ]

  let transform_style_3d = style [ transform_style Preserve_3d ]
  let transform_style_flat = style [ transform_style Flat ]
  let transform_box_border = style [ Css.transform_box Border_box ]
  let transform_box_content = style [ Css.transform_box Content_box ]
  let transform_box_fill = style [ Css.transform_box Fill_box ]
  let transform_box_stroke = style [ Css.transform_box Stroke_box ]
  let transform_box_view = style [ Css.transform_box View_box ]
  let backface_visible = style [ backface_visibility Visible ]
  let backface_hidden = style [ backface_visibility Hidden ]

  (** {1 Transform Origin Utilities}

      Tailwind v4 uses theme variable references for origin utilities when theme
      variables are defined. *)

  let origin_with_ref name (default : Css.transform_origin) default_css () =
    match Var.get_theme_value name with
    | Some value_str ->
        let decl =
          Css.custom_declaration ~layer:"theme" ("--" ^ name) Css.String
            value_str
        in
        let ref_str = "var(--" ^ name ^ ") var(--" ^ name ^ ")" in
        style [ decl; transform_origin (Arbitrary ref_str) ]
    | None ->
        let v : Css.transform_origin =
          Var (Var.theme_ref name ~default ~default_css)
        in
        style [ transform_origin v ]

  let origin_center () =
    origin_with_ref "transform-origin-center" Center "center" ()

  let origin_top () = origin_with_ref "transform-origin-top" Top "top" ()

  let origin_bottom () =
    origin_with_ref "transform-origin-bottom" Bottom "bottom" ()

  let origin_left () =
    origin_with_ref "transform-origin-left" (XY (Zero, Zero)) "0 0" ()

  let origin_right () =
    origin_with_ref "transform-origin-right"
      (XY (Pct 100., Pct 100.))
      "100% 100%" ()

  let origin_top_left () =
    origin_with_ref "transform-origin-top-left" (XY (Zero, Zero)) "0 0" ()

  let origin_top_right () =
    origin_with_ref "transform-origin-top-right"
      (XY (Pct 100., Zero))
      "100% 0" ()

  let origin_bottom_left () =
    origin_with_ref "transform-origin-bottom-left"
      (XY (Zero, Pct 100.))
      "0 100%" ()

  let origin_bottom_right () =
    origin_with_ref "transform-origin-bottom-right"
      (XY (Pct 100., Pct 100.))
      "100% 100%" ()

  let origin_arbitrary s =
    (* Convert underscore to space for arbitrary values like 50px_100px *)
    let value = String.map (fun c -> if c = '_' then ' ' else c) s in
    (* If value has no spaces (single value like var(--x)), duplicate for x and
       y *)
    let value =
      if not (String.contains value ' ') then value ^ " " ^ value else value
    in
    style [ transform_origin (Arbitrary value) ]

  (** {1 Transform Control Utilities} *)

  (* Tailwind v4 transform utility uses individual rotate-x/y/z and skew-x/y
     variables with empty fallbacks. The output is: transform:
     var(--tw-rotate-x,) var(--tw-rotate-y,) var(--tw-rotate-z,)
     var(--tw-skew-x,) var(--tw-skew-y,); When a variable is set (e.g.,
     --tw-rotate-x: rotateX(45deg)), it becomes part of the transform. *)
  let transform =
    (* Create var references with Empty fallbacks for optional transform
       components *)
    let rotate_x_ref = Var.reference_with_empty_fallback tw_rotate_x_var in
    let rotate_y_ref = Var.reference_with_empty_fallback tw_rotate_y_var in
    let rotate_z_ref = Var.reference_with_empty_fallback tw_rotate_z_var in
    let skew_x_ref = Var.reference_with_empty_fallback tw_skew_x_var in
    let skew_y_ref = Var.reference_with_empty_fallback tw_skew_y_var in
    (* Collect @property rules for these variables *)
    let property_rules =
      collect_property_rules
        [
          tw_rotate_x_var;
          tw_rotate_y_var;
          tw_rotate_z_var;
          tw_skew_x_var;
          tw_skew_y_var;
        ]
    in
    style ~property_rules
      [
        transforms
          [
            Var rotate_x_ref;
            Var rotate_y_ref;
            Var rotate_z_ref;
            Var skew_x_ref;
            Var skew_y_ref;
          ];
      ]

  let transform_none = style [ Css.transform None ]

  (* transform-cpu is an alias for transform *)
  let transform_cpu =
    let rotate_x_ref = Var.reference_with_empty_fallback tw_rotate_x_var in
    let rotate_y_ref = Var.reference_with_empty_fallback tw_rotate_y_var in
    let rotate_z_ref = Var.reference_with_empty_fallback tw_rotate_z_var in
    let skew_x_ref = Var.reference_with_empty_fallback tw_skew_x_var in
    let skew_y_ref = Var.reference_with_empty_fallback tw_skew_y_var in
    let property_rules =
      collect_property_rules
        [
          tw_rotate_x_var;
          tw_rotate_y_var;
          tw_rotate_z_var;
          tw_skew_x_var;
          tw_skew_y_var;
        ]
    in
    style ~property_rules
      [
        transforms
          [
            Var rotate_x_ref;
            Var rotate_y_ref;
            Var rotate_z_ref;
            Var skew_x_ref;
            Var skew_y_ref;
          ];
      ]

  (* transform-gpu adds translateZ(0) for GPU acceleration plus all var refs *)
  let transform_gpu =
    let rotate_x_ref = Var.reference_with_empty_fallback tw_rotate_x_var in
    let rotate_y_ref = Var.reference_with_empty_fallback tw_rotate_y_var in
    let rotate_z_ref = Var.reference_with_empty_fallback tw_rotate_z_var in
    let skew_x_ref = Var.reference_with_empty_fallback tw_skew_x_var in
    let skew_y_ref = Var.reference_with_empty_fallback tw_skew_y_var in
    let property_rules =
      collect_property_rules
        [
          tw_rotate_x_var;
          tw_rotate_y_var;
          tw_rotate_z_var;
          tw_skew_x_var;
          tw_skew_y_var;
        ]
    in
    style ~property_rules
      [
        transforms
          [
            Translate_z Zero;
            Var rotate_x_ref;
            Var rotate_y_ref;
            Var rotate_z_ref;
            Var skew_x_ref;
            Var skew_y_ref;
          ];
      ]

  (** {1 Parsing Functions} *)

  let ( >|= ) = Parse.( >|= )

  (** {1 Utility Conversion Functions} *)

  let to_style = function
    | Rotate n -> rotate n
    | Rotate_arbitrary a -> rotate_arbitrary a
    | Translate_x n -> translate_x n
    | Translate_x_full -> translate_x_full
    | Translate_x_px -> translate_x_px
    | Translate_x_arbitrary len -> translate_x_arbitrary len
    | Translate_y n -> translate_y n
    | Translate_y_full -> translate_y_full
    | Translate_y_px -> translate_y_px
    | Translate_y_arbitrary len -> translate_y_arbitrary len
    | Translate_full -> translate_full
    | Translate_1_2 -> translate_1_2
    | Translate_arbitrary len -> translate_arbitrary len
    | Neg_translate_arbitrary s -> neg_translate_arbitrary_style s
    | Neg_translate_full -> neg_translate_full
    | Neg_translate_x_arbitrary s -> neg_translate_x_arbitrary_style s
    | Neg_translate_x_full -> neg_translate_x_full
    | Neg_translate_x_1_2 -> neg_translate_x_1_2
    | Neg_translate_y_arbitrary s -> neg_translate_y_arbitrary_style s
    | Neg_translate_y_full -> neg_translate_y_full
    | Neg_translate_y_1_2 -> neg_translate_y_1_2
    | Translate_z n -> translate_z n
    | Translate_z_px -> translate_z_px
    | Neg_translate_z_arbitrary s -> neg_translate_z_arbitrary_style s
    | Neg_translate_z_px -> neg_translate_z_px
    | Translate_3d -> translate_3d
    | Scale n -> scale n
    | Scale_arbitrary f -> scale_arbitrary f
    | Scale_x n -> scale_x n
    | Scale_x_arbitrary f -> scale_x_arbitrary f
    | Scale_y n -> scale_y n
    | Scale_y_arbitrary f -> scale_y_arbitrary f
    | Scale_z n -> scale_z n
    | Scale_3d -> scale_3d
    | Skew_x n -> skew_x n
    | Skew_x_arbitrary a -> skew_x_arbitrary a
    | Skew_y n -> skew_y n
    | Skew_y_arbitrary a -> skew_y_arbitrary a
    | Skew n -> transform_with_both_skew n
    | Rotate_x n -> rotate_x n
    | Rotate_x_arbitrary a -> rotate_x_arbitrary a
    | Rotate_y n -> rotate_y n
    | Rotate_y_arbitrary a -> rotate_y_arbitrary a
    | Rotate_z n -> rotate_z n
    | Rotate_z_arbitrary a -> rotate_z_arbitrary a
    | Perspective_none -> perspective_none
    | Perspective_dramatic -> perspective_dramatic
    | Perspective_normal -> perspective_normal
    | Perspective_arbitrary len -> perspective_arbitrary len
    | Perspective_origin_center -> perspective_origin_center ()
    | Perspective_origin_top -> perspective_origin_top ()
    | Perspective_origin_bottom -> perspective_origin_bottom ()
    | Perspective_origin_left -> perspective_origin_left ()
    | Perspective_origin_right -> perspective_origin_right ()
    | Perspective_origin_top_left -> perspective_origin_top_left ()
    | Perspective_origin_top_right -> perspective_origin_top_right ()
    | Perspective_origin_bottom_left -> perspective_origin_bottom_left ()
    | Perspective_origin_bottom_right -> perspective_origin_bottom_right ()
    | Perspective_origin_arbitrary s -> perspective_origin_arbitrary s
    | Transform_style_3d -> transform_style_3d
    | Transform_style_flat -> transform_style_flat
    | Transform_box_border -> transform_box_border
    | Transform_box_content -> transform_box_content
    | Transform_box_fill -> transform_box_fill
    | Transform_box_stroke -> transform_box_stroke
    | Transform_box_view -> transform_box_view
    | Backface_visible -> backface_visible
    | Backface_hidden -> backface_hidden
    | Transform -> transform
    | Transform_cpu -> transform_cpu
    | Transform_none -> transform_none
    | Transform_gpu -> transform_gpu
    | Origin_center -> origin_center ()
    | Origin_top -> origin_top ()
    | Origin_bottom -> origin_bottom ()
    | Origin_left -> origin_left ()
    | Origin_right -> origin_right ()
    | Origin_top_left -> origin_top_left ()
    | Origin_top_right -> origin_top_right ()
    | Origin_bottom_left -> origin_bottom_left ()
    | Origin_bottom_right -> origin_bottom_right ()
    | Origin_arbitrary s -> origin_arbitrary s

  let suborder = function
    | Transform -> 2000
    | Transform_cpu -> 2001
    | Transform_gpu -> 2002
    | Transform_none -> 2003
    (* Combined translate utilities: negative first, then positive *)
    | Neg_translate_arbitrary _ -> 85
    | Neg_translate_full -> 86
    | Translate_1_2 -> 87
    | Translate_arbitrary _ -> 89
    | Translate_full -> 91
    (* Translate utilities come first *)
    | Neg_translate_x_arbitrary _ -> 100
    | Neg_translate_x_full -> 101
    | Neg_translate_x_1_2 -> 102
    | Translate_x n -> 110 + n
    | Translate_x_full -> 130
    | Translate_x_px -> 131
    | Translate_x_arbitrary _ -> 199
    | Neg_translate_y_arbitrary _ -> 200
    | Neg_translate_y_full -> 201
    | Neg_translate_y_1_2 -> 202
    | Translate_y n -> 210 + n
    | Translate_y_full -> 230
    | Translate_y_px -> 231
    | Translate_y_arbitrary _ -> 299
    | Neg_translate_z_arbitrary _ -> 299
    | Neg_translate_z_px -> 300
    | Translate_z n -> 301 + n
    | Translate_z_px -> 320
    | Translate_3d -> 320
    (* Scale utilities *)
    | Scale n -> 400 + n
    | Scale_arbitrary _ -> 499
    | Scale_x n -> 500 + n
    | Scale_x_arbitrary _ -> 599
    | Scale_y n -> 600 + n
    | Scale_y_arbitrary _ -> 699
    | Scale_z n -> 700 + n
    | Scale_3d -> 750
    (* Rotate utilities *)
    | Rotate n -> 800 + n
    | Rotate_arbitrary _ -> 899
    | Rotate_x n -> 900 + n
    | Rotate_x_arbitrary _ -> 999
    | Rotate_y n -> 1000 + n
    | Rotate_y_arbitrary _ -> 1099
    | Rotate_z n -> 1100 + n
    | Rotate_z_arbitrary _ -> 1199
    (* Skew utilities *)
    | Skew_x n -> 1200 + n
    | Skew_x_arbitrary _ -> 1299
    | Skew_y n -> 1300 + n
    | Skew_y_arbitrary _ -> 1398
    | Skew n -> 1200 + n (* Combined skew, same order as skew-x *)
    (* Other transform utilities - arbitrary before named (alphabetical by
       class) *)
    | Perspective_arbitrary _ -> 1400
    | Perspective_dramatic -> 1400
    | Perspective_none -> 1401
    | Perspective_normal -> 1402
    | Perspective_origin_arbitrary _ -> 1499
    | Perspective_origin_bottom -> 1500
    | Perspective_origin_bottom_left -> 1501
    | Perspective_origin_bottom_right -> 1502
    | Perspective_origin_center -> 1503
    | Perspective_origin_left -> 1504
    | Perspective_origin_right -> 1505
    | Perspective_origin_top -> 1506
    | Perspective_origin_top_left -> 1507
    | Perspective_origin_top_right -> 1508
    (* Alphabetical by class name: backface-hidden, backface-visible,
       transform-3d, transform-border, ..., transform-flat, ...,
       transform-view *)
    | Backface_hidden -> 1600
    | Backface_visible -> 1601
    | Transform_style_3d -> 1602
    | Transform_box_border -> 1603
    | Transform_box_content -> 1604
    | Transform_box_fill -> 1605
    | Transform_style_flat -> 1606
    | Transform_box_stroke -> 1607
    | Transform_box_view -> 1608
    (* Transform origin - alphabetical: bottom, bottom-left, bottom-right,
       center, left, right, top, top-left, top-right *)
    | Origin_bottom -> 1700
    | Origin_bottom_left -> 1701
    | Origin_bottom_right -> 1702
    | Origin_center -> 1703
    | Origin_left -> 1704
    | Origin_right -> 1705
    | Origin_top -> 1706
    | Origin_top_left -> 1707
    | Origin_top_right -> 1708
    | Origin_arbitrary _ -> 1699

  let of_class class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "rotate"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_bracket_angle n with
        | Ok a -> Ok (Rotate_arbitrary a)
        | Error _ -> err_not_utility)
    | [ "rotate"; n ] -> Parse.int_any n >|= fun n -> Rotate n
    | [ "translate"; "x"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_bracket_length n with
        | Ok len -> Ok (Translate_x_arbitrary len)
        | Error _ -> err_not_utility)
    | [ "translate"; "x"; "full" ] -> Ok Translate_x_full
    | [ "translate"; "x"; "px" ] -> Ok Translate_x_px
    | [ "translate"; "x"; n ] -> Parse.int_any n >|= fun n -> Translate_x n
    | [ "translate"; "y"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_bracket_length n with
        | Ok len -> Ok (Translate_y_arbitrary len)
        | Error _ -> err_not_utility)
    | [ "translate"; "y"; "full" ] -> Ok Translate_y_full
    | [ "translate"; "y"; "px" ] -> Ok Translate_y_px
    | [ "translate"; "y"; n ] -> Parse.int_any n >|= fun n -> Translate_y n
    | [ "translate"; "z"; "px" ] -> Ok Translate_z_px
    | [ "translate"; "z"; n ] -> Parse.int_any n >|= fun n -> Translate_z n
    | [ "translate"; "full" ] -> Ok Translate_full
    | [ "translate"; "1/2" ] -> Ok Translate_1_2
    | [ "translate"; "3d" ] -> Ok Translate_3d
    | "translate" :: rest
      when match rest with
           | [] | [ "x"; _ ] | [ "y"; _ ] | [ "z"; _ ] | [ "full" ] | [ "1/2" ]
             ->
               false
           | _ -> true -> (
        let value = String.concat "-" rest in
        match parse_bracket_length value with
        | Ok len -> Ok (Translate_arbitrary len)
        | Error _ -> err_not_utility)
    (* Negative translate utilities: -translate-x-N, -translate-y-N,
       -translate-z-N Split by '-' gives [""; "translate"; axis; n] *)
    | [ ""; "translate"; value ] when Parse.is_bracket_var value ->
        let inner = Parse.bracket_inner value in
        Ok (Neg_translate_arbitrary inner)
    | [ ""; "translate"; "full" ] -> Ok Neg_translate_full
    | [ ""; "translate"; "x"; value ] when Parse.is_bracket_var value ->
        let inner = Parse.bracket_inner value in
        Ok (Neg_translate_x_arbitrary inner)
    | [ ""; "translate"; "x"; "full" ] -> Ok Neg_translate_x_full
    | [ ""; "translate"; "x"; n ] ->
        Parse.int_pos ~name:"translate-x" n >|= fun n -> Translate_x (-n)
    | [ ""; "translate"; "y"; value ] when Parse.is_bracket_var value ->
        let inner = Parse.bracket_inner value in
        Ok (Neg_translate_y_arbitrary inner)
    | [ ""; "translate"; "y"; "full" ] -> Ok Neg_translate_y_full
    | [ ""; "translate"; "y"; n ] ->
        Parse.int_pos ~name:"translate-y" n >|= fun n -> Translate_y (-n)
    | [ ""; "translate"; "z"; value ] when Parse.is_bracket_var value ->
        let inner = Parse.bracket_inner value in
        Ok (Neg_translate_z_arbitrary inner)
    | [ ""; "translate"; "z"; "px" ] -> Ok Neg_translate_z_px
    | [ ""; "translate"; "z"; n ] ->
        Parse.int_pos ~name:"translate-z" n >|= fun n -> Translate_z (-n)
    | [ "scale"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_bracket_number n with
        | Ok f -> Ok (Scale_arbitrary f)
        | Error _ -> err_not_utility)
    | [ "scale"; "3d" ] -> Ok Scale_3d
    | [ "scale"; n ] -> Parse.int_pos ~name:"scale" n >|= fun n -> Scale n
    | [ "scale"; "x"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_bracket_number n with
        | Ok f -> Ok (Scale_x_arbitrary f)
        | Error _ -> err_not_utility)
    | [ "scale"; "x"; n ] ->
        Parse.int_pos ~name:"scale-x" n >|= fun n -> Scale_x n
    | [ "scale"; "y"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_bracket_number n with
        | Ok f -> Ok (Scale_y_arbitrary f)
        | Error _ -> err_not_utility)
    | [ "scale"; "y"; n ] ->
        Parse.int_pos ~name:"scale-y" n >|= fun n -> Scale_y n
    | [ "scale"; "z"; n ] ->
        Parse.int_pos ~name:"scale-z" n >|= fun n -> Scale_z n
    | [ "skew"; "x"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_bracket_angle n with
        | Ok a -> Ok (Skew_x_arbitrary a)
        | Error _ -> err_not_utility)
    | [ "skew"; "x"; n ] -> Parse.int_any n >|= fun n -> Skew_x n
    | [ "skew"; "y"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_bracket_angle n with
        | Ok a -> Ok (Skew_y_arbitrary a)
        | Error _ -> err_not_utility)
    | [ "skew"; "y"; n ] -> Parse.int_any n >|= fun n -> Skew_y n
    | [ "skew"; n ] -> Parse.int_any n >|= fun n -> Skew n
    | [ "rotate"; "x"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_bracket_angle n with
        | Ok a -> Ok (Rotate_x_arbitrary a)
        | Error _ -> err_not_utility)
    | [ "rotate"; "x"; n ] -> Parse.int_any n >|= fun n -> Rotate_x n
    | [ "rotate"; "y"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_bracket_angle n with
        | Ok a -> Ok (Rotate_y_arbitrary a)
        | Error _ -> err_not_utility)
    | [ "rotate"; "y"; n ] -> Parse.int_any n >|= fun n -> Rotate_y n
    | [ "rotate"; "z"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_bracket_angle n with
        | Ok a -> Ok (Rotate_z_arbitrary a)
        | Error _ -> err_not_utility)
    | [ "rotate"; "z"; n ] -> Parse.int_any n >|= fun n -> Rotate_z n
    (* Negative rotate: -rotate-N *)
    | [ ""; "rotate"; n ] ->
        Parse.int_pos ~name:"rotate" n >|= fun n -> Rotate (-n)
    | [ ""; "rotate"; "x"; n ] ->
        Parse.int_pos ~name:"rotate-x" n >|= fun n -> Rotate_x (-n)
    | [ ""; "rotate"; "y"; n ] ->
        Parse.int_pos ~name:"rotate-y" n >|= fun n -> Rotate_y (-n)
    | [ ""; "rotate"; "z"; n ] ->
        Parse.int_pos ~name:"rotate-z" n >|= fun n -> Rotate_z (-n)
    (* Negative scale: -scale-N *)
    | [ ""; "scale"; n ] ->
        Parse.int_pos ~name:"scale" n >|= fun n -> Scale (-n)
    | [ ""; "scale"; "x"; n ] ->
        Parse.int_pos ~name:"scale-x" n >|= fun n -> Scale_x (-n)
    | [ ""; "scale"; "y"; n ] ->
        Parse.int_pos ~name:"scale-y" n >|= fun n -> Scale_y (-n)
    | [ ""; "scale"; "z"; n ] ->
        Parse.int_pos ~name:"scale-z" n >|= fun n -> Scale_z (-n)
    (* Negative skew: -skew-N, -skew-x-N, -skew-y-N *)
    | [ ""; "skew"; "x"; n ] ->
        Parse.int_pos ~name:"skew-x" n >|= fun n -> Skew_x (-n)
    | [ ""; "skew"; "y"; n ] ->
        Parse.int_pos ~name:"skew-y" n >|= fun n -> Skew_y (-n)
    | [ ""; "skew"; n ] -> Parse.int_pos ~name:"skew" n >|= fun n -> Skew (-n)
    | [ "perspective"; "none" ] -> Ok Perspective_none
    | [ "perspective"; "dramatic" ] -> Ok Perspective_dramatic
    | [ "perspective"; "normal" ] -> Ok Perspective_normal
    | "perspective" :: rest
      when match rest with "origin" :: _ | [] -> false | _ -> true -> (
        let value = String.concat "-" rest in
        match parse_bracket_length value with
        | Ok len -> Ok (Perspective_arbitrary len)
        | Error _ -> err_not_utility)
    | [ "perspective"; "origin"; "center" ] -> Ok Perspective_origin_center
    | [ "perspective"; "origin"; "top" ] -> Ok Perspective_origin_top
    | [ "perspective"; "origin"; "bottom" ] -> Ok Perspective_origin_bottom
    | [ "perspective"; "origin"; "left" ] -> Ok Perspective_origin_left
    | [ "perspective"; "origin"; "right" ] -> Ok Perspective_origin_right
    | [ "perspective"; "origin"; "top"; "left" ] ->
        Ok Perspective_origin_top_left
    | [ "perspective"; "origin"; "top"; "right" ] ->
        Ok Perspective_origin_top_right
    | [ "perspective"; "origin"; "bottom"; "left" ] ->
        Ok Perspective_origin_bottom_left
    | [ "perspective"; "origin"; "bottom"; "right" ] ->
        Ok Perspective_origin_bottom_right
    | "perspective" :: "origin" :: rest when List.length rest > 0 ->
        let value = String.concat "-" rest in
        let len = String.length value in
        if len > 2 && value.[0] = '[' && value.[len - 1] = ']' then
          let inner = String.sub value 1 (len - 2) in
          (* Convert underscores to spaces *)
          let inner = String.map (fun c -> if c = '_' then ' ' else c) inner in
          Ok (Perspective_origin_arbitrary inner)
        else err_not_utility
    | [ "transform"; "style"; "3d" ] -> Ok Transform_style_3d
    | [ "transform"; "style"; "flat" ] -> Ok Transform_style_flat
    | [ "transform"; "3d" ] -> Ok Transform_style_3d
    | [ "transform"; "flat" ] -> Ok Transform_style_flat
    | [ "backface"; "hidden" ] -> Ok Backface_hidden
    | [ "backface"; "visible" ] -> Ok Backface_visible
    | [ "transform"; "border" ] -> Ok Transform_box_border
    | [ "transform"; "content" ] -> Ok Transform_box_content
    | [ "transform"; "fill" ] -> Ok Transform_box_fill
    | [ "transform"; "stroke" ] -> Ok Transform_box_stroke
    | [ "transform"; "view" ] -> Ok Transform_box_view
    | [ "transform" ] -> Ok Transform
    | [ "transform"; "cpu" ] -> Ok Transform_cpu
    | [ "transform"; "none" ] -> Ok Transform_none
    | [ "transform"; "gpu" ] -> Ok Transform_gpu
    | [ "origin"; "center" ] -> Ok Origin_center
    | [ "origin"; "top" ] -> Ok Origin_top
    | [ "origin"; "bottom" ] -> Ok Origin_bottom
    | [ "origin"; "left" ] -> Ok Origin_left
    | [ "origin"; "right" ] -> Ok Origin_right
    | [ "origin"; "top"; "left" ] -> Ok Origin_top_left
    | [ "origin"; "top"; "right" ] -> Ok Origin_top_right
    | [ "origin"; "bottom"; "left" ] -> Ok Origin_bottom_left
    | [ "origin"; "bottom"; "right" ] -> Ok Origin_bottom_right
    | "origin" :: rest when List.length rest > 0 ->
        let value = String.concat "-" rest in
        let len = String.length value in
        if len > 2 && value.[0] = '[' && value.[len - 1] = ']' then
          let inner = String.sub value 1 (len - 2) in
          (* Convert underscores to spaces *)
          let inner = String.map (fun c -> if c = '_' then ' ' else c) inner in
          Ok (Origin_arbitrary inner)
        else err_not_utility
    | _ -> err_not_utility

  let pp_angle_bracket a = "[" ^ Css.Pp.to_string Css.pp_angle a ^ "]"

  let pp_length_bracket len =
    "[" ^ Css.Pp.to_string (pp_length ~always:true) len ^ "]"

  let pp_number_bracket f =
    let s = string_of_float f in
    let s = if String.ends_with ~suffix:"." s then s ^ "0" else s in
    "[" ^ s ^ "]"

  let to_class = function
    | Rotate n -> neg_class "rotate-" n
    | Rotate_arbitrary a -> "rotate-" ^ pp_angle_bracket a
    | Translate_x n -> neg_class "translate-x-" n
    | Translate_x_full -> "translate-x-full"
    | Translate_x_px -> "translate-x-px"
    | Translate_x_arbitrary len -> "translate-x-" ^ pp_length_bracket len
    | Translate_y n -> neg_class "translate-y-" n
    | Translate_y_full -> "translate-y-full"
    | Translate_y_px -> "translate-y-px"
    | Translate_y_arbitrary len -> "translate-y-" ^ pp_length_bracket len
    | Translate_z n -> neg_class "translate-z-" n
    | Translate_z_px -> "translate-z-px"
    | Neg_translate_z_arbitrary s -> "-translate-z-[" ^ s ^ "]"
    | Neg_translate_z_px -> "-translate-z-px"
    | Translate_3d -> "translate-3d"
    | Translate_full -> "translate-full"
    | Translate_1_2 -> "translate-1/2"
    | Translate_arbitrary len -> "translate-" ^ pp_length_bracket len
    | Neg_translate_arbitrary s -> "-translate-[" ^ s ^ "]"
    | Neg_translate_x_arbitrary s -> "-translate-x-[" ^ s ^ "]"
    | Neg_translate_y_arbitrary s -> "-translate-y-[" ^ s ^ "]"
    | Neg_translate_full -> "-translate-full"
    | Neg_translate_x_full -> "-translate-x-full"
    | Neg_translate_x_1_2 -> "-translate-x-1/2"
    | Neg_translate_y_full -> "-translate-y-full"
    | Neg_translate_y_1_2 -> "-translate-y-1/2"
    | Scale n -> neg_class "scale-" n
    | Scale_arbitrary f -> "scale-" ^ pp_number_bracket f
    | Scale_x n -> neg_class "scale-x-" n
    | Scale_x_arbitrary f -> "scale-x-" ^ pp_number_bracket f
    | Scale_y n -> neg_class "scale-y-" n
    | Scale_y_arbitrary f -> "scale-y-" ^ pp_number_bracket f
    | Scale_z n -> neg_class "scale-z-" n
    | Scale_3d -> "scale-3d"
    | Skew_x n -> neg_class "skew-x-" n
    | Skew_x_arbitrary a -> "skew-x-" ^ pp_angle_bracket a
    | Skew_y n -> neg_class "skew-y-" n
    | Skew_y_arbitrary a -> "skew-y-" ^ pp_angle_bracket a
    | Skew n -> neg_class "skew-" n
    | Rotate_x n -> neg_class "rotate-x-" n
    | Rotate_x_arbitrary a -> "rotate-x-" ^ pp_angle_bracket a
    | Rotate_y n -> neg_class "rotate-y-" n
    | Rotate_y_arbitrary a -> "rotate-y-" ^ pp_angle_bracket a
    | Rotate_z n -> neg_class "rotate-z-" n
    | Rotate_z_arbitrary a -> "rotate-z-" ^ pp_angle_bracket a
    | Perspective_none -> "perspective-none"
    | Perspective_dramatic -> "perspective-dramatic"
    | Perspective_normal -> "perspective-normal"
    | Perspective_arbitrary len ->
        "perspective-[" ^ Css.Pp.to_string (pp_length ~always:true) len ^ "]"
    | Perspective_origin_center -> "perspective-origin-center"
    | Perspective_origin_top -> "perspective-origin-top"
    | Perspective_origin_bottom -> "perspective-origin-bottom"
    | Perspective_origin_left -> "perspective-origin-left"
    | Perspective_origin_right -> "perspective-origin-right"
    | Perspective_origin_top_left -> "perspective-origin-top-left"
    | Perspective_origin_top_right -> "perspective-origin-top-right"
    | Perspective_origin_bottom_left -> "perspective-origin-bottom-left"
    | Perspective_origin_bottom_right -> "perspective-origin-bottom-right"
    | Perspective_origin_arbitrary s ->
        let s = String.map (fun c -> if c = ' ' then '_' else c) s in
        "perspective-origin-[" ^ s ^ "]"
    | Transform_style_3d -> "transform-3d"
    | Transform_style_flat -> "transform-flat"
    | Transform_box_border -> "transform-border"
    | Transform_box_content -> "transform-content"
    | Transform_box_fill -> "transform-fill"
    | Transform_box_stroke -> "transform-stroke"
    | Transform_box_view -> "transform-view"
    | Backface_visible -> "backface-visible"
    | Backface_hidden -> "backface-hidden"
    | Transform -> "transform"
    | Transform_cpu -> "transform-cpu"
    | Transform_none -> "transform-none"
    | Transform_gpu -> "transform-gpu"
    | Origin_center -> "origin-center"
    | Origin_top -> "origin-top"
    | Origin_bottom -> "origin-bottom"
    | Origin_left -> "origin-left"
    | Origin_right -> "origin-right"
    | Origin_top_left -> "origin-top-left"
    | Origin_top_right -> "origin-top-right"
    | Origin_bottom_left -> "origin-bottom-left"
    | Origin_bottom_right -> "origin-bottom-right"
    | Origin_arbitrary s ->
        (* Convert spaces back to underscores for class name *)
        let s = String.map (fun c -> if c = ' ' then '_' else c) s in
        "origin-[" ^ s ^ "]"
end

open Handler

(** Register the transform utility handlers *)
let () = Utility.register (module Handler)

(** Public API returning Utility.t *)
let utility x = Utility.base (Self x)

let rotate n = utility (Rotate n)
let translate_x n = utility (Translate_x n)
let translate_y n = utility (Translate_y n)
let scale n = utility (Scale n)
let scale_x n = utility (Scale_x n)
let scale_y n = utility (Scale_y n)
let skew_x n = utility (Skew_x n)
let skew_y n = utility (Skew_y n)
let translate_z n = utility (Translate_z n)
let rotate_x n = utility (Rotate_x n)
let rotate_y n = utility (Rotate_y n)
let rotate_z n = utility (Rotate_z n)
let scale_z n = utility (Scale_z n)
let perspective_none = utility Perspective_none
let perspective_dramatic = utility Perspective_dramatic
let perspective_normal = utility Perspective_normal
let neg_translate_x_1_2 = utility Neg_translate_x_1_2
let neg_translate_y_1_2 = utility Neg_translate_y_1_2
let perspective_origin_center = utility Perspective_origin_center
let perspective_origin_top = utility Perspective_origin_top
let perspective_origin_bottom = utility Perspective_origin_bottom
let perspective_origin_left = utility Perspective_origin_left
let perspective_origin_right = utility Perspective_origin_right
let transform_style_3d = utility Transform_style_3d
let transform_style_flat = utility Transform_style_flat
let transform_box_border = utility Transform_box_border
let transform_box_content = utility Transform_box_content
let transform_box_fill = utility Transform_box_fill
let transform_box_stroke = utility Transform_box_stroke
let transform_box_view = utility Transform_box_view
let backface_visible = utility Backface_visible
let backface_hidden = utility Backface_hidden
let transform = utility Transform
let transform_none = utility Transform_none
let transform_gpu = utility Transform_gpu
let origin_center = utility Origin_center
let origin_top = utility Origin_top
let origin_bottom = utility Origin_bottom
let origin_left = utility Origin_left
let origin_right = utility Origin_right
let origin_top_left = utility Origin_top_left
let origin_top_right = utility Origin_top_right
let origin_bottom_left = utility Origin_bottom_left
let origin_bottom_right = utility Origin_bottom_right
