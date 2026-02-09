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
    | Translate_x_arbitrary of Css.length
    | Translate_y of int
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
    | (* Combined translate utilities *)
      Translate_full
    | Translate_1_2
    | (* Negative translate utilities for centering *)
      Neg_translate_x_1_2
    | Neg_translate_y_1_2
    | (* 3D Transforms *)
      Translate_z of int
    | Rotate_x of int
    | Rotate_y of int
    | Rotate_z of int
    | Scale_z of int
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
    | (* Transform style *)
      Transform_style_3d
    | Transform_style_flat
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

  (* Scale variables - use first-usage order with low property_order (5-7) for
     tie-breaking *)
  let tw_scale_x_var =
    Var.property_default Css.Number_percentage ~initial:(Num 1.0)
      ~universal:true ~property_order:5 ~family:`Scale "tw-scale-x"

  let tw_scale_y_var =
    Var.property_default Css.Number_percentage ~initial:(Num 1.0)
      ~universal:true ~property_order:6 ~family:`Scale "tw-scale-y"

  let tw_scale_z_var =
    Var.property_default Css.Number_percentage ~initial:(Num 1.0)
      ~universal:true ~property_order:7 ~family:`Scale "tw-scale-z"

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

  let rotate n = style [ Css.rotate (Deg (float_of_int n)) ]
  let rotate_arbitrary angle = style [ Css.rotate angle ]

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
    let props =
      collect_property_rules
        [ tw_translate_x_var; tw_translate_y_var; tw_translate_z_var ]
    in
    style ~property_rules:props
      (spacing_decl :: axis_decl
      :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  let translate_x n = translate_axis tw_translate_x_var n
  let translate_y n = translate_axis tw_translate_y_var n

  let translate_x_arbitrary len =
    let axis_decl, _ = Var.binding tw_translate_x_var len in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    let props =
      collect_property_rules
        [ tw_translate_x_var; tw_translate_y_var; tw_translate_z_var ]
    in
    style ~property_rules:props
      (axis_decl :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  let translate_y_arbitrary len =
    let axis_decl, _ = Var.binding tw_translate_y_var len in
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    let props =
      collect_property_rules
        [ tw_translate_x_var; tw_translate_y_var; tw_translate_z_var ]
    in
    style ~property_rules:props
      (axis_decl :: [ Css.translate (XY (Var tx_ref, Var ty_ref)) ])

  let scale n =
    let value : Css.number_percentage = Css.Pct (float_of_int n) in
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
    let value : Css.number_percentage = Css.Pct (float_of_int n) in
    (* Only uses X variable *)
    let d, _ = Var.binding tw_scale_x_var value in
    style (d :: [ Css.transform (Scale_x (float_of_int n /. 100.0)) ])

  let scale_y n =
    let value : Css.number_percentage = Css.Pct (float_of_int n) in
    (* Only uses Y variable *)
    let d, _ = Var.binding tw_scale_y_var value in
    style (d :: [ Css.transform (Scale_y (float_of_int n /. 100.0)) ])

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

  let skew_axis var mk_transform deg =
    let transform_val = mk_transform (Css.Deg (float_of_int deg)) in
    let d, _ = Var.binding var transform_val in
    style [ d; Css.transform transform_val ]

  let skew_x deg = skew_axis tw_skew_x_var (fun a -> Css.Skew_x a) deg
  let skew_y deg = skew_axis tw_skew_y_var (fun a -> Css.Skew_y a) deg

  let skew_x_arbitrary angle =
    let transform_val = Css.Skew_x angle in
    let d, _ = Var.binding tw_skew_x_var transform_val in
    style [ d; Css.transform transform_val ]

  let skew_y_arbitrary angle =
    let transform_val = Css.Skew_y angle in
    let d, _ = Var.binding tw_skew_y_var transform_val in
    style [ d; Css.transform transform_val ]

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

  (* Negative translate utilities for centering *)
  let neg_translate_x_1_2 =
    style [ Css.transform (Css.Translate_x (Css.Pct (-50.0))) ]

  let neg_translate_y_1_2 =
    style [ Css.transform (Css.Translate_y (Css.Pct (-50.0))) ]

  (** {1 3D Transform Utilities} *)

  let rotate_x n = style [ Css.transform (Rotate_x (Deg (float_of_int n))) ]
  let rotate_y n = style [ Css.transform (Rotate_y (Deg (float_of_int n))) ]
  let rotate_z n = style [ Css.transform (Rotate_z (Deg (float_of_int n))) ]

  let translate_z n =
    style [ Css.transform (Translate_z (Px (float_of_int n))) ]

  let scale_z n =
    let value : Css.number_percentage = Css.Pct (float_of_int n) in
    let d, _ = Var.binding tw_scale_z_var value in
    style (d :: [ Css.transform (Scale_z (float_of_int n /. 100.0)) ])

  let perspective_none = style [ Css.perspective None ]

  let perspective_dramatic =
    let decl, r = Var.binding perspective_dramatic_var (Px 100.0) in
    style (decl :: [ Css.perspective (Var r) ])

  let perspective_normal =
    let decl, r = Var.binding perspective_normal_var (Px 500.0) in
    style (decl :: [ Css.perspective (Var r) ])

  let perspective_arbitrary len = style [ Css.perspective len ]

  let perspective_origin_center =
    style [ perspective_origin Css.Perspective_center ]

  let perspective_origin_top = style [ perspective_origin Css.Perspective_top ]

  let perspective_origin_bottom =
    style [ perspective_origin Css.Perspective_bottom ]

  let perspective_origin_left =
    style [ perspective_origin Css.Perspective_left ]

  let perspective_origin_right =
    style [ perspective_origin Css.Perspective_right ]

  let transform_style_3d = style [ transform_style Preserve_3d ]
  let transform_style_flat = style [ transform_style Flat ]
  let backface_visible = style [ backface_visibility Visible ]
  let backface_hidden = style [ backface_visibility Hidden ]

  (** {1 Transform Origin Utilities} *)

  let origin_center = style [ transform_origin Center ]
  let origin_top = style [ transform_origin Top ]
  let origin_bottom = style [ transform_origin Bottom ]
  let origin_left = style [ transform_origin (X (Pct 0.0)) ]
  let origin_right = style [ transform_origin (X (Pct 100.0)) ]
  let origin_top_left = style [ transform_origin (XY (Pct 0.0, Pct 0.0)) ]
  let origin_top_right = style [ transform_origin (XY (Pct 100.0, Pct 0.0)) ]
  let origin_bottom_left = style [ transform_origin (XY (Pct 0.0, Pct 100.0)) ]

  let origin_bottom_right =
    style [ transform_origin (XY (Pct 100.0, Pct 100.0)) ]

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
    | Translate_x_arbitrary len -> translate_x_arbitrary len
    | Translate_y n -> translate_y n
    | Translate_y_arbitrary len -> translate_y_arbitrary len
    | Translate_full -> translate_full
    | Translate_1_2 -> translate_1_2
    | Neg_translate_x_1_2 -> neg_translate_x_1_2
    | Neg_translate_y_1_2 -> neg_translate_y_1_2
    | Translate_z n -> translate_z n
    | Scale n -> scale n
    | Scale_arbitrary f -> scale_arbitrary f
    | Scale_x n -> scale_x n
    | Scale_x_arbitrary f -> scale_x_arbitrary f
    | Scale_y n -> scale_y n
    | Scale_y_arbitrary f -> scale_y_arbitrary f
    | Scale_z n -> scale_z n
    | Skew_x n -> skew_x n
    | Skew_x_arbitrary a -> skew_x_arbitrary a
    | Skew_y n -> skew_y n
    | Skew_y_arbitrary a -> skew_y_arbitrary a
    | Rotate_x n -> rotate_x n
    | Rotate_y n -> rotate_y n
    | Rotate_z n -> rotate_z n
    | Perspective_none -> perspective_none
    | Perspective_dramatic -> perspective_dramatic
    | Perspective_normal -> perspective_normal
    | Perspective_arbitrary len -> perspective_arbitrary len
    | Perspective_origin_center -> perspective_origin_center
    | Perspective_origin_top -> perspective_origin_top
    | Perspective_origin_bottom -> perspective_origin_bottom
    | Perspective_origin_left -> perspective_origin_left
    | Perspective_origin_right -> perspective_origin_right
    | Transform_style_3d -> transform_style_3d
    | Transform_style_flat -> transform_style_flat
    | Backface_visible -> backface_visible
    | Backface_hidden -> backface_hidden
    | Transform -> transform
    | Transform_cpu -> transform_cpu
    | Transform_none -> transform_none
    | Transform_gpu -> transform_gpu
    | Origin_center -> origin_center
    | Origin_top -> origin_top
    | Origin_bottom -> origin_bottom
    | Origin_left -> origin_left
    | Origin_right -> origin_right
    | Origin_top_left -> origin_top_left
    | Origin_top_right -> origin_top_right
    | Origin_bottom_left -> origin_bottom_left
    | Origin_bottom_right -> origin_bottom_right

  let suborder = function
    | Transform -> 2000
    | Transform_cpu -> 2001
    | Transform_gpu -> 2002
    | Transform_none -> 2003
    (* Combined translate utilities - alphabetical: 1/2 before full *)
    | Translate_1_2 -> 90
    | Translate_full -> 91
    (* Translate utilities come first *)
    | Translate_x n -> 100 + n
    | Translate_x_arbitrary _ -> 199
    | Neg_translate_x_1_2 -> 150
    | Translate_y n -> 200 + n
    | Translate_y_arbitrary _ -> 299
    | Neg_translate_y_1_2 -> 250
    | Translate_z n -> 300 + n
    (* Scale utilities *)
    | Scale n -> 400 + n
    | Scale_arbitrary _ -> 499
    | Scale_x n -> 500 + n
    | Scale_x_arbitrary _ -> 599
    | Scale_y n -> 600 + n
    | Scale_y_arbitrary _ -> 699
    | Scale_z n -> 700 + n
    (* Rotate utilities *)
    | Rotate n -> 800 + n
    | Rotate_arbitrary _ -> 899
    | Rotate_x n -> 900 + n
    | Rotate_y n -> 1000 + n
    | Rotate_z n -> 1100 + n
    (* Skew utilities *)
    | Skew_x n -> 1200 + n
    | Skew_x_arbitrary _ -> 1299
    | Skew_y n -> 1300 + n
    | Skew_y_arbitrary _ -> 1398
    (* Other transform utilities - arbitrary before named (alphabetical by
       class) *)
    | Perspective_arbitrary _ -> 1400
    | Perspective_dramatic -> 1400
    | Perspective_none -> 1401
    | Perspective_normal -> 1402
    | Perspective_origin_center -> 1500
    | Perspective_origin_top -> 1501
    | Perspective_origin_bottom -> 1502
    | Perspective_origin_left -> 1503
    | Perspective_origin_right -> 1504
    | Transform_style_3d -> 1600
    | Transform_style_flat -> 1601
    | Backface_visible -> 1602
    | Backface_hidden -> 1603
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

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
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
    | [ "translate"; "x"; n ] -> Parse.int_any n >|= fun n -> Translate_x n
    | [ "translate"; "y"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_bracket_length n with
        | Ok len -> Ok (Translate_y_arbitrary len)
        | Error _ -> err_not_utility)
    | [ "translate"; "y"; n ] -> Parse.int_any n >|= fun n -> Translate_y n
    | [ "translate"; "z"; n ] -> Parse.int_any n >|= fun n -> Translate_z n
    | [ "translate"; "full" ] -> Ok Translate_full
    | [ "translate"; "1/2" ] -> Ok Translate_1_2
    (* Negative translate utilities: -translate-x-N, -translate-y-N,
       -translate-z-N Split by '-' gives [""; "translate"; axis; n] *)
    | [ ""; "translate"; "x"; n ] ->
        Parse.int_pos ~name:"translate-x" n >|= fun n -> Translate_x (-n)
    | [ ""; "translate"; "y"; n ] ->
        Parse.int_pos ~name:"translate-y" n >|= fun n -> Translate_y (-n)
    | [ ""; "translate"; "z"; n ] ->
        Parse.int_pos ~name:"translate-z" n >|= fun n -> Translate_z (-n)
    | [ "scale"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_bracket_number n with
        | Ok f -> Ok (Scale_arbitrary f)
        | Error _ -> err_not_utility)
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
    | [ "rotate"; "x"; n ] -> Parse.int_any n >|= fun n -> Rotate_x n
    | [ "rotate"; "y"; n ] -> Parse.int_any n >|= fun n -> Rotate_y n
    | [ "rotate"; "z"; n ] -> Parse.int_any n >|= fun n -> Rotate_z n
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
    | [ "transform"; "style"; "3d" ] -> Ok Transform_style_3d
    | [ "transform"; "style"; "flat" ] -> Ok Transform_style_flat
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
    | _ -> err_not_utility

  let pp_angle_bracket a = "[" ^ Css.Pp.to_string Css.pp_angle a ^ "]"

  let pp_length_bracket len =
    "[" ^ Css.Pp.to_string (pp_length ~always:true) len ^ "]"

  let pp_number_bracket f =
    let s = string_of_float f in
    let s = if String.ends_with ~suffix:"." s then s ^ "0" else s in
    "[" ^ s ^ "]"

  let to_class = function
    | Rotate n -> "rotate-" ^ string_of_int n
    | Rotate_arbitrary a -> "rotate-" ^ pp_angle_bracket a
    | Translate_x n -> neg_class "translate-x-" n
    | Translate_x_arbitrary len -> "translate-x-" ^ pp_length_bracket len
    | Translate_y n -> neg_class "translate-y-" n
    | Translate_y_arbitrary len -> "translate-y-" ^ pp_length_bracket len
    | Translate_z n -> neg_class "translate-z-" n
    | Translate_full -> "translate-full"
    | Translate_1_2 -> "translate-1/2"
    | Neg_translate_x_1_2 -> "-translate-x-1/2"
    | Neg_translate_y_1_2 -> "-translate-y-1/2"
    | Scale n -> "scale-" ^ string_of_int n
    | Scale_arbitrary f -> "scale-" ^ pp_number_bracket f
    | Scale_x n -> "scale-x-" ^ string_of_int n
    | Scale_x_arbitrary f -> "scale-x-" ^ pp_number_bracket f
    | Scale_y n -> "scale-y-" ^ string_of_int n
    | Scale_y_arbitrary f -> "scale-y-" ^ pp_number_bracket f
    | Scale_z n -> "scale-z-" ^ string_of_int n
    | Skew_x n -> neg_class "skew-x-" n
    | Skew_x_arbitrary a -> "skew-x-" ^ pp_angle_bracket a
    | Skew_y n -> neg_class "skew-y-" n
    | Skew_y_arbitrary a -> "skew-y-" ^ pp_angle_bracket a
    | Rotate_x n -> neg_class "rotate-x-" n
    | Rotate_y n -> neg_class "rotate-y-" n
    | Rotate_z n -> neg_class "rotate-z-" n
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
    | Transform_style_3d -> "transform-style-3d"
    | Transform_style_flat -> "transform-style-flat"
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
