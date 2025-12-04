(** Transform utilities for 2D and 3D transformations. *)

(** Error helpers *)
let err_not_utility = Error (`Msg "Not a transform utility")

module Handler = struct
  open Style
  open Css

  type t =
    | (* 2D Transforms *)
      Rotate of int
    | Translate_x of int
    | Translate_y of int
    | Scale of int
    | Scale_x of int
    | Scale_y of int
    | Skew_x of int
    | Skew_y of int
    | (* Negative translate utilities for centering *)
      Neg_translate_x_1_2
    | Neg_translate_y_1_2
    | (* 3D Transforms *)
      Translate_z of int
    | Rotate_x of int
    | Rotate_y of int
    | Rotate_z of int
    | Scale_z of int
    | Perspective of int
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
    | Transform_none
    | Transform_gpu

  type Utility.base += Self of t

  (** Priority for transform utilities *)
  let name = "transforms"

  (* Match Tailwind ordering: transforms before animations and cursor *)
  let priority = 9

  (* Tailwind v4 uses rotate-x/y/z and skew-x/y variables for the transform
     utility. These variables contain the full transform function values, e.g.:
     --tw-rotate-x: rotateX(45deg) --tw-skew-x: skewX(10deg) *)
  let tw_rotate_x_var =
    Var.channel ~needs_property:true Css.Transform "tw-rotate-x"

  let tw_rotate_y_var =
    Var.channel ~needs_property:true Css.Transform "tw-rotate-y"

  let tw_rotate_z_var =
    Var.channel ~needs_property:true Css.Transform "tw-rotate-z"

  let tw_skew_x_var = Var.channel ~needs_property:true Css.Transform "tw-skew-x"
  let tw_skew_y_var = Var.channel ~needs_property:true Css.Transform "tw-skew-y"

  (* Scale variables need @property defaults for composition *)
  let tw_scale_x_var =
    Var.property_default Css.Number_percentage ~initial:(Num 1.0)
      ~universal:true "tw-scale-x"

  let tw_scale_y_var =
    Var.property_default Css.Number_percentage ~initial:(Num 1.0)
      ~universal:true "tw-scale-y"

  let tw_scale_z_var =
    Var.property_default Css.Number_percentage ~initial:(Num 1.0)
      ~universal:true "tw-scale-z"

  (** {1 2D Transform Utilities} *)

  let rotate n = style [ Css.rotate (Deg (float_of_int n)) ]

  let translate_x n =
    let len : length = if n = 0 then Zero else Rem (float_of_int n *. 0.25) in
    style [ Css.transform (Translate_x len) ]

  let translate_y n =
    let len : length = if n = 0 then Zero else Rem (float_of_int n *. 0.25) in
    style [ Css.transform (Translate_y len) ]

  let scale n =
    let value : Css.number_percentage = Css.Pct (float_of_int n) in
    let dx, _ = Var.binding tw_scale_x_var value in
    let dy, _ = Var.binding tw_scale_y_var value in
    let dz, _ = Var.binding tw_scale_z_var value in
    let props =
      [
        Var.property_rule tw_scale_x_var;
        Var.property_rule tw_scale_y_var;
        Var.property_rule tw_scale_z_var;
      ]
      |> List.filter_map (fun x -> x)
      |> concat
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

  let skew_x deg =
    (* Store the full transform value: Skew_x angle *)
    let transform_val : Css.transform = Skew_x (Deg (float_of_int deg)) in
    let d, _ = Var.binding tw_skew_x_var transform_val in
    style [ d; Css.transform transform_val ]

  let skew_y deg =
    (* Store the full transform value: Skew_y angle *)
    let transform_val : Css.transform = Skew_y (Deg (float_of_int deg)) in
    let d, _ = Var.binding tw_skew_y_var transform_val in
    style [ d; Css.transform transform_val ]

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

  let perspective n =
    let value : length = if n = 0 then Zero else Px (float_of_int n) in
    style [ Css.perspective value ]

  let perspective_origin_center = style [ perspective_origin "center" ]
  let perspective_origin_top = style [ perspective_origin "top" ]
  let perspective_origin_bottom = style [ perspective_origin "bottom" ]
  let perspective_origin_left = style [ perspective_origin "left" ]
  let perspective_origin_right = style [ perspective_origin "right" ]
  let transform_style_3d = style [ transform_style Preserve_3d ]
  let transform_style_flat = style [ transform_style Flat ]
  let backface_visible = style [ backface_visibility Visible ]
  let backface_hidden = style [ backface_visibility Hidden ]

  (** {1 Transform Control Utilities} *)

  (* Tailwind v4 transform utility uses individual rotate-x/y/z and skew-x/y
     variables with empty fallbacks. The output is: transform:
     var(--tw-rotate-x,) var(--tw-rotate-y,) var(--tw-rotate-z,)
     var(--tw-skew-x,) var(--tw-skew-y,); When a variable is set (e.g.,
     --tw-rotate-x: rotateX(45deg)), it becomes part of the transform. *)
  let transform =
    (* Create var references with Empty fallbacks *)
    let rotate_x_ref : Css.transform Css.var =
      Css.var_ref ~fallback:Empty "tw-rotate-x"
    in
    let rotate_y_ref : Css.transform Css.var =
      Css.var_ref ~fallback:Empty "tw-rotate-y"
    in
    let rotate_z_ref : Css.transform Css.var =
      Css.var_ref ~fallback:Empty "tw-rotate-z"
    in
    let skew_x_ref : Css.transform Css.var =
      Css.var_ref ~fallback:Empty "tw-skew-x"
    in
    let skew_y_ref : Css.transform Css.var =
      Css.var_ref ~fallback:Empty "tw-skew-y"
    in
    (* Collect @property rules for these variables *)
    let property_rules =
      [
        Var.property_rule tw_rotate_x_var;
        Var.property_rule tw_rotate_y_var;
        Var.property_rule tw_rotate_z_var;
        Var.property_rule tw_skew_x_var;
        Var.property_rule tw_skew_y_var;
      ]
      |> List.filter_map (fun x -> x)
      |> Css.concat
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
  let transform_gpu = style [ Css.transform (Translate_z Zero) ]

  (** {1 Parsing Functions} *)

  let ( >|= ) = Parse.( >|= )

  (** {1 Utility Conversion Functions} *)

  let to_style = function
    | Rotate n -> rotate n
    | Translate_x n -> translate_x n
    | Translate_y n -> translate_y n
    | Neg_translate_x_1_2 -> neg_translate_x_1_2
    | Neg_translate_y_1_2 -> neg_translate_y_1_2
    | Translate_z n -> translate_z n
    | Scale n -> scale n
    | Scale_x n -> scale_x n
    | Scale_y n -> scale_y n
    | Scale_z n -> scale_z n
    | Skew_x n -> skew_x n
    | Skew_y n -> skew_y n
    | Rotate_x n -> rotate_x n
    | Rotate_y n -> rotate_y n
    | Rotate_z n -> rotate_z n
    | Perspective n -> perspective n
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
    | Transform_none -> transform_none
    | Transform_gpu -> transform_gpu

  let suborder = function
    | Transform -> 0
    | Transform_none -> 1
    | Transform_gpu -> 2
    (* Translate utilities come first *)
    | Translate_x n -> 100 + n
    | Neg_translate_x_1_2 -> 150
    | Translate_y n -> 200 + n
    | Neg_translate_y_1_2 -> 250
    | Translate_z n -> 300 + n
    (* Scale utilities *)
    | Scale n -> 400 + n
    | Scale_x n -> 500 + n
    | Scale_y n -> 600 + n
    | Scale_z n -> 700 + n
    (* Rotate utilities *)
    | Rotate n -> 800 + n
    | Rotate_x n -> 900 + n
    | Rotate_y n -> 1000 + n
    | Rotate_z n -> 1100 + n
    (* Skew utilities *)
    | Skew_x n -> 1200 + n
    | Skew_y n -> 1300 + n
    (* Other transform utilities *)
    | Perspective n -> 1400 + n
    | Perspective_origin_center -> 1500
    | Perspective_origin_top -> 1501
    | Perspective_origin_bottom -> 1502
    | Perspective_origin_left -> 1503
    | Perspective_origin_right -> 1504
    | Transform_style_3d -> 1600
    | Transform_style_flat -> 1601
    | Backface_visible -> 1602
    | Backface_hidden -> 1603

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "rotate"; n ] -> Parse.int_any n >|= fun n -> Rotate n
    | [ "translate"; "x"; n ] -> Parse.int_any n >|= fun n -> Translate_x n
    | [ "translate"; "y"; n ] -> Parse.int_any n >|= fun n -> Translate_y n
    | [ "translate"; "z"; n ] -> Parse.int_any n >|= fun n -> Translate_z n
    | [ "scale"; n ] -> Parse.int_pos ~name:"scale" n >|= fun n -> Scale n
    | [ "scale"; "x"; n ] ->
        Parse.int_pos ~name:"scale-x" n >|= fun n -> Scale_x n
    | [ "scale"; "y"; n ] ->
        Parse.int_pos ~name:"scale-y" n >|= fun n -> Scale_y n
    | [ "scale"; "z"; n ] ->
        Parse.int_pos ~name:"scale-z" n >|= fun n -> Scale_z n
    | [ "skew"; "x"; n ] -> Parse.int_any n >|= fun n -> Skew_x n
    | [ "skew"; "y"; n ] -> Parse.int_any n >|= fun n -> Skew_y n
    | [ "rotate"; "x"; n ] -> Parse.int_any n >|= fun n -> Rotate_x n
    | [ "rotate"; "y"; n ] -> Parse.int_any n >|= fun n -> Rotate_y n
    | [ "rotate"; "z"; n ] -> Parse.int_any n >|= fun n -> Rotate_z n
    | [ "perspective"; n ] ->
        Parse.int_pos ~name:"perspective" n >|= fun n -> Perspective n
    | [ "perspective"; "origin"; "center" ] -> Ok Perspective_origin_center
    | [ "perspective"; "origin"; "top" ] -> Ok Perspective_origin_top
    | [ "perspective"; "origin"; "bottom" ] -> Ok Perspective_origin_bottom
    | [ "perspective"; "origin"; "left" ] -> Ok Perspective_origin_left
    | [ "perspective"; "origin"; "right" ] -> Ok Perspective_origin_right
    | [ "transform"; "style"; "3d" ] -> Ok Transform_style_3d
    | [ "transform"; "style"; "flat" ] -> Ok Transform_style_flat
    | [ "transform" ] -> Ok Transform
    | [ "transform"; "none" ] -> Ok Transform_none
    | [ "transform"; "gpu" ] -> Ok Transform_gpu
    | _ -> err_not_utility

  let to_class = function
    | Rotate n -> "rotate-" ^ string_of_int n
    | Translate_x n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "translate-x-" ^ string_of_int (abs n)
    | Translate_y n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "translate-y-" ^ string_of_int (abs n)
    | Translate_z n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "translate-z-" ^ string_of_int (abs n)
    | Neg_translate_x_1_2 -> "-translate-x-1/2"
    | Neg_translate_y_1_2 -> "-translate-y-1/2"
    | Scale n -> "scale-" ^ string_of_int n
    | Scale_x n -> "scale-x-" ^ string_of_int n
    | Scale_y n -> "scale-y-" ^ string_of_int n
    | Scale_z n -> "scale-z-" ^ string_of_int n
    | Skew_x n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "skew-x-" ^ string_of_int (abs n)
    | Skew_y n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "skew-y-" ^ string_of_int (abs n)
    | Rotate_x n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "rotate-x-" ^ string_of_int (abs n)
    | Rotate_y n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "rotate-y-" ^ string_of_int (abs n)
    | Rotate_z n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "rotate-z-" ^ string_of_int (abs n)
    | Perspective n -> "perspective-" ^ string_of_int n
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
    | Transform_none -> "transform-none"
    | Transform_gpu -> "transform-gpu"
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
let perspective n = utility (Perspective n)
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
