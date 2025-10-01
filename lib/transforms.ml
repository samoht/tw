(** Transform utilities for 2D and 3D transformations

    What's included:
    - 2D: `rotate-*`, `translate-x-*`, `translate-y-*` (supports negatives),
      `scale*`, `skew*`.
    - 3D: `rotate-x/y/z-*`, `translate-z-*`, `scale-z-*`, `perspective-*`,
      perspective origin, transform style/gpu toggles.

    What's not:
    - Full transform composition shorthands beyond helpers provided here. Use
      `transform` utility or build custom transforms with `Css.transform`.

    Parsing contract (`of_string`):
    - Accepts tokens like ["rotate"; n], ["translate"; "x"; n], ["scale"; n],
      ["perspective"; n], etc. Unknown tokens yield `Error (`Msg "Not a
      transform utility")`. *)

open Style
open Css

(** {1 Transforms Utility Type} *)

type utility =
  (* 2D Transforms *)
  | Rotate of int
  | Translate_x of int
  | Translate_y of int
  | Scale of int
  | Scale_x of int
  | Scale_y of int
  | Skew_x of int
  | Skew_y of int
  (* 3D Transforms *)
  | Translate_z of int
  | Rotate_x of int
  | Rotate_y of int
  | Rotate_z of int
  | Scale_z of int
  | Perspective of int
  (* Perspective origin *)
  | Perspective_origin_center
  | Perspective_origin_top
  | Perspective_origin_bottom
  | Perspective_origin_left
  | Perspective_origin_right
  (* Transform style *)
  | Transform_style_3d
  | Transform_style_flat
  (* Transform control *)
  | Transform
  | Transform_none
  | Transform_gpu

(* Transform variables using new API *)
let tw_translate_x_var = Var.channel Css.Length "tw-translate-x"
let tw_translate_y_var = Var.channel Css.Length "tw-translate-y"
let tw_rotate_var = Var.channel Css.Angle "tw-rotate"
let tw_skew_x_var = Var.channel Css.Angle "tw-skew-x"
let tw_skew_y_var = Var.channel Css.Angle "tw-skew-y"

(* Scale variables need @property defaults for composition *)
let tw_scale_x_var =
  Var.property_default Css.Number_percentage ~initial:(Num 1.0) ~universal:true
    "tw-scale-x"

let tw_scale_y_var =
  Var.property_default Css.Number_percentage ~initial:(Num 1.0) ~universal:true
    "tw-scale-y"

let tw_scale_z_var =
  Var.property_default Css.Number_percentage ~initial:(Num 1.0) ~universal:true
    "tw-scale-z"

module Parse = Parse

(** {1 2D Transform Utilities} *)

(* Note: @property rules are now automatically extracted from variables that
   have property metadata, so we don't need these anymore *)

let rotate n =
  let class_name = "rotate-" ^ string_of_int n in
  style class_name [ rotate (Deg (float_of_int n)) ]

let translate_x n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "translate-x-" ^ string_of_int (abs n) in
  let len : length = if n = 0 then Zero else Rem (float_of_int n *. 0.25) in
  style class_name [ transform (Translate_x len) ]

let translate_y n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "translate-y-" ^ string_of_int (abs n) in
  let len : length = if n = 0 then Zero else Rem (float_of_int n *. 0.25) in
  style class_name [ transform (Translate_y len) ]

let scale n =
  let class_name = "scale-" ^ string_of_int n in
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
  style class_name ~property_rules:props
    (dx :: dy :: dz :: [ scale (XY (Var scale_x_ref, Var scale_y_ref)) ])

let scale_x n =
  let value : Css.number_percentage = Css.Pct (float_of_int n) in
  let class_name = "scale-x-" ^ string_of_int n in
  (* Only uses X variable *)
  let d, _ = Var.binding tw_scale_x_var value in
  style class_name (d :: [ transform (Scale_x (float_of_int n /. 100.0)) ])

let scale_y n =
  let value : Css.number_percentage = Css.Pct (float_of_int n) in
  let class_name = "scale-y-" ^ string_of_int n in
  (* Only uses Y variable *)
  let d, _ = Var.binding tw_scale_y_var value in
  style class_name (d :: [ transform (Scale_y (float_of_int n /. 100.0)) ])

let skew_x deg =
  let prefix = if deg < 0 then "-" else "" in
  let class_name = prefix ^ "skew-x-" ^ string_of_int (abs deg) in
  let d, v = Var.binding tw_skew_x_var (Deg (float_of_int deg)) in
  style class_name (d :: [ transform (Skew_x (Var v)) ])

let skew_y deg =
  let prefix = if deg < 0 then "-" else "" in
  let class_name = prefix ^ "skew-y-" ^ string_of_int (abs deg) in
  let d, v = Var.binding tw_skew_y_var (Deg (float_of_int deg)) in
  style class_name (d :: [ transform (Skew_y (Var v)) ])

(* Negative translate utilities for centering *)
let neg_translate_x_1_2 =
  style "-translate-x-1/2" [ Css.transform (Css.Translate_x (Css.Pct (-50.0))) ]

let neg_translate_y_1_2 =
  style "-translate-y-1/2" [ Css.transform (Css.Translate_y (Css.Pct (-50.0))) ]

(** {1 3D Transform Utilities} *)

let rotate_x n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "rotate-x-" ^ string_of_int (abs n) in
  style class_name [ transform (Rotate_x (Deg (float_of_int n))) ]

let rotate_y n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "rotate-y-" ^ string_of_int (abs n) in
  style class_name [ transform (Rotate_y (Deg (float_of_int n))) ]

let rotate_z n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "rotate-z-" ^ string_of_int (abs n) in
  style class_name [ transform (Rotate_z (Deg (float_of_int n))) ]

let translate_z n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "translate-z-" ^ string_of_int (abs n) in
  style class_name [ transform (Translate_z (Px (float_of_int n))) ]

let scale_z n =
  let value : Css.number_percentage = Css.Pct (float_of_int n) in
  let class_name = "scale-z-" ^ string_of_int n in
  let d, _ = Var.binding tw_scale_z_var value in
  style class_name (d :: [ transform (Scale_z (float_of_int n /. 100.0)) ])

let perspective n =
  let class_name = "perspective-" ^ string_of_int n in
  let value : length = if n = 0 then Zero else Px (float_of_int n) in
  style class_name [ perspective value ]

let perspective_origin_center =
  style "perspective-origin-center" [ perspective_origin "center" ]

let perspective_origin_top =
  style "perspective-origin-top" [ perspective_origin "top" ]

let perspective_origin_bottom =
  style "perspective-origin-bottom" [ perspective_origin "bottom" ]

let perspective_origin_left =
  style "perspective-origin-left" [ perspective_origin "left" ]

let perspective_origin_right =
  style "perspective-origin-right" [ perspective_origin "right" ]

let transform_style_3d =
  style "transform-style-3d" [ transform_style Preserve_3d ]

let transform_style_flat = style "transform-style-flat" [ transform_style Flat ]
let backface_visible = style "backface-visible" [ backface_visibility Visible ]
let backface_hidden = style "backface-hidden" [ backface_visibility Hidden ]

(** {1 Transform Control Utilities} *)

let transform =
  let translate_x_decl, translate_x_var = Var.binding tw_translate_x_var Zero in
  let translate_y_decl, translate_y_var = Var.binding tw_translate_y_var Zero in
  let rotate_decl, rotate_var = Var.binding tw_rotate_var (Deg 0.0) in
  let skew_x_decl, skew_x_var = Var.binding tw_skew_x_var (Deg 0.0) in
  let skew_y_decl, skew_y_var = Var.binding tw_skew_y_var (Deg 0.0) in
  style "transform"
    (translate_x_decl :: translate_y_decl :: rotate_decl :: skew_x_decl
   :: skew_y_decl
    :: [
         transforms
           [
             Translate_x (Var translate_x_var);
             Translate_y (Var translate_y_var);
             Rotate (Var rotate_var);
             Skew_x (Var skew_x_var);
             Skew_y (Var skew_y_var);
             Scale_x 1.0;
             Scale_y 1.0;
           ];
       ])

let transform_none = style "transform-none" [ Css.transform None ]
let transform_gpu = style "transform-gpu" [ Css.transform (Translate_z Zero) ]

(** {1 Parsing Functions} *)

let ( >|= ) = Parse.( >|= )

(** {1 Utility Conversion Functions} *)

let to_style = function
  | Rotate n -> rotate n
  | Translate_x n -> translate_x n
  | Translate_y n -> translate_y n
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
  | Transform -> transform
  | Transform_none -> transform_none
  | Transform_gpu -> transform_gpu

let of_string = function
  | [ "rotate"; n ] -> Parse.int_any n >|= fun n -> Rotate n
  | [ "translate"; "x"; n ] -> Parse.int_any n >|= fun n -> Translate_x n
  | [ "translate"; "y"; n ] -> Parse.int_any n >|= fun n -> Translate_y n
  | [ "translate"; "z"; n ] -> Parse.int_any n >|= fun n -> Translate_z n
  | [ "scale"; n ] -> Parse.int_pos ~name:"scale" n >|= fun n -> Scale n
  | [ "scale"; "x"; n ] -> Parse.int_pos ~name:"scale-x" n >|= fun n -> Scale_x n
  | [ "scale"; "y"; n ] -> Parse.int_pos ~name:"scale-y" n >|= fun n -> Scale_y n
  | [ "scale"; "z"; n ] -> Parse.int_pos ~name:"scale-z" n >|= fun n -> Scale_z n
  | [ "skew"; "x"; n ] -> Parse.int_any n >|= fun n -> Skew_x n
  | [ "skew"; "y"; n ] -> Parse.int_any n >|= fun n -> Skew_y n
  | [ "rotate"; "x"; n ] -> Parse.int_any n >|= fun n -> Rotate_x n
  | [ "rotate"; "y"; n ] -> Parse.int_any n >|= fun n -> Rotate_y n
  | [ "rotate"; "z"; n ] -> Parse.int_any n >|= fun n -> Rotate_z n
  | [ "perspective"; n ] -> Parse.int_pos ~name:"perspective" n >|= fun n -> Perspective n
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
  | _ -> Error (`Msg "Not a transform utility")

let suborder = function
  | Transform -> 0
  | Transform_none -> 1
  | Transform_gpu -> 2
  | Rotate n -> 100 + n
  | Rotate_x n -> 150 + n
  | Rotate_y n -> 160 + n
  | Rotate_z n -> 170 + n
  | Scale n -> 200 + n
  | Scale_x n -> 250 + n
  | Scale_y n -> 260 + n
  | Scale_z n -> 270 + n
  | Translate_x n -> 300 + n
  | Translate_y n -> 350 + n
  | Translate_z n -> 360 + n
  | Skew_x n -> 400 + n
  | Skew_y n -> 450 + n
  | Perspective n -> 500 + n
  | Perspective_origin_center -> 600
  | Perspective_origin_top -> 601
  | Perspective_origin_bottom -> 602
  | Perspective_origin_left -> 603
  | Perspective_origin_right -> 604
  | Transform_style_3d -> 700
  | Transform_style_flat -> 701
