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

open Core
open Css

(* Transform variables defined at top level *)
let tw_translate_x_def, tw_translate_x_var = var "tw-translate-x" Length Zero
let tw_translate_y_def, tw_translate_y_var = var "tw-translate-y" Length Zero
let tw_rotate_def, tw_rotate_var = var "tw-rotate" Angle (Deg 0.0)
let tw_skew_x_def, tw_skew_x_var = var "tw-skew-x" Angle (Deg 0.0)
let tw_skew_y_def, tw_skew_y_var = var "tw-skew-y" Angle (Deg 0.0)
let tw_scale_x_def, tw_scale_x_var = var "tw-scale-x" Float 1.0
let tw_scale_y_def, tw_scale_y_var = var "tw-scale-y" Float 1.0

module Parse = Parse

(** {1 2D Transform Utilities} *)

let rotate n =
  let class_name = "rotate-" ^ string_of_int n in
  style class_name [ rotate (Deg (float_of_int n)) ]

let translate_x n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "translate-x-" ^ string_of_int (abs n) in
  let len = if n = 0 then Zero else Rem (float_of_int n *. 0.25) in
  style class_name [ transform [ Translate_x len ] ]

let translate_y n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "translate-y-" ^ string_of_int (abs n) in
  let len = if n = 0 then Zero else Rem (float_of_int n *. 0.25) in
  style class_name [ transform [ Translate_y len ] ]

let scale n =
  let value = float_of_int n /. 100.0 in
  (* Convert percentage to float *)
  let class_name = "scale-" ^ string_of_int n in
  let def_scale_x, scale_x_var = var "tw-scale-x" Float value in
  let def_scale_y, scale_y_var = var "tw-scale-y" Float value in
  style class_name
    [
      def_scale_x;
      def_scale_y;
      transform
        [
          Scale_x (Scale_var { var_name = scale_x_var.name; fallback = None });
          Scale_y (Scale_var { var_name = scale_y_var.name; fallback = None });
        ];
    ]

let scale_x n =
  let value = float_of_int n /. 100.0 in
  (* Convert percentage to float *)
  let class_name = "scale-x-" ^ string_of_int n in
  let def_x, _scale_x = var "tw-scale-x" Float value in
  style class_name
    [
      def_x;
      transform
        [ Scale_x (Scale_var { var_name = "tw-scale-x"; fallback = Some 1.0 }) ];
    ]

let scale_y n =
  let value = float_of_int n /. 100.0 in
  (* Convert percentage to float *)
  let class_name = "scale-y-" ^ string_of_int n in
  let def_y, _scale_y = var "tw-scale-y" Float value in
  style class_name
    [
      def_y;
      transform
        [ Scale_y (Scale_var { var_name = "tw-scale-y"; fallback = Some 1.0 }) ];
    ]

let skew_x deg =
  let prefix = if deg < 0 then "-" else "" in
  let class_name = prefix ^ "skew-x-" ^ string_of_int (abs deg) in
  let def_skew, skew_var = var "tw-skew-x" Angle (Deg (float_of_int deg)) in
  style class_name [ def_skew; transform [ Skew_x (Var skew_var) ] ]

let skew_y deg =
  let prefix = if deg < 0 then "-" else "" in
  let class_name = prefix ^ "skew-y-" ^ string_of_int (abs deg) in
  let def_skew, skew_var = var "tw-skew-y" Angle (Deg (float_of_int deg)) in
  style class_name [ def_skew; transform [ Skew_y (Var skew_var) ] ]

(* Negative translate utilities for centering *)
let neg_translate_x_1_2 =
  style "-translate-x-1/2"
    [ Css.transform [ Css.Translate_x (Css.Pct (-50.0)) ] ]

let neg_translate_y_1_2 =
  style "-translate-y-1/2"
    [ Css.transform [ Css.Translate_y (Css.Pct (-50.0)) ] ]

(** {1 3D Transform Utilities} *)

let rotate_x n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "rotate-x-" ^ string_of_int (abs n) in
  style class_name [ transform [ Rotate_x (Deg (float_of_int n)) ] ]

let rotate_y n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "rotate-y-" ^ string_of_int (abs n) in
  style class_name [ transform [ Rotate_y (Deg (float_of_int n)) ] ]

let rotate_z n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "rotate-z-" ^ string_of_int (abs n) in
  style class_name [ transform [ Rotate_z (Deg (float_of_int n)) ] ]

let translate_z n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "translate-z-" ^ string_of_int (abs n) in
  style class_name [ transform [ Translate_z (Px n) ] ]

let scale_z n =
  let value = float_of_int n /. 100.0 in
  let class_name = "scale-z-" ^ string_of_int n in
  let def, scale_z_var = var "tw-scale-z" Float value in
  style class_name
    [
      def;
      transform
        [
          Scale_z
            (Scale_var { var_name = scale_z_var.name; fallback = Some 1.0 });
        ];
    ]

let perspective n =
  let class_name = "perspective-" ^ string_of_int n in
  let value = if n = 0 then Zero else Px n in
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
  style "transform"
    [
      tw_translate_x_def;
      tw_translate_y_def;
      tw_rotate_def;
      tw_skew_x_def;
      tw_skew_y_def;
      tw_scale_x_def;
      tw_scale_y_def;
      transform
        [
          Translate_x (Var tw_translate_x_var);
          Translate_y (Var tw_translate_y_var);
          Rotate_var { var_name = tw_rotate_var.name; fallback = None };
          Skew_x (Var tw_skew_x_var);
          Skew_y (Var tw_skew_y_var);
          Scale_x
            (Scale_var { var_name = tw_scale_x_var.name; fallback = None });
          Scale_y
            (Scale_var { var_name = tw_scale_y_var.name; fallback = None });
        ];
    ]

let transform_none = style "transform-none" [ Css.transform [ Transform_none ] ]
let transform_gpu = style "transform-gpu" [ Css.transform [ Translate_z Zero ] ]

(** {1 Parsing Functions} *)

let ( >|= ) = Parse.( >|= )

let of_string = function
  | [ "rotate"; n ] -> Parse.int_any n >|= rotate
  | [ "translate"; "x"; n ] -> Parse.int_any n >|= translate_x
  | [ "translate"; "y"; n ] -> Parse.int_any n >|= translate_y
  | [ "translate"; "z"; n ] -> Parse.int_any n >|= translate_z
  | [ "scale"; n ] -> Parse.int_pos ~name:"scale" n >|= scale
  | [ "scale"; "x"; n ] -> Parse.int_pos ~name:"scale-x" n >|= scale_x
  | [ "scale"; "y"; n ] -> Parse.int_pos ~name:"scale-y" n >|= scale_y
  | [ "scale"; "z"; n ] -> Parse.int_pos ~name:"scale-z" n >|= scale_z
  | [ "skew"; "x"; n ] -> Parse.int_any n >|= skew_x
  | [ "skew"; "y"; n ] -> Parse.int_any n >|= skew_y
  | [ "rotate"; "x"; n ] -> Parse.int_any n >|= rotate_x
  | [ "rotate"; "y"; n ] -> Parse.int_any n >|= rotate_y
  | [ "rotate"; "z"; n ] -> Parse.int_any n >|= rotate_z
  | [ "perspective"; n ] -> Parse.int_pos ~name:"perspective" n >|= perspective
  | [ "perspective"; "origin"; "center" ] -> Ok perspective_origin_center
  | [ "perspective"; "origin"; "top" ] -> Ok perspective_origin_top
  | [ "perspective"; "origin"; "bottom" ] -> Ok perspective_origin_bottom
  | [ "perspective"; "origin"; "left" ] -> Ok perspective_origin_left
  | [ "perspective"; "origin"; "right" ] -> Ok perspective_origin_right
  | [ "transform"; "style"; "3d" ] -> Ok transform_style_3d
  | [ "transform"; "style"; "flat" ] -> Ok transform_style_flat
  | [ "transform" ] -> Ok transform
  | [ "transform"; "none" ] -> Ok transform_none
  | [ "transform"; "gpu" ] -> Ok transform_gpu
  | _ -> Error (`Msg "Not a transform utility")
