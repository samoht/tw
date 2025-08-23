(** Transform utilities for 2D and 3D transformations *)

open Core
open Css

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
  let value = string_of_int n ^ "%" in
  let class_name = "scale-" ^ string_of_int n in
  style class_name
    [
      custom_property "--tw-scale-x" value;
      custom_property "--tw-scale-y" value;
      transform
        [ Scale (Scale_var { var_name = "tw-scale-x"; fallback = Some 1.0 }) ];
    ]

let scale_x n =
  let value = string_of_int n ^ "%" in
  let class_name = "scale-x-" ^ string_of_int n in
  style class_name
    [
      custom_property "--tw-scale-x" value;
      transform
        [ Scale_x (Scale_var { var_name = "tw-scale-x"; fallback = Some 1.0 }) ];
    ]

let scale_y n =
  let value = string_of_int n ^ "%" in
  let class_name = "scale-y-" ^ string_of_int n in
  style class_name
    [
      custom_property "--tw-scale-y" value;
      transform
        [ Scale_y (Scale_var { var_name = "tw-scale-y"; fallback = Some 1.0 }) ];
    ]

let skew_x deg =
  let prefix = if deg < 0 then "-" else "" in
  let class_name = prefix ^ "skew-x-" ^ string_of_int (abs deg) in
  style class_name
    [
      custom_property "--tw-skew-x" (string_of_int deg ^ "deg");
      transform
        [ Skew_x (Angle_var { var_name = "tw-skew-x"; fallback = None }) ];
    ]

let skew_y deg =
  let prefix = if deg < 0 then "-" else "" in
  let class_name = prefix ^ "skew-y-" ^ string_of_int (abs deg) in
  style class_name
    [
      custom_property "--tw-skew-y" (string_of_int deg ^ "deg");
      transform
        [ Skew_y (Angle_var { var_name = "tw-skew-y"; fallback = None }) ];
    ]

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
  style class_name
    [
      custom_property "--tw-scale-z" (Pp.float value);
      transform
        [
          Translate_var
            {
              var_name = "tw-scale-x) scaleY(var(--tw-scale-y";
              fallback = None;
            };
          Scale_z (Scale_var { var_name = "tw-scale-z"; fallback = Some 1.0 });
          Translate_var
            { var_name = "tw-skew-x) skewY(var(--tw-skew-y"; fallback = None };
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
      custom_property "--tw-translate-x" "0";
      custom_property "--tw-translate-y" "0";
      custom_property "--tw-rotate" "0";
      custom_property "--tw-skew-x" "0";
      custom_property "--tw-skew-y" "0";
      custom_property "--tw-scale-x" "1";
      custom_property "--tw-scale-y" "1";
      transform
        [
          Translate_x (Var (var "tw-translate-x"));
          Translate_y (Var (var "tw-translate-y"));
          Rotate_var { var_name = "tw-rotate"; fallback = None };
          Skew_x (Angle_var { var_name = "tw-skew-x"; fallback = None });
          Skew_y (Angle_var { var_name = "tw-skew-y"; fallback = None });
          Scale_x (Scale_var { var_name = "tw-scale-x"; fallback = None });
          Scale_y (Scale_var { var_name = "tw-scale-y"; fallback = None });
        ];
    ]

let transform_none = style "transform-none" [ Css.transform [ Transform_none ] ]
let transform_gpu = style "transform-gpu" [ Css.transform [ Translate_z Zero ] ]

(** {1 Parsing Functions} *)

let int_of_string_with_sign s =
  match int_of_string_opt s with
  | None -> Error (`Msg ("Invalid number: " ^ s))
  | Some n -> Ok n

let int_of_string_positive name s =
  match int_of_string_opt s with
  | None -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))
  | Some n when n >= 0 -> Ok n
  | Some _ -> Error (`Msg (name ^ " must be non-negative: " ^ s))

let ( >|= ) r f = Result.map f r

let of_string = function
  | [ "rotate"; n ] -> int_of_string_with_sign n >|= rotate
  | [ "translate"; "x"; n ] -> int_of_string_with_sign n >|= translate_x
  | [ "translate"; "y"; n ] -> int_of_string_with_sign n >|= translate_y
  | [ "translate"; "z"; n ] -> int_of_string_with_sign n >|= translate_z
  | [ "scale"; n ] -> int_of_string_positive "scale" n >|= scale
  | [ "scale"; "x"; n ] -> int_of_string_positive "scale-x" n >|= scale_x
  | [ "scale"; "y"; n ] -> int_of_string_positive "scale-y" n >|= scale_y
  | [ "scale"; "z"; n ] -> int_of_string_positive "scale-z" n >|= scale_z
  | [ "skew"; "x"; n ] -> int_of_string_with_sign n >|= skew_x
  | [ "skew"; "y"; n ] -> int_of_string_with_sign n >|= skew_y
  | [ "rotate"; "x"; n ] -> int_of_string_with_sign n >|= rotate_x
  | [ "rotate"; "y"; n ] -> int_of_string_with_sign n >|= rotate_y
  | [ "rotate"; "z"; n ] -> int_of_string_with_sign n >|= rotate_z
  | [ "perspective"; n ] ->
      int_of_string_positive "perspective" n >|= perspective
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
