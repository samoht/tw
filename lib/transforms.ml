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

(* Extend variable kinds for transforms *)
type _ Var.kind +=
  | (* Transform variables *)
      Tw_translate_x :
      Css.length Var.kind
  | Tw_translate_y : Css.length Var.kind
  | Tw_rotate : Css.angle Var.kind
  | Tw_skew_x : Css.angle Var.kind
  | Tw_skew_y : Css.angle Var.kind
  | Tw_scale_x : float Var.kind
  | Tw_scale_y : float Var.kind
  | Tw_scale_z : float Var.kind

(* Transform variables using new API *)
let tw_translate_x_var =
  Var.create Tw_translate_x "tw-translate-x" ~layer:Utility ~fallback:Zero

let tw_translate_y_var =
  Var.create Tw_translate_y "tw-translate-y" ~layer:Utility ~fallback:Css.Zero

let tw_rotate_var =
  Var.create Tw_rotate "tw-rotate" ~layer:Utility ~fallback:(Deg 0.0)

let tw_skew_x_var =
  Var.create Tw_skew_x "tw-skew-x" ~layer:Utility ~fallback:(Deg 0.0)

let tw_skew_y_var =
  Var.create Tw_skew_y "tw-skew-y" ~layer:Utility ~fallback:(Deg 0.0)

let tw_scale_x_var =
  Var.create Tw_scale_x "tw-scale-x" ~layer:Utility ~fallback:1.0

let tw_scale_y_var =
  Var.create Tw_scale_y "tw-scale-y" ~layer:Utility ~fallback:1.0

let tw_scale_z_var =
  Var.create Tw_scale_z "tw-scale-z" ~layer:Utility ~fallback:1.0

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
  style class_name [ transform [ Translate_x len ] ]

let translate_y n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "translate-y-" ^ string_of_int (abs n) in
  let len : length = if n = 0 then Zero else Rem (float_of_int n *. 0.25) in
  style class_name [ transform [ Translate_y len ] ]

let scale n =
  let value = float_of_int n /. 100.0 in
  let class_name = "scale-" ^ string_of_int n in
  (* Sets all three scale variables but only uses X and Y in the scale property
     for 2D scale *)
  style class_name
    ~vars:
      [
        Binding (tw_scale_x_var, value);
        Binding (tw_scale_y_var, value);
        Binding (tw_scale_z_var, value);
      ]
    [ Css.scale (Css.XY (value, value)) ]

let scale_x n =
  let value = float_of_int n /. 100.0 in
  (* Convert percentage to float *)
  let class_name = "scale-x-" ^ string_of_int n in
  (* Only uses X variable *)
  style class_name
    ~vars:[ Binding (tw_scale_x_var, value) ]
    [ transform [ Scale_x value ] ]

let scale_y n =
  let value = float_of_int n /. 100.0 in
  (* Convert percentage to float *)
  let class_name = "scale-y-" ^ string_of_int n in
  (* Only uses Y variable *)
  style class_name
    ~vars:[ Binding (tw_scale_y_var, value) ]
    [ transform [ Scale_y value ] ]

let skew_x deg =
  let prefix = if deg < 0 then "-" else "" in
  let class_name = prefix ^ "skew-x-" ^ string_of_int (abs deg) in
  style class_name
    ~vars:[ Binding (tw_skew_x_var, Deg (float_of_int deg)) ]
    [ transform [ Skew_x (Var (Var.use tw_skew_x_var)) ] ]

let skew_y deg =
  let prefix = if deg < 0 then "-" else "" in
  let class_name = prefix ^ "skew-y-" ^ string_of_int (abs deg) in
  style class_name
    ~vars:[ Binding (tw_skew_y_var, Deg (float_of_int deg)) ]
    [ transform [ Skew_y (Var (Var.use tw_skew_y_var)) ] ]

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
  style class_name [ transform [ Translate_z (Px (float_of_int n)) ] ]

let scale_z n =
  let value = float_of_int n /. 100.0 in
  let class_name = "scale-z-" ^ string_of_int n in
  style class_name
    ~vars:[ Binding (tw_scale_z_var, value) ]
    [ transform [ Scale_z value ] ]

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
  style "transform"
    ~vars:
      [
        Binding (tw_translate_x_var, Zero);
        Binding (tw_translate_y_var, Zero);
        Binding (tw_rotate_var, Deg 0.0);
        Binding (tw_skew_x_var, Deg 0.0);
        Binding (tw_skew_y_var, Deg 0.0);
        Binding (tw_scale_x_var, 1.0);
        Binding (tw_scale_y_var, 1.0);
      ]
    [
      transform
        [
          Translate_x (Var (Var.use tw_translate_x_var));
          Translate_y (Var (Var.use tw_translate_y_var));
          Rotate (Var (Var.use tw_rotate_var));
          Skew_x (Var (Var.use tw_skew_x_var));
          Skew_y (Var (Var.use tw_skew_y_var));
          Scale_x 1.0;
          Scale_y 1.0;
        ];
    ]

let transform_none = style "transform-none" [ Css.transform [ None ] ]
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
