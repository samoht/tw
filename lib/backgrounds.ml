(** Background and gradient utilities *)

open Core

(* Gradient variables defined at top level with default transparent values *)
let tw_gradient_from_def, tw_gradient_from_var =
  Css.var "tw-gradient-from" Color Transparent

let _tw_gradient_via_def, _tw_gradient_via_var =
  Css.var ~fallback:(Value Css.Transparent) "tw-gradient-via" Color Transparent

let tw_gradient_to_def, tw_gradient_to_var =
  Css.var "tw-gradient-to" Color Transparent

type direction =
  | Bottom
  | Bottom_right
  | Right
  | Top_right
  | Top
  | Top_left
  | Left
  | Bottom_left

let bg_gradient_to = function
  | Bottom ->
      style "bg-gradient-to-b"
        [
          tw_gradient_from_def;
          tw_gradient_to_def;
          Css.background_image
            (Linear_gradient
               ( To_bottom,
                 [
                   Color_var tw_gradient_from_var; Color_var tw_gradient_to_var;
                 ] ));
        ]
  | Bottom_right ->
      style "bg-gradient-to-br"
        [
          tw_gradient_from_def;
          tw_gradient_to_def;
          Css.background_image
            (Linear_gradient
               ( To_bottom_right,
                 [
                   Color_var tw_gradient_from_var; Color_var tw_gradient_to_var;
                 ] ));
        ]
  | Right ->
      style "bg-gradient-to-r"
        [
          tw_gradient_from_def;
          tw_gradient_to_def;
          Css.background_image
            (Linear_gradient
               ( To_right,
                 [
                   Color_var tw_gradient_from_var; Color_var tw_gradient_to_var;
                 ] ));
        ]
  | Top_right ->
      style "bg-gradient-to-tr"
        [
          tw_gradient_from_def;
          tw_gradient_to_def;
          Css.background_image
            (Linear_gradient
               ( To_top_right,
                 [
                   Color_var tw_gradient_from_var; Color_var tw_gradient_to_var;
                 ] ));
        ]
  | Top ->
      style "bg-gradient-to-t"
        [
          tw_gradient_from_def;
          tw_gradient_to_def;
          Css.background_image
            (Linear_gradient
               ( To_top,
                 [
                   Color_var tw_gradient_from_var; Color_var tw_gradient_to_var;
                 ] ));
        ]
  | Top_left ->
      style "bg-gradient-to-tl"
        [
          tw_gradient_from_def;
          tw_gradient_to_def;
          Css.background_image
            (Linear_gradient
               ( To_top_left,
                 [
                   Color_var tw_gradient_from_var; Color_var tw_gradient_to_var;
                 ] ));
        ]
  | Left ->
      style "bg-gradient-to-l"
        [
          tw_gradient_from_def;
          tw_gradient_to_def;
          Css.background_image
            (Linear_gradient
               ( To_left,
                 [
                   Color_var tw_gradient_from_var; Color_var tw_gradient_to_var;
                 ] ));
        ]
  | Bottom_left ->
      style "bg-gradient-to-bl"
        [
          tw_gradient_from_def;
          tw_gradient_to_def;
          Css.background_image
            (Linear_gradient
               ( To_bottom_left,
                 [
                   Color_var tw_gradient_from_var; Color_var tw_gradient_to_var;
                 ] ));
        ]

(* Legacy fixed-direction helpers removed in favor of bg_gradient_to *)

(** Helper to build gradient color class names and variable references *)
let gradient_color_helpers ~prefix ?(shade = 500) color =
  let class_name =
    if Color.is_base_color color || Color.is_custom_color color then
      prefix ^ Color.pp color
    else Pp.str [ prefix; Color.pp color; "-"; string_of_int shade ]
  in
  let var_str =
    if Color.is_base_color color || Color.is_custom_color color then
      Pp.str [ "var(--color-"; Color.pp color; ")" ]
    else
      Pp.str [ "var(--color-"; Color.pp color; "-"; string_of_int shade; ")" ]
  in
  (class_name, var_str)

(** Common gradient stops dependencies *)
let gradient_deps_base =
  [
    "--tw-gradient-position";
    "--tw-gradient-from";
    "--tw-gradient-from-position";
    "--tw-gradient-to";
    "--tw-gradient-to-position";
  ]

let _gradient_deps_with_via =
  "--tw-gradient-via" :: "--tw-gradient-via-position" :: gradient_deps_base

let from_color ?(shade = 500) color =
  let class_name, _ = gradient_color_helpers ~prefix:"from-" ~shade color in
  let color_value = Color.to_css color shade in
  (* Create a new declaration that overrides the global variable's value *)
  let from_override, _ = Css.var "tw-gradient-from" Color color_value in
  style class_name [ from_override ]

let via_color ?(shade = 500) color =
  let class_name, _ = gradient_color_helpers ~prefix:"via-" ~shade color in
  let color_value = Color.to_css color shade in
  (* Create a new declaration that overrides the global variable's value *)
  let via_override, _ = Css.var "tw-gradient-via" Color color_value in
  style class_name [ via_override ]

let to_color ?(shade = 500) color =
  let class_name, _ = gradient_color_helpers ~prefix:"to-" ~shade color in
  let color_value = Color.to_css color shade in
  (* Create a new declaration that overrides the global variable's value *)
  let to_override, _ = Css.var "tw-gradient-to" Color color_value in
  style class_name [ to_override ]
