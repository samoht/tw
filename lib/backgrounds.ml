(** Background and gradient utilities *)

open Core

(* Gradient variables defined at top level with default transparent values *)
let tw_gradient_from_def, tw_gradient_from_var =
  Var.utility Var.Gradient_from Transparent

let _tw_gradient_via_def, _tw_gradient_via_var =
  Var.utility ~fallback:Css.Transparent Var.Gradient_via Transparent

let tw_gradient_to_def, tw_gradient_to_var =
  Var.utility Var.Gradient_to Transparent

type direction =
  | Bottom
  | Bottom_right
  | Right
  | Top_right
  | Top
  | Top_left
  | Left
  | Bottom_left

(* Shared @property rules for gradient variables (colors + positions + stops) *)
let gradient_property_rules =
  [
    (* Colors *)
    Var.property Var.Gradient_from ~syntax:"<color>" ~inherits:false
      ~initial:"transparent";
    Var.property Var.Gradient_via ~syntax:"<color>" ~inherits:false
      ~initial:"transparent";
    Var.property Var.Gradient_to ~syntax:"<color>" ~inherits:false
      ~initial:"transparent";
    (* Positions *)
    Var.property Var.Gradient_from_position ~syntax:"<length-percentage>"
      ~inherits:false ~initial:"0%";
    Var.property Var.Gradient_via_position ~syntax:"<length-percentage>"
      ~inherits:false ~initial:"50%";
    Var.property Var.Gradient_to_position ~syntax:"<length-percentage>"
      ~inherits:false ~initial:"100%";
    (* Stops composition helpers *)
    Var.property Var.Gradient_stops ~syntax:"*" ~inherits:false ~initial:"";
    Var.property Var.Gradient_via_stops ~syntax:"*" ~inherits:false ~initial:"";
    Var.property Var.Gradient_position ~syntax:"*" ~inherits:false ~initial:"";
  ]

let bg_gradient_to = function
  | Bottom ->
      style "bg-gradient-to-b" ~property_rules:gradient_property_rules
        [
          tw_gradient_from_def;
          tw_gradient_to_def;
          Css.background_image
            (Linear_gradient
               (To_bottom, [ Var tw_gradient_from_var; Var tw_gradient_to_var ]));
        ]
  | Bottom_right ->
      style "bg-gradient-to-br" ~property_rules:gradient_property_rules
        [
          tw_gradient_from_def;
          tw_gradient_to_def;
          Css.background_image
            (Linear_gradient
               ( To_bottom_right,
                 [ Var tw_gradient_from_var; Var tw_gradient_to_var ] ));
        ]
  | Right ->
      style "bg-gradient-to-r" ~property_rules:gradient_property_rules
        [
          tw_gradient_from_def;
          tw_gradient_to_def;
          Css.background_image
            (Linear_gradient
               (To_right, [ Var tw_gradient_from_var; Var tw_gradient_to_var ]));
        ]
  | Top_right ->
      style "bg-gradient-to-tr" ~property_rules:gradient_property_rules
        [
          tw_gradient_from_def;
          tw_gradient_to_def;
          Css.background_image
            (Linear_gradient
               ( To_top_right,
                 [ Var tw_gradient_from_var; Var tw_gradient_to_var ] ));
        ]
  | Top ->
      style "bg-gradient-to-t" ~property_rules:gradient_property_rules
        [
          tw_gradient_from_def;
          tw_gradient_to_def;
          Css.background_image
            (Linear_gradient
               (To_top, [ Var tw_gradient_from_var; Var tw_gradient_to_var ]));
        ]
  | Top_left ->
      style "bg-gradient-to-tl" ~property_rules:gradient_property_rules
        [
          tw_gradient_from_def;
          tw_gradient_to_def;
          Css.background_image
            (Linear_gradient
               ( To_top_left,
                 [ Var tw_gradient_from_var; Var tw_gradient_to_var ] ));
        ]
  | Left ->
      style "bg-gradient-to-l" ~property_rules:gradient_property_rules
        [
          tw_gradient_from_def;
          tw_gradient_to_def;
          Css.background_image
            (Linear_gradient
               (To_left, [ Var tw_gradient_from_var; Var tw_gradient_to_var ]));
        ]
  | Bottom_left ->
      style "bg-gradient-to-bl" ~property_rules:gradient_property_rules
        [
          tw_gradient_from_def;
          tw_gradient_to_def;
          Css.background_image
            (Linear_gradient
               ( To_bottom_left,
                 [ Var tw_gradient_from_var; Var tw_gradient_to_var ] ));
        ]

(* Legacy fixed-direction helpers removed in favor of bg_gradient_to *)

(** Helper to build gradient color class names *)
let gradient_color_class_name ~prefix ?(shade = 500) color =
  if Color.is_base_color color || Color.is_custom_color color then
    prefix ^ Color.pp color
  else Pp.str [ prefix; Color.pp color; "-"; string_of_int shade ]

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
  let class_name = gradient_color_class_name ~prefix:"from-" ~shade color in
  let color_value = Color.to_css color shade in
  (* Create a new declaration that overrides the global variable's value *)
  let from_override, _ = Var.utility Var.Gradient_from color_value in
  style class_name [ from_override ]

let via_color ?(shade = 500) color =
  let class_name = gradient_color_class_name ~prefix:"via-" ~shade color in
  let color_value = Color.to_css color shade in
  (* Create a new declaration that overrides the global variable's value *)
  let via_override, _ = Var.utility Var.Gradient_via color_value in
  style class_name [ via_override ]

let to_color ?(shade = 500) color =
  let class_name = gradient_color_class_name ~prefix:"to-" ~shade color in
  let color_value = Color.to_css color shade in
  (* Create a new declaration that overrides the global variable's value *)
  let to_override, _ = Var.utility Var.Gradient_to color_value in
  style class_name [ to_override ]
