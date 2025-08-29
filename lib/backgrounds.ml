(** Background and gradient utilities *)

open Core

(* Gradient variables defined at top level with default #0000 values *)
let tw_gradient_from_def, tw_gradient_from_var =
  Var.utility Var.Gradient_from (Hex { hash = true; value = "0000" })

let _tw_gradient_via_def, _tw_gradient_via_var =
  Var.utility
    ~fallback:(Css.Hex { hash = true; value = "0000" })
    Var.Gradient_via
    (Hex { hash = true; value = "0000" })

let tw_gradient_to_def, tw_gradient_to_var =
  Var.utility Var.Gradient_to (Hex { hash = true; value = "0000" })

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
    (* Position must come first in properties layer *)
    Var.property Var.Gradient_position ~syntax:"*" ~inherits:false
      ~initial:"initial";
    (* Colors *)
    Var.property Var.Gradient_from ~syntax:"<color>" ~inherits:false
      ~initial:"#0000";
    Var.property Var.Gradient_via ~syntax:"<color>" ~inherits:false
      ~initial:"#0000";
    Var.property Var.Gradient_to ~syntax:"<color>" ~inherits:false
      ~initial:"#0000";
    (* Stops composition helpers *)
    Var.property Var.Gradient_stops ~syntax:"*" ~inherits:false
      ~initial:"initial";
    Var.property Var.Gradient_via_stops ~syntax:"*" ~inherits:false
      ~initial:"initial";
    (* Positions *)
    Var.property Var.Gradient_from_position ~syntax:"<length-percentage>"
      ~inherits:false ~initial:"0%";
    Var.property Var.Gradient_via_position ~syntax:"<length-percentage>"
      ~inherits:false ~initial:"50%";
    Var.property Var.Gradient_to_position ~syntax:"<length-percentage>"
      ~inherits:false ~initial:"100%";
  ]

let gradient_to_spec : direction -> string * Css.gradient_direction = function
  | Bottom -> ("bg-gradient-to-b", To_bottom)
  | Bottom_right -> ("bg-gradient-to-br", To_bottom_right)
  | Right -> ("bg-gradient-to-r", To_right)
  | Top_right -> ("bg-gradient-to-tr", To_top_right)
  | Top -> ("bg-gradient-to-t", To_top)
  | Top_left -> ("bg-gradient-to-tl", To_top_left)
  | Left -> ("bg-gradient-to-l", To_left)
  | Bottom_left -> ("bg-gradient-to-bl", To_bottom_left)

let bg_gradient_to dir =
  let class_name, dir_val = gradient_to_spec dir in
  style class_name ~property_rules:gradient_property_rules
    [
      tw_gradient_from_def;
      tw_gradient_to_def;
      Css.background_image
        (Linear_gradient
           (dir_val, [ Var tw_gradient_from_var; Var tw_gradient_to_var ]));
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
  style class_name ~property_rules:gradient_property_rules [ from_override ]

let via_color ?(shade = 500) color =
  let class_name = gradient_color_class_name ~prefix:"via-" ~shade color in
  let color_value = Color.to_css color shade in
  (* Create a new declaration that overrides the global variable's value *)
  let via_override, _ = Var.utility Var.Gradient_via color_value in
  style class_name ~property_rules:gradient_property_rules [ via_override ]

let to_color ?(shade = 500) color =
  let class_name = gradient_color_class_name ~prefix:"to-" ~shade color in
  let color_value = Color.to_css color shade in
  (* Create a new declaration that overrides the global variable's value *)
  let to_override, _ = Var.utility Var.Gradient_to color_value in
  style class_name ~property_rules:gradient_property_rules [ to_override ]

let of_string = function
  | [ "bg"; "gradient"; "to"; dir ] -> (
      match dir with
      | "b" -> Ok (bg_gradient_to Bottom)
      | "br" -> Ok (bg_gradient_to Bottom_right)
      | "r" -> Ok (bg_gradient_to Right)
      | "tr" -> Ok (bg_gradient_to Top_right)
      | "t" -> Ok (bg_gradient_to Top)
      | "tl" -> Ok (bg_gradient_to Top_left)
      | "l" -> Ok (bg_gradient_to Left)
      | "bl" -> Ok (bg_gradient_to Bottom_left)
      | _ -> Error (`Msg "Unknown gradient direction"))
  | "from" :: rest -> (
      match Color.color_and_shade_of_string_list rest with
      | Ok (color, shade) -> Ok (from_color ~shade color)
      | Error _ -> Error (`Msg "Invalid from color"))
  | "via" :: rest -> (
      match Color.color_and_shade_of_string_list rest with
      | Ok (color, shade) -> Ok (via_color ~shade color)
      | Error _ -> Error (`Msg "Invalid via color"))
  | "to" :: rest -> (
      match Color.color_and_shade_of_string_list rest with
      | Ok (color, shade) -> Ok (to_color ~shade color)
      | Error _ -> Error (`Msg "Invalid to color"))
  | _ -> Error (`Msg "Unknown background class")
