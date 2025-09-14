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
  Css.stylesheet
    [
      (* Position must come first in properties layer *)
      Css.Property
        (Var.property ~inherits:false ~initial:"to bottom" Var.Gradient_position);
      (* Colors *)
      Css.Property
        (Var.property ~inherits:false ~initial:(Css.hex "#0000")
           Var.Gradient_from);
      Css.Property
        (Var.property ~inherits:false ~initial:(Css.hex "#0000")
           Var.Gradient_via);
      Css.Property
        (Var.property ~inherits:false ~initial:(Css.hex "#0000") Var.Gradient_to);
      (* Stops composition helpers *)
      Css.Property (Var.property ~inherits:false ~initial:"" Var.Gradient_stops);
      Css.Property
        (Var.property ~inherits:false ~initial:"" Var.Gradient_via_stops);
      (* Positions *)
      Css.Property
        (Var.property ~inherits:false ~initial:0. Var.Gradient_from_position);
      Css.Property
        (Var.property ~inherits:false ~initial:50. Var.Gradient_via_position);
      Css.Property
        (Var.property ~inherits:false ~initial:100. Var.Gradient_to_position);
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
  else String.concat "" [ prefix; Color.pp color; "-"; string_of_int shade ]

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
  (* Create color theme variable and reference it *)
  let color_name = Color.pp color in
  let color_theme_def, color_theme_var =
    Var.theme (Var.Color (color_name, Some shade)) (Color.to_css color shade)
  in
  (* Set gradient-from to reference the color variable *)
  let from_override, _ = Var.utility Var.Gradient_from (Var color_theme_var) in
  (* Set gradient-stops - this is the actual CSS value containing var()
     references *)
  let stops_value =
    "var(--tw-gradient-via-stops, var(--tw-gradient-position), \
     var(--tw-gradient-from)var(--tw-gradient-from-position), \
     var(--tw-gradient-to)var(--tw-gradient-to-position))"
  in
  let stops_override, _ = Var.utility Var.Gradient_stops stops_value in
  style class_name ~property_rules:gradient_property_rules
    [ color_theme_def; from_override; stops_override ]

let via_color ?(shade = 500) color =
  let class_name = gradient_color_class_name ~prefix:"via-" ~shade color in
  (* Create color theme variable and reference it *)
  let color_name = Color.pp color in
  let color_theme_def, color_theme_var =
    Var.theme (Var.Color (color_name, Some shade)) (Color.to_css color shade)
  in
  (* Set gradient-via to reference the color variable *)
  let via_override, _ = Var.utility Var.Gradient_via (Var color_theme_var) in
  (* Set gradient-via-stops - specific for via utilities *)
  let via_stops_value =
    "var(--tw-gradient-position), \
     var(--tw-gradient-from)var(--tw-gradient-from-position), \
     var(--tw-gradient-via)var(--tw-gradient-via-position), \
     var(--tw-gradient-to)var(--tw-gradient-to-position)"
  in
  let via_stops_override, _ =
    Var.utility Var.Gradient_via_stops via_stops_value
  in
  (* Set gradient-stops to reference via-stops *)
  let stops_value = "var(--tw-gradient-via-stops)" in
  let stops_override, _ = Var.utility Var.Gradient_stops stops_value in
  style class_name ~property_rules:gradient_property_rules
    [ color_theme_def; via_override; via_stops_override; stops_override ]

let to_color ?(shade = 500) color =
  let class_name = gradient_color_class_name ~prefix:"to-" ~shade color in
  (* Create color theme variable and reference it *)
  let color_name = Color.pp color in
  let color_theme_def, color_theme_var =
    Var.theme (Var.Color (color_name, Some shade)) (Color.to_css color shade)
  in
  (* Set gradient-to to reference the color variable *)
  let to_override, _ = Var.utility Var.Gradient_to (Var color_theme_var) in
  (* Set gradient-stops - same as from utilities *)
  let stops_value =
    "var(--tw-gradient-via-stops, var(--tw-gradient-position), \
     var(--tw-gradient-from)var(--tw-gradient-from-position), \
     var(--tw-gradient-to)var(--tw-gradient-to-position))"
  in
  let stops_override, _ = Var.utility Var.Gradient_stops stops_value in
  style class_name ~property_rules:gradient_property_rules
    [ color_theme_def; to_override; stops_override ]

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
      match Color.color_shade_of_strings rest with
      | Ok (color, shade) -> Ok (from_color ~shade color)
      | Error _ -> Error (`Msg "Invalid from color"))
  | "via" :: rest -> (
      match Color.color_shade_of_strings rest with
      | Ok (color, shade) -> Ok (via_color ~shade color)
      | Error _ -> Error (`Msg "Invalid via color"))
  | "to" :: rest -> (
      match Color.color_shade_of_strings rest with
      | Ok (color, shade) -> Ok (to_color ~shade color)
      | Error _ -> Error (`Msg "Invalid to color"))
  | _ -> Error (`Msg "Unknown background class")
