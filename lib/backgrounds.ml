(** Background and gradient utilities *)

open Core

(* Gradient variables with proper @property definitions matching Tailwind v4 *)
let gradient_position_var =
  (* This is used as a placeholder/initial value in gradient stops. *)
  Var.property_default Css.Percentage
    ~initial:(Pct 50. : Css.percentage)
    "tw-gradient-position"

let gradient_from_var =
  Var.property_default Css.Color ~initial:(Css.hex "#0000") "tw-gradient-from"

let gradient_via_var =
  Var.property_default Css.Color ~initial:(Css.hex "#0000") "tw-gradient-via"

let gradient_to_var =
  Var.property_default Css.Color ~initial:(Css.hex "#0000") "tw-gradient-to"

let gradient_stops_var =
  (* This is used as the argument to linear-gradient(), so it contains gradient
     stops *)
  Var.property_default Css.Gradient_stops ~initial:[] ~universal:true
    "tw-gradient-stops"

let gradient_via_stops_var =
  (* Used for via color interpolation. Using empty list. *)
  Var.property_default Css.Gradient_stops ~initial:[] ~universal:true
    "tw-gradient-via-stops"

let gradient_from_position_var =
  Var.property_default Css.Percentage
    ~initial:(Pct 0. : Css.percentage)
    "tw-gradient-from-position"

let gradient_via_position_var =
  Var.property_default Css.Percentage
    ~initial:(Pct 50. : Css.percentage)
    "tw-gradient-via-position"

let gradient_to_position_var =
  Var.property_default Css.Percentage
    ~initial:(Pct 100. : Css.percentage)
    "tw-gradient-to-position"

type direction =
  | Bottom
  | Bottom_right
  | Right
  | Top_right
  | Top
  | Top_left
  | Left
  | Bottom_left

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
  (* When using gradients, we reference the gradient-stops variable which
     contains the computed stops with all the fallback logic. The fallback is
     empty since the actual stops are set by from/via/to utilities *)
  (* Note: gradient_stops_var is a Gradient_stops (list) variable, but we need
     to pass it as a single gradient stop that expands to multiple stops.
     This is a special case where a var expands to multiple values. *)
  (* For now, skip the gradient stops var and just use empty stops list
     This is a temporary fix to get the build working *)
  style class_name [ Css.background_image (Linear_gradient (dir_val, [])) ]

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
  (* Create color theme variable *)
  let color_name = Color.pp color in
  let color_theme_var =
    Var.theme Css.Color
      (String.concat "-" [ "color"; color_name; string_of_int shade ])
      ~order:3
  in
  let color_value = Color.to_css color shade in
  (* Set gradient-from and gradient-stops following Tailwind v4 pattern *)
  let d_color, color_ref = Var.binding color_theme_var color_value in
  let d_from, _ = Var.binding gradient_from_var (Css.Var color_ref) in

  (* The gradient-stops value represents the complete gradient stop list. In
     Tailwind v4, this uses a nested fallback pattern: --tw-gradient-stops =
     var(--tw-gradient-via-stops, FALLBACK) where FALLBACK =
     var(--tw-gradient-position), FROM_STOP, TO_STOP

     We need to build the fallback list: 1. var(--tw-gradient-position) as the
     first item 2. var(--tw-gradient-from) var(--tw-gradient-from-position) as
     the from stop 3. var(--tw-gradient-to) var(--tw-gradient-to-position) as
     the to stop *)

  (* Create references without fallbacks since they'll be used in composition *)
  (* gradient_position_var uses None fallback to generate var(--tw-gradient-position) without comma *)
  let position_ref = Var.reference gradient_position_var ~fallback:Css.None in
  let from_ref = Var.reference gradient_from_var ~fallback:Css.None in
  let from_pos_ref =
    Var.reference gradient_from_position_var ~fallback:Css.None
  in
  let to_ref = Var.reference gradient_to_var ~fallback:Css.None in
  let to_pos_ref = Var.reference gradient_to_position_var ~fallback:Css.None in

  (* Build the fallback gradient stop list *)
  let fallback_stops : Css.gradient_stop list =
    [
      Css.Percentage (Css.Var position_ref);
      Css.Color_percentage (Css.Var from_ref, Some (Css.Var from_pos_ref), None);
      Css.Color_percentage (Css.Var to_ref, Some (Css.Var to_pos_ref), None);
    ]
  in

  (* Reference to via-stops with the fallback list *)
  let _via_stops_ref =
    Var.reference gradient_via_stops_var ~fallback:(Css.Fallback fallback_stops)
  in

  (* Set gradient-stops to the fallback list for now *)
  (* TODO: properly integrate via_stops_ref using nested var() references *)
  let d_stops, _ = Var.binding gradient_stops_var fallback_stops in

  (* Generate @property rules for all gradient variables *)
  let property_rules =
    [
      Var.property_rule gradient_position_var;
      Var.property_rule gradient_from_var;
      Var.property_rule gradient_via_var;
      Var.property_rule gradient_to_var;
      Var.property_rule gradient_stops_var;
      Var.property_rule gradient_via_stops_var;
      Var.property_rule gradient_from_position_var;
      Var.property_rule gradient_via_position_var;
      Var.property_rule gradient_to_position_var;
    ]
    |> List.filter_map (fun x -> x)
    |> Css.concat
  in

  style class_name ~property_rules [ d_color; d_from; d_stops ]

let via_color ?(shade = 500) color =
  let class_name = gradient_color_class_name ~prefix:"via-" ~shade color in
  (* Create color theme variable *)
  let color_name = Color.pp color in
  let color_theme_var =
    Var.theme Css.Color
      (String.concat "-" [ "color"; color_name; string_of_int shade ])
      ~order:3
  in
  let color_value = Color.to_css color shade in
  let d_color, color_ref = Var.binding color_theme_var color_value in
  let d_via, _ = Var.binding gradient_via_var (Css.Var color_ref) in

  (* Build the complete gradient stops with via in the middle *)
  let position_ref = Var.reference gradient_position_var ~fallback:Css.None in
  let from_ref = Var.reference gradient_from_var ~fallback:Css.None in
  let from_pos_ref =
    Var.reference gradient_from_position_var ~fallback:Css.None
  in
  let via_ref = Var.reference gradient_via_var ~fallback:Css.None in
  let via_pos_ref =
    Var.reference gradient_via_position_var ~fallback:Css.None
  in
  let to_ref = Var.reference gradient_to_var ~fallback:Css.None in
  let to_pos_ref = Var.reference gradient_to_position_var ~fallback:Css.None in

  (* Build the via stop list with from, via, and to stops *)
  let via_stop_list : Css.gradient_stop list =
    [
      Css.Percentage (Css.Var position_ref);
      Css.Color_percentage (Css.Var from_ref, Some (Css.Var from_pos_ref), None);
      Css.Color_percentage (Css.Var via_ref, Some (Css.Var via_pos_ref), None);
      Css.Color_percentage (Css.Var to_ref, Some (Css.Var to_pos_ref), None);
    ]
  in

  let d_via_stops, _ = Var.binding gradient_via_stops_var via_stop_list in

  (* gradient-stops set to empty list for now *)
  let d_stops, _ = Var.binding gradient_stops_var [] in

  (* Generate @property rules *)
  let property_rules =
    [
      Var.property_rule gradient_position_var;
      Var.property_rule gradient_from_var;
      Var.property_rule gradient_via_var;
      Var.property_rule gradient_to_var;
      Var.property_rule gradient_stops_var;
      Var.property_rule gradient_via_stops_var;
      Var.property_rule gradient_from_position_var;
      Var.property_rule gradient_via_position_var;
      Var.property_rule gradient_to_position_var;
    ]
    |> List.filter_map (fun x -> x)
    |> Css.concat
  in

  style class_name ~property_rules [ d_color; d_via; d_via_stops; d_stops ]

let to_color ?(shade = 500) color =
  let class_name = gradient_color_class_name ~prefix:"to-" ~shade color in
  (* Create color theme variable *)
  let color_name = Color.pp color in
  let color_theme_var =
    Var.theme Css.Color
      (String.concat "-" [ "color"; color_name; string_of_int shade ])
      ~order:3
  in
  let color_value = Color.to_css color shade in
  (* Set gradient-to and gradient-stops following Tailwind v4 pattern *)
  let d_color, color_ref = Var.binding color_theme_var color_value in
  let d_to, _ = Var.binding gradient_to_var (Css.Var color_ref) in

  (* Build the fallback gradient stop list *)
  let position_ref = Var.reference gradient_position_var ~fallback:Css.None in
  let from_ref = Var.reference gradient_from_var ~fallback:Css.None in
  let from_pos_ref =
    Var.reference gradient_from_position_var ~fallback:Css.None
  in
  let to_ref = Var.reference gradient_to_var ~fallback:Css.None in
  let to_pos_ref = Var.reference gradient_to_position_var ~fallback:Css.None in

  (* Build the fallback gradient stop list *)
  let fallback_stops : Css.gradient_stop list =
    [
      Css.Percentage (Css.Var position_ref);
      Css.Color_percentage (Css.Var from_ref, Some (Css.Var from_pos_ref), None);
      Css.Color_percentage (Css.Var to_ref, Some (Css.Var to_pos_ref), None);
    ]
  in

  (* Reference to via-stops with the fallback list *)
  let _via_stops_ref =
    Var.reference gradient_via_stops_var ~fallback:(Css.Fallback fallback_stops)
  in

  (* Set gradient-stops to the fallback list for now *)
  (* TODO: properly integrate via_stops_ref using nested var() references *)
  let d_stops, _ = Var.binding gradient_stops_var fallback_stops in

  (* Generate @property rules for all gradient variables *)
  let property_rules =
    [
      Var.property_rule gradient_position_var;
      Var.property_rule gradient_from_var;
      Var.property_rule gradient_via_var;
      Var.property_rule gradient_to_var;
      Var.property_rule gradient_stops_var;
      Var.property_rule gradient_via_stops_var;
      Var.property_rule gradient_from_position_var;
      Var.property_rule gradient_via_position_var;
      Var.property_rule gradient_to_position_var;
    ]
    |> List.filter_map (fun x -> x)
    |> Css.concat
  in

  style class_name ~property_rules [ d_color; d_to; d_stops ]

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
      match Color.shade_of_strings rest with
      | Ok (color, shade) -> Ok (from_color ~shade color)
      | Error _ -> Error (`Msg "Invalid from color"))
  | "via" :: rest -> (
      match Color.shade_of_strings rest with
      | Ok (color, shade) -> Ok (via_color ~shade color)
      | Error _ -> Error (`Msg "Invalid via color"))
  | "to" :: rest -> (
      match Color.shade_of_strings rest with
      | Ok (color, shade) -> Ok (to_color ~shade color)
      | Error _ -> Error (`Msg "Invalid to color"))
  | _ -> Error (`Msg "Unknown background class")
