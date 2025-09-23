(** Background and gradient utilities *)

open Core

(* Extend variable kinds for backgrounds and gradients *)
type _ Var.kind +=
  | (* Gradient variables *)
      Gradient_from :
      Css.color Var.kind
  | Gradient_via : Css.color Var.kind
  | Gradient_to : Css.color Var.kind

(* Gradient variables using new API - no values, just definitions *)
let gradient_from_var =
  Var.create Gradient_from "tw-gradient-from" ~layer:Utility
    ~fallback:(Css.hex "#0000")
  |> Var.with_property ~syntax:Css.Color ~initial:(Css.hex "#0000")

let gradient_via_var =
  Var.create Gradient_via "tw-gradient-via" ~layer:Utility
    ~fallback:(Css.hex "#0000")
  |> Var.with_property ~syntax:Css.Color ~initial:(Css.hex "#0000")

let gradient_to_var =
  Var.create Gradient_to "tw-gradient-to" ~layer:Utility
    ~fallback:(Css.hex "#0000")
  |> Var.with_property ~syntax:Css.Color ~initial:(Css.hex "#0000")

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
  style class_name
    ~vars:
      [
        Binding (gradient_from_var, Css.hex "#0000");
        Binding (gradient_to_var, Css.hex "#0000");
      ]
    [
      Css.background_image
        (Linear_gradient
           ( dir_val,
             [ Var (Var.use gradient_from_var); Var (Var.use gradient_to_var) ]
           ));
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
  (* Create color theme variable *)
  let color_name = Color.pp color in
  let color_theme_var =
    Var.create
      (Var.Color (color_name, Some shade))
      (String.concat "-" [ "color"; color_name; string_of_int shade ])
      ~layer:Theme
      ~order:(Color.color_order color shade)
  in
  let color_value = Color.to_css color shade in
  (* Simply set the gradient-from color - composition happens in
     background-image *)
  style class_name
    ~vars:
      [
        Binding (color_theme_var, color_value);
        Binding (gradient_from_var, Css.Var (Var.use color_theme_var));
      ]
    []

let via_color ?(shade = 500) color =
  let class_name = gradient_color_class_name ~prefix:"via-" ~shade color in
  (* Create color theme variable *)
  let color_name = Color.pp color in
  let color_theme_var =
    Var.create
      (Var.Color (color_name, Some shade))
      (String.concat "-" [ "color"; color_name; string_of_int shade ])
      ~layer:Theme
      ~order:(Color.color_order color shade)
  in
  let color_value = Color.to_css color shade in
  (* Simply set the gradient-via color - composition happens in
     background-image *)
  style class_name
    ~vars:
      [
        Binding (color_theme_var, color_value);
        Binding (gradient_via_var, Css.Var (Var.use color_theme_var));
      ]
    []

let to_color ?(shade = 500) color =
  let class_name = gradient_color_class_name ~prefix:"to-" ~shade color in
  (* Create color theme variable *)
  let color_name = Color.pp color in
  let color_theme_var =
    Var.create
      (Var.Color (color_name, Some shade))
      (String.concat "-" [ "color"; color_name; string_of_int shade ])
      ~layer:Theme
      ~order:(Color.color_order color shade)
  in
  let color_value = Color.to_css color shade in
  (* Simply set the gradient-to color - composition happens in
     background-image *)
  style class_name
    ~vars:
      [
        Binding (color_theme_var, color_value);
        Binding (gradient_to_var, Css.Var (Var.use color_theme_var));
      ]
    []

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
