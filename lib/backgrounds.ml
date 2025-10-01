(** Background and gradient utilities *)

open Style

(** {1 Background Utility Type} *)

type utility =
  | Bg_gradient_to of direction
  | From of Color.color * int
  | Via of Color.color * int
  | To of Color.color * int

and direction =
  | Bottom
  | Bottom_right
  | Right
  | Top_right
  | Top
  | Top_left
  | Left
  | Bottom_left

(* Gradient variables with proper @property definitions matching Tailwind v4 *)
let gradient_position_var =
  (* This is used as a placeholder/initial value in gradient stops. *)
  Var.property_default Percentage ~initial:(Pct 0.) ~universal:true
    "tw-gradient-position"

let gradient_from_var =
  Var.property_default Color ~initial:(Css.hex "#0000") "tw-gradient-from"

let gradient_via_var =
  Var.property_default Color ~initial:(Css.hex "#0000") "tw-gradient-via"

let gradient_to_var =
  Var.property_default Color ~initial:(Css.hex "#0000") "tw-gradient-to"

let gradient_stops_var =
  Var.channel ~needs_property:true Gradient_stop "tw-gradient-stops"

let gradient_via_stops_var =
  Var.channel ~needs_property:true Gradient_stop "tw-gradient-via-stops"

let gradient_from_position_var =
  Var.property_default Percentage ~initial:(Pct 0.) "tw-gradient-from-position"

let gradient_via_position_var =
  Var.property_default Percentage ~initial:(Pct 50.) "tw-gradient-via-position"

let gradient_to_position_var =
  Var.property_default Percentage ~initial:(Pct 100.) "tw-gradient-to-position"

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
  style class_name [ Css.background_image (Linear_gradient (dir_val, [])) ]

(* Legacy fixed-direction helpers removed in favor of bg_gradient_to *)

(** Helper to build gradient color class names *)
let gradient_color_class_name ~prefix ?(shade = 500) color =
  if Color.is_base_color color || Color.is_custom_color color then
    prefix ^ Color.pp color
  else String.concat "" [ prefix; Color.pp color; "-"; string_of_int shade ]

(** Common helper for gradient color utilities *)
let gradient_color ~prefix ~set_var ?(shade = 500) color =
  let class_name = gradient_color_class_name ~prefix ~shade color in
  (* Use the shared color variable from Color module *)
  let color_theme_var = Color.get_color_var color shade in
  let color_value = Color.to_css color shade in
  let d_color, color_ref = Var.binding color_theme_var color_value in

  (* Set the appropriate gradient variable *)
  let d_var, _ = Var.binding set_var (Var color_ref : Css.color) in

  (* Build variable references for gradient stops *)
  let position_ref = Var.reference gradient_position_var in
  let from_ref = Var.reference gradient_from_var in
  let from_pos_ref = Var.reference gradient_from_position_var in
  let to_ref = Var.reference gradient_to_var in
  let to_pos_ref = Var.reference gradient_to_position_var in

  (* Build the fallback gradient stop list (without via) *)
  let fallback_stops : Css.gradient_stop =
    List
      [
        Percentage (Var position_ref);
        Color_percentage (Var from_ref, Some (Var from_pos_ref), None);
        Color_percentage (Var to_ref, Some (Var to_pos_ref), None);
      ]
  in

  (* Handle via-specific logic *)
  let d_stops, d_via_stops_opt =
    match prefix with
    | "via-" ->
        (* For via, build complete stop list with via in the middle *)
        let via_ref = Var.reference gradient_via_var in
        let via_pos_ref = Var.reference gradient_via_position_var in
        let via_stop_list : Css.gradient_stop =
          List
            [
              Percentage (Var position_ref);
              Color_percentage (Var from_ref, Some (Var from_pos_ref), None);
              Color_percentage (Var via_ref, Some (Var via_pos_ref), None);
              Color_percentage (Var to_ref, Some (Var to_pos_ref), None);
            ]
        in
        let d_via_stops, via_stops_ref =
          Var.binding gradient_via_stops_var via_stop_list
        in
        let d_stops_via, _ =
          Var.binding gradient_stops_var (Var via_stops_ref)
        in
        (d_stops_via, Some d_via_stops)
    | _ ->
        (* For from/to, reference via-stops with fallback *)
        let via_stops_ref =
          Var.reference_with_fallback gradient_via_stops_var fallback_stops
        in
        let d_stops, _ = Var.binding gradient_stops_var (Var via_stops_ref) in
        (d_stops, None)
  in

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

  (* Build declarations list *)
  let declarations =
    match d_via_stops_opt with
    | Some d_via_stops -> [ d_color; d_var; d_via_stops; d_stops ]
    | None -> [ d_color; d_var; d_stops ]
  in

  style class_name ~property_rules declarations

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
  gradient_color ~prefix:"from-" ~set_var:gradient_from_var ~shade color

let via_color ?(shade = 500) color =
  gradient_color ~prefix:"via-" ~set_var:gradient_via_var ~shade color

let to_color ?(shade = 500) color =
  gradient_color ~prefix:"to-" ~set_var:gradient_to_var ~shade color

let of_string = function
  | [ "bg"; "gradient"; "to"; dir ] -> (
      match dir with
      | "b" -> Ok (Bg_gradient_to Bottom)
      | "br" -> Ok (Bg_gradient_to Bottom_right)
      | "r" -> Ok (Bg_gradient_to Right)
      | "tr" -> Ok (Bg_gradient_to Top_right)
      | "t" -> Ok (Bg_gradient_to Top)
      | "tl" -> Ok (Bg_gradient_to Top_left)
      | "l" -> Ok (Bg_gradient_to Left)
      | "bl" -> Ok (Bg_gradient_to Bottom_left)
      | _ -> Error (`Msg "Unknown gradient direction"))
  | "from" :: rest -> (
      match Color.shade_of_strings rest with
      | Ok (color, shade) -> Ok (From (color, shade))
      | Error _ -> Error (`Msg "Invalid from color"))
  | "via" :: rest -> (
      match Color.shade_of_strings rest with
      | Ok (color, shade) -> Ok (Via (color, shade))
      | Error _ -> Error (`Msg "Invalid via color"))
  | "to" :: rest -> (
      match Color.shade_of_strings rest with
      | Ok (color, shade) -> Ok (To (color, shade))
      | Error _ -> Error (`Msg "Invalid to color"))
  | _ -> Error (`Msg "Unknown background class")

(** {1 Utility Conversion Functions} *)

let to_style = function
  | Bg_gradient_to dir -> bg_gradient_to dir
  | From (color, shade) -> from_color ~shade color
  | Via (color, shade) -> via_color ~shade color
  | To (color, shade) -> to_color ~shade color

let suborder = function
  | Bg_gradient_to _ -> 0
  | From (color, shade) ->
      1000
      + Color.suborder_with_shade (Color.pp color ^ "-" ^ string_of_int shade)
  | Via (color, shade) ->
      2000
      + Color.suborder_with_shade (Color.pp color ^ "-" ^ string_of_int shade)
  | To (color, shade) ->
      3000
      + Color.suborder_with_shade (Color.pp color ^ "-" ^ string_of_int shade)
