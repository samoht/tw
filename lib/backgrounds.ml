(** Background and gradient utilities

    What's included:
    - `bg-gradient-to-*` - Linear gradient direction utilities.
    - `from-*` - Gradient starting color with optional opacity.
    - `via-*` - Gradient middle color with optional opacity.
    - `to-*` - Gradient ending color with optional opacity.

    What's not:
    - Radial or conic gradients.
    - Multiple gradient stops beyond from/via/to.
    - Background size, position, repeat utilities.

    Parsing contract (`of_string`):
    - Accepts ["bg"; "gradient"; "to"; direction], ["from"; color; shade],
      ["via"; color; shade], ["to"; color; shade]. Unknown tokens yield `Error
      (`Msg "Unknown background class")`. *)

type direction =
  | Bottom
  | Bottom_right
  | Right
  | Top_right
  | Top
  | Top_left
  | Left
  | Bottom_left

module Handler = struct
  type t =
    | Bg of Color.color * int
    | Bg_gradient_to of direction
    | From of Color.color * int
    | Via of Color.color * int
    | To of Color.color * int

  type Utility.base += Self of t

  let to_class (t : t) =
    match t with
    | Bg (color, shade) ->
        if Color.is_base_color color || Color.is_custom_color color then
          "bg-" ^ Color.pp color
        else "bg-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | Bg_gradient_to dir -> (
        match dir with
        | Bottom -> "bg-gradient-to-b"
        | Bottom_right -> "bg-gradient-to-br"
        | Right -> "bg-gradient-to-r"
        | Top_right -> "bg-gradient-to-tr"
        | Top -> "bg-gradient-to-t"
        | Top_left -> "bg-gradient-to-tl"
        | Left -> "bg-gradient-to-l"
        | Bottom_left -> "bg-gradient-to-bl")
    | From (color, shade) ->
        if Color.is_base_color color || Color.is_custom_color color then
          "from-" ^ Color.pp color
        else "from-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | Via (color, shade) ->
        if Color.is_base_color color || Color.is_custom_color color then
          "via-" ^ Color.pp color
        else "via-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | To (color, shade) ->
        if Color.is_base_color color || Color.is_custom_color color then
          "to-" ^ Color.pp color
        else "to-" ^ Color.pp color ^ "-" ^ string_of_int shade

  let to_spec (dir : direction) : Css.gradient_direction =
    match dir with
    | Bottom -> To_bottom
    | Bottom_right -> To_bottom_right
    | Right -> To_right
    | Top_right -> To_top_right
    | Top -> To_top
    | Top_left -> To_top_left
    | Left -> To_left
    | Bottom_left -> To_bottom_left

  open Style
  open Css

  let name = "backgrounds"
  let priority = 20

  (* Gradient variables with proper @property definitions matching Tailwind
     v4 *)
  let gradient_position_var =
    (* The gradient direction. Use To_bottom as a sentinel so @property
       initial-value is omitted, matching Tailwind's "initial" in the properties
       layer. Put before --tw-gradient-from. *)
    Var.property_default Gradient_direction ~initial:To_bottom ~universal:true
      ~property_order:60 "tw-gradient-position"

  let gradient_from_var =
    Var.property_default Color ~initial:(Css.hex "#0000") ~property_order:61
      "tw-gradient-from"

  let gradient_via_var =
    Var.property_default Color ~initial:(Css.hex "#0000") ~property_order:62
      "tw-gradient-via"

  let gradient_to_var =
    Var.property_default Color ~initial:(Css.hex "#0000") ~property_order:63
      "tw-gradient-to"

  let gradient_stops_var =
    Var.property_default Gradient_stop ~initial:(List []) ~universal:true
      ~property_order:64 "tw-gradient-stops"

  let gradient_via_stops_var =
    Var.channel ~needs_property:true ~property_order:65 Gradient_stop
      "tw-gradient-via-stops"

  let gradient_from_position_var =
    Var.property_default Percentage ~initial:(Pct 0.) ~property_order:66
      "tw-gradient-from-position"

  let gradient_via_position_var =
    Var.property_default Percentage ~initial:(Pct 50.) ~property_order:67
      "tw-gradient-via-position"

  let gradient_to_position_var =
    Var.property_default Percentage ~initial:(Pct 100.) ~property_order:68
      "tw-gradient-to-position"

  let bg_gradient_to' dir =
    (* Set --tw-gradient-position to the typed direction with oklab
       interpolation *)
    let dir_val = With_interpolation (to_spec dir, In_oklab) in
    let d_position, _ = Var.binding gradient_position_var dir_val in
    (* Reference --tw-gradient-stops for linear-gradient *)
    let stops_ref = Var.reference gradient_stops_var in
    (* Include property rules for the variables *)
    let property_rules =
      [
        Var.property_rule gradient_position_var;
        Var.property_rule gradient_stops_var;
      ]
      |> List.filter_map (fun x -> x)
      |> Css.concat
    in
    style ~property_rules
      [ d_position; Css.background_image (Linear_gradient_var stops_ref) ]

  (** Helper to get color value and optional theme variable declaration. For
      custom/arbitrary colors: returns ([], color_value) - no theme variable.
      For named colors: returns ([theme_var_decl], Var(theme_var_ref)) *)
  let color_binding ?(shade = 500) color =
    let color_value = Color.to_css color shade in
    if Color.is_custom_color color then
      (* Arbitrary color: no theme variable, use value directly *)
      ([], color_value)
    else
      (* Named color: create theme variable *)
      let color_theme_var = Color.get_color_var color shade in
      let d_color, color_ref = Var.binding color_theme_var color_value in
      ([ d_color ], (Var color_ref : Css.color))

  (** Common helper for gradient color utilities *)
  let gradient_color ~prefix ~set_var ?(shade = 500) color =
    let theme_decls, gradient_color_value = color_binding ~shade color in

    (* Set the appropriate gradient variable *)
    let d_var, _ = Var.binding set_var gradient_color_value in

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
          Direction (Var position_ref);
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
                Direction (Var position_ref);
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
    let base_declarations = theme_decls @ [ d_var ] in
    let declarations =
      match d_via_stops_opt with
      | Some d_via_stops -> base_declarations @ [ d_via_stops; d_stops ]
      | None -> base_declarations @ [ d_stops ]
    in

    style ~property_rules declarations

  let from_color' ?(shade = 500) color =
    gradient_color ~prefix:"from-" ~set_var:gradient_from_var ~shade color

  let via_color' ?(shade = 500) color =
    gradient_color ~prefix:"via-" ~set_var:gradient_via_var ~shade color

  let to_color' ?(shade = 500) color =
    gradient_color ~prefix:"to-" ~set_var:gradient_to_var ~shade color

  let bg' ?(shade = 500) color =
    let theme_decls, color_value = color_binding ~shade color in
    style (theme_decls @ [ Css.background_color color_value ])

  let to_style = function
    | Bg (color, shade) -> bg' ~shade color
    | Bg_gradient_to dir -> bg_gradient_to' dir
    | From (color, shade) -> from_color' ~shade color
    | Via (color, shade) -> via_color' ~shade color
    | To (color, shade) -> to_color' ~shade color

  let suborder = function
    | Bg (color, shade) ->
        Color.suborder_with_shade (Color.pp color ^ "-" ^ string_of_int shade)
    | Bg_gradient_to _ -> 10000
    | From (color, shade) ->
        11000
        + Color.suborder_with_shade (Color.pp color ^ "-" ^ string_of_int shade)
    | Via (color, shade) ->
        12000
        + Color.suborder_with_shade (Color.pp color ^ "-" ^ string_of_int shade)
    | To (color, shade) ->
        13000
        + Color.suborder_with_shade (Color.pp color ^ "-" ^ string_of_int shade)

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "bg"; "gradient"; "to"; "b" ] -> Ok (Bg_gradient_to Bottom)
    | [ "bg"; "gradient"; "to"; "br" ] -> Ok (Bg_gradient_to Bottom_right)
    | [ "bg"; "gradient"; "to"; "r" ] -> Ok (Bg_gradient_to Right)
    | [ "bg"; "gradient"; "to"; "tr" ] -> Ok (Bg_gradient_to Top_right)
    | [ "bg"; "gradient"; "to"; "t" ] -> Ok (Bg_gradient_to Top)
    | [ "bg"; "gradient"; "to"; "tl" ] -> Ok (Bg_gradient_to Top_left)
    | [ "bg"; "gradient"; "to"; "l" ] -> Ok (Bg_gradient_to Left)
    | [ "bg"; "gradient"; "to"; "bl" ] -> Ok (Bg_gradient_to Bottom_left)
    | "bg" :: rest -> (
        match Color.shade_of_strings rest with
        | Ok (color, shade) -> Ok (Bg (color, shade))
        | Error _ -> Error (`Msg "Invalid background color"))
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
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let bg color shade = utility (Bg (color, shade))
let bg_gradient_to dir = utility (Bg_gradient_to dir)
let from_color ?(shade = 500) color = utility (From (color, shade))
let via_color ?(shade = 500) color = utility (Via (color, shade))
let to_color ?(shade = 500) color = utility (To (color, shade))
