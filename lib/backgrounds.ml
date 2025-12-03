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

  (* Convert direction to Tailwind's gradient position string format e.g., "to
     right in oklab" *)
  let direction_to_position_string (dir : direction) : string =
    let dir_str =
      match dir with
      | Bottom -> "to bottom"
      | Bottom_right -> "to bottom right"
      | Right -> "to right"
      | Top_right -> "to top right"
      | Top -> "to top"
      | Top_left -> "to top left"
      | Left -> "to left"
      | Bottom_left -> "to bottom left"
    in
    dir_str ^ " in oklab"

  open Style
  open Css

  let name = "backgrounds"
  let priority = 18

  (* Gradient variables with proper @property definitions matching Tailwind
     v4 *)
  (* --tw-gradient-position stores the gradient direction + color space
     e.g., "to right in oklab" *)
  let gradient_position_var =
    Var.channel ~needs_property:true String "tw-gradient-position"

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
    Var.property_default Percentage ~initial:(Pct 0.)
      "tw-gradient-from-position"

  let gradient_via_position_var =
    Var.property_default Percentage ~initial:(Pct 50.)
      "tw-gradient-via-position"

  let gradient_to_position_var =
    Var.property_default Percentage ~initial:(Pct 100.)
      "tw-gradient-to-position"

  let bg_gradient_to' dir =
    (* Set --tw-gradient-position with direction string (e.g., "to right in
       oklab") *)
    let position_str = direction_to_position_string dir in
    let d_position, _ = Var.binding gradient_position_var position_str in
    (* Create a reference to --tw-gradient-stops for the linear-gradient *)
    let stops_ref : Css.gradient_stop Css.var =
      Css.var_ref "tw-gradient-stops"
    in
    style [ d_position; Css.background_image (Linear_gradient_var stops_ref) ]

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

  (** Build the raw CSS string for gradient stops without via. Format:
      var(--tw-gradient-position),var(--tw-gradient-from)var(--tw-gradient-from-position),var(--tw-gradient-to)var(--tw-gradient-to-position)
      Note: No spaces - matches Tailwind's minified output format *)
  let fallback_stops_string =
    "var(--tw-gradient-position),var(--tw-gradient-from)var(--tw-gradient-from-position),var(--tw-gradient-to)var(--tw-gradient-to-position)"

  (** Build the raw CSS string for gradient stops with via. Format:
      var(--tw-gradient-position),var(--tw-gradient-from)var(--tw-gradient-from-position),var(--tw-gradient-via)var(--tw-gradient-via-position),var(--tw-gradient-to)var(--tw-gradient-to-position)
      Note: No spaces - matches Tailwind's minified output format *)
  let via_stops_string =
    "var(--tw-gradient-position),var(--tw-gradient-from)var(--tw-gradient-from-position),var(--tw-gradient-via)var(--tw-gradient-via-position),var(--tw-gradient-to)var(--tw-gradient-to-position)"

  (** Common helper for gradient color utilities *)
  let gradient_color ~prefix ~set_var ?(shade = 500) color =
    let theme_decls, gradient_color_value = color_binding ~shade color in

    (* Set the appropriate gradient variable *)
    let d_var, _ = Var.binding set_var gradient_color_value in

    (* Handle via-specific logic *)
    let d_stops, d_via_stops_opt =
      match prefix with
      | "via-" ->
          (* For via: --tw-gradient-via-stops: <via_stops_string>;
             --tw-gradient-stops: var(--tw-gradient-via-stops); *)
          let via_stops_value : Css.gradient_stop = Raw via_stops_string in
          let d_via_stops, via_stops_ref =
            Var.binding gradient_via_stops_var via_stops_value
          in
          let d_stops_via, _ =
            Var.binding gradient_stops_var (Var via_stops_ref)
          in
          (d_stops_via, Some d_via_stops)
      | _ ->
          (* For from/to: --tw-gradient-stops: var(--tw-gradient-via-stops,
             <fallback_stops_string>); *)
          let fallback_value : Css.gradient_stop = Raw fallback_stops_string in
          let via_stops_ref =
            Var.reference_with_fallback gradient_via_stops_var fallback_value
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
