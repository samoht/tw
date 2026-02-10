(** Touch-action utilities

    @see <https://tailwindcss.com/docs/touch-action>
      Tailwind CSS Touch Action documentation *)

module Handler = struct
  open Style
  open Css

  type t =
    | Touch_auto
    | Touch_none
    | Touch_manipulation
    | Touch_pan_x
    | Touch_pan_y
    | Touch_pan_left
    | Touch_pan_right
    | Touch_pan_up
    | Touch_pan_down
    | Touch_pinch_zoom

  type Utility.base += Self of t

  let name = "touch"
  let priority = 12

  (* CSS Variables for composable touch-action values *)
  let tw_pan_x_var =
    Var.channel ~needs_property:true ~property_order:50 Touch_action "tw-pan-x"

  let tw_pan_y_var =
    Var.channel ~needs_property:true ~property_order:51 Touch_action "tw-pan-y"

  let tw_pinch_zoom_var =
    Var.channel ~needs_property:true ~property_order:52 Touch_action
      "tw-pinch-zoom"

  let touch_props =
    List.filter_map Var.property_rule
      [ tw_pan_x_var; tw_pan_y_var; tw_pinch_zoom_var ]
    |> concat

  (* Create a touch-action value that references all three vars with empty
     fallbacks *)
  let composable_touch_action () =
    let pan_x_ref = Var.reference_with_empty_fallback tw_pan_x_var in
    let pan_y_ref = Var.reference_with_empty_fallback tw_pan_y_var in
    let pinch_zoom_ref = Var.reference_with_empty_fallback tw_pinch_zoom_var in
    touch_action (Vars [ pan_x_ref; pan_y_ref; pinch_zoom_ref ])

  let to_class = function
    | Touch_auto -> "touch-auto"
    | Touch_none -> "touch-none"
    | Touch_manipulation -> "touch-manipulation"
    | Touch_pan_x -> "touch-pan-x"
    | Touch_pan_y -> "touch-pan-y"
    | Touch_pan_left -> "touch-pan-left"
    | Touch_pan_right -> "touch-pan-right"
    | Touch_pan_up -> "touch-pan-up"
    | Touch_pan_down -> "touch-pan-down"
    | Touch_pinch_zoom -> "touch-pinch-zoom"

  let to_style = function
    | Touch_auto -> style [ touch_action Auto ]
    | Touch_none -> style [ touch_action None ]
    | Touch_manipulation -> style [ touch_action Manipulation ]
    | Touch_pan_x ->
        let decl, _ = Var.binding tw_pan_x_var Pan_x in
        style ~property_rules:touch_props [ decl; composable_touch_action () ]
    | Touch_pan_y ->
        let decl, _ = Var.binding tw_pan_y_var Pan_y in
        style ~property_rules:touch_props [ decl; composable_touch_action () ]
    | Touch_pan_left ->
        let decl, _ = Var.binding tw_pan_x_var Pan_left in
        style ~property_rules:touch_props [ decl; composable_touch_action () ]
    | Touch_pan_right ->
        let decl, _ = Var.binding tw_pan_x_var Pan_right in
        style ~property_rules:touch_props [ decl; composable_touch_action () ]
    | Touch_pan_up ->
        let decl, _ = Var.binding tw_pan_y_var Pan_up in
        style ~property_rules:touch_props [ decl; composable_touch_action () ]
    | Touch_pan_down ->
        let decl, _ = Var.binding tw_pan_y_var Pan_down in
        style ~property_rules:touch_props [ decl; composable_touch_action () ]
    | Touch_pinch_zoom ->
        let decl, _ = Var.binding tw_pinch_zoom_var Pinch_zoom in
        style ~property_rules:touch_props [ decl; composable_touch_action () ]

  let suborder = function
    | Touch_auto -> 0
    | Touch_none -> 1
    | Touch_manipulation -> 2
    | Touch_pan_x -> 3
    | Touch_pan_y -> 4
    | Touch_pan_left -> 5
    | Touch_pan_right -> 6
    | Touch_pan_up -> 7
    | Touch_pan_down -> 8
    | Touch_pinch_zoom -> 9

  let of_class class_name =
    match class_name with
    | "touch-auto" -> Ok Touch_auto
    | "touch-none" -> Ok Touch_none
    | "touch-manipulation" -> Ok Touch_manipulation
    | "touch-pan-x" -> Ok Touch_pan_x
    | "touch-pan-y" -> Ok Touch_pan_y
    | "touch-pan-left" -> Ok Touch_pan_left
    | "touch-pan-right" -> Ok Touch_pan_right
    | "touch-pan-up" -> Ok Touch_pan_up
    | "touch-pan-down" -> Ok Touch_pan_down
    | "touch-pinch-zoom" -> Ok Touch_pinch_zoom
    | _ -> Error (`Msg "Not a touch-action utility")
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let touch_auto = utility Touch_auto
let touch_none = utility Touch_none
let touch_manipulation = utility Touch_manipulation
let touch_pan_x = utility Touch_pan_x
let touch_pan_y = utility Touch_pan_y
let touch_pan_left = utility Touch_pan_left
let touch_pan_right = utility Touch_pan_right
let touch_pan_up = utility Touch_pan_up
let touch_pan_down = utility Touch_pan_down
let touch_pinch_zoom = utility Touch_pinch_zoom
