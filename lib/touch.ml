(** Touch-action utilities

    @see <https://tailwindcss.com/docs/touch-action>
      Tailwind CSS Touch Action documentation *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  (* The ten values a [touch-*] class names, spelled as the utility's own type
     rather than as a [Css.touch_action]. A composed list, a CSS-wide keyword
     and a [var()] are all touch-action values and none of them is a utility, so
     carrying the CSS type here would leave [data] partial and [to_class]
     raising on a value it was handed. *)
  type t =
    | Auto
    | Manipulation
    | No_action
    | Pan_left
    | Pan_right
    | Pan_x
    | Pan_down
    | Pan_up
    | Pan_y
    | Pinch_zoom

  type Utility.base += Self of t

  let name = "touch"
  let priority _ = 14

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

  (* Helper for composable touch styles *)
  let composable_style var value =
    let decl, _ = Var.binding var value in
    style ~property_rules:touch_props [ decl; composable_touch_action () ]

  (* Class suffix, style and cascade suborder of one utility, alphabetical
     except that the x-axis pans come before the y-axis ones. One match covers
     all three, so a constructor added to [t] without an entry here is a compile
     error. *)
  let data : t -> string * Style.t * int = function
    | Auto -> ("auto", style [ touch_action Css.Auto ], 0)
    | Manipulation ->
        ("manipulation", style [ touch_action Css.Manipulation ], 1)
    | No_action -> ("none", style [ touch_action Css.None ], 2)
    | Pan_left -> ("pan-left", composable_style tw_pan_x_var Css.Pan_left, 3)
    | Pan_right -> ("pan-right", composable_style tw_pan_x_var Css.Pan_right, 4)
    | Pan_x -> ("pan-x", composable_style tw_pan_x_var Css.Pan_x, 5)
    | Pan_down -> ("pan-down", composable_style tw_pan_y_var Css.Pan_down, 6)
    | Pan_up -> ("pan-up", composable_style tw_pan_y_var Css.Pan_up, 7)
    | Pan_y -> ("pan-y", composable_style tw_pan_y_var Css.Pan_y, 8)
    | Pinch_zoom ->
        ("pinch-zoom", composable_style tw_pinch_zoom_var Css.Pinch_zoom, 9)

  (* Every value a class names, for the class-name lookup. *)
  let all =
    [
      Auto;
      Manipulation;
      No_action;
      Pan_left;
      Pan_right;
      Pan_x;
      Pan_down;
      Pan_up;
      Pan_y;
      Pinch_zoom;
    ]

  let to_class v =
    let suffix, _, _ = data v in
    "touch-" ^ suffix

  let to_style _theme v =
    let _, s, _ = data v in
    s

  let suborder v =
    let _, _, o = data v in
    o

  let of_class_map = List.map (fun v -> (to_class v, v)) all

  let of_class _theme cls =
    match List.assoc_opt cls of_class_map with
    | Some t -> Ok t
    | None -> Error (`Msg "Not a touch-action utility")

  let examples = [ Auto ]
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let touch_auto = utility Auto
let touch_none = utility No_action
let touch_manipulation = utility Manipulation
let touch_pan_x = utility Pan_x
let touch_pan_y = utility Pan_y
let touch_pan_left = utility Pan_left
let touch_pan_right = utility Pan_right
let touch_pan_up = utility Pan_up
let touch_pan_down = utility Pan_down
let touch_pinch_zoom = utility Pinch_zoom
