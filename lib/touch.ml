(** Touch-action utilities

    @see <https://tailwindcss.com/docs/touch-action>
      Tailwind CSS Touch Action documentation *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t = Action of Css.touch_action
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
     except that the x-axis pans come before the y-axis ones. Written as a match
     rather than a lookup table: a constructor added to [Css.touch_action]
     without an entry here is a compile error, not a [Not_found] raised out of
     [to_class] partway through rendering a sheet. The CSS-wide keywords and the
     composed and var() forms are spelled out for the same reason. *)
  let data : Css.touch_action -> string * Style.t * int = function
    | Css.Auto -> ("auto", style [ touch_action Css.Auto ], 0)
    | Css.Manipulation ->
        ("manipulation", style [ touch_action Css.Manipulation ], 1)
    | Css.None -> ("none", style [ touch_action Css.None ], 2)
    | Css.Pan_left -> ("pan-left", composable_style tw_pan_x_var Css.Pan_left, 3)
    | Css.Pan_right ->
        ("pan-right", composable_style tw_pan_x_var Css.Pan_right, 4)
    | Css.Pan_x -> ("pan-x", composable_style tw_pan_x_var Css.Pan_x, 5)
    | Css.Pan_down -> ("pan-down", composable_style tw_pan_y_var Css.Pan_down, 6)
    | Css.Pan_up -> ("pan-up", composable_style tw_pan_y_var Css.Pan_up, 7)
    | Css.Pan_y -> ("pan-y", composable_style tw_pan_y_var Css.Pan_y, 8)
    | Css.Pinch_zoom ->
        ("pinch-zoom", composable_style tw_pinch_zoom_var Css.Pinch_zoom, 9)
    | Css.Actions _ | Css.Inherit | Css.Initial | Css.Unset | Css.Revert
    | Css.Revert_layer | Css.Vars _ | Css.Var _ ->
        (* No touch class names a composed list, a CSS-wide keyword or a var(),
           so [of_class] never builds one. *)
        invalid_arg "touch: value has no class name"

  (* Every value a class names, for the class-name lookup. *)
  let all : Css.touch_action list =
    [
      Css.Auto;
      Css.Manipulation;
      Css.None;
      Css.Pan_left;
      Css.Pan_right;
      Css.Pan_x;
      Css.Pan_down;
      Css.Pan_up;
      Css.Pan_y;
      Css.Pinch_zoom;
    ]

  let to_class (Action v) =
    let suffix, _, _ = data v in
    "touch-" ^ suffix

  let to_style _theme (Action v) =
    let _, s, _ = data v in
    s

  let suborder (Action v) =
    let _, _, o = data v in
    o

  let of_class_map = List.map (fun v -> (to_class (Action v), Action v)) all

  let of_class _theme cls =
    match List.assoc_opt cls of_class_map with
    | Some t -> Ok t
    | None -> Error (`Msg "Not a touch-action utility")

  let examples = [ Action Css.Auto ]
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let touch_auto = utility (Action Css.Auto)
let touch_none = utility (Action Css.None)
let touch_manipulation = utility (Action Css.Manipulation)
let touch_pan_x = utility (Action Css.Pan_x)
let touch_pan_y = utility (Action Css.Pan_y)
let touch_pan_left = utility (Action Css.Pan_left)
let touch_pan_right = utility (Action Css.Pan_right)
let touch_pan_up = utility (Action Css.Pan_up)
let touch_pan_down = utility (Action Css.Pan_down)
let touch_pinch_zoom = utility (Action Css.Pinch_zoom)
