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

  (* Single source of truth: (touch-action value, class_suffix, style_fn) *)
  (* Alphabetically ordered - suborder derived from position *)
  let touch_data : (Css.touch_action * string * (unit -> Style.t)) list =
    [
      (Css.Auto, "auto", fun () -> style [ touch_action Css.Auto ]);
      ( Css.Manipulation,
        "manipulation",
        fun () -> style [ touch_action Css.Manipulation ] );
      (Css.None, "none", fun () -> style [ touch_action Css.None ]);
      (* x-axis pan utilities come before y-axis *)
      ( Css.Pan_left,
        "pan-left",
        fun () -> composable_style tw_pan_x_var Css.Pan_left );
      ( Css.Pan_right,
        "pan-right",
        fun () -> composable_style tw_pan_x_var Css.Pan_right );
      (Css.Pan_x, "pan-x", fun () -> composable_style tw_pan_x_var Css.Pan_x);
      ( Css.Pan_down,
        "pan-down",
        fun () -> composable_style tw_pan_y_var Css.Pan_down );
      (Css.Pan_up, "pan-up", fun () -> composable_style tw_pan_y_var Css.Pan_up);
      (Css.Pan_y, "pan-y", fun () -> composable_style tw_pan_y_var Css.Pan_y);
      ( Css.Pinch_zoom,
        "pinch-zoom",
        fun () -> composable_style tw_pinch_zoom_var Css.Pinch_zoom );
    ]

  (* Derived lookup tables *)
  let to_class_map =
    List.map (fun (v, suffix, _) -> (v, "touch-" ^ suffix)) touch_data

  let to_style_map = List.map (fun (v, _, style_fn) -> (v, style_fn)) touch_data
  let suborder_map = List.mapi (fun i (v, _, _) -> (v, i)) touch_data

  let of_class_map =
    List.map (fun (v, suffix, _) -> ("touch-" ^ suffix, Action v)) touch_data

  (* Handler functions derived from maps *)
  let to_class (Action v) = List.assoc v to_class_map
  let to_style _theme (Action v) = (List.assoc v to_style_map) ()
  let suborder (Action v) = List.assoc v suborder_map

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
