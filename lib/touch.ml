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

  (* Helper for composable touch styles *)
  let composable_style var value =
    let decl, _ = Var.binding var value in
    style ~property_rules:touch_props [ decl; composable_touch_action () ]

  (* Single source of truth: (handler, class_suffix, style_fn) *)
  (* Alphabetically ordered - suborder derived from position *)
  let touch_data =
    [
      (Touch_auto, "auto", fun () -> style [ touch_action Auto ]);
      ( Touch_manipulation,
        "manipulation",
        fun () -> style [ touch_action Manipulation ] );
      (Touch_none, "none", fun () -> style [ touch_action None ]);
      (* x-axis pan utilities come before y-axis *)
      ( Touch_pan_left,
        "pan-left",
        fun () -> composable_style tw_pan_x_var Pan_left );
      ( Touch_pan_right,
        "pan-right",
        fun () -> composable_style tw_pan_x_var Pan_right );
      (Touch_pan_x, "pan-x", fun () -> composable_style tw_pan_x_var Pan_x);
      ( Touch_pan_down,
        "pan-down",
        fun () -> composable_style tw_pan_y_var Pan_down );
      (Touch_pan_up, "pan-up", fun () -> composable_style tw_pan_y_var Pan_up);
      (Touch_pan_y, "pan-y", fun () -> composable_style tw_pan_y_var Pan_y);
      ( Touch_pinch_zoom,
        "pinch-zoom",
        fun () -> composable_style tw_pinch_zoom_var Pinch_zoom );
    ]

  (* Derived lookup tables *)
  let to_class_map =
    List.map (fun (t, suffix, _) -> (t, "touch-" ^ suffix)) touch_data

  let to_style_map = List.map (fun (t, _, style_fn) -> (t, style_fn)) touch_data
  let suborder_map = List.mapi (fun i (t, _, _) -> (t, i)) touch_data

  let of_class_map =
    List.map (fun (t, suffix, _) -> ("touch-" ^ suffix, t)) touch_data

  (* Handler functions derived from maps *)
  let to_class t = List.assoc t to_class_map
  let to_style t = (List.assoc t to_style_map) ()
  let suborder t = List.assoc t suborder_map

  let of_class cls =
    match List.assoc_opt cls of_class_map with
    | Some t -> Ok t
    | None -> Error (`Msg "Not a touch-action utility")
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
