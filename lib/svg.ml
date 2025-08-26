open Core
open Css

(** {1 Fill Utilities} *)

let fill_none = style "fill-none" [ fill None ]
let fill_current = style "fill-current" [ fill Current_color ]

let fill color ?(shade = 500) () =
  let class_name =
    if Color.is_base_color color then Pp.str [ "fill-"; Color.to_name color ]
    else Pp.str [ "fill-"; Color.to_name color; "-"; string_of_int shade ]
  in
  let var_name =
    if Color.is_base_color color then Pp.str [ "color-"; Color.to_name color ]
    else Pp.str [ "color-"; Color.to_name color; "-"; string_of_int shade ]
  in
  let default_color =
    Color.to_css color (if Color.is_base_color color then 500 else shade)
  in
  let css_var = Css.var ~default_value:default_color var_name in
  style class_name [ fill (Color (Css.Var css_var)) ]

(** {1 Stroke Utilities} *)

let stroke_none = style "stroke-none" [ stroke None ]
let stroke_current = style "stroke-current" [ stroke Current_color ]

let stroke color ?(shade = 500) () =
  let class_name =
    if Color.is_base_color color then Pp.str [ "stroke-"; Color.to_name color ]
    else Pp.str [ "stroke-"; Color.to_name color; "-"; string_of_int shade ]
  in
  let var_name =
    if Color.is_base_color color then Pp.str [ "color-"; Color.to_name color ]
    else Pp.str [ "color-"; Color.to_name color; "-"; string_of_int shade ]
  in
  let default_color =
    Color.to_css color (if Color.is_base_color color then 500 else shade)
  in
  let css_var = Css.var ~default_value:default_color var_name in
  style class_name [ stroke (Color (Css.Var css_var)) ]

(** {1 Stroke Width Utilities} *)

let stroke_0 = style "stroke-0" [ stroke_width (Px 0) ]
let stroke_1 = style "stroke-1" [ stroke_width (Px 1) ]
let stroke_2 = style "stroke-2" [ stroke_width (Px 2) ]

let stroke_width n =
  style (Pp.str [ "stroke-"; string_of_int n ]) [ stroke_width (Px n) ]

(** {1 Parsing Functions} *)

let of_string = function
  | [ "fill"; "none" ] -> Ok fill_none
  | [ "fill"; "current" ] -> Ok fill_current
  | [ "stroke"; "none" ] -> Ok stroke_none
  | [ "stroke"; "current" ] -> Ok stroke_current
  | [ "stroke"; "0" ] -> Ok stroke_0
  | [ "stroke"; "1" ] -> Ok stroke_1
  | [ "stroke"; "2" ] -> Ok stroke_2
  | [ "stroke"; n ] -> (
      match int_of_string_opt n with
      | Some width -> Ok (stroke_width width)
      | None -> Error (`Msg (Pp.str [ "Invalid stroke width: "; n ])))
  | parts ->
      Error (`Msg (Pp.str [ "Unknown SVG utility: "; String.concat "-" parts ]))
