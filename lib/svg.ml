open Style
open Css

(** {1 SVG Utility Type} *)

type utility =
  | Fill_none
  | Fill_current
  | Stroke_none
  | Stroke_current
  | Stroke_0
  | Stroke_1
  | Stroke_2
  | Stroke_width of int

(** Helper to create SVG color utilities *)
let color_util prefix property color ?(shade = 500) () =
  let class_name =
    if Color.is_base_color color then prefix ^ "-" ^ Color.to_name color
    else prefix ^ "-" ^ Color.to_name color ^ "-" ^ string_of_int shade
  in
  let var_name =
    if Color.is_base_color color then "color-" ^ Color.to_name color
    else "color-" ^ Color.to_name color ^ "-" ^ string_of_int shade
  in
  let typed_color =
    Color.to_css color (if Color.is_base_color color then 500 else shade)
  in
  let def, css_var = Css.var var_name Css.Color typed_color in
  style class_name [ def; property (Css.Color (Var css_var) : Css.svg_paint) ]

(** {1 Fill Utilities} *)

let fill_none = style "fill-none" [ fill None ]
let fill_current = style "fill-current" [ fill Current_color ]
let fill = color_util "fill" fill

(** {1 Stroke Utilities} *)

let stroke_none = style "stroke-none" [ stroke None ]
let stroke_current = style "stroke-current" [ stroke Current_color ]
let stroke = color_util "stroke" stroke

(** {1 Stroke Width Utilities} *)

let stroke_0 = style "stroke-0" [ stroke_width (Px 0.) ]
let stroke_1 = style "stroke-1" [ stroke_width (Px 1.) ]
let stroke_2 = style "stroke-2" [ stroke_width (Px 2.) ]

let stroke_width n =
  style
    (String.concat "" [ "stroke-"; string_of_int n ])
    [ stroke_width (Px (float_of_int n)) ]

(** {1 Utility Conversion Functions} *)

let to_style = function
  | Fill_none -> fill_none
  | Fill_current -> fill_current
  | Stroke_none -> stroke_none
  | Stroke_current -> stroke_current
  | Stroke_0 -> stroke_0
  | Stroke_1 -> stroke_1
  | Stroke_2 -> stroke_2
  | Stroke_width n -> stroke_width n

let of_string = function
  | [ "fill"; "none" ] -> Ok Fill_none
  | [ "fill"; "current" ] -> Ok Fill_current
  | [ "stroke"; "none" ] -> Ok Stroke_none
  | [ "stroke"; "current" ] -> Ok Stroke_current
  | [ "stroke"; "0" ] -> Ok Stroke_0
  | [ "stroke"; "1" ] -> Ok Stroke_1
  | [ "stroke"; "2" ] -> Ok Stroke_2
  | [ "stroke"; n ] -> (
      match int_of_string_opt n with
      | Some width -> Ok (Stroke_width width)
      | None -> Error (`Msg ("Invalid stroke width: " ^ n)))
  | _ -> Error (`Msg "Not an SVG utility")

let suborder = function
  | Fill_none -> 0
  | Fill_current -> 1
  | Stroke_none -> 2
  | Stroke_current -> 3
  | Stroke_0 -> 4
  | Stroke_1 -> 5
  | Stroke_2 -> 6
  | Stroke_width n -> 10 + n
