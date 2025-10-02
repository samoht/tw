(** SVG utilities for fill, stroke, and stroke-width

    What's included:
    - Fill: `fill-none`, `fill-current`.
    - Stroke: `stroke-none`, `stroke-current`.
    - Stroke width: `stroke-0`, `stroke-1`, `stroke-2`, `stroke-*`.

    What's not:
    - SVG color utilities (handled separately).
    - Advanced stroke properties like stroke-dasharray, stroke-linecap.

    Parsing contract (`of_string`):
    - Accepts tokens like ["fill"; "none"], ["fill"; "current"],
      ["stroke"; "none"], ["stroke"; n].
    - Unknown tokens yield `Error (`Msg "Not an SVG utility")`. *)

open Style
open Css

(** Local SVG utility type *)
type t =
  | Fill_none
  | Fill_current
  | Stroke_none
  | Stroke_current
  | Stroke_0
  | Stroke_1
  | Stroke_2
  | Stroke_width of int

(** Extensible variant for SVG utilities *)
type Utility.base += Svg of t

(** Error helper *)
let err_not_utility = Error (`Msg "Not an SVG utility")

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

(** Fill utility helper *)
let fill = color_util "fill" fill

(** Stroke utility helper *)
let stroke = color_util "stroke" stroke

(** Conversion functions *)
let to_style = function
  | Fill_none -> style "fill-none" Css.[ fill None ]
  | Fill_current -> style "fill-current" Css.[ fill Current_color ]
  | Stroke_none -> style "stroke-none" Css.[ stroke None ]
  | Stroke_current -> style "stroke-current" Css.[ stroke Current_color ]
  | Stroke_0 -> style "stroke-0" Css.[ stroke_width (Px 0.) ]
  | Stroke_1 -> style "stroke-1" Css.[ stroke_width (Px 1.) ]
  | Stroke_2 -> style "stroke-2" Css.[ stroke_width (Px 2.) ]
  | Stroke_width n ->
      style
        (String.concat "" [ "stroke-"; string_of_int n ])
        Css.[ stroke_width (Px (float_of_int n)) ]

let suborder = function
  | Fill_none -> 0
  | Fill_current -> 1
  | Stroke_none -> 2
  | Stroke_current -> 3
  | Stroke_0 -> 4
  | Stroke_1 -> 5
  | Stroke_2 -> 6
  | Stroke_width n -> 10 + n

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
      | None -> err_not_utility)
  | _ -> err_not_utility

(** Priority for SVG utilities *)
let priority = 800

(** Typed handler *)
let handler : t Utility.handler = { to_style; priority; suborder; of_string }

(** Wrapper functions for extensible variant *)
let wrap x = Svg x

let unwrap = function Svg x -> Some x | _ -> None

(** Public API *)
let utility x = Utility.base (Svg x)

let fill_none = utility Fill_none
let fill_current = utility Fill_current
let stroke_none = utility Stroke_none
let stroke_current = utility Stroke_current
let stroke_0 = utility Stroke_0
let stroke_1 = utility Stroke_1
let stroke_2 = utility Stroke_2
let stroke_width n = utility (Stroke_width n)

(** Register handler with Utility system *)
let () = Utility.register ~wrap ~unwrap handler
