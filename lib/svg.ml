(** SVG utilities for fill, stroke, and stroke-width. *)

(** Error helper *)
let err_not_utility = Error (`Msg "Not an SVG utility")

module Handler = struct
  open Style

  type t =
    | Fill_none
    | Fill_current
    | Stroke_none
    | Stroke_current
    | Stroke_0
    | Stroke_1
    | Stroke_2
    | Stroke_width of int

  type Utility.base += Self of t

  let name = "svg"
  let priority = 28

  let to_style = function
    | Fill_none -> style Css.[ fill None ]
    | Fill_current -> style Css.[ fill Current_color ]
    | Stroke_none -> style Css.[ stroke None ]
    | Stroke_current -> style Css.[ stroke Current_color ]
    | Stroke_0 -> style Css.[ stroke_width (Px 0.) ]
    | Stroke_1 -> style Css.[ stroke_width (Px 1.) ]
    | Stroke_2 -> style Css.[ stroke_width (Px 2.) ]
    | Stroke_width n -> style Css.[ stroke_width (Px (float_of_int n)) ]

  let suborder = function
    | Fill_none -> 0
    | Fill_current -> 1
    | Stroke_none -> 2
    | Stroke_current -> 3
    | Stroke_0 -> 4
    | Stroke_1 -> 5
    | Stroke_2 -> 6
    | Stroke_width n -> 10 + n

  let to_class = function
    | Fill_none -> "fill-none"
    | Fill_current -> "fill-current"
    | Stroke_none -> "stroke-none"
    | Stroke_current -> "stroke-current"
    | Stroke_0 -> "stroke-0"
    | Stroke_1 -> "stroke-1"
    | Stroke_2 -> "stroke-2"
    | Stroke_width n -> "stroke-" ^ string_of_int n

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
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
end

let () = Utility.register (module Handler)

open Handler

let color_util property color ?(shade = 500) () =
  let var_name =
    if Color.is_base_color color then "color-" ^ Color.to_name color
    else "color-" ^ Color.to_name color ^ "-" ^ string_of_int shade
  in
  let typed_color =
    Color.to_css color (if Color.is_base_color color then 500 else shade)
  in
  let def, css_var = Css.var var_name Css.Color typed_color in
  Style.style [ def; property (Css.Color (Css.Var css_var) : Css.svg_paint) ]

let utility x = Utility.base (Self x)
let fill = color_util Css.fill
let stroke = color_util Css.stroke
let fill_none = utility Fill_none
let fill_current = utility Fill_current
let stroke_none = utility Stroke_none
let stroke_current = utility Stroke_current
let stroke_0 = utility Stroke_0
let stroke_1 = utility Stroke_1
let stroke_2 = utility Stroke_2
let stroke_width n = utility (Stroke_width n)
