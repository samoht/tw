(** SVG utilities for fill, stroke, and stroke-width. *)

(** Error helper *)
let err_not_utility = Error (`Msg "Not an SVG utility")

module Handler = struct
  open Style

  type t =
    | Fill_none
    | Fill_current
    | Fill_current_opacity of Color.opacity_modifier
    | Fill_color of Color.color * int
    | Fill_color_opacity of Color.color * int * Color.opacity_modifier
    | Stroke_none
    | Stroke_current
    | Stroke_current_opacity of Color.opacity_modifier
    | Stroke_color of Color.color * int
    | Stroke_color_opacity of Color.color * int * Color.opacity_modifier
    | Stroke_0
    | Stroke_1
    | Stroke_2
    | Stroke_width of int

  type Utility.base += Self of t

  let name = "svg"
  let priority = 30

  (* Format opacity modifier for class names *)
  let opacity_suffix = function
    | Color.No_opacity -> ""
    | Color.Opacity_percent p ->
        if Float.is_integer p then Printf.sprintf "/%d" (int_of_float p)
        else Printf.sprintf "/%g" p
    | Color.Opacity_arbitrary f -> Printf.sprintf "/[%g]" f
    | Color.Opacity_named name -> "/" ^ name

  (* Fill color style with scheme support *)
  let fill_color_style color shade =
    let color_var = Color.get_color_var color shade in
    let color_value = Color.to_css color shade in
    let theme_decl, color_ref = Var.binding color_var color_value in
    style [ theme_decl; Css.fill (Css.Color (Css.Var color_ref)) ]

  (* Stroke color style with scheme support *)
  let stroke_color_style color shade =
    let color_var = Color.get_color_var color shade in
    let color_value = Color.to_css color shade in
    let theme_decl, color_ref = Var.binding color_var color_value in
    style [ theme_decl; Css.stroke (Css.Color (Css.Var color_ref)) ]

  let to_style = function
    | Fill_none -> style Css.[ fill None ]
    | Fill_current -> style Css.[ fill Current_color ]
    | Fill_current_opacity opacity -> Color.fill_current_with_opacity opacity
    | Fill_color (color, shade) -> fill_color_style color shade
    | Fill_color_opacity (color, shade, opacity) ->
        Color.fill_with_opacity color shade opacity
    | Stroke_none -> style Css.[ stroke None ]
    | Stroke_current -> style Css.[ stroke Current_color ]
    | Stroke_current_opacity opacity ->
        Color.stroke_current_with_opacity opacity
    | Stroke_color (color, shade) -> stroke_color_style color shade
    | Stroke_color_opacity (color, shade, opacity) ->
        Color.stroke_with_opacity color shade opacity
    | Stroke_0 -> style Css.[ stroke_width (Px 0.) ]
    | Stroke_1 -> style Css.[ stroke_width (Px 1.) ]
    | Stroke_2 -> style Css.[ stroke_width (Px 2.) ]
    | Stroke_width n -> style Css.[ stroke_width (Px (float_of_int n)) ]

  let suborder = function
    | Fill_none -> 0
    | Fill_current -> 1
    | Fill_current_opacity _ -> 2
    | Fill_color (color, shade) ->
        let base =
          if Color.is_base_color color then
            Color.suborder_with_shade (Color.color_to_string color)
          else
            Color.suborder_with_shade
              (Color.color_to_string color ^ "-" ^ string_of_int shade)
        in
        100 + base
    | Fill_color_opacity (color, shade, _) ->
        let base =
          if Color.is_base_color color then
            Color.suborder_with_shade (Color.color_to_string color)
          else
            Color.suborder_with_shade
              (Color.color_to_string color ^ "-" ^ string_of_int shade)
        in
        100 + base
    | Stroke_none -> 1000
    | Stroke_current -> 1001
    | Stroke_current_opacity _ -> 1002
    | Stroke_color (color, shade) ->
        let base =
          if Color.is_base_color color then
            Color.suborder_with_shade (Color.color_to_string color)
          else
            Color.suborder_with_shade
              (Color.color_to_string color ^ "-" ^ string_of_int shade)
        in
        1100 + base
    | Stroke_color_opacity (color, shade, _) ->
        let base =
          if Color.is_base_color color then
            Color.suborder_with_shade (Color.color_to_string color)
          else
            Color.suborder_with_shade
              (Color.color_to_string color ^ "-" ^ string_of_int shade)
        in
        1100 + base
    | Stroke_0 -> 2000
    | Stroke_1 -> 2001
    | Stroke_2 -> 2002
    | Stroke_width n -> 2010 + n

  let to_class = function
    | Fill_none -> "fill-none"
    | Fill_current -> "fill-current"
    | Fill_current_opacity opacity -> "fill-current" ^ opacity_suffix opacity
    | Fill_color (c, shade) ->
        if Color.is_base_color c || Color.is_custom_color c then
          "fill-" ^ Color.color_to_string c
        else "fill-" ^ Color.color_to_string c ^ "-" ^ string_of_int shade
    | Fill_color_opacity (c, shade, opacity) ->
        if Color.is_base_color c || Color.is_custom_color c then
          "fill-" ^ Color.color_to_string c ^ opacity_suffix opacity
        else
          "fill-" ^ Color.color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Stroke_none -> "stroke-none"
    | Stroke_current -> "stroke-current"
    | Stroke_current_opacity opacity ->
        "stroke-current" ^ opacity_suffix opacity
    | Stroke_color (c, shade) ->
        if Color.is_base_color c || Color.is_custom_color c then
          "stroke-" ^ Color.color_to_string c
        else "stroke-" ^ Color.color_to_string c ^ "-" ^ string_of_int shade
    | Stroke_color_opacity (c, shade, opacity) ->
        if Color.is_base_color c || Color.is_custom_color c then
          "stroke-" ^ Color.color_to_string c ^ opacity_suffix opacity
        else
          "stroke-" ^ Color.color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Stroke_0 -> "stroke-0"
    | Stroke_1 -> "stroke-1"
    | Stroke_2 -> "stroke-2"
    | Stroke_width n -> "stroke-" ^ string_of_int n

  let has_opacity s = String.contains s '/'

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "fill"; "none" ] -> Ok Fill_none
    | [ "fill"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let _, opacity = Color.parse_opacity_modifier current_str in
        match opacity with
        | Color.No_opacity -> Ok Fill_current
        | _ -> Ok (Fill_current_opacity opacity))
    | "fill" :: color_parts when List.exists has_opacity color_parts -> (
        match Color.shade_and_opacity_of_strings color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Fill_color_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "fill" :: color_parts -> (
        match Color.shade_of_strings color_parts with
        | Ok (color, shade) -> Ok (Fill_color (color, shade))
        | Error e -> Error e)
    | [ "stroke"; "none" ] -> Ok Stroke_none
    | [ "stroke"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let _, opacity = Color.parse_opacity_modifier current_str in
        match opacity with
        | Color.No_opacity -> Ok Stroke_current
        | _ -> Ok (Stroke_current_opacity opacity))
    | [ "stroke"; "0" ] -> Ok Stroke_0
    | [ "stroke"; "1" ] -> Ok Stroke_1
    | [ "stroke"; "2" ] -> Ok Stroke_2
    | [ "stroke"; n ] -> (
        match int_of_string_opt n with
        | Some width -> Ok (Stroke_width width)
        | None -> err_not_utility)
    | "stroke" :: color_parts when List.exists has_opacity color_parts -> (
        match Color.shade_and_opacity_of_strings color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Stroke_color_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "stroke" :: color_parts -> (
        match Color.shade_of_strings color_parts with
        | Ok (color, shade) -> Ok (Stroke_color (color, shade))
        | Error e -> Error e)
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
