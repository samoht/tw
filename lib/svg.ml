(** SVG utilities for fill, stroke, and stroke-width. *)

(** Error helper *)
let err_not_utility = Error (`Msg "Not an SVG utility")

module Handler = struct
  open Style

  type t =
    | Fill_none
    | Fill_inherit
    | Fill_transparent
    | Fill_current
    | Fill_current_opacity of Color.opacity_modifier
    | Fill_color of Color.color * int
    | Fill_color_opacity of Color.color * int * Color.opacity_modifier
    | Stroke_none
    | Stroke_inherit
    | Stroke_transparent
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
        if Float.is_integer p then "/" ^ Pp.int (int_of_float p)
        else "/" ^ Pp.float p
    | Color.Opacity_bracket_percent p ->
        if Float.is_integer p then "/[" ^ Pp.int (int_of_float p) ^ "%]"
        else "/[" ^ Pp.float p ^ "%]"
    | Color.Opacity_arbitrary f -> "/[" ^ Pp.float f ^ "]"
    | Color.Opacity_named name -> "/" ^ name
    | Color.Opacity_var v -> "/[" ^ v ^ "]"

  (* Fill color style with scheme support *)
  let fill_color_style color shade =
    if Color.is_custom_color color then
      let css_color = Color.to_css color shade in
      style [ Css.fill (Css.Color css_color) ]
    else
      let color_var =
        Color.property_color_var ~property_prefix:"fill" color shade
      in
      let color_value =
        Color.property_color_value ~property_prefix:"fill" color shade
      in
      let theme_decl, color_ref = Var.binding color_var color_value in
      style [ theme_decl; Css.fill (Css.Color (Css.Var color_ref)) ]

  (* Stroke color style with scheme support *)
  let stroke_color_style color shade =
    if Color.is_custom_color color then
      let css_color = Color.to_css color shade in
      style [ Css.stroke (Css.Color css_color) ]
    else
      let color_var =
        Color.property_color_var ~property_prefix:"stroke" color shade
      in
      let color_value =
        Color.property_color_value ~property_prefix:"stroke" color shade
      in
      let theme_decl, color_ref = Var.binding color_var color_value in
      style [ theme_decl; Css.stroke (Css.Color (Css.Var color_ref)) ]

  let to_style = function
    | Fill_none -> style Css.[ fill None ]
    | Fill_inherit -> style Css.[ fill Inherit ]
    | Fill_transparent -> style Css.[ fill (Color (Css.hex "0000")) ]
    | Fill_current -> style ~merge_key:"fill-current" Css.[ fill Current_color ]
    | Fill_current_opacity opacity -> Color.fill_current_with_opacity opacity
    | Fill_color (color, shade) -> fill_color_style color shade
    | Fill_color_opacity (color, shade, opacity) ->
        Color.fill_with_opacity color shade opacity
    | Stroke_none -> style Css.[ stroke None ]
    | Stroke_inherit -> style Css.[ stroke Inherit ]
    | Stroke_transparent -> style Css.[ stroke (Color (Css.hex "0000")) ]
    | Stroke_current ->
        style ~merge_key:"stroke-current" Css.[ stroke Current_color ]
    | Stroke_current_opacity opacity ->
        Color.stroke_current_with_opacity opacity
    | Stroke_color (color, shade) -> stroke_color_style color shade
    | Stroke_color_opacity (color, shade, opacity) ->
        Color.stroke_with_opacity color shade opacity
    | Stroke_0 -> style Css.[ stroke_width (Px 0.) ]
    | Stroke_1 -> style Css.[ stroke_width (Px 1.) ]
    | Stroke_2 -> style Css.[ stroke_width (Px 2.) ]
    | Stroke_width n -> style Css.[ stroke_width (Px (float_of_int n)) ]

  (* Alphabetical suborder from first 4 chars of a string *)
  let alpha_order s =
    let v = ref 0 in
    for i = 0 to min 3 (String.length s - 1) do
      v := (!v * 256) + Char.code s.[i]
    done;
    !v

  let color_suffix color shade =
    if Color.is_base_color color || Color.is_custom_color color then
      Color.color_to_string color
    else Color.color_to_string color ^ "-" ^ string_of_int shade

  let suborder = function
    | Fill_none -> alpha_order "none"
    | Fill_inherit -> alpha_order "inherit"
    | Fill_transparent -> alpha_order "transparent"
    | Fill_current -> alpha_order "current"
    | Fill_current_opacity _ -> alpha_order "current"
    | Fill_color (color, shade) -> alpha_order (color_suffix color shade)
    | Fill_color_opacity (color, shade, _) ->
        alpha_order (color_suffix color shade)
    | Stroke_none -> 0x10000000 + alpha_order "none"
    | Stroke_inherit -> 0x10000000 + alpha_order "inherit"
    | Stroke_transparent -> 0x10000000 + alpha_order "transparent"
    | Stroke_current -> 0x10000000 + alpha_order "current"
    | Stroke_current_opacity _ -> 0x10000000 + alpha_order "current"
    | Stroke_color (color, shade) ->
        0x10000000 + alpha_order (color_suffix color shade)
    | Stroke_color_opacity (color, shade, _) ->
        0x10000000 + alpha_order (color_suffix color shade)
    | Stroke_0 -> 0x20000000
    | Stroke_1 -> 0x20000001
    | Stroke_2 -> 0x20000002
    | Stroke_width n -> 0x20000010 + n

  let to_class = function
    | Fill_none -> "fill-none"
    | Fill_inherit -> "fill-inherit"
    | Fill_transparent -> "fill-transparent"
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
    | Stroke_inherit -> "stroke-inherit"
    | Stroke_transparent -> "stroke-transparent"
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
    let parts = Parse.split_class class_name in
    match parts with
    | [ "fill"; "none" ] -> Ok Fill_none
    | [ "fill"; "inherit" ] -> Ok Fill_inherit
    | [ "fill"; "transparent" ] -> Ok Fill_transparent
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
    | [ "stroke"; "inherit" ] -> Ok Stroke_inherit
    | [ "stroke"; "transparent" ] -> Ok Stroke_transparent
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
