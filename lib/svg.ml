(** SVG utilities for fill, stroke, and stroke-width. *)

module Css = Cascade.Css

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
    | Fill_bracket_color of string * Css.color
    | Fill_bracket_color_opacity of string * Css.color * Color.opacity_modifier
    | Fill_bracket_var of string
    | Fill_bracket_var_opacity of string * Color.opacity_modifier
    | Fill_bracket_typed_var of string
    | Fill_bracket_typed_var_opacity of string * Color.opacity_modifier
    | Stroke_none
    | Stroke_inherit
    | Stroke_transparent
    | Stroke_current
    | Stroke_current_opacity of Color.opacity_modifier
    | Stroke_color of Color.color * int
    | Stroke_color_opacity of Color.color * int * Color.opacity_modifier
    | Stroke_bracket_color of string * Css.color
    | Stroke_bracket_color_opacity of
        string * Css.color * Color.opacity_modifier
    | Stroke_bracket_var of string
    | Stroke_bracket_var_opacity of string * Color.opacity_modifier
    | Stroke_bracket_typed_var of string
    | Stroke_bracket_typed_var_opacity of string * Color.opacity_modifier
    | Stroke_0
    | Stroke_1
    | Stroke_2
    | Stroke_width of int
    | Stroke_width_bracket of string
    | Stroke_width_typed_var of
        string (* full inner: "length:var(--my-width)" *)

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

  (* Bracket color: fill/stroke with a typed Css.color, converting to hex when
     possible for minified output *)
  let bracket_color_style ~property css_color =
    let color =
      match Color.css_color_to_hex css_color with
      | Some hex_c -> hex_c
      | None -> css_color
    in
    style [ property (Css.Color color : Css.svg_paint) ]

  (* Bracket var: fill/stroke "var(--my-color)" *)
  let bracket_var_style ~property ~merge_key v =
    let bare_name = Parse.extract_var_name v in
    style ~merge_key
      [ property (Css.Color (Css.Var (Var.bracket bare_name)) : Css.svg_paint) ]

  (* Extract a hex string (with leading #) from a Css.color, for oklab
     conversion *)
  let extract_hex_string css_color =
    match Color.css_color_to_hex css_color with
    | Some (Hex { value; _ }) -> "#" ^ value
    | _ -> (
        match css_color with
        | Css.Hex { value; _ } -> "#" ^ value
        | _ -> "#000000")

  (* Bracket color with opacity: convert to hex first, then apply oklab alpha *)
  let bracket_color_opacity_style ~property css_color opacity =
    let percent = Color.opacity_to_percent opacity in
    let alpha = percent /. 100.0 in
    let hex = extract_hex_string css_color in
    let oklab_value = Color.hex_to_oklab_alpha hex alpha in
    style [ property (Css.Color oklab_value : Css.svg_paint) ]

  (* Bracket var with opacity: var fallback + @supports color-mix *)
  let bracket_var_opacity_style ~property ~merge_key v opacity =
    let percent = Color.opacity_to_percent opacity in
    let bare_name = Parse.extract_var_name v in
    let var_color : Css.color = Css.Var (Var.bracket bare_name) in
    let fallback_decl = property (Css.Color var_color : Css.svg_paint) in
    let oklab_color =
      Css.color_mix ~in_space:Oklab var_color Css.Transparent ~percent1:percent
    in
    let oklab_decl = property (Css.Color oklab_color : Css.svg_paint) in
    let supports_block =
      Css.supports ~condition:Color.color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
    in
    Style.style ~merge_key ~rules:(Some [ supports_block ]) [ fallback_decl ]

  (* Parse bracket width value *)
  let parse_bracket_width inner : Css.length =
    if
      String.length inner > 2
      && String.sub inner (String.length inner - 2) 2 = "px"
    then
      let num_str = String.sub inner 0 (String.length inner - 2) in
      match float_of_string_opt num_str with Some f -> Px f | None -> Px 0.
    else if String.length inner > 1 && inner.[String.length inner - 1] = '%'
    then
      let num_str = String.sub inner 0 (String.length inner - 1) in
      match float_of_string_opt num_str with Some f -> Pct f | None -> Pct 0.
    else match float_of_string_opt inner with Some f -> Px f | None -> Px 0.

  let to_style = function
    | Fill_none -> style Css.[ fill None ]
    | Fill_inherit -> style Css.[ fill Inherit ]
    | Fill_transparent -> style Css.[ fill (Color (Css.hex "0000")) ]
    | Fill_current -> style ~merge_key:"fill-current" Css.[ fill Current_color ]
    | Fill_current_opacity opacity -> Color.fill_current_with_opacity opacity
    | Fill_color (color, shade) -> fill_color_style color shade
    | Fill_color_opacity (color, shade, opacity) ->
        Color.fill_with_opacity color shade opacity
    | Fill_bracket_color (_, css_color) ->
        bracket_color_style ~property:Css.fill css_color
    | Fill_bracket_color_opacity (_, css_color, opacity) ->
        bracket_color_opacity_style ~property:Css.fill css_color opacity
    | Fill_bracket_var v | Fill_bracket_typed_var v ->
        bracket_var_style ~property:Css.fill ~merge_key:"fill-" v
    | Fill_bracket_var_opacity (v, opacity)
    | Fill_bracket_typed_var_opacity (v, opacity) ->
        bracket_var_opacity_style ~property:Css.fill ~merge_key:"fill-" v
          opacity
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
    | Stroke_bracket_color (_, css_color) ->
        bracket_color_style ~property:Css.stroke css_color
    | Stroke_bracket_color_opacity (_, css_color, opacity) ->
        bracket_color_opacity_style ~property:Css.stroke css_color opacity
    | Stroke_bracket_var v | Stroke_bracket_typed_var v ->
        bracket_var_style ~property:Css.stroke ~merge_key:"stroke-" v
    | Stroke_bracket_var_opacity (v, opacity)
    | Stroke_bracket_typed_var_opacity (v, opacity) ->
        bracket_var_opacity_style ~property:Css.stroke ~merge_key:"stroke-" v
          opacity
    | Stroke_0 -> style Css.[ stroke_width (Px 0.) ]
    | Stroke_1 -> style Css.[ stroke_width (Px 1.) ]
    | Stroke_2 -> style Css.[ stroke_width (Px 2.) ]
    | Stroke_width n -> style Css.[ stroke_width (Px (float_of_int n)) ]
    | Stroke_width_bracket inner ->
        let w = parse_bracket_width inner in
        style [ Css.stroke_width w ]
    | Stroke_width_typed_var inner ->
        let var_part =
          match String.index_opt inner ':' with
          | Some i -> String.sub inner (i + 1) (String.length inner - i - 1)
          | None -> inner
        in
        let bare_name = Parse.extract_var_name var_part in
        style ~merge_key:"stroke-width-"
          [ Css.stroke_width (Var (Var.bracket bare_name)) ]

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

  (* Suborder: determines ordering within the svg priority group. Three groups:
     fill (0), stroke-color (1), stroke-width (2). Within each group, bracket
     values sort first, then alphabetically. *)
  let suborder t =
    let group, detail =
      match t with
      | Fill_none -> (0, alpha_order "none")
      | Fill_inherit -> (0, alpha_order "inherit")
      | Fill_transparent -> (0, alpha_order "transparent")
      | Fill_current | Fill_current_opacity _ -> (0, alpha_order "current")
      | Fill_color (color, shade) | Fill_color_opacity (color, shade, _) ->
          (0, alpha_order (color_suffix color shade))
      | Fill_bracket_color _ | Fill_bracket_color_opacity _ -> (0, 0)
      | Fill_bracket_var _ | Fill_bracket_var_opacity _
      | Fill_bracket_typed_var _ | Fill_bracket_typed_var_opacity _ ->
          (0, 1)
      | Stroke_none -> (1, alpha_order "none")
      | Stroke_inherit -> (1, alpha_order "inherit")
      | Stroke_transparent -> (1, alpha_order "transparent")
      | Stroke_current | Stroke_current_opacity _ -> (1, alpha_order "current")
      | Stroke_color (color, shade) | Stroke_color_opacity (color, shade, _) ->
          (1, alpha_order (color_suffix color shade))
      | Stroke_bracket_color _ | Stroke_bracket_color_opacity _ -> (1, 0)
      | Stroke_bracket_var _ | Stroke_bracket_var_opacity _
      | Stroke_bracket_typed_var _ | Stroke_bracket_typed_var_opacity _ ->
          (1, 1)
      | Stroke_0 -> (2, 0)
      | Stroke_1 -> (2, 1)
      | Stroke_2 -> (2, 2)
      | Stroke_width n -> (2, 10 + n)
      | Stroke_width_bracket _ -> (2, 100)
      | Stroke_width_typed_var _ -> (2, 100)
    in
    (* alpha_order returns at most 4 bytes = 0x7F7F7F7F on ASCII, so multiply
       group by 0x100000000 to ensure no overlap. *)
    (group lsl 32) + detail

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
    | Fill_bracket_color (v, _) -> "fill-[" ^ v ^ "]"
    | Fill_bracket_color_opacity (v, _, opacity) ->
        "fill-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Fill_bracket_var v -> "fill-[" ^ v ^ "]"
    | Fill_bracket_var_opacity (v, opacity) ->
        "fill-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Fill_bracket_typed_var v -> "fill-[color:" ^ v ^ "]"
    | Fill_bracket_typed_var_opacity (v, opacity) ->
        "fill-[color:" ^ v ^ "]" ^ opacity_suffix opacity
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
    | Stroke_bracket_color (v, _) -> "stroke-[" ^ v ^ "]"
    | Stroke_bracket_color_opacity (v, _, opacity) ->
        "stroke-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Stroke_bracket_var v -> "stroke-[" ^ v ^ "]"
    | Stroke_bracket_var_opacity (v, opacity) ->
        "stroke-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Stroke_bracket_typed_var v -> "stroke-[color:" ^ v ^ "]"
    | Stroke_bracket_typed_var_opacity (v, opacity) ->
        "stroke-[color:" ^ v ^ "]" ^ opacity_suffix opacity
    | Stroke_0 -> "stroke-0"
    | Stroke_1 -> "stroke-1"
    | Stroke_2 -> "stroke-2"
    | Stroke_width n -> "stroke-" ^ string_of_int n
    | Stroke_width_bracket v -> "stroke-[" ^ v ^ "]"
    | Stroke_width_typed_var v -> "stroke-[" ^ v ^ "]"

  let has_opacity s = String.contains s '/'

  let starts prefix s =
    String.length s >= String.length prefix
    && String.sub s 0 (String.length prefix) = prefix

  let is_numeric_start c = (c >= '0' && c <= '9') || c = '.' || c = '-'

  (* Parse bracket value for fill/stroke: determine if it's a color or typed
     var. Returns the variant constructor for the appropriate type. *)
  let parse_bracket_fill v =
    let base_str, opacity = Color.parse_opacity_modifier v in
    let base_inner = Parse.bracket_inner base_str in
    if starts "color:" base_inner then
      let var_part = String.sub base_inner 6 (String.length base_inner - 6) in
      match opacity with
      | Color.No_opacity -> Ok (Fill_bracket_typed_var var_part)
      | _ -> Ok (Fill_bracket_typed_var_opacity (var_part, opacity))
    else if starts "var(" base_inner then
      match opacity with
      | Color.No_opacity -> Ok (Fill_bracket_var base_inner)
      | _ -> Ok (Fill_bracket_var_opacity (base_inner, opacity))
    else
      match Color.parse_bracket_color base_inner with
      | Some css_color -> (
          match opacity with
          | Color.No_opacity -> Ok (Fill_bracket_color (base_inner, css_color))
          | _ ->
              Ok (Fill_bracket_color_opacity (base_inner, css_color, opacity)))
      | None -> err_not_utility

  let parse_bracket_stroke_color v =
    let base_str, opacity = Color.parse_opacity_modifier v in
    let base_inner = Parse.bracket_inner base_str in
    if starts "color:" base_inner then
      let var_part = String.sub base_inner 6 (String.length base_inner - 6) in
      match opacity with
      | Color.No_opacity -> Ok (Stroke_bracket_typed_var var_part)
      | _ -> Ok (Stroke_bracket_typed_var_opacity (var_part, opacity))
    else if starts "var(" base_inner then
      match opacity with
      | Color.No_opacity -> Ok (Stroke_bracket_var base_inner)
      | _ -> Ok (Stroke_bracket_var_opacity (base_inner, opacity))
    else
      match Color.parse_bracket_color base_inner with
      | Some css_color -> (
          match opacity with
          | Color.No_opacity ->
              Ok (Stroke_bracket_color (base_inner, css_color))
          | _ ->
              Ok (Stroke_bracket_color_opacity (base_inner, css_color, opacity))
          )
      | None -> err_not_utility

  (* Parse bracket value for stroke width: [1.5], [12px], [50%],
     [length:var(...)], [number:var(...)], [percentage:var(...)] *)
  let parse_bracket_stroke_width inner =
    if
      starts "length:" inner || starts "number:" inner
      || starts "percentage:" inner
    then Ok (Stroke_width_typed_var inner)
    else if String.length inner > 0 && is_numeric_start inner.[0] then
      Ok (Stroke_width_bracket inner)
    else err_not_utility

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
    | [ "fill"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (Color.parse_opacity_modifier v)) ->
        parse_bracket_fill v
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
    | [ "stroke"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (Color.parse_opacity_modifier v)) ->
        (* Bracket value: could be color or width *)
        let base_str, _ = Color.parse_opacity_modifier v in
        let base_inner = Parse.bracket_inner base_str in
        let normalized =
          String.map (fun c -> if c = '_' then ' ' else c) base_inner
        in
        if
          starts "color:" base_inner || starts "var(" base_inner
          || starts "#" base_inner
          || Parse.is_css_color_fn normalized
        then parse_bracket_stroke_color v
        else parse_bracket_stroke_width base_inner
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
