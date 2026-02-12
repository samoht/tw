(** Divide utilities for creating gaps between child elements

    @see <https://tailwindcss.com/docs/divide-width>
      Tailwind CSS Divide Width documentation *)

module Handler = struct
  open Style
  open Css

  type t =
    | Divide_x_reverse
    | Divide_y_reverse
    | Divide_color of Color.color * int
    | Divide_color_opacity of Color.color * int * Color.opacity_modifier
    | Divide_transparent
    | Divide_current
    | Divide_current_opacity of Color.opacity_modifier
    | Divide_inherit

  type Utility.base += Self of t

  let name = "divide"
  let priority = 8

  (* CSS Variables for divide reverse *)
  let divide_x_reverse_var =
    Var.property_default Css.Number_percentage ~initial:(Num 0.0)
      ~universal:true ~property_order:10 "tw-divide-x-reverse"

  let divide_y_reverse_var =
    Var.property_default Css.Number_percentage ~initial:(Num 0.0)
      ~universal:true ~property_order:11 "tw-divide-y-reverse"

  (* divide-x-reverse utility sets --tw-divide-x-reverse: 1 on children *)
  let divide_x_reverse_style () =
    let selector =
      Css.Selector.(where [ class_ "divide-x-reverse" >> not [ Last_child ] ])
    in
    let decl, _ = Var.binding divide_x_reverse_var (Css.Num 1.0) in
    let property_rules =
      [ Var.property_rule divide_x_reverse_var ] |> List.filter_map Fun.id
    in
    let rule = Css.rule ~selector [ decl ] in
    style ~rules:(Some [ rule ]) ~property_rules:(Css.concat property_rules) []

  (* divide-y-reverse utility sets --tw-divide-y-reverse: 1 on children *)
  let divide_y_reverse_style () =
    let selector =
      Css.Selector.(where [ class_ "divide-y-reverse" >> not [ Last_child ] ])
    in
    let decl, _ = Var.binding divide_y_reverse_var (Css.Num 1.0) in
    let property_rules =
      [ Var.property_rule divide_y_reverse_var ] |> List.filter_map Fun.id
    in
    let rule = Css.rule ~selector [ decl ] in
    style ~rules:(Some [ rule ]) ~property_rules:(Css.concat property_rules) []

  (* Divide color utilities use nested rules with :where(.divide-X >
     :not(:last-child)) We construct the full class name in the selector like
     space-x-reverse does. *)
  let divide_color_style color shade =
    let class_name =
      if Color.is_base_color color || Color.is_custom_color color then
        "divide-" ^ Color.color_to_string color
      else "divide-" ^ Color.color_to_string color ^ "-" ^ string_of_int shade
    in
    let selector =
      Css.Selector.(
        where [ Combined (Class class_name, Child, Not [ Last_child ]) ])
    in
    if Color.is_custom_color color then
      let css_color = Color.to_css color shade in
      let rule = Css.rule ~selector [ Css.border_color css_color ] in
      style ~rules:(Some [ rule ]) []
    else
      let color_var = Color.get_color_var color shade in
      let color_value =
        Color.to_css color (if Color.is_base_color color then 500 else shade)
      in
      let decl, color_ref = Var.binding color_var color_value in
      (* Include theme decl in the rule so it gets extracted, but props stays
         empty to avoid emitting an empty .divide-X {} marker class *)
      let rule =
        Css.rule ~selector [ decl; Css.border_color (Css.Var color_ref) ]
      in
      style ~rules:(Some [ rule ]) []

  let divide_transparent_style () =
    let selector =
      Css.Selector.(
        where
          [ Combined (Class "divide-transparent", Child, Not [ Last_child ]) ])
    in
    let rule = Css.rule ~selector [ Css.border_color (Css.hex "#0000") ] in
    style ~rules:(Some [ rule ]) []

  let divide_current_style () =
    let selector =
      Css.Selector.(
        where [ Combined (Class "divide-current", Child, Not [ Last_child ]) ])
    in
    let rule = Css.rule ~selector [ Css.border_color Css.Current ] in
    style ~rules:(Some [ rule ]) []

  let divide_inherit_style () =
    let selector =
      Css.Selector.(
        where [ Combined (Class "divide-inherit", Child, Not [ Last_child ]) ])
    in
    let rule = Css.rule ~selector [ Css.border_color Css.Inherit ] in
    style ~rules:(Some [ rule ]) []

  (* Format opacity modifier for class names *)
  let opacity_suffix = function
    | Color.No_opacity -> ""
    | Color.Opacity_percent p ->
        if Float.is_integer p then Printf.sprintf "/%d" (int_of_float p)
        else Printf.sprintf "/%g" p
    | Color.Opacity_arbitrary f -> Printf.sprintf "/[%g]" f

  (* Divide color with opacity using Color helpers *)
  let divide_color_opacity_style color shade opacity =
    let base_class_name =
      if Color.is_base_color color || Color.is_custom_color color then
        "divide-" ^ Color.color_to_string color
      else "divide-" ^ Color.color_to_string color ^ "-" ^ string_of_int shade
    in
    let class_name = base_class_name ^ opacity_suffix opacity in
    let selector =
      Css.Selector.(
        where [ Combined (Class class_name, Child, Not [ Last_child ]) ])
    in
    Color.divide_with_opacity color shade opacity selector

  let divide_current_opacity_style opacity =
    let class_name = "divide-current" ^ opacity_suffix opacity in
    let selector =
      Css.Selector.(
        where [ Combined (Class class_name, Child, Not [ Last_child ]) ])
    in
    Color.divide_current_with_opacity opacity selector

  (* Helper to check if a string contains an opacity modifier *)
  let has_opacity s = String.contains s '/'

  let to_class = function
    | Divide_x_reverse -> "divide-x-reverse"
    | Divide_y_reverse -> "divide-y-reverse"
    | Divide_color (c, shade) ->
        if Color.is_base_color c || Color.is_custom_color c then
          "divide-" ^ Color.color_to_string c
        else "divide-" ^ Color.color_to_string c ^ "-" ^ string_of_int shade
    | Divide_color_opacity (c, shade, opacity) ->
        if Color.is_base_color c || Color.is_custom_color c then
          "divide-" ^ Color.color_to_string c ^ opacity_suffix opacity
        else
          "divide-" ^ Color.color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Divide_transparent -> "divide-transparent"
    | Divide_current -> "divide-current"
    | Divide_current_opacity opacity ->
        "divide-current" ^ opacity_suffix opacity
    | Divide_inherit -> "divide-inherit"

  let to_style = function
    | Divide_x_reverse -> divide_x_reverse_style ()
    | Divide_y_reverse -> divide_y_reverse_style ()
    | Divide_color (color, shade) -> divide_color_style color shade
    | Divide_color_opacity (color, shade, opacity) ->
        divide_color_opacity_style color shade opacity
    | Divide_transparent -> divide_transparent_style ()
    | Divide_current -> divide_current_style ()
    | Divide_current_opacity opacity -> divide_current_opacity_style opacity
    | Divide_inherit -> divide_inherit_style ()

  let suborder = function
    | Divide_x_reverse -> 0
    | Divide_y_reverse -> 1
    (* Colors come after reverse utilities, use 100+ for ordering *)
    | Divide_color (color, shade) ->
        let base =
          if Color.is_base_color color then
            Color.suborder_with_shade (Color.color_to_string color)
          else
            Color.suborder_with_shade
              (Color.color_to_string color ^ "-" ^ string_of_int shade)
        in
        100 + base
    | Divide_color_opacity (color, shade, _) ->
        let base =
          if Color.is_base_color color then
            Color.suborder_with_shade (Color.color_to_string color)
          else
            Color.suborder_with_shade
              (Color.color_to_string color ^ "-" ^ string_of_int shade)
        in
        100 + base
    | Divide_current -> 100 + (4 * 1000)
    | Divide_current_opacity _ -> 100 + (4 * 1000)
    | Divide_inherit -> 100 + (9 * 1000)
    | Divide_transparent -> 100 + (25 * 1000)

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "divide"; "x"; "reverse" ] -> Ok Divide_x_reverse
    | [ "divide"; "y"; "reverse" ] -> Ok Divide_y_reverse
    | [ "divide"; "transparent" ] -> Ok Divide_transparent
    | [ "divide"; "inherit" ] -> Ok Divide_inherit
    | [ "divide"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let _, opacity = Color.parse_opacity_modifier current_str in
        match opacity with
        | Color.No_opacity -> Ok Divide_current
        | _ -> Ok (Divide_current_opacity opacity))
    | "divide" :: color_parts when List.exists has_opacity color_parts -> (
        match Color.shade_and_opacity_of_strings color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Divide_color_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "divide" :: color_parts -> (
        match Color.shade_of_strings color_parts with
        | Ok (color, shade) -> Ok (Divide_color (color, shade))
        | Error e -> Error e)
    | _ -> Error (`Msg "Not a divide utility")
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let divide_x_reverse = utility Divide_x_reverse
let divide_y_reverse = utility Divide_y_reverse
