(** Arbitrary property utilities: [property:value] with optional /opacity. *)

let err_not_utility = Error (`Msg "Not an arbitrary property utility")

(** Parse a CSS color name string into a typed color value. *)
let parse_css_color value =
  match String.lowercase_ascii value with
  | "red" -> Some (Css.color_name Red)
  | "blue" -> Some (Css.color_name Blue)
  | "green" -> Some (Css.color_name Green)
  | "white" -> Some (Css.color_name White)
  | "black" -> Some (Css.color_name Black)
  | "yellow" -> Some (Css.color_name Yellow)
  | "cyan" -> Some (Css.color_name Cyan)
  | "magenta" -> Some (Css.color_name Magenta)
  | "gray" -> Some (Css.color_name Gray)
  | "grey" -> Some (Css.color_name Grey)
  | "orange" -> Some (Css.color_name Orange)
  | "purple" -> Some (Css.color_name Purple)
  | "pink" -> Some (Css.color_name Pink)
  | "silver" -> Some (Css.color_name Silver)
  | "maroon" -> Some (Css.color_name Maroon)
  | "fuchsia" -> Some (Css.color_name Fuchsia)
  | "lime" -> Some (Css.color_name Lime)
  | "olive" -> Some (Css.color_name Olive)
  | "navy" -> Some (Css.color_name Navy)
  | "teal" -> Some (Css.color_name Teal)
  | "aqua" -> Some (Css.color_name Aqua)
  | "transparent" -> Some Css.Transparent
  | "currentcolor" -> Some Css.Current
  | s when String.length s > 0 && s.[0] = '#' ->
      Some (Css.hex (String.sub s 1 (String.length s - 1)))
  | _ -> None

(** Map a CSS property name to its declaration constructor (color properties
    only). *)
let color_property_of_name = function
  | "color" -> Some Css.color
  | "background-color" -> Some Css.background_color
  | "border-color" -> Some Css.border_color
  | "outline-color" -> Some Css.outline_color
  | "text-decoration-color" -> Some Css.text_decoration_color
  | "accent-color" -> Some Css.accent_color
  | "caret-color" -> Some Css.caret_color
  | "fill" -> Some (fun c -> Css.fill (Css.Color c : Css.svg_paint))
  | "stroke" -> Some (fun c -> Css.stroke (Css.Color c : Css.svg_paint))
  | _ -> None

module Handler = struct
  open Style

  type t =
    | Color_opacity of {
        property : string;
        value : string;
        opacity : Color.opacity_modifier;
      }

  type Utility.base += Self of t

  let name = "arbitrary"
  let priority = 35

  let to_style (Color_opacity { property; value; opacity }) =
    match (color_property_of_name property, parse_css_color value) with
    | Some prop, Some color -> (
        match opacity with
        | Color.Opacity_named name ->
            let bare = Parse.extract_var_name name in
            let var_name = "opacity-" ^ bare in
            let fallback =
              Color.opacity_fallback_for_theme_value var_name bare
            in
            (* srgb fallback: resolve the theme value to get the actual
               percentage *)
            let srgb_percent =
              match Var.theme_value var_name with
              | Some v -> (
                  match float_of_string_opt (String.trim v) with
                  | Some f -> f *. 100.0
                  | None -> 100.0)
              | None -> 100.0
            in
            let srgb_fallback =
              Css.color_mix ~in_space:Srgb ~percent1:srgb_percent color
                Css.Transparent
            in
            let fallback_decl = prop srgb_fallback in
            let oklab_color =
              Css.color_mix_var_percent_with_fallback ~in_space:Oklab ~var_name
                ~fallback color Css.Transparent
            in
            let oklab_decl = prop oklab_color in
            let supports_block =
              Css.supports ~condition:Color.color_mix_supports_condition
                [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
            in
            style ~rules:(Some [ supports_block ]) [ fallback_decl ]
        | Color.Opacity_percent p ->
            let srgb_fallback =
              Css.color_mix ~in_space:Srgb ~percent1:p color Css.Transparent
            in
            let fallback_decl = prop srgb_fallback in
            let oklab_color =
              Css.color_mix ~in_space:Oklab ~percent1:p color Css.Transparent
            in
            let oklab_decl = prop oklab_color in
            let supports_block =
              Css.supports ~condition:Color.color_mix_supports_condition
                [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
            in
            style ~rules:(Some [ supports_block ]) [ fallback_decl ]
        | Color.Opacity_arbitrary f ->
            let p = f *. 100.0 in
            let srgb_fallback =
              Css.color_mix ~in_space:Srgb ~percent1:p color Css.Transparent
            in
            let fallback_decl = prop srgb_fallback in
            let oklab_color =
              Css.color_mix ~in_space:Oklab ~percent1:p color Css.Transparent
            in
            let oklab_decl = prop oklab_color in
            let supports_block =
              Css.supports ~condition:Color.color_mix_supports_condition
                [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
            in
            style ~rules:(Some [ supports_block ]) [ fallback_decl ]
        | Color.Opacity_bracket_percent p ->
            let srgb_fallback =
              Css.color_mix ~in_space:Srgb ~percent1:p color Css.Transparent
            in
            let fallback_decl = prop srgb_fallback in
            let oklab_color =
              Css.color_mix ~in_space:Oklab ~percent1:p color Css.Transparent
            in
            let oklab_decl = prop oklab_color in
            let supports_block =
              Css.supports ~condition:Color.color_mix_supports_condition
                [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
            in
            style ~rules:(Some [ supports_block ]) [ fallback_decl ]
        | Color.Opacity_var var_str ->
            let bare = Parse.extract_var_name var_str in
            let srgb_fallback =
              Css.color_mix_var_percent ~in_space:Srgb ~var_name:bare color
                Css.Transparent
            in
            let fallback_decl = prop srgb_fallback in
            let oklab_color =
              Css.color_mix_var_percent ~in_space:Oklab ~var_name:bare color
                Css.Transparent
            in
            let oklab_decl = prop oklab_color in
            let supports_block =
              Css.supports ~condition:Color.color_mix_supports_condition
                [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
            in
            style ~rules:(Some [ supports_block ]) [ fallback_decl ]
        | Color.No_opacity -> style [ prop color ])
    | _ -> style []

  let suborder _ = 0

  let to_class (Color_opacity { property; value; opacity }) =
    let opacity_suffix =
      match opacity with
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
    in
    "[" ^ property ^ ":" ^ value ^ "]" ^ opacity_suffix

  let of_class class_name =
    (* Must start with [ and contain : *)
    let len = String.length class_name in
    if len < 3 || class_name.[0] <> '[' then err_not_utility
    else
      (* Find the closing ] tracking bracket depth *)
      let rec find_close i depth =
        if i >= len then None
        else
          match class_name.[i] with
          | '[' -> find_close (i + 1) (depth + 1)
          | ']' -> if depth = 1 then Some i else find_close (i + 1) (depth - 1)
          | _ -> find_close (i + 1) depth
      in
      match find_close 0 0 with
      | None -> err_not_utility
      | Some close_pos -> (
          let inner = String.sub class_name 1 (close_pos - 1) in
          (* Find the colon that separates property from value *)
          let rec find_colon i =
            if i >= String.length inner then None
            else if inner.[i] = ':' then Some i
            else find_colon (i + 1)
          in
          match find_colon 0 with
          | None -> err_not_utility
          | Some colon_pos ->
              let property = String.sub inner 0 colon_pos in
              let value =
                String.sub inner (colon_pos + 1)
                  (String.length inner - colon_pos - 1)
              in
              (* Parse opacity modifier after ] *)
              let opacity_str =
                if close_pos + 1 < len then
                  String.sub class_name (close_pos + 1) (len - close_pos - 1)
                else ""
              in
              let opacity =
                if String.length opacity_str > 0 && opacity_str.[0] = '/' then
                  let op_part =
                    String.sub opacity_str 1 (String.length opacity_str - 1)
                  in
                  (* Parse the opacity part directly (already split after /) *)
                  if
                    String.length op_part > 2
                    && op_part.[0] = '['
                    && op_part.[String.length op_part - 1] = ']'
                  then
                    let inner =
                      String.sub op_part 1 (String.length op_part - 2)
                    in
                    if String.ends_with ~suffix:"%" inner then
                      let num_str =
                        String.sub inner 0 (String.length inner - 1)
                      in
                      match float_of_string_opt num_str with
                      | Some f -> Color.Opacity_bracket_percent f
                      | None -> Color.No_opacity
                    else
                      match float_of_string_opt inner with
                      | Some f -> Color.Opacity_arbitrary f
                      | None ->
                          if Parse.is_var inner then Color.Opacity_var inner
                          else Color.No_opacity
                  else
                    match float_of_string_opt op_part with
                    | Some f when f >= 0. -> Color.Opacity_percent f
                    | _ ->
                        if
                          Parse.is_valid_theme_name op_part
                          && Var.theme_value ("opacity-" ^ op_part) <> None
                        then Color.Opacity_named op_part
                        else Color.No_opacity
                else Color.No_opacity
              in
              if opacity = Color.No_opacity && opacity_str = "" then
                err_not_utility
              else if
                opacity = Color.No_opacity && String.length opacity_str > 0
              then err_not_utility
              else Ok (Color_opacity { property; value; opacity }))
end

let () = Utility.register (module Handler)
