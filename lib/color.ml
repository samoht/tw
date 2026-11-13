(** Colour utilities: text, border, outline, accent, caret and placeholder
    colours. *)

module Css = Cascade.Css
include Srgb
include Color_value
include Color_opacity

module Tailwind = struct
  let get_color = Palette.oklch_css
end

(** {1 Parsing Functions} *)

module Handler = struct
  (* Which border edge a border-{side}-{color} utility paints: a physical edge,
     a logical axis (border-{inline,block}-color) or a logical edge
     (border-{inline,block}-{start,end}-color). *)
  module Side = struct
    type t =
      | Top
      | Right
      | Bottom
      | Left
      | Inline_axis
      | Block_axis
      | Inline_start
      | Inline_end
      | Block_start
      | Block_end
  end

  (* The colour value of a border-{side}-{color}: a named theme colour, an
     arbitrary bracket colour, or a keyword. *)
  module Side_color = struct
    type t =
      | Named of color * int
      | Named_opacity of color * int * opacity_modifier
      | Bracket of string * Css.color
      | Bracket_opacity of string * Css.color * opacity_modifier
      | Raw of string * string
        (* border-t-[notacolour]: the colour is this family's last resort, so a
           bracket no reader took reaches the side's own longhand verbatim. *)
      | Transparent
      | Current
      | Current_opacity of opacity_modifier
      | Inherit
  end

  (** Local color utility type *)
  type t =
    (* Text colors *)
    | Text of color * int
    | Text_opacity of color * int * opacity_modifier
    | Text_transparent
    | Text_current
    | Text_current_opacity of opacity_modifier
    | Text_inherit
    | Text_bracket_color of string * Css.color
      (* text-[#0088cc], text-[black] - string is original bracket content *)
    | Text_bracket_raw of string * string
      (* text-[notacolour], text-[foo:red]: the colour is this family's last
         resort, so a bracket no reader took reaches [color] verbatim, which is
         Tailwind's token-stream contract. The browser discards the declaration;
         the selector is what the class was for. *)
    | Text_bracket_color_opacity of string * Css.color * opacity_modifier
    | Text_bracket_var of string (* text-[var(--value)] *)
    | Text_bracket_var_opacity of string * opacity_modifier
    | Text_bracket_typed_var of string (* text-[color:var(--value)] *)
    | Text_bracket_typed_var_opacity of string * opacity_modifier
    (* Border colors *)
    | Border of color * int
    | Border_opacity of color * int * opacity_modifier
    | Border_transparent
    | Border_current
    | Border_current_opacity of opacity_modifier
    | Border_inherit
    | Border_bracket_color of string * Css.color
    | Border_bracket_raw of string * string
      (* border-[notacolour]: the colour is this family's last resort, so a
         bracket no reader took reaches [border-color] verbatim. *)
    | Border_bracket_color_opacity of string * Css.color * opacity_modifier
    | Border_side_color of Side.t * Side_color.t
    (* Accent colors *)
    | Accent of color * int
    | Accent_opacity of color * int * opacity_modifier
    | Accent_transparent
    | Accent_current
    | Accent_current_opacity of opacity_modifier
    | Accent_inherit
    | Accent_bracket_color of string * Css.color
    | Accent_bracket_raw of string * string
      (* accent-[notacolour]: the colour is this family's last resort, so a
         bracket no reader took reaches [accent-color] verbatim. *)
    | Accent_bracket_color_opacity of string * Css.color * opacity_modifier
    (* Caret colors *)
    | Caret of color * int
    | Caret_opacity of color * int * opacity_modifier
    | Caret_current
    | Caret_current_opacity of opacity_modifier
    | Caret_inherit
    | Caret_transparent
    | Caret_bracket_color of string * Css.color
    | Caret_bracket_raw of string * string
      (* caret-[notacolour]: the colour is this family's last resort, so a
         bracket no reader took reaches [caret-color] verbatim. *)
    | Caret_bracket_color_opacity of string * Css.color * opacity_modifier
    (* Outline colors *)
    | Outline of color * int
    | Outline_opacity of color * int * opacity_modifier
    | Outline_current
    | Outline_current_opacity of opacity_modifier
    | Outline_inherit
    | Outline_transparent
    | Outline_bracket_color of string * Css.color
      (* outline-[#0088cc], outline-[black] *)
    | Outline_bracket_raw of string * string
      (* outline-[notacolour]: the colour is this family's last resort, the way
         it is for [text-]. *)
    | Outline_bracket_color_opacity of string * Css.color * opacity_modifier
    | Outline_bracket_var of string (* outline-[var(--value)] *)
    | Outline_bracket_var_opacity of string * opacity_modifier
    | Outline_bracket_typed_var of string (* outline-[color:var(--value)] *)
    | Outline_bracket_typed_var_opacity of string * opacity_modifier
    (* Placeholder colors *)
    | Placeholder of color * int
    | Placeholder_opacity of color * int * opacity_modifier
    | Placeholder_transparent
    | Placeholder_current
    | Placeholder_current_opacity of opacity_modifier
    | Placeholder_inherit
    | Placeholder_bracket_color of string * Css.color
    | Placeholder_bracket_raw of string * string
      (* placeholder-[notacolour]: the colour is this family's last resort. *)
    | Placeholder_bracket_color_opacity of string * Css.color * opacity_modifier

  (** Extensible variant for color utilities *)

  (* The colour helpers other families reach through the handler. *)
  let all_palette_declarations = all_palette_declarations
  let color_binding = color_binding
  let colors_with_opacity_style = colors_with_opacity_style
  let theme_color_of_name = theme_color_of_name
  let theme_color_decl = theme_color_decl

  open Style
  open Css

  let name = "color"

  (* Color families sort at their property's canonical rank, not together:
     border-color (rank ~65) joins border-width/style at priority 19; the
     [color] property (rank ~86) opens the late-typography block at priority 26,
     just before text-transform, and the placeholder, caret and accent colours
     close the same block after the underline offset; outline-color (rank ~92)
     joins the outline width, offset and style utilities at priority 28. The
     rest (background, ...) stay at 25. [_opacity] variants lead each group so
     the type resolves to the color [t] rather than the shadowed [Css.Border] /
     [Css.user_select] [Text] constructors. *)
  let priority = function
    | Border_opacity _ | Border _ | Border_transparent | Border_current
    | Border_current_opacity _ | Border_inherit | Border_bracket_color _
    | Border_bracket_raw _ | Border_side_color _
    | Border_bracket_color_opacity _ ->
        19
    | Text_opacity _ | Text _ | Text_transparent | Text_current
    | Text_current_opacity _ | Text_inherit | Text_bracket_color _
    | Text_bracket_raw _ | Text_bracket_color_opacity _ | Text_bracket_var _
    | Text_bracket_var_opacity _ | Text_bracket_typed_var _
    | Text_bracket_typed_var_opacity _ | Placeholder_opacity _ | Placeholder _
    | Placeholder_transparent | Placeholder_current
    | Placeholder_current_opacity _ | Placeholder_inherit
    | Placeholder_bracket_color _ | Placeholder_bracket_raw _
    | Placeholder_bracket_color_opacity _ | Caret_opacity _ | Caret _
    | Caret_transparent | Caret_current | Caret_current_opacity _
    | Caret_inherit | Caret_bracket_color _ | Caret_bracket_color_opacity _
    | Accent_opacity _ | Accent _ | Accent_transparent | Accent_current
    | Accent_current_opacity _ | Accent_inherit | Accent_bracket_color _
    | Accent_bracket_color_opacity _ | Accent_bracket_raw _
    | Caret_bracket_raw _ ->
        26
    | Outline_opacity _ | Outline _ | Outline_current
    | Outline_current_opacity _ | Outline_inherit | Outline_transparent
    | Outline_bracket_color _ | Outline_bracket_raw _
    | Outline_bracket_color_opacity _ | Outline_bracket_var _
    | Outline_bracket_var_opacity _ | Outline_bracket_typed_var _
    | Outline_bracket_typed_var_opacity _ ->
        28

  (* Helper to check if a string contains an opacity modifier *)
  let has_opacity s = String.contains s '/'

  (* A data-type hint chooses the longhand and says nothing about the value, so
     a family that writes one colour longhand reads what follows any hint.
     [border-[…]] is the exception: [length:] and [line-width:] name the width,
     which the borders handler owns, and naming them in [not_mine] leaves such a
     bracket to it rather than reading the value here as a colour. *)
  let bracket_color_after_hint ?(not_mine = []) inner : Css.color option =
    match Parse.data_type_hint inner with
    | Some (hint, _) when List.mem hint not_mine -> None
    | _ -> Option.bind (Parse.value_after_hint inner) parse_bracket_color

  (* The colour is a colour family's last resort: a bracket no reader took is
     still a declaration, forwarded verbatim under the token-stream contract.
     [not_mine] is the family's own list of hints another handler owns - a
     [line-width:] bracket is the border width's, not the border colour's - so
     the last resort must decline those too, or two handlers claim one class. *)
  let last_resort ?(not_mine = []) inner : string option =
    match Parse.data_type_hint inner with
    | Some (hint, _) when List.mem hint not_mine -> None
    | _ -> Parse.arbitrary_declaration_value inner

  (* A per-side border bracket: a colour or a hinted var, under a modifier or
     not, and a bracket no reader took is the side's last resort at full opacity
     only, as it is for the all-sides utility. *)
  let parse_border_side_bracket ~theme v =
    let not_mine = [ "length"; "line-width" ] in
    let base, opacity = parse_opacity_modifier ~theme v in
    let inner = Parse.bracket_inner base in
    match (bracket_color_after_hint ~not_mine inner, opacity) with
    | Some css_color, No_opacity -> Ok (Side_color.Bracket (inner, css_color))
    | Some css_color, opacity ->
        Ok (Side_color.Bracket_opacity (inner, css_color, opacity))
    | None, No_opacity -> (
        match last_resort ~not_mine inner with
        | Some raw -> Ok (Side_color.Raw (inner, raw))
        | None -> Error (`Msg ("Invalid border side bracket: " ^ v)))
    | None, _ -> Error (`Msg ("Invalid border side bracket: " ^ v))

  (* The colour a border-{side}-* class names, read the way the all-sides border
     colour reads its own: the keywords take a modifier as a palette colour
     does. *)
  let parse_border_side_color ~theme = function
    | [ "transparent" ] -> Ok Side_color.Transparent
    | [ "inherit" ] -> Ok Side_color.Inherit
    | [ current_str ] when String.starts_with ~prefix:"current" current_str -> (
        match parse_opacity_modifier ~theme current_str with
        | "current", No_opacity -> Ok Side_color.Current
        | "current", opacity -> Ok (Side_color.Current_opacity opacity)
        | _ -> Error (`Msg ("Invalid border side colour: " ^ current_str)))
    | [ v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (parse_opacity_modifier ~theme v)) ->
        parse_border_side_bracket ~theme v
    | color_parts when List.exists has_opacity color_parts ->
        Result.map
          (fun (color, shade, opacity) ->
            Side_color.Named_opacity (color, shade, opacity))
          (shade_and_opacity_of_strings ~theme color_parts)
    | color_parts ->
        Result.map
          (fun (color, shade) -> Side_color.Named (color, shade))
          (shade_of_strings ~theme color_parts)

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "text"; "transparent" ] -> Ok Text_transparent
    | [ "text"; "inherit" ] -> Ok Text_inherit
    | [ "text"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = parse_opacity_modifier ~theme current_str in
        match opacity with
        | No_opacity when base = "current" -> Ok Text_current
        | No_opacity -> Error (`Msg ("Invalid text: " ^ current_str))
        | _ -> Ok (Text_current_opacity opacity))
    | [ "text"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (parse_opacity_modifier ~theme v))
      -> (
        let base_str, opacity = parse_opacity_modifier ~theme v in
        let base_inner = Parse.bracket_inner base_str in
        match parse_bracket_hint base_inner with
        | Some (Typed_var var_part) -> (
            match opacity with
            | No_opacity -> Ok (Text_bracket_typed_var var_part)
            | _ -> Ok (Text_bracket_typed_var_opacity (var_part, opacity)))
        | Some (Bare_var v) -> (
            match opacity with
            | No_opacity -> Ok (Text_bracket_var v)
            | _ -> Ok (Text_bracket_var_opacity (v, opacity)))
        | Some (Plain_color css_color) -> (
            match opacity with
            | No_opacity -> Ok (Text_bracket_color (base_inner, css_color))
            | _ ->
                Ok (Text_bracket_color_opacity (base_inner, css_color, opacity))
            )
        | None -> (
            (* The colour is this family's last resort, so a bracket no reader
               took still reaches [color]. An opacity modifier has nothing to
               mix - the value is not a colour - so that form stays refused,
               where the CLI writes a color-mix around the raw token. *)
            match (opacity, Parse.arbitrary_declaration_value base_inner) with
            | No_opacity, Some raw -> Ok (Text_bracket_raw (base_inner, raw))
            | _ -> Error (`Msg ("Invalid text bracket value: " ^ base_inner))))
    | "text" :: color_parts when List.exists has_opacity color_parts -> (
        match shade_and_opacity_of_strings ~theme color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Text_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "text" :: color_parts -> (
        match shade_of_strings ~theme color_parts with
        | Ok (color, shade) -> Ok (Text (color, shade))
        | Error e -> Error e)
    | [ "border"; "transparent" ] -> Ok Border_transparent
    | [ "border"; "inherit" ] -> Ok Border_inherit
    | [ "border"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = parse_opacity_modifier ~theme current_str in
        match opacity with
        | No_opacity when base = "current" -> Ok Border_current
        | No_opacity -> Error (`Msg ("Invalid border: " ^ current_str))
        | _ -> Ok (Border_current_opacity opacity))
    | [ "border"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (parse_opacity_modifier ~theme v))
      -> (
        let base_str, opacity = parse_opacity_modifier ~theme v in
        let base_inner = Parse.bracket_inner base_str in
        match
          bracket_color_after_hint ~not_mine:[ "length"; "line-width" ]
            base_inner
        with
        | Some css_color -> (
            match opacity with
            | No_opacity -> Ok (Border_bracket_color (base_inner, css_color))
            | _ ->
                Ok
                  (Border_bracket_color_opacity (base_inner, css_color, opacity))
            )
        | None -> (
            match
              ( opacity,
                last_resort ~not_mine:[ "length"; "line-width" ] base_inner )
            with
            | No_opacity, Some raw -> Ok (Border_bracket_raw (base_inner, raw))
            | _ -> Error (`Msg ("Invalid border bracket value: " ^ base_inner)))
        )
    | "border" :: side :: rest
      when rest <> []
           &&
           match side with
           | "t" | "r" | "b" | "l" | "x" | "y" | "s" | "e" | "bs" | "be" -> true
           | _ -> false ->
        let bs =
          match side with
          | "t" -> Side.Top
          | "r" -> Side.Right
          | "b" -> Side.Bottom
          | "x" -> Side.Inline_axis
          | "y" -> Side.Block_axis
          | "s" -> Side.Inline_start
          | "e" -> Side.Inline_end
          | "bs" -> Side.Block_start
          | "be" -> Side.Block_end
          | _ -> Side.Left
        in
        Result.map
          (fun value -> Border_side_color (bs, value))
          (parse_border_side_color ~theme rest)
    | "border" :: color_parts when List.exists has_opacity color_parts -> (
        match shade_and_opacity_of_strings ~theme color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Border_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "border" :: color_parts -> (
        match shade_of_strings ~theme color_parts with
        | Ok (color, shade) -> Ok (Border (color, shade))
        | Error e -> Error e)
    | [ "accent"; "transparent" ] -> Ok Accent_transparent
    | [ "accent"; "inherit" ] -> Ok Accent_inherit
    | [ "accent"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = parse_opacity_modifier ~theme current_str in
        match opacity with
        | No_opacity when base = "current" -> Ok Accent_current
        | No_opacity -> Error (`Msg ("Invalid accent: " ^ current_str))
        | _ -> Ok (Accent_current_opacity opacity))
    | [ "accent"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (parse_opacity_modifier ~theme v))
      -> (
        let base_str, opacity = parse_opacity_modifier ~theme v in
        let base_inner = Parse.bracket_inner base_str in
        match bracket_color_after_hint base_inner with
        | Some css_color -> (
            match opacity with
            | No_opacity -> Ok (Accent_bracket_color (base_inner, css_color))
            | _ ->
                Ok
                  (Accent_bracket_color_opacity (base_inner, css_color, opacity))
            )
        | None -> (
            match (opacity, last_resort base_inner) with
            | No_opacity, Some raw -> Ok (Accent_bracket_raw (base_inner, raw))
            | _ -> Error (`Msg ("Invalid accent bracket value: " ^ base_inner)))
        )
    | "accent" :: color_parts when List.exists has_opacity color_parts -> (
        match shade_and_opacity_of_strings ~theme color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Accent_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "accent" :: color_parts -> (
        match shade_of_strings ~theme color_parts with
        | Ok (color, shade) -> Ok (Accent (color, shade))
        | Error e -> Error e)
    | [ "caret"; "inherit" ] -> Ok Caret_inherit
    | [ "caret"; "transparent" ] -> Ok Caret_transparent
    | [ "caret"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = parse_opacity_modifier ~theme current_str in
        match opacity with
        | No_opacity when base = "current" -> Ok Caret_current
        | No_opacity -> Error (`Msg ("Invalid caret: " ^ current_str))
        | _ -> Ok (Caret_current_opacity opacity))
    | [ "caret"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (parse_opacity_modifier ~theme v))
      -> (
        let base_str, opacity = parse_opacity_modifier ~theme v in
        let base_inner = Parse.bracket_inner base_str in
        match bracket_color_after_hint base_inner with
        | Some css_color -> (
            match opacity with
            | No_opacity -> Ok (Caret_bracket_color (base_inner, css_color))
            | _ ->
                Ok
                  (Caret_bracket_color_opacity (base_inner, css_color, opacity))
            )
        | None -> (
            match (opacity, last_resort base_inner) with
            | No_opacity, Some raw -> Ok (Caret_bracket_raw (base_inner, raw))
            | _ -> Error (`Msg ("Invalid caret bracket value: " ^ base_inner))))
    | "caret" :: color_parts when List.exists has_opacity color_parts -> (
        match shade_and_opacity_of_strings ~theme color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Caret_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "caret" :: color_parts -> (
        match shade_of_strings ~theme color_parts with
        | Ok (color, shade) -> Ok (Caret (color, shade))
        | Error e -> Error e)
    | [ "outline"; "transparent" ] -> Ok Outline_transparent
    | [ "outline"; "inherit" ] -> Ok Outline_inherit
    | [ "outline"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = parse_opacity_modifier ~theme current_str in
        match opacity with
        | No_opacity when base = "current" -> Ok Outline_current
        | No_opacity -> Error (`Msg ("Invalid outline: " ^ current_str))
        | _ -> Ok (Outline_current_opacity opacity))
    | [ "outline"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (parse_opacity_modifier ~theme v))
      -> (
        let base_str, opacity = parse_opacity_modifier ~theme v in
        let base_inner = Parse.bracket_inner base_str in
        match parse_bracket_hint base_inner with
        | Some (Typed_var var_part) -> (
            match opacity with
            | No_opacity -> Ok (Outline_bracket_typed_var var_part)
            | _ -> Ok (Outline_bracket_typed_var_opacity (var_part, opacity)))
        | Some (Bare_var v) -> (
            match opacity with
            | No_opacity -> Ok (Outline_bracket_var v)
            | _ -> Ok (Outline_bracket_var_opacity (v, opacity)))
        | Some (Plain_color css_color) -> (
            match opacity with
            | No_opacity -> Ok (Outline_bracket_color (base_inner, css_color))
            | _ ->
                Ok
                  (Outline_bracket_color_opacity (base_inner, css_color, opacity))
            )
        | None -> (
            match (opacity, Parse.arbitrary_declaration_value base_inner) with
            | No_opacity, Some raw -> Ok (Outline_bracket_raw (base_inner, raw))
            | _ -> Error (`Msg ("Invalid outline bracket value: " ^ base_inner))
            ))
    | "outline" :: color_parts when List.exists has_opacity color_parts -> (
        match shade_and_opacity_of_strings ~theme color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Outline_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "outline" :: color_parts -> (
        match shade_of_strings ~theme color_parts with
        | Ok (color, shade) -> Ok (Outline (color, shade))
        | Error e -> Error e)
    | [ "placeholder"; "transparent" ] -> Ok Placeholder_transparent
    | [ "placeholder"; "inherit" ] -> Ok Placeholder_inherit
    | [ "placeholder"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = parse_opacity_modifier ~theme current_str in
        match opacity with
        | No_opacity when base = "current" -> Ok Placeholder_current
        | No_opacity -> Error (`Msg ("Invalid placeholder: " ^ current_str))
        | _ -> Ok (Placeholder_current_opacity opacity))
    | [ "placeholder"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (parse_opacity_modifier ~theme v))
      -> (
        let base_str, opacity = parse_opacity_modifier ~theme v in
        let base_inner = Parse.bracket_inner base_str in
        match bracket_color_after_hint base_inner with
        | Some css_color -> (
            match opacity with
            | No_opacity ->
                Ok (Placeholder_bracket_color (base_inner, css_color))
            | _ ->
                Ok
                  (Placeholder_bracket_color_opacity
                     (base_inner, css_color, opacity)))
        | None -> (
            match (opacity, last_resort base_inner) with
            | No_opacity, Some raw ->
                Ok (Placeholder_bracket_raw (base_inner, raw))
            | _ ->
                Error
                  (`Msg ("Invalid placeholder bracket value: " ^ base_inner))))
    | "placeholder" :: color_parts when List.exists has_opacity color_parts -> (
        match shade_and_opacity_of_strings ~theme color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Placeholder_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "placeholder" :: color_parts -> (
        match shade_of_strings ~theme color_parts with
        | Ok (color, shade) -> Ok (Placeholder (color, shade))
        | Error e -> Error e)
    | _ -> Error (`Msg "Not a color utility")

  (** Text color utilities *)

  let text' ?theme color shade =
    if is_custom_color color then
      let css_color = to_css color shade in
      style [ Css.color css_color ]
    else
      let color_name = scheme_color_name color shade in
      let prop_name = "text-color-" ^ color_name in
      let has_property_scoped = Scheme.theme_value theme prop_name <> None in
      let cv, color_value =
        if has_property_scoped then
          ( property_color_var ?theme ~property_prefix:"text-color" color shade,
            property_color_value ?theme ~property_prefix:"text-color" color
              shade )
        else (color_var color shade, token_value ?theme color shade)
      in
      let decls, color = bound ?theme cv color_value in
      style (decls @ [ Css.color color ])

  let text_transparent = style [ Css.color (Css.hex "#0000") ]
  let text_current = style [ Css.color Current ]
  let text_inherit = style [ Css.color Inherit ]

  (** Border color utilities *)

  let border_color' ?theme color shade =
    if is_custom_color color then
      let css_color = to_css color shade in
      style [ Css.border_color css_color ]
    else
      let color_var = color_var color shade in
      let color_value = token_value ?theme color shade in
      let decls, color = bound ?theme color_var color_value in
      style (decls @ [ Css.border_color color ])

  let border_transparent = style [ Css.border_color (Css.hex "#0000") ]
  let border_current = style [ Css.border_color Current ]
  let border_inherit = style [ Css.border_color Inherit ]

  (* Per-side border colour emission. *)
  (* The longhand each side names, for a value no typed setter can hold. *)
  let properties_of_side : Side.t -> string list = function
    | Side.Top -> [ "border-top-color" ]
    | Side.Right -> [ "border-right-color" ]
    | Side.Bottom -> [ "border-bottom-color" ]
    | Side.Left -> [ "border-left-color" ]
    | Side.Inline_axis -> [ "border-inline-color" ]
    | Side.Block_axis -> [ "border-block-color" ]
    | Side.Inline_start -> [ "border-inline-start-color" ]
    | Side.Inline_end -> [ "border-inline-end-color" ]
    | Side.Block_start -> [ "border-block-start-color" ]
    | Side.Block_end -> [ "border-block-end-color" ]

  let setters_of_side : Side.t -> (Css.color -> Css.declaration) list = function
    | Side.Top -> [ Css.border_top_color ]
    | Side.Right -> [ Css.border_right_color ]
    | Side.Bottom -> [ Css.border_bottom_color ]
    | Side.Left -> [ Css.border_left_color ]
    | Side.Inline_axis ->
        [ (fun c -> Css.border_inline_color (Css.logical_border_color c)) ]
    | Side.Block_axis ->
        [ (fun c -> Css.border_block_color (Css.logical_border_color c)) ]
    | Side.Inline_start -> [ Css.border_inline_start_color ]
    | Side.Inline_end -> [ Css.border_inline_end_color ]
    | Side.Block_start -> [ Css.border_block_start_color ]
    | Side.Block_end -> [ Css.border_block_end_color ]

  (** Accent color utilities *)

  let accent' ?theme color shade =
    if is_custom_color color then
      let css_color = to_css color shade in
      style [ Css.accent_color css_color ]
    else
      let color_var =
        property_color_var ?theme ~property_prefix:"accent-color" color shade
      in
      let color_value =
        property_color_value ?theme ~property_prefix:"accent-color" color shade
      in
      let decls, color = bound ?theme color_var color_value in
      style (decls @ [ Css.accent_color color ])

  let accent_transparent = style [ Css.accent_color (Css.hex "#0000") ]
  let accent_current = style [ Css.accent_color Current ]
  let accent_inherit = style [ Css.accent_color Inherit ]

  (** Caret color utilities *)

  let caret' ?theme color shade =
    if is_custom_color color then
      let css_color = to_css color shade in
      style [ Css.caret_color css_color ]
    else
      let color_var =
        property_color_var ?theme ~property_prefix:"caret-color" color shade
      in
      let color_value =
        property_color_value ?theme ~property_prefix:"caret-color" color shade
      in
      let decls, color = bound ?theme color_var color_value in
      style (decls @ [ Css.caret_color color ])

  let caret_current = style [ Css.caret_color Current ]
  let caret_inherit = style [ Css.caret_color Inherit ]
  let caret_transparent = style [ Css.caret_color (Css.hex "#0000") ]

  (** Outline color utilities *)

  let outline' ?theme color shade =
    if is_custom_color color then
      let css_color = to_css color shade in
      style [ Css.outline_color css_color ]
    else
      let color_var =
        property_color_var ?theme ~property_prefix:"outline-color" color shade
      in
      let color_value =
        property_color_value ?theme ~property_prefix:"outline-color" color shade
      in
      let decls, color = bound ?theme color_var color_value in
      style (decls @ [ Css.outline_color color ])

  let outline_current = style [ Css.outline_color Current ]
  let outline_inherit = style [ Css.outline_color Inherit ]
  let outline_transparent = style [ Css.outline_color (Css.hex "#0000") ]

  let outline_bracket_var_style v =
    let bare_name = Parse.extract_var_name v in
    style ~merge_key:"outline-"
      [ Css.outline_color (Css.Var (Var.bracket bare_name)) ]

  let outline_bracket_typed_var_style v =
    let bare_name = Parse.extract_var_name v in
    style ~merge_key:"outline-"
      [ Css.outline_color (Css.Var (Var.bracket bare_name)) ]

  (** Text color with opacity *)
  let text_with_opacity ?theme c shade opacity =
    let property_prefix =
      if not (is_custom_color c) then
        let color_name = scheme_color_name c shade in
        let prop_name = "text-color-" ^ color_name in
        if Scheme.theme_value theme prop_name <> None then Some "text-color"
        else None
      else None
    in
    color_with_opacity_style ?theme ~property:Css.color ?property_prefix c shade
      opacity

  (** Border color with opacity *)
  let border_with_opacity ?theme c shade opacity =
    color_with_opacity_style ?theme ~property:Css.border_color c shade opacity

  (** Accent color with opacity *)
  let accent_with_opacity ?theme c shade opacity =
    color_with_opacity_style ?theme ~property:Css.accent_color
      ~property_prefix:"accent-color" c shade opacity

  (** Caret color with opacity *)
  let caret_with_opacity ?theme c shade opacity =
    color_with_opacity_style ?theme ~property:Css.caret_color
      ~property_prefix:"caret-color" c shade opacity

  (** Outline color with opacity *)
  let outline_with_opacity ?theme c shade opacity =
    color_with_opacity_style ?theme ~property:Css.outline_color
      ~property_prefix:"outline-color" c shade opacity

  (** Current color with opacity using color-mix with progressive enhancement *)
  let current_color_with_opacity ~property opacity =
    let property : Css.color -> Css.declaration = property in
    (* Fallback: just currentColor (browsers that don't support color-mix) *)
    let fallback_decl = property Css.Current in
    (* Progressive enhancement: color-mix(in oklab, currentcolor NN%,
       transparent) *)
    let oklab_decl = property (mix_alpha opacity Css.Current) in
    (* Create @supports block with oklab version as top-level rule. Use
       placeholder selector that rule.ml replaces with actual class. *)
    let supports_block = color_mix_supports [ oklab_decl ] in
    style ~rules:(Some [ supports_block ]) [ fallback_decl ]

  let border_side_color_style theme side value =
    let sides = setters_of_side side in
    let apply c = style (List.map (fun set -> set c) sides) in
    match value with
    | Side_color.Named (color, shade) ->
        if is_custom_color color then apply (to_css color shade)
        else
          let color_var = color_var color shade in
          let color_value = token_value ~theme color shade in
          let decls, color = bound ~theme color_var color_value in
          style (decls @ List.map (fun set -> set color) sides)
    | Side_color.Named_opacity (color, shade, opacity) ->
        colors_with_opacity_style ~theme ~properties:sides color shade opacity
    | Side_color.Bracket (_, css_color) ->
        bracket_colors_style ~theme ~properties:sides css_color
    | Side_color.Bracket_opacity (_, css_color, opacity) ->
        bracket_colors_opacity_style ~theme ~properties:sides css_color opacity
    | Side_color.Raw (_, value) ->
        style
          (List.filter_map
             (fun property -> Parse.opaque_declaration property value)
             (properties_of_side side))
    | Side_color.Transparent -> apply (Css.hex "#0000")
    | Side_color.Current -> apply Css.Current
    | Side_color.Current_opacity opacity ->
        let current : Css.color = Css.Current in
        let mixed = mix_alpha opacity current in
        style
          ~rules:
            (Some [ color_mix_supports (List.map (fun set -> set mixed) sides) ])
          (List.map (fun set -> set current) sides)
    | Side_color.Inherit -> apply Css.Inherit

  let outline_bracket_color_opacity_style ~theme inner css_color opacity =
    let merge_key =
      if String.length inner > 0 && inner.[0] = '#' then
        (* Hex bracket colors: strip bracket+opacity for merging *)
        "outline-"
      else
        (* Named bracket colors: unique per variant to prevent merging.
           Different opacity syntaxes (e.g. /50 vs /[0.5]) produce identical CSS
           but Tailwind keeps them separate. *)
        "outline-[" ^ inner ^ "]" ^ opacity_suffix opacity
    in
    bracket_color_opacity_style ~theme ~property:Css.outline_color ~merge_key
      css_color opacity

  let outline_bracket_var_opacity_style v opacity =
    let bare_name = Parse.extract_var_name v in
    let var_color : Css.color = Css.Var (Var.bracket bare_name) in
    let fallback_decl = Css.outline_color var_color in
    let oklab_decl = Css.outline_color (mix_alpha opacity var_color) in
    let supports_block = color_mix_supports [ oklab_decl ] in
    style ~merge_key:"outline-" ~rules:(Some [ supports_block ])
      [ fallback_decl ]

  let outline_bracket_var_opacity v opacity =
    let bare_name = Parse.extract_var_name v in
    let var_color : Css.color = Css.Var (Var.bracket bare_name) in
    let fallback_decl = Css.outline_color var_color in
    let oklab_decl = Css.outline_color (mix_alpha opacity var_color) in
    let supports_block = color_mix_supports [ oklab_decl ] in
    style ~merge_key:"outline-" ~rules:(Some [ supports_block ])
      [ fallback_decl ]

  let with_pseudo pseudo = function
    | Style.Style s -> Style.Style { s with pseudo_suffix = Some pseudo }
    | other -> other

  let to_style theme =
    (* Shadow the scheme-reading colour helpers with theme-applied versions so
       the match arms below read from the explicitly threaded scheme. *)
    let text' color shade = text' ~theme color shade in
    let border_color' color shade = border_color' ~theme color shade in
    let accent' color shade = accent' ~theme color shade in
    let caret' color shade = caret' ~theme color shade in
    let outline' color shade = outline' ~theme color shade in
    let text_with_opacity color shade opacity =
      text_with_opacity ~theme color shade opacity
    in
    let border_with_opacity color shade opacity =
      border_with_opacity ~theme color shade opacity
    in
    let accent_with_opacity color shade opacity =
      accent_with_opacity ~theme color shade opacity
    in
    let caret_with_opacity color shade opacity =
      caret_with_opacity ~theme color shade opacity
    in
    let outline_with_opacity color shade opacity =
      outline_with_opacity ~theme color shade opacity
    in
    function
    (* [Text_opacity] leads so the match resolves to the colour [t] rather than
       to the [Text] constructor [open Css] brings into scope. *)
    | Text_opacity (color, shade, opacity) ->
        text_with_opacity color shade opacity
    | Text (color, shade) -> text' color shade
    | Text_transparent -> text_transparent
    | Text_current -> text_current
    | Text_current_opacity opacity ->
        current_color_with_opacity ~property:Css.color opacity
    | Text_inherit -> text_inherit
    | Text_bracket_color (_orig, css_color) ->
        bracket_color_style ~theme ~merge_key:"text-" ~property:Css.color
          css_color
    | Text_bracket_raw (_, value) ->
        style (Option.to_list (Parse.opaque_declaration "color" value))
    | Text_bracket_color_opacity (_orig, css_color, opacity) ->
        bracket_color_opacity_style ~theme ~property:Css.color css_color opacity
    | Text_bracket_var v ->
        let bare_name = Parse.extract_var_name v in
        style ~merge_key:"text-" [ Css.color (Css.Var (Var.bracket bare_name)) ]
    | Text_bracket_var_opacity (v, opacity) ->
        let bare_name = Parse.extract_var_name v in
        let var_color : Css.color = Css.Var (Var.bracket bare_name) in
        let fallback_decl = Css.color var_color in
        let oklab_decl = Css.color (mix_alpha opacity var_color) in
        let supports_block = color_mix_supports [ oklab_decl ] in
        style ~merge_key:"text-" ~rules:(Some [ supports_block ])
          [ fallback_decl ]
    | Text_bracket_typed_var v ->
        let bare_name = Parse.extract_var_name v in
        style ~merge_key:"text-" [ Css.color (Css.Var (Var.bracket bare_name)) ]
    | Text_bracket_typed_var_opacity (v, opacity) ->
        let bare_name = Parse.extract_var_name v in
        let var_color : Css.color = Css.Var (Var.bracket bare_name) in
        let fallback_decl = Css.color var_color in
        let oklab_decl = Css.color (mix_alpha opacity var_color) in
        let supports_block = color_mix_supports [ oklab_decl ] in
        style ~merge_key:"text-" ~rules:(Some [ supports_block ])
          [ fallback_decl ]
    | Border_side_color (side, value) ->
        border_side_color_style theme side value
    | Border (color, shade) -> border_color' color shade
    | Border_opacity (color, shade, opacity) ->
        border_with_opacity color shade opacity
    | Border_transparent -> border_transparent
    | Border_current -> border_current
    | Border_current_opacity opacity ->
        current_color_with_opacity ~property:Css.border_color opacity
    | Border_inherit -> border_inherit
    | Border_bracket_color (_orig, css_color) ->
        bracket_color_style ~theme ~merge_key:"border-"
          ~property:Css.border_color css_color
    | Border_bracket_raw (_, value) ->
        style (Option.to_list (Parse.opaque_declaration "border-color" value))
    | Border_bracket_color_opacity (_orig, css_color, opacity) ->
        bracket_color_opacity_style ~theme ~property:Css.border_color css_color
          opacity
    | Accent (color, shade) -> accent' color shade
    | Accent_opacity (color, shade, opacity) ->
        accent_with_opacity color shade opacity
    | Accent_transparent -> accent_transparent
    | Accent_current -> accent_current
    | Accent_current_opacity opacity ->
        current_color_with_opacity ~property:Css.accent_color opacity
    | Accent_inherit -> accent_inherit
    | Accent_bracket_color (_orig, css_color) ->
        bracket_color_style ~theme ~merge_key:"accent-"
          ~property:Css.accent_color css_color
    | Accent_bracket_raw (_, value) ->
        style (Option.to_list (Parse.opaque_declaration "accent-color" value))
    | Accent_bracket_color_opacity (_orig, css_color, opacity) ->
        bracket_color_opacity_style ~theme ~property:Css.accent_color css_color
          opacity
    | Caret (color, shade) -> caret' color shade
    | Caret_opacity (color, shade, opacity) ->
        caret_with_opacity color shade opacity
    | Caret_current -> caret_current
    | Caret_current_opacity opacity ->
        current_color_with_opacity ~property:Css.caret_color opacity
    | Caret_inherit -> caret_inherit
    | Caret_transparent -> caret_transparent
    | Caret_bracket_color (_orig, css_color) ->
        bracket_color_style ~theme ~merge_key:"caret-" ~property:Css.caret_color
          css_color
    | Caret_bracket_raw (_, value) ->
        style (Option.to_list (Parse.opaque_declaration "caret-color" value))
    | Caret_bracket_color_opacity (_orig, css_color, opacity) ->
        bracket_color_opacity_style ~theme ~property:Css.caret_color css_color
          opacity
    | Outline (color, shade) -> outline' color shade
    | Outline_opacity (color, shade, opacity) ->
        outline_with_opacity color shade opacity
    | Outline_current -> outline_current
    | Outline_current_opacity opacity ->
        current_color_with_opacity ~property:Css.outline_color opacity
    | Outline_inherit -> outline_inherit
    | Outline_transparent -> outline_transparent
    | Outline_bracket_color (_orig, css_color) ->
        bracket_color_style ~theme ~merge_key:"outline-"
          ~property:Css.outline_color css_color
    | Outline_bracket_raw (_, value) ->
        style (Option.to_list (Parse.opaque_declaration "outline-color" value))
    | Outline_bracket_color_opacity (inner, css_color, opacity) ->
        outline_bracket_color_opacity_style ~theme inner css_color opacity
    | Outline_bracket_var v -> outline_bracket_var_style v
    | Outline_bracket_var_opacity (v, opacity) ->
        outline_bracket_var_opacity_style v opacity
    | Outline_bracket_typed_var v -> outline_bracket_typed_var_style v
    | Outline_bracket_typed_var_opacity (v, opacity) ->
        outline_bracket_var_opacity v opacity
    | Placeholder (color, shade) ->
        with_pseudo Css.Selector.Placeholder (text' color shade)
    | Placeholder_opacity (color, shade, opacity) ->
        if is_fully_opaque opacity && not (is_custom_color color) then
          with_pseudo Css.Selector.Placeholder (text' color shade)
        else
          with_pseudo Css.Selector.Placeholder
            (text_with_opacity color shade opacity)
    | Placeholder_transparent ->
        with_pseudo Css.Selector.Placeholder text_transparent
    | Placeholder_current -> with_pseudo Css.Selector.Placeholder text_current
    | Placeholder_current_opacity opacity ->
        with_pseudo Css.Selector.Placeholder
          (current_color_with_opacity ~property:Css.color opacity)
    | Placeholder_inherit -> with_pseudo Css.Selector.Placeholder text_inherit
    | Placeholder_bracket_color (_orig, css_color) ->
        with_pseudo Css.Selector.Placeholder
          (bracket_color_style ~theme ~property:Css.color css_color)
    | Placeholder_bracket_raw (_, value) ->
        with_pseudo Css.Selector.Placeholder
          (style (Option.to_list (Parse.opaque_declaration "color" value)))
    | Placeholder_bracket_color_opacity (_orig, css_color, opacity) ->
        with_pseudo Css.Selector.Placeholder
          (bracket_color_opacity_style ~theme ~property:Css.color css_color
             opacity)

  (* Suborder for the non-text color families: border first (0-9999), then the
     rest. text-color runs at priority 26 (see [priority]) with a fixed suborder
     inside the late-typography block. *)
  let suborder = function
    (* [Text_opacity] leads so the match resolves to the colour [t] rather than
       to the [Text] constructor [open Css] brings into scope. All text colors
       share suborder 8350 (priority 26, after white-space and before
       text-transform) so they sort alphabetically, matching Tailwind. *)
    | Text_opacity (color, shade, _) ->
        let _ = (color, shade) in
        8350
    | Text (color, shade) ->
        let _ = (color, shade) in
        8350
    | Text_transparent -> 8350
    | Text_current -> 8350
    | Text_current_opacity _ -> 8350
    | Text_inherit -> 8350
    | Text_bracket_color _ | Text_bracket_raw _ -> 8350
    | Text_bracket_color_opacity _ -> 8350
    | Text_bracket_var _ -> 8350
    | Text_bracket_var_opacity _ -> 8350
    | Text_bracket_typed_var _ -> 8350
    | Text_bracket_typed_var_opacity _ -> 8350
    | Border (color, shade) ->
        (* Border colors share suborder 1500 with borders.ml's named border
           colors (Border_color), at priority 19, so named and arbitrary tie and
           sort together by class name - matching Tailwind. *)
        let _ = (color, shade) in
        1500
    | Border_opacity (color, shade, _) ->
        let _ = (color, shade) in
        1500
    | Border_transparent -> 1500
    | Border_current -> 1500
    | Border_current_opacity _ -> 1500
    | Border_inherit -> 1500
    | Border_bracket_color _ | Border_bracket_raw _ -> 1500
    | Border_bracket_color_opacity _ -> 1500
    (* Per-side border colours sort after the all-sides ones, and side-major:
       every colour a side names writes a colour another side writes too, so
       their order decides which one wins. Tailwind runs the axes and the
       logical sides first, then the physical ones. Within a side the colours
       tie and sort by class name, so one slot per side is enough; they stay ten
       apart to leave room. *)
    | Border_side_color (side, _) -> (
        match side with
        | Side.Inline_axis -> 1600
        | Side.Block_axis -> 1610
        | Side.Inline_start -> 1620
        | Side.Inline_end -> 1630
        | Side.Block_start -> 1640
        | Side.Block_end -> 1650
        | Side.Top -> 1660
        | Side.Right -> 1670
        | Side.Bottom -> 1680
        | Side.Left -> 1690)
    (* The three colour families that close the late-typography block, in
       Tailwind's order: placeholder, then caret, then accent, all after the
       underline offset (max 69999). Each family shares one suborder so its
       members tie and sort alphabetically by class name. *)
    | Placeholder _ -> 80000
    | Placeholder_opacity _ -> 80000
    | Placeholder_transparent -> 80000
    | Placeholder_current -> 80000
    | Placeholder_current_opacity _ -> 80000
    | Placeholder_inherit -> 80000
    | Placeholder_bracket_color _ | Placeholder_bracket_raw _ -> 80000
    | Placeholder_bracket_color_opacity _ -> 80000
    | Caret (color, shade) ->
        let _ = (color, shade) in
        81000
    | Caret_opacity (color, shade, _) ->
        let _ = (color, shade) in
        81000
    | Caret_current -> 81000
    | Caret_current_opacity _ -> 81000
    | Caret_inherit -> 81000
    | Caret_transparent -> 81000
    | Caret_bracket_color _ | Caret_bracket_raw _ -> 81000
    | Caret_bracket_color_opacity _ -> 81000
    | Accent (color, shade) ->
        let _ = (color, shade) in
        82000
    | Accent_opacity (color, shade, _) ->
        let _ = (color, shade) in
        82000
    | Accent_transparent -> 82000
    | Accent_current -> 82000
    | Accent_current_opacity _ -> 82000
    | Accent_inherit -> 82000
    | Accent_bracket_color _ | Accent_bracket_raw _ -> 82000
    | Accent_bracket_color_opacity _ -> 82000
    (* Outline colors run at priority 28 with the rest of the outline family
       (see [priority]): the 3000 base puts them after borders.ml's outline
       width (1999-2010) and offset (2200-2299) and before its outline styles
       (30000-30004). *)
    | Outline (color, shade) ->
        let base =
          if is_base_color color then
            suborder_with_shade (color_to_string color)
          else
            suborder_with_shade
              (color_to_string color ^ "-" ^ string_of_int shade)
        in
        3000 + base
    | Outline_opacity (color, shade, _) ->
        let base =
          if is_base_color color then
            suborder_with_shade (color_to_string color)
          else
            suborder_with_shade
              (color_to_string color ^ "-" ^ string_of_int shade)
        in
        3000 + base
    | Outline_current ->
        3000 + (4 * 1000) (* c -> between cyan(4) and emerald(5) *)
    | Outline_current_opacity _ -> 3000 + (4 * 1000)
    | Outline_inherit -> 3000 + (9 * 1000) + 999
    (* i -> after every indigo shade and before lime(10) *)
    | Outline_transparent -> 3000 + (22 * 1000)
    (* t -> between teal and violet *)
    | Outline_bracket_color _ | Outline_bracket_raw _ -> 3000
    | Outline_bracket_color_opacity _ -> 3000
    | Outline_bracket_var _ -> 3000
    | Outline_bracket_var_opacity _ -> 3000
    | Outline_bracket_typed_var _ -> 3000
    | Outline_bracket_typed_var_opacity _ -> 3000

  let to_class = function
    (* [Text_opacity] leads so the match resolves to the colour [t] rather than
       to the [Text] constructor [open Css] brings into scope. *)
    | Text_opacity (c, shade, opacity) ->
        if is_shadeless c then
          "text-" ^ color_to_string c ^ opacity_suffix opacity
        else
          "text-" ^ color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Text (c, shade) ->
        if is_shadeless c then "text-" ^ color_to_string c
        else "text-" ^ color_to_string c ^ "-" ^ string_of_int shade
    | Text_transparent -> "text-transparent"
    | Text_current -> "text-current"
    | Text_current_opacity opacity -> "text-current" ^ opacity_suffix opacity
    | Text_inherit -> "text-inherit"
    | Text_bracket_color (v, _) | Text_bracket_raw (v, _) -> "text-[" ^ v ^ "]"
    | Text_bracket_color_opacity (v, _, opacity) ->
        "text-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Text_bracket_var v -> "text-[" ^ v ^ "]"
    | Text_bracket_var_opacity (v, opacity) ->
        "text-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Text_bracket_typed_var v -> "text-[color:" ^ v ^ "]"
    | Text_bracket_typed_var_opacity (v, opacity) ->
        "text-[color:" ^ v ^ "]" ^ opacity_suffix opacity
    | Border (c, shade) ->
        if is_shadeless c then "border-" ^ color_to_string c
        else "border-" ^ color_to_string c ^ "-" ^ string_of_int shade
    | Border_opacity (c, shade, opacity) ->
        if is_shadeless c then
          "border-" ^ color_to_string c ^ opacity_suffix opacity
        else
          "border-" ^ color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Border_transparent -> "border-transparent"
    | Border_current -> "border-current"
    | Border_current_opacity opacity ->
        "border-current" ^ opacity_suffix opacity
    | Border_inherit -> "border-inherit"
    | Border_bracket_color (v, _) | Border_bracket_raw (v, _) ->
        "border-[" ^ v ^ "]"
    | Border_side_color (side, value) ->
        let s =
          match side with
          | Side.Top -> "t"
          | Side.Right -> "r"
          | Side.Bottom -> "b"
          | Side.Left -> "l"
          | Side.Inline_axis -> "x"
          | Side.Block_axis -> "y"
          | Side.Inline_start -> "s"
          | Side.Inline_end -> "e"
          | Side.Block_start -> "bs"
          | Side.Block_end -> "be"
        in
        let v =
          match value with
          | Side_color.Named (c, shade) ->
              if is_shadeless c then color_to_string c
              else color_to_string c ^ "-" ^ string_of_int shade
          | Side_color.Named_opacity (c, shade, opacity) ->
              (if is_shadeless c then color_to_string c
               else color_to_string c ^ "-" ^ string_of_int shade)
              ^ opacity_suffix opacity
          | Side_color.Bracket (orig, _) | Side_color.Raw (orig, _) ->
              "[" ^ orig ^ "]"
          | Side_color.Bracket_opacity (orig, _, opacity) ->
              "[" ^ orig ^ "]" ^ opacity_suffix opacity
          | Side_color.Transparent -> "transparent"
          | Side_color.Current -> "current"
          | Side_color.Current_opacity opacity ->
              "current" ^ opacity_suffix opacity
          | Side_color.Inherit -> "inherit"
        in
        "border-" ^ s ^ "-" ^ v
    | Border_bracket_color_opacity (v, _, opacity) ->
        "border-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Accent (c, shade) ->
        if is_shadeless c then "accent-" ^ color_to_string c
        else "accent-" ^ color_to_string c ^ "-" ^ string_of_int shade
    | Accent_opacity (c, shade, opacity) ->
        if is_shadeless c then
          "accent-" ^ color_to_string c ^ opacity_suffix opacity
        else
          "accent-" ^ color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Accent_transparent -> "accent-transparent"
    | Accent_current -> "accent-current"
    | Accent_current_opacity opacity ->
        "accent-current" ^ opacity_suffix opacity
    | Accent_inherit -> "accent-inherit"
    | Accent_bracket_color (v, _) | Accent_bracket_raw (v, _) ->
        "accent-[" ^ v ^ "]"
    | Accent_bracket_color_opacity (v, _, opacity) ->
        "accent-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Caret (c, shade) ->
        if is_shadeless c then "caret-" ^ color_to_string c
        else "caret-" ^ color_to_string c ^ "-" ^ string_of_int shade
    | Caret_opacity (c, shade, opacity) ->
        if is_shadeless c then
          "caret-" ^ color_to_string c ^ opacity_suffix opacity
        else
          "caret-" ^ color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Caret_current -> "caret-current"
    | Caret_current_opacity opacity -> "caret-current" ^ opacity_suffix opacity
    | Caret_inherit -> "caret-inherit"
    | Caret_transparent -> "caret-transparent"
    | Caret_bracket_color (v, _) | Caret_bracket_raw (v, _) ->
        "caret-[" ^ v ^ "]"
    | Caret_bracket_color_opacity (v, _, opacity) ->
        "caret-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Outline (c, shade) ->
        if is_shadeless c then "outline-" ^ color_to_string c
        else "outline-" ^ color_to_string c ^ "-" ^ string_of_int shade
    | Outline_opacity (c, shade, opacity) ->
        if is_shadeless c then
          "outline-" ^ color_to_string c ^ opacity_suffix opacity
        else
          "outline-" ^ color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Outline_current -> "outline-current"
    | Outline_current_opacity opacity ->
        "outline-current" ^ opacity_suffix opacity
    | Outline_inherit -> "outline-inherit"
    | Outline_transparent -> "outline-transparent"
    | Outline_bracket_color (v, _) | Outline_bracket_raw (v, _) ->
        "outline-[" ^ v ^ "]"
    | Outline_bracket_color_opacity (v, _, opacity) ->
        "outline-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Outline_bracket_var v -> "outline-[" ^ v ^ "]"
    | Outline_bracket_var_opacity (v, opacity) ->
        "outline-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Outline_bracket_typed_var v -> "outline-[color:" ^ v ^ "]"
    | Outline_bracket_typed_var_opacity (v, opacity) ->
        "outline-[color:" ^ v ^ "]" ^ opacity_suffix opacity
    | Placeholder (c, shade) ->
        if is_shadeless c then "placeholder-" ^ color_to_string c
        else "placeholder-" ^ color_to_string c ^ "-" ^ string_of_int shade
    | Placeholder_opacity (c, shade, opacity) ->
        if is_shadeless c then
          "placeholder-" ^ color_to_string c ^ opacity_suffix opacity
        else
          "placeholder-" ^ color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Placeholder_transparent -> "placeholder-transparent"
    | Placeholder_current -> "placeholder-current"
    | Placeholder_current_opacity opacity ->
        "placeholder-current" ^ opacity_suffix opacity
    | Placeholder_inherit -> "placeholder-inherit"
    | Placeholder_bracket_color (v, _) | Placeholder_bracket_raw (v, _) ->
        "placeholder-[" ^ v ^ "]"
    | Placeholder_bracket_color_opacity (v, _, opacity) ->
        "placeholder-[" ^ v ^ "]" ^ opacity_suffix opacity

  let examples =
    [
      Text_transparent;
      Border_transparent;
      Outline_transparent;
      Accent_transparent;
      Caret_transparent;
      Placeholder_transparent;
    ]
end

open Handler

module Utility_factory = Utility.Make (Handler)
(** Register color handler with Utility system *)

(** Public API *)
let utility = Utility_factory.v

let text ?opacity ?(shade = 500) color =
  check_shade ~utility:"text" color shade;
  match opacity with
  | None -> utility (Text (color, shade))
  | Some pct -> utility (Text_opacity (color, shade, opacity_of_int pct))

let border_color ?opacity ?(shade = 500) color =
  check_shade ~utility:"border_color" color shade;
  match opacity with
  | None -> utility (Border (color, shade))
  | Some pct -> utility (Border_opacity (color, shade, opacity_of_int pct))

let text_transparent = utility Text_transparent
let text_current = utility Text_current
let text_inherit = utility Text_inherit
let border_transparent = utility Border_transparent
let border_current = utility Border_current
let border_inherit = utility Border_inherit

let outline_color ?opacity ?(shade = 500) color =
  check_shade ~utility:"outline_color" color shade;
  match opacity with
  | None -> utility (Outline (color, shade))
  | Some pct -> utility (Outline_opacity (color, shade, opacity_of_int pct))

let outline_transparent = utility Outline_transparent
let outline_current = utility Outline_current
let outline_inherit = utility Outline_inherit

let accent ?opacity ?(shade = 500) color =
  check_shade ~utility:"accent" color shade;
  match opacity with
  | None -> utility (Accent (color, shade))
  | Some pct -> utility (Accent_opacity (color, shade, opacity_of_int pct))

let accent_current = utility Accent_current
let accent_inherit = utility Accent_inherit

let caret ?opacity ?(shade = 500) color =
  check_shade ~utility:"caret" color shade;
  match opacity with
  | None -> utility (Caret (color, shade))
  | Some pct -> utility (Caret_opacity (color, shade, opacity_of_int pct))

let caret_current = utility Caret_current
let caret_inherit = utility Caret_inherit
let caret_transparent = utility Caret_transparent
