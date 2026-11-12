(** Text decoration utilities: the decoration line, colour, style and thickness,
    and the underline offset. *)

module Css = Cascade.Css

let underline_offset_auto_var =
  Var.theme Css.Length "text-underline-offset-auto" ~order:(6, 55)

module Handler = struct
  open Style
  open Css

  type t =
    | (* Decoration color *)
      Decoration_color of Color.color * int option
    | Decoration_color_opacity of Color.color * int * Color.opacity_modifier
    | Decoration_transparent
    | Decoration_current
    | Decoration_current_opacity of Color.opacity_modifier
    | Decoration_inherit
    | Decoration_bracket_color of string * Css.color
    | Decoration_bracket_invalid_color of string
    | Decoration_bracket_color_opacity of
        string * Css.color * Color.opacity_modifier
    | Decoration_bracket_var of string
    | Decoration_bracket_var_opacity of string * Color.opacity_modifier
    | Decoration_bracket_color_var of string
    | Decoration_bracket_color_var_opacity of string * Color.opacity_modifier
    | (* Text decoration lines *)
      Underline
    | Overline
    | Line_through
    | No_underline
    | (* Decoration styles *)
      Decoration_solid
    | Decoration_double
    | Decoration_dotted
    | Decoration_dashed
    | Decoration_wavy
    | Decoration_thickness of int
    | Decoration_from_font
    | Decoration_auto
    | Decoration_bracket_thickness of string * Css.length
    | Decoration_bracket_pct of string * Css.length
    | Decoration_bracket_length_var of string
    | Decoration_bracket_pct_var of string
    | (* Underline offset *)
      Underline_offset_auto
    | Underline_offset_0
    | Underline_offset_1
    | Underline_offset_2
    | Underline_offset_4
    | Underline_offset_8
    | Underline_offset_px of float
    | Underline_offset_arbitrary of string * Css.length
    | Underline_offset_raw of string * string
    | Underline_offset_var of string
    | Underline_offset_neg_px of float
    | Underline_offset_neg_arbitrary of string * Css.length
    | Underline_offset_neg_var of string

  let name = "text_decoration"

  (* Tailwind's tail runs text-transform, font-style and text-decoration-line,
     then the decoration colour, style and thickness and the underline offset,
     all in the priority-26 band the late text families share. *)
  let priority _ = 26
  let err_not_utility = Error (`Msg "Not a text-decoration utility")

  (* Tailwind reads the bracket as a colour unless it is a length or a
     percentage, so a bare number lands on the colour branch whether it is
     written [2] or [1.5]. *)
  let is_bare_number str =
    let is_number_char c = (c >= '0' && c <= '9') || c = '.' || c = '-' in
    str <> ""
    && String.for_all is_number_char str
    && float_of_string_opt str <> None

  (* A data-type hint chooses which longhand a bracket lands in and says nothing
     about the value. [indent-] and [underline-offset-] each write one longhand,
     so every hint lands there and the length reader is handed what follows it.
     The bracket text kept on the constructor still carries the hint, because
     the class name is what the markup holds. *)
  let arbitrary_length_after_hint inner : Css.length option =
    Option.bind (Parse.value_after_hint inner) Parse.arbitrary_length

  (* An arbitrary decoration thickness is any CSS length. *)
  let parse_decoration_thickness inner : Css.length option =
    Parse.arbitrary_length inner

  (* Tailwind's generator writes the percentage through and its minifier folds
     it to em, 50% -> .5em, which is the spelling its own test corpus records
     and the one [--diff] compares. Reading the unminified sheet instead reports
     a difference that is the minifier's. *)
  let parse_decoration_pct inner : Css.length option =
    match Parse.arbitrary_length inner with
    | Some (Pct pct) -> Some (Em (pct /. 100.))
    | _ -> None

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "underline" ] -> Ok Underline
    | [ "overline" ] -> Ok Overline
    | [ "line"; "through" ] -> Ok Line_through
    | [ "no"; "underline" ] -> Ok No_underline
    | [ "decoration"; "solid" ] -> Ok Decoration_solid
    | [ "decoration"; "double" ] -> Ok Decoration_double
    | [ "decoration"; "dotted" ] -> Ok Decoration_dotted
    | [ "decoration"; "dashed" ] -> Ok Decoration_dashed
    | [ "decoration"; "wavy" ] -> Ok Decoration_wavy
    | [ "decoration"; "from"; "font" ] -> Ok Decoration_from_font
    | [ "decoration"; "auto" ] -> Ok Decoration_auto
    | [ "decoration"; "transparent" ] -> Ok Decoration_transparent
    | [ "decoration"; "inherit" ] -> Ok Decoration_inherit
    | [ "decoration"; n ] -> (
        (* Parse opacity modifier first *)
        let base_str, opacity = Color.parse_opacity_modifier ~theme n in
        (* Check for "current" with optional opacity *)
        match (base_str, opacity) with
        | "current", Color.No_opacity -> Ok Decoration_current
        | "current", opacity -> Ok (Decoration_current_opacity opacity)
        | _ -> (
            if
              (* Check for bracket values *)
              Parse.is_bracket_value base_str
            then
              let inner = Parse.bracket_inner base_str in
              (* [raw] is the bracket text the class was written with, hint
                 included, so the class prints back the way it was read. *)
              let bracket_color raw value =
                match Color.parse_bracket_color value with
                | Some c -> (
                    match opacity with
                    | Color.No_opacity -> Ok (Decoration_bracket_color (raw, c))
                    | _ ->
                        Ok (Decoration_bracket_color_opacity (raw, c, opacity)))
                | None -> err_not_utility
              in
              let bracket_pct raw value =
                match parse_decoration_pct value with
                | Some len -> Ok (Decoration_bracket_pct (raw, len))
                | None -> err_not_utility
              in
              let bracket_thickness raw value =
                if
                  String.length value > 0
                  && value.[String.length value - 1] = '%'
                then bracket_pct raw value
                else
                  match parse_decoration_thickness value with
                  | Some len -> Ok (Decoration_bracket_thickness (raw, len))
                  | None -> err_not_utility
              in
              (* A data-type hint says how to read the value written after it,
                 so only a var() reference there names a custom property. *)
              if String.starts_with ~prefix:"color:" inner then
                let value = String.sub inner 6 (String.length inner - 6) in
                if Parse.is_var value then
                  match opacity with
                  | Color.No_opacity -> Ok (Decoration_bracket_color_var value)
                  | _ ->
                      Ok (Decoration_bracket_color_var_opacity (value, opacity))
                else bracket_color inner value
              else if String.starts_with ~prefix:"var(" inner then
                match opacity with
                | Color.No_opacity -> Ok (Decoration_bracket_var inner)
                | _ -> Ok (Decoration_bracket_var_opacity (inner, opacity))
              else if String.starts_with ~prefix:"length:" inner then
                let value = String.sub inner 7 (String.length inner - 7) in
                if Parse.is_var value then
                  Ok (Decoration_bracket_length_var value)
                else bracket_thickness inner value
              else if String.starts_with ~prefix:"percentage:" inner then
                let value = String.sub inner 11 (String.length inner - 11) in
                if Parse.is_var value then Ok (Decoration_bracket_pct_var value)
                else bracket_pct inner value
              else
                (* Every colour spelling CSS knows, not only a [#] hex and a
                   colour function: a named colour and a keyword name a
                   decoration colour too. The colour reader runs before the
                   percentage one because [hsl(200_50%_50%)] ends in a [%]. *)
                match Color.parse_bracket_color inner with
                | Some _ -> bracket_color inner inner
                | None -> (
                    if
                      String.length inner > 0
                      && inner.[String.length inner - 1] = '%'
                    then
                      match parse_decoration_pct inner with
                      | Some len -> Ok (Decoration_bracket_pct (inner, len))
                      | None -> err_not_utility
                    else if is_bare_number inner then
                      (* Tailwind resolves the ambiguous unitless spelling
                         through the colour candidate path. Its declaration is
                         invalid CSS and browsers discard it; accepting an inert
                         candidate is therefore equivalent, while treating it as
                         px is not. *)
                      Ok (Decoration_bracket_invalid_color inner)
                    else
                      match parse_decoration_thickness inner with
                      | Some len ->
                          Ok (Decoration_bracket_thickness (inner, len))
                      | None -> err_not_utility)
            else
              (* Try parsing as number (decoration thickness) *)
              match Parse.int_any base_str with
              | Ok i when opacity = Color.No_opacity ->
                  Ok (Decoration_thickness i)
              | _ -> (
                  (* A shadeless colour (white, black, a theme name) carries its
                     opacity on the single segment, so read the segment as a
                     colour and an opacity together. *)
                  match Color.shade_and_opacity_of_strings ~theme [ n ] with
                  | Ok (c, _, Color.No_opacity) ->
                      Ok (Decoration_color (c, None))
                  | Ok (c, shade, opacity) ->
                      Ok (Decoration_color_opacity (c, shade, opacity))
                  | Error _ -> err_not_utility)))
    | [ "decoration"; color; shade ] -> (
        match Color.shade_and_opacity_of_strings ~theme [ color; shade ] with
        | Ok (c, s, Color.No_opacity) -> Ok (Decoration_color (c, Some s))
        | Ok (c, s, opacity) -> Ok (Decoration_color_opacity (c, s, opacity))
        | Error _ -> err_not_utility)
    | [ "underline"; "offset"; "auto" ] -> Ok Underline_offset_auto
    | [ "underline"; "offset"; "0" ] -> Ok Underline_offset_0
    | [ "underline"; "offset"; "1" ] -> Ok Underline_offset_1
    | [ "underline"; "offset"; "2" ] -> Ok Underline_offset_2
    | [ "underline"; "offset"; "4" ] -> Ok Underline_offset_4
    | [ "underline"; "offset"; "8" ] -> Ok Underline_offset_8
    | [ "underline"; "offset"; n ] when Parse.is_bracket_value n -> (
        let inner = Parse.bracket_inner n in
        if Parse.is_var inner then Ok (Underline_offset_var inner)
        else
          match arbitrary_length_after_hint inner with
          | Some len -> Ok (Underline_offset_arbitrary (inner, len))
          | None -> (
              match Parse.arbitrary_declaration_value inner with
              | Some raw -> Ok (Underline_offset_raw (inner, raw))
              | None -> err_not_utility))
    | [ "underline"; "offset"; n ] -> (
        match Parse.decimal_float n with
        | Some px -> Ok (Underline_offset_px px)
        | None -> err_not_utility)
    | [ ""; "underline"; "offset"; n ] when Parse.is_bracket_value n -> (
        let inner = Parse.bracket_inner n in
        if Parse.is_var inner then Ok (Underline_offset_neg_var inner)
        else
          match arbitrary_length_after_hint inner with
          | Some len -> Ok (Underline_offset_neg_arbitrary (inner, len))
          | None -> err_not_utility)
    | [ ""; "underline"; "offset"; n ] -> (
        match Parse.decimal_float n with
        | Some px -> Ok (Underline_offset_neg_px px)
        | None -> err_not_utility)
    | _ -> err_not_utility

  let to_class = function
    | Decoration_color (color, None) -> "decoration-" ^ Color.pp color
    | Decoration_color (color, Some shade) ->
        "decoration-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | Decoration_color_opacity (color, shade, opacity) ->
        let base =
          if Color.is_shadeless color then "decoration-" ^ Color.pp color
          else "decoration-" ^ Color.pp color ^ "-" ^ string_of_int shade
        in
        base ^ "/" ^ Color.pp_opacity opacity
    | Decoration_transparent -> "decoration-transparent"
    | Decoration_current -> "decoration-current"
    | Decoration_current_opacity opacity ->
        "decoration-current/" ^ Color.pp_opacity opacity
    | Decoration_inherit -> "decoration-inherit"
    | Decoration_bracket_color (v, _) -> "decoration-[" ^ v ^ "]"
    | Decoration_bracket_invalid_color v -> "decoration-[" ^ v ^ "]"
    | Decoration_bracket_color_opacity (v, _, opacity) ->
        "decoration-[" ^ v ^ "]/" ^ Color.pp_opacity opacity
    | Decoration_bracket_var v -> "decoration-[" ^ v ^ "]"
    | Decoration_bracket_var_opacity (v, opacity) ->
        "decoration-[" ^ v ^ "]/" ^ Color.pp_opacity opacity
    | Decoration_bracket_color_var v -> "decoration-[color:" ^ v ^ "]"
    | Decoration_bracket_color_var_opacity (v, opacity) ->
        "decoration-[color:" ^ v ^ "]/" ^ Color.pp_opacity opacity
    | Underline -> "underline"
    | Overline -> "overline"
    | Line_through -> "line-through"
    | No_underline -> "no-underline"
    | Decoration_solid -> "decoration-solid"
    | Decoration_double -> "decoration-double"
    | Decoration_dotted -> "decoration-dotted"
    | Decoration_dashed -> "decoration-dashed"
    | Decoration_wavy -> "decoration-wavy"
    | Decoration_thickness n -> "decoration-" ^ string_of_int n
    | Decoration_from_font -> "decoration-from-font"
    | Decoration_auto -> "decoration-auto"
    | Decoration_bracket_thickness (v, _) -> "decoration-[" ^ v ^ "]"
    | Decoration_bracket_pct (v, _) -> "decoration-[" ^ v ^ "]"
    | Decoration_bracket_length_var v -> "decoration-[length:" ^ v ^ "]"
    | Decoration_bracket_pct_var v -> "decoration-[percentage:" ^ v ^ "]"
    | Underline_offset_auto -> "underline-offset-auto"
    | Underline_offset_0 -> "underline-offset-0"
    | Underline_offset_1 -> "underline-offset-1"
    | Underline_offset_2 -> "underline-offset-2"
    | Underline_offset_4 -> "underline-offset-4"
    | Underline_offset_8 -> "underline-offset-8"
    | Underline_offset_px px ->
        let s = string_of_float px in
        let s =
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        "underline-offset-" ^ s
    | Underline_offset_arbitrary (raw, _) | Underline_offset_raw (raw, _) ->
        "underline-offset-[" ^ raw ^ "]"
    | Underline_offset_var v -> "underline-offset-[" ^ v ^ "]"
    | Underline_offset_neg_px px ->
        let s = string_of_float px in
        let s =
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        "-underline-offset-" ^ s
    | Underline_offset_neg_arbitrary (raw, _) ->
        "-underline-offset-[" ^ raw ^ "]"
    | Underline_offset_neg_var v -> "-underline-offset-[" ^ v ^ "]"

  let suborder = function
    (* Text decoration lines - suborder >= 8000 (alphabetical) *)
    | Line_through -> 8400
    | No_underline -> 8401
    | Overline -> 8402
    | Underline -> 8403
    (* The decoration properties follow the line, in Tailwind's own order:
       colour, then style, then thickness. All the colour variants share a
       suborder so the tie is broken alphabetically by class name. *)
    | Decoration_bracket_color _ -> 8410
    | Decoration_bracket_invalid_color _ -> 8410
    | Decoration_bracket_color_opacity _ -> 8410
    | Decoration_bracket_color_var _ -> 8411
    | Decoration_bracket_color_var_opacity _ -> 8411
    | Decoration_bracket_var _ -> 8412
    | Decoration_bracket_var_opacity _ -> 8412
    | Decoration_color _ -> 8413
    | Decoration_color_opacity _ -> 8413
    | Decoration_transparent -> 8413
    | Decoration_current -> 8413
    | Decoration_current_opacity _ -> 8413
    | Decoration_inherit -> 8413
    (* Decoration styles - same suborder, sorted by class name *)
    | Decoration_solid -> 8420
    | Decoration_double -> 8420
    | Decoration_dotted -> 8420
    | Decoration_dashed -> 8420
    | Decoration_wavy -> 8420
    (* Decoration thickness - numeric values by n, then brackets, then
       auto/from-font. The numeric band is open-ended (the class carries any
       integer), so the brackets sit far enough above it that no thickness a
       stylesheet would ask for reaches them, and below the underline offset. *)
    | Decoration_thickness n -> 8430 + n
    | Decoration_bracket_thickness _ -> 30000
    | Decoration_bracket_pct _ -> 30000
    | Decoration_bracket_length_var _ -> 30000
    | Decoration_bracket_pct_var _ -> 30000
    | Decoration_auto -> 30010
    | Decoration_from_font -> 30010
    (* Underline offset — negatives first, then positives, then auto *)
    | Underline_offset_neg_px px -> 50000 + int_of_float px
    | Underline_offset_neg_arbitrary _ -> 59989
    | Underline_offset_neg_var _ -> 59990
    | Underline_offset_0 -> 60000
    | Underline_offset_1 -> 60001
    | Underline_offset_2 -> 60002
    | Underline_offset_4 -> 60003
    | Underline_offset_8 -> 60004
    | Underline_offset_px px -> 60005 + int_of_float px
    | Underline_offset_arbitrary _ | Underline_offset_raw _ -> 69989
    | Underline_offset_var _ -> 69990
    | Underline_offset_auto -> 69999

  let underline = style [ text_decoration_line Underline ]
  let overline = style [ text_decoration_line Overline ]
  let line_through = style [ text_decoration_line Line_through ]
  let no_underline = style [ text_decoration_line None ]
  let decoration_solid = style [ text_decoration_style Solid ]
  let decoration_double = style [ text_decoration_style Double ]
  let decoration_dotted = style [ text_decoration_style Dotted ]
  let decoration_dashed = style [ text_decoration_style Dashed ]
  let decoration_wavy = style [ text_decoration_style Wavy ]

  let decoration_thickness n =
    style [ text_decoration_thickness (Px (float_of_int n)) ]

  let decoration_from_font = style [ text_decoration_thickness From_font ]
  let decoration_auto = style [ text_decoration_thickness Auto ]

  let decoration_bracket_thickness (len : Css.length) =
    style [ text_decoration_thickness len ]

  let decoration_bracket_pct (len : Css.length) =
    style [ text_decoration_thickness len ]

  let decoration_bracket_length_var v =
    let bare_name = Parse.extract_var_name v in
    let var_ref : Css.length Css.var = Var.bracket bare_name in
    style [ text_decoration_thickness (Var var_ref) ]

  let decoration_bracket_pct_var = decoration_bracket_length_var

  let decoration_color ?theme ?(shade = 500) (color : Color.color) =
    if Color.is_custom_color color then
      let css_color = Color.to_css color shade in
      style
        [
          webkit_text_decoration_color css_color;
          text_decoration_color css_color;
        ]
    else
      let color_var =
        Color.property_color_var ?theme ~property_prefix:"text-decoration-color"
          color shade
      in
      let color_value =
        Color.property_color_value ?theme
          ~property_prefix:"text-decoration-color" color shade
      in
      let decls, color = Color.bound ?theme color_var color_value in
      style
        (decls
        @ [ webkit_text_decoration_color color; text_decoration_color color ])

  let decoration_color_with_opacity ?theme (color : Color.color) shade opacity =
    let percent = Color.opacity_to_percent opacity in
    let scheme = Scheme.or_default theme in
    let color_name = Color.scheme_color_name color shade in
    match Color.opacity_keyword color with
    | Some keyword ->
        let color = Color.apply_alpha opacity keyword in
        style
          [ webkit_text_decoration_color color; text_decoration_color color ]
    | None -> (
        match Scheme.hex_color scheme color_name with
        | Some hex_value ->
            (* Scheme has hex: fallback is hex+alpha, @supports has color-mix
               with webkit *)
            let hex_with_alpha = Color.hex_with_alpha hex_value percent in
            let fallback_decl =
              text_decoration_color (Css.hex hex_with_alpha)
            in
            let decls, color =
              Color.bound ?theme
                (Color.color_var color shade)
                (Css.hex hex_value)
            in
            let oklab_color = Color.mix_alpha opacity color in
            let webkit_decl = webkit_text_decoration_color oklab_color in
            let oklab_decl = text_decoration_color oklab_color in
            let supports_block =
              Color.color_mix_supports [ webkit_decl; oklab_decl ]
            in
            style ~rules:(Some [ supports_block ]) (decls @ [ fallback_decl ])
        | None ->
            (* No scheme hex: use property-scoped variable *)
            let color_var =
              Color.property_color_var ?theme
                ~property_prefix:"text-decoration-color" color shade
            in
            let color_value =
              Color.property_color_value ?theme
                ~property_prefix:"text-decoration-color" color shade
            in
            (* A project token may be any CSS colour, not necessarily a palette
               colour that can be converted to a compile-time hex fallback, so
               the modifier's alpha rides on an sRGB mix of the value itself. An
               alpha read from a var has no percentage to mix in, leaving the
               colour at full opacity. *)
            let fallback_color =
              if Color.opacity_var_bare_of opacity <> None then color_value
              else Color.opacity_fallback ~percent color shade color_value
            in
            let fallback_decl = text_decoration_color fallback_color in
            let decls, color = Color.bound ?theme color_var color_value in
            let oklab_color = Color.mix_alpha opacity color in
            let webkit_decl = webkit_text_decoration_color oklab_color in
            let oklab_decl = text_decoration_color oklab_color in
            let supports_block =
              Color.color_mix_supports [ webkit_decl; oklab_decl ]
            in
            style ~rules:(Some [ supports_block ]) (decls @ [ fallback_decl ]))

  let decoration_transparent = style [ text_decoration_color (Css.hex "#0000") ]
  let decoration_current = style [ text_decoration_color Current ]

  let decoration_inherit =
    style
      [ webkit_text_decoration_color Inherit; text_decoration_color Inherit ]

  let decoration_current_with_opacity opacity =
    let fallback_decl = text_decoration_color Current in
    let oklab_color = Color.mix_alpha opacity Css.Current in
    let webkit_decl = webkit_text_decoration_color oklab_color in
    let oklab_decl = text_decoration_color oklab_color in
    let supports_block = Color.color_mix_supports [ webkit_decl; oklab_decl ] in
    style ~rules:(Some [ supports_block ]) [ fallback_decl ]

  let decoration_bracket_color_style ~theme c =
    let enhanced = Color.resolve_bracket_css_color c in
    match Color.pre_color_mix_fallback theme enhanced with
    | None -> style ~merge_key:"decoration-" [ text_decoration_color enhanced ]
    | Some fallback ->
        let supports_block =
          Color.color_mix_supports
            [
              webkit_text_decoration_color enhanced;
              text_decoration_color enhanced;
            ]
        in
        style ~merge_key:"decoration-" ~rules:(Some [ supports_block ])
          [ text_decoration_color fallback ]

  (* An opacity modifier applies to the colour the bracket was parsed into. The
     modifier read the bracket text back as a hex and answered black whenever
     that failed, and it wrote the mix in place of the colour for every spelling
     that is not a [#] hex, where Tailwind folds the alpha in. *)
  let decoration_bracket_color_with_opacity ~theme c opacity =
    match Color.bracket_color_opacity ~theme c opacity with
    | Color.Folded value ->
        style ~merge_key:"decoration-" [ text_decoration_color value ]
    | Color.Guarded { fallback; mixed } ->
        let supports_block =
          Color.color_mix_supports
            [ webkit_text_decoration_color mixed; text_decoration_color mixed ]
        in
        style ~merge_key:"decoration-" ~rules:(Some [ supports_block ])
          [ text_decoration_color fallback ]

  let decoration_bracket_var_style v =
    let bare_name = Parse.extract_var_name v in
    let var_color : Css.color = Css.Var (Var.bracket bare_name) in
    style ~merge_key:"decoration-"
      [
        webkit_text_decoration_color var_color; text_decoration_color var_color;
      ]

  let decoration_bracket_var_with_opacity v opacity =
    let bare_name = Parse.extract_var_name v in
    let var_color : Css.color = Css.Var (Var.bracket bare_name) in
    let fallback_webkit = webkit_text_decoration_color var_color in
    let fallback_decl = text_decoration_color var_color in
    let oklab_color = Color.mix_alpha opacity var_color in
    let webkit_decl = webkit_text_decoration_color oklab_color in
    let oklab_decl = text_decoration_color oklab_color in
    let supports_block = Color.color_mix_supports [ webkit_decl; oklab_decl ] in
    style ~merge_key:"decoration-" ~rules:(Some [ supports_block ])
      [ fallback_webkit; fallback_decl ]

  let underline_offset_auto ?theme () =
    match Scheme.theme_value theme "text-underline-offset-auto" with
    | Some _ ->
        let decl, ref_ =
          Var.binding underline_offset_auto_var (Auto : Css.length)
        in
        style [ decl; text_underline_offset (Var ref_) ]
    | None -> style [ text_underline_offset (Auto : Css.length) ]

  let underline_offset_0 = style [ text_underline_offset Zero ]
  let underline_offset_1 = style [ text_underline_offset (Px 1.) ]
  let underline_offset_2 = style [ text_underline_offset (Px 2.) ]
  let underline_offset_4 = style [ text_underline_offset (Px 4.) ]
  let underline_offset_8 = style [ text_underline_offset (Px 8.) ]

  let to_style theme =
    let decoration_color ?shade color = decoration_color ~theme ?shade color in
    let decoration_color_with_opacity color shade opacity =
      decoration_color_with_opacity ~theme color shade opacity
    in
    let underline_offset_auto () = underline_offset_auto ~theme () in
    function
    | Decoration_color (color, None) -> decoration_color color
    | Decoration_color (color, Some shade) -> decoration_color ~shade color
    | Decoration_color_opacity (color, shade, opacity) ->
        decoration_color_with_opacity color shade opacity
    | Decoration_transparent -> decoration_transparent
    | Decoration_current -> decoration_current
    | Decoration_current_opacity opacity ->
        decoration_current_with_opacity opacity
    | Decoration_inherit -> decoration_inherit
    | Decoration_bracket_color (_, c) -> decoration_bracket_color_style ~theme c
    | Decoration_bracket_invalid_color _ -> style []
    | Decoration_bracket_color_opacity (_, c, opacity) ->
        decoration_bracket_color_with_opacity ~theme c opacity
    | Decoration_bracket_var v -> decoration_bracket_var_style v
    | Decoration_bracket_var_opacity (v, opacity) ->
        decoration_bracket_var_with_opacity v opacity
    | Decoration_bracket_color_var v -> decoration_bracket_var_style v
    | Decoration_bracket_color_var_opacity (v, opacity) ->
        decoration_bracket_var_with_opacity v opacity
    | Underline -> underline
    | Overline -> overline
    | Line_through -> line_through
    | No_underline -> no_underline
    | Decoration_solid -> decoration_solid
    | Decoration_double -> decoration_double
    | Decoration_dotted -> decoration_dotted
    | Decoration_dashed -> decoration_dashed
    | Decoration_wavy -> decoration_wavy
    | Decoration_thickness n -> decoration_thickness n
    | Decoration_from_font -> decoration_from_font
    | Decoration_auto -> decoration_auto
    | Decoration_bracket_thickness (_, len) -> decoration_bracket_thickness len
    | Decoration_bracket_pct (_, len) -> decoration_bracket_pct len
    | Decoration_bracket_length_var v -> decoration_bracket_length_var v
    | Decoration_bracket_pct_var v -> decoration_bracket_pct_var v
    | Underline_offset_auto -> underline_offset_auto ()
    | Underline_offset_0 -> underline_offset_0
    | Underline_offset_1 -> underline_offset_1
    | Underline_offset_2 -> underline_offset_2
    | Underline_offset_4 -> underline_offset_4
    | Underline_offset_8 -> underline_offset_8
    | Underline_offset_px px -> style [ text_underline_offset (Px px) ]
    | Underline_offset_arbitrary (_, len) -> style [ text_underline_offset len ]
    | Underline_offset_raw (_, v) ->
        style
          (Option.to_list (Parse.opaque_declaration "text-underline-offset" v))
    | Underline_offset_var v ->
        let bare_name = Parse.extract_var_name v in
        let var_ref : Css.length Css.var = Var.bracket bare_name in
        style [ text_underline_offset (Var var_ref) ]
    | Underline_offset_neg_px px ->
        style
          [
            text_underline_offset
              (Calc (Calc.mul (Calc.length (Px px)) (Calc.float (-1.))));
          ]
    | Underline_offset_neg_arbitrary (_, len) ->
        style
          [
            text_underline_offset
              (Calc (Calc.mul (Calc.length len) (Calc.float (-1.))));
          ]
    | Underline_offset_neg_var v ->
        let bare_name = Parse.extract_var_name v in
        style
          [
            text_underline_offset
              (Calc (Calc.mul (Calc.var bare_name) (Calc.float (-1.))));
          ]

  let examples =
    [
      Decoration_transparent;
      Decoration_solid;
      Decoration_auto;
      Line_through;
      Underline_offset_auto;
    ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v

let decoration_color ?(shade = 500) color =
  Color.check_shade ~utility:"decoration_color" color shade;
  utility (Decoration_color (color, Some shade))

let underline = utility Underline
let overline = utility Overline
let line_through = utility Line_through
let no_underline = utility No_underline
let decoration_solid = utility Decoration_solid
let decoration_double = utility Decoration_double
let decoration_dotted = utility Decoration_dotted
let decoration_dashed = utility Decoration_dashed
let decoration_wavy = utility Decoration_wavy
let decoration_thickness n = utility (Decoration_thickness n)
let decoration_from_font = utility Decoration_from_font
let underline_offset n = utility (Underline_offset_px (float_of_int n))
let underline_offset_auto = utility Underline_offset_auto
let underline_offset_0 = utility Underline_offset_0
let underline_offset_1 = utility Underline_offset_1
let underline_offset_2 = utility Underline_offset_2
let underline_offset_4 = utility Underline_offset_4
let underline_offset_8 = utility Underline_offset_8
