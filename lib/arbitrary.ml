(** Arbitrary property utilities: [property:value] with optional /opacity. *)

module Css = Cascade.Css

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
    | Parsed_decl of { property : string; value : string }
  (* Plain [property:value] (no /opacity), parsed by cascade into a typed,
     var-tracking declaration. *)

  type Utility.base += Self of t

  let name = "arbitrary"
  let priority = 36

  (* Render a known colour-property declaration ([color], [background-color],
     ...) with a parsed colour value and an /opacity modifier. *)
  let color_opacity_render theme prop color opacity =
    match opacity with
    | Color.Opacity_named name ->
        let bare = Parse.extract_var_name name in
        let var_name = "opacity-" ^ bare in
        let fallback =
          Color.opacity_fallback_for_theme_value ~theme var_name bare
        in
        (* srgb fallback: resolve the theme value to get the actual
           percentage *)
        let srgb_percent =
          match Scheme.theme_value (Some theme) var_name with
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
          Css.color_mix_var_pct_fallback ~in_space:Oklab ~var_name ~fallback
            color Css.Transparent
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
    | Color.No_opacity -> style [ prop color ]

  (* A var-valued colour with /opacity: oklab color-mix under @supports, with an
     srgb fallback. The fallback resolves the var against the theme when known
     (Tailwind inlines the resolved colour), else keeps the raw var (matching
     Tailwind for non-theme vars). [emit] places the colour on the target
     property ([prop] for a known property, [Css.var] for a custom one). *)
  let color_var_opacity_style theme (emit : Css.color -> Css.declaration) value
      opacity =
    let bare = Parse.extract_var_name value in
    let var_ref : Css.color Css.var = Var.bracket bare in
    let percent = Color.opacity_to_percent opacity in
    let oklab_decl =
      emit
        (Css.color_mix ~in_space:Oklab (Css.Var var_ref) Css.Transparent
           ~percent1:percent)
    in
    (* The srgb fallback inlines the resolved theme colour when known (matching
       Tailwind), else keeps the raw var. Emitting the referenced [--token] into
       @layer theme needs the registering theme-var mechanism (see
       [Color.color_var]); arbitrary references via [Var.bracket] don't trigger
       it, so theme-var-referencing values still differ in the theme layer (the
       same gap as [backgrounds.ml]'s bg-[color:var(--token)]). *)
    let fallback =
      match Scheme.theme_value (Some theme) bare with
      | Some v -> (
          match Css.parse_color (String.trim v) with
          | Some c ->
              emit
                (Css.color_mix ~in_space:Srgb c Css.Transparent
                   ~percent1:percent)
          | None -> emit (Css.Var var_ref : Css.color))
      | None -> emit (Css.Var var_ref : Css.color)
    in
    let supports =
      Css.supports ~condition:Color.color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
    in
    style ~rules:(Some [ supports ]) [ fallback ]

  (* Place a colour on the declaration's target property: a known colour
     property uses its typed constructor; a custom property ([--name]) uses the
     typed [Css.var] form (kept in the utilities layer), never a token
     stream. *)
  let color_emitter property : (Css.color -> Css.declaration) option =
    match color_property_of_name property with
    | Some prop -> Some prop
    | None ->
        if String.length property > 2 && String.sub property 0 2 = "--" then
          let name = String.sub property 2 (String.length property - 2) in
          Some (fun c -> fst (Css.var ~layer:"utilities" name Css.Color c))
        else None

  (* Arbitrary values use [_] for spaces (Tailwind); a literal underscore is
     escaped as [\_]. *)
  let unescape_value value =
    let b = Buffer.create (String.length value) in
    let n = String.length value in
    let i = ref 0 in
    while !i < n do
      if value.[!i] = '\\' && !i + 1 < n && value.[!i + 1] = '_' then (
        Buffer.add_char b '_';
        incr i)
      else if value.[!i] = '_' then Buffer.add_char b ' '
      else Buffer.add_char b value.[!i];
      incr i
    done;
    Buffer.contents b

  let to_style theme t =
    match t with
    | Parsed_decl { property; value } -> (
        (* [~layer:"utilities"] only affects a custom property; it keeps the
           declaration in the utilities layer (the build's theme/utilities
           filter drops layerless custom properties). *)
        match
          Css.parse_declaration ~layer:"utilities" property
            (unescape_value value)
        with
        | Some decl -> style [ decl ]
        | None -> style [])
    | Color_opacity { property; value; opacity } -> (
        match color_emitter property with
        (* of_class only accepts renderable colour declarations; defensive. *)
        | None -> style []
        | Some emit -> (
            match parse_css_color value with
            | Some color -> color_opacity_render theme emit color opacity
            | None ->
                if Parse.is_var value then
                  color_var_opacity_style theme emit value opacity
                else style []))

  let suborder _ = 0

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

  let to_class = function
    | Color_opacity { property; value; opacity } ->
        "[" ^ property ^ ":" ^ value ^ "]" ^ opacity_suffix opacity
    | Parsed_decl { property; value } -> "[" ^ property ^ ":" ^ value ^ "]"

  let of_class theme class_name =
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
          | Some colon_pos -> (
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
                          && Scheme.theme_value (Some theme)
                               ("opacity-" ^ op_part)
                             <> None
                        then Color.Opacity_named op_part
                        else Color.No_opacity
                else Color.No_opacity
              in
              let is_colour_value =
                Parse.is_var value || parse_css_color value <> None
              in
              if opacity <> Color.No_opacity then
                (* The /opacity form wraps the value in color-mix, so it needs a
                   colour target (known colour property or custom property) and
                   a colour value. Non-colour cases are rejected (Tailwind
                   blindly color-mixes them, which is meaningless). *)
                if color_emitter property <> None && is_colour_value then
                  Ok (Color_opacity { property; value; opacity })
                else err_not_utility
              else
                (* Plain [property:value]: any property whose value cascade can
                   parse becomes a typed declaration. *)
                match Css.parse_declaration property (unescape_value value) with
                | Some _ -> Ok (Parsed_decl { property; value })
                | None -> err_not_utility))
end

let () = Utility.register (module Handler)
