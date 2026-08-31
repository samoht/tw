(** Arbitrary property utilities: [property:value] with optional /opacity. *)

module Css = Cascade.Css

let err_not_utility = Error (`Msg "Not an arbitrary property utility")

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

(* Tailwind's [--alpha(<color>/<percentage>)]: the colour and the alpha it is
   mixed with. The separating slash is the last one, so a colour that carries
   its own (as [oklch(1 0 0 / 50%)] does) still reads. *)
let alpha_fn_parts value =
  let n = String.length value in
  let prefix = "--alpha(" in
  let plen = String.length prefix in
  if n > plen + 1 && String.sub value 0 plen = prefix && value.[n - 1] = ')'
  then
    let inner = String.sub value plen (n - plen - 1) in
    match String.rindex_opt inner '/' with
    | Some i ->
        Some
          ( String.trim (String.sub inner 0 i),
            String.trim (String.sub inner (i + 1) (String.length inner - i - 1))
          )
    | None -> None
  else None

module Handler = struct
  open Style

  type alpha_fn = {
    spelling : string;
        (** the [--alpha(...)] text the class was written with *)
    alpha : Color.opacity_modifier;  (** the alpha that function applies *)
  }
  (** A value written as Tailwind's [--alpha(<color>/<percentage>)]. *)

  type t =
    | Color_opacity of {
        property : string;
        value : string;  (** the colour, with any [--alpha()] wrapper undone *)
        alpha_fn : alpha_fn option;
        opacity : Color.opacity_modifier;
            (** the [/] modifier written after the closing bracket *)
      }
    | Parsed_decl of { property : string; value : string }
  (* Plain [property:value] (no /opacity), parsed by cascade into a typed,
     var-tracking declaration. *)

  type Utility.base += Self of t

  let name = "arbitrary"

  (* Tailwind sorts an arbitrary property by the property it declares, not by
     its name, so [[order:3]] lands with the [order-*] utilities and
     [[mask-type:luminance]] with the masks. Every one of them sorted at the far
     end of the layer instead. A property no family claims has no slot to take
     and keeps that place. *)
  let unclaimed = 37

  let slot t =
    let property, value =
      match t with
      | Color_opacity { property; value; _ } -> (property, value)
      | Parsed_decl { property; value } -> (property, value)
    in
    match
      Css.parse_declaration ~layer:"utilities" property
        (Parse.decode_arbitrary_value value)
    with
    | None -> None
    | Some decl -> Utility.order_of_property (Css.Declaration.property_key decl)

  let priority t =
    match slot t with Some (priority, _) -> priority | None -> unclaimed

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
    | Color.Opacity_percent { value = p; _ } ->
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
        let p = f.value *. 100.0 in
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
    | Color.Opacity_bracket_percent { value = p; _ } ->
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
        let bare = Color.opacity_var_bare var_str in
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
    match Color.Handler.theme_color_of_name bare with
    (* A reference to a palette token renders from the palette, which is what
       puts the token in [@layer theme] and gives the fallback a colour rather
       than the bare reference. *)
    | Some (c, shade) ->
        Color.Handler.colors_with_opacity_style ~theme ~properties:[ emit ] c
          shade opacity
    | None ->
        let var_ref : Css.color Css.var = Var.bracket bare in
        let percent = Color.opacity_to_percent opacity in
        let oklab_decl =
          emit
            (Css.color_mix ~in_space:Oklab (Css.Var var_ref) Css.Transparent
               ~percent1:percent)
        in
        (* The srgb fallback inlines the resolved theme colour when known
           (matching Tailwind), else keeps the raw var. Emitting the referenced
           [--token] into @layer theme needs the registering theme-var mechanism
           (see [Color.color_var]); arbitrary references via [Var.bracket] don't
           trigger it, so theme-var-referencing values still differ in the theme
           layer (the same gap as [backgrounds.ml]'s
           bg-[color:var(--token)]). *)
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
  let to_style theme t =
    match t with
    | Parsed_decl { property; value } -> (
        (* [~layer:"utilities"] only affects a custom property; it keeps the
           declaration in the utilities layer (the build's theme/utilities
           filter drops layerless custom properties). *)
        match
          Css.parse_declaration ~layer:"utilities" property
            (Parse.decode_arbitrary_value value)
        with
        | Some decl -> style [ decl ]
        | None -> style [])
    | Color_opacity { property; value; alpha_fn; opacity } -> (
        match color_emitter property with
        (* of_class only accepts renderable colour declarations; defensive. *)
        | None -> style []
        | Some emit -> (
            (* The [/] modifier applies to the colour the value denotes, which
               is already a mix when the value was written with [--alpha()], so
               the two nest. Mixing with [transparent] only scales the alpha,
               which reads the same in either interpolation space. *)
            let inner, outer =
              match alpha_fn with
              | Some { alpha; _ } -> (alpha, opacity)
              | None -> (opacity, Color.No_opacity)
            in
            let emit c =
              match outer with
              | Color.No_opacity -> emit c
              | o -> emit (Color.mix_alpha ~in_space:Oklab o c)
            in
            (* A var reference renders from the theme when it names a palette
               token, so it is tried before the CSS colour reader (which reads
               [var()] as an opaque colour). *)
            if Parse.is_var value then
              color_var_opacity_style theme emit value inner
            else
              match Css.parse_color (Parse.decode_arbitrary_value value) with
              | Some color -> color_opacity_render theme emit color inner
              | None -> style []))

  let suborder t =
    let offset =
      match t with
      | Parsed_decl { property = "mask-type"; _ } -> 1
      | Color_opacity _ | Parsed_decl _ -> 0
    in
    match slot t with Some (_, sub) -> sub + offset | None -> 0

  let to_class = function
    | Color_opacity { property; value; alpha_fn; opacity } ->
        let written =
          match alpha_fn with Some { spelling; _ } -> spelling | None -> value
        in
        "[" ^ property ^ ":" ^ written ^ "]" ^ Color.opacity_suffix opacity
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
              let raw_value =
                String.sub inner (colon_pos + 1)
                  (String.length inner - colon_pos - 1)
              in
              (* [--alpha(C/P)] is the [/opacity] form spelled as a function, so
                 it resolves to the same fallback and [@supports] pair. *)
              let value, fn_alpha =
                match alpha_fn_parts raw_value with
                | Some (c, p) -> (
                    (* [--alpha()] writes the alpha as a percentage; the [/]
                       modifier writes the bare number. *)
                    let bare =
                      if String.ends_with ~suffix:"%" p then
                        String.sub p 0 (String.length p - 1)
                      else p
                    in
                    match Color.opacity_of_string ~theme bare with
                    | Some alpha -> (c, Some { spelling = raw_value; alpha })
                    | None -> (raw_value, None))
                | None -> (raw_value, None)
              in
              (* What follows the closing bracket is part of the class name, so
                 it has to be a [/opacity] modifier in full: a suffix that does
                 not parse names a class Tailwind does not recognise. *)
              let suffix =
                String.sub class_name (close_pos + 1) (len - close_pos - 1)
              in
              let modifier =
                if suffix = "" then Some Color.No_opacity
                else if suffix.[0] = '/' then
                  Color.opacity_of_string ~theme
                    (String.sub suffix 1 (String.length suffix - 1))
                else None
              in
              match modifier with
              | None -> err_not_utility
              | Some opacity -> (
                  if fn_alpha <> None || opacity <> Color.No_opacity then
                    (* The /opacity form wraps the value in color-mix, so it
                       needs a colour target (known colour property or custom
                       property) and a colour value. Non-colour cases are
                       rejected (Tailwind blindly color-mixes them, which is
                       meaningless). *)
                    let is_colour_value =
                      Parse.is_var value
                      || Css.parse_color (Parse.decode_arbitrary_value value)
                         <> None
                    in
                    if color_emitter property <> None && is_colour_value then
                      Ok
                        (Color_opacity
                           { property; value; alpha_fn = fn_alpha; opacity })
                    else err_not_utility
                  else
                    (* Plain [property:value]: any property whose value cascade
                       can parse becomes a typed declaration. *)
                    match
                      Css.parse_declaration property
                        (Parse.decode_arbitrary_value value)
                    with
                    | Some _ -> Ok (Parsed_decl { property; value })
                    | None -> err_not_utility)))

  let examples = []
end

let () = Utility.register (module Handler)
