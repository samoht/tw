(** Ring utilities: ring width and colour, inset ring, ring offset. *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t =
    | Ring_none
    | Ring_xs
    | Ring_sm
    | Ring_md
    | Ring_lg
    | Ring_xl
    | Ring_width of int
    | Ring_inset
    | Ring_color of Color.color * int
    | Ring_color_opacity of Color.color * int * Color.opacity_modifier
    | Ring_keyword_opacity of Css.color * string * Color.opacity_modifier
    | Ring_offset_width of int
    | Ring_offset_bracket_length of string
    | Ring_offset_color of Color.color * int
    | Ring_offset_color_opacity of Color.color * int * Color.opacity_modifier
    | Ring_offset_keyword_opacity of Css.color * string * Color.opacity_modifier
    | Ring_offset_transparent
    | Ring_offset_current
    | Ring_offset_current_opacity of Color.opacity_modifier
    | Ring_offset_inherit
    | Ring_offset_bracket_color of string * Css.color
    | Ring_offset_bracket_color_opacity of
        string * Css.color * Color.opacity_modifier
    | Ring_offset_bracket_color_var of string
    | Ring_offset_bracket_cvar_opacity of string * Color.opacity_modifier
    | Ring_offset_bracket_var of string
    | Ring_offset_bracket_var_opacity of string * Color.opacity_modifier
    | Inset_ring_color of Color.color * int
    | Inset_ring_color_opacity of Color.color * int * Color.opacity_modifier
    | Inset_ring_keyword_opacity of Css.color * string * Color.opacity_modifier
    | Inset_ring_transparent
    | Inset_ring_current
    | Inset_ring_current_opacity of Color.opacity_modifier
    | Inset_ring_inherit
    | Inset_ring_bracket_color of string * Css.color
    | Inset_ring_bracket_color_opacity of
        string * Css.color * Color.opacity_modifier
    | Inset_ring_bracket_color_var of string
    | Inset_ring_bracket_cvar_opacity of string * Color.opacity_modifier
    | Inset_ring_bracket_var of string
    | Inset_ring_bracket_var_opacity of string * Color.opacity_modifier
    | Inset_ring_default
    | Inset_ring_width of int
    | Inset_ring_bracket_length of string
    | Ring_transparent
    | Ring_current
    | Ring_current_opacity of Color.opacity_modifier
    | Ring_inherit
    | Ring_bracket_color of string * Css.color
    | Ring_bracket_color_opacity of string * Css.color * Color.opacity_modifier
    | Ring_bracket_color_var of string
    | Ring_bracket_color_var_opacity of string * Color.opacity_modifier
    | Ring_bracket_var of string
    | Ring_bracket_var_opacity of string * Color.opacity_modifier
    | Ring_bracket_length of string
    | Ring_raw of string * string * Color.opacity_modifier
    | Ring_offset_raw of string * string * Color.opacity_modifier
    | Inset_ring_raw of string * string * Color.opacity_modifier

  let name = "ring"
  let priority = function Ring_inset -> 40 | _ -> 27

  (* The ring and shadow variables share one [box-shadow] composition, so they
     are declared once, in [Effects]. *)
  let shadow_var = Effects.shadow_var
  let inset_shadow_var = Effects.inset_shadow_var
  let ring_color_var = Effects.ring_color_var
  let ring_shadow_var = Effects.ring_shadow_var
  let inset_ring_color_var = Effects.inset_ring_color_var
  let inset_ring_shadow_var = Effects.inset_ring_shadow_var
  let ring_inset_var = Effects.ring_inset_var
  let ring_offset_width_var = Effects.ring_offset_width_var
  let ring_offset_color_var = Effects.ring_offset_color_var
  let ring_offset_shadow_var = Effects.ring_offset_shadow_var
  let shadow_property_rules = Effects.shadow_property_rules
  let shadow_property_metadata = Effects.shadow_property_metadata

  (* A bracket colour on one of the ring channels: the channel is the property
     the colour lands on. *)
  let bracket_color_var_style ~theme var c =
    Color.bracket_color_style ~theme ~property:(Var.set var) c

  let bracket_color_var_opacity_style ~theme var c opacity =
    Color.bracket_color_opacity_style ~theme ~property:(Var.set var) c opacity

  (* A ring colour channel under a modifier: [fallback] set in the open for a
     browser without [color-mix()], [color] mixed with the modifier's alpha
     behind the guard, with [decls] beside it. *)
  let channel_opacity ?merge_key ?(decls = []) var ~fallback opacity color =
    let mixed = Var.set var (Color.mix_alpha opacity color) in
    let supports_block = Color.color_mix_supports (decls @ [ mixed ]) in
    Style.style ?merge_key ~rules:(Some [ supports_block ])
      [ Var.set var fallback ]

  (* A palette colour on a ring channel, read through the family's own
     [--<prefix>-<colour>] token first. A token bound to a value no hex can
     carry, a [var()], has nothing to fold the alpha into, so the fallback is
     the colour at full opacity, as Tailwind writes it. *)
  let palette_channel_opacity ?theme ~property_prefix var color shade opacity =
    let cvar = Color.property_color_var ?theme ~property_prefix color shade in
    let value =
      Color.property_color_value ?theme ~property_prefix color shade
    in
    let decls, color_ref = Color.bound ?theme cvar value in
    let fallback =
      match Color.hex_alpha_color ?theme color shade opacity with
      | Some hex_alpha -> Css.hex hex_alpha
      | None -> value
    in
    channel_opacity ~decls var ~fallback opacity color_ref

  let bracket_var_channel_opacity ~merge_key var v opacity =
    let var_color : Css.color =
      Css.Var (Var.bracket (Parse.extract_var_name v))
    in
    channel_opacity ~merge_key var ~fallback:var_color opacity var_color

  (* A ring of the given width, a bracket length or the scale's pixels: [var(
     --tw-ring-inset,) 0 0 0 calc(<width> + var(--tw-ring-offset-width))
     var(--tw-ring-color, currentcolor)], with every variable it reads
     declared. *)
  let ring_of_width (width : Css.length) =
    let offset_width_ref = Var.reference ring_offset_width_var in
    let spread : Css.length =
      Calc (Expr (Val width, Add, Var offset_width_ref))
    in
    let color : Css.color =
      Var
        (Var.bracket
           ~fallback:(Fallback (Css.Current : Css.color))
           "tw-ring-color")
    in
    let ring_shadow_value =
      Css.shadow ~inset_var:(Var.name ring_inset_var) ~h_offset:Zero
        ~v_offset:Zero ~blur:Zero ~spread ~color ()
    in
    let d_ring = Var.set ring_shadow_var ring_shadow_value in

    (* Reference shadow variables through @property defaults *)
    let v_inset = Var.reference inset_shadow_var in
    let v_inset_ring = Var.reference inset_ring_shadow_var in
    let v_ring_offset = Var.reference ring_offset_shadow_var in
    let v_ring = Var.reference ring_shadow_var in
    let v_shadow = Var.reference shadow_var in

    let box_shadow_vars : Css.shadow list =
      [
        Css.Var v_inset;
        Css.Var v_inset_ring;
        Css.Var v_ring_offset;
        Css.Var v_ring;
        Css.Var v_shadow;
      ]
    in

    style ~property_rules:shadow_property_rules
      [ d_ring; Css.box_shadows box_shadow_vars ]

  let ring_internal width_px = ring_of_width (Px (float_of_int width_px))

  (* Tailwind v4 ring widths: ring-0=0px, ring-1=1px, ring-2=2px, ring-4=4px,
     ring-8=8px. Bare ring uses --default-ring-width (default 1px). *)
  let ring_none = ring_internal 0
  let ring_xs = ring_internal 1
  let ring_sm = ring_internal 2
  let ring_lg = ring_internal 4
  let ring_xl = ring_internal 8

  (** Bare [ring] — uses scheme's [default_ring_width] (configurable via
      Tailwind's [@theme \{ --default-ring-width \}], default 1px). *)
  let ring_default ?theme () =
    let scheme = Scheme.or_default theme in
    ring_internal scheme.Scheme.default_ring_width

  let inset_ring_internal width_px =
    let spread : Css.length = Px (float_of_int width_px) in
    let color : Css.color =
      Var
        (Var.bracket
           ~fallback:(Fallback (Css.Current : Css.color))
           "tw-inset-ring-color")
    in
    let shadow_value =
      Css.shadow ~inset:true ~h_offset:Zero ~v_offset:Zero ~blur:Zero ~spread
        ~color ()
    in
    let d = Var.set inset_ring_shadow_var shadow_value in
    let v_inset = Var.reference inset_shadow_var in
    let v_inset_ring = Var.reference inset_ring_shadow_var in
    let v_ring_offset = Var.reference ring_offset_shadow_var in
    let v_ring = Var.reference ring_shadow_var in
    let v_shadow = Var.reference shadow_var in
    let box_shadow_vars : Css.shadow list =
      [
        Css.Var v_inset;
        Css.Var v_inset_ring;
        Css.Var v_ring_offset;
        Css.Var v_ring;
        Css.Var v_shadow;
      ]
    in
    style ~property_rules:shadow_property_rules
      [ d; Css.box_shadows box_shadow_vars ]

  let inset_ring_default () = inset_ring_internal 1

  let ring_inset =
    let decl, _var_ref =
      Var.binding ring_inset_var (Css.Variables.custom_value_ident "inset")
    in
    (* Register the ring/shadow @property family, like the other ring utilities;
       Tailwind emits it for ring-inset too. *)
    style ~metadata:shadow_property_metadata
      ~property_rules:shadow_property_rules [ decl ]

  let ring_color ?theme color shade =
    let cvar =
      Color.property_color_var ?theme ~property_prefix:"ring-color" color shade
    in
    let color_value =
      Color.property_color_value ?theme ~property_prefix:"ring-color" color
        shade
    in
    let decls, color = Color.bound ?theme cvar color_value in
    let d = Var.set ring_color_var color in
    style (decls @ [ d ])

  let ring_color_with_opacity ?theme color shade opacity =
    palette_channel_opacity ?theme ~property_prefix:"ring-color" ring_color_var
      color shade opacity

  let ring_offset_width n =
    (* Sets --tw-ring-offset-width and --tw-ring-offset-shadow Format:
       var(--tw-ring-inset,) 0 0 0 var(--tw-ring-offset-width)
       var(--tw-ring-offset-color) *)
    let width_value : Css.length = Px (float_of_int n) in
    let d_width, width_ref = Var.binding ring_offset_width_var width_value in
    let color_ref = Var.reference ring_offset_color_var in
    let shadow_value =
      Css.shadow ~inset:false ~inset_var:(Var.name ring_inset_var)
        ~h_offset:Zero ~v_offset:Zero ~blur:Zero ~spread:(Var width_ref)
        ~color:(Var color_ref) ()
    in
    let d_shadow = Var.set ring_offset_shadow_var shadow_value in
    style [ d_width; d_shadow ]

  let ring_offset_color ?theme color shade =
    (* Sets --tw-ring-offset-color to reference theme color variable. Uses
       property-scoped variable: --ring-offset-color-blue-500 *)
    let color_theme_var =
      Color.property_color_var ?theme ~property_prefix:"ring-offset-color" color
        shade
    in
    let color_value =
      Color.property_color_value ?theme ~property_prefix:"ring-offset-color"
        color shade
    in
    let decls, color = Color.bound ?theme color_theme_var color_value in
    let d = Var.set ring_offset_color_var color in
    style (decls @ [ d ])

  (* One unit table, read once. [parse_bracket_width] is the option form with
     the zero the callers that cannot fail fall back to; the two used to be
     written out separately and agreed only by hand, so adding a unit to one
     alone would have reintroduced a silently-zero width. *)
  let plain_bracket_width value : Css.length option =
    let suffixed suffix mk =
      let n = String.length suffix in
      if String.ends_with ~suffix value && String.length value > n then
        Some (String.sub value 0 (String.length value - n))
        |> Option.map (fun num -> Option.map mk (float_of_string_opt num))
      else None
    in
    match suffixed "px" (fun f -> (Px f : Css.length)) with
    | Some found -> found
    | None -> (
        match suffixed "rem" (fun f -> (Rem f : Css.length)) with
        | Some found -> found
        | None ->
            Option.map
              (fun f -> (Px f : Css.length))
              (float_of_string_opt value))

  (* A [length:] hint says how to read the value written after it; only a var()
     reference there names a custom property. *)
  let parse_bracket_width_opt inner : Css.length option =
    if String.starts_with ~prefix:"length:" inner then
      let v = String.sub inner 7 (String.length inner - 7) in
      if Parse.is_var v then
        Some (Css.Var (Var.bracket (Parse.extract_var_name v)) : Css.length)
      else plain_bracket_width v
    else plain_bracket_width inner

  let parse_bracket_width inner : Css.length =
    match parse_bracket_width_opt inner with Some l -> l | None -> Px 0.

  (* Ring-offset color utilities *)
  let ring_offset_color_with_opacity ?theme color shade opacity =
    palette_channel_opacity ?theme ~property_prefix:"ring-offset-color"
      ring_offset_color_var color shade opacity

  let ring_offset_transparent =
    let d = Var.set ring_offset_color_var Css.Transparent in
    style [ d ]

  let ring_offset_current =
    let d = Var.set ring_offset_color_var Css.Current in
    style [ d ]

  let ring_offset_current_with_opacity opacity =
    channel_opacity ring_offset_color_var ~fallback:Css.Current opacity
      Css.Current

  let ring_offset_inherit =
    let d = Var.set ring_offset_color_var Css.Inherit in
    style [ d ]

  let ring_offset_bracket_length inner =
    let width_value = parse_bracket_width inner in
    let d_width, width_ref = Var.binding ring_offset_width_var width_value in
    let color_ref = Var.reference ring_offset_color_var in
    let shadow_value =
      Css.shadow ~inset:false ~inset_var:(Var.name ring_inset_var)
        ~h_offset:Zero ~v_offset:Zero ~blur:Zero ~spread:(Var width_ref)
        ~color:(Var color_ref) ()
    in
    let d_shadow = Var.set ring_offset_shadow_var shadow_value in
    style [ d_width; d_shadow ]

  let ring_offset_bracket_color ~theme (c : Css.color) =
    bracket_color_var_style ~theme ring_offset_color_var c

  let ring_offset_bracket_color_opacity ~theme (c : Css.color) opacity =
    bracket_color_var_opacity_style ~theme ring_offset_color_var c opacity

  let ring_offset_bracket_color_var v =
    let bare_name = Parse.extract_var_name v in
    let d = Var.set ring_offset_color_var (Css.Var (Var.bracket bare_name)) in
    Style.style ~merge_key:("ring-offset-color:" ^ v) [ d ]

  let ring_offset_bracket_cvar_opacity v opacity =
    bracket_var_channel_opacity ~merge_key:("ring-offset-color:" ^ v)
      ring_offset_color_var v opacity

  let ring_offset_bracket_var v =
    let bare_name = Parse.extract_var_name v in
    let d = Var.set ring_offset_color_var (Css.Var (Var.bracket bare_name)) in
    Style.style ~merge_key:("ring-offset-" ^ v) [ d ]

  let ring_offset_bracket_var_opacity v opacity =
    bracket_var_channel_opacity ~merge_key:("ring-offset-" ^ v)
      ring_offset_color_var v opacity

  (* Inset-ring utilities *)
  let inset_ring_color ?theme color shade =
    let cvar =
      Color.property_color_var ?theme ~property_prefix:"inset-ring-color" color
        shade
    in
    let color_value =
      Color.property_color_value ?theme ~property_prefix:"inset-ring-color"
        color shade
    in
    let decls, color = Color.bound ?theme cvar color_value in
    let d = Var.set inset_ring_color_var color in
    style (decls @ [ d ])

  let inset_ring_color_with_opacity ?theme color shade opacity =
    palette_channel_opacity ?theme ~property_prefix:"inset-ring-color"
      inset_ring_color_var color shade opacity

  let inset_ring_transparent =
    let d = Var.set inset_ring_color_var Css.Transparent in
    style [ d ]

  let inset_ring_current =
    let d = Var.set inset_ring_color_var Css.Current in
    style [ d ]

  let inset_ring_current_with_opacity opacity =
    channel_opacity inset_ring_color_var ~fallback:Css.Current opacity
      Css.Current

  let inset_ring_inherit =
    let d = Var.set inset_ring_color_var Css.Inherit in
    style [ d ]

  let inset_ring_bracket_color ~theme (c : Css.color) =
    bracket_color_var_style ~theme inset_ring_color_var c

  let inset_ring_bracket_color_opacity ~theme (c : Css.color) opacity =
    bracket_color_var_opacity_style ~theme inset_ring_color_var c opacity

  let inset_ring_bracket_color_var v =
    let bare_name = Parse.extract_var_name v in
    let d = Var.set inset_ring_color_var (Css.Var (Var.bracket bare_name)) in
    Style.style ~merge_key:("inset-ring-color:" ^ v) [ d ]

  let inset_ring_bracket_cvar_opacity v opacity =
    bracket_var_channel_opacity ~merge_key:("inset-ring-color:" ^ v)
      inset_ring_color_var v opacity

  let inset_ring_bracket_var v =
    let bare_name = Parse.extract_var_name v in
    let d = Var.set inset_ring_color_var (Css.Var (Var.bracket bare_name)) in
    Style.style ~merge_key:("inset-ring-" ^ v) [ d ]

  let inset_ring_bracket_var_opacity v opacity =
    bracket_var_channel_opacity ~merge_key:("inset-ring-" ^ v)
      inset_ring_color_var v opacity

  (* Ring color utilities *)
  let ring_transparent =
    let d = Var.set ring_color_var Css.Transparent in
    style [ d ]

  let ring_current =
    let d = Var.set ring_color_var Css.Current in
    style [ d ]

  let ring_current_with_opacity opacity =
    channel_opacity ring_color_var ~fallback:Css.Current opacity Css.Current

  let ring_inherit =
    let d = Var.set ring_color_var Css.Inherit in
    style [ d ]

  let ring_keyword_with_opacity var keyword opacity =
    let fallback = Var.set var keyword in
    let mixed = Color.apply_alpha opacity keyword in
    let enhanced = Var.set var mixed in
    let supports_block = Color.color_mix_supports [ enhanced ] in
    style ~rules:(Some [ supports_block ]) [ fallback ]

  let ring_bracket_color ~theme (c : Css.color) =
    bracket_color_var_style ~theme ring_color_var c

  let ring_bracket_color_with_opacity ~theme (c : Css.color) opacity =
    bracket_color_var_opacity_style ~theme ring_color_var c opacity

  let ring_bracket_color_var v =
    let bare_name = Parse.extract_var_name v in
    let d = Var.set ring_color_var (Css.Var (Var.bracket bare_name)) in
    Style.style ~merge_key:("ring-color:" ^ v) [ d ]

  let ring_bracket_cvar_opacity v opacity =
    bracket_var_channel_opacity ~merge_key:("ring-color:" ^ v) ring_color_var v
      opacity

  let ring_bracket_var v =
    let bare_name = Parse.extract_var_name v in
    let d = Var.set ring_color_var (Css.Var (Var.bracket bare_name)) in
    Style.style ~merge_key:("ring-" ^ v) [ d ]

  let ring_bracket_var_with_opacity v opacity =
    bracket_var_channel_opacity ~merge_key:("ring-" ^ v) ring_color_var v
      opacity

  (* A token-stream ring colour, forwarded as written into its channel. Under a
     modifier it takes the mix an [--alpha()] call expands to, and when that mix
     reads a custom property or [currentcolor], the pair Tailwind's polyfill
     writes: what a browser without [color-mix()] reads in the open, the mix
     behind the guard. A ring colour registers nothing: Tailwind writes no
     [@property] for one, whatever the colour's spelling. *)
  let raw_ring_color ~theme var value opacity =
    let set v = Css.custom_property ~layer:"utilities" (Var.css_name var) v in
    let mixed =
      match opacity with
      | Color.No_opacity -> value
      | opacity ->
          Option.value ~default:value
            (Parse.alpha_mix ~alpha:(Color.opacity_percentage opacity) value)
    in
    match Parse.color_mix_fallback ~resolve:(Color.theme_token theme) mixed with
    | None -> style [ set mixed ]
    | Some in_the_open ->
        style
          ~rules:(Some [ Color.color_mix_supports [ set mixed ] ])
          [ set in_the_open ]

  let to_style theme =
    let ring_color_with_opacity color shade opacity =
      ring_color_with_opacity ~theme color shade opacity
    in
    let ring_offset_color_with_opacity color shade opacity =
      ring_offset_color_with_opacity ~theme color shade opacity
    in
    let inset_ring_color_with_opacity color shade opacity =
      inset_ring_color_with_opacity ~theme color shade opacity
    in
    let ring_default () = ring_default ~theme () in
    let ring_color color shade = ring_color ~theme color shade in
    let ring_offset_color color shade = ring_offset_color ~theme color shade in
    let inset_ring_color color shade = inset_ring_color ~theme color shade in
    let ring_bracket_color = ring_bracket_color ~theme in
    let ring_bracket_color_with_opacity =
      ring_bracket_color_with_opacity ~theme
    in
    let ring_offset_bracket_color = ring_offset_bracket_color ~theme in
    let ring_offset_bracket_color_opacity =
      ring_offset_bracket_color_opacity ~theme
    in
    let inset_ring_bracket_color = inset_ring_bracket_color ~theme in
    let inset_ring_bracket_color_opacity =
      inset_ring_bracket_color_opacity ~theme
    in
    fun t ->
      match t with
      | Ring_none -> ring_none
      | Ring_xs -> ring_xs
      | Ring_sm -> ring_sm
      | Ring_md -> ring_default ()
      | Ring_lg -> ring_lg
      | Ring_xl -> ring_xl
      | Ring_width w -> ring_internal w
      | Ring_inset -> ring_inset
      | Ring_color (color, shade) -> ring_color color shade
      | Ring_color_opacity (color, shade, opacity) ->
          ring_color_with_opacity color shade opacity
      | Ring_keyword_opacity (keyword, _, opacity) ->
          ring_keyword_with_opacity ring_color_var keyword opacity
      | Ring_transparent -> ring_transparent
      | Ring_current -> ring_current
      | Ring_current_opacity opacity -> ring_current_with_opacity opacity
      | Ring_inherit -> ring_inherit
      | Ring_bracket_color (_orig, c) -> ring_bracket_color c
      | Ring_bracket_color_opacity (_orig, c, o) ->
          ring_bracket_color_with_opacity c o
      | Ring_bracket_color_var v -> ring_bracket_color_var v
      | Ring_bracket_color_var_opacity (v, o) -> ring_bracket_cvar_opacity v o
      | Ring_bracket_var v -> ring_bracket_var v
      | Ring_bracket_var_opacity (v, o) -> ring_bracket_var_with_opacity v o
      | Ring_raw (_, value, opacity) ->
          raw_ring_color ~theme ring_color_var value opacity
      | Ring_bracket_length inner -> ring_of_width (parse_bracket_width inner)
      | Ring_offset_width n -> ring_offset_width n
      | Ring_offset_bracket_length inner -> ring_offset_bracket_length inner
      | Ring_offset_color (color, shade) -> ring_offset_color color shade
      | Ring_offset_color_opacity (color, shade, opacity) ->
          ring_offset_color_with_opacity color shade opacity
      | Ring_offset_keyword_opacity (keyword, _, opacity) ->
          ring_keyword_with_opacity ring_offset_color_var keyword opacity
      | Ring_offset_transparent -> ring_offset_transparent
      | Ring_offset_current -> ring_offset_current
      | Ring_offset_current_opacity opacity ->
          ring_offset_current_with_opacity opacity
      | Ring_offset_inherit -> ring_offset_inherit
      | Ring_offset_bracket_color (_orig, c) -> ring_offset_bracket_color c
      | Ring_offset_bracket_color_opacity (_orig, c, o) ->
          ring_offset_bracket_color_opacity c o
      | Ring_offset_bracket_color_var v -> ring_offset_bracket_color_var v
      | Ring_offset_bracket_cvar_opacity (v, o) ->
          ring_offset_bracket_cvar_opacity v o
      | Ring_offset_bracket_var v -> ring_offset_bracket_var v
      | Ring_offset_bracket_var_opacity (v, o) ->
          ring_offset_bracket_var_opacity v o
      | Ring_offset_raw (_, value, opacity) ->
          raw_ring_color ~theme ring_offset_color_var value opacity
      | Inset_ring_color (color, shade) -> inset_ring_color color shade
      | Inset_ring_color_opacity (color, shade, opacity) ->
          inset_ring_color_with_opacity color shade opacity
      | Inset_ring_keyword_opacity (keyword, _, opacity) ->
          ring_keyword_with_opacity inset_ring_color_var keyword opacity
      | Inset_ring_transparent -> inset_ring_transparent
      | Inset_ring_current -> inset_ring_current
      | Inset_ring_current_opacity opacity ->
          inset_ring_current_with_opacity opacity
      | Inset_ring_inherit -> inset_ring_inherit
      | Inset_ring_bracket_color (_orig, c) -> inset_ring_bracket_color c
      | Inset_ring_bracket_color_opacity (_orig, c, o) ->
          inset_ring_bracket_color_opacity c o
      | Inset_ring_bracket_color_var v -> inset_ring_bracket_color_var v
      | Inset_ring_bracket_cvar_opacity (v, o) ->
          inset_ring_bracket_cvar_opacity v o
      | Inset_ring_bracket_var v -> inset_ring_bracket_var v
      | Inset_ring_bracket_var_opacity (v, o) ->
          inset_ring_bracket_var_opacity v o
      | Inset_ring_raw (_, value, opacity) ->
          raw_ring_color ~theme inset_ring_color_var value opacity
      | Inset_ring_default -> inset_ring_default ()
      | Inset_ring_width n -> inset_ring_internal n
      | Inset_ring_bracket_length inner ->
          let spread = parse_bracket_width inner in
          let color : Css.color =
            Var
              (Var.bracket
                 ~fallback:(Fallback (Css.Current : Css.color))
                 "tw-inset-ring-color")
          in
          let shadow_value =
            Css.shadow ~inset:true ~h_offset:Zero ~v_offset:Zero ~blur:Zero
              ~spread ~color ()
          in
          let d = Var.set inset_ring_shadow_var shadow_value in
          let v_inset = Var.reference inset_shadow_var in
          let v_inset_ring = Var.reference inset_ring_shadow_var in
          let v_ring_offset = Var.reference ring_offset_shadow_var in
          let v_ring = Var.reference ring_shadow_var in
          let v_shadow = Var.reference shadow_var in
          let box_shadow_vars : Css.shadow list =
            [
              Css.Var v_inset;
              Css.Var v_inset_ring;
              Css.Var v_ring_offset;
              Css.Var v_ring;
              Css.Var v_shadow;
            ]
          in
          style ~property_rules:shadow_property_rules
            [ d; Css.box_shadows box_shadow_vars ]

  let err_not_utility = Error (`Msg "Not a ring utility")

  let parse_ring_bracket ~theme kind v =
    let base_str, opacity = Color.parse_opacity_modifier ~theme v in
    let inner = Parse.bracket_inner base_str in
    match kind with
    | `Ring -> (
        match Color.parse_bracket_hint inner with
        | Some (Color.Typed_var var_part) -> (
            match opacity with
            | Color.No_opacity -> Ok (Ring_bracket_color_var var_part)
            | _ -> Ok (Ring_bracket_color_var_opacity (var_part, opacity)))
        | Some (Color.Bare_var v) -> (
            match opacity with
            | Color.No_opacity -> Ok (Ring_bracket_var v)
            | _ -> Ok (Ring_bracket_var_opacity (v, opacity)))
        | Some (Color.Plain_color c) -> (
            match opacity with
            | Color.No_opacity -> Ok (Ring_bracket_color (inner, c))
            | _ -> Ok (Ring_bracket_color_opacity (inner, c, opacity)))
        | None -> (
            (* A width takes no opacity; Tailwind compiles nothing for
               [ring-[3px]/50]. *)
            match (parse_bracket_width_opt inner, opacity) with
            | Some _, Color.No_opacity -> Ok (Ring_bracket_length inner)
            | Some _, _ -> err_not_utility
            | None, _ -> (
                match Parse.arbitrary_declaration_value inner with
                | Some value -> Ok (Ring_raw (inner, value, opacity))
                | None -> err_not_utility)))
    | `Ring_offset -> (
        match Color.parse_bracket_hint inner with
        | Some (Color.Typed_var var_part) -> (
            match opacity with
            | Color.No_opacity -> Ok (Ring_offset_bracket_color_var var_part)
            | _ -> Ok (Ring_offset_bracket_cvar_opacity (var_part, opacity)))
        | Some (Color.Bare_var v) -> (
            match opacity with
            | Color.No_opacity -> Ok (Ring_offset_bracket_var v)
            | _ -> Ok (Ring_offset_bracket_var_opacity (v, opacity)))
        | Some (Color.Plain_color c) -> (
            match opacity with
            | Color.No_opacity -> Ok (Ring_offset_bracket_color (inner, c))
            | _ -> Ok (Ring_offset_bracket_color_opacity (inner, c, opacity)))
        | None -> (
            match (parse_bracket_width_opt inner, opacity) with
            | Some _, Color.No_opacity -> Ok (Ring_offset_bracket_length inner)
            | Some _, _ -> err_not_utility
            | None, _ -> (
                match Parse.arbitrary_declaration_value inner with
                | Some value -> Ok (Ring_offset_raw (inner, value, opacity))
                | None -> err_not_utility)))
    | `Inset_ring -> (
        match Color.parse_bracket_hint inner with
        | Some (Color.Typed_var var_part) -> (
            match opacity with
            | Color.No_opacity -> Ok (Inset_ring_bracket_color_var var_part)
            | _ -> Ok (Inset_ring_bracket_cvar_opacity (var_part, opacity)))
        | Some (Color.Bare_var v) -> (
            match opacity with
            | Color.No_opacity -> Ok (Inset_ring_bracket_var v)
            | _ -> Ok (Inset_ring_bracket_var_opacity (v, opacity)))
        | Some (Color.Plain_color c) -> (
            match opacity with
            | Color.No_opacity -> Ok (Inset_ring_bracket_color (inner, c))
            | _ -> Ok (Inset_ring_bracket_color_opacity (inner, c, opacity)))
        | None -> (
            match (parse_bracket_width_opt inner, opacity) with
            | Some _, Color.No_opacity -> Ok (Inset_ring_bracket_length inner)
            | Some _, _ -> err_not_utility
            | None, _ -> (
                match Parse.arbitrary_declaration_value inner with
                | Some value -> Ok (Inset_ring_raw (inner, value, opacity))
                | None -> err_not_utility)))

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "ring" ] -> Ok Ring_md
    | [ "ring"; "0" ] -> Ok Ring_none
    | [ "ring"; "1" ] -> Ok Ring_xs
    | [ "ring"; "2" ] -> Ok Ring_sm
    | [ "ring"; "4" ] -> Ok Ring_lg
    | [ "ring"; "8" ] -> Ok Ring_xl
    | [ "ring"; n ]
      when match Parse.decimal_int n with Some w -> w > 0 | None -> false ->
        Ok (Ring_width (int_of_string n))
    | [ "ring"; "inset" ] -> Ok Ring_inset
    | [ "ring"; "transparent" ] -> Ok Ring_transparent
    | [ "ring"; "inherit" ] -> Ok Ring_inherit
    | [ "ring"; value ]
      when let base, opacity = Color.parse_opacity_modifier ~theme value in
           opacity <> Color.No_opacity
           && (base = "transparent" || base = "inherit") ->
        let base, opacity = Color.parse_opacity_modifier ~theme value in
        let keyword =
          if base = "transparent" then Css.Transparent else Css.Inherit
        in
        Ok (Ring_keyword_opacity (keyword, base, opacity))
    | [ "ring"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = Color.parse_opacity_modifier ~theme current_str in
        match opacity with
        | Color.No_opacity when base = "current" -> Ok Ring_current
        | Color.No_opacity -> err_not_utility
        | _ -> Ok (Ring_current_opacity opacity))
    | [ "ring"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value
                (fst (Color.parse_opacity_modifier ~theme v)) ->
        parse_ring_bracket ~theme `Ring v
    (* Shadeless theme colours (ring-black, ring-white, with optional /opacity);
       shaded colours like ring-red need an explicit shade and use the 3-part
       arm below. *)
    | [ "ring"; v ]
      when let base, _ = Color.parse_opacity_modifier ~theme v in
           match Color.shade_of_strings ~theme [ base ] with
           | Ok (c, _) -> Color.is_shadeless c || Color.is_theme_named c
           | Error _ -> false -> (
        let base, opacity = Color.parse_opacity_modifier ~theme v in
        match Color.shade_of_strings ~theme [ base ] with
        | Ok (c, s) -> (
            match opacity with
            | Color.No_opacity -> Ok (Ring_color (c, s))
            | _ -> Ok (Ring_color_opacity (c, s, opacity)))
        | Error _ -> err_not_utility)
    | [ "ring"; "offset"; "transparent" ] -> Ok Ring_offset_transparent
    | [ "ring"; "offset"; "inherit" ] -> Ok Ring_offset_inherit
    | [ "ring"; "offset"; value ]
      when let base, opacity = Color.parse_opacity_modifier ~theme value in
           opacity <> Color.No_opacity
           && (base = "transparent" || base = "inherit") ->
        let base, opacity = Color.parse_opacity_modifier ~theme value in
        let keyword =
          if base = "transparent" then Css.Transparent else Css.Inherit
        in
        Ok (Ring_offset_keyword_opacity (keyword, base, opacity))
    | [ "ring"; "offset"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = Color.parse_opacity_modifier ~theme current_str in
        match opacity with
        | Color.No_opacity when base = "current" -> Ok Ring_offset_current
        | Color.No_opacity -> err_not_utility
        | _ -> Ok (Ring_offset_current_opacity opacity))
    | [ "ring"; "offset"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value
                (fst (Color.parse_opacity_modifier ~theme v)) ->
        parse_ring_bracket ~theme `Ring_offset v
    (* A shadeless theme colour names the offset colour the way it names the
       ring (ring-offset-white, with an optional /opacity); a bare number is the
       width, below. *)
    | [ "ring"; "offset"; v ]
      when let base, _ = Color.parse_opacity_modifier ~theme v in
           match Color.shade_of_strings ~theme [ base ] with
           | Ok (c, _) -> Color.is_shadeless c || Color.is_theme_named c
           | Error _ -> false -> (
        let base, opacity = Color.parse_opacity_modifier ~theme v in
        match Color.shade_of_strings ~theme [ base ] with
        | Ok (c, s) -> (
            match opacity with
            | Color.No_opacity -> Ok (Ring_offset_color (c, s))
            | _ -> Ok (Ring_offset_color_opacity (c, s, opacity)))
        | Error _ -> err_not_utility)
    | [ "ring"; "offset"; n ] -> (
        match Parse.int_any n with
        | Ok width -> Ok (Ring_offset_width width)
        | Error _ -> err_not_utility)
    | [ "ring"; color; shade ] -> (
        match Color.shade_and_opacity_of_strings ~theme [ color; shade ] with
        | Ok (c, s, Color.No_opacity) -> Ok (Ring_color (c, s))
        | Ok (c, s, opacity) -> Ok (Ring_color_opacity (c, s, opacity))
        | Error _ -> err_not_utility)
    | [ "ring"; "offset"; color; shade ] -> (
        match Color.shade_and_opacity_of_strings ~theme [ color; shade ] with
        | Ok (c, s, Color.No_opacity) -> Ok (Ring_offset_color (c, s))
        | Ok (c, s, opacity) -> Ok (Ring_offset_color_opacity (c, s, opacity))
        | Error _ -> err_not_utility)
    | [ "inset"; "ring" ] -> Ok Inset_ring_default
    | [ "inset"; "ring"; "transparent" ] -> Ok Inset_ring_transparent
    | [ "inset"; "ring"; "inherit" ] -> Ok Inset_ring_inherit
    | [ "inset"; "ring"; value ]
      when let base, opacity = Color.parse_opacity_modifier ~theme value in
           opacity <> Color.No_opacity
           && (base = "transparent" || base = "inherit") ->
        let base, opacity = Color.parse_opacity_modifier ~theme value in
        let keyword =
          if base = "transparent" then Css.Transparent else Css.Inherit
        in
        Ok (Inset_ring_keyword_opacity (keyword, base, opacity))
    | [ "inset"; "ring"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = Color.parse_opacity_modifier ~theme current_str in
        match opacity with
        | Color.No_opacity when base = "current" -> Ok Inset_ring_current
        | Color.No_opacity -> err_not_utility
        | _ -> Ok (Inset_ring_current_opacity opacity))
    | [ "inset"; "ring"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value
                (fst (Color.parse_opacity_modifier ~theme v)) ->
        parse_ring_bracket ~theme `Inset_ring v
    (* Shadeless theme colours (inset-ring-black, with optional /opacity). *)
    | [ "inset"; "ring"; v ]
      when let base, _ = Color.parse_opacity_modifier ~theme v in
           match Color.shade_of_strings ~theme [ base ] with
           | Ok (c, _) -> Color.is_shadeless c || Color.is_theme_named c
           | Error _ -> false -> (
        let base, opacity = Color.parse_opacity_modifier ~theme v in
        match Color.shade_of_strings ~theme [ base ] with
        | Ok (c, s) -> (
            match opacity with
            | Color.No_opacity -> Ok (Inset_ring_color (c, s))
            | _ -> Ok (Inset_ring_color_opacity (c, s, opacity)))
        | Error _ -> err_not_utility)
    | [ "inset"; "ring"; n ] -> (
        match Parse.int_any n with
        | Ok width -> Ok (Inset_ring_width width)
        | Error _ -> err_not_utility)
    | [ "inset"; "ring"; color; shade ] -> (
        match Color.shade_and_opacity_of_strings ~theme [ color; shade ] with
        | Ok (c, s, Color.No_opacity) -> Ok (Inset_ring_color (c, s))
        | Ok (c, s, opacity) -> Ok (Inset_ring_color_opacity (c, s, opacity))
        | Error _ -> err_not_utility)
    | _ -> err_not_utility

  let to_class = function
    | Ring_none -> "ring-0"
    | Ring_xs -> "ring-1"
    | Ring_sm -> "ring-2"
    | Ring_md -> "ring"
    | Ring_lg -> "ring-4"
    | Ring_xl -> "ring-8"
    | Ring_width w -> "ring-" ^ string_of_int w
    | Ring_inset -> "ring-inset"
    | Ring_color (color, shade) ->
        if Color.is_shadeless color then "ring-" ^ Color.pp color
        else "ring-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | Ring_color_opacity (color, shade, opacity) ->
        let base =
          if Color.is_shadeless color then "ring-" ^ Color.pp color
          else "ring-" ^ Color.pp color ^ "-" ^ string_of_int shade
        in
        base ^ "/" ^ Color.pp_opacity opacity
    | Ring_keyword_opacity (_, spelling, opacity) ->
        "ring-" ^ spelling ^ "/" ^ Color.pp_opacity opacity
    | Ring_transparent -> "ring-transparent"
    | Ring_current -> "ring-current"
    | Ring_current_opacity o -> "ring-current/" ^ Color.pp_opacity o
    | Ring_inherit -> "ring-inherit"
    | Ring_bracket_color (orig, _c) -> "ring-[" ^ orig ^ "]"
    | Ring_bracket_color_opacity (orig, _c, o) ->
        "ring-[" ^ orig ^ "]/" ^ Color.pp_opacity o
    | Ring_bracket_color_var v -> "ring-[color:" ^ v ^ "]"
    | Ring_bracket_color_var_opacity (v, o) ->
        "ring-[color:" ^ v ^ "]/" ^ Color.pp_opacity o
    | Ring_bracket_var v -> "ring-[" ^ v ^ "]"
    | Ring_bracket_var_opacity (v, o) ->
        "ring-[" ^ v ^ "]/" ^ Color.pp_opacity o
    | Ring_bracket_length l -> "ring-[" ^ l ^ "]"
    | Ring_raw (spelling, _, opacity) ->
        "ring-[" ^ spelling ^ "]" ^ Color.opacity_suffix opacity
    | Ring_offset_width n -> "ring-offset-" ^ string_of_int n
    | Ring_offset_bracket_length l -> "ring-offset-[" ^ l ^ "]"
    | Ring_offset_color (color, shade) ->
        if Color.is_shadeless color then "ring-offset-" ^ Color.pp color
        else "ring-offset-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | Ring_offset_color_opacity (color, shade, opacity) ->
        let base =
          if Color.is_shadeless color then "ring-offset-" ^ Color.pp color
          else "ring-offset-" ^ Color.pp color ^ "-" ^ string_of_int shade
        in
        base ^ "/" ^ Color.pp_opacity opacity
    | Ring_offset_keyword_opacity (_, spelling, opacity) ->
        "ring-offset-" ^ spelling ^ "/" ^ Color.pp_opacity opacity
    | Ring_offset_transparent -> "ring-offset-transparent"
    | Ring_offset_current -> "ring-offset-current"
    | Ring_offset_current_opacity o ->
        "ring-offset-current/" ^ Color.pp_opacity o
    | Ring_offset_inherit -> "ring-offset-inherit"
    | Ring_offset_bracket_color (orig, _c) -> "ring-offset-[" ^ orig ^ "]"
    | Ring_offset_bracket_color_opacity (orig, _c, o) ->
        "ring-offset-[" ^ orig ^ "]/" ^ Color.pp_opacity o
    | Ring_offset_bracket_color_var v -> "ring-offset-[color:" ^ v ^ "]"
    | Ring_offset_bracket_cvar_opacity (v, o) ->
        "ring-offset-[color:" ^ v ^ "]/" ^ Color.pp_opacity o
    | Ring_offset_bracket_var v -> "ring-offset-[" ^ v ^ "]"
    | Ring_offset_bracket_var_opacity (v, o) ->
        "ring-offset-[" ^ v ^ "]/" ^ Color.pp_opacity o
    | Ring_offset_raw (spelling, _, opacity) ->
        "ring-offset-[" ^ spelling ^ "]" ^ Color.opacity_suffix opacity
    | Inset_ring_color (color, shade) ->
        if Color.is_shadeless color then "inset-ring-" ^ Color.pp color
        else "inset-ring-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | Inset_ring_color_opacity (color, shade, opacity) ->
        let base =
          if Color.is_shadeless color then "inset-ring-" ^ Color.pp color
          else "inset-ring-" ^ Color.pp color ^ "-" ^ string_of_int shade
        in
        base ^ "/" ^ Color.pp_opacity opacity
    | Inset_ring_keyword_opacity (_, spelling, opacity) ->
        "inset-ring-" ^ spelling ^ "/" ^ Color.pp_opacity opacity
    | Inset_ring_transparent -> "inset-ring-transparent"
    | Inset_ring_current -> "inset-ring-current"
    | Inset_ring_current_opacity o -> "inset-ring-current/" ^ Color.pp_opacity o
    | Inset_ring_inherit -> "inset-ring-inherit"
    | Inset_ring_bracket_color (orig, _c) -> "inset-ring-[" ^ orig ^ "]"
    | Inset_ring_bracket_color_opacity (orig, _c, o) ->
        "inset-ring-[" ^ orig ^ "]/" ^ Color.pp_opacity o
    | Inset_ring_bracket_color_var v -> "inset-ring-[color:" ^ v ^ "]"
    | Inset_ring_bracket_cvar_opacity (v, o) ->
        "inset-ring-[color:" ^ v ^ "]/" ^ Color.pp_opacity o
    | Inset_ring_bracket_var v -> "inset-ring-[" ^ v ^ "]"
    | Inset_ring_bracket_var_opacity (v, o) ->
        "inset-ring-[" ^ v ^ "]/" ^ Color.pp_opacity o
    | Inset_ring_raw (spelling, _, opacity) ->
        "inset-ring-[" ^ spelling ^ "]" ^ Color.opacity_suffix opacity
    | Inset_ring_default -> "inset-ring"
    | Inset_ring_width n -> "inset-ring-" ^ string_of_int n
    | Inset_ring_bracket_length l -> "inset-ring-[" ^ l ^ "]"

  let suborder = function
    | Ring_md -> 31000
    | Ring_none -> 31001
    | Ring_xs -> 31002
    | Ring_sm -> 31003
    | Ring_width n -> 31001 + n
    | Ring_lg -> 31005
    | Ring_xl -> 31009
    | Ring_bracket_length _ -> 31010
    | Ring_color _ | Ring_color_opacity _ | Ring_keyword_opacity _
    | Ring_transparent | Ring_current | Ring_current_opacity _ | Ring_inherit
    | Ring_bracket_color _ | Ring_bracket_color_opacity _
    | Ring_bracket_color_var _ | Ring_bracket_color_var_opacity _
    | Ring_bracket_var _ | Ring_bracket_var_opacity _ | Ring_raw _ ->
        41000
    | Ring_inset -> 2000
    | Inset_ring_default -> 33000
    | Inset_ring_width n -> 33001 + n
    | Inset_ring_bracket_length _ -> 33100
    | Inset_ring_color _ | Inset_ring_color_opacity _
    | Inset_ring_keyword_opacity _ | Inset_ring_transparent | Inset_ring_current
    | Inset_ring_current_opacity _ | Inset_ring_inherit
    | Inset_ring_bracket_color _ | Inset_ring_bracket_color_opacity _
    | Inset_ring_bracket_color_var _ | Inset_ring_bracket_cvar_opacity _
    | Inset_ring_bracket_var _ | Inset_ring_bracket_var_opacity _
    | Inset_ring_raw _ ->
        43000
    | Ring_offset_width n -> 50000 + n
    | Ring_offset_bracket_length _ -> 50100
    | Ring_offset_color _ | Ring_offset_color_opacity _
    | Ring_offset_keyword_opacity _ | Ring_offset_transparent
    | Ring_offset_current | Ring_offset_current_opacity _ | Ring_offset_inherit
    | Ring_offset_bracket_color _ | Ring_offset_bracket_color_opacity _
    | Ring_offset_bracket_color_var _ | Ring_offset_bracket_cvar_opacity _
    | Ring_offset_bracket_var _ | Ring_offset_bracket_var_opacity _
    | Ring_offset_raw _ ->
        51000

  let examples = [ Ring_none; Inset_ring_default; Ring_offset_width 0 ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let ring_inset = utility Ring_inset
let ring_none = utility Ring_none
let ring_xs = utility Ring_xs
let ring_sm = utility Ring_sm
let ring = utility Ring_md
let ring_md = utility Ring_md
let ring_lg = utility Ring_lg
let ring_xl = utility Ring_xl

let ring_color ?opacity ?(shade = 500) color =
  Color.check_shade ~utility:"ring_color" color shade;
  match opacity with
  | None -> utility (Ring_color (color, shade))
  | Some pct ->
      utility (Ring_color_opacity (color, shade, Color.opacity_of_int pct))

let ring_offset_width n =
  if n < 0 then invalid_arg "ring_offset_width: width must be non-negative"
  else utility (Ring_offset_width n)

let ring_offset_color ?opacity ?(shade = 500) color =
  Color.check_shade ~utility:"ring_offset_color" color shade;
  match opacity with
  | None -> utility (Ring_offset_color (color, shade))
  | Some pct ->
      utility
        (Ring_offset_color_opacity (color, shade, Color.opacity_of_int pct))

let ring_offset_transparent = utility Ring_offset_transparent
let ring_offset_current = utility Ring_offset_current
let ring_offset_inherit = utility Ring_offset_inherit
let inset_ring = utility Inset_ring_default
