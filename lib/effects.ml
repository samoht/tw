(** Box shadow, inset shadow and opacity utilities, and the ring and shadow
    variables their [box-shadow] composition shares with {!Ring}. *)

module Css = Cascade.Css

module Handler = struct
  open Style

  (* Capture the project Pp helpers before [open Css] shadows Pp with Css.Pp. *)
  let pp_float = Pp.float
  let pp_str = Pp.str

  open Css

  type shadow_shape =
    | Two_xs
    | Xs
    | Sm
    | Default
    | Md
    | Lg
    | Xl
    | Two_xl
    | Inner

  type inset_shadow_shape = Ish_2xs | Ish_xs | Ish_sm

  (* One layer of an arbitrary shadow value, read from its bracket. *)
  type arbitrary_shadow = {
    h_offset : Css.length;
    v_offset : Css.length;
    blur : Css.length option;
    spread : Css.length option;
    color : Parse.shadow_colour;
  }

  type t =
    (* Shadows — shape utilities *)
    | Shadow_none
    | Shadow_2xs
    | Shadow_xs
    | Shadow_sm
    | Shadow
    | Shadow_md
    | Shadow_lg
    | Shadow_xl
    | Shadow_2xl
    | Shadow_inner
    | Shadow_arbitrary of string
    | Shadow_raw of string * string * Color.opacity_modifier
    | Shadow_theme of string (* shadow from a --shadow-* token *)
    | Shadow_theme_opacity of string * Color.opacity_modifier
    | Inset_shadow_theme of string (* shadow from an --inset-shadow-* token *)
    | Inset_shadow_theme_opacity of string * Color.opacity_modifier
    | Shadow_arbitrary_opacity of string * Color.opacity_modifier
    | Shadow_shape_opacity of shadow_shape * Color.opacity_modifier
    (* Shadows — color utilities *)
    | Shadow_color of Color.color * int
    | Shadow_color_opacity of Color.color * int * Color.opacity_modifier
    | Shadow_current
    | Shadow_current_opacity of Color.opacity_modifier
    | Shadow_inherit
    | Shadow_transparent
    | Shadow_transparent_opacity of Color.opacity_modifier
    | Shadow_bracket_color of string * Css.color
    | Shadow_bracket_color_opacity of
        string * Css.color * Color.opacity_modifier
    | Shadow_bracket_color_var of string
    | Shadow_bracket_color_var_opacity of string * Color.opacity_modifier
    | Shadow_bracket_shadow of string (* shadow-[shadow:...] type hint *)
    | Shadow_bracket_shadow_opacity of string * Color.opacity_modifier
    | Shadow_bracket_var of string
    | Shadow_bracket_var_opacity of string * Color.opacity_modifier
    (* Inset shadows. The named scale is inset-shadow-{2xs,xs,sm}; bare
       `inset-shadow` is only valid when a threaded @theme defines the
       --inset-shadow token (v4.3.1 has no default for it). *)
    | Inset_shadow_none
    | Inset_shadow_2xs
    | Inset_shadow_xs
    | Inset_shadow_sm
    | Inset_shadow
    | Inset_shadow_arbitrary of string (* For inset-shadow-[12px_12px_#color] *)
    | Inset_shadow_raw of string * string * Color.opacity_modifier
    | Inset_shadow_arbitrary_opacity of string * Color.opacity_modifier
    | Inset_shadow_shape_opacity of inset_shadow_shape * Color.opacity_modifier
    (* Inset shadow color utilities *)
    | Inset_shadow_color of Color.color * int
    | Inset_shadow_color_opacity of Color.color * int * Color.opacity_modifier
    | Inset_shadow_current
    | Inset_shadow_current_opacity of Color.opacity_modifier
    | Inset_shadow_inherit
    | Inset_shadow_transparent
    | Inset_shadow_transparent_opacity of Color.opacity_modifier
    | Inset_shadow_bracket_color of string * Css.color
    | Inset_shadow_bracket_color_opacity of
        string * Css.color * Color.opacity_modifier
    | Inset_shadow_bracket_color_var of string
    | Inset_shadow_bracket_cvar_opacity of string * Color.opacity_modifier
    | Inset_shadow_bracket_shadow of string
    | Inset_shadow_bracket_shadow_opacity of string * Color.opacity_modifier
    | Inset_shadow_bracket_var of string
    | Inset_shadow_bracket_var_opacity of string * Color.opacity_modifier
    (* Opacity *)
    | Opacity of int
    | Opacity_decimal of float (* For values like opacity-2.5 *)
    (* The bracket as written, beside either the number it denotes or, when the
       opacity grammar cannot read it, the author's text: Tailwind hands a
       bracket to the declaration unvalidated. *)
    | Opacity_arbitrary of string * [ `Num of float | `Raw of string ]
    | Opacity_var of string (* opacity-[var(--value)] *)

  let of_shape = function
    | Two_xs -> Shadow_2xs
    | Xs -> Shadow_xs
    | Sm -> Shadow_sm
    | Default -> Shadow
    | Md -> Shadow_md
    | Lg -> Shadow_lg
    | Xl -> Shadow_xl
    | Two_xl -> Shadow_2xl
    | Inner -> Shadow_inner

  let of_inset_shape = function
    | Ish_2xs -> Inset_shadow_2xs
    | Ish_xs -> Inset_shadow_xs
    | Ish_sm -> Inset_shadow_sm

  (* A named [--opacity-*] token on a shadow size, not on a colour: Tailwind
     reads no alpha off it and writes the size as if no modifier were given, so
     the class keeps the modifier in its name and nothing else. *)
  let without_named_size_alpha = function
    | Shadow_shape_opacity (shape, Color.Opacity_named _) -> of_shape shape
    | Shadow_theme_opacity (name, Color.Opacity_named _) -> Shadow_theme name
    | Shadow_arbitrary_opacity (arb, Color.Opacity_named _) ->
        Shadow_arbitrary arb
    | Shadow_bracket_shadow_opacity (s, Color.Opacity_named _) ->
        Shadow_bracket_shadow s
    | Shadow_bracket_var_opacity (v, Color.Opacity_named _) ->
        Shadow_bracket_var v
    | Shadow_raw (inner, value, Color.Opacity_named _) ->
        Shadow_raw (inner, value, Color.No_opacity)
    | Inset_shadow_shape_opacity (shape, Color.Opacity_named _) ->
        of_inset_shape shape
    | Inset_shadow_theme_opacity (name, Color.Opacity_named _) ->
        Inset_shadow_theme name
    | Inset_shadow_arbitrary_opacity (arb, Color.Opacity_named _) ->
        Inset_shadow_arbitrary arb
    | Inset_shadow_bracket_shadow_opacity (s, Color.Opacity_named _) ->
        Inset_shadow_bracket_shadow s
    | Inset_shadow_bracket_var_opacity (v, Color.Opacity_named _) ->
        Inset_shadow_bracket_var v
    | Inset_shadow_raw (inner, value, Color.Opacity_named _) ->
        Inset_shadow_raw (inner, value, Color.No_opacity)
    | t -> t

  let name = "effects"
  let priority _ = 27

  (* Shadow variables with property registration. Order: translate (0-2), scale
     (3-5), border-style (6), gradients (7-15), font-weight (16), shadows
     (17-22), rings (23-30), animation (31-32). *)
  let shadow_var =
    Var.property_default Css.Shadow
      ~initial:
        (shadow ~h_offset:(Px 0.0) ~v_offset:(Px 0.0) ~color:(Css.hex "#0000")
           ())
      ~universal:true ~property_order:7 ~family:`Shadow "tw-shadow"

  let shadow_color_var =
    Var.channel ~needs_property:true ~property_order:8 ~family:`Shadow Css.Color
      "tw-shadow-color"

  let shadow_alpha_var =
    Var.property_default Css.Percentage ~initial:(Pct 100.) ~property_order:9
      ~family:`Shadow "tw-shadow-alpha"

  let inset_shadow_var =
    Var.property_default Css.Shadow
      ~initial:
        (shadow ~h_offset:(Px 0.0) ~v_offset:(Px 0.0) ~color:(Css.hex "#0000")
           ())
      ~universal:true ~property_order:10 ~family:`Inset_shadow "tw-inset-shadow"

  let inset_shadow_color_var =
    Var.channel ~needs_property:true ~property_order:11 ~family:`Inset_shadow
      Css.Color "tw-inset-shadow-color"

  let inset_shadow_alpha_var =
    Var.property_default Css.Percentage ~initial:(Pct 100.) ~property_order:12
      ~family:`Inset_shadow "tw-inset-shadow-alpha"

  (* Ring variables - positions 13-20 *)
  let ring_color_var =
    Var.channel ~needs_property:true ~property_order:13 ~family:`Ring Css.Color
      "tw-ring-color"

  let ring_shadow_var =
    Var.property_default Css.Shadow
      ~initial:
        (shadow ~h_offset:(Px 0.0) ~v_offset:(Px 0.0) ~color:(Css.hex "#0000")
           ())
      ~universal:true ~property_order:14 ~family:`Ring "tw-ring-shadow"

  let inset_ring_color_var =
    Var.channel ~needs_property:true ~property_order:15 ~family:`Inset_ring
      Css.Color "tw-inset-ring-color"

  let inset_ring_shadow_var =
    Var.property_default Css.Shadow
      ~initial:
        (shadow ~h_offset:(Px 0.0) ~v_offset:(Px 0.0) ~color:(Css.hex "#0000")
           ())
      ~universal:true ~property_order:16 ~family:`Inset_ring
      "tw-inset-ring-shadow"

  let ring_inset_var =
    Var.channel ~needs_property:true ~property_order:17 ~family:`Ring Css.Value
      "tw-ring-inset"

  let ring_offset_width_var =
    Var.property_default Css.Length
      ~initial:(Zero : Css.length)
      ~property_order:18 ~family:`Ring "tw-ring-offset-width"

  let ring_offset_color_var =
    Var.property_default Css.Color ~initial:(Css.hex "#fff") ~universal:true
      ~property_order:19 ~family:`Ring "tw-ring-offset-color"

  let ring_offset_shadow_var =
    Var.property_default Css.Shadow
      ~initial:
        (shadow ~h_offset:(Px 0.0) ~v_offset:(Px 0.0) ~color:(Css.hex "#0000")
           ())
      ~universal:true ~property_order:20 ~family:`Ring "tw-ring-offset-shadow"

  (* Note: ring_width_var was removed - Tailwind v4 embeds the width directly in
     the --tw-ring-shadow calc() expression instead of using a separate
     variable *)

  (* ============ Shared shadow helpers ============ *)

  let shadow_property_rules =
    [
      Var.property_rule shadow_var;
      Var.property_rule shadow_color_var;
      Var.property_rule shadow_alpha_var;
      Var.property_rule inset_shadow_var;
      Var.property_rule inset_shadow_color_var;
      Var.property_rule inset_shadow_alpha_var;
      Var.property_rule ring_color_var;
      Var.property_rule ring_shadow_var;
      Var.property_rule inset_ring_color_var;
      Var.property_rule inset_ring_shadow_var;
      Var.property_rule ring_inset_var;
      Var.property_rule ring_offset_width_var;
      Var.property_rule ring_offset_color_var;
      Var.property_rule ring_offset_shadow_var;
    ]
    |> List.filter_map Fun.id |> Css.concat

  let shadow_property_metadata =
    [
      Var.metadata shadow_var;
      Var.metadata shadow_color_var;
      Var.metadata shadow_alpha_var;
      Var.metadata inset_shadow_var;
      Var.metadata inset_shadow_color_var;
      Var.metadata inset_shadow_alpha_var;
      Var.metadata ring_color_var;
      Var.metadata ring_shadow_var;
      Var.metadata inset_ring_color_var;
      Var.metadata inset_ring_shadow_var;
      Var.metadata ring_inset_var;
      Var.metadata ring_offset_width_var;
      Var.metadata ring_offset_color_var;
      Var.metadata ring_offset_shadow_var;
    ]

  let shadow_opacity_decl opacity =
    Var.set shadow_alpha_var (Color.opacity_alpha_value opacity)

  let box_shadow_composition v_shadow =
    let v_inset = Var.reference inset_shadow_var in
    let v_inset_ring = Var.reference inset_ring_shadow_var in
    let v_ring_offset = Var.reference ring_offset_shadow_var in
    let v_ring = Var.reference ring_shadow_var in
    Css.box_shadows
      [
        Css.Var v_inset;
        Css.Var v_inset_ring;
        Css.Var v_ring_offset;
        Css.Var v_ring;
        Css.Var v_shadow;
      ]

  (* Tailwind treats one identifier among two to four shadow lengths as the
     colour token, even when that identifier is not in the CSS named-colour
     table. Keep the authored token order and route that colour through the
     family's channel variable. *)
  let raw_shadow_color ~color_var value opacity =
    let tokens =
      String.split_on_char ' ' value |> List.filter (fun token -> token <> "")
    in
    let lengths, other =
      List.partition
        (fun token ->
          String.equal token "0"
          || Option.is_some (Parse.arbitrary_length token))
        tokens
    in
    match other with
    | [ color ] when List.length lengths >= 2 && List.length lengths <= 4 ->
        let fallback =
          match opacity with
          | Color.No_opacity -> color
          | _ ->
              pp_str
                [
                  "oklab(from ";
                  color;
                  " l a b / ";
                  Css.Pp.to_string ~minify:true
                    (Css.Values.pp_percentage ~always:true)
                    (Color.opacity_percentage opacity);
                  ")";
                ]
        in
        let replacement =
          pp_str [ "var("; Var.css_name color_var; ","; fallback; ")" ]
        in
        String.concat " "
          (List.map
             (fun token ->
               if String.equal token color then replacement else token)
             tokens)
    | _ -> value

  let shadow_raw value opacity =
    let value = raw_shadow_color ~color_var:shadow_color_var value opacity in
    let shadow_decl =
      Css.custom_property ~layer:"utilities" (Var.css_name shadow_var) value
    in
    let declarations =
      match opacity with
      | Color.No_opacity -> [ shadow_decl ]
      | _ -> [ shadow_opacity_decl opacity; shadow_decl ]
    in
    style ~metadata:shadow_property_metadata
      ~property_rules:shadow_property_rules
      (declarations @ [ box_shadow_composition (Var.reference shadow_var) ])

  let shadow_shape_data (shape : shadow_shape) :
      (Css.length * Css.length * Css.length option * Css.length option * string)
      list =
    match shape with
    | Two_xs -> [ (Zero, Px 1., None, None, "#0000000d") ]
    | Inner -> [ (Zero, Px 2., Some (Px 4.), Some Zero, "#0000000d") ]
    | Xs -> [ (Zero, Px 1., Some (Px 2.), Some Zero, "#0000000d") ]
    | Sm ->
        [
          (Zero, Px 1., Some (Px 3.), Some Zero, "#0000001a");
          (Zero, Px 1., Some (Px 2.), Some (Px (-1.)), "#0000001a");
        ]
    | Default ->
        [
          (Zero, Px 1., Some (Px 3.), Some Zero, "#0000001a");
          (Zero, Px 1., Some (Px 2.), Some (Px (-1.)), "#0000001a");
        ]
    | Md ->
        [
          (Zero, Px 4., Some (Px 6.), Some (Px (-1.)), "#0000001a");
          (Zero, Px 2., Some (Px 4.), Some (Px (-2.)), "#0000001a");
        ]
    | Lg ->
        [
          (Zero, Px 10., Some (Px 15.), Some (Px (-3.)), "#0000001a");
          (Zero, Px 4., Some (Px 6.), Some (Px (-4.)), "#0000001a");
        ]
    | Xl ->
        [
          (Zero, Px 20., Some (Px 25.), Some (Px (-5.)), "#0000001a");
          (Zero, Px 8., Some (Px 10.), Some (Px (-6.)), "#0000001a");
        ]
    | Two_xl -> [ (Zero, Px 25., Some (Px 50.), Some (Px (-12.)), "#00000040") ]

  (* Only [shadow-inner] draws inside the box. *)
  let shape_is_inset = function Inner -> true | _ -> false

  let shape_shadow_value shape =
    let data = shadow_shape_data shape in
    let inset = shape_is_inset shape in
    let shadow_list =
      List.map
        (fun (h_offset, v_offset, blur, spread, fallback_hex) ->
          let color_ref =
            Var.reference_with_fallback shadow_color_var (Css.hex fallback_hex)
          in
          Css.shadow ~inset ~h_offset ~v_offset ?blur ?spread
            ~color:(Var color_ref) ())
        data
    in
    match shadow_list with [ s ] -> s | _ -> List shadow_list

  (* Publish the shadow scales through the theme-token registry, the way rule.ml
     publishes the breakpoints, so [theme(static)] emits them. The utilities
     inline the value into [--tw-shadow] rather than referencing a [--shadow-*]
     token, so nothing else would put them in the sheet. *)
  let () =
    let render shadows =
      Css.Pp.to_string ~minify:true
        (Css.Pp.list
           ~sep:(fun ctx () -> Css.Pp.string ctx ", ")
           Css.Properties.pp_shadow)
        shadows
    in
    List.iter
      (fun (name, shape) ->
        let shadows =
          List.map
            (fun (h_offset, v_offset, blur, spread, hex) ->
              Css.shadow ~h_offset ~v_offset ?blur ?spread ~color:(Css.hex hex)
                ())
            (shadow_shape_data shape)
        in
        Scheme.register_default_token name (render shadows))
      [
        ("shadow-2xs", Two_xs);
        ("shadow-xs", Xs);
        ("shadow-sm", Sm);
        ("shadow-md", Md);
        ("shadow-lg", Lg);
        ("shadow-xl", Xl);
        ("shadow-2xl", Two_xl);
      ]

  (* The scale's shadow with its colours as written, for the modifier path,
     which folds the alpha into them as it does for a project token's. *)
  let shape_shadow_literal shape : Css.shadow =
    let inset = shape_is_inset shape in
    let shadow_list =
      List.map
        (fun (h_offset, v_offset, blur, spread, hex) ->
          Css.shadow ~inset ~h_offset ~v_offset ?blur ?spread
            ~color:(Css.hex hex) ())
        (shadow_shape_data shape)
    in
    match shadow_list with [ s ] -> s | _ -> List shadow_list

  let shadow_none =
    let shadow_value =
      Css.shadow ~h_offset:Zero ~v_offset:Zero ~color:(Css.hex "#0000") ()
    in
    let d_shadow, v_shadow = Var.binding shadow_var shadow_value in
    style ~metadata:shadow_property_metadata
      ~property_rules:shadow_property_rules
      [ d_shadow; box_shadow_composition v_shadow ]

  let shadow_shape_style shape =
    let d_shadow, v_shadow =
      Var.binding shadow_var (shape_shadow_value shape)
    in
    style ~metadata:shadow_property_metadata
      ~property_rules:shadow_property_rules
      [ d_shadow; box_shadow_composition v_shadow ]

  let shadow_2xs = shadow_shape_style Two_xs
  let shadow_xs = shadow_shape_style Xs
  let shadow_sm = shadow_shape_style Sm
  let shadow = shadow_shape_style Default
  let shadow_md = shadow_shape_style Md
  let shadow_lg = shadow_shape_style Lg
  let shadow_xl = shadow_shape_style Xl
  let shadow_2xl = shadow_shape_style Two_xl
  let shadow_inner = shadow_shape_style Inner

  (* One layer of a bracket: two lengths, a blur, a spread. *)
  let parse_arbitrary_shadow (s : string) : arbitrary_shadow option =
    match Parse.shadow_layer s with
    | Some ([ h_offset; v_offset ], color) ->
        Some { h_offset; v_offset; blur = None; spread = None; color }
    | Some ([ h_offset; v_offset; blur ], color) ->
        Some { h_offset; v_offset; blur = Some blur; spread = None; color }
    | Some ([ h_offset; v_offset; blur; spread ], color) ->
        Some
          { h_offset; v_offset; blur = Some blur; spread = Some spread; color }
    | _ -> None

  (* A layer that names no colour paints in the current colour, so read a
     missing one as [currentcolor] rather than carrying the absence around. *)
  let body_color (body : Css.shadow_body) : Css.color =
    match body.color with Some c -> c | None -> Css.Current

  (* Rewrite every layer's colour, one shadow at a time. *)
  let map_shadow_color ~f (sh : Css.shadow) : Css.shadow =
    let map_body (body : Css.shadow_body) : Css.shadow_body =
      { body with color = Some (f (body_color body)) }
    in
    match sh with
    | Css.Shadow body -> Css.Shadow (map_body body)
    | Css.Inset (Css.Body body) -> Css.Inset (Css.Body (map_body body))
    | Css.Inset (Css.Toggle t) ->
        Css.Inset (Css.Toggle { t with body = map_body t.body })
    | other -> other

  let map_shadow_colors ~f (sh : Css.shadow) : Css.shadow =
    match sh with
    | List shadows -> List (List.map (map_shadow_color ~f) shadows)
    | _ -> map_shadow_color ~f sh

  let rec shadow_colors (sh : Css.shadow) : Css.color list =
    match sh with
    | Css.Shadow body | Css.Inset (Css.Body body) -> [ body_color body ]
    | Css.Inset (Css.Toggle t) -> [ body_color t.body ]
    | Css.List shadows -> List.concat_map shadow_colors shadows
    | _ -> []

  (** Wrap a shadow's color with [var(--tw-shadow-color, <fallback>)], each
      layer's colour giving the fallback. *)
  let wrap_shadow_colors_by ~color_var ~f (sh : Css.shadow) : Css.shadow =
    map_shadow_colors sh ~f:(fun c ->
        Css.Var (Var.reference_with_fallback color_var (f c)))

  (** Converts CSS color functions to hex for Tailwind parity. *)
  let wrap_shadow_colors ~color_var (sh : Css.shadow) : Css.shadow =
    wrap_shadow_colors_by ~color_var sh ~f:(fun c ->
        match Color.css_color_to_hex c with Some h -> h | None -> c)

  (* The shadow with its layers' colours replaced in order, one per layer that
     has one, which is the order [shadow_colors] reads them in. *)
  let with_shadow_colours (sh : Css.shadow) (colours : Css.color list) =
    let rec go (sh : Css.shadow) colours : Css.shadow * Css.color list =
      match sh with
      | Css.List layers ->
          let layers, rest =
            List.fold_left
              (fun (acc, colours) layer ->
                let layer, colours = go layer colours in
                (layer :: acc, colours))
              ([], colours) layers
          in
          ((Css.List (List.rev layers) : Css.shadow), rest)
      | Css.Shadow _ | Css.Inset _ -> (
          match colours with
          | c :: rest -> (map_shadow_color ~f:(fun _ -> c) sh, rest)
          | [] -> (sh, []))
      | other -> (other, colours)
    in
    fst (go sh colours)

  (* An arbitrary shadow the value reader produced, with the modifier's alpha
     folded into every layer. Both families write the same shape: the alpha and
     the unguarded value in a rule of their own, the guarded value in an
     [@supports] behind it, and the composition in the base rule. *)
  let arbitrary_shadow_alpha_style ~shadow_var ~color_var ~alpha_decl
      ~composition (sh : Css.shadow) opacity =
    let rebuild colours =
      let colours =
        List.map
          (fun c : Css.color ->
            Css.Var (Var.reference_with_fallback color_var c))
          colours
      in
      Var.set shadow_var (with_shadow_colours sh colours)
    in
    let d_shadow, supports =
      Color.shadow_alpha_decls opacity ~colours:(shadow_colors sh) ~rebuild
    in
    let v_shadow = Var.reference shadow_var in
    match supports with
    | None ->
        style ~metadata:shadow_property_metadata
          ~property_rules:shadow_property_rules
          [ alpha_decl; d_shadow; composition v_shadow ]
    | Some supports_block ->
        let alpha_shadow_rule =
          Css.rule ~selector:(Css.Selector.class_ "_") [ alpha_decl; d_shadow ]
        in
        style
          ~rules:(Some [ alpha_shadow_rule; supports_block ])
          ~metadata:shadow_property_metadata
          ~property_rules:shadow_property_rules
          [ composition v_shadow ]

  let arbitrary_shadow_layer ?inset { h_offset; v_offset; blur; spread; color }
      =
    Css.shadow ?inset ~h_offset ~v_offset ?blur ?spread
      ~color:(Color.shadow_token_colour color)
      ()

  let shadow_shape_opacity_style shape opacity =
    arbitrary_shadow_alpha_style ~shadow_var ~color_var:shadow_color_var
      ~alpha_decl:(shadow_opacity_decl opacity)
      ~composition:box_shadow_composition
      (shape_shadow_literal shape)
      opacity

  (* A shadow bracket is a shadow list or a var(); anything else - the docs'
     [<value>] placeholder included - is not one. *)
  let is_shadow_bracket inner =
    Parse.is_var inner
    || parse_arbitrary_shadow inner <> None
    || Parse.shadow (Parse.decode_arbitrary_value inner) <> None

  (* A bracket shadow holding a [color-mix()] Tailwind polyfills: the shadow
     every browser reads and the shadow as written, or nothing when no layer's
     colour reads a custom property or [currentcolor] through a mix. *)
  let polyfilled_shadow ~theme normalized : (Css.shadow * Css.shadow) option =
    match
      Parse.color_mix_fallback ~resolve:(Color.theme_token theme) normalized
    with
    | None -> None
    | Some in_the_open -> (
        match (Parse.shadow in_the_open, Parse.shadow normalized) with
        | Some in_the_open, Some as_written -> Some (in_the_open, as_written)
        | _ -> None)

  (* The pair on a family's channel: what a browser without [color-mix()] reads
     in a rule of its own, the shadow as written behind the guard, and the
     composition in the base rule, each layer's colour behind the family's
     colour channel. *)
  let shadow_polyfill_style ~shadow_var ~color_var ~composition
      (in_the_open, as_written) =
    let set sh = Var.set shadow_var (wrap_shadow_colors ~color_var sh) in
    let self decls = Css.rule ~selector:(Css.Selector.class_ "_") decls in
    style ~metadata:shadow_property_metadata
      ~property_rules:shadow_property_rules
      ~rules:
        (Some
           [
             self [ set in_the_open ];
             Color.color_mix_supports [ set as_written ];
           ])
      [ composition (Var.reference shadow_var) ]

  let shadow_arbitrary ~theme (arb : string) =
    let normalized = Parse.decode_arbitrary_value arb in
    (* The reading below takes one shadow, so anything with a comma - a layer
       list, or a colour function carrying one - goes to the value parser
       instead. *)
    match
      if String.contains arb ',' then Option.None
      else parse_arbitrary_shadow arb
    with
    | Some { h_offset; v_offset; blur; spread; color = Parse.Var_token v } ->
        (* A trailing var() is the colour, not the blur (Tailwind). *)
        let color_ref =
          Var.reference_with_fallback shadow_color_var
            (Color.bracket_var_color v)
        in
        let shadow_value =
          Css.shadow ~h_offset ~v_offset ?blur ?spread ~color:(Var color_ref) ()
        in
        let d_shadow, v_shadow = Var.binding shadow_var shadow_value in
        style ~metadata:shadow_property_metadata
          ~property_rules:shadow_property_rules
          [ d_shadow; box_shadow_composition v_shadow ]
    | _ -> (
        match polyfilled_shadow ~theme normalized with
        | Some pair ->
            shadow_polyfill_style ~shadow_var ~color_var:shadow_color_var
              ~composition:box_shadow_composition pair
        | None -> (
            match Parse.shadow normalized with
            | Some shadow ->
                let shadow_value =
                  wrap_shadow_colors ~color_var:shadow_color_var shadow
                in
                let d_shadow, v_shadow = Var.binding shadow_var shadow_value in
                style ~metadata:shadow_property_metadata
                  ~property_rules:shadow_property_rules
                  [ d_shadow; box_shadow_composition v_shadow ]
            | None -> shadow_none))

  let is_theme_shadow theme n =
    Scheme.theme_value (Some theme) ("shadow-" ^ n) <> None

  let is_theme_inset_shadow theme n =
    Scheme.theme_value (Some theme) ("inset-shadow-" ^ n) <> None

  (* A shadow the project named in its [@theme]. Tailwind inlines the token's
     value rather than referencing it, routing the colour through the family's
     shadow-colour channel exactly as an arbitrary shadow does. *)
  let shadow_theme theme name =
    match Scheme.theme_value (Some theme) ("shadow-" ^ name) with
    | None -> style []
    | Some raw -> (
        match Parse.shadow raw with
        | None -> style []
        | Some shadow ->
            let shadow_value =
              wrap_shadow_colors ~color_var:shadow_color_var shadow
            in
            let d_shadow, v_shadow = Var.binding shadow_var shadow_value in
            style ~metadata:shadow_property_metadata
              ~property_rules:shadow_property_rules
              [ d_shadow; box_shadow_composition v_shadow ])

  let shadow_arbitrary_opacity (arb : string) opacity =
    match parse_arbitrary_shadow arb with
    | Some layer ->
        arbitrary_shadow_alpha_style ~shadow_var ~color_var:shadow_color_var
          ~alpha_decl:(shadow_opacity_decl opacity)
          ~composition:box_shadow_composition
          (arbitrary_shadow_layer layer)
          opacity
    | None -> (
        (* The bracket reaches here because [is_shadow_bracket] read it as a
           shadow, and the reader that said so is [Parse.shadow], which reads a
           colour keyword, a layer list and the [inset] keyword the reading
           above does not. Falling out to [shadow-none] lost the lengths and the
           modifier's own declaration along with the colour. *)
        match Parse.shadow (Parse.decode_arbitrary_value arb) with
        | Some shadow ->
            arbitrary_shadow_alpha_style ~shadow_var ~color_var:shadow_color_var
              ~alpha_decl:(shadow_opacity_decl opacity)
              ~composition:box_shadow_composition shadow opacity
        | None -> shadow_none)

  (* ============ Shadow color utilities ============ *)

  let shadow_channel : Color.channel =
    {
      color = shadow_color_var;
      alpha = shadow_alpha_var;
      property_prefix = "box-shadow-color";
      metadata = shadow_property_metadata;
      property_rules = shadow_property_rules;
    }

  (* A shadow read whole from a custom property. Under a modifier Tailwind sets
     the alpha channel and leaves the value as it is: there is no colour in it
     to fold the alpha into. *)
  let shadow_raw_var ?(opacity = Color.No_opacity) v =
    let var_name = Parse.extract_var_name v in
    let shadow_value : Css.shadow = Css.Var (Var.bracket var_name) in
    let d_shadow, v_shadow = Var.binding shadow_var shadow_value in
    let alpha =
      match opacity with
      | Color.No_opacity -> []
      | opacity -> [ shadow_opacity_decl opacity ]
    in
    style ~metadata:shadow_property_metadata
      ~property_rules:shadow_property_rules
      (alpha @ [ d_shadow; box_shadow_composition v_shadow ])

  (* Inset shadow utilities - sets --tw-inset-shadow and composites
     box-shadow *)

  (* Compose a built --tw-inset-shadow value into the full box-shadow stack.
     Shared by the named scale, the bare themed utility and
     inset-shadow-none. *)
  let inset_shadow_compose inset_value =
    let d_inset_shadow, v_inset_shadow =
      Var.binding inset_shadow_var inset_value
    in
    let v_inset_ring = Var.reference inset_ring_shadow_var in
    let v_ring_offset = Var.reference ring_offset_shadow_var in
    let v_ring = Var.reference ring_shadow_var in
    let v_shadow = Var.reference shadow_var in
    let box_shadow_vars : Css.shadow list =
      [
        Css.Var v_inset_shadow;
        Css.Var v_inset_ring;
        Css.Var v_ring_offset;
        Css.Var v_ring;
        Css.Var v_shadow;
      ]
    in
    style ~metadata:shadow_property_metadata
      ~property_rules:shadow_property_rules
      [ d_inset_shadow; Css.box_shadows box_shadow_vars ]

  let inset_shadow_theme theme name =
    match Scheme.theme_value (Some theme) ("inset-shadow-" ^ name) with
    | None -> style []
    | Some raw -> (
        match Parse.shadow raw with
        | None -> style []
        | Some shadow ->
            inset_shadow_compose
              (wrap_shadow_colors ~color_var:inset_shadow_color_var shadow))

  (* A project shadow under a modifier takes the alpha the way an arbitrary one
     does: Tailwind reads the token's value at build time and folds the alpha
     into every layer's colour. *)
  let shadow_theme_opacity theme name opacity =
    match
      Option.bind
        (Scheme.theme_value (Some theme) ("shadow-" ^ name))
        Parse.shadow
    with
    | None -> style []
    | Some shadow ->
        arbitrary_shadow_alpha_style ~shadow_var ~color_var:shadow_color_var
          ~alpha_decl:(shadow_opacity_decl opacity)
          ~composition:box_shadow_composition shadow opacity

  (* v4.3.1 default inset-shadow scale (verified against the bare CLI). The
     named utilities are inset-shadow-{2xs,xs,sm}; each is a single inset
     shadow. The alpha is .05 (#0000000d). 2xs has no blur. *)
  let inset_shadow_shape_data (shape : inset_shadow_shape) : Css.shadow_body =
    let body v_offset blur : Css.shadow_body =
      {
        h_offset = Zero;
        v_offset;
        blur;
        spread = None;
        color = Some (Css.hex "#0000000d");
      }
    in
    match shape with
    | Ish_2xs -> body (Px 1.) None
    | Ish_xs -> body (Px 1.) (Some (Px 1.))
    | Ish_sm -> body (Px 2.) (Some (Px 4.))

  (* An inset shadow built from a body, with [color] standing in for the one the
     body carries: the scale publishes the authored colour, a utility swaps in
     the [--tw-inset-shadow-color] reference that falls back to it. *)
  let inset_shadow_of_body (body : Css.shadow_body) ~color =
    Css.shadow ~inset:true ~h_offset:body.h_offset ~v_offset:body.v_offset
      ?blur:body.blur ?spread:body.spread ~color ()

  let inset_shadow_body_color (body : Css.shadow_body) =
    Option.value body.color ~default:(Css.hex "#0000000d")

  (* The inset-shadow scale, published the same way. *)
  let () =
    List.iter
      (fun (name, shape) ->
        let body = inset_shadow_shape_data shape in
        let value =
          inset_shadow_of_body body ~color:(inset_shadow_body_color body)
        in
        Scheme.register_default_token name
          (Css.Pp.to_string ~minify:true Css.Properties.pp_shadow value))
      [
        ("inset-shadow-2xs", Ish_2xs);
        ("inset-shadow-xs", Ish_xs);
        ("inset-shadow-sm", Ish_sm);
      ]

  (* Theme token name for a shape's scale value (matches the @theme keys, e.g.
     --inset-shadow-sm). *)
  let inset_shadow_shape_token = function
    | Ish_2xs -> "inset-shadow-2xs"
    | Ish_xs -> "inset-shadow-xs"
    | Ish_sm -> "inset-shadow-sm"

  (* Read a theme override for an inset-shadow token (e.g. "inset 0 2px 4px
     rgb(0 0 0 / 0.05)") as the CSS shadow value it is. The grammar settles what
     the token may hold, so a length in any unit, a spread, and the colour in
     the spelling the project wrote all survive; tw re-emits with [inset] set,
     so an override that omits the keyword is read the same way. *)
  let parse_inset_shadow_override (s : string) : Css.shadow_body option =
    match Parse.shadow s with
    | Some (Inset (Body body)) | Some (Shadow body) -> Some body
    | Some _ | None -> None

  (* The shadow body for a named shape: a threaded @theme override if present,
     else the v4.3.1 default scale. *)
  let inset_shadow_data_for ?theme shape =
    match Scheme.theme_value theme (inset_shadow_shape_token shape) with
    | Some override -> (
        match parse_inset_shadow_override override with
        | Some data -> data
        | None -> inset_shadow_shape_data shape)
    | None -> inset_shadow_shape_data shape

  let inset_shadow_shape_style ?theme shape =
    let body = inset_shadow_data_for ?theme shape in
    let color_ref =
      Var.reference_with_fallback inset_shadow_color_var
        (inset_shadow_body_color body)
    in
    inset_shadow_compose (inset_shadow_of_body body ~color:(Var color_ref))

  let inset_shadow_none =
    inset_shadow_compose
      (Css.shadow ~inset:true ~h_offset:Zero ~v_offset:Zero
         ~color:(Css.hex "#0000") ())

  (* Bare `inset-shadow` reads the threaded --inset-shadow token (it has no
     v4.3.1 default; of_class only reaches here when the token is defined). *)
  let inset_shadow_themed ?theme () =
    let body =
      match Scheme.theme_value theme "inset-shadow" with
      | Some override -> (
          match parse_inset_shadow_override override with
          | Some body -> body
          | None -> inset_shadow_shape_data Ish_sm)
      | None -> inset_shadow_shape_data Ish_sm
    in
    let color_ref =
      Var.reference_with_fallback inset_shadow_color_var
        (inset_shadow_body_color body)
    in
    inset_shadow_compose (inset_shadow_of_body body ~color:(Var color_ref))

  (* ============ Inset shadow helpers ============ *)

  let inset_shadow_opacity_decl opacity =
    Var.set inset_shadow_alpha_var (Color.opacity_alpha_value opacity)

  let inset_box_shadow_composition v_inset_shadow =
    let v_inset_ring = Var.reference inset_ring_shadow_var in
    let v_ring_offset = Var.reference ring_offset_shadow_var in
    let v_ring = Var.reference ring_shadow_var in
    let v_shadow = Var.reference shadow_var in
    Css.box_shadows
      [
        Css.Var v_inset_shadow;
        Css.Var v_inset_ring;
        Css.Var v_ring_offset;
        Css.Var v_ring;
        Css.Var v_shadow;
      ]

  let inset_shadow_raw value opacity =
    let value =
      raw_shadow_color ~color_var:inset_shadow_color_var value opacity
    in
    let shadow_decl =
      Css.custom_property ~layer:"utilities"
        (Var.css_name inset_shadow_var)
        ("inset " ^ value)
    in
    let declarations =
      match opacity with
      | Color.No_opacity -> [ shadow_decl ]
      | _ -> [ inset_shadow_opacity_decl opacity; shadow_decl ]
    in
    style ~metadata:shadow_property_metadata
      ~property_rules:shadow_property_rules
      (declarations
      @ [ inset_box_shadow_composition (Var.reference inset_shadow_var) ])

  (* Parse multi-value shadow (comma-separated) *)
  let parse_multi_shadow (s : string) =
    let parts = Parse.split_top_level ',' s in
    let parsed = List.filter_map parse_arbitrary_shadow parts in
    if List.compare_lengths parsed parts = 0 then Some parsed else None

  (* [Parse.shadow] reads a non-inset shadow, which is the spelling an
     [inset-shadow-*] bracket carries; the utility supplies the [inset] keyword
     itself, once per layer, the way Tailwind does. *)
  let rec force_inset (sh : Css.shadow) : Css.shadow =
    match sh with
    | Css.Shadow body -> Css.Inset (Css.Body body)
    | Css.List shadows -> Css.List (List.map force_inset shadows)
    | other -> other

  let inset_shadow_arbitrary_layers (arb : string) =
    match parse_multi_shadow arb with
    | Some parsed_parts ->
        let shadow_values =
          List.map
            (fun { h_offset; v_offset; blur; spread; color } ->
              let color_ref =
                Var.reference_with_fallback inset_shadow_color_var
                  (Color.shadow_token_colour color)
              in
              Css.shadow ~inset:true ~h_offset ~v_offset ?blur ?spread
                ~color:(Var color_ref) ())
            parsed_parts
        in
        let inset_value =
          match shadow_values with [ s ] -> s | _ -> List shadow_values
        in
        let d_inset_shadow, v_inset_shadow =
          Var.binding inset_shadow_var inset_value
        in
        style ~metadata:shadow_property_metadata
          ~property_rules:shadow_property_rules
          [ d_inset_shadow; inset_box_shadow_composition v_inset_shadow ]
    | None -> (
        (* The bracket reaches here because [is_shadow_bracket] read it as a
           shadow, and the reader that said so is [Parse.shadow], which knows
           every colour spelling rather than the hex, [var()] and colour
           function the reading above covers. Reading it with the same reader
           that accepted it keeps the shadow the class wrote; falling out to
           [inset-shadow-none] lost its lengths and spread along with the
           colour. *)
        match Parse.shadow (Parse.decode_arbitrary_value arb) with
        | Some shadow ->
            inset_shadow_compose
              (wrap_shadow_colors ~color_var:inset_shadow_color_var
                 (force_inset shadow))
        | None -> inset_shadow_none)

  (* A layer whose colour needs Tailwind's polyfill takes the pair; the rest
     read as layers, each with [inset] set. *)
  let inset_shadow_arbitrary ~theme (arb : string) =
    match polyfilled_shadow ~theme (Parse.decode_arbitrary_value arb) with
    | Some (in_the_open, as_written) ->
        shadow_polyfill_style ~shadow_var:inset_shadow_var
          ~color_var:inset_shadow_color_var
          ~composition:inset_box_shadow_composition
          (force_inset in_the_open, force_inset as_written)
    | None -> inset_shadow_arbitrary_layers arb

  let inset_shadow_shape_opacity_style ?theme shape opacity =
    let body = inset_shadow_data_for ?theme shape in
    arbitrary_shadow_alpha_style ~shadow_var:inset_shadow_var
      ~color_var:inset_shadow_color_var
      ~alpha_decl:(inset_shadow_opacity_decl opacity)
      ~composition:inset_box_shadow_composition
      (inset_shadow_of_body body ~color:(inset_shadow_body_color body))
      opacity

  let inset_shadow_theme_opacity theme name opacity =
    match
      Option.bind
        (Scheme.theme_value (Some theme) ("inset-shadow-" ^ name))
        Parse.shadow
    with
    | None -> style []
    | Some shadow ->
        arbitrary_shadow_alpha_style ~shadow_var:inset_shadow_var
          ~color_var:inset_shadow_color_var
          ~alpha_decl:(inset_shadow_opacity_decl opacity)
          ~composition:inset_box_shadow_composition shadow opacity

  let inset_shadow_arbitrary_opacity (arb : string) opacity =
    match parse_multi_shadow arb with
    | Some layers ->
        let shadow : Css.shadow =
          match List.map (arbitrary_shadow_layer ~inset:true) layers with
          | [ single ] -> single
          | many -> Css.List many
        in
        arbitrary_shadow_alpha_style ~shadow_var:inset_shadow_var
          ~color_var:inset_shadow_color_var
          ~alpha_decl:(inset_shadow_opacity_decl opacity)
          ~composition:inset_box_shadow_composition shadow opacity
    | None -> (
        (* Read the bracket with the reader that accepted it, as
           [inset_shadow_arbitrary] does without a modifier. *)
        match Parse.shadow (Parse.decode_arbitrary_value arb) with
        | Some shadow ->
            arbitrary_shadow_alpha_style ~shadow_var:inset_shadow_var
              ~color_var:inset_shadow_color_var
              ~alpha_decl:(inset_shadow_opacity_decl opacity)
              ~composition:inset_box_shadow_composition (force_inset shadow)
              opacity
        | None -> inset_shadow_none)

  (* ============ Inset shadow color utilities ============ *)

  let inset_shadow_channel : Color.channel =
    {
      color = inset_shadow_color_var;
      alpha = inset_shadow_alpha_var;
      property_prefix = "inset-box-shadow-color";
      metadata = shadow_property_metadata;
      property_rules = shadow_property_rules;
    }

  let inset_shadow_raw_var ?(opacity = Color.No_opacity) v =
    let var_name = Parse.extract_var_name v in
    (* Tailwind prepends [inset] so the composed shadow is actually inset. *)
    let shadow_value : Css.shadow =
      Css.Inset (Css.Var (Var.bracket var_name))
    in
    let decl, v_inset_shadow = Var.binding inset_shadow_var shadow_value in
    let alpha =
      match opacity with
      | Color.No_opacity -> []
      | opacity -> [ inset_shadow_opacity_decl opacity ]
    in
    style ~metadata:shadow_property_metadata
      ~property_rules:shadow_property_rules
      (alpha @ [ decl; inset_box_shadow_composition v_inset_shadow ])

  let opacity n =
    let value = float_of_int n /. 100.0 in
    style [ opacity (Opacity_number value) ]

  let to_style theme t =
    match without_named_size_alpha t with
    | Shadow_none -> shadow_none
    | Shadow_2xs -> shadow_2xs
    | Shadow_xs -> shadow_xs
    | Shadow_sm -> shadow_sm
    | Shadow -> shadow
    | Shadow_md -> shadow_md
    | Shadow_lg -> shadow_lg
    | Shadow_xl -> shadow_xl
    | Shadow_2xl -> shadow_2xl
    | Shadow_inner -> shadow_inner
    | Shadow_arbitrary arb -> shadow_arbitrary ~theme arb
    | Shadow_raw (_, value, opacity) -> shadow_raw value opacity
    | Shadow_theme name -> shadow_theme theme name
    | Shadow_theme_opacity (name, op) -> shadow_theme_opacity theme name op
    | Inset_shadow_theme name -> inset_shadow_theme theme name
    | Inset_shadow_theme_opacity (name, op) ->
        inset_shadow_theme_opacity theme name op
    | Shadow_arbitrary_opacity (arb, op) -> shadow_arbitrary_opacity arb op
    | Shadow_shape_opacity (shape, op) -> shadow_shape_opacity_style shape op
    | Shadow_color (c, s) -> Color.channel_color ~theme shadow_channel c s
    | Shadow_color_opacity (c, s, op) ->
        Color.channel_color_opacity ~theme shadow_channel c s op
    | Shadow_current -> Color.channel_current shadow_channel
    | Shadow_current_opacity op ->
        Color.channel_current_opacity shadow_channel op
    | Shadow_inherit -> Color.channel_inherit shadow_channel
    | Shadow_transparent -> Color.channel_transparent shadow_channel
    | Shadow_transparent_opacity op ->
        Color.channel_transparent_opacity shadow_channel op
    | Shadow_bracket_color (_orig, c) ->
        Color.channel_bracket_color ~theme shadow_channel c
    | Shadow_bracket_color_opacity (_orig, c, op) ->
        Color.channel_bracket_color_opacity ~theme shadow_channel c op
    | Shadow_bracket_color_var v -> Color.channel_bracket_var shadow_channel v
    | Shadow_bracket_color_var_opacity (v, op) ->
        Color.channel_bracket_var_opacity shadow_channel v op
    | Shadow_bracket_shadow s ->
        if Parse.is_var s then shadow_raw_var s else shadow_arbitrary ~theme s
    | Shadow_bracket_shadow_opacity (s, opacity) ->
        if Parse.is_var s then shadow_raw_var ~opacity s
        else shadow_arbitrary_opacity s opacity
    | Shadow_bracket_var v -> shadow_raw_var v
    | Shadow_bracket_var_opacity (v, opacity) -> shadow_raw_var ~opacity v
    | Inset_shadow_none -> inset_shadow_none
    | Inset_shadow_2xs -> inset_shadow_shape_style ~theme Ish_2xs
    | Inset_shadow_xs -> inset_shadow_shape_style ~theme Ish_xs
    | Inset_shadow_sm -> inset_shadow_shape_style ~theme Ish_sm
    | Inset_shadow -> inset_shadow_themed ~theme ()
    | Inset_shadow_arbitrary arb -> inset_shadow_arbitrary ~theme arb
    | Inset_shadow_raw (_, value, opacity) -> inset_shadow_raw value opacity
    | Inset_shadow_arbitrary_opacity (arb, op) ->
        inset_shadow_arbitrary_opacity arb op
    | Inset_shadow_shape_opacity (shape, op) ->
        inset_shadow_shape_opacity_style ~theme shape op
    | Inset_shadow_color (c, s) ->
        Color.channel_color ~theme inset_shadow_channel c s
    | Inset_shadow_color_opacity (c, s, op) ->
        Color.channel_color_opacity ~theme inset_shadow_channel c s op
    | Inset_shadow_current -> Color.channel_current inset_shadow_channel
    | Inset_shadow_current_opacity op ->
        Color.channel_current_opacity inset_shadow_channel op
    | Inset_shadow_inherit -> Color.channel_inherit inset_shadow_channel
    | Inset_shadow_transparent -> Color.channel_transparent inset_shadow_channel
    | Inset_shadow_transparent_opacity op ->
        Color.channel_transparent_opacity inset_shadow_channel op
    | Inset_shadow_bracket_color (_orig, c) ->
        Color.channel_bracket_color ~theme inset_shadow_channel c
    | Inset_shadow_bracket_color_opacity (_orig, c, op) ->
        Color.channel_bracket_color_opacity ~theme inset_shadow_channel c op
    | Inset_shadow_bracket_color_var v ->
        Color.channel_bracket_var inset_shadow_channel v
    | Inset_shadow_bracket_cvar_opacity (v, op) ->
        Color.channel_bracket_var_opacity inset_shadow_channel v op
    | Inset_shadow_bracket_shadow s ->
        if Parse.is_var s then inset_shadow_raw_var s
        else inset_shadow_arbitrary ~theme s
    | Inset_shadow_bracket_shadow_opacity (s, opacity) ->
        if Parse.is_var s then inset_shadow_raw_var ~opacity s
        else inset_shadow_arbitrary_opacity s opacity
    | Inset_shadow_bracket_var v -> inset_shadow_raw_var v
    | Inset_shadow_bracket_var_opacity (v, opacity) ->
        inset_shadow_raw_var ~opacity v
    | Opacity n -> opacity n
    | Opacity_decimal f ->
        let value = f /. 100.0 in
        style [ Css.opacity (Css.Opacity_number value) ]
    | Opacity_arbitrary (_, `Num f) ->
        style [ Css.opacity (Css.Opacity_number f) ]
    | Opacity_arbitrary (_, `Raw v) -> (
        match Parse.opaque_declaration "opacity" v with
        | Some declaration -> style [ declaration ]
        | None -> style [])
    | Opacity_var v ->
        let bare_name = Parse.extract_var_name v in
        let var_ref : Css.opacity Css.var = Var.bracket bare_name in
        style [ Css.opacity (Css.Var var_ref) ]

  let err_not_utility = Error (`Msg "Not an effects utility")
  let starts prefix s = String.starts_with ~prefix s

  let parse_shadow_bracket ~theme v =
    let base_str, opacity = Color.parse_opacity_modifier ~theme v in
    let inner = Parse.bracket_inner base_str in
    if starts "shadow:" inner then
      (* The hint says how to read the value written after it; only a var()
         reference there names a custom property. *)
      let shadow_part = String.sub inner 7 (String.length inner - 7) in
      if not (is_shadow_bracket shadow_part) then err_not_utility
      else
        match opacity with
        | Color.No_opacity -> Ok (Shadow_bracket_shadow shadow_part)
        | _ -> Ok (Shadow_bracket_shadow_opacity (shadow_part, opacity))
    else
      match Color.parse_bracket_hint inner with
      | Some (Color.Typed_var var_part) -> (
          match opacity with
          | Color.No_opacity -> Ok (Shadow_bracket_color_var var_part)
          | _ -> Ok (Shadow_bracket_color_var_opacity (var_part, opacity)))
      | Some (Color.Bare_var v) -> (
          match opacity with
          | Color.No_opacity -> Ok (Shadow_bracket_var v)
          | _ -> Ok (Shadow_bracket_var_opacity (v, opacity)))
      | Some (Color.Plain_color c) -> (
          match opacity with
          | Color.No_opacity -> Ok (Shadow_bracket_color (inner, c))
          | _ -> Ok (Shadow_bracket_color_opacity (inner, c, opacity)))
      | None when not (is_shadow_bracket inner) -> (
          match Parse.arbitrary_declaration_value inner with
          | Some value -> Ok (Shadow_raw (inner, value, opacity))
          | None -> err_not_utility)
      | None -> (
          match opacity with
          | Color.No_opacity -> Ok (Shadow_arbitrary inner)
          | _ -> Ok (Shadow_arbitrary_opacity (inner, opacity)))

  (* Whether a bracket spells [inset] in any layer. The utility supplies the
     keyword itself, so Tailwind writes the author's on top of it, [inset inset
     0 1px ...], a shadow no browser draws and one that takes the whole
     [box-shadow] composition down with it. That value has no typed form, and
     the class is refused rather than drawn as the shadow the author did not
     get. *)
  let spells_inset inner =
    match Parse.shadow (Parse.decode_arbitrary_value inner) with
    | Some (Css.Inset _ : Css.shadow) -> true
    | Some (Css.List layers : Css.shadow) ->
        List.exists
          (function (Css.Inset _ : Css.shadow) -> true | _ -> false)
          layers
    | _ -> false

  let parse_inset_shadow_bracket ~theme v =
    let base_str, opacity = Color.parse_opacity_modifier ~theme v in
    let inner = Parse.bracket_inner base_str in
    if spells_inset inner then err_not_utility
    else if starts "shadow:" inner then
      (* The hint says how to read the value written after it; only a var()
         reference there names a custom property. *)
      let shadow_part = String.sub inner 7 (String.length inner - 7) in
      if not (is_shadow_bracket shadow_part) then err_not_utility
      else
        match opacity with
        | Color.No_opacity -> Ok (Inset_shadow_bracket_shadow shadow_part)
        | _ -> Ok (Inset_shadow_bracket_shadow_opacity (shadow_part, opacity))
    else
      match Color.parse_bracket_hint inner with
      | Some (Color.Typed_var var_part) -> (
          match opacity with
          | Color.No_opacity -> Ok (Inset_shadow_bracket_color_var var_part)
          | _ -> Ok (Inset_shadow_bracket_cvar_opacity (var_part, opacity)))
      | Some (Color.Bare_var v) -> (
          match opacity with
          | Color.No_opacity -> Ok (Inset_shadow_bracket_var v)
          | _ -> Ok (Inset_shadow_bracket_var_opacity (v, opacity)))
      | Some (Color.Plain_color c) -> (
          match opacity with
          | Color.No_opacity -> Ok (Inset_shadow_bracket_color (inner, c))
          | _ -> Ok (Inset_shadow_bracket_color_opacity (inner, c, opacity)))
      | None when not (is_shadow_bracket inner) -> (
          match Parse.arbitrary_declaration_value inner with
          | Some value -> Ok (Inset_shadow_raw (inner, value, opacity))
          | None -> err_not_utility)
      | None -> (
          match opacity with
          | Color.No_opacity -> Ok (Inset_shadow_arbitrary inner)
          | _ -> Ok (Inset_shadow_arbitrary_opacity (inner, opacity)))

  (* A sized shadow inlines its token's value rather than reading it through
     [var()], so a token the [@theme] block removed has to be refused here:
     nothing downstream would see it go. The size is read under its opacity. *)
  let removed_size theme family n =
    match fst (Color.parse_opacity_modifier n) with
    | ("2xs" | "xs" | "sm" | "md" | "lg" | "xl" | "2xl" | "inner") as size ->
        Scheme.is_removed theme (family ^ "-" ^ size)
    | _ -> false

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "shadow" ] when Scheme.is_removed theme "shadow" -> err_not_utility
    | [ base ]
      when String.starts_with ~prefix:"shadow/" base
           && Scheme.is_removed theme "shadow" ->
        err_not_utility
    | [ "shadow"; n ] when removed_size theme "shadow" n -> err_not_utility
    | [ "inset"; "shadow"; n ] when removed_size theme "inset-shadow" n ->
        err_not_utility
    | [ "shadow"; "none" ] -> Ok Shadow_none
    | [ "shadow"; "2xs" ] -> Ok Shadow_2xs
    | [ "shadow"; "xs" ] -> Ok Shadow_xs
    | [ "shadow"; "sm" ] -> Ok Shadow_sm
    | [ "shadow" ] -> Ok Shadow
    | [ base ] when String.starts_with ~prefix:"shadow/" base -> (
        let _, opacity = Color.parse_opacity_modifier ~theme base in
        match opacity with
        | Color.No_opacity -> err_not_utility
        | op -> Ok (Shadow_shape_opacity (Default, op)))
    | [ "shadow"; "md" ] -> Ok Shadow_md
    | [ "shadow"; "lg" ] -> Ok Shadow_lg
    | [ "shadow"; "xl" ] -> Ok Shadow_xl
    | [ "shadow"; "2xl" ] -> Ok Shadow_2xl
    | [ "shadow"; "inner" ] -> Ok Shadow_inner
    | [ "shadow"; "inherit" ] -> Ok Shadow_inherit
    | [ "shadow"; "transparent" ] -> Ok Shadow_transparent
    | [ "shadow"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = Color.parse_opacity_modifier ~theme current_str in
        match opacity with
        | Color.No_opacity when base = "current" -> Ok Shadow_current
        | Color.No_opacity -> err_not_utility
        | _ -> Ok (Shadow_current_opacity opacity))
    | [ "shadow"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value
                (fst (Color.parse_opacity_modifier ~theme v)) ->
        parse_shadow_bracket ~theme v
    | [ "shadow"; name ] when String.contains name '/' -> (
        let base, opacity = Color.parse_opacity_modifier ~theme name in
        match (base, opacity) with
        | _, Color.No_opacity -> err_not_utility
        | "2xs", op -> Ok (Shadow_shape_opacity (Two_xs, op))
        | "xs", op -> Ok (Shadow_shape_opacity (Xs, op))
        | "sm", op -> Ok (Shadow_shape_opacity (Sm, op))
        | "md", op -> Ok (Shadow_shape_opacity (Md, op))
        | "lg", op -> Ok (Shadow_shape_opacity (Lg, op))
        | "xl", op -> Ok (Shadow_shape_opacity (Xl, op))
        | "2xl", op -> Ok (Shadow_shape_opacity (Two_xl, op))
        | "inner", op -> Ok (Shadow_shape_opacity (Inner, op))
        | "transparent", op -> Ok (Shadow_transparent_opacity op)
        | name, op when is_theme_shadow theme name ->
            Ok (Shadow_theme_opacity (name, op))
        (* Not a size: a shadeless colour with an alpha, e.g. shadow-white/10 *)
        | base, op -> (
            match Color.shade_of_strings ~theme [ base ] with
            | Ok (c, s) -> Ok (Shadow_color_opacity (c, s, op))
            | Error _ -> err_not_utility))
    | [ "shadow"; color; shade ] -> (
        match Color.shade_and_opacity_of_strings ~theme [ color; shade ] with
        | Ok (c, s, Color.No_opacity) -> Ok (Shadow_color (c, s))
        | Ok (c, s, opacity) -> Ok (Shadow_color_opacity (c, s, opacity))
        | Error _ -> err_not_utility)
    (* A shadeless colour has no shade segment: shadow-white, shadow-black. *)
    | [ "shadow"; name ] when is_theme_shadow theme name ->
        Ok (Shadow_theme name)
    | [ "shadow"; color ] -> (
        match Color.shade_of_strings ~theme [ color ] with
        | Ok (c, s) -> Ok (Shadow_color (c, s))
        | Error _ -> err_not_utility)
    | [ "inset"; "shadow"; "none" ] -> Ok Inset_shadow_none
    | [ "inset"; "shadow"; "2xs" ] -> Ok Inset_shadow_2xs
    | [ "inset"; "shadow"; "xs" ] -> Ok Inset_shadow_xs
    | [ "inset"; "shadow"; "sm" ] -> Ok Inset_shadow_sm
    (* Bare `inset-shadow` has no v4.3.1 default token; accept it only when a
       threaded @theme defines --inset-shadow. inset-shadow-{md,lg,xl,2xl} do
       not exist in v4.3.1 and fall through to err_not_utility. *)
    | [ "inset"; "shadow" ]
      when Scheme.theme_value (Some theme) "inset-shadow" <> None ->
        Ok Inset_shadow
    | [ "inset"; "shadow" ] -> err_not_utility
    | [ "inset"; "shadow"; name ] when is_theme_inset_shadow theme name ->
        Ok (Inset_shadow_theme name)
    | [ "inset"; "shadow"; "inherit" ] -> Ok Inset_shadow_inherit
    | [ "inset"; "shadow"; "transparent" ] -> Ok Inset_shadow_transparent
    | [ "inset"; "shadow"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = Color.parse_opacity_modifier ~theme current_str in
        match opacity with
        | Color.No_opacity when base = "current" -> Ok Inset_shadow_current
        | Color.No_opacity -> err_not_utility
        | _ -> Ok (Inset_shadow_current_opacity opacity))
    | [ "inset"; "shadow"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value
                (fst (Color.parse_opacity_modifier ~theme v)) ->
        parse_inset_shadow_bracket ~theme v
    | [ "inset"; "shadow"; name ] when String.contains name '/' -> (
        let base, opacity = Color.parse_opacity_modifier ~theme name in
        match (base, opacity) with
        | _, Color.No_opacity -> err_not_utility
        | "2xs", op -> Ok (Inset_shadow_shape_opacity (Ish_2xs, op))
        | "xs", op -> Ok (Inset_shadow_shape_opacity (Ish_xs, op))
        | "sm", op -> Ok (Inset_shadow_shape_opacity (Ish_sm, op))
        | "transparent", op -> Ok (Inset_shadow_transparent_opacity op)
        | name, op when is_theme_inset_shadow theme name ->
            Ok (Inset_shadow_theme_opacity (name, op))
        | base, op -> (
            match Color.shade_of_strings ~theme [ base ] with
            | Ok (c, s) -> Ok (Inset_shadow_color_opacity (c, s, op))
            | Error _ -> err_not_utility))
    | [ "inset"; "shadow"; color; shade ] -> (
        match Color.shade_and_opacity_of_strings ~theme [ color; shade ] with
        | Ok (c, s, Color.No_opacity) -> Ok (Inset_shadow_color (c, s))
        | Ok (c, s, opacity) -> Ok (Inset_shadow_color_opacity (c, s, opacity))
        | Error _ -> err_not_utility)
    | [ "inset"; "shadow"; color ] -> (
        match Color.shade_of_strings ~theme [ color ] with
        | Ok (c, s) -> Ok (Inset_shadow_color (c, s))
        | Error _ -> err_not_utility)
    | [ "opacity"; n ] when String.length n > 0 && n.[0] = '[' ->
        let len = String.length n in
        if len > 2 && n.[len - 1] = ']' then
          let inner = String.sub n 1 (len - 2) in
          if Parse.is_var inner then Ok (Opacity_var inner)
          else
            match Parse.decimal_float (Parse.decode_arbitrary_value inner) with
            | Some f -> Ok (Opacity_arbitrary (inner, `Num f))
            | None -> (
                match Parse.arbitrary_declaration_value inner with
                | Some v -> Ok (Opacity_arbitrary (inner, `Raw v))
                | None -> err_not_utility)
        else err_not_utility
    | [ "opacity"; n ] -> (
        match Parse.spacing_value ~name:"opacity" n with
        | Ok f when String.contains n '.' -> Ok (Opacity_decimal f)
        | Ok f ->
            let i = int_of_float f in
            if i >= 0 && i <= 100 then Ok (Opacity i) else err_not_utility
        | Error _ -> err_not_utility)
    | _ -> err_not_utility

  (* A shadeless colour (white, black, a theme name) takes no shade suffix. *)
  let color_shade c s =
    Color.pp c ^ if Color.is_shadeless c then "" else "-" ^ string_of_int s

  let to_class = function
    | Shadow_none -> "shadow-none"
    | Shadow_2xs -> "shadow-2xs"
    | Shadow_xs -> "shadow-xs"
    | Shadow_sm -> "shadow-sm"
    | Shadow -> "shadow"
    | Shadow_md -> "shadow-md"
    | Shadow_lg -> "shadow-lg"
    | Shadow_xl -> "shadow-xl"
    | Shadow_2xl -> "shadow-2xl"
    | Shadow_inner -> "shadow-inner"
    | Shadow_arbitrary arb -> "shadow-[" ^ arb ^ "]"
    | Shadow_raw (spelling, _, opacity) ->
        "shadow-[" ^ spelling ^ "]" ^ Color.opacity_suffix opacity
    | Shadow_theme name -> "shadow-" ^ name
    | Shadow_theme_opacity (name, op) ->
        "shadow-" ^ name ^ "/" ^ Color.pp_opacity op
    | Inset_shadow_theme name -> "inset-shadow-" ^ name
    | Inset_shadow_theme_opacity (name, op) ->
        "inset-shadow-" ^ name ^ "/" ^ Color.pp_opacity op
    | Shadow_arbitrary_opacity (arb, op) ->
        "shadow-[" ^ arb ^ "]/" ^ Color.pp_opacity op
    | Shadow_shape_opacity (shape, op) ->
        (match shape with
          | Two_xs -> "shadow-2xs"
          | Xs -> "shadow-xs"
          | Sm -> "shadow-sm"
          | Default -> "shadow"
          | Md -> "shadow-md"
          | Lg -> "shadow-lg"
          | Xl -> "shadow-xl"
          | Two_xl -> "shadow-2xl"
          | Inner -> "shadow-inner")
        ^ "/" ^ Color.pp_opacity op
    | Shadow_color (c, s) -> "shadow-" ^ color_shade c s
    | Shadow_color_opacity (c, s, op) ->
        "shadow-" ^ color_shade c s ^ "/" ^ Color.pp_opacity op
    | Shadow_current -> "shadow-current"
    | Shadow_current_opacity op -> "shadow-current/" ^ Color.pp_opacity op
    | Shadow_inherit -> "shadow-inherit"
    | Shadow_transparent -> "shadow-transparent"
    | Shadow_transparent_opacity op ->
        "shadow-transparent/" ^ Color.pp_opacity op
    | Shadow_bracket_color (orig, _c) -> "shadow-[" ^ orig ^ "]"
    | Shadow_bracket_color_opacity (orig, _c, op) ->
        "shadow-[" ^ orig ^ "]/" ^ Color.pp_opacity op
    | Shadow_bracket_color_var v -> "shadow-[color:" ^ v ^ "]"
    | Shadow_bracket_color_var_opacity (v, op) ->
        "shadow-[color:" ^ v ^ "]/" ^ Color.pp_opacity op
    | Shadow_bracket_shadow s -> "shadow-[shadow:" ^ s ^ "]"
    | Shadow_bracket_shadow_opacity (s, op) ->
        "shadow-[shadow:" ^ s ^ "]/" ^ Color.pp_opacity op
    | Shadow_bracket_var v -> "shadow-[" ^ v ^ "]"
    | Shadow_bracket_var_opacity (v, op) ->
        "shadow-[" ^ v ^ "]/" ^ Color.pp_opacity op
    | Inset_shadow_none -> "inset-shadow-none"
    | Inset_shadow_2xs -> "inset-shadow-2xs"
    | Inset_shadow_xs -> "inset-shadow-xs"
    | Inset_shadow_sm -> "inset-shadow-sm"
    | Inset_shadow -> "inset-shadow"
    | Inset_shadow_arbitrary arb -> "inset-shadow-[" ^ arb ^ "]"
    | Inset_shadow_raw (spelling, _, opacity) ->
        "inset-shadow-[" ^ spelling ^ "]" ^ Color.opacity_suffix opacity
    | Inset_shadow_arbitrary_opacity (arb, op) ->
        "inset-shadow-[" ^ arb ^ "]/" ^ Color.pp_opacity op
    | Inset_shadow_shape_opacity (shape, op) ->
        (match shape with
          | Ish_2xs -> "inset-shadow-2xs"
          | Ish_xs -> "inset-shadow-xs"
          | Ish_sm -> "inset-shadow-sm")
        ^ "/" ^ Color.pp_opacity op
    | Inset_shadow_color (c, s) -> "inset-shadow-" ^ color_shade c s
    | Inset_shadow_color_opacity (c, s, op) ->
        "inset-shadow-" ^ color_shade c s ^ "/" ^ Color.pp_opacity op
    | Inset_shadow_current -> "inset-shadow-current"
    | Inset_shadow_current_opacity op ->
        "inset-shadow-current/" ^ Color.pp_opacity op
    | Inset_shadow_inherit -> "inset-shadow-inherit"
    | Inset_shadow_transparent -> "inset-shadow-transparent"
    | Inset_shadow_transparent_opacity op ->
        "inset-shadow-transparent/" ^ Color.pp_opacity op
    | Inset_shadow_bracket_color (orig, _c) -> "inset-shadow-[" ^ orig ^ "]"
    | Inset_shadow_bracket_color_opacity (orig, _c, op) ->
        "inset-shadow-[" ^ orig ^ "]/" ^ Color.pp_opacity op
    | Inset_shadow_bracket_color_var v -> "inset-shadow-[color:" ^ v ^ "]"
    | Inset_shadow_bracket_cvar_opacity (v, op) ->
        "inset-shadow-[color:" ^ v ^ "]/" ^ Color.pp_opacity op
    | Inset_shadow_bracket_shadow s -> "inset-shadow-[shadow:" ^ s ^ "]"
    | Inset_shadow_bracket_shadow_opacity (s, op) ->
        "inset-shadow-[shadow:" ^ s ^ "]/" ^ Color.pp_opacity op
    | Inset_shadow_bracket_var v -> "inset-shadow-[" ^ v ^ "]"
    | Inset_shadow_bracket_var_opacity (v, op) ->
        "inset-shadow-[" ^ v ^ "]/" ^ Color.pp_opacity op
    | Opacity n -> "opacity-" ^ string_of_int n
    | Opacity_decimal f -> "opacity-" ^ pp_float f
    | Opacity_arbitrary (raw, _) -> "opacity-[" ^ raw ^ "]"
    | Opacity_var v -> "opacity-[" ^ v ^ "]"

  let suborder t =
    match without_named_size_alpha t with
    | Opacity n -> n * 100
    | Opacity_decimal f -> int_of_float (f *. 100.0)
    | Opacity_arbitrary _ -> 20000
    | Opacity_var _ -> 20001
    (* Shadow shape+opacity utilities come before regular shapes. Within
       arbitrary opacity, sort by arb color type: var() first, then
       currentcolor, then hex — matching Tailwind ordering *)
    | Shadow_arbitrary_opacity (arb, _) ->
        if String.contains arb '#' then 29990
        else
          let len = String.length arb in
          let rec has_var i =
            if i > len - 4 then false
            else if
              arb.[i] = 'v'
              && arb.[i + 1] = 'a'
              && arb.[i + 2] = 'r'
              && arb.[i + 3] = '('
            then true
            else has_var (i + 1)
          in
          if has_var 0 then 29988 else 29989
    | Shadow_bracket_var_opacity _ | Shadow_bracket_shadow_opacity _ -> 29988
    | Shadow_shape_opacity _ | Shadow_theme_opacity _ -> 29991
    (* Shadow shape utilities — all same suborder, natural_compare decides
       within-group order: bracket values ([) sort before named (none/sm/xl) *)
    | Shadow | Shadow_2xs | Shadow_xs | Shadow_2xl | Shadow_inner | Shadow_lg
    | Shadow_md | Shadow_none | Shadow_sm | Shadow_xl | Shadow_arbitrary _
    | Shadow_raw _ | Shadow_bracket_shadow _ | Shadow_bracket_var _
    | Shadow_theme _ ->
        30000
    (* Shadow color utilities *)
    | Shadow_color _ | Shadow_color_opacity _ | Shadow_current
    | Shadow_current_opacity _ | Shadow_inherit | Shadow_transparent
    | Shadow_transparent_opacity _ | Shadow_bracket_color _
    | Shadow_bracket_color_opacity _ | Shadow_bracket_color_var _
    | Shadow_bracket_color_var_opacity _ ->
        40000
    (* Inset shadow opacity utilities — same relative scheme as shadow *)
    | Inset_shadow_arbitrary_opacity (arb, _) ->
        let len = String.length arb in
        let rec has_var i =
          if i > len - 4 then false
          else if
            arb.[i] = 'v'
            && arb.[i + 1] = 'a'
            && arb.[i + 2] = 'r'
            && arb.[i + 3] = '('
          then true
          else has_var (i + 1)
        in
        if has_var 0 then 31988
        else if String.contains arb '#' then 31990
        else 31989
    | Inset_shadow_bracket_var_opacity _ | Inset_shadow_bracket_shadow_opacity _
      ->
        31988
    | Inset_shadow_shape_opacity _ | Inset_shadow_theme_opacity _ -> 31991
    (* Inset shadow shape utilities — all same suborder, natural_compare
       decides *)
    | Inset_shadow | Inset_shadow_2xs | Inset_shadow_xs | Inset_shadow_none
    | Inset_shadow_sm | Inset_shadow_arbitrary _ | Inset_shadow_raw _
    | Inset_shadow_bracket_shadow _ | Inset_shadow_bracket_var _
    | Inset_shadow_theme _ ->
        32000
    (* Inset shadow color utilities *)
    | Inset_shadow_color _ | Inset_shadow_color_opacity _ | Inset_shadow_current
    | Inset_shadow_current_opacity _ | Inset_shadow_inherit
    | Inset_shadow_transparent | Inset_shadow_bracket_color _
    | Inset_shadow_transparent_opacity _ | Inset_shadow_bracket_color_opacity _
    | Inset_shadow_bracket_color_var _ | Inset_shadow_bracket_cvar_opacity _ ->
        42000

  let examples = [ Shadow_none; Opacity 50 ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let order u = Some (Utility.order u)
let shadow_none = utility Shadow_none
let shadow_2xs = utility Shadow_2xs
let shadow_xs = utility Shadow_xs
let shadow_sm = utility Shadow_sm
let shadow = utility Shadow
let shadow_md = utility Shadow_md
let shadow_lg = utility Shadow_lg
let shadow_xl = utility Shadow_xl
let shadow_2xl = utility Shadow_2xl
let shadow_inner = utility Shadow_inner
let inset_shadow_none = utility Inset_shadow_none
let inset_shadow_2xs = utility Inset_shadow_2xs
let inset_shadow_xs = utility Inset_shadow_xs
let inset_shadow_sm = utility Inset_shadow_sm
let inset_shadow = utility Inset_shadow
let opacity n = utility (Opacity n)

(* The ring/shadow variables, shared with Ring and Forms. *)
let shadow_var = Handler.shadow_var
let inset_shadow_var = Handler.inset_shadow_var
let inset_ring_color_var = Handler.inset_ring_color_var
let inset_ring_shadow_var = Handler.inset_ring_shadow_var
let ring_inset_var = Handler.ring_inset_var
let ring_offset_width_var = Handler.ring_offset_width_var
let ring_offset_color_var = Handler.ring_offset_color_var
let ring_color_var = Handler.ring_color_var
let ring_offset_shadow_var = Handler.ring_offset_shadow_var
let ring_shadow_var = Handler.ring_shadow_var
let shadow_property_rules = Handler.shadow_property_rules
let shadow_property_metadata = Handler.shadow_property_metadata
