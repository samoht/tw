(** Text shadow utilities for CSS text-shadow property. *)

module Css = Cascade.Css

(* Capture project Pp helpers before open Css shadows Pp with Css.Pp. *)
let pp_str = Pp.str
let pp_hex_byte = Pp.hex_byte

module Handler = struct
  open Style
  open Css

  type shape = S_2xs | S_xs | S_sm | S_md | S_lg

  (* Color in an arbitrary shadow value *)
  type arb_color =
    | Hex of string
    | Var_ref of string
    | Css_color of Css.color
    | No_color

  type t =
    | None
    | Shape of shape
    | Shape_opacity of shape * Color.opacity_modifier
    | Color of Color.color * int
    | Color_opacity of Color.color * int * Color.opacity_modifier
    | Current
    | Current_opacity of Color.opacity_modifier
    | Inherit
    | Transparent
    | Transparent_opacity of Color.opacity_modifier
    | Bracket_hex of string
    | Bracket_hex_opacity of string * Color.opacity_modifier
    | Bracket_color of string * Css.color
    | Bracket_color_opacity of string * Css.color * Color.opacity_modifier
    | Bracket_color_var of string
    | Bracket_cvar_opacity of string * Color.opacity_modifier
    | Bracket_shadow of string
    | Bracket_var of string
    | Arbitrary of string
    | Arbitrary_opacity of string * Color.opacity_modifier

  let name = "text_shadow"

  (* An opacity-bearing shadow writes the alpha channel before text-shadow, so
     Tailwind places it after user-select and before backface visibility. The
     ordinary shape and colour utilities remain in the later text-shadow
     band. *)
  let priority = function
    | Shape_opacity _ | Arbitrary_opacity _ -> 38
    | _ -> 41

  let text_shadow_color_var =
    Var.channel ~needs_property:true ~property_order:35 ~family:`Text_shadow
      Css.Color "tw-text-shadow-color"

  (* The alpha channel is a [<percentage>], and an opacity modifier may name a
     custom property to read it from, so the channel takes both spellings. *)
  let text_shadow_alpha_name = "tw-text-shadow-alpha"
  let full_alpha : Css.percentage = Pct 100.

  let text_shadow_alpha_var =
    Var.property_default Css.Percentage ~initial:full_alpha ~property_order:36
      ~family:`Text_shadow text_shadow_alpha_name

  let text_shadow_property_rules =
    [
      Var.property_rule text_shadow_color_var;
      Var.property_rule text_shadow_alpha_var;
    ]
    |> List.filter_map Fun.id |> Css.concat

  let text_shadow_property_metadata =
    [ Var.metadata text_shadow_color_var; Var.metadata text_shadow_alpha_var ]

  let shorten_hex = Color.shorten_hex_str

  (* A [#] value only names a colour when what follows is a hex spelling;
     [Css.hex] raises on anything else, here once the sheet is rendered. *)
  let is_hex_value s =
    String.length s > 0 && s.[0] = '#' && Stdlib.Option.is_some (Css.hex_opt s)

  let alpha_decl percent =
    let alpha : Css.percentage = Pct percent in
    fst (Var.binding text_shadow_alpha_var alpha)

  let opacity_percentage opacity : Css.percentage =
    match Color.opacity_var_bare_of opacity with
    | Some name -> Var (Var.bracket name)
    | None -> Pct (Color.opacity_to_percent opacity)

  let opacity_decl opacity =
    fst (Var.binding text_shadow_alpha_var (opacity_percentage opacity))

  let color_mix_supports decls =
    Css.supports ~condition:Color.color_mix_supports_condition
      [ Css.rule ~selector:(Css.Selector.class_ "_") decls ]

  let relative_color_supports =
    Css.Supports.property "color" "lab(from red l a b)"

  let make_color_var vn : Css.color = Css.Var (Var.bracket vn)

  let make_full_color_var (v : string) : Css.color =
    match Css.parse_color v with
    | Some c -> c
    | None -> make_color_var (Parse.extract_var_name v)

  (* A relative colour is carried as a verbatim body, so this one path spells
     the alpha the channel would otherwise be given as a value. *)
  let relative_oklab_from_color v opacity =
    let alpha =
      Css.Pp.to_string ~minify:true
        (Css.Values.pp_percentage ~always:true)
        (opacity_percentage opacity)
    in
    Css.parse_color (pp_str [ "oklab(from "; v; " l a b / "; alpha; ")" ])

  (* ============ Parse arbitrary shadow ============ *)

  (* The two colours the utility has to keep as written: a [#] hex it
     re-shortens and a var() it wraps. Everything else in the value is read as a
     length, so a colour spelled any other way lands in a length slot. *)
  let scan_verbatim_colour (s : string) :
      (length * length * length option * arb_color) option =
    let normalized = String.map (fun c -> if c = '_' then ' ' else c) s in
    let parts = String.split_on_char ' ' normalized in
    let rec find_color_and_lengths acc (parts : string list) :
        string list * arb_color =
      match parts with
      | [] -> (List.rev acc, No_color)
      | x :: _rest when String.length x > 0 && x.[0] = '#' ->
          (List.rev acc, Hex x)
      | x :: _rest when String.length x > 4 && String.sub x 0 4 = "var(" ->
          (List.rev acc, Var_ref x)
      | x :: rest when Parse.is_css_color_fn x -> (
          (* A colour function may carry spaces, so it runs to the end of the
             value. *)
          match Css.parse_color (String.concat " " (x :: rest)) with
          | Some c -> (List.rev acc, Css_color c)
          | None -> find_color_and_lengths (x :: acc) rest)
      | x :: rest -> find_color_and_lengths (x :: acc) rest
    in
    let length_strs, color = find_color_and_lengths [] parts in
    let lengths = List.filter_map Parse.arbitrary_length length_strs in
    (* A token that is not a length makes the value not a shadow. Dropping it
       instead would slide the surviving lengths into the wrong slots. CSS
       text-shadow has no spread, so a fourth length is not one either. *)
    if List.compare_lengths lengths length_strs <> 0 then Stdlib.Option.None
    else
      match color with
      | Hex h when not (is_hex_value h) -> Stdlib.Option.None
      | _ -> (
          match lengths with
          | [ h; v ] -> Some (h, v, Stdlib.Option.None, color)
          | [ h; v; blur ] -> Some (h, v, Some blur, color)
          | _ -> Stdlib.Option.None)

  (* What the scan above cannot spell goes to the value parser, which reads the
     rest of the CSS colour grammar: [red] is a colour to it and a failed length
     to the scan. This is the fallback the box-shadow family already has. *)
  let parse_arbitrary_shadow (s : string) :
      (length * length * length option * arb_color) option =
    match scan_verbatim_colour s with
    | Some _ as shadow -> shadow
    | Stdlib.Option.None -> (
        let normalized = String.map (fun c -> if c = '_' then ' ' else c) s in
        match Css.parse_shadow normalized with
        (* CSS text-shadow has no spread, so a body carrying one is not one. *)
        | Some
            (Shadow
               { h_offset; v_offset; blur; spread = Stdlib.Option.None; color })
          ->
            let color =
              match color with
              | Some c -> Css_color c
              | Stdlib.Option.None -> No_color
            in
            Some (h_offset, v_offset, blur, color)
        | _ -> Stdlib.Option.None)

  (* ============ Shape definitions ============ *)

  (* v4.3.1 default text-shadow scale (the values the bare CLI emits). 2xs/xs
     are single shadows; sm/md/lg are 3-shadow stacks. *)
  let shape_shadows (shape : shape) :
      (length * length * length option * string) list =
    match shape with
    | S_2xs -> [ ((Px 0. : length), Px 1., Some (Px 0.), "#00000026") ]
    | S_xs -> [ ((Px 0. : length), Px 1., Some (Px 1.), "#00000033") ]
    | S_sm ->
        [
          ((Px 0. : length), Px 1., Some (Px 0.), "#00000013");
          (Px 0., Px 1., Some (Px 1.), "#00000013");
          (Px 0., Px 2., Some (Px 2.), "#00000013");
        ]
    | S_md ->
        [
          ((Px 0. : length), Px 1., Some (Px 1.), "#0000001a");
          (Px 0., Px 1., Some (Px 2.), "#0000001a");
          (Px 0., Px 2., Some (Px 4.), "#0000001a");
        ]
    | S_lg ->
        [
          ((Px 0. : length), Px 1., Some (Px 2.), "#0000001a");
          (Px 0., Px 3., Some (Px 2.), "#0000001a");
          (Px 0., Px 4., Some (Px 8.), "#0000001a");
        ]

  (* Publish the scale through the theme-token registry, the way rule.ml
     publishes the breakpoints, so [theme(static)] emits it. The utilities
     inline the value rather than referencing a [--text-shadow-*] token, so
     nothing else would put it in the sheet. The reference keeps the [px] on a
     zero offset here, so render the lengths as written. *)
  let () =
    List.iter
      (fun (name, shape) ->
        let one (h, v, blur, hex) =
          String.concat " "
            ([
               Css.Pp.to_string ~minify:true (Css.pp_length ~always:true) h;
               Css.Pp.to_string ~minify:true (Css.pp_length ~always:true) v;
             ]
            @ (match blur with
              | Some b ->
                  [
                    Css.Pp.to_string ~minify:true (Css.pp_length ~always:true) b;
                  ]
              | None -> [])
            @ [ hex ])
        in
        Scheme.register_default_token name
          (String.concat ", " (List.map one (shape_shadows shape))))
      [
        ("text-shadow-2xs", S_2xs);
        ("text-shadow-xs", S_xs);
        ("text-shadow-sm", S_sm);
        ("text-shadow-md", S_md);
        ("text-shadow-lg", S_lg);
      ]

  (* Theme token name for a shape's scale value (matches the @theme keys, e.g.
     --text-shadow-2xs). *)
  let shape_token = function
    | S_2xs -> "text-shadow-2xs"
    | S_xs -> "text-shadow-xs"
    | S_sm -> "text-shadow-sm"
    | S_md -> "text-shadow-md"
    | S_lg -> "text-shadow-lg"

  (* The hex spelling of a colour, for the colour maths that reads one. A colour
     the fold refuses - one whose channels are not all bytes, or one in a space
     with no sRGB spelling - has none. *)
  let hex_string_of_css_color (c : Css.color) : string option =
    let spell r g b a =
      let bh = pp_hex_byte in
      "#" ^ bh r ^ bh g ^ bh b ^ if a = 255 then "" else bh a
    in
    match (Color.css_color_to_hex c, c) with
    | Some (Css.Hex { r; g; b; a } | Css.Authored_hex { r; g; b; a; _ }), _
    | _, (Css.Hex { r; g; b; a } | Css.Authored_hex { r; g; b; a; _ }) ->
        Some (spell r g b a)
    | _ -> Stdlib.Option.None

  (* Parse a theme shadow-list override (e.g. "0px 1px 0px rgb(0 0 0 / 0.1), 0px
     2px 2px rgb(0 0 0 / 0.06)") into the same (h, v, blur, hex) tuples
     [shape_shadows] returns. Splits on top-level commas, then for each shadow
     takes the leading length tokens and converts the trailing colour to a hex
     string. *)
  let hex_string_of_color (s : string) : string =
    match Css.parse_color s with
    | Some c -> (
        match hex_string_of_css_color c with Some hex -> hex | None -> s)
    | None -> s

  let parse_shadow_list (s : string) :
      (length * length * length option * string) list =
    let split_top_level str =
      let buf = Buffer.create 32 and acc = ref [] and depth = ref 0 in
      String.iter
        (fun c ->
          if c = '(' then (
            incr depth;
            Buffer.add_char buf c)
          else if c = ')' then (
            decr depth;
            Buffer.add_char buf c)
          else if c = ',' && !depth = 0 then (
            acc := Buffer.contents buf :: !acc;
            Buffer.clear buf)
          else Buffer.add_char buf c)
        str;
      acc := Buffer.contents buf :: !acc;
      List.rev_map String.trim !acc
    in
    let parse_len str : length option =
      match str with
      | "0" -> Some (Zero : length)
      | _ ->
          let n = String.length str in
          let cut suf = String.sub str 0 (n - String.length suf) in
          if Filename.check_suffix str "px" then
            Option.map
              (fun f -> (Px f : length))
              (float_of_string_opt (cut "px"))
          else if Filename.check_suffix str "rem" then
            Option.map
              (fun f -> (Rem f : length))
              (float_of_string_opt (cut "rem"))
          else None
    in
    let parse_one shadow =
      let toks = String.split_on_char ' ' shadow |> List.filter (( <> ) "") in
      let rec take_lengths acc = function
        | t :: rest -> (
            match parse_len t with
            | Some l -> take_lengths (l :: acc) rest
            | None -> (List.rev acc, t :: rest))
        | [] -> (List.rev acc, [])
      in
      let lengths, color_toks = take_lengths [] toks in
      let color = hex_string_of_color (String.concat " " color_toks) in
      match lengths with
      | [ h; v ] -> Some (h, v, Stdlib.Option.None, color)
      | [ h; v; blur ] -> Some (h, v, Some blur, color)
      | _ -> Stdlib.Option.None
    in
    List.filter_map parse_one (split_top_level s)

  (* Shadows for a shape: a threaded [@theme] override if present, else the
     v4.3.1 default scale. *)
  let shadows_for ?theme shape =
    match Scheme.theme_value theme (shape_token shape) with
    | Some override -> parse_shadow_list override
    | None -> shape_shadows shape

  let shape_text_shadow ?theme shape =
    let shadows = shadows_for ?theme shape in
    let text_shadows =
      List.map
        (fun (h, v, blur, fallback_hex) ->
          let color_ref =
            Var.reference_with_fallback text_shadow_color_var
              (Css.hex fallback_hex)
          in
          Css.Text_shadow
            { h_offset = h; v_offset = v; blur; color = Some (Var color_ref) })
        shadows
    in
    match text_shadows with
    | [ single ] -> Css.text_shadow single
    | multiple -> Css.text_shadows multiple

  let shape_text_shadow_opacity ?theme shape opacity =
    let shadows = shadows_for ?theme shape in
    let percent = Color.opacity_to_percent opacity in
    let alpha = percent /. 100.0 in
    let text_shadows =
      List.map
        (fun (h, v, blur, fallback_hex) ->
          let base_hex =
            if String.length fallback_hex = 9 then String.sub fallback_hex 0 7
            else fallback_hex
          in
          let oklab_fallback = Color.hex_to_oklab_alpha base_hex alpha in
          let color_ref =
            Var.reference_with_fallback text_shadow_color_var oklab_fallback
          in
          Css.Text_shadow
            { h_offset = h; v_offset = v; blur; color = Some (Var color_ref) })
        shadows
    in
    match text_shadows with
    | [ single ] -> Css.text_shadow single
    | multiple -> Css.text_shadows multiple

  (* ============ Color-setting styles ============ *)

  let color_hex ?theme c shade =
    let color_name = Color.scheme_color_name c shade in
    let scheme = match theme with Some t -> t | None -> Scheme.default in
    match Scheme.hex_color scheme color_name with
    | Some h -> h
    | Stdlib.Option.None -> (
        match Scheme.theme_value theme ("color-" ^ color_name) with
        | Some h -> h
        | Stdlib.Option.None ->
            let oklch = Color.to_oklch c shade in
            let rgb = Color.oklch_to_rgb oklch in
            Color.rgb_to_hex rgb)

  let set_color ?theme c shade =
    let color_value =
      Color.property_color_value ?theme ~property_prefix:"text-shadow-color" c
        shade
    in
    let base_decl, _ = Var.binding text_shadow_color_var color_value in
    let theme_color_var =
      Color.property_color_var ?theme ~property_prefix:"text-shadow-color" c
        shade
    in
    let theme_decl, color_ref = Var.binding theme_color_var color_value in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:text_shadow_alpha_name
        (Css.Var color_ref) Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ theme_decl; enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~metadata:text_shadow_property_metadata
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_color_opacity ?theme c shade opacity =
    let percent = Color.opacity_to_percent opacity in
    let hex_value = color_hex ?theme c shade in
    let hex_with_alpha = Color.hex_with_alpha hex_value percent in
    let base_decl, _ =
      Var.binding text_shadow_color_var (Css.hex hex_with_alpha)
    in
    let theme_color_var = Color.color_var c shade in
    let theme_decl, color_ref =
      Var.binding theme_color_var (Css.hex hex_value)
    in
    let inner_mix =
      Css.color_mix ~in_space:Oklab (Css.Var color_ref) Css.Transparent
        ~percent1:percent
    in
    let outer_mix =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:text_shadow_alpha_name
        inner_mix Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var outer_mix in
    let supports_block = color_mix_supports [ theme_decl; enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~metadata:text_shadow_property_metadata
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_current () =
    let base_decl, _ = Var.binding text_shadow_color_var Css.Current in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:text_shadow_alpha_name
        Css.Current Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~metadata:text_shadow_property_metadata
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_current_opacity opacity =
    let percent = Color.opacity_to_percent opacity in
    let base_decl, _ = Var.binding text_shadow_color_var Css.Current in
    let inner_mix =
      Css.color_mix ~in_space:Oklab Css.Current Css.Transparent
        ~percent1:percent
    in
    let outer_mix =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:text_shadow_alpha_name
        inner_mix Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var outer_mix in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~metadata:text_shadow_property_metadata
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_transparent () =
    let base_decl, _ = Var.binding text_shadow_color_var Css.Transparent in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:text_shadow_alpha_name
        Css.Transparent Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~metadata:text_shadow_property_metadata
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_transparent_opacity opacity =
    let base_decl, _ = Var.binding text_shadow_color_var Css.Transparent in
    let inner_mix = Color.apply_alpha opacity Css.Transparent in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:text_shadow_alpha_name
        inner_mix Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~metadata:text_shadow_property_metadata
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_inherit () =
    let base_decl, _ = Var.binding text_shadow_color_var Css.Inherit in
    style ~metadata:text_shadow_property_metadata
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_bracket_hex hex =
    let short = shorten_hex ("#" ^ hex) in
    let base_decl, _ = Var.binding text_shadow_color_var (Css.hex short) in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:text_shadow_alpha_name
        (Css.hex short) Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~metadata:text_shadow_property_metadata
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_bracket_hex_opacity hex opacity =
    let percent = Color.opacity_to_percent opacity in
    let hex_with_alpha = Color.hex_with_alpha hex percent in
    let base_decl, _ =
      Var.binding text_shadow_color_var (Css.hex hex_with_alpha)
    in
    let alpha = percent /. 100.0 in
    let oklab_color = Color.hex_to_oklab_alpha hex alpha in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:text_shadow_alpha_name
        oklab_color Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~metadata:text_shadow_property_metadata
      ~property_rules:text_shadow_property_rules [ base_decl ]

  (* A bracket colour spelled any way but a [#] hex: a name, a colour function
     or a relative colour. One with an sRGB hex takes it, so it reads the same
     as the hex arm above; one without stays as written. *)
  let set_bracket_color (c : Css.color) =
    let c = match Color.css_color_to_hex c with Some h -> h | None -> c in
    let base_decl, _ = Var.binding text_shadow_color_var c in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:text_shadow_alpha_name
        c Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~metadata:text_shadow_property_metadata
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_bracket_color_opacity (c : Css.color) opacity =
    let c = match Color.css_color_to_hex c with Some h -> h | None -> c in
    let guarded = Color.mix_alpha ~in_space:Oklab opacity c in
    (* A modifier reading a custom property has no percentage a plain fallback
       can hold, so the fallback keeps the colour as written. *)
    let base_value =
      if Stdlib.Option.is_some (Color.opacity_var_bare_of opacity) then c
      else Color.mix_alpha ~in_space:Srgb opacity c
    in
    let base_decl, _ = Var.binding text_shadow_color_var base_value in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:text_shadow_alpha_name
        guarded Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~metadata:text_shadow_property_metadata
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_bracket_color_var var_expr =
    let var_name = Parse.extract_var_name var_expr in
    let var_color = make_color_var var_name in
    let base_decl, _ = Var.binding text_shadow_color_var var_color in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:text_shadow_alpha_name
        var_color Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~metadata:text_shadow_property_metadata
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_bracket_color_var_opacity var_expr opacity =
    let percent = Color.opacity_to_percent opacity in
    let var_name = Parse.extract_var_name var_expr in
    let var_color = make_color_var var_name in
    let base_decl, _ = Var.binding text_shadow_color_var var_color in
    let inner_mix =
      Css.color_mix ~in_space:Oklab var_color Css.Transparent ~percent1:percent
    in
    let outer_mix =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:text_shadow_alpha_name
        inner_mix Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var outer_mix in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~metadata:text_shadow_property_metadata
      ~property_rules:text_shadow_property_rules [ base_decl ]

  (* ============ Arbitrary shadow styles ============ *)

  let make_text_shadow_var var_expr : Css.text_shadow =
    Css.Var (Var.bracket (Parse.extract_var_name var_expr))

  let arbitrary_shadow_style arb =
    match parse_arbitrary_shadow arb with
    | Some (h_offset, v_offset, blur, color) ->
        let fallback_color : Css.color =
          match color with
          | Hex c -> Css.hex (shorten_hex c)
          | Var_ref v -> make_full_color_var v
          | Css_color c -> (
              match Color.css_color_to_hex c with Some h -> h | None -> c)
          | No_color -> Css.Current
        in
        let color_ref =
          Var.reference_with_fallback text_shadow_color_var fallback_color
        in
        style ~metadata:text_shadow_property_metadata
          ~property_rules:text_shadow_property_rules
          [
            Css.text_shadow
              (Css.Text_shadow
                 { h_offset; v_offset; blur; color = Some (Var color_ref) });
          ]
    | Stdlib.Option.None -> style [ Css.text_shadow Css.None ]

  let arbitrary_shadow_opacity_style arb opacity =
    match parse_arbitrary_shadow arb with
    | Some (h_offset, v_offset, blur, color) ->
        let percent = Color.opacity_to_percent opacity in
        let alpha = percent /. 100.0 in
        let dynamic_opacity = Color.opacity_var_bare_of opacity <> None in
        let alpha_d = opacity_decl opacity in
        let base_fallback : Css.color =
          match color with
          | Hex c -> Color.hex_to_oklab_alpha c alpha
          | Var_ref v -> make_full_color_var v
          | Css_color c -> (
              match hex_string_of_css_color c with
              | Some hex -> Color.hex_to_oklab_alpha hex alpha
              | None -> if dynamic_opacity then c else Color.mix_alpha opacity c
              )
          | No_color -> Css.Current
        in
        let base_color_ref =
          Var.reference_with_fallback text_shadow_color_var base_fallback
        in
        let base_shadow =
          Css.text_shadow
            (Css.Text_shadow
               { h_offset; v_offset; blur; color = Some (Var base_color_ref) })
        in
        let relative_support origin =
          match relative_oklab_from_color origin opacity with
          | Some relative_color ->
              let enhanced_ref =
                Var.reference_with_fallback text_shadow_color_var relative_color
              in
              let enhanced_shadow =
                Css.text_shadow
                  (Css.Text_shadow
                     {
                       h_offset;
                       v_offset;
                       blur;
                       color = Some (Var enhanced_ref);
                     })
              in
              Some
                [
                  Css.supports ~condition:relative_color_supports
                    [
                      Css.rule ~selector:(Css.Selector.class_ "_")
                        [ enhanced_shadow ];
                    ];
                ]
          | None -> Stdlib.Option.None
        in
        let rules =
          match color with
          | Hex c when dynamic_opacity -> relative_support c
          | Css_color c when dynamic_opacity ->
              let origin = Css.Pp.to_string ~minify:true Css.pp_color c in
              relative_support origin
          | Hex _ | Css_color _ -> Stdlib.Option.None
          | Var_ref v -> relative_support v
          | No_color ->
              let color_mix_fallback = Color.mix_alpha opacity Css.Current in
              let enhanced_ref =
                Var.reference_with_fallback text_shadow_color_var
                  color_mix_fallback
              in
              let enhanced_shadow =
                Css.text_shadow
                  (Css.Text_shadow
                     {
                       h_offset;
                       v_offset;
                       blur;
                       color = Some (Var enhanced_ref);
                     })
              in
              let supports_block = color_mix_supports [ enhanced_shadow ] in
              Some [ supports_block ]
        in
        style ~rules ~metadata:text_shadow_property_metadata
          ~property_rules:text_shadow_property_rules [ alpha_d; base_shadow ]
    | Stdlib.Option.None -> style [ Css.text_shadow Css.None ]

  (* ============ Style dispatch ============ *)

  let to_style theme =
    let set_color c shade = set_color ~theme c shade in
    let set_color_opacity c shade opacity =
      set_color_opacity ~theme c shade opacity
    in
    let shape_text_shadow shape = shape_text_shadow ~theme shape in
    let shape_text_shadow_opacity shape opacity =
      shape_text_shadow_opacity ~theme shape opacity
    in
    function
    | None ->
        style ~metadata:text_shadow_property_metadata
          ~property_rules:text_shadow_property_rules
          [ Css.text_shadow Css.None ]
    | Shape shape ->
        style ~metadata:text_shadow_property_metadata
          ~property_rules:text_shadow_property_rules
          [ shape_text_shadow shape ]
    | Shape_opacity (shape, opacity) ->
        let percent = Color.opacity_to_percent opacity in
        style ~metadata:text_shadow_property_metadata
          ~property_rules:text_shadow_property_rules
          [ alpha_decl percent; shape_text_shadow_opacity shape opacity ]
    | Color (c, shade) -> set_color c shade
    | Color_opacity (c, shade, opacity) -> set_color_opacity c shade opacity
    | Current -> set_current ()
    | Current_opacity opacity -> set_current_opacity opacity
    | Inherit -> set_inherit ()
    | Transparent -> set_transparent ()
    | Transparent_opacity opacity -> set_transparent_opacity opacity
    | Bracket_hex hex -> set_bracket_hex hex
    | Bracket_hex_opacity (hex, opacity) -> set_bracket_hex_opacity hex opacity
    | Bracket_color (_orig, c) -> set_bracket_color c
    | Bracket_color_opacity (_orig, c, opacity) ->
        set_bracket_color_opacity c opacity
    | Bracket_color_var var_expr -> set_bracket_color_var var_expr
    | Bracket_cvar_opacity (var_expr, opacity) ->
        set_bracket_color_var_opacity var_expr opacity
    | Bracket_shadow var_expr ->
        style ~metadata:text_shadow_property_metadata
          ~property_rules:text_shadow_property_rules
          [ Css.text_shadow (make_text_shadow_var var_expr) ]
    | Bracket_var var_expr ->
        style ~metadata:text_shadow_property_metadata
          ~property_rules:text_shadow_property_rules
          [ Css.text_shadow (make_text_shadow_var var_expr) ]
    | Arbitrary arb -> arbitrary_shadow_style arb
    | Arbitrary_opacity (arb, opacity) ->
        arbitrary_shadow_opacity_style arb opacity

  (* ============ Parsing ============ *)

  let err_not_utility = Error (`Msg "Not a text shadow utility")
  let has_opacity s = String.contains s '/'

  let starts_with prefix s =
    String.length s >= String.length prefix
    && String.sub s 0 (String.length prefix) = prefix

  let is_shadow_value inner =
    (* A shadow value has explicit length dimensions like "10px_10px" *)
    let has_length_unit s =
      let len = String.length s in
      (* Scan past digits (and optional decimal part), return position after *)
      let scan_number i =
        let j = ref i in
        while !j < len && s.[!j] >= '0' && s.[!j] <= '9' do
          incr j
        done;
        if !j < len && s.[!j] = '.' then (
          incr j;
          while !j < len && s.[!j] >= '0' && s.[!j] <= '9' do
            incr j
          done);
        !j
      in
      let has_unit_at j =
        let rest = if j < len then String.sub s j (min 3 (len - j)) else "" in
        starts_with "px" rest || starts_with "rem" rest || starts_with "em" rest
      in
      let rec check i =
        if i >= len - 1 then false
        else if s.[i] >= '0' && s.[i] <= '9' then
          let j = scan_number (i + 1) in
          has_unit_at j || check j
        else check (i + 1)
      in
      check 0
    in
    let has_typed_prefix =
      starts_with "color:" inner || starts_with "shadow:" inner
    in
    has_length_unit inner && not has_typed_prefix

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "text"; "shadow"; "none" ] -> Ok None
    (* Bare `text-shadow` is not a utility in v4 (no `--text-shadow` token); the
       named scale is `text-shadow-{2xs,xs,sm,md,lg}`. *)
    | [ "text"; "shadow" ] -> err_not_utility
    | [ "text"; "shadow"; size_str ] -> (
        let base, opacity = Color.parse_opacity_modifier ~theme size_str in
        let shape_opt =
          match base with
          | "2xs" -> Some S_2xs
          | "xs" -> Some S_xs
          | "sm" -> Some S_sm
          | "md" -> Some S_md
          | "lg" -> Some S_lg
          | _ -> Stdlib.Option.None
        in
        match (shape_opt, opacity) with
        | Some shape, Color.No_opacity -> Ok (Shape shape)
        | Some shape, op -> Ok (Shape_opacity (shape, op))
        | Stdlib.Option.None, Color.No_opacity when base = "inherit" ->
            Ok Inherit
        | Stdlib.Option.None, _ when base = "inherit" -> err_not_utility
        | Stdlib.Option.None, Color.No_opacity when base = "transparent" ->
            Ok Transparent
        | Stdlib.Option.None, op when base = "transparent" ->
            Ok (Transparent_opacity op)
        | Stdlib.Option.None, _ when starts_with "current" base -> (
            match opacity with
            | Color.No_opacity when base = "current" -> Ok Current
            | Color.No_opacity -> err_not_utility
            | op -> Ok (Current_opacity op))
        | Stdlib.Option.None, _ when Parse.is_bracket_value base -> (
            let inner = Parse.bracket_inner base in
            if starts_with "color:" inner then
              (* The hint says the payload is a colour, not that it names a
                 variable. A [var()] still reads as one; a colour CSS can spell
                 is that colour, as Tailwind emits it. *)
              let payload = String.sub inner 6 (String.length inner - 6) in
              (* A [var()] keeps the variable path even though it parses as a
                 colour: with an opacity modifier Tailwind writes the plain
                 reference as the fallback and mixes only inside the [@supports]
                 guard, which is what that path builds. *)
              let as_colour =
                if Parse.is_var payload then Stdlib.Option.None
                else Color.parse_bracket_color payload
              in
              match (as_colour, opacity) with
              (* [inner], not [payload]: the class name keeps the hint the
                 author wrote, so it reads back as the class they spelled. *)
              | Some c, Color.No_opacity -> Ok (Bracket_color (inner, c))
              | Some c, op -> Ok (Bracket_color_opacity (inner, c, op))
              | Stdlib.Option.None, Color.No_opacity ->
                  Ok (Bracket_color_var payload)
              | Stdlib.Option.None, op ->
                  Ok (Bracket_cvar_opacity (payload, op))
            else if starts_with "shadow:" inner then
              let var_part = String.sub inner 7 (String.length inner - 7) in
              Ok (Bracket_shadow var_part)
            else if Parse.is_var inner && not (is_shadow_value inner) then
              Ok (Bracket_var inner)
            else if is_hex_value inner then
              let hex = String.sub inner 1 (String.length inner - 1) in
              match opacity with
              | Color.No_opacity -> Ok (Bracket_hex hex)
              | op -> Ok (Bracket_hex_opacity (hex, op))
            else
              match Color.parse_bracket_color inner with
              | Some c -> (
                  match opacity with
                  | Color.No_opacity -> Ok (Bracket_color (inner, c))
                  | op -> Ok (Bracket_color_opacity (inner, c, op)))
              | Stdlib.Option.None -> (
                  if parse_arbitrary_shadow inner = Stdlib.Option.None then
                    (* Not a shadow, so not a utility: it used to fall back to
                       [text-shadow: none]. *)
                    err_not_utility
                  else
                    match opacity with
                    | Color.No_opacity -> Ok (Arbitrary inner)
                    | op -> Ok (Arbitrary_opacity (inner, op))))
        (* Not a size: a shadeless colour, which the multi-segment colour cases
           below never see because it fits in this single segment. *)
        | Stdlib.Option.None, Color.No_opacity -> (
            match Color.shade_of_strings ~theme [ base ] with
            | Ok (color, shade) -> Ok (Color (color, shade))
            | Error e -> Error e)
        | Stdlib.Option.None, op -> (
            match Color.shade_of_strings ~theme [ base ] with
            | Ok (color, shade) -> Ok (Color_opacity (color, shade, op))
            | Error e -> Error e))
    | "text" :: "shadow" :: color_parts when List.exists has_opacity color_parts
      -> (
        match Color.shade_and_opacity_of_strings ~theme color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Color_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "text" :: "shadow" :: color_parts -> (
        match Color.shade_of_strings ~theme color_parts with
        | Ok (color, shade) -> Ok (Color (color, shade))
        | Error e -> Error e)
    | _ -> err_not_utility

  (* ============ Class name generation ============ *)

  let shape_to_string = function
    | S_2xs -> "2xs"
    | S_xs -> "xs"
    | S_sm -> "sm"
    | S_md -> "md"
    | S_lg -> "lg"

  let to_class = function
    | None -> "text-shadow-none"
    | Shape shape -> "text-shadow-" ^ shape_to_string shape
    | Shape_opacity (shape, opacity) ->
        "text-shadow-" ^ shape_to_string shape ^ "/" ^ Color.pp_opacity opacity
    | Color (c, shade) ->
        "text-shadow-" ^ Color.color_to_string c
        ^ if Color.is_shadeless c then "" else "-" ^ string_of_int shade
    | Color_opacity (c, shade, opacity) ->
        "text-shadow-" ^ Color.color_to_string c
        ^ (if Color.is_shadeless c then "" else "-" ^ string_of_int shade)
        ^ "/" ^ Color.pp_opacity opacity
    | Current -> "text-shadow-current"
    | Current_opacity opacity ->
        "text-shadow-current/" ^ Color.pp_opacity opacity
    | Inherit -> "text-shadow-inherit"
    | Transparent -> "text-shadow-transparent"
    | Transparent_opacity opacity ->
        "text-shadow-transparent/" ^ Color.pp_opacity opacity
    | Bracket_hex hex -> "text-shadow-[#" ^ hex ^ "]"
    | Bracket_hex_opacity (hex, opacity) ->
        "text-shadow-[#" ^ hex ^ "]/" ^ Color.pp_opacity opacity
    | Bracket_color (orig, _) -> "text-shadow-[" ^ orig ^ "]"
    | Bracket_color_opacity (orig, _, opacity) ->
        "text-shadow-[" ^ orig ^ "]/" ^ Color.pp_opacity opacity
    | Bracket_color_var var_expr -> "text-shadow-[color:" ^ var_expr ^ "]"
    | Bracket_cvar_opacity (var_expr, opacity) ->
        "text-shadow-[color:" ^ var_expr ^ "]/" ^ Color.pp_opacity opacity
    | Bracket_shadow var_expr -> "text-shadow-[shadow:" ^ var_expr ^ "]"
    | Bracket_var var_expr -> "text-shadow-[" ^ var_expr ^ "]"
    | Arbitrary arb -> "text-shadow-[" ^ arb ^ "]"
    | Arbitrary_opacity (arb, opacity) ->
        "text-shadow-[" ^ arb ^ "]/" ^ Color.pp_opacity opacity

  (* ============ Suborder ============ *)

  (* Utilities that set --tw-text-shadow-alpha AND text-shadow come before all
     other utilities. Within that group, relative color @supports (lab) come
     first, then color-mix @supports, then no-@supports. *)
  let suborder = function
    | Arbitrary_opacity (arb, _) -> (
        match parse_arbitrary_shadow arb with
        | Some (_, _, _, Var_ref _) -> 6 (* @supports lab *)
        | Some (_, _, _, No_color) -> 7 (* @supports color-mix *)
        | Some (_, _, _, (Hex _ | Css_color _)) -> 8 (* no @supports *)
        | Stdlib.Option.None -> 9)
    | Shape_opacity _ -> 8 (* no @supports *)
    | _ -> 0

  let examples = [ Shape S_sm; Current ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let text_shadow_none = utility None
let text_shadow_2xs = utility (Shape S_2xs)
let text_shadow_xs = utility (Shape S_xs)
let text_shadow_sm = utility (Shape S_sm)
let text_shadow_md = utility (Shape S_md)
let text_shadow_lg = utility (Shape S_lg)
let text_shadow_arbitrary arb = utility (Arbitrary arb)
