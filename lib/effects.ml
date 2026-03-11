(** Visual effects utilities for shadows, opacity, and filters. *)

let current_scheme : Scheme.t ref = ref Scheme.default
let set_scheme scheme = current_scheme := scheme

module Handler = struct
  open Style

  let pp_float = Pp.float

  open Css

  type shadow_shape = Sh_sm | Sh_default | Sh_md | Sh_lg | Sh_xl | Sh_2xl

  (* Color in an arbitrary shadow value *)
  type arb_color = Arb_hex of string | Arb_var of string | Arb_none

  type t =
    (* Shadows — shape utilities *)
    | Shadow_none
    | Shadow_sm
    | Shadow
    | Shadow_md
    | Shadow_lg
    | Shadow_xl
    | Shadow_2xl
    | Shadow_inner
    | Shadow_arbitrary of string
    | Shadow_arbitrary_opacity of string * Color.opacity_modifier
    | Shadow_shape_opacity of shadow_shape * Color.opacity_modifier
    (* Shadows — color utilities *)
    | Shadow_color of Color.color * int
    | Shadow_color_opacity of Color.color * int * Color.opacity_modifier
    | Shadow_current
    | Shadow_current_opacity of Color.opacity_modifier
    | Shadow_inherit
    | Shadow_transparent
    | Shadow_bracket_hex of string
    | Shadow_bracket_hex_opacity of string * Color.opacity_modifier
    | Shadow_bracket_color_var of string
    | Shadow_bracket_color_var_opacity of string * Color.opacity_modifier
    | Shadow_bracket_shadow of string (* shadow-[shadow:...] type hint *)
    | Shadow_bracket_var of string
    (* Inset shadows *)
    | Inset_shadow_none
    | Inset_shadow_sm
    | Inset_shadow
    | Inset_shadow_md
    | Inset_shadow_lg
    | Inset_shadow_xl
    | Inset_shadow_2xl
    | Inset_shadow_arbitrary of string (* For inset-shadow-[12px_12px_#color] *)
    (* Opacity *)
    | Opacity of int
    | Opacity_decimal of float (* For values like opacity-2.5 *)
    | Opacity_arbitrary of float
    | Opacity_var of string (* opacity-[var(--value)] *)
    (* Rings *)
    | Ring_none
    | Ring_xs
    | Ring_sm
    | Ring_md
    | Ring_lg
    | Ring_xl
    | Ring_inset
    | Ring_color of Color.color * int
    | Ring_color_opacity of Color.color * int * Color.opacity_modifier
    | Ring_offset_width of int
    | Ring_offset_bracket_length of string
    | Ring_offset_color of Color.color * int
    | Ring_offset_color_opacity of Color.color * int * Color.opacity_modifier
    | Ring_offset_transparent
    | Ring_offset_current
    | Ring_offset_current_opacity of Color.opacity_modifier
    | Ring_offset_inherit
    | Ring_offset_bracket_hex of string
    | Ring_offset_bracket_hex_opacity of string * Color.opacity_modifier
    | Ring_offset_bracket_color_var of string
    | Ring_offset_bracket_color_var_opacity of string * Color.opacity_modifier
    | Ring_offset_bracket_var of string
    | Ring_offset_bracket_var_opacity of string * Color.opacity_modifier
    | Inset_ring_color of Color.color * int
    | Inset_ring_color_opacity of Color.color * int * Color.opacity_modifier
    | Inset_ring_transparent
    | Inset_ring_current
    | Inset_ring_current_opacity of Color.opacity_modifier
    | Inset_ring_inherit
    | Inset_ring_bracket_hex of string
    | Inset_ring_bracket_hex_opacity of string * Color.opacity_modifier
    | Inset_ring_bracket_color_var of string
    | Inset_ring_bracket_color_var_opacity of string * Color.opacity_modifier
    | Inset_ring_bracket_var of string
    | Inset_ring_bracket_var_opacity of string * Color.opacity_modifier
    | Inset_ring_default
    | Inset_ring_width of int
    | Inset_ring_bracket_length of string
    | Ring_transparent
    | Ring_current
    | Ring_current_opacity of Color.opacity_modifier
    | Ring_inherit
    | Ring_bracket_hex of string
    | Ring_bracket_hex_opacity of string * Color.opacity_modifier
    | Ring_bracket_color_var of string
    | Ring_bracket_color_var_opacity of string * Color.opacity_modifier
    | Ring_bracket_var of string
    | Ring_bracket_var_opacity of string * Color.opacity_modifier
    | Ring_bracket_length of string
    (* Mix blend modes *)
    | Mix_blend_normal
    | Mix_blend_multiply
    | Mix_blend_screen
    | Mix_blend_overlay
    | Mix_blend_darken
    | Mix_blend_lighten
    | Mix_blend_color_dodge
    | Mix_blend_color_burn
    | Mix_blend_hard_light
    | Mix_blend_soft_light
    | Mix_blend_difference
    | Mix_blend_exclusion
    | Mix_blend_hue
    | Mix_blend_saturation
    | Mix_blend_color
    | Mix_blend_luminosity
    | Mix_blend_plus_darker
    | Mix_blend_plus_lighter
    (* Background blend modes *)
    | Bg_blend_normal
    | Bg_blend_multiply
    | Bg_blend_screen
    | Bg_blend_overlay
    | Bg_blend_darken
    | Bg_blend_lighten
    | Bg_blend_color_dodge
    | Bg_blend_color_burn
    | Bg_blend_hard_light
    | Bg_blend_soft_light
    | Bg_blend_difference
    | Bg_blend_exclusion
    | Bg_blend_hue
    | Bg_blend_saturation
    | Bg_blend_color
    | Bg_blend_luminosity

  type Utility.base += Self of t

  let name = "effects"
  let priority = 25

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
    Var.property_default Css.Float ~initial:100.0 ~property_order:9
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
    Var.property_default Css.Float ~initial:100.0 ~property_order:12
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
    Var.channel ~needs_property:true ~property_order:17 ~family:`Ring Css.String
      "tw-ring-inset"

  let ring_offset_width_var =
    Var.property_default Css.Length
      ~initial:(Zero : Css.length)
      ~initial_css:"0px" ~property_order:18 ~family:`Ring "tw-ring-offset-width"

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
    |> List.filter_map (fun x -> x)
    |> Css.concat

  let shorten_hex = Color.shorten_hex_str

  let shadow_alpha_decl percent =
    Css.custom_property ~layer:"utilities" "--tw-shadow-alpha"
      (pp_float percent ^ "%")

  let color_mix_supports decls =
    Css.supports ~condition:Color.color_mix_supports_condition
      [ Css.rule ~selector:(Css.Selector.class_ "_") decls ]

  let relative_color_supports =
    Css.Supports.Property ("color", "lab(from red l a b)")

  let make_color_var vn : Css.color = Css.Var (Css.var_ref vn)

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

  let shadow_shape_data (shape : shadow_shape) :
      (Css.length * Css.length * Css.length option * Css.length option * string)
      list =
    match shape with
    | Sh_sm ->
        [
          (Zero, Px 1., Some (Px 3.), Some Zero, "#0000001a");
          (Zero, Px 1., Some (Px 2.), Some (Px (-1.)), "#0000001a");
        ]
    | Sh_default ->
        [
          (Zero, Px 1., Some (Px 3.), Some Zero, "#0000001a");
          (Zero, Px 1., Some (Px 2.), Some (Px (-1.)), "#0000001a");
        ]
    | Sh_md ->
        [
          (Zero, Px 4., Some (Px 6.), Some (Px (-1.)), "#0000001a");
          (Zero, Px 2., Some (Px 4.), Some (Px (-2.)), "#0000001a");
        ]
    | Sh_lg ->
        [
          (Zero, Px 10., Some (Px 15.), Some (Px (-3.)), "#0000001a");
          (Zero, Px 4., Some (Px 6.), Some (Px (-4.)), "#0000001a");
        ]
    | Sh_xl ->
        [
          (Zero, Px 20., Some (Px 25.), Some (Px (-5.)), "#0000001a");
          (Zero, Px 8., Some (Px 10.), Some (Px (-6.)), "#0000001a");
        ]
    | Sh_2xl -> [ (Zero, Px 25., Some (Px 50.), Some (Px (-12.)), "#0000001a") ]

  let shape_shadow_value shape =
    let data = shadow_shape_data shape in
    let shadow_list =
      List.map
        (fun (h_offset, v_offset, blur, spread, fallback_hex) ->
          let color_ref =
            Var.reference_with_fallback shadow_color_var (Css.hex fallback_hex)
          in
          Css.shadow ~h_offset ~v_offset ?blur ?spread ~color:(Var color_ref) ())
        data
    in
    match shadow_list with [ s ] -> s | _ -> List shadow_list

  let shape_shadow_opacity_value shape opacity =
    let data = shadow_shape_data shape in
    let percent = Color.opacity_to_percent opacity in
    let alpha = percent /. 100.0 in
    let shadow_list =
      List.map
        (fun (h_offset, v_offset, blur, spread, fallback_hex) ->
          let base_hex =
            if String.length fallback_hex = 9 then String.sub fallback_hex 0 7
            else fallback_hex
          in
          let oklab_fallback = Color.hex_to_oklab_alpha base_hex alpha in
          let color_ref =
            Var.reference_with_fallback shadow_color_var oklab_fallback
          in
          Css.shadow ~h_offset ~v_offset ?blur ?spread ~color:(Var color_ref) ())
        data
    in
    match shadow_list with [ s ] -> s | _ -> List shadow_list

  let shadow_none =
    let shadow_value =
      Css.shadow ~h_offset:Zero ~v_offset:Zero ~color:(Css.hex "#0000") ()
    in
    let d_shadow, v_shadow = Var.binding shadow_var shadow_value in
    style ~property_rules:shadow_property_rules
      [ d_shadow; box_shadow_composition v_shadow ]

  let shadow_shape_style shape =
    let d_shadow, v_shadow =
      Var.binding shadow_var (shape_shadow_value shape)
    in
    style ~property_rules:shadow_property_rules
      [ d_shadow; box_shadow_composition v_shadow ]

  let shadow_shape_opacity_style shape opacity =
    let percent = Color.opacity_to_percent opacity in
    let d_shadow, v_shadow =
      Var.binding shadow_var (shape_shadow_opacity_value shape opacity)
    in
    style ~property_rules:shadow_property_rules
      [ shadow_alpha_decl percent; d_shadow; box_shadow_composition v_shadow ]

  let shadow_sm = shadow_shape_style Sh_sm
  let shadow = shadow_shape_style Sh_default
  let shadow_md = shadow_shape_style Sh_md
  let shadow_lg = shadow_shape_style Sh_lg
  let shadow_xl = shadow_shape_style Sh_xl
  let shadow_2xl = shadow_shape_style Sh_2xl

  let shadow_inner =
    (* Define inset shadow variable *)
    let inset_shadow_value =
      Css.shadow ~inset:true ~h_offset:(Px 0.) ~v_offset:(Px 2.) ~blur:(Px 4.)
        ()
    in
    (* Create the box-shadow declaration with the shadow value *)
    let box_shadow_decl = Css.box_shadow inset_shadow_value in

    let d_inset, _ = Var.binding inset_shadow_var inset_shadow_value in
    let d_inset_ring, _ =
      Var.binding inset_ring_shadow_var
        (Css.shadow ~h_offset:Zero ~v_offset:Zero ~color:(Css.hex "#0000") ())
    in
    let d_ring_offset, _ =
      Var.binding ring_offset_shadow_var
        (Css.shadow ~h_offset:Zero ~v_offset:Zero ~color:(Css.hex "#0000") ())
    in
    let d_ring, _ =
      Var.binding ring_shadow_var
        (Css.shadow ~h_offset:Zero ~v_offset:Zero ~color:(Css.hex "#0000") ())
    in
    style
      (d_inset :: d_inset_ring :: d_ring_offset :: d_ring :: [ box_shadow_decl ])

  (* Parse arbitrary shadow value like "12px_12px_#0088cc" *)
  let parse_arbitrary_shadow (s : string) :
      (Css.length * Css.length * Css.length option * arb_color) option =
    let normalized = String.map (fun c -> if c = '_' then ' ' else c) s in
    let parts = String.split_on_char ' ' normalized in
    let parse_length str : Css.length option =
      let len = String.length str in
      if len >= 1 then (
        let num_end = ref 0 in
        while
          !num_end < len
          && (str.[!num_end] = '-'
             || str.[!num_end] = '.'
             || (str.[!num_end] >= '0' && str.[!num_end] <= '9'))
        do
          incr num_end
        done;
        let num_str = String.sub str 0 !num_end in
        let unit_str = String.sub str !num_end (len - !num_end) in
        match float_of_string_opt num_str with
        | Some n -> (
            match unit_str with
            | "px" -> Some (Px n)
            | "rem" -> Some (Rem n)
            | "em" -> Some (Em n)
            | "" when n = 0.0 -> Some Zero
            | _ -> None)
        | None -> None)
      else None
    in
    let rec find_color_and_lengths acc (parts : string list) :
        string list * arb_color =
      match parts with
      | [] -> (List.rev acc, Arb_none)
      | x :: _rest when String.length x > 0 && x.[0] = '#' ->
          (List.rev acc, Arb_hex x)
      | x :: _rest when String.length x > 4 && String.sub x 0 4 = "var(" ->
          (List.rev acc, Arb_var x)
      | x :: rest -> find_color_and_lengths (x :: acc) rest
    in
    let length_strs, color = find_color_and_lengths [] parts in
    let lengths = List.filter_map parse_length length_strs in
    match lengths with
    | [ h; v ] -> Some (h, v, None, color)
    | [ h; v; blur ] -> Some (h, v, Some blur, color)
    | [ h; v; blur; _spread ] -> Some (h, v, Some blur, color)
    | _ -> None

  let shadow_arbitrary (arb : string) =
    match parse_arbitrary_shadow arb with
    | Some (h_offset, v_offset, blur, color) ->
        let fallback_color : Css.color =
          match color with
          | Arb_hex c -> Css.hex (shorten_hex c)
          | Arb_var v -> make_color_var (Parse.extract_var_name v)
          | Arb_none -> Css.Current
        in
        let color_ref =
          Var.reference_with_fallback shadow_color_var fallback_color
        in
        let shadow_value =
          Css.shadow ~h_offset ~v_offset ?blur ~color:(Var color_ref) ()
        in
        let d_shadow, v_shadow = Var.binding shadow_var shadow_value in
        style ~property_rules:shadow_property_rules
          [ d_shadow; box_shadow_composition v_shadow ]
    | None -> shadow_none

  let shadow_arbitrary_opacity (arb : string) opacity =
    match parse_arbitrary_shadow arb with
    | Some (h_offset, v_offset, blur, color) -> (
        let percent = Color.opacity_to_percent opacity in
        let alpha = percent /. 100.0 in
        let alpha_d = shadow_alpha_decl percent in
        let base_fallback : Css.color =
          match color with
          | Arb_hex c -> Color.hex_to_oklab_alpha c alpha
          | Arb_var v -> make_color_var (Parse.extract_var_name v)
          | Arb_none -> Css.Current
        in
        let base_color_ref =
          Var.reference_with_fallback shadow_color_var base_fallback
        in
        let base_shadow =
          Css.shadow ~h_offset ~v_offset ?blur ~color:(Var base_color_ref) ()
        in
        let d_shadow, v_shadow = Var.binding shadow_var base_shadow in
        let supports_rules =
          match color with
          | Arb_hex _ -> []
          | Arb_var v ->
              let vn = Parse.extract_var_name v in
              let raw_fb =
                Printf.sprintf "oklab(from var(--%s) l a b / %s%%)" vn
                  (pp_float percent)
              in
              let enhanced_ref =
                Css.var_ref ~fallback:(Raw_fallback raw_fb) "tw-shadow-color"
              in
              let enhanced_shadow =
                Css.shadow ~h_offset ~v_offset ?blur ~color:(Var enhanced_ref)
                  ()
              in
              let d_enhanced, _ = Var.binding shadow_var enhanced_shadow in
              let supports_block =
                Css.supports ~condition:relative_color_supports
                  [
                    Css.rule ~selector:(Css.Selector.class_ "_") [ d_enhanced ];
                  ]
              in
              [ supports_block ]
          | Arb_none ->
              let color_mix_fallback =
                Css.color_mix ~in_space:Oklab Css.Current Css.Transparent
                  ~percent1:percent
              in
              let enhanced_ref =
                Var.reference_with_fallback shadow_color_var color_mix_fallback
              in
              let enhanced_shadow =
                Css.shadow ~h_offset ~v_offset ?blur ~color:(Var enhanced_ref)
                  ()
              in
              let d_enhanced, _ = Var.binding shadow_var enhanced_shadow in
              let supports_block = color_mix_supports [ d_enhanced ] in
              [ supports_block ]
        in
        match supports_rules with
        | [] ->
            (* No @supports — keep all declarations in one block *)
            style ~property_rules:shadow_property_rules
              [ alpha_d; d_shadow; box_shadow_composition v_shadow ]
        | _ ->
            (* Has @supports — split: alpha+shadow first (as regular rule), then
               @supports, then box-shadow in props (base rule).
               extract_style_with_rules puts ordered_entries before base_rule
               when has_regular_rules=true, giving the correct order:
               [alpha+shadow] -> [@supports] -> [box-shadow] *)
            let alpha_shadow_rule =
              Css.rule ~selector:(Css.Selector.class_ "_") [ alpha_d; d_shadow ]
            in
            style
              ~rules:(Stdlib.Option.Some (alpha_shadow_rule :: supports_rules))
              ~property_rules:shadow_property_rules
              [ box_shadow_composition v_shadow ])
    | Stdlib.Option.None -> shadow_none

  (* ============ Shadow color utilities ============ *)

  let shadow_color_hex c shade =
    let color_name = Color.scheme_color_name c shade in
    match Scheme.hex_color (Color.scheme ()) color_name with
    | Stdlib.Option.Some h -> h
    | Stdlib.Option.None -> (
        match Var.theme_value ("box-shadow-color-" ^ color_name) with
        | Stdlib.Option.Some h -> h
        | Stdlib.Option.None ->
            let oklch = Color.to_oklch c shade in
            let rgb = Color.oklch_to_rgb oklch in
            Color.rgb_to_hex rgb)

  let set_shadow_color c shade =
    let hex_value = shadow_color_hex c shade in
    let base_decl, _ = Var.binding shadow_color_var (Css.hex hex_value) in
    let theme_color_var =
      Color.property_color_var ~property_prefix:"box-shadow-color" c shade
    in
    let theme_decl, color_ref =
      Var.binding theme_color_var (Css.hex hex_value)
    in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:"tw-shadow-alpha"
        (Css.Var color_ref) Css.Transparent
    in
    let enhanced_decl, _ = Var.binding shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ theme_decl; enhanced_decl ] in
    style ~rules:(Some [ supports_block ]) ~property_rules:shadow_property_rules
      [ base_decl ]

  let set_shadow_color_opacity c shade opacity =
    let percent = Color.opacity_to_percent opacity in
    let hex_value = shadow_color_hex c shade in
    let hex_with_alpha = Color.hex_with_alpha hex_value percent in
    let base_decl, _ = Var.binding shadow_color_var (Css.hex hex_with_alpha) in
    let theme_color_var =
      Color.property_color_var ~property_prefix:"box-shadow-color" c shade
    in
    let theme_decl, color_ref =
      Var.binding theme_color_var (Css.hex hex_value)
    in
    let inner_mix =
      Css.color_mix ~in_space:Oklab (Css.Var color_ref) Css.Transparent
        ~percent1:percent
    in
    let outer_mix =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:"tw-shadow-alpha"
        inner_mix Css.Transparent
    in
    let enhanced_decl, _ = Var.binding shadow_color_var outer_mix in
    let supports_block = color_mix_supports [ theme_decl; enhanced_decl ] in
    style ~rules:(Some [ supports_block ]) ~property_rules:shadow_property_rules
      [ base_decl ]

  let set_shadow_current () =
    let base_decl, _ = Var.binding shadow_color_var Css.Current in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:"tw-shadow-alpha"
        Css.Current Css.Transparent
    in
    let enhanced_decl, _ = Var.binding shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ]) ~property_rules:shadow_property_rules
      [ base_decl ]

  let set_shadow_current_opacity opacity =
    let percent = Color.opacity_to_percent opacity in
    let base_decl, _ = Var.binding shadow_color_var Css.Current in
    let inner_mix =
      Css.color_mix ~in_space:Oklab Css.Current Css.Transparent
        ~percent1:percent
    in
    let outer_mix =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:"tw-shadow-alpha"
        inner_mix Css.Transparent
    in
    let enhanced_decl, _ = Var.binding shadow_color_var outer_mix in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ]) ~property_rules:shadow_property_rules
      [ base_decl ]

  let set_shadow_transparent () =
    let base_decl, _ = Var.binding shadow_color_var Css.Transparent in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:"tw-shadow-alpha"
        Css.Transparent Css.Transparent
    in
    let enhanced_decl, _ = Var.binding shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ]) ~property_rules:shadow_property_rules
      [ base_decl ]

  let set_shadow_inherit () =
    let base_decl, _ = Var.binding shadow_color_var Css.Inherit in
    style ~property_rules:shadow_property_rules [ base_decl ]

  let set_shadow_bracket_hex hex =
    let short = shorten_hex ("#" ^ hex) in
    let base_decl, _ = Var.binding shadow_color_var (Css.hex short) in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:"tw-shadow-alpha"
        (Css.hex short) Css.Transparent
    in
    let enhanced_decl, _ = Var.binding shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ]) ~property_rules:shadow_property_rules
      [ base_decl ]

  let set_shadow_bracket_hex_opacity hex opacity =
    let percent = Color.opacity_to_percent opacity in
    let hex_with_alpha = Color.hex_with_alpha hex percent in
    let base_decl, _ = Var.binding shadow_color_var (Css.hex hex_with_alpha) in
    let alpha = percent /. 100.0 in
    let oklab_color = Color.hex_to_oklab_alpha hex alpha in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:"tw-shadow-alpha"
        oklab_color Css.Transparent
    in
    let enhanced_decl, _ = Var.binding shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ]) ~property_rules:shadow_property_rules
      [ base_decl ]

  let set_shadow_bracket_color_var var_expr =
    let var_name = Parse.extract_var_name var_expr in
    let var_color = make_color_var var_name in
    let base_decl, _ = Var.binding shadow_color_var var_color in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:"tw-shadow-alpha"
        var_color Css.Transparent
    in
    let enhanced_decl, _ = Var.binding shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ]) ~property_rules:shadow_property_rules
      [ base_decl ]

  let set_shadow_bracket_color_var_opacity var_expr opacity =
    let percent = Color.opacity_to_percent opacity in
    let var_name = Parse.extract_var_name var_expr in
    let var_color = make_color_var var_name in
    let base_decl, _ = Var.binding shadow_color_var var_color in
    let inner_mix =
      Css.color_mix ~in_space:Oklab var_color Css.Transparent ~percent1:percent
    in
    let outer_mix =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:"tw-shadow-alpha"
        inner_mix Css.Transparent
    in
    let enhanced_decl, _ = Var.binding shadow_color_var outer_mix in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ]) ~property_rules:shadow_property_rules
      [ base_decl ]

  let shadow_raw_var v =
    let var_name = Parse.extract_var_name v in
    let shadow_value : Css.shadow = Css.Var (Css.var_ref var_name) in
    let d_shadow, v_shadow = Var.binding shadow_var shadow_value in
    style ~property_rules:shadow_property_rules
      [ d_shadow; box_shadow_composition v_shadow ]

  (* Inset shadow utilities - sets --tw-inset-shadow and composites
     box-shadow *)
  type inset_shadow_size = [ `None | `Sm | `Default | `Md | `Lg | `Xl | `Xxl ]

  let inset_shadow_internal (size : inset_shadow_size) =
    (* Inset shadow sizes matching Tailwind v4: inset-shadow-none: 0 0 #0000
       inset-shadow-sm: inset 0 1px 1px color inset-shadow: inset 0 2px 4px
       color inset-shadow-md: inset 0 4px 6px color inset-shadow-lg: inset 0 4px
       8px color inset-shadow-xl: inset 0 6px 10px color inset-shadow-2xl: inset
       0 8px 25px color *)
    let color_ref =
      Var.reference_with_fallback inset_shadow_color_var (Css.hex "#0000000d")
    in
    let inset_value =
      match size with
      | `None ->
          Css.shadow ~h_offset:Zero ~v_offset:Zero ~color:(Css.hex "#0000") ()
      | `Sm ->
          Css.shadow ~inset:true ~h_offset:Zero ~v_offset:(Px 1.) ~blur:(Px 1.)
            ~color:(Var color_ref) ()
      | `Default ->
          Css.shadow ~inset:true ~h_offset:Zero ~v_offset:(Px 2.) ~blur:(Px 4.)
            ~color:(Var color_ref) ()
      | `Md ->
          Css.shadow ~inset:true ~h_offset:Zero ~v_offset:(Px 4.) ~blur:(Px 6.)
            ~color:(Var color_ref) ()
      | `Lg ->
          Css.shadow ~inset:true ~h_offset:Zero ~v_offset:(Px 4.) ~blur:(Px 8.)
            ~color:(Var color_ref) ()
      | `Xl ->
          Css.shadow ~inset:true ~h_offset:Zero ~v_offset:(Px 6.) ~blur:(Px 10.)
            ~color:(Var color_ref) ()
      | `Xxl ->
          Css.shadow ~inset:true ~h_offset:Zero ~v_offset:(Px 8.) ~blur:(Px 25.)
            ~color:(Var color_ref) ()
    in

    (* Set --tw-inset-shadow variable *)
    let d_inset_shadow, v_inset_shadow =
      Var.binding inset_shadow_var inset_value
    in

    (* Reference other shadow variables through @property defaults *)
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

    (* Collect property rules *)
    let property_rules =
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
      |> List.filter_map (fun x -> x)
    in

    style
      ~property_rules:(Css.concat property_rules)
      [ d_inset_shadow; Css.box_shadows box_shadow_vars ]

  let inset_shadow_none = inset_shadow_internal `None
  let inset_shadow_sm = inset_shadow_internal `Sm
  let inset_shadow = inset_shadow_internal `Default
  let inset_shadow_md = inset_shadow_internal `Md
  let inset_shadow_lg = inset_shadow_internal `Lg
  let inset_shadow_xl = inset_shadow_internal `Xl
  let inset_shadow_2xl = inset_shadow_internal `Xxl

  let inset_shadow_arbitrary (arb : string) =
    match parse_arbitrary_shadow arb with
    | Stdlib.Option.Some (h_offset, v_offset, blur, color) ->
        let fallback_color : Css.color =
          match color with
          | Arb_hex c -> Css.hex (shorten_hex c)
          | Arb_var v -> make_color_var (Parse.extract_var_name v)
          | Arb_none -> Css.Current
        in
        let color_ref =
          Var.reference_with_fallback inset_shadow_color_var fallback_color
        in
        let inset_value =
          Css.shadow ~inset:true ~h_offset ~v_offset ?blur
            ~color:(Var color_ref) ()
        in
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
        style [ d_inset_shadow; Css.box_shadows box_shadow_vars ]
    | Stdlib.Option.None ->
        (* Fallback: just output transparent inset shadow *)
        inset_shadow_none

  let ring_internal width_px =
    (* Build ring shadow using typed constructors: var(--tw-ring-inset,) 0 0 0
       calc(Xpx + var(--tw-ring-offset-width)) var(--tw-ring-color,
       currentcolor) *)
    let offset_width_ref = Var.reference ring_offset_width_var in
    let spread : Css.length =
      Calc (Expr (Val (Px (float_of_int width_px)), Add, Var offset_width_ref))
    in
    let color : Css.color =
      Var (Css.var_ref ~fallback:(Fallback Css.Current) "tw-ring-color")
    in
    let ring_shadow_value =
      Css.shadow ~inset_var:"tw-ring-inset" ~h_offset:Zero ~v_offset:Zero
        ~blur:Zero ~spread ~color ()
    in
    let d_ring, _ = Var.binding ring_shadow_var ring_shadow_value in

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

    (* Collect all property rules needed for ring *)
    let property_rules =
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
      |> List.filter_map (fun x -> x)
    in

    style
      ~property_rules:(Css.concat property_rules)
      [ d_ring; Css.box_shadows box_shadow_vars ]

  (* Tailwind v4 ring widths: ring-0=0px, ring-1=1px, ring-2=2px, ring-4=4px,
     ring-8=8px. Bare ring uses --default-ring-width (default 1px). *)
  let ring_none = ring_internal 0
  let ring_xs = ring_internal 1
  let ring_sm = ring_internal 2
  let ring_lg = ring_internal 4
  let ring_xl = ring_internal 8

  (** Bare [ring] — uses scheme's [default_ring_width] (configurable via
      Tailwind's [@theme \{ --default-ring-width \}], default 1px). *)
  let ring_default () = ring_internal !current_scheme.default_ring_width

  let inset_ring_internal width_px =
    let spread : Css.length = Px (float_of_int width_px) in
    let color : Css.color =
      Var (Css.var_ref ~fallback:(Fallback Css.Current) "tw-inset-ring-color")
    in
    let shadow_value =
      Css.shadow ~inset:true ~h_offset:Zero ~v_offset:Zero ~blur:Zero ~spread
        ~color ()
    in
    let d, _ = Var.binding inset_ring_shadow_var shadow_value in
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
    let property_rules =
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
      |> List.filter_map (fun x -> x)
    in
    style
      ~property_rules:(Css.concat property_rules)
      [ d; Css.box_shadows box_shadow_vars ]

  let inset_ring_default () = inset_ring_internal 1

  let ring_inset =
    let decl, _var_ref = Var.binding ring_inset_var "inset" in
    style [ decl ]

  let ring_color color shade =
    let cvar =
      Color.property_color_var ~property_prefix:"ring-color" color shade
    in
    let color_value =
      Color.property_color_value ~property_prefix:"ring-color" color shade
    in
    let color_decl, color_ref = Var.binding cvar color_value in
    let d, _ = Var.binding ring_color_var (Css.Var color_ref) in
    style [ color_decl; d ]

  let ring_color_with_opacity color shade opacity =
    let percent = Color.opacity_to_percent opacity in
    match Color.hex_alpha_color color shade opacity with
    | Some hex_alpha ->
        let fallback, _ = Var.binding ring_color_var (Css.hex hex_alpha) in
        let cvar =
          Color.property_color_var ~property_prefix:"ring-color" color shade
        in
        let color_value =
          Color.property_color_value ~property_prefix:"ring-color" color shade
        in
        let theme_decl, color_ref = Var.binding cvar color_value in
        let oklab_decl, _ =
          Var.binding ring_color_var
            (Css.color_mix ~in_space:Oklab (Css.Var color_ref) Css.Transparent
               ~percent1:percent)
        in
        let supports_block =
          Css.supports ~condition:Color.color_mix_supports_condition
            [
              Css.rule ~selector:(Css.Selector.class_ "_")
                [ theme_decl; oklab_decl ];
            ]
        in
        Style.style ~rules:(Some [ supports_block ]) [ fallback ]
    | None -> ring_color color shade

  let ring_offset_width n =
    (* Sets --tw-ring-offset-width and --tw-ring-offset-shadow Format:
       var(--tw-ring-inset,) 0 0 0 var(--tw-ring-offset-width)
       var(--tw-ring-offset-color) *)
    let width_value : Css.length = Px (float_of_int n) in
    let d_width, width_ref = Var.binding ring_offset_width_var width_value in
    let color_ref = Var.reference ring_offset_color_var in
    let shadow_value =
      Css.shadow ~inset:false ~inset_var:"tw-ring-inset" ~h_offset:Zero
        ~v_offset:Zero ~blur:Zero ~spread:(Var width_ref) ~color:(Var color_ref)
        ()
    in
    let d_shadow, _ = Var.binding ring_offset_shadow_var shadow_value in
    style [ d_width; d_shadow ]

  let ring_offset_color color shade =
    (* Sets --tw-ring-offset-color to reference theme color variable. Uses
       property-scoped variable: --ring-offset-color-blue-500 *)
    let color_theme_var =
      Color.property_color_var ~property_prefix:"ring-offset-color" color shade
    in
    let color_value =
      Color.property_color_value ~property_prefix:"ring-offset-color" color
        shade
    in
    let color_decl, color_ref = Var.binding color_theme_var color_value in
    let d, _ = Var.binding ring_offset_color_var (Css.Var color_ref) in
    style [ color_decl; d ]

  let parse_bracket_width inner : Css.length =
    if String.length inner > 6 && String.sub inner 0 7 = "length:" then
      let v = String.sub inner 7 (String.length inner - 7) in
      let bare = Parse.extract_var_name v in
      Var (Css.var_ref bare)
    else if
      String.length inner > 2
      && String.sub inner (String.length inner - 2) 2 = "px"
    then
      let num_str = String.sub inner 0 (String.length inner - 2) in
      match float_of_string_opt num_str with Some f -> Px f | None -> Px 0.
    else if
      String.length inner > 3
      && String.sub inner (String.length inner - 3) 3 = "rem"
    then
      let num_str = String.sub inner 0 (String.length inner - 3) in
      match float_of_string_opt num_str with Some f -> Rem f | None -> Rem 0.
    else match float_of_string_opt inner with Some f -> Px f | None -> Px 0.

  let parse_bracket_width_opt inner =
    if String.length inner > 6 && String.sub inner 0 7 = "length:" then
      Some (parse_bracket_width inner)
    else if
      String.length inner > 2
      && String.sub inner (String.length inner - 2) 2 = "px"
    then
      let num_str = String.sub inner 0 (String.length inner - 2) in
      match float_of_string_opt num_str with
      | Some _ -> Some (parse_bracket_width inner)
      | None -> None
    else if
      String.length inner > 3
      && String.sub inner (String.length inner - 3) 3 = "rem"
    then
      let num_str = String.sub inner 0 (String.length inner - 3) in
      match float_of_string_opt num_str with
      | Some _ -> Some (parse_bracket_width inner)
      | None -> None
    else
      match float_of_string_opt inner with
      | Some _ -> Some (parse_bracket_width inner)
      | None -> None

  (* Ring-offset color utilities *)
  let ring_offset_color_with_opacity color shade opacity =
    let percent = Color.opacity_to_percent opacity in
    match Color.hex_alpha_color color shade opacity with
    | Some hex_alpha ->
        let fallback, _ =
          Var.binding ring_offset_color_var (Css.hex hex_alpha)
        in
        let cvar =
          Color.property_color_var ~property_prefix:"ring-offset-color" color
            shade
        in
        let color_value =
          Color.property_color_value ~property_prefix:"ring-offset-color" color
            shade
        in
        let theme_decl, color_ref = Var.binding cvar color_value in
        let oklab_decl, _ =
          Var.binding ring_offset_color_var
            (Css.color_mix ~in_space:Oklab (Css.Var color_ref) Css.Transparent
               ~percent1:percent)
        in
        let supports_block =
          Css.supports ~condition:Color.color_mix_supports_condition
            [
              Css.rule ~selector:(Css.Selector.class_ "_")
                [ theme_decl; oklab_decl ];
            ]
        in
        Style.style ~rules:(Some [ supports_block ]) [ fallback ]
    | None -> ring_offset_color color shade

  let ring_offset_transparent =
    let d, _ = Var.binding ring_offset_color_var Css.Transparent in
    style [ d ]

  let ring_offset_current =
    let d, _ = Var.binding ring_offset_color_var Css.Current in
    style [ d ]

  let ring_offset_current_with_opacity opacity =
    let percent = Color.opacity_to_percent opacity in
    let fallback, _ = Var.binding ring_offset_color_var Css.Current in
    let oklab_color =
      Css.color_mix ~in_space:Oklab Css.Current Css.Transparent
        ~percent1:percent
    in
    let oklab_decl, _ = Var.binding ring_offset_color_var oklab_color in
    let supports_block =
      Css.supports ~condition:Color.color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
    in
    Style.style ~rules:(Some [ supports_block ]) [ fallback ]

  let ring_offset_inherit =
    let d, _ = Var.binding ring_offset_color_var Css.Inherit in
    style [ d ]

  let ring_offset_bracket_length inner =
    let width_value = parse_bracket_width inner in
    let d_width, width_ref = Var.binding ring_offset_width_var width_value in
    let color_ref = Var.reference ring_offset_color_var in
    let shadow_value =
      Css.shadow ~inset:false ~inset_var:"tw-ring-inset" ~h_offset:Zero
        ~v_offset:Zero ~blur:Zero ~spread:(Var width_ref) ~color:(Var color_ref)
        ()
    in
    let d_shadow, _ = Var.binding ring_offset_shadow_var shadow_value in
    style [ d_width; d_shadow ]

  let ring_offset_bracket_hex inner =
    let c = Color.hex inner in
    let css_color = Color.to_css c 500 in
    let d, _ = Var.binding ring_offset_color_var css_color in
    style [ d ]

  let ring_offset_bracket_hex_with_opacity inner opacity =
    let percent = Color.opacity_to_percent opacity in
    let hex =
      if String.starts_with ~prefix:"#" inner then inner else "#" ^ inner
    in
    let alpha = percent /. 100.0 in
    let d, _ =
      Var.binding ring_offset_color_var (Color.hex_to_oklab_alpha hex alpha)
    in
    style [ d ]

  let ring_offset_bracket_color_var v =
    let bare_name = Parse.extract_var_name v in
    let d, _ =
      Var.binding ring_offset_color_var (Css.Var (Css.var_ref bare_name))
    in
    Style.style ~merge_key:("ring-offset-color:" ^ v) [ d ]

  let ring_offset_bracket_color_var_with_opacity v opacity =
    let percent = Color.opacity_to_percent opacity in
    let bare_name = Parse.extract_var_name v in
    let var_color : Css.color = Css.Var (Css.var_ref bare_name) in
    let fallback, _ = Var.binding ring_offset_color_var var_color in
    let oklab_color =
      Css.color_mix ~in_space:Oklab var_color Css.Transparent ~percent1:percent
    in
    let oklab_decl, _ = Var.binding ring_offset_color_var oklab_color in
    let supports_block =
      Css.supports ~condition:Color.color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
    in
    Style.style ~merge_key:("ring-offset-color:" ^ v)
      ~rules:(Some [ supports_block ]) [ fallback ]

  let ring_offset_bracket_var v =
    let bare_name = Parse.extract_var_name v in
    let d, _ =
      Var.binding ring_offset_color_var (Css.Var (Css.var_ref bare_name))
    in
    Style.style ~merge_key:("ring-offset-" ^ v) [ d ]

  let ring_offset_bracket_var_with_opacity v opacity =
    let percent = Color.opacity_to_percent opacity in
    let bare_name = Parse.extract_var_name v in
    let var_color : Css.color = Css.Var (Css.var_ref bare_name) in
    let fallback, _ = Var.binding ring_offset_color_var var_color in
    let oklab_color =
      Css.color_mix ~in_space:Oklab var_color Css.Transparent ~percent1:percent
    in
    let oklab_decl, _ = Var.binding ring_offset_color_var oklab_color in
    let supports_block =
      Css.supports ~condition:Color.color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
    in
    Style.style ~merge_key:("ring-offset-" ^ v) ~rules:(Some [ supports_block ])
      [ fallback ]

  (* Inset-ring utilities *)
  let inset_ring_color color shade =
    let cvar =
      Color.property_color_var ~property_prefix:"inset-ring-color" color shade
    in
    let color_value =
      Color.property_color_value ~property_prefix:"inset-ring-color" color shade
    in
    let color_decl, color_ref = Var.binding cvar color_value in
    let d, _ = Var.binding inset_ring_color_var (Css.Var color_ref) in
    style [ color_decl; d ]

  let inset_ring_color_with_opacity color shade opacity =
    let percent = Color.opacity_to_percent opacity in
    match Color.hex_alpha_color color shade opacity with
    | Some hex_alpha ->
        let fallback, _ =
          Var.binding inset_ring_color_var (Css.hex hex_alpha)
        in
        let cvar =
          Color.property_color_var ~property_prefix:"inset-ring-color" color
            shade
        in
        let color_value =
          Color.property_color_value ~property_prefix:"inset-ring-color" color
            shade
        in
        let theme_decl, color_ref = Var.binding cvar color_value in
        let oklab_decl, _ =
          Var.binding inset_ring_color_var
            (Css.color_mix ~in_space:Oklab (Css.Var color_ref) Css.Transparent
               ~percent1:percent)
        in
        let supports_block =
          Css.supports ~condition:Color.color_mix_supports_condition
            [
              Css.rule ~selector:(Css.Selector.class_ "_")
                [ theme_decl; oklab_decl ];
            ]
        in
        Style.style ~rules:(Some [ supports_block ]) [ fallback ]
    | None -> inset_ring_color color shade

  let inset_ring_transparent =
    let d, _ = Var.binding inset_ring_color_var Css.Transparent in
    style [ d ]

  let inset_ring_current =
    let d, _ = Var.binding inset_ring_color_var Css.Current in
    style [ d ]

  let inset_ring_current_with_opacity opacity =
    let percent = Color.opacity_to_percent opacity in
    let fallback, _ = Var.binding inset_ring_color_var Css.Current in
    let oklab_color =
      Css.color_mix ~in_space:Oklab Css.Current Css.Transparent
        ~percent1:percent
    in
    let oklab_decl, _ = Var.binding inset_ring_color_var oklab_color in
    let supports_block =
      Css.supports ~condition:Color.color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
    in
    Style.style ~rules:(Some [ supports_block ]) [ fallback ]

  let inset_ring_inherit =
    let d, _ = Var.binding inset_ring_color_var Css.Inherit in
    style [ d ]

  let inset_ring_bracket_hex inner =
    let c = Color.hex inner in
    let css_color = Color.to_css c 500 in
    let d, _ = Var.binding inset_ring_color_var css_color in
    style [ d ]

  let inset_ring_bracket_hex_with_opacity inner opacity =
    let percent = Color.opacity_to_percent opacity in
    let hex =
      if String.starts_with ~prefix:"#" inner then inner else "#" ^ inner
    in
    let alpha = percent /. 100.0 in
    let d, _ =
      Var.binding inset_ring_color_var (Color.hex_to_oklab_alpha hex alpha)
    in
    style [ d ]

  let inset_ring_bracket_color_var v =
    let bare_name = Parse.extract_var_name v in
    let d, _ =
      Var.binding inset_ring_color_var (Css.Var (Css.var_ref bare_name))
    in
    Style.style ~merge_key:("inset-ring-color:" ^ v) [ d ]

  let inset_ring_bracket_color_var_with_opacity v opacity =
    let percent = Color.opacity_to_percent opacity in
    let bare_name = Parse.extract_var_name v in
    let var_color : Css.color = Css.Var (Css.var_ref bare_name) in
    let fallback, _ = Var.binding inset_ring_color_var var_color in
    let oklab_color =
      Css.color_mix ~in_space:Oklab var_color Css.Transparent ~percent1:percent
    in
    let oklab_decl, _ = Var.binding inset_ring_color_var oklab_color in
    let supports_block =
      Css.supports ~condition:Color.color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
    in
    Style.style ~merge_key:("inset-ring-color:" ^ v)
      ~rules:(Some [ supports_block ]) [ fallback ]

  let inset_ring_bracket_var v =
    let bare_name = Parse.extract_var_name v in
    let d, _ =
      Var.binding inset_ring_color_var (Css.Var (Css.var_ref bare_name))
    in
    Style.style ~merge_key:("inset-ring-" ^ v) [ d ]

  let inset_ring_bracket_var_with_opacity v opacity =
    let percent = Color.opacity_to_percent opacity in
    let bare_name = Parse.extract_var_name v in
    let var_color : Css.color = Css.Var (Css.var_ref bare_name) in
    let fallback, _ = Var.binding inset_ring_color_var var_color in
    let oklab_color =
      Css.color_mix ~in_space:Oklab var_color Css.Transparent ~percent1:percent
    in
    let oklab_decl, _ = Var.binding inset_ring_color_var oklab_color in
    let supports_block =
      Css.supports ~condition:Color.color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
    in
    Style.style ~merge_key:("inset-ring-" ^ v) ~rules:(Some [ supports_block ])
      [ fallback ]

  (* Ring color utilities *)
  let ring_transparent =
    let d, _ = Var.binding ring_color_var Css.Transparent in
    style [ d ]

  let ring_current =
    let d, _ = Var.binding ring_color_var Css.Current in
    style [ d ]

  let ring_current_with_opacity opacity =
    let percent = Color.opacity_to_percent opacity in
    let fallback, _ = Var.binding ring_color_var Css.Current in
    let oklab_color =
      Css.color_mix ~in_space:Oklab Css.Current Css.Transparent
        ~percent1:percent
    in
    let oklab_decl, _ = Var.binding ring_color_var oklab_color in
    let supports_block =
      Css.supports ~condition:Color.color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
    in
    Style.style ~rules:(Some [ supports_block ]) [ fallback ]

  let ring_inherit =
    let d, _ = Var.binding ring_color_var Css.Inherit in
    style [ d ]

  let ring_bracket_hex inner =
    let c = Color.hex inner in
    let css_color = Color.to_css c 500 in
    let d, _ = Var.binding ring_color_var css_color in
    style [ d ]

  let ring_bracket_hex_with_opacity inner opacity =
    let percent = Color.opacity_to_percent opacity in
    let hex =
      if String.starts_with ~prefix:"#" inner then inner else "#" ^ inner
    in
    let alpha = percent /. 100.0 in
    let d, _ =
      Var.binding ring_color_var (Color.hex_to_oklab_alpha hex alpha)
    in
    style [ d ]

  let ring_bracket_color_var v =
    let bare_name = Parse.extract_var_name v in
    let d, _ = Var.binding ring_color_var (Css.Var (Css.var_ref bare_name)) in
    Style.style ~merge_key:("ring-color:" ^ v) [ d ]

  let ring_bracket_color_var_with_opacity v opacity =
    let percent = Color.opacity_to_percent opacity in
    let bare_name = Parse.extract_var_name v in
    let var_color : Css.color = Css.Var (Css.var_ref bare_name) in
    let fallback, _ = Var.binding ring_color_var var_color in
    let oklab_color =
      Css.color_mix ~in_space:Oklab var_color Css.Transparent ~percent1:percent
    in
    let oklab_decl, _ = Var.binding ring_color_var oklab_color in
    let supports_block =
      Css.supports ~condition:Color.color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
    in
    Style.style ~merge_key:("ring-color:" ^ v) ~rules:(Some [ supports_block ])
      [ fallback ]

  let ring_bracket_var v =
    let bare_name = Parse.extract_var_name v in
    let d, _ = Var.binding ring_color_var (Css.Var (Css.var_ref bare_name)) in
    Style.style ~merge_key:("ring-" ^ v) [ d ]

  let ring_bracket_var_with_opacity v opacity =
    let percent = Color.opacity_to_percent opacity in
    let bare_name = Parse.extract_var_name v in
    let var_color : Css.color = Css.Var (Css.var_ref bare_name) in
    let fallback, _ = Var.binding ring_color_var var_color in
    let oklab_color =
      Css.color_mix ~in_space:Oklab var_color Css.Transparent ~percent1:percent
    in
    let oklab_decl, _ = Var.binding ring_color_var oklab_color in
    let supports_block =
      Css.supports ~condition:Color.color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
    in
    Style.style ~merge_key:("ring-" ^ v) ~rules:(Some [ supports_block ])
      [ fallback ]

  let opacity n =
    let value = float_of_int n /. 100.0 in
    style [ opacity (Opacity_number value) ]

  let mix_blend_normal = style [ mix_blend_mode Normal ]
  let mix_blend_multiply = style [ mix_blend_mode Multiply ]
  let mix_blend_screen = style [ mix_blend_mode Screen ]
  let mix_blend_overlay = style [ mix_blend_mode Overlay ]
  let mix_blend_darken = style [ mix_blend_mode Darken ]
  let mix_blend_lighten = style [ mix_blend_mode Lighten ]
  let mix_blend_color_dodge = style [ mix_blend_mode Color_dodge ]
  let mix_blend_color_burn = style [ mix_blend_mode Color_burn ]
  let mix_blend_hard_light = style [ mix_blend_mode Hard_light ]
  let mix_blend_soft_light = style [ mix_blend_mode Soft_light ]
  let mix_blend_difference = style [ mix_blend_mode Difference ]
  let mix_blend_exclusion = style [ mix_blend_mode Exclusion ]
  let mix_blend_hue = style [ mix_blend_mode Hue ]
  let mix_blend_saturation = style [ mix_blend_mode Saturation ]
  let mix_blend_color = style [ mix_blend_mode Color ]
  let mix_blend_luminosity = style [ mix_blend_mode Luminosity ]
  let mix_blend_plus_darker = style [ mix_blend_mode Plus_darker ]
  let mix_blend_plus_lighter = style [ mix_blend_mode Plus_lighter ]

  (* Background blend mode styles *)
  let bg_blend_normal = style [ background_blend_mode Normal ]
  let bg_blend_multiply = style [ background_blend_mode Multiply ]
  let bg_blend_screen = style [ background_blend_mode Screen ]
  let bg_blend_overlay = style [ background_blend_mode Overlay ]
  let bg_blend_darken = style [ background_blend_mode Darken ]
  let bg_blend_lighten = style [ background_blend_mode Lighten ]
  let bg_blend_color_dodge = style [ background_blend_mode Color_dodge ]
  let bg_blend_color_burn = style [ background_blend_mode Color_burn ]
  let bg_blend_hard_light = style [ background_blend_mode Hard_light ]
  let bg_blend_soft_light = style [ background_blend_mode Soft_light ]
  let bg_blend_difference = style [ background_blend_mode Difference ]
  let bg_blend_exclusion = style [ background_blend_mode Exclusion ]
  let bg_blend_hue = style [ background_blend_mode Hue ]
  let bg_blend_saturation = style [ background_blend_mode Saturation ]
  let bg_blend_color = style [ background_blend_mode Color ]
  let bg_blend_luminosity = style [ background_blend_mode Luminosity ]

  let to_style = function
    | Shadow_none -> shadow_none
    | Shadow_sm -> shadow_sm
    | Shadow -> shadow
    | Shadow_md -> shadow_md
    | Shadow_lg -> shadow_lg
    | Shadow_xl -> shadow_xl
    | Shadow_2xl -> shadow_2xl
    | Shadow_inner -> shadow_inner
    | Shadow_arbitrary arb -> shadow_arbitrary arb
    | Shadow_arbitrary_opacity (arb, op) -> shadow_arbitrary_opacity arb op
    | Shadow_shape_opacity (shape, op) -> shadow_shape_opacity_style shape op
    | Shadow_color (c, s) -> set_shadow_color c s
    | Shadow_color_opacity (c, s, op) -> set_shadow_color_opacity c s op
    | Shadow_current -> set_shadow_current ()
    | Shadow_current_opacity op -> set_shadow_current_opacity op
    | Shadow_inherit -> set_shadow_inherit ()
    | Shadow_transparent -> set_shadow_transparent ()
    | Shadow_bracket_hex h ->
        set_shadow_bracket_hex (String.sub h 1 (String.length h - 1))
    | Shadow_bracket_hex_opacity (h, op) ->
        set_shadow_bracket_hex_opacity (String.sub h 1 (String.length h - 1)) op
    | Shadow_bracket_color_var v -> set_shadow_bracket_color_var v
    | Shadow_bracket_color_var_opacity (v, op) ->
        set_shadow_bracket_color_var_opacity v op
    | Shadow_bracket_shadow s -> shadow_raw_var s
    | Shadow_bracket_var v -> shadow_raw_var v
    | Inset_shadow_none -> inset_shadow_none
    | Inset_shadow_sm -> inset_shadow_sm
    | Inset_shadow -> inset_shadow
    | Inset_shadow_md -> inset_shadow_md
    | Inset_shadow_lg -> inset_shadow_lg
    | Inset_shadow_xl -> inset_shadow_xl
    | Inset_shadow_2xl -> inset_shadow_2xl
    | Inset_shadow_arbitrary arb -> inset_shadow_arbitrary arb
    | Opacity n -> opacity n
    | Opacity_decimal f ->
        let value = f /. 100.0 in
        style [ Css.opacity (Css.Opacity_number value) ]
    | Opacity_arbitrary f -> style [ Css.opacity (Css.Opacity_number f) ]
    | Opacity_var v ->
        let bare_name = Parse.extract_var_name v in
        let var_ref : Css.opacity Css.var = Css.var_ref bare_name in
        style [ Css.opacity (Css.Var var_ref) ]
    | Ring_none -> ring_none
    | Ring_xs -> ring_xs
    | Ring_sm -> ring_sm
    | Ring_md -> ring_default ()
    | Ring_lg -> ring_lg
    | Ring_xl -> ring_xl
    | Ring_inset -> ring_inset
    | Ring_color (color, shade) -> ring_color color shade
    | Ring_color_opacity (color, shade, opacity) ->
        ring_color_with_opacity color shade opacity
    | Ring_transparent -> ring_transparent
    | Ring_current -> ring_current
    | Ring_current_opacity opacity -> ring_current_with_opacity opacity
    | Ring_inherit -> ring_inherit
    | Ring_bracket_hex h -> ring_bracket_hex h
    | Ring_bracket_hex_opacity (h, o) -> ring_bracket_hex_with_opacity h o
    | Ring_bracket_color_var v -> ring_bracket_color_var v
    | Ring_bracket_color_var_opacity (v, o) ->
        ring_bracket_color_var_with_opacity v o
    | Ring_bracket_var v -> ring_bracket_var v
    | Ring_bracket_var_opacity (v, o) -> ring_bracket_var_with_opacity v o
    | Ring_bracket_length inner ->
        let width_value = parse_bracket_width inner in
        (* Build ring shadow like ring_internal but with bracket length *)
        let offset_width_ref = Var.reference ring_offset_width_var in
        let spread : Css.length =
          Calc (Expr (Val width_value, Add, Var offset_width_ref))
        in
        let color : Css.color =
          Var (Css.var_ref ~fallback:(Fallback Css.Current) "tw-ring-color")
        in
        let ring_shadow_value =
          Css.shadow ~inset_var:"tw-ring-inset" ~h_offset:Zero ~v_offset:Zero
            ~blur:Zero ~spread ~color ()
        in
        let d_ring, _ = Var.binding ring_shadow_var ring_shadow_value in
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
        let property_rules =
          [
            Var.property_rule shadow_var;
            Var.property_rule inset_shadow_var;
            Var.property_rule ring_shadow_var;
            Var.property_rule inset_ring_shadow_var;
            Var.property_rule ring_offset_width_var;
            Var.property_rule ring_offset_color_var;
            Var.property_rule ring_offset_shadow_var;
          ]
          |> List.filter_map (fun x -> x)
        in
        style
          ~property_rules:(Css.concat property_rules)
          [ d_ring; Css.box_shadows box_shadow_vars ]
    | Ring_offset_width n -> ring_offset_width n
    | Ring_offset_bracket_length inner -> ring_offset_bracket_length inner
    | Ring_offset_color (color, shade) -> ring_offset_color color shade
    | Ring_offset_color_opacity (color, shade, opacity) ->
        ring_offset_color_with_opacity color shade opacity
    | Ring_offset_transparent -> ring_offset_transparent
    | Ring_offset_current -> ring_offset_current
    | Ring_offset_current_opacity opacity ->
        ring_offset_current_with_opacity opacity
    | Ring_offset_inherit -> ring_offset_inherit
    | Ring_offset_bracket_hex h -> ring_offset_bracket_hex h
    | Ring_offset_bracket_hex_opacity (h, o) ->
        ring_offset_bracket_hex_with_opacity h o
    | Ring_offset_bracket_color_var v -> ring_offset_bracket_color_var v
    | Ring_offset_bracket_color_var_opacity (v, o) ->
        ring_offset_bracket_color_var_with_opacity v o
    | Ring_offset_bracket_var v -> ring_offset_bracket_var v
    | Ring_offset_bracket_var_opacity (v, o) ->
        ring_offset_bracket_var_with_opacity v o
    | Inset_ring_color (color, shade) -> inset_ring_color color shade
    | Inset_ring_color_opacity (color, shade, opacity) ->
        inset_ring_color_with_opacity color shade opacity
    | Inset_ring_transparent -> inset_ring_transparent
    | Inset_ring_current -> inset_ring_current
    | Inset_ring_current_opacity opacity ->
        inset_ring_current_with_opacity opacity
    | Inset_ring_inherit -> inset_ring_inherit
    | Inset_ring_bracket_hex h -> inset_ring_bracket_hex h
    | Inset_ring_bracket_hex_opacity (h, o) ->
        inset_ring_bracket_hex_with_opacity h o
    | Inset_ring_bracket_color_var v -> inset_ring_bracket_color_var v
    | Inset_ring_bracket_color_var_opacity (v, o) ->
        inset_ring_bracket_color_var_with_opacity v o
    | Inset_ring_bracket_var v -> inset_ring_bracket_var v
    | Inset_ring_bracket_var_opacity (v, o) ->
        inset_ring_bracket_var_with_opacity v o
    | Inset_ring_default -> inset_ring_default ()
    | Inset_ring_width n -> inset_ring_internal n
    | Inset_ring_bracket_length inner ->
        let spread = parse_bracket_width inner in
        let color : Css.color =
          Var
            (Css.var_ref ~fallback:(Fallback Css.Current) "tw-inset-ring-color")
        in
        let shadow_value =
          Css.shadow ~inset:true ~h_offset:Zero ~v_offset:Zero ~blur:Zero
            ~spread ~color ()
        in
        let d, _ = Var.binding inset_ring_shadow_var shadow_value in
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
        let property_rules =
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
          |> List.filter_map (fun x -> x)
        in
        style
          ~property_rules:(Css.concat property_rules)
          [ d; Css.box_shadows box_shadow_vars ]
    | Mix_blend_normal -> mix_blend_normal
    | Mix_blend_multiply -> mix_blend_multiply
    | Mix_blend_screen -> mix_blend_screen
    | Mix_blend_overlay -> mix_blend_overlay
    | Mix_blend_darken -> mix_blend_darken
    | Mix_blend_lighten -> mix_blend_lighten
    | Mix_blend_color_dodge -> mix_blend_color_dodge
    | Mix_blend_color_burn -> mix_blend_color_burn
    | Mix_blend_hard_light -> mix_blend_hard_light
    | Mix_blend_soft_light -> mix_blend_soft_light
    | Mix_blend_difference -> mix_blend_difference
    | Mix_blend_exclusion -> mix_blend_exclusion
    | Mix_blend_hue -> mix_blend_hue
    | Mix_blend_saturation -> mix_blend_saturation
    | Mix_blend_color -> mix_blend_color
    | Mix_blend_luminosity -> mix_blend_luminosity
    | Mix_blend_plus_darker -> mix_blend_plus_darker
    | Mix_blend_plus_lighter -> mix_blend_plus_lighter
    | Bg_blend_normal -> bg_blend_normal
    | Bg_blend_multiply -> bg_blend_multiply
    | Bg_blend_screen -> bg_blend_screen
    | Bg_blend_overlay -> bg_blend_overlay
    | Bg_blend_darken -> bg_blend_darken
    | Bg_blend_lighten -> bg_blend_lighten
    | Bg_blend_color_dodge -> bg_blend_color_dodge
    | Bg_blend_color_burn -> bg_blend_color_burn
    | Bg_blend_hard_light -> bg_blend_hard_light
    | Bg_blend_soft_light -> bg_blend_soft_light
    | Bg_blend_difference -> bg_blend_difference
    | Bg_blend_exclusion -> bg_blend_exclusion
    | Bg_blend_hue -> bg_blend_hue
    | Bg_blend_saturation -> bg_blend_saturation
    | Bg_blend_color -> bg_blend_color
    | Bg_blend_luminosity -> bg_blend_luminosity

  let err_not_utility = Error (`Msg "Not an effects utility")

  let parse_ring_bracket kind v =
    let base_str, opacity = Color.parse_opacity_modifier v in
    let inner = Parse.bracket_inner base_str in
    let starts prefix s =
      String.length s >= String.length prefix
      && String.sub s 0 (String.length prefix) = prefix
    in
    match kind with
    | `Ring -> (
        if starts "color:" inner then
          let var_part = String.sub inner 6 (String.length inner - 6) in
          match opacity with
          | Color.No_opacity -> Ok (Ring_bracket_color_var var_part)
          | _ -> Ok (Ring_bracket_color_var_opacity (var_part, opacity))
        else if starts "var(" inner then
          match opacity with
          | Color.No_opacity -> Ok (Ring_bracket_var inner)
          | _ -> Ok (Ring_bracket_var_opacity (inner, opacity))
        else if starts "#" inner then
          match opacity with
          | Color.No_opacity -> Ok (Ring_bracket_hex inner)
          | _ -> Ok (Ring_bracket_hex_opacity (inner, opacity))
        else if starts "length:" inner then Ok (Ring_bracket_length inner)
        else
          match parse_bracket_width_opt inner with
          | Some _ -> Ok (Ring_bracket_length inner)
          | None -> err_not_utility)
    | `Ring_offset -> (
        if starts "color:" inner then
          let var_part = String.sub inner 6 (String.length inner - 6) in
          match opacity with
          | Color.No_opacity -> Ok (Ring_offset_bracket_color_var var_part)
          | _ -> Ok (Ring_offset_bracket_color_var_opacity (var_part, opacity))
        else if starts "var(" inner then
          match opacity with
          | Color.No_opacity -> Ok (Ring_offset_bracket_var inner)
          | _ -> Ok (Ring_offset_bracket_var_opacity (inner, opacity))
        else if starts "#" inner then
          match opacity with
          | Color.No_opacity -> Ok (Ring_offset_bracket_hex inner)
          | _ -> Ok (Ring_offset_bracket_hex_opacity (inner, opacity))
        else if starts "length:" inner then
          Ok (Ring_offset_bracket_length inner)
        else
          match parse_bracket_width_opt inner with
          | Some _ -> Ok (Ring_offset_bracket_length inner)
          | None -> err_not_utility)
    | `Inset_ring -> (
        if starts "color:" inner then
          let var_part = String.sub inner 6 (String.length inner - 6) in
          match opacity with
          | Color.No_opacity -> Ok (Inset_ring_bracket_color_var var_part)
          | _ -> Ok (Inset_ring_bracket_color_var_opacity (var_part, opacity))
        else if starts "var(" inner then
          match opacity with
          | Color.No_opacity -> Ok (Inset_ring_bracket_var inner)
          | _ -> Ok (Inset_ring_bracket_var_opacity (inner, opacity))
        else if starts "#" inner then
          match opacity with
          | Color.No_opacity -> Ok (Inset_ring_bracket_hex inner)
          | _ -> Ok (Inset_ring_bracket_hex_opacity (inner, opacity))
        else if starts "length:" inner then Ok (Inset_ring_bracket_length inner)
        else
          match parse_bracket_width_opt inner with
          | Some _ -> Ok (Inset_ring_bracket_length inner)
          | None -> err_not_utility)

  let parse_shadow_bracket v =
    let base_str, opacity = Color.parse_opacity_modifier v in
    let inner = Parse.bracket_inner base_str in
    let starts prefix s =
      String.length s >= String.length prefix
      && String.sub s 0 (String.length prefix) = prefix
    in
    if starts "shadow:" inner then
      let shadow_part = String.sub inner 7 (String.length inner - 7) in
      Ok (Shadow_bracket_shadow shadow_part)
    else if starts "color:" inner then
      let var_part = String.sub inner 6 (String.length inner - 6) in
      match opacity with
      | Color.No_opacity -> Ok (Shadow_bracket_color_var var_part)
      | _ -> Ok (Shadow_bracket_color_var_opacity (var_part, opacity))
    else if starts "var(" inner then
      match opacity with
      | Color.No_opacity -> Ok (Shadow_bracket_var inner)
      | _ -> err_not_utility
    else if starts "#" inner then
      match opacity with
      | Color.No_opacity -> Ok (Shadow_bracket_hex inner)
      | _ -> Ok (Shadow_bracket_hex_opacity (inner, opacity))
    else
      match opacity with
      | Color.No_opacity -> Ok (Shadow_arbitrary inner)
      | _ -> Ok (Shadow_arbitrary_opacity (inner, opacity))

  let of_class class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "shadow"; "none" ] -> Ok Shadow_none
    | [ "shadow"; "sm" ] -> Ok Shadow_sm
    | [ "shadow" ] -> Ok Shadow
    | [ base ] when String.starts_with ~prefix:"shadow/" base -> (
        let _, opacity = Color.parse_opacity_modifier base in
        match opacity with
        | Color.No_opacity -> err_not_utility
        | op -> Ok (Shadow_shape_opacity (Sh_default, op)))
    | [ "shadow"; "md" ] -> Ok Shadow_md
    | [ "shadow"; "lg" ] -> Ok Shadow_lg
    | [ "shadow"; "xl" ] -> Ok Shadow_xl
    | [ "shadow"; "2xl" ] -> Ok Shadow_2xl
    | [ "shadow"; "inner" ] -> Ok Shadow_inner
    | [ "shadow"; "inherit" ] -> Ok Shadow_inherit
    | [ "shadow"; "transparent" ] -> Ok Shadow_transparent
    | [ "shadow"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = Color.parse_opacity_modifier current_str in
        match opacity with
        | Color.No_opacity when base = "current" -> Ok Shadow_current
        | Color.No_opacity -> err_not_utility
        | _ -> Ok (Shadow_current_opacity opacity))
    | [ "shadow"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (Color.parse_opacity_modifier v)) ->
        parse_shadow_bracket v
    | [ "shadow"; name ] when String.contains name '/' -> (
        let base, opacity = Color.parse_opacity_modifier name in
        match (base, opacity) with
        | _, Color.No_opacity -> err_not_utility
        | "sm", op -> Ok (Shadow_shape_opacity (Sh_sm, op))
        | "md", op -> Ok (Shadow_shape_opacity (Sh_md, op))
        | "lg", op -> Ok (Shadow_shape_opacity (Sh_lg, op))
        | "xl", op -> Ok (Shadow_shape_opacity (Sh_xl, op))
        | "2xl", op -> Ok (Shadow_shape_opacity (Sh_2xl, op))
        | _ -> err_not_utility)
    | [ "shadow"; color; shade ] -> (
        let shade_str, opacity = Color.parse_opacity_modifier shade in
        match (Color.of_string color, Parse.int_any shade_str) with
        | Ok c, Ok s -> (
            match opacity with
            | Color.No_opacity -> Ok (Shadow_color (c, s))
            | _ -> Ok (Shadow_color_opacity (c, s, opacity)))
        | _ -> err_not_utility)
    | [ "inset"; "shadow"; "none" ] -> Ok Inset_shadow_none
    | [ "inset"; "shadow"; "sm" ] -> Ok Inset_shadow_sm
    | [ "inset"; "shadow" ] -> Ok Inset_shadow
    | [ "inset"; "shadow"; "md" ] -> Ok Inset_shadow_md
    | [ "inset"; "shadow"; "lg" ] -> Ok Inset_shadow_lg
    | [ "inset"; "shadow"; "xl" ] -> Ok Inset_shadow_xl
    | [ "inset"; "shadow"; "2xl" ] -> Ok Inset_shadow_2xl
    | [ "inset"; "shadow"; arb ] when String.length arb > 2 && arb.[0] = '[' ->
        (* arbitrary value like inset-shadow-[12px_12px_#color] *)
        let len = String.length arb in
        if arb.[len - 1] = ']' then
          let inner = String.sub arb 1 (len - 2) in
          Ok (Inset_shadow_arbitrary inner)
        else err_not_utility
    | [ "opacity"; n ] when String.length n > 0 && n.[0] = '[' ->
        let len = String.length n in
        if len > 2 && n.[len - 1] = ']' then
          let inner = String.sub n 1 (len - 2) in
          if Parse.is_var inner then Ok (Opacity_var inner)
          else
            match float_of_string_opt inner with
            | Some f -> Ok (Opacity_arbitrary f)
            | None -> err_not_utility
        else err_not_utility
    | [ "opacity"; n ] -> (
        match Parse.spacing_value ~name:"opacity" n with
        | Ok f when String.contains n '.' -> Ok (Opacity_decimal f)
        | Ok f ->
            let i = int_of_float f in
            if i >= 0 && i <= 100 then Ok (Opacity i) else err_not_utility
        | Error _ -> err_not_utility)
    | [ "ring" ] -> Ok Ring_md
    | [ "ring"; "0" ] -> Ok Ring_none
    | [ "ring"; "1" ] -> Ok Ring_xs
    | [ "ring"; "2" ] -> Ok Ring_sm
    | [ "ring"; "4" ] -> Ok Ring_lg
    | [ "ring"; "8" ] -> Ok Ring_xl
    | [ "ring"; "inset" ] -> Ok Ring_inset
    | [ "ring"; "transparent" ] -> Ok Ring_transparent
    | [ "ring"; "inherit" ] -> Ok Ring_inherit
    | [ "ring"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = Color.parse_opacity_modifier current_str in
        match opacity with
        | Color.No_opacity when base = "current" -> Ok Ring_current
        | Color.No_opacity -> err_not_utility
        | _ -> Ok (Ring_current_opacity opacity))
    | [ "ring"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (Color.parse_opacity_modifier v)) ->
        parse_ring_bracket `Ring v
    | [ "ring"; "offset"; "transparent" ] -> Ok Ring_offset_transparent
    | [ "ring"; "offset"; "inherit" ] -> Ok Ring_offset_inherit
    | [ "ring"; "offset"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = Color.parse_opacity_modifier current_str in
        match opacity with
        | Color.No_opacity when base = "current" -> Ok Ring_offset_current
        | Color.No_opacity -> err_not_utility
        | _ -> Ok (Ring_offset_current_opacity opacity))
    | [ "ring"; "offset"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (Color.parse_opacity_modifier v)) ->
        parse_ring_bracket `Ring_offset v
    | [ "ring"; "offset"; n ] -> (
        match Parse.int_any n with
        | Ok width -> Ok (Ring_offset_width width)
        | Error _ -> err_not_utility)
    | [ "ring"; color; shade ] -> (
        (* Check for opacity modifier in shade (e.g., "500/50" or
           "500/[0.5]") *)
        let shade_str, opacity = Color.parse_opacity_modifier shade in
        match (Color.of_string color, Parse.int_any shade_str) with
        | Ok c, Ok s -> (
            match opacity with
            | Color.No_opacity -> Ok (Ring_color (c, s))
            | _ -> Ok (Ring_color_opacity (c, s, opacity)))
        | _ -> err_not_utility)
    | [ "ring"; "offset"; color; shade ] -> (
        let shade_str, opacity = Color.parse_opacity_modifier shade in
        match (Color.of_string color, Parse.int_any shade_str) with
        | Ok c, Ok s -> (
            match opacity with
            | Color.No_opacity -> Ok (Ring_offset_color (c, s))
            | _ -> Ok (Ring_offset_color_opacity (c, s, opacity)))
        | _ -> err_not_utility)
    | [ "inset"; "ring" ] -> Ok Inset_ring_default
    | [ "inset"; "ring"; "transparent" ] -> Ok Inset_ring_transparent
    | [ "inset"; "ring"; "inherit" ] -> Ok Inset_ring_inherit
    | [ "inset"; "ring"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = Color.parse_opacity_modifier current_str in
        match opacity with
        | Color.No_opacity when base = "current" -> Ok Inset_ring_current
        | Color.No_opacity -> err_not_utility
        | _ -> Ok (Inset_ring_current_opacity opacity))
    | [ "inset"; "ring"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (Color.parse_opacity_modifier v)) ->
        parse_ring_bracket `Inset_ring v
    | [ "inset"; "ring"; n ] -> (
        match Parse.int_any n with
        | Ok width -> Ok (Inset_ring_width width)
        | Error _ -> err_not_utility)
    | [ "inset"; "ring"; color; shade ] -> (
        let shade_str, opacity = Color.parse_opacity_modifier shade in
        match (Color.of_string color, Parse.int_any shade_str) with
        | Ok c, Ok s -> (
            match opacity with
            | Color.No_opacity -> Ok (Inset_ring_color (c, s))
            | _ -> Ok (Inset_ring_color_opacity (c, s, opacity)))
        | _ -> err_not_utility)
    | [ "mix"; "blend"; "normal" ] -> Ok Mix_blend_normal
    | [ "mix"; "blend"; "multiply" ] -> Ok Mix_blend_multiply
    | [ "mix"; "blend"; "screen" ] -> Ok Mix_blend_screen
    | [ "mix"; "blend"; "overlay" ] -> Ok Mix_blend_overlay
    | [ "mix"; "blend"; "darken" ] -> Ok Mix_blend_darken
    | [ "mix"; "blend"; "lighten" ] -> Ok Mix_blend_lighten
    | [ "mix"; "blend"; "color"; "dodge" ] -> Ok Mix_blend_color_dodge
    | [ "mix"; "blend"; "color"; "burn" ] -> Ok Mix_blend_color_burn
    | [ "mix"; "blend"; "hard"; "light" ] -> Ok Mix_blend_hard_light
    | [ "mix"; "blend"; "soft"; "light" ] -> Ok Mix_blend_soft_light
    | [ "mix"; "blend"; "difference" ] -> Ok Mix_blend_difference
    | [ "mix"; "blend"; "exclusion" ] -> Ok Mix_blend_exclusion
    | [ "mix"; "blend"; "hue" ] -> Ok Mix_blend_hue
    | [ "mix"; "blend"; "saturation" ] -> Ok Mix_blend_saturation
    | [ "mix"; "blend"; "color" ] -> Ok Mix_blend_color
    | [ "mix"; "blend"; "luminosity" ] -> Ok Mix_blend_luminosity
    | [ "mix"; "blend"; "plus"; "darker" ] -> Ok Mix_blend_plus_darker
    | [ "mix"; "blend"; "plus"; "lighter" ] -> Ok Mix_blend_plus_lighter
    | [ "bg"; "blend"; "normal" ] -> Ok Bg_blend_normal
    | [ "bg"; "blend"; "multiply" ] -> Ok Bg_blend_multiply
    | [ "bg"; "blend"; "screen" ] -> Ok Bg_blend_screen
    | [ "bg"; "blend"; "overlay" ] -> Ok Bg_blend_overlay
    | [ "bg"; "blend"; "darken" ] -> Ok Bg_blend_darken
    | [ "bg"; "blend"; "lighten" ] -> Ok Bg_blend_lighten
    | [ "bg"; "blend"; "color"; "dodge" ] -> Ok Bg_blend_color_dodge
    | [ "bg"; "blend"; "color"; "burn" ] -> Ok Bg_blend_color_burn
    | [ "bg"; "blend"; "hard"; "light" ] -> Ok Bg_blend_hard_light
    | [ "bg"; "blend"; "soft"; "light" ] -> Ok Bg_blend_soft_light
    | [ "bg"; "blend"; "difference" ] -> Ok Bg_blend_difference
    | [ "bg"; "blend"; "exclusion" ] -> Ok Bg_blend_exclusion
    | [ "bg"; "blend"; "hue" ] -> Ok Bg_blend_hue
    | [ "bg"; "blend"; "saturation" ] -> Ok Bg_blend_saturation
    | [ "bg"; "blend"; "color" ] -> Ok Bg_blend_color
    | [ "bg"; "blend"; "luminosity" ] -> Ok Bg_blend_luminosity
    | _ -> err_not_utility

  let to_class = function
    | Shadow_none -> "shadow-none"
    | Shadow_sm -> "shadow-sm"
    | Shadow -> "shadow"
    | Shadow_md -> "shadow-md"
    | Shadow_lg -> "shadow-lg"
    | Shadow_xl -> "shadow-xl"
    | Shadow_2xl -> "shadow-2xl"
    | Shadow_inner -> "shadow-inner"
    | Shadow_arbitrary arb -> "shadow-[" ^ arb ^ "]"
    | Shadow_arbitrary_opacity (arb, op) ->
        "shadow-[" ^ arb ^ "]/" ^ Color.pp_opacity op
    | Shadow_shape_opacity (shape, op) ->
        (match shape with
          | Sh_sm -> "shadow-sm"
          | Sh_default -> "shadow"
          | Sh_md -> "shadow-md"
          | Sh_lg -> "shadow-lg"
          | Sh_xl -> "shadow-xl"
          | Sh_2xl -> "shadow-2xl")
        ^ "/" ^ Color.pp_opacity op
    | Shadow_color (c, s) -> "shadow-" ^ Color.pp c ^ "-" ^ string_of_int s
    | Shadow_color_opacity (c, s, op) ->
        "shadow-" ^ Color.pp c ^ "-" ^ string_of_int s ^ "/"
        ^ Color.pp_opacity op
    | Shadow_current -> "shadow-current"
    | Shadow_current_opacity op -> "shadow-current/" ^ Color.pp_opacity op
    | Shadow_inherit -> "shadow-inherit"
    | Shadow_transparent -> "shadow-transparent"
    | Shadow_bracket_hex h -> "shadow-[" ^ h ^ "]"
    | Shadow_bracket_hex_opacity (h, op) ->
        "shadow-[" ^ h ^ "]/" ^ Color.pp_opacity op
    | Shadow_bracket_color_var v -> "shadow-[color:" ^ v ^ "]"
    | Shadow_bracket_color_var_opacity (v, op) ->
        "shadow-[color:" ^ v ^ "]/" ^ Color.pp_opacity op
    | Shadow_bracket_shadow s -> "shadow-[shadow:" ^ s ^ "]"
    | Shadow_bracket_var v -> "shadow-[" ^ v ^ "]"
    | Inset_shadow_none -> "inset-shadow-none"
    | Inset_shadow_sm -> "inset-shadow-sm"
    | Inset_shadow -> "inset-shadow"
    | Inset_shadow_md -> "inset-shadow-md"
    | Inset_shadow_lg -> "inset-shadow-lg"
    | Inset_shadow_xl -> "inset-shadow-xl"
    | Inset_shadow_2xl -> "inset-shadow-2xl"
    | Inset_shadow_arbitrary arb -> "inset-shadow-[" ^ arb ^ "]"
    | Opacity n -> "opacity-" ^ string_of_int n
    | Opacity_decimal f -> "opacity-" ^ pp_float f
    | Opacity_arbitrary f -> "opacity-[" ^ pp_float f ^ "]"
    | Opacity_var v -> "opacity-[" ^ v ^ "]"
    | Ring_none -> "ring-0"
    | Ring_xs -> "ring-1"
    | Ring_sm -> "ring-2"
    | Ring_md -> "ring"
    | Ring_lg -> "ring-4"
    | Ring_xl -> "ring-8"
    | Ring_inset -> "ring-inset"
    | Ring_color (color, shade) ->
        "ring-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | Ring_color_opacity (color, shade, opacity) ->
        "ring-" ^ Color.pp color ^ "-" ^ string_of_int shade ^ "/"
        ^ Color.pp_opacity opacity
    | Ring_transparent -> "ring-transparent"
    | Ring_current -> "ring-current"
    | Ring_current_opacity o -> "ring-current/" ^ Color.pp_opacity o
    | Ring_inherit -> "ring-inherit"
    | Ring_bracket_hex h -> "ring-[" ^ h ^ "]"
    | Ring_bracket_hex_opacity (h, o) ->
        "ring-[" ^ h ^ "]/" ^ Color.pp_opacity o
    | Ring_bracket_color_var v -> "ring-[color:" ^ v ^ "]"
    | Ring_bracket_color_var_opacity (v, o) ->
        "ring-[color:" ^ v ^ "]/" ^ Color.pp_opacity o
    | Ring_bracket_var v -> "ring-[" ^ v ^ "]"
    | Ring_bracket_var_opacity (v, o) ->
        "ring-[" ^ v ^ "]/" ^ Color.pp_opacity o
    | Ring_bracket_length l -> "ring-[" ^ l ^ "]"
    | Ring_offset_width n -> "ring-offset-" ^ string_of_int n
    | Ring_offset_bracket_length l -> "ring-offset-[" ^ l ^ "]"
    | Ring_offset_color (color, shade) ->
        "ring-offset-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | Ring_offset_color_opacity (color, shade, opacity) ->
        "ring-offset-" ^ Color.pp color ^ "-" ^ string_of_int shade ^ "/"
        ^ Color.pp_opacity opacity
    | Ring_offset_transparent -> "ring-offset-transparent"
    | Ring_offset_current -> "ring-offset-current"
    | Ring_offset_current_opacity o ->
        "ring-offset-current/" ^ Color.pp_opacity o
    | Ring_offset_inherit -> "ring-offset-inherit"
    | Ring_offset_bracket_hex h -> "ring-offset-[" ^ h ^ "]"
    | Ring_offset_bracket_hex_opacity (h, o) ->
        "ring-offset-[" ^ h ^ "]/" ^ Color.pp_opacity o
    | Ring_offset_bracket_color_var v -> "ring-offset-[color:" ^ v ^ "]"
    | Ring_offset_bracket_color_var_opacity (v, o) ->
        "ring-offset-[color:" ^ v ^ "]/" ^ Color.pp_opacity o
    | Ring_offset_bracket_var v -> "ring-offset-[" ^ v ^ "]"
    | Ring_offset_bracket_var_opacity (v, o) ->
        "ring-offset-[" ^ v ^ "]/" ^ Color.pp_opacity o
    | Inset_ring_color (color, shade) ->
        "inset-ring-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | Inset_ring_color_opacity (color, shade, opacity) ->
        "inset-ring-" ^ Color.pp color ^ "-" ^ string_of_int shade ^ "/"
        ^ Color.pp_opacity opacity
    | Inset_ring_transparent -> "inset-ring-transparent"
    | Inset_ring_current -> "inset-ring-current"
    | Inset_ring_current_opacity o -> "inset-ring-current/" ^ Color.pp_opacity o
    | Inset_ring_inherit -> "inset-ring-inherit"
    | Inset_ring_bracket_hex h -> "inset-ring-[" ^ h ^ "]"
    | Inset_ring_bracket_hex_opacity (h, o) ->
        "inset-ring-[" ^ h ^ "]/" ^ Color.pp_opacity o
    | Inset_ring_bracket_color_var v -> "inset-ring-[color:" ^ v ^ "]"
    | Inset_ring_bracket_color_var_opacity (v, o) ->
        "inset-ring-[color:" ^ v ^ "]/" ^ Color.pp_opacity o
    | Inset_ring_bracket_var v -> "inset-ring-[" ^ v ^ "]"
    | Inset_ring_bracket_var_opacity (v, o) ->
        "inset-ring-[" ^ v ^ "]/" ^ Color.pp_opacity o
    | Inset_ring_default -> "inset-ring"
    | Inset_ring_width n -> "inset-ring-" ^ string_of_int n
    | Inset_ring_bracket_length l -> "inset-ring-[" ^ l ^ "]"
    | Mix_blend_normal -> "mix-blend-normal"
    | Mix_blend_multiply -> "mix-blend-multiply"
    | Mix_blend_screen -> "mix-blend-screen"
    | Mix_blend_overlay -> "mix-blend-overlay"
    | Mix_blend_darken -> "mix-blend-darken"
    | Mix_blend_lighten -> "mix-blend-lighten"
    | Mix_blend_color_dodge -> "mix-blend-color-dodge"
    | Mix_blend_color_burn -> "mix-blend-color-burn"
    | Mix_blend_hard_light -> "mix-blend-hard-light"
    | Mix_blend_soft_light -> "mix-blend-soft-light"
    | Mix_blend_difference -> "mix-blend-difference"
    | Mix_blend_exclusion -> "mix-blend-exclusion"
    | Mix_blend_hue -> "mix-blend-hue"
    | Mix_blend_saturation -> "mix-blend-saturation"
    | Mix_blend_color -> "mix-blend-color"
    | Mix_blend_luminosity -> "mix-blend-luminosity"
    | Mix_blend_plus_darker -> "mix-blend-plus-darker"
    | Mix_blend_plus_lighter -> "mix-blend-plus-lighter"
    | Bg_blend_normal -> "bg-blend-normal"
    | Bg_blend_multiply -> "bg-blend-multiply"
    | Bg_blend_screen -> "bg-blend-screen"
    | Bg_blend_overlay -> "bg-blend-overlay"
    | Bg_blend_darken -> "bg-blend-darken"
    | Bg_blend_lighten -> "bg-blend-lighten"
    | Bg_blend_color_dodge -> "bg-blend-color-dodge"
    | Bg_blend_color_burn -> "bg-blend-color-burn"
    | Bg_blend_hard_light -> "bg-blend-hard-light"
    | Bg_blend_soft_light -> "bg-blend-soft-light"
    | Bg_blend_difference -> "bg-blend-difference"
    | Bg_blend_exclusion -> "bg-blend-exclusion"
    | Bg_blend_hue -> "bg-blend-hue"
    | Bg_blend_saturation -> "bg-blend-saturation"
    | Bg_blend_color -> "bg-blend-color"
    | Bg_blend_luminosity -> "bg-blend-luminosity"

  let suborder = function
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
    | Shadow_shape_opacity _ -> 29991
    (* Shadow shape utilities — all same suborder, natural_compare decides
       within-group order: bracket values ([) sort before named (none/sm/xl) *)
    | Shadow | Shadow_2xl | Shadow_inner | Shadow_lg | Shadow_md | Shadow_none
    | Shadow_sm | Shadow_xl | Shadow_arbitrary _ | Shadow_bracket_shadow _
    | Shadow_bracket_var _ ->
        30000
    (* Shadow color utilities *)
    | Shadow_color _ | Shadow_color_opacity _ | Shadow_current
    | Shadow_current_opacity _ | Shadow_inherit | Shadow_transparent
    | Shadow_bracket_hex _ | Shadow_bracket_hex_opacity _
    | Shadow_bracket_color_var _ | Shadow_bracket_color_var_opacity _ ->
        35000
    (* Inset shadow utilities *)
    | Inset_shadow -> 31000
    | Inset_shadow_2xl -> 31001
    | Inset_shadow_lg -> 31002
    | Inset_shadow_md -> 31003
    | Inset_shadow_none -> 31004
    | Inset_shadow_sm -> 31005
    | Inset_shadow_xl -> 31006
    | Inset_shadow_arbitrary _ -> 31007
    (* Background blend modes come after opacity, before mix-blend *)
    | Bg_blend_color -> 22000
    | Bg_blend_color_burn -> 22001
    | Bg_blend_color_dodge -> 22002
    | Bg_blend_darken -> 22003
    | Bg_blend_difference -> 22004
    | Bg_blend_exclusion -> 22005
    | Bg_blend_hard_light -> 22006
    | Bg_blend_hue -> 22007
    | Bg_blend_lighten -> 22008
    | Bg_blend_luminosity -> 22009
    | Bg_blend_multiply -> 22010
    | Bg_blend_normal -> 22011
    | Bg_blend_overlay -> 22012
    | Bg_blend_saturation -> 22013
    | Bg_blend_screen -> 22014
    | Bg_blend_soft_light -> 22015
    (* Mix blend modes come after bg-blend, before shadows *)
    | Mix_blend_color -> 24000
    | Mix_blend_color_burn -> 24001
    | Mix_blend_color_dodge -> 24002
    | Mix_blend_darken -> 24003
    | Mix_blend_difference -> 24004
    | Mix_blend_exclusion -> 24005
    | Mix_blend_hard_light -> 24006
    | Mix_blend_hue -> 24007
    | Mix_blend_lighten -> 24008
    | Mix_blend_luminosity -> 24009
    | Mix_blend_multiply -> 24010
    | Mix_blend_normal -> 24011
    | Mix_blend_overlay -> 24012
    | Mix_blend_plus_darker -> 24013
    | Mix_blend_plus_lighter -> 24014
    | Mix_blend_saturation -> 24015
    | Mix_blend_screen -> 24016
    | Mix_blend_soft_light -> 24017
    (* Ring utilities come after shadows. Ordered to match Tailwind: 1. ring
       widths, 2. ring-color, 3. inset-ring-color, 4. ring-offset-width, 5.
       ring-offset-color, 6. ring-inset *)
    | Ring_md -> 40000
    | Ring_none -> 40001
    | Ring_xs -> 40002
    | Ring_sm -> 40003
    | Ring_lg -> 40004
    | Ring_xl -> 40005
    | Ring_bracket_length _ -> 40010
    | Ring_color _ | Ring_color_opacity _ | Ring_transparent | Ring_current
    | Ring_current_opacity _ | Ring_inherit | Ring_bracket_hex _
    | Ring_bracket_hex_opacity _ | Ring_bracket_color_var _
    | Ring_bracket_color_var_opacity _ | Ring_bracket_var _
    | Ring_bracket_var_opacity _ ->
        50000
    | Ring_inset -> 51000
    | Inset_ring_default -> 55000
    | Inset_ring_width n -> 55001 + n
    | Inset_ring_bracket_length _ -> 55100
    | Inset_ring_color _ | Inset_ring_color_opacity _ | Inset_ring_transparent
    | Inset_ring_current | Inset_ring_current_opacity _ | Inset_ring_inherit
    | Inset_ring_bracket_hex _ | Inset_ring_bracket_hex_opacity _
    | Inset_ring_bracket_color_var _ | Inset_ring_bracket_color_var_opacity _
    | Inset_ring_bracket_var _ | Inset_ring_bracket_var_opacity _ ->
        60000
    | Ring_offset_width n -> 80000 + n
    | Ring_offset_bracket_length _ -> 80100
    | Ring_offset_color _ | Ring_offset_color_opacity _
    | Ring_offset_transparent | Ring_offset_current
    | Ring_offset_current_opacity _ | Ring_offset_inherit
    | Ring_offset_bracket_hex _ | Ring_offset_bracket_hex_opacity _
    | Ring_offset_bracket_color_var _ | Ring_offset_bracket_color_var_opacity _
    | Ring_offset_bracket_var _ | Ring_offset_bracket_var_opacity _ ->
        100000
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let order u = Some (Utility.order u)
let mix_blend_luminosity = utility Mix_blend_luminosity
let shadow_none = utility Shadow_none
let shadow_sm = utility Shadow_sm
let shadow = utility Shadow
let shadow_md = utility Shadow_md
let shadow_lg = utility Shadow_lg
let shadow_xl = utility Shadow_xl
let shadow_2xl = utility Shadow_2xl
let shadow_inner = utility Shadow_inner
let inset_shadow_none = utility Inset_shadow_none
let inset_shadow_sm = utility Inset_shadow_sm
let inset_shadow = utility Inset_shadow
let inset_shadow_md = utility Inset_shadow_md
let inset_shadow_lg = utility Inset_shadow_lg
let inset_shadow_xl = utility Inset_shadow_xl
let inset_shadow_2xl = utility Inset_shadow_2xl
let ring_inset = utility Ring_inset
let ring_none = utility Ring_none
let ring_xs = utility Ring_xs
let ring_sm = utility Ring_sm
let ring = utility Ring_md
let ring_md = utility Ring_md
let ring_lg = utility Ring_lg
let ring_xl = utility Ring_xl
let ring_color color shade = utility (Ring_color (color, shade))
let inset_ring = utility Inset_ring_default
let opacity n = utility (Opacity n)
let mix_blend_normal = utility Mix_blend_normal
let mix_blend_multiply = utility Mix_blend_multiply
let mix_blend_screen = utility Mix_blend_screen
let mix_blend_overlay = utility Mix_blend_overlay
let mix_blend_darken = utility Mix_blend_darken
let mix_blend_lighten = utility Mix_blend_lighten
let mix_blend_color_dodge = utility Mix_blend_color_dodge
let mix_blend_color_burn = utility Mix_blend_color_burn
let mix_blend_hard_light = utility Mix_blend_hard_light
let mix_blend_soft_light = utility Mix_blend_soft_light
let mix_blend_difference = utility Mix_blend_difference
let mix_blend_exclusion = utility Mix_blend_exclusion
let mix_blend_hue = utility Mix_blend_hue
let mix_blend_saturation = utility Mix_blend_saturation
let mix_blend_color = utility Mix_blend_color
let mix_blend_plus_darker = utility Mix_blend_plus_darker
let mix_blend_plus_lighter = utility Mix_blend_plus_lighter

(* Export ring/shadow variables for use by Forms *)
let shadow_var = Handler.shadow_var
let ring_inset_var = Handler.ring_inset_var
let ring_offset_width_var = Handler.ring_offset_width_var
let ring_offset_color_var = Handler.ring_offset_color_var
let ring_color_var = Handler.ring_color_var
let ring_offset_shadow_var = Handler.ring_offset_shadow_var
let ring_shadow_var = Handler.ring_shadow_var
