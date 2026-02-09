(** Visual effects utilities for shadows, opacity, and filters. *)

module Handler = struct
  open Style
  open Css

  type t =
    (* Shadows *)
    | Shadow_none
    | Shadow_sm
    | Shadow
    | Shadow_md
    | Shadow_lg
    | Shadow_xl
    | Shadow_2xl
    | Shadow_inner
    (* Inset shadows *)
    | Inset_shadow_none
    | Inset_shadow_sm
    | Inset_shadow
    | Inset_shadow_md
    | Inset_shadow_lg
    | Inset_shadow_xl
    | Inset_shadow_2xl
    (* Opacity *)
    | Opacity of int
    | Opacity_arbitrary of float
    (* Rings *)
    | Ring_none
    | Ring_xs
    | Ring_sm
    | Ring_md
    | Ring_lg
    | Ring_xl
    | Ring_inset
    | Ring_color of Color.color * int
    | Ring_offset_width of int
    | Ring_offset_color of Color.color * int
    | Inset_ring_color of Color.color * int
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

  let shadow_none =
    (* Shadow-none sets shadow to transparent 0 0 *)
    let shadow_value =
      Css.shadow ~h_offset:Zero ~v_offset:Zero ~color:(Css.hex "#0000") ()
    in

    (* Only set the --tw-shadow variable to 0 0 #0000 *)
    let d_shadow, v_shadow = Var.binding shadow_var shadow_value in

    (* Reference other variables through their @property defaults *)
    let v_inset = Var.reference inset_shadow_var in
    let v_inset_ring = Var.reference inset_ring_shadow_var in
    let v_ring_offset = Var.reference ring_offset_shadow_var in
    let v_ring = Var.reference ring_shadow_var in

    let box_shadow_vars : Css.shadow =
      Css.List
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
      [ d_shadow; Css.box_shadow box_shadow_vars ]

  let shadow_sm =
    (* Shadow-sm with composite shadows matching Tailwind v4 *)
    (* Reference color variable with fallback *)
    let color_ref =
      Var.reference_with_fallback shadow_color_var (Css.hex "#0000001a")
    in
    let shadow_list =
      [
        Css.shadow ~h_offset:Zero ~v_offset:(Px 1.) ~blur:(Px 3.) ~spread:Zero
          ~color:(Var color_ref) ();
        Css.shadow ~h_offset:Zero ~v_offset:(Px 1.) ~blur:(Px 2.)
          ~spread:(Px (-1.)) ~color:(Var color_ref) ();
      ]
    in

    (* Set shadow variable with the list *)
    let d_shadow, v_shadow = Var.binding shadow_var (List shadow_list) in

    (* Reference other variables through their @property defaults *)
    let v_inset = Var.reference inset_shadow_var in
    let v_inset_ring = Var.reference inset_ring_shadow_var in
    let v_ring_offset = Var.reference ring_offset_shadow_var in
    let v_ring = Var.reference ring_shadow_var in

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
      (d_shadow :: [ Css.box_shadows box_shadow_vars ])

  let shadow =
    (* Default shadow - same as shadow-sm in Tailwind v4 *)
    (* Reference color variable with fallback *)
    let color_ref =
      Var.reference_with_fallback shadow_color_var (Css.hex "#0000001a")
    in
    let shadow_list =
      [
        Css.shadow ~h_offset:Zero ~v_offset:(Px 1.) ~blur:(Px 3.) ~spread:Zero
          ~color:(Var color_ref) ();
        Css.shadow ~h_offset:Zero ~v_offset:(Px 1.) ~blur:(Px 2.)
          ~spread:(Px (-1.)) ~color:(Var color_ref) ();
      ]
    in

    (* Only set --tw-shadow, use var references for the composition *)
    let d_shadow, v_shadow = Var.binding shadow_var (List shadow_list) in

    (* Reference other variables through their @property defaults *)
    let inset_ref = Var.reference inset_shadow_var in
    let inset_ring_ref = Var.reference inset_ring_shadow_var in
    let ring_offset_ref = Var.reference ring_offset_shadow_var in
    let ring_ref = Var.reference ring_shadow_var in

    let box_shadow_vars : Css.shadow list =
      [
        Css.Var inset_ref;
        Css.Var inset_ring_ref;
        Css.Var ring_offset_ref;
        Css.Var ring_ref;
        Css.Var v_shadow;
        (* Use the v_shadow from binding, not reference *)
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
      [ d_shadow; Css.box_shadows box_shadow_vars ]

  let shadow_md =
    (* Shadow-md with CSS variable for color *)
    let color_ref =
      Var.reference_with_fallback shadow_color_var (Css.hex "#0000001a")
    in
    let shadow_list =
      [
        Css.shadow ~h_offset:Zero ~v_offset:(Px 4.) ~blur:(Px 6.)
          ~spread:(Px (-1.)) ~color:(Var color_ref) ();
        Css.shadow ~h_offset:Zero ~v_offset:(Px 2.) ~blur:(Px 4.)
          ~spread:(Px (-2.)) ~color:(Var color_ref) ();
      ]
    in

    let d_shadow, v_shadow = Var.binding shadow_var (List shadow_list) in
    let inset_ref = Var.reference inset_shadow_var in
    let inset_ring_ref = Var.reference inset_ring_shadow_var in
    let ring_offset_ref = Var.reference ring_offset_shadow_var in
    let ring_ref = Var.reference ring_shadow_var in

    let box_shadow_vars : Css.shadow list =
      [
        Css.Var inset_ref;
        Css.Var inset_ring_ref;
        Css.Var ring_offset_ref;
        Css.Var ring_ref;
        Css.Var v_shadow;
        (* Use the v_shadow from binding, not reference *)
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
      [ d_shadow; Css.box_shadows box_shadow_vars ]

  let shadow_lg =
    (* Shadow-lg with CSS variable for color *)
    let color_ref =
      Var.reference_with_fallback shadow_color_var (Css.hex "#0000001a")
    in
    let shadow_list =
      [
        Css.shadow ~h_offset:Zero ~v_offset:(Px 10.) ~blur:(Px 15.)
          ~spread:(Px (-3.)) ~color:(Var color_ref) ();
        Css.shadow ~h_offset:Zero ~v_offset:(Px 4.) ~blur:(Px 6.)
          ~spread:(Px (-4.)) ~color:(Var color_ref) ();
      ]
    in

    let d_shadow, v_shadow = Var.binding shadow_var (List shadow_list) in
    let inset_ref = Var.reference inset_shadow_var in
    let inset_ring_ref = Var.reference inset_ring_shadow_var in
    let ring_offset_ref = Var.reference ring_offset_shadow_var in
    let ring_ref = Var.reference ring_shadow_var in

    let box_shadow_vars : Css.shadow list =
      [
        Css.Var inset_ref;
        Css.Var inset_ring_ref;
        Css.Var ring_offset_ref;
        Css.Var ring_ref;
        Css.Var v_shadow;
        (* Use the v_shadow from binding, not reference *)
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
      [ d_shadow; Css.box_shadows box_shadow_vars ]

  let shadow_xl =
    (* Shadow-xl with CSS variable for color *)
    let color_ref =
      Var.reference_with_fallback shadow_color_var (Css.hex "#0000001a")
    in
    let shadow_list =
      [
        Css.shadow ~h_offset:Zero ~v_offset:(Px 20.) ~blur:(Px 25.)
          ~spread:(Px (-5.)) ~color:(Var color_ref) ();
        Css.shadow ~h_offset:Zero ~v_offset:(Px 8.) ~blur:(Px 10.)
          ~spread:(Px (-6.)) ~color:(Var color_ref) ();
      ]
    in

    let d_shadow, v_shadow = Var.binding shadow_var (List shadow_list) in
    let inset_ref = Var.reference inset_shadow_var in
    let inset_ring_ref = Var.reference inset_ring_shadow_var in
    let ring_offset_ref = Var.reference ring_offset_shadow_var in
    let ring_ref = Var.reference ring_shadow_var in

    let box_shadow_vars : Css.shadow list =
      [
        Css.Var inset_ref;
        Css.Var inset_ring_ref;
        Css.Var ring_offset_ref;
        Css.Var ring_ref;
        Css.Var v_shadow;
        (* Use the v_shadow from binding, not reference *)
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
      [ d_shadow; Css.box_shadows box_shadow_vars ]

  let shadow_2xl =
    (* Shadow-2xl with CSS variable for color *)
    let color_ref =
      Var.reference_with_fallback shadow_color_var (Css.hex "#0000001a")
    in
    let shadow_list =
      [
        Css.shadow ~h_offset:Zero ~v_offset:(Px 25.) ~blur:(Px 50.)
          ~spread:(Px (-12.)) ~color:(Var color_ref) ();
      ]
    in

    let d_shadow, v_shadow = Var.binding shadow_var (List shadow_list) in
    let inset_ref = Var.reference inset_shadow_var in
    let inset_ring_ref = Var.reference inset_ring_shadow_var in
    let ring_offset_ref = Var.reference ring_offset_shadow_var in
    let ring_ref = Var.reference ring_shadow_var in

    let box_shadow_vars : Css.shadow list =
      [
        Css.Var inset_ref;
        Css.Var inset_ring_ref;
        Css.Var ring_offset_ref;
        Css.Var ring_ref;
        Css.Var v_shadow;
        (* Use the v_shadow from binding, not reference *)
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
      [ d_shadow; Css.box_shadows box_shadow_vars ]

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
      | `Default | `Md ->
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

  type ring_width = [ `None | `Xs | `Sm | `Md | `Lg | `Xl ]

  let ring_internal (w : ring_width) =
    (* Tailwind v4 ring widths: ring=1px, ring-1=1px, ring-2=2px, ring-4=4px,
       ring-8=8px *)
    let width_px =
      match w with
      | `None -> 0
      | `Xs -> 1
      | `Sm -> 2
      | `Md -> 1 (* Default ring is 1px in v4 *)
      | `Lg -> 4
      | `Xl -> 8
    in

    (* Tailwind v4 uses a complex ring-shadow format: var(--tw-ring-inset,) 0 0
       0 calc(Xpx + var(--tw-ring-offset-width)) var(--tw-ring-color,
       currentcolor)

       Since our typed shadow system cannot express calc() in spread, we use
       custom_property to create the raw CSS value directly. *)
    let ring_shadow_str =
      Printf.sprintf
        "var(--tw-ring-inset,)0 0 0 calc(%dpx + \
         var(--tw-ring-offset-width))var(--tw-ring-color,currentcolor)"
        width_px
    in
    let d_ring =
      Css.custom_property ~layer:"utilities" "--tw-ring-shadow" ring_shadow_str
    in

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

  let ring_none = ring_internal `None
  let ring_xs = ring_internal `Xs
  let ring_sm = ring_internal `Sm
  let ring_md = ring_internal `Md
  let ring_lg = ring_internal `Lg
  let ring_xl = ring_internal `Xl

  let ring_inset =
    let decl, _var_ref = Var.binding ring_inset_var "inset" in
    style [ decl ]

  let ring_color color shade =
    (* Use theme color variable reference like text color does: --tw-ring-color:
       var(--color-blue-400) *)
    let color_theme_var = Color.get_color_var color shade in
    let color_value = Color.to_css color shade in
    (* Bind the theme variable to get its declaration and reference *)
    let color_decl, color_ref = Var.binding color_theme_var color_value in
    (* Now set ring-color-var to reference the theme variable *)
    let d, _ = Var.binding ring_color_var (Css.Var color_ref) in
    (* Include color_decl so the theme variable gets emitted to @layer theme *)
    style [ color_decl; d ]

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
    (* Sets --tw-ring-offset-color to reference theme color variable *)
    let color_theme_var = Color.get_color_var color shade in
    let color_value = Color.to_css color shade in
    let color_decl, color_ref = Var.binding color_theme_var color_value in
    let d, _ = Var.binding ring_offset_color_var (Css.Var color_ref) in
    style [ color_decl; d ]

  let inset_ring_color color shade =
    (* Sets --tw-inset-ring-color to reference theme color variable *)
    let color_theme_var = Color.get_color_var color shade in
    let color_value = Color.to_css color shade in
    let color_decl, color_ref = Var.binding color_theme_var color_value in
    let d, _ = Var.binding inset_ring_color_var (Css.Var color_ref) in
    style [ color_decl; d ]

  let opacity n =
    let value = float_of_int n /. 100.0 in
    style [ opacity value ]

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
  let ( >|= ) = Parse.( >|= )

  let to_style = function
    | Shadow_none -> shadow_none
    | Shadow_sm -> shadow_sm
    | Shadow -> shadow
    | Shadow_md -> shadow_md
    | Shadow_lg -> shadow_lg
    | Shadow_xl -> shadow_xl
    | Shadow_2xl -> shadow_2xl
    | Shadow_inner -> shadow_inner
    | Inset_shadow_none -> inset_shadow_none
    | Inset_shadow_sm -> inset_shadow_sm
    | Inset_shadow -> inset_shadow
    | Inset_shadow_md -> inset_shadow_md
    | Inset_shadow_lg -> inset_shadow_lg
    | Inset_shadow_xl -> inset_shadow_xl
    | Inset_shadow_2xl -> inset_shadow_2xl
    | Opacity n -> opacity n
    | Opacity_arbitrary f -> style [ Css.opacity f ]
    | Ring_none -> ring_none
    | Ring_xs -> ring_xs
    | Ring_sm -> ring_sm
    | Ring_md -> ring_md
    | Ring_lg -> ring_lg
    | Ring_xl -> ring_xl
    | Ring_inset -> ring_inset
    | Ring_color (color, shade) -> ring_color color shade
    | Ring_offset_width n -> ring_offset_width n
    | Ring_offset_color (color, shade) -> ring_offset_color color shade
    | Inset_ring_color (color, shade) -> inset_ring_color color shade
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

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "shadow"; "none" ] -> Ok Shadow_none
    | [ "shadow"; "sm" ] -> Ok Shadow_sm
    | [ "shadow" ] -> Ok Shadow
    | [ "shadow"; "md" ] -> Ok Shadow_md
    | [ "shadow"; "lg" ] -> Ok Shadow_lg
    | [ "shadow"; "xl" ] -> Ok Shadow_xl
    | [ "shadow"; "2xl" ] -> Ok Shadow_2xl
    | [ "shadow"; "inner" ] -> Ok Shadow_inner
    | [ "inset"; "shadow"; "none" ] -> Ok Inset_shadow_none
    | [ "inset"; "shadow"; "sm" ] -> Ok Inset_shadow_sm
    | [ "inset"; "shadow" ] -> Ok Inset_shadow
    | [ "inset"; "shadow"; "md" ] -> Ok Inset_shadow_md
    | [ "inset"; "shadow"; "lg" ] -> Ok Inset_shadow_lg
    | [ "inset"; "shadow"; "xl" ] -> Ok Inset_shadow_xl
    | [ "inset"; "shadow"; "2xl" ] -> Ok Inset_shadow_2xl
    | [ "opacity"; n ] when String.length n > 0 && n.[0] = '[' ->
        (* arbitrary value like opacity-[0.75] *)
        let len = String.length n in
        if len > 2 && n.[len - 1] = ']' then
          let inner = String.sub n 1 (len - 2) in
          match float_of_string_opt inner with
          | Some f -> Ok (Opacity_arbitrary f)
          | None -> err_not_utility
        else err_not_utility
    | [ "opacity"; n ] ->
        Parse.int_bounded ~name:"opacity" ~min:0 ~max:100 n >|= fun n ->
        Opacity n
    | [ "ring" ] -> Ok Ring_md
    | [ "ring"; "0" ] -> Ok Ring_none
    | [ "ring"; "1" ] -> Ok Ring_xs
    | [ "ring"; "2" ] -> Ok Ring_sm
    | [ "ring"; "3" ] -> Ok Ring_md
    | [ "ring"; "4" ] -> Ok Ring_lg
    | [ "ring"; "8" ] -> Ok Ring_xl
    | [ "ring"; "inset" ] -> Ok Ring_inset
    | [ "ring"; "offset"; n ] -> (
        match Parse.int_any n with
        | Ok width -> Ok (Ring_offset_width width)
        | Error _ -> err_not_utility)
    | [ "ring"; color; shade ] -> (
        match (Color.of_string color, Parse.int_any shade) with
        | Ok c, Ok s -> Ok (Ring_color (c, s))
        | _ -> err_not_utility)
    | [ "ring"; "offset"; color; shade ] -> (
        match (Color.of_string color, Parse.int_any shade) with
        | Ok c, Ok s -> Ok (Ring_offset_color (c, s))
        | _ -> err_not_utility)
    | [ "inset"; "ring"; color; shade ] -> (
        match (Color.of_string color, Parse.int_any shade) with
        | Ok c, Ok s -> Ok (Inset_ring_color (c, s))
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
    | Inset_shadow_none -> "inset-shadow-none"
    | Inset_shadow_sm -> "inset-shadow-sm"
    | Inset_shadow -> "inset-shadow"
    | Inset_shadow_md -> "inset-shadow-md"
    | Inset_shadow_lg -> "inset-shadow-lg"
    | Inset_shadow_xl -> "inset-shadow-xl"
    | Inset_shadow_2xl -> "inset-shadow-2xl"
    | Opacity n -> "opacity-" ^ string_of_int n
    | Opacity_arbitrary f -> Printf.sprintf "opacity-[%g]" f
    | Ring_none -> "ring-0"
    | Ring_xs -> "ring-1"
    | Ring_sm -> "ring-2"
    | Ring_md -> "ring"
    | Ring_lg -> "ring-4"
    | Ring_xl -> "ring-8"
    | Ring_inset -> "ring-inset"
    | Ring_color (color, shade) ->
        "ring-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | Ring_offset_width n -> "ring-offset-" ^ string_of_int n
    | Ring_offset_color (color, shade) ->
        "ring-offset-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | Inset_ring_color (color, shade) ->
        "inset-ring-" ^ Color.pp color ^ "-" ^ string_of_int shade
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
    | Opacity n -> n
    | Opacity_arbitrary _ -> 200 (* arbitrary values come after named values *)
    (* Shadow utilities - alphabetical order *)
    | Shadow -> 1000
    | Shadow_2xl -> 1001
    | Shadow_inner -> 1002
    | Shadow_lg -> 1003
    | Shadow_md -> 1004
    | Shadow_none -> 1005
    | Shadow_sm -> 1006
    | Shadow_xl -> 1007
    (* Inset shadow utilities - alphabetical order *)
    | Inset_shadow -> 1100
    | Inset_shadow_2xl -> 1101
    | Inset_shadow_lg -> 1102
    | Inset_shadow_md -> 1103
    | Inset_shadow_none -> 1104
    | Inset_shadow_sm -> 1105
    | Inset_shadow_xl -> 1106
    (* Mix blend modes - alphabetical order *)
    | Mix_blend_color -> 2000
    | Mix_blend_color_burn -> 2001
    | Mix_blend_color_dodge -> 2002
    | Mix_blend_darken -> 2003
    | Mix_blend_difference -> 2004
    | Mix_blend_exclusion -> 2005
    | Mix_blend_hard_light -> 2006
    | Mix_blend_hue -> 2007
    | Mix_blend_lighten -> 2008
    | Mix_blend_luminosity -> 2009
    | Mix_blend_multiply -> 2010
    | Mix_blend_normal -> 2011
    | Mix_blend_overlay -> 2012
    | Mix_blend_plus_darker -> 2013
    | Mix_blend_plus_lighter -> 2014
    | Mix_blend_saturation -> 2015
    | Mix_blend_screen -> 2016
    | Mix_blend_soft_light -> 2017
    (* Background blend modes - alphabetical order *)
    | Bg_blend_color -> 3000
    | Bg_blend_color_burn -> 3001
    | Bg_blend_color_dodge -> 3002
    | Bg_blend_darken -> 3003
    | Bg_blend_difference -> 3004
    | Bg_blend_exclusion -> 3005
    | Bg_blend_hard_light -> 3006
    | Bg_blend_hue -> 3007
    | Bg_blend_lighten -> 3008
    | Bg_blend_luminosity -> 3009
    | Bg_blend_multiply -> 3010
    | Bg_blend_normal -> 3011
    | Bg_blend_overlay -> 3012
    | Bg_blend_saturation -> 3013
    | Bg_blend_screen -> 3014
    | Bg_blend_soft_light -> 3015
    (* Ring utilities ordered to match Tailwind: 1. ring widths, 2. ring-color,
       3. inset-ring-color, 4. ring-offset-width, 5. ring-offset-color, 6.
       ring-inset *)
    | Ring_md -> 10000 (* ring - comes first *)
    | Ring_none -> 10001 (* ring-0 *)
    | Ring_xs -> 10002 (* ring-1 *)
    | Ring_sm -> 10003 (* ring-2 *)
    | Ring_lg -> 10004 (* ring-4 *)
    | Ring_xl -> 10005 (* ring-8 *)
    | Ring_color (color, shade) ->
        20000
        + Color.suborder_with_shade (Color.pp color ^ "-" ^ string_of_int shade)
    | Inset_ring_color (color, shade) ->
        50000
        + Color.suborder_with_shade (Color.pp color ^ "-" ^ string_of_int shade)
    | Ring_offset_width n -> 80000 + n
    | Ring_offset_color (color, shade) ->
        100000
        + Color.suborder_with_shade (Color.pp color ^ "-" ^ string_of_int shade)
    | Ring_inset -> 150000
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
