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
    (* Opacity *)
    | Opacity of int
    (* Rings *)
    | Ring_none
    | Ring_xs
    | Ring_sm
    | Ring_md
    | Ring_lg
    | Ring_xl
    | Ring_inset
    | Ring_color of Color.color * int
    (* Transitions *)
    | Transition_none
    | Transition_all
    | Transition_colors
    | Transition_opacity
    | Transition_shadow
    | Transition_transform
    (* Duration *)
    | Duration of int
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

  type Utility.base += Self of t

  let name = "effects"
  let priority = 17

  (* Shadow variables with property registration - using convert for type
     compatibility *)
  let shadow_var =
    Var.property_default Css.Shadow
      ~initial:
        (shadow ~h_offset:(Px 0.0) ~v_offset:(Px 0.0) ~color:(Css.hex "#0000")
           ())
      ~universal:true "tw-shadow"

  let shadow_alpha_var =
    Var.property_default Css.Float ~initial:100.0 "tw-shadow-alpha"

  let inset_shadow_color_var =
    Var.channel ~needs_property:true Css.Color "tw-inset-shadow-color"

  let inset_shadow_alpha_var =
    Var.property_default Css.Float ~initial:100.0 "tw-inset-shadow-alpha"

  let shadow_color_var =
    Var.channel ~needs_property:true Css.Color "tw-shadow-color"

  let inset_shadow_var =
    Var.property_default Css.Shadow
      ~initial:
        (shadow ~h_offset:(Px 0.0) ~v_offset:(Px 0.0) ~color:(Css.hex "#0000")
           ())
      ~universal:true "tw-inset-shadow"

  (* Ring variables *)
  let ring_color_var =
    Var.channel ~needs_property:true Css.Color "tw-ring-color"

  let inset_ring_color_var =
    Var.channel ~needs_property:true Css.Color "tw-inset-ring-color"

  let ring_inset_var =
    Var.channel ~needs_property:true Css.String "tw-ring-inset"

  let ring_shadow_var =
    Var.property_default Css.Shadow
      ~initial:
        (shadow ~h_offset:(Px 0.0) ~v_offset:(Px 0.0) ~color:(Css.hex "#0000")
           ())
      ~universal:true "tw-ring-shadow"

  let inset_ring_shadow_var =
    Var.property_default Css.Shadow
      ~initial:
        (shadow ~h_offset:(Px 0.0) ~v_offset:(Px 0.0) ~color:(Css.hex "#0000")
           ())
      ~universal:true "tw-inset-ring-shadow"

  let ring_offset_shadow_var =
    Var.property_default Css.Shadow
      ~initial:
        (shadow ~h_offset:(Px 0.0) ~v_offset:(Px 0.0) ~color:(Css.hex "#0000")
           ())
      ~universal:true "tw-ring-offset-shadow"

  let ring_offset_color_var =
    Var.property_default Css.Color ~initial:(Css.hex "#fff") ~universal:true
      "tw-ring-offset-color"

  let ring_offset_width_var =
    Var.property_default Css.Length
      ~initial:(Zero : Css.length)
      "tw-ring-offset-width"

  let ring_width_var = Var.channel Css.Length "tw-ring-width"

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

  type ring_width = [ `None | `Xs | `Sm | `Md | `Lg | `Xl ]

  let ring_internal (w : ring_width) =
    let width =
      match w with
      | `None -> "0"
      | `Xs -> "1px"
      | `Sm -> "2px"
      | `Md -> "3px"
      | `Lg -> "4px"
      | `Xl -> "8px"
    in
    let width_len : length =
      match width with
      | "0" -> Zero
      | "1px" -> Px 1.
      | "2px" -> Px 2.
      | "4px" -> Px 4.
      | "8px" -> Px 8.
      | _ -> Px 3.
    in
    let d_ring_color, ring_color_ref =
      Var.binding ring_color_var (Css.hex "#0000001a")
    in
    let ring_shadow_value =
      Css.shadow ~h_offset:Zero ~v_offset:Zero ~spread:width_len
        ~color:(Var ring_color_ref) ()
    in

    (* Create box-shadow using CSS variable composition *)
    let d_inset, v_inset =
      Var.binding inset_shadow_var
        (Css.shadow ~h_offset:Zero ~v_offset:Zero ~color:(Css.hex "#0000") ())
    in
    let d_inset_ring, v_inset_ring =
      Var.binding inset_ring_shadow_var
        (Css.shadow ~h_offset:Zero ~v_offset:Zero ~color:(Css.hex "#0000") ())
    in
    let d_ring_offset, v_ring_offset =
      Var.binding ring_offset_shadow_var
        (Css.shadow ~h_offset:Zero ~v_offset:Zero ~color:(Css.hex "#0000") ())
    in
    let d_ring, v_ring = Var.binding ring_shadow_var ring_shadow_value in
    let d_shadow, v_shadow =
      Var.binding shadow_var
        (Css.shadow ~h_offset:Zero ~v_offset:Zero ~color:(Css.hex "#0000") ())
    in
    let box_shadow_vars : Css.shadow list =
      [
        Css.Var v_inset;
        Css.Var v_inset_ring;
        Css.Var v_ring_offset;
        Css.Var v_ring;
        Css.Var v_shadow;
      ]
    in

    let d_width, _ = Var.binding ring_width_var width_len in
    style
      (d_ring_color :: d_width :: d_inset :: d_inset_ring :: d_ring_offset
     :: d_ring :: d_shadow
      :: [ Css.box_shadows box_shadow_vars ])

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
    let color_value = Color.to_css color shade in
    let d, _ = Var.binding ring_color_var color_value in
    style (d :: [])

  let transition_none =
    style
      [
        transition
          (Shorthand
             {
               property = None;
               duration = Some (S 0.0);
               timing_function = None;
               delay = None;
             });
      ]

  let transition_all =
    style
      [
        transition
          (Shorthand
             {
               property = All;
               duration = Some (Ms 150.);
               timing_function = Some (Cubic_bezier (0.4, 0.0, 0.2, 1.0));
               delay = None;
             });
      ]

  let transition_colors =
    style
      [
        Css.transitions
          [
            Shorthand
              {
                property = Property "background-color";
                duration = Some (Ms 150.);
                timing_function = Some (Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
            Shorthand
              {
                property = Property "border-color";
                duration = Some (Ms 150.);
                timing_function = Some (Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
            Shorthand
              {
                property = Property "color";
                duration = Some (Ms 150.);
                timing_function = Some (Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
            Shorthand
              {
                property = Property "fill";
                duration = Some (Ms 150.);
                timing_function = Some (Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
            Shorthand
              {
                property = Property "stroke";
                duration = Some (Ms 150.);
                timing_function = Some (Cubic_bezier (0.4, 0.0, 0.2, 1.0));
                delay = None;
              };
          ];
      ]

  let transition_opacity =
    style
      [
        transition
          (Shorthand
             {
               property = Property "opacity";
               duration = Some (Ms 150.);
               timing_function = Some (Cubic_bezier (0.4, 0.0, 0.2, 1.0));
               delay = None;
             });
      ]

  let transition_shadow =
    style
      [
        transition
          (Shorthand
             {
               property = Property "box-shadow";
               duration = Some (Ms 150.);
               timing_function = Some (Cubic_bezier (0.4, 0.0, 0.2, 1.0));
               delay = None;
             });
      ]

  let transition_transform =
    style
      [
        transition
          (Shorthand
             {
               property = Property "transform";
               duration = Some (Ms 150.);
               timing_function = Some (Cubic_bezier (0.4, 0.0, 0.2, 1.0));
               delay = None;
             });
      ]

  let duration n =
    let seconds = float_of_int n /. 1000.0 in
    style [ transition_duration (S seconds) ]

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
    | Opacity n -> opacity n
    | Ring_none -> ring_none
    | Ring_xs -> ring_xs
    | Ring_sm -> ring_sm
    | Ring_md -> ring_md
    | Ring_lg -> ring_lg
    | Ring_xl -> ring_xl
    | Ring_inset -> ring_inset
    | Ring_color (color, shade) -> ring_color color shade
    | Transition_none -> transition_none
    | Transition_all -> transition_all
    | Transition_colors -> transition_colors
    | Transition_opacity -> transition_opacity
    | Transition_shadow -> transition_shadow
    | Transition_transform -> transition_transform
    | Duration n -> duration n
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
    | [ "transition" ] -> Ok Transition_all
    | [ "transition"; "none" ] -> Ok Transition_none
    | [ "transition"; "all" ] -> Ok Transition_all
    | [ "transition"; "colors" ] -> Ok Transition_colors
    | [ "transition"; "opacity" ] -> Ok Transition_opacity
    | [ "transition"; "shadow" ] -> Ok Transition_shadow
    | [ "transition"; "transform" ] -> Ok Transition_transform
    | [ "duration"; n ] ->
        Parse.int_pos ~name:"duration" n >|= fun n -> Duration n
    | [ "mix"; "blend"; "normal" ] -> Ok Mix_blend_normal
    | [ "mix"; "blend"; "multiply" ] -> Ok Mix_blend_multiply
    | [ "mix"; "blend"; "screen" ] -> Ok Mix_blend_screen
    | [ "mix"; "blend"; "overlay" ] -> Ok Mix_blend_overlay
    | [ "mix"; "blend"; "darken" ] -> Ok Mix_blend_darken
    | [ "mix"; "blend"; "lighten" ] -> Ok Mix_blend_lighten
    | [ "mix"; "blend"; "color-dodge" ] -> Ok Mix_blend_color_dodge
    | [ "mix"; "blend"; "color-burn" ] -> Ok Mix_blend_color_burn
    | [ "mix"; "blend"; "hard-light" ] -> Ok Mix_blend_hard_light
    | [ "mix"; "blend"; "soft-light" ] -> Ok Mix_blend_soft_light
    | [ "mix"; "blend"; "difference" ] -> Ok Mix_blend_difference
    | [ "mix"; "blend"; "exclusion" ] -> Ok Mix_blend_exclusion
    | [ "mix"; "blend"; "hue" ] -> Ok Mix_blend_hue
    | [ "mix"; "blend"; "saturation" ] -> Ok Mix_blend_saturation
    | [ "mix"; "blend"; "color" ] -> Ok Mix_blend_color
    | [ "mix"; "blend"; "luminosity" ] -> Ok Mix_blend_luminosity
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
    | Opacity n -> "opacity-" ^ string_of_int n
    | Ring_none -> "ring-0"
    | Ring_xs -> "ring-1"
    | Ring_sm -> "ring-2"
    | Ring_md -> "ring"
    | Ring_lg -> "ring-4"
    | Ring_xl -> "ring-8"
    | Ring_inset -> "ring-inset"
    | Ring_color (color, shade) ->
        "ring-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | Transition_none -> "transition-none"
    | Transition_all -> "transition-all"
    | Transition_colors -> "transition-colors"
    | Transition_opacity -> "transition-opacity"
    | Transition_shadow -> "transition-shadow"
    | Transition_transform -> "transition-transform"
    | Duration n -> "duration-" ^ string_of_int n
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

  let suborder = function
    | Opacity n -> n
    | Shadow_none -> 1001
    | Shadow -> 1000
    | Shadow_sm -> 1002
    | Shadow_md -> 1003
    | Shadow_lg -> 1004
    | Shadow_xl -> 1005
    | Shadow_2xl -> 1006
    | Shadow_inner -> 1007
    | Mix_blend_normal -> 2000
    | Mix_blend_multiply -> 2001
    | Mix_blend_screen -> 2002
    | Mix_blend_overlay -> 2003
    | Mix_blend_darken -> 2004
    | Mix_blend_lighten -> 2005
    | Mix_blend_color_dodge -> 2006
    | Mix_blend_color_burn -> 2007
    | Mix_blend_hard_light -> 2008
    | Mix_blend_soft_light -> 2009
    | Mix_blend_difference -> 2010
    | Mix_blend_exclusion -> 2011
    | Mix_blend_hue -> 2012
    | Mix_blend_saturation -> 2013
    | Mix_blend_color -> 2014
    | Mix_blend_luminosity -> 2015
    | Ring_none -> 4000
    | Ring_xs -> 4001
    | Ring_sm -> 4002
    | Ring_md -> 4003
    | Ring_lg -> 4004
    | Ring_xl -> 4005
    | Ring_inset -> 4006
    | Ring_color (color, shade) ->
        4100
        + Color.suborder_with_shade (Color.pp color ^ "-" ^ string_of_int shade)
    | Transition_none -> 5000
    | Transition_all -> 5001
    | Transition_colors -> 5002
    | Transition_opacity -> 5003
    | Transition_shadow -> 5004
    | Transition_transform -> 5005
    | Duration n -> 6000 + n
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
let ring_inset = utility Ring_inset
let ring_none = utility Ring_none
let ring_xs = utility Ring_xs
let ring_sm = utility Ring_sm
let ring = utility Ring_md
let ring_md = utility Ring_md
let ring_lg = utility Ring_lg
let ring_xl = utility Ring_xl
let ring_color color shade = utility (Ring_color (color, shade))
let transition_none = utility Transition_none
let transition_all = utility Transition_all
let transition_colors = utility Transition_colors
let transition_opacity = utility Transition_opacity
let transition_shadow = utility Transition_shadow
let transition_transform = utility Transition_transform
let duration n = utility (Duration n)
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
