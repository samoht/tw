(** Visual effects utilities for shadows, opacity, and filters

    What's included:
    - Shadows: `shadow-none/sm/(default)/md/lg/xl/2xl/inner`.
    - Opacity: [opacity-0..100].
    - Filter & backdrop-filter helpers: see Filters module for full coverage.
    - Ring and transition helpers.

    What's not:
    - Full mix-blend and backdrop-blend sets are not exposed where the typed
      `Css` API lacks variants. You can extend with `style "mix-blend-multiply"
      [Css.property "mix-blend-mode" "multiply"]`.

    Parsing contract (`of_string`):
    - Accepts `opacity-<n>` and common transition aliases. Unknown tokens yield
      `Error (`Msg "Not an effects utility")`. *)

open Core
open Css
module Parse = Parse

(** {1 Shadow and Ring Variables} *)

(* Shadow variables with property registration - using convert for type
   compatibility *)
let shadow_var =
  Var.property_default Css.Shadow
    ~initial:
      (shadow ~h_offset:(Px 0.0) ~v_offset:(Px 0.0) ~color:(Css.hex "#0000") ())
    ~universal:true "tw-shadow"

let shadow_alpha_var =
  Var.property_default Css.Float ~initial:100.0 "tw-shadow-alpha"

let inset_shadow_color_var =
  Var.property_default Css.Color ~initial:Css.Transparent ~universal:true
    "tw-inset-shadow-color"

let inset_shadow_alpha_var =
  Var.property_default Css.Float ~initial:100.0 "tw-inset-shadow-alpha"

let shadow_color_var =
  Var.property_default Css.Color ~initial:Css.Transparent ~universal:true
    "tw-shadow-color"

let inset_shadow_var =
  Var.property_default Css.Shadow
    ~initial:
      (shadow ~h_offset:(Px 0.0) ~v_offset:(Px 0.0) ~color:(Css.hex "#0000") ())
    ~universal:true "tw-inset-shadow"

(* Ring variables *)
let ring_color_var =
  Var.property_default Css.Color ~initial:Css.Transparent ~universal:true
    "tw-ring-color"

let inset_ring_color_var =
  Var.property_default Css.Color ~initial:Css.Transparent ~universal:true
    "tw-inset-ring-color"

let ring_inset_var =
  Var.property_default Css.String ~initial:"" ~universal:true "tw-ring-inset"

let ring_shadow_var =
  Var.property_default Css.Shadow
    ~initial:
      (shadow ~h_offset:(Px 0.0) ~v_offset:(Px 0.0) ~color:(Css.hex "#0000") ())
    ~universal:true "tw-ring-shadow"

let inset_ring_shadow_var =
  Var.property_default Css.Shadow
    ~initial:
      (shadow ~h_offset:(Px 0.0) ~v_offset:(Px 0.0) ~color:(Css.hex "#0000") ())
    ~universal:true "tw-inset-ring-shadow"

let ring_offset_shadow_var =
  Var.property_default Css.Shadow
    ~initial:
      (shadow ~h_offset:(Px 0.0) ~v_offset:(Px 0.0) ~color:(Css.hex "#0000") ())
    ~universal:true "tw-ring-offset-shadow"

let ring_offset_color_var =
  Var.property_default Css.Color ~initial:(Css.hex "#fff") ~universal:true
    "tw-ring-offset-color"

let ring_offset_width_var =
  Var.property_default Css.Length
    ~initial:(Zero : Css.length)
    "tw-ring-offset-width"

let ring_width_var = Var.channel Css.Length "tw-ring-width"

(** {1 Shadow Utilities} *)

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

  style "shadow-none"
    ~property_rules:(Css.concat property_rules)
    [ d_shadow; Css.box_shadow box_shadow_vars ]

let shadow_sm =
  (* Shadow-sm with composite shadows matching Tailwind v4 *)
  (* Reference color variable with fallback *)
  let color_ref =
    Var.reference shadow_color_var
      ~fallback:(Css.Fallback (Css.hex "#0000001a"))
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
  style "shadow-sm"
    ~property_rules:(Css.concat property_rules)
    (d_shadow :: [ Css.box_shadows box_shadow_vars ])

let shadow =
  (* Default shadow - same as shadow-sm in Tailwind v4 *)
  (* Reference color variable with fallback *)
  let color_ref =
    Var.reference shadow_color_var
      ~fallback:(Css.Fallback (Css.hex "#0000001a"))
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
  style "shadow"
    ~property_rules:(Css.concat property_rules)
    [ d_shadow; Css.box_shadows box_shadow_vars ]

let shadow_md =
  (* Shadow-md with CSS variable for color *)
  let color_ref =
    Var.reference shadow_color_var
      ~fallback:(Css.Fallback (Css.hex "#0000001a"))
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
  style "shadow-md"
    ~property_rules:(Css.concat property_rules)
    [ d_shadow; Css.box_shadows box_shadow_vars ]

let shadow_lg =
  (* Shadow-lg with CSS variable for color *)
  let color_ref =
    Var.reference shadow_color_var
      ~fallback:(Css.Fallback (Css.hex "#0000001a"))
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
  style "shadow-lg"
    ~property_rules:(Css.concat property_rules)
    [ d_shadow; Css.box_shadows box_shadow_vars ]

let shadow_xl =
  (* Shadow-xl with CSS variable for color *)
  let color_ref =
    Var.reference shadow_color_var
      ~fallback:(Css.Fallback (Css.hex "#0000001a"))
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
  style "shadow-xl"
    ~property_rules:(Css.concat property_rules)
    [ d_shadow; Css.box_shadows box_shadow_vars ]

let shadow_2xl =
  (* Shadow-2xl with CSS variable for color *)
  let color_ref =
    Var.reference shadow_color_var
      ~fallback:(Css.Fallback (Css.hex "#0000001a"))
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
  style "shadow-2xl"
    ~property_rules:(Css.concat property_rules)
    [ d_shadow; Css.box_shadows box_shadow_vars ]

let shadow_inner =
  (* Define inset shadow variable *)
  let inset_shadow_value =
    Css.shadow ~inset:true ~h_offset:(Px 0.) ~v_offset:(Px 2.) ~blur:(Px 4.) ()
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
  style "shadow-inner"
    (d_inset :: d_inset_ring :: d_ring_offset :: d_ring :: [ box_shadow_decl ])

(** {1 Opacity Utilities} *)

let opacity_0 = style "opacity-0" [ opacity 0.0 ]
let opacity_5 = style "opacity-5" [ opacity 0.05 ]
let opacity_10 = style "opacity-10" [ opacity 0.1 ]
let opacity_20 = style "opacity-20" [ opacity 0.2 ]
let opacity_25 = style "opacity-25" [ opacity 0.25 ]
let opacity_30 = style "opacity-30" [ opacity 0.3 ]
let opacity_40 = style "opacity-40" [ opacity 0.4 ]
let opacity_50 = style "opacity-50" [ opacity 0.5 ]
let opacity_60 = style "opacity-60" [ opacity 0.6 ]
let opacity_70 = style "opacity-70" [ opacity 0.7 ]
let opacity_75 = style "opacity-75" [ opacity 0.75 ]
let opacity_80 = style "opacity-80" [ opacity 0.8 ]
let opacity_90 = style "opacity-90" [ opacity 0.9 ]
let opacity_95 = style "opacity-95" [ opacity 0.95 ]
let opacity_100 = style "opacity-100" [ opacity 1.0 ]

(** {1 Mix Blend Mode Utilities} *)

(** {1 Ring Utilities} *)

type ring_width = [ `None | `Xs | `Sm | `Md | `Lg | `Xl ]

let ring_internal (w : ring_width) =
  let width, class_suffix =
    match w with
    | `None -> ("0", "0")
    | `Xs -> ("1px", "1")
    | `Sm -> ("2px", "2")
    | `Md -> ("3px", "")
    | `Lg -> ("4px", "4")
    | `Xl -> ("8px", "8")
  in
  let class_name =
    if class_suffix = "" then "ring" else "ring-" ^ class_suffix
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
  style class_name
    (d_ring_color :: d_width :: d_inset :: d_inset_ring :: d_ring_offset
   :: d_ring :: d_shadow
    :: [ Css.box_shadows box_shadow_vars ])

let ring_none = ring_internal `None
let ring_xs = ring_internal `Xs
let ring_sm = ring_internal `Sm
let ring = ring_internal `Md (* Default ring *)
let ring_md = ring_internal `Md
let ring_lg = ring_internal `Lg
let ring_xl = ring_internal `Xl

let ring_inset =
  (* Ring inset needs special handling - it's a string value "inset" Since we
     don't have a String kind, we'll handle this differently *)
  style "ring-inset"
    [ (* TODO: Add support for string-valued CSS variables or handle ring-inset
         specially *) ]

let ring_color color shade =
  let class_name =
    if Color.is_base_color color then
      String.concat "" [ "ring-"; Color.pp color ]
    else String.concat "" [ "ring-"; Color.pp color; "-"; string_of_int shade ]
  in
  let color_value = Color.to_css color shade in
  let d, _ = Var.binding ring_color_var color_value in
  style class_name (d :: [])

(** {1 Transition Utilities} *)

let transition_none =
  style "transition-none"
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
  style "transition-all"
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
  style "transition-colors"
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
  style "transition-opacity"
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
  style "transition-shadow"
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
  style "transition-transform"
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
  let class_name = "duration-" ^ string_of_int n in
  style class_name [ transition_duration (Ms (float_of_int n)) ]

(** {1 Opacity Utility} *)

let opacity n =
  let class_name = "opacity-" ^ string_of_int n in
  let value = float_of_int n /. 100.0 in
  style class_name [ opacity value ]

(** {1 Mix Blend Mode Utilities} *)

let mix_blend_normal = style "mix-blend-normal" [ mix_blend_mode Normal ]
let mix_blend_multiply = style "mix-blend-multiply" [ mix_blend_mode Multiply ]
let mix_blend_screen = style "mix-blend-screen" [ mix_blend_mode Screen ]
let mix_blend_overlay = style "mix-blend-overlay" [ mix_blend_mode Overlay ]
let mix_blend_darken = style "mix-blend-darken" [ mix_blend_mode Darken ]
let mix_blend_lighten = style "mix-blend-lighten" [ mix_blend_mode Lighten ]

let mix_blend_color_dodge =
  style "mix-blend-color-dodge" [ mix_blend_mode Color_dodge ]

let mix_blend_color_burn =
  style "mix-blend-color-burn" [ mix_blend_mode Color_burn ]

let mix_blend_hard_light =
  style "mix-blend-hard-light" [ mix_blend_mode Hard_light ]

let mix_blend_soft_light =
  style "mix-blend-soft-light" [ mix_blend_mode Soft_light ]

let mix_blend_difference =
  style "mix-blend-difference" [ mix_blend_mode Difference ]

let mix_blend_exclusion =
  style "mix-blend-exclusion" [ mix_blend_mode Exclusion ]

let mix_blend_hue = style "mix-blend-hue" [ mix_blend_mode Hue ]

let mix_blend_saturation =
  style "mix-blend-saturation" [ mix_blend_mode Saturation ]

let mix_blend_color = style "mix-blend-color" [ mix_blend_mode Color ]

let mix_blend_luminosity =
  style "mix-blend-luminosity" [ mix_blend_mode Luminosity ]

(** {1 Parsing Functions} *)

let ( >|= ) = Parse.( >|= )

let of_string = function
  | [ "shadow"; "none" ] -> Ok shadow_none
  | [ "shadow"; "sm" ] -> Ok shadow_sm
  | [ "shadow" ] -> Ok shadow
  | [ "shadow"; "md" ] -> Ok shadow_md
  | [ "shadow"; "lg" ] -> Ok shadow_lg
  | [ "shadow"; "xl" ] -> Ok shadow_xl
  | [ "shadow"; "2xl" ] -> Ok shadow_2xl
  | [ "shadow"; "inner" ] -> Ok shadow_inner
  | [ "opacity"; n ] ->
      Parse.int_bounded ~name:"opacity" ~min:0 ~max:100 n >|= opacity
  | [ "ring" ] -> Ok ring
  | [ "ring"; "0" ] -> Ok ring_none
  | [ "ring"; "1" ] -> Ok ring_xs
  | [ "ring"; "2" ] -> Ok ring_sm
  | [ "ring"; "3" ] -> Ok ring_md
  | [ "ring"; "4" ] -> Ok ring_lg
  | [ "ring"; "8" ] -> Ok ring_xl
  | [ "ring"; "inset" ] -> Ok ring_inset
  | [ "transition" ] -> Ok transition_all
  | [ "transition"; "none" ] -> Ok transition_none
  | [ "transition"; "all" ] -> Ok transition_all
  | [ "transition"; "colors" ] -> Ok transition_colors
  | [ "transition"; "opacity" ] -> Ok transition_opacity
  | [ "transition"; "shadow" ] -> Ok transition_shadow
  | [ "transition"; "transform" ] -> Ok transition_transform
  | [ "duration"; n ] -> Parse.int_pos ~name:"duration" n >|= duration
  | [ "mix"; "blend"; "normal" ] -> Ok mix_blend_normal
  | [ "mix"; "blend"; "multiply" ] -> Ok mix_blend_multiply
  | [ "mix"; "blend"; "screen" ] -> Ok mix_blend_screen
  | [ "mix"; "blend"; "overlay" ] -> Ok mix_blend_overlay
  | [ "mix"; "blend"; "darken" ] -> Ok mix_blend_darken
  | [ "mix"; "blend"; "lighten" ] -> Ok mix_blend_lighten
  | [ "mix"; "blend"; "color-dodge" ] -> Ok mix_blend_color_dodge
  | [ "mix"; "blend"; "color-burn" ] -> Ok mix_blend_color_burn
  | [ "mix"; "blend"; "hard-light" ] -> Ok mix_blend_hard_light
  | [ "mix"; "blend"; "soft-light" ] -> Ok mix_blend_soft_light
  | [ "mix"; "blend"; "difference" ] -> Ok mix_blend_difference
  | [ "mix"; "blend"; "exclusion" ] -> Ok mix_blend_exclusion
  | [ "mix"; "blend"; "hue" ] -> Ok mix_blend_hue
  | [ "mix"; "blend"; "saturation" ] -> Ok mix_blend_saturation
  | [ "mix"; "blend"; "color" ] -> Ok mix_blend_color
  | [ "mix"; "blend"; "luminosity" ] -> Ok mix_blend_luminosity
  | _ -> Error (`Msg "Not an effects utility")
