(** Background and gradient utilities

    What's included:
    - `bg-gradient-to-*` - Linear gradient direction utilities.
    - `from-*` - Gradient starting color with optional opacity.
    - `via-*` - Gradient middle color with optional opacity.
    - `to-*` - Gradient ending color with optional opacity.

    What's not:
    - Radial or conic gradients.
    - Multiple gradient stops beyond from/via/to.
    - Background size, position, repeat utilities.

    Parsing contract (`of_string`):
    - Accepts ["bg"; "gradient"; "to"; direction], ["from"; color; shade],
      ["via"; color; shade], ["to"; color; shade]. Unknown tokens yield `Error
      (`Msg "Unknown background class")`. *)

type direction =
  | Bottom
  | Bottom_right
  | Right
  | Top_right
  | Top
  | Top_left
  | Left
  | Bottom_left

module Handler = struct
  (* Helper to check if a string contains an opacity modifier *)
  let has_opacity s = String.contains s '/'

  type t =
    | Bg of Color.color * int
    | Bg_gradient_to of direction
    | From of Color.color * int
    | From_opacity of Color.color * int * Color.opacity_modifier
    | Via of Color.color * int
    | Via_opacity of Color.color * int * Color.opacity_modifier
    | To of Color.color * int
    | To_opacity of Color.color * int * Color.opacity_modifier
    | Bg_origin_border
    | Bg_origin_padding
    | Bg_origin_content
    | Bg_clip_border
    | Bg_clip_padding
    | Bg_clip_content
    | Bg_clip_text

  type Utility.base += Self of t

  (* Format opacity modifier for class names *)
  let opacity_suffix = function
    | Color.No_opacity -> ""
    | Color.Opacity_percent p ->
        if Float.is_integer p then Printf.sprintf "/%d" (int_of_float p)
        else Printf.sprintf "/%g" p
    | Color.Opacity_arbitrary f -> Printf.sprintf "/[%g]" f
    | Color.Opacity_named name -> "/" ^ name

  let to_class (t : t) =
    match t with
    | Bg (color, shade) ->
        if Color.is_base_color color || Color.is_custom_color color then
          "bg-" ^ Color.pp color
        else "bg-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | Bg_gradient_to dir -> (
        match dir with
        | Bottom -> "bg-gradient-to-b"
        | Bottom_right -> "bg-gradient-to-br"
        | Right -> "bg-gradient-to-r"
        | Top_right -> "bg-gradient-to-tr"
        | Top -> "bg-gradient-to-t"
        | Top_left -> "bg-gradient-to-tl"
        | Left -> "bg-gradient-to-l"
        | Bottom_left -> "bg-gradient-to-bl")
    | From (color, shade) ->
        if Color.is_base_color color || Color.is_custom_color color then
          "from-" ^ Color.pp color
        else "from-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | From_opacity (color, shade, opacity) ->
        let base =
          if Color.is_base_color color || Color.is_custom_color color then
            "from-" ^ Color.pp color
          else "from-" ^ Color.pp color ^ "-" ^ string_of_int shade
        in
        base ^ opacity_suffix opacity
    | Via (color, shade) ->
        if Color.is_base_color color || Color.is_custom_color color then
          "via-" ^ Color.pp color
        else "via-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | Via_opacity (color, shade, opacity) ->
        let base =
          if Color.is_base_color color || Color.is_custom_color color then
            "via-" ^ Color.pp color
          else "via-" ^ Color.pp color ^ "-" ^ string_of_int shade
        in
        base ^ opacity_suffix opacity
    | To (color, shade) ->
        if Color.is_base_color color || Color.is_custom_color color then
          "to-" ^ Color.pp color
        else "to-" ^ Color.pp color ^ "-" ^ string_of_int shade
    | To_opacity (color, shade, opacity) ->
        let base =
          if Color.is_base_color color || Color.is_custom_color color then
            "to-" ^ Color.pp color
          else "to-" ^ Color.pp color ^ "-" ^ string_of_int shade
        in
        base ^ opacity_suffix opacity
    | Bg_origin_border -> "bg-origin-border"
    | Bg_origin_padding -> "bg-origin-padding"
    | Bg_origin_content -> "bg-origin-content"
    | Bg_clip_border -> "bg-clip-border"
    | Bg_clip_padding -> "bg-clip-padding"
    | Bg_clip_content -> "bg-clip-content"
    | Bg_clip_text -> "bg-clip-text"

  let to_spec (dir : direction) : Css.gradient_direction =
    match dir with
    | Bottom -> To_bottom
    | Bottom_right -> To_bottom_right
    | Right -> To_right
    | Top_right -> To_top_right
    | Top -> To_top
    | Top_left -> To_top_left
    | Left -> To_left
    | Bottom_left -> To_bottom_left

  open Style
  open Css

  let name = "backgrounds"
  let priority = 20

  (* Gradient variables with proper @property definitions matching Tailwind v4.
     Order in @layer properties: translate (0-2), scale (3-5), border-style (6),
     gradient (7-15), font-weight (16), shadows (17-22), rings (23-30),
     animation (31-32). *)
  let gradient_position_var =
    (* The gradient direction. Use To_bottom as a sentinel so @property
       initial-value is omitted, matching Tailwind's "initial" in the properties
       layer. Put before --tw-gradient-from. *)
    Var.property_default Gradient_direction ~initial:To_bottom ~universal:true
      ~property_order:7 ~family:`Gradient "tw-gradient-position"

  let gradient_from_var =
    Var.property_default Color ~initial:(Css.hex "#0000") ~property_order:8
      ~family:`Gradient "tw-gradient-from"

  let gradient_via_var =
    Var.property_default Color ~initial:(Css.hex "#0000") ~property_order:9
      ~family:`Gradient "tw-gradient-via"

  let gradient_to_var =
    Var.property_default Color ~initial:(Css.hex "#0000") ~property_order:10
      ~family:`Gradient "tw-gradient-to"

  let gradient_stops_var =
    Var.property_default Gradient_stop ~initial:(List []) ~universal:true
      ~property_order:11 ~family:`Gradient "tw-gradient-stops"

  let gradient_via_stops_var =
    Var.channel ~needs_property:true ~property_order:12 ~family:`Gradient
      Gradient_stop "tw-gradient-via-stops"

  let gradient_from_position_var =
    Var.property_default Percentage ~initial:(Pct 0.) ~property_order:13
      ~family:`Gradient "tw-gradient-from-position"

  let gradient_via_position_var =
    Var.property_default Percentage ~initial:(Pct 50.) ~property_order:14
      ~family:`Gradient "tw-gradient-via-position"

  let gradient_to_position_var =
    Var.property_default Percentage ~initial:(Pct 100.) ~property_order:15
      ~family:`Gradient "tw-gradient-to-position"

  let bg_gradient_to' dir =
    (* Set --tw-gradient-position to the typed direction with oklab
       interpolation *)
    let dir_val = With_interpolation (to_spec dir, In_oklab) in
    let d_position, _ = Var.binding gradient_position_var dir_val in
    (* Reference --tw-gradient-stops for linear-gradient *)
    let stops_ref = Var.reference gradient_stops_var in
    (* Include property rules for the variables *)
    let property_rules =
      [
        Var.property_rule gradient_position_var;
        Var.property_rule gradient_stops_var;
      ]
      |> List.filter_map (fun x -> x)
      |> Css.concat
    in
    style ~property_rules
      [ d_position; Css.background_image (Linear_gradient_var stops_ref) ]

  (** Helper to get color value and optional theme variable declaration. For
      custom/arbitrary colors: returns ([], color_value) - no theme variable.
      For named colors: returns ([theme_var_decl], Var(theme_var_ref)) *)
  let color_binding ?(shade = 500) color =
    let color_value = Color.to_css color shade in
    if Color.is_custom_color color then
      (* Arbitrary color: no theme variable, use value directly *)
      ([], color_value)
    else
      (* Named color: create theme variable *)
      let color_theme_var = Color.get_color_var color shade in
      let d_color, color_ref = Var.binding color_theme_var color_value in
      ([ d_color ], (Var color_ref : Css.color))

  (** Common helper for gradient color utilities *)
  let gradient_color ~prefix ~set_var ?(shade = 500) color =
    let theme_decls, gradient_color_value = color_binding ~shade color in

    (* Set the appropriate gradient variable *)
    let d_var, _ = Var.binding set_var gradient_color_value in

    (* Build variable references for gradient stops *)
    let position_ref = Var.reference gradient_position_var in
    let from_ref = Var.reference gradient_from_var in
    let from_pos_ref = Var.reference gradient_from_position_var in
    let to_ref = Var.reference gradient_to_var in
    let to_pos_ref = Var.reference gradient_to_position_var in

    (* Build the fallback gradient stop list (without via) *)
    let fallback_stops : Css.gradient_stop =
      List
        [
          Direction (Var position_ref);
          Color_percentage (Var from_ref, Some (Var from_pos_ref), None);
          Color_percentage (Var to_ref, Some (Var to_pos_ref), None);
        ]
    in

    (* Handle via-specific logic *)
    let d_stops, d_via_stops_opt =
      match prefix with
      | "via-" ->
          (* For via, build complete stop list with via in the middle *)
          let via_ref = Var.reference gradient_via_var in
          let via_pos_ref = Var.reference gradient_via_position_var in
          let via_stop_list : Css.gradient_stop =
            List
              [
                Direction (Var position_ref);
                Color_percentage (Var from_ref, Some (Var from_pos_ref), None);
                Color_percentage (Var via_ref, Some (Var via_pos_ref), None);
                Color_percentage (Var to_ref, Some (Var to_pos_ref), None);
              ]
          in
          let d_via_stops, via_stops_ref =
            Var.binding gradient_via_stops_var via_stop_list
          in
          let d_stops_via, _ =
            Var.binding gradient_stops_var (Var via_stops_ref)
          in
          (d_stops_via, Some d_via_stops)
      | _ ->
          (* For from/to, reference via-stops with fallback *)
          let via_stops_ref =
            Var.reference_with_fallback gradient_via_stops_var fallback_stops
          in
          let d_stops, _ = Var.binding gradient_stops_var (Var via_stops_ref) in
          (d_stops, None)
    in

    (* Generate @property rules for all gradient variables *)
    let property_rules =
      [
        Var.property_rule gradient_position_var;
        Var.property_rule gradient_from_var;
        Var.property_rule gradient_via_var;
        Var.property_rule gradient_to_var;
        Var.property_rule gradient_stops_var;
        Var.property_rule gradient_via_stops_var;
        Var.property_rule gradient_from_position_var;
        Var.property_rule gradient_via_position_var;
        Var.property_rule gradient_to_position_var;
      ]
      |> List.filter_map (fun x -> x)
      |> Css.concat
    in

    (* Build declarations list *)
    let base_declarations = theme_decls @ [ d_var ] in
    let declarations =
      match d_via_stops_opt with
      | Some d_via_stops -> base_declarations @ [ d_via_stops; d_stops ]
      | None -> base_declarations @ [ d_stops ]
    in

    style ~property_rules declarations

  let from_color' ?(shade = 500) color =
    gradient_color ~prefix:"from-" ~set_var:gradient_from_var ~shade color

  let via_color' ?(shade = 500) color =
    gradient_color ~prefix:"via-" ~set_var:gradient_via_var ~shade color

  let to_color' ?(shade = 500) color =
    gradient_color ~prefix:"to-" ~set_var:gradient_to_var ~shade color

  let bg' ?(shade = 500) color =
    let theme_decls, color_value = color_binding ~shade color in
    style (theme_decls @ [ Css.background_color color_value ])

  let bg_origin_border = style [ Css.background_origin Border_box ]
  let bg_origin_padding = style [ Css.background_origin Padding_box ]
  let bg_origin_content = style [ Css.background_origin Content_box ]
  let bg_clip_border = style [ Css.background_clip Border_box ]
  let bg_clip_padding = style [ Css.background_clip Padding_box ]
  let bg_clip_content = style [ Css.background_clip Content_box ]

  let bg_clip_text =
    style [ Css.webkit_background_clip Text; Css.background_clip Text ]

  (* Gradient color with opacity - generates same structure as Tailwind: 1.
     Fallback rule with hex alpha (for scheme colors) 2. @supports block with
     color-mix using theme variable 3. Separate rule with --tw-gradient-stops *)
  let gradient_color_opacity ~prefix ~set_var ?(shade = 500) color opacity =
    let percent = Color.opacity_to_percent opacity in
    let color_name = Color.scheme_color_name color shade in
    let scheme = Color.get_current_scheme () in

    (* Build variable references for gradient stops *)
    let position_ref = Var.reference gradient_position_var in
    let from_ref = Var.reference gradient_from_var in
    let from_pos_ref = Var.reference gradient_from_position_var in
    let to_ref = Var.reference gradient_to_var in
    let to_pos_ref = Var.reference gradient_to_position_var in

    (* Build the fallback gradient stop list (without via) *)
    let fallback_stops : Css.gradient_stop =
      List
        [
          Direction (Var position_ref);
          Color_percentage (Var from_ref, Some (Var from_pos_ref), None);
          Color_percentage (Var to_ref, Some (Var to_pos_ref), None);
        ]
    in

    (* Handle via-specific logic for stops *)
    let d_stops, d_via_stops_opt =
      match prefix with
      | "via-" ->
          let via_ref = Var.reference gradient_via_var in
          let via_pos_ref = Var.reference gradient_via_position_var in
          let via_stop_list : Css.gradient_stop =
            List
              [
                Direction (Var position_ref);
                Color_percentage (Var from_ref, Some (Var from_pos_ref), None);
                Color_percentage (Var via_ref, Some (Var via_pos_ref), None);
                Color_percentage (Var to_ref, Some (Var to_pos_ref), None);
              ]
          in
          let d_via_stops, via_stops_ref =
            Var.binding gradient_via_stops_var via_stop_list
          in
          let d_stops_via, _ =
            Var.binding gradient_stops_var (Var via_stops_ref)
          in
          (d_stops_via, Some d_via_stops)
      | _ ->
          let via_stops_ref =
            Var.reference_with_fallback gradient_via_stops_var fallback_stops
          in
          let d_stops, _ = Var.binding gradient_stops_var (Var via_stops_ref) in
          (d_stops, None)
    in

    (* Generate @property rules *)
    let property_rules =
      [
        Var.property_rule gradient_position_var;
        Var.property_rule gradient_from_var;
        Var.property_rule gradient_via_var;
        Var.property_rule gradient_to_var;
        Var.property_rule gradient_stops_var;
        Var.property_rule gradient_via_stops_var;
        Var.property_rule gradient_from_position_var;
        Var.property_rule gradient_via_position_var;
        Var.property_rule gradient_to_position_var;
      ]
      |> List.filter_map (fun x -> x)
      |> Css.concat
    in

    match Scheme.get_hex_color scheme color_name with
    | Some hex_value ->
        (* Scheme color: generate fallback + @supports + stops (same as
           Tailwind) Tailwind outputs: 1. .from-X/N { --tw-gradient-from:
           #RRGGBBAA } 2. @supports { .from-X/N { --tw-gradient-from:
           color-mix(...) } } 3. .from-X/N { --tw-gradient-stops: ... } To
           match, we put fallback in props, @supports in rules, and stops as
           separate rule in rules. *)
        let hex_alpha = Color.hex_with_alpha hex_value percent in
        let d_fallback, _ = Var.binding set_var (Css.hex hex_alpha) in

        (* Theme variable for @supports block *)
        let color_var = Color.get_color_var color shade in
        let theme_decl, color_ref = Var.binding color_var (Css.hex hex_value) in
        let oklab_color =
          Css.color_mix ~in_space:Oklab (Css.Var color_ref) Css.Transparent
            ~percent1:percent
        in
        let d_oklab, _ = Var.binding set_var oklab_color in

        (* Build @supports block with placeholder selector *)
        let supports_rule =
          Css.supports ~condition:Color.color_mix_supports_condition
            [
              Css.rule ~selector:(Css.Selector.class_ "_")
                [ theme_decl; d_oklab ];
            ]
        in

        (* Build stops rule with placeholder selector (will be replaced) *)
        let stops_decls =
          match d_via_stops_opt with
          | Some d_via_stops -> [ d_via_stops; d_stops ]
          | None -> [ d_stops ]
        in
        let stops_rule =
          Css.rule ~selector:(Css.Selector.class_ "_") stops_decls
        in

        (* Props has fallback, rules has @supports then stops *)
        style ~property_rules
          ~rules:(Some [ supports_rule; stops_rule ])
          [ d_fallback ]
    | None ->
        (* Non-scheme color: use color-mix directly *)
        let oklch = Color.to_oklch color shade in
        let color_value =
          Css.color_mix ~in_space:Oklab
            (Css.oklch oklch.l oklch.c oklch.h)
            Css.Transparent ~percent1:percent
        in
        let d_var, _ = Var.binding set_var color_value in
        let declarations =
          match d_via_stops_opt with
          | Some d_via_stops -> [ d_var; d_via_stops; d_stops ]
          | None -> [ d_var; d_stops ]
        in
        style ~property_rules declarations

  let from_color_opacity' ?(shade = 500) color opacity =
    gradient_color_opacity ~prefix:"from-" ~set_var:gradient_from_var ~shade
      color opacity

  let via_color_opacity' ?(shade = 500) color opacity =
    gradient_color_opacity ~prefix:"via-" ~set_var:gradient_via_var ~shade color
      opacity

  let to_color_opacity' ?(shade = 500) color opacity =
    gradient_color_opacity ~prefix:"to-" ~set_var:gradient_to_var ~shade color
      opacity

  let to_style = function
    | Bg (color, shade) -> bg' ~shade color
    | Bg_gradient_to dir -> bg_gradient_to' dir
    | From (color, shade) -> from_color' ~shade color
    | From_opacity (color, shade, opacity) ->
        from_color_opacity' ~shade color opacity
    | Via (color, shade) -> via_color' ~shade color
    | Via_opacity (color, shade, opacity) ->
        via_color_opacity' ~shade color opacity
    | To (color, shade) -> to_color' ~shade color
    | To_opacity (color, shade, opacity) ->
        to_color_opacity' ~shade color opacity
    | Bg_origin_border -> bg_origin_border
    | Bg_origin_padding -> bg_origin_padding
    | Bg_origin_content -> bg_origin_content
    | Bg_clip_border -> bg_clip_border
    | Bg_clip_padding -> bg_clip_padding
    | Bg_clip_content -> bg_clip_content
    | Bg_clip_text -> bg_clip_text

  let suborder = function
    (* Tailwind order: solid bg-colors before gradient utilities *)
    | Bg (color, shade) ->
        Color.suborder_with_shade (Color.pp color ^ "-" ^ string_of_int shade)
    | Bg_gradient_to _ -> 100000
    | From (color, shade) | From_opacity (color, shade, _) ->
        110000
        + Color.suborder_with_shade (Color.pp color ^ "-" ^ string_of_int shade)
    | Via (color, shade) | Via_opacity (color, shade, _) ->
        120000
        + Color.suborder_with_shade (Color.pp color ^ "-" ^ string_of_int shade)
    | To (color, shade) | To_opacity (color, shade, _) ->
        130000
        + Color.suborder_with_shade (Color.pp color ^ "-" ^ string_of_int shade)
    (* bg-origin utilities - alphabetical: border, content, padding *)
    | Bg_origin_border -> 140000
    | Bg_origin_content -> 140001
    | Bg_origin_padding -> 140002
    (* bg-clip utilities - alphabetical: border, content, padding, text *)
    | Bg_clip_border -> 150000
    | Bg_clip_content -> 150001
    | Bg_clip_padding -> 150002
    | Bg_clip_text -> 150003

  let of_class class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "bg"; "gradient"; "to"; "b" ] -> Ok (Bg_gradient_to Bottom)
    | [ "bg"; "gradient"; "to"; "br" ] -> Ok (Bg_gradient_to Bottom_right)
    | [ "bg"; "gradient"; "to"; "r" ] -> Ok (Bg_gradient_to Right)
    | [ "bg"; "gradient"; "to"; "tr" ] -> Ok (Bg_gradient_to Top_right)
    | [ "bg"; "gradient"; "to"; "t" ] -> Ok (Bg_gradient_to Top)
    | [ "bg"; "gradient"; "to"; "tl" ] -> Ok (Bg_gradient_to Top_left)
    | [ "bg"; "gradient"; "to"; "l" ] -> Ok (Bg_gradient_to Left)
    | [ "bg"; "gradient"; "to"; "bl" ] -> Ok (Bg_gradient_to Bottom_left)
    | [ "bg"; "origin"; "border" ] -> Ok Bg_origin_border
    | [ "bg"; "origin"; "padding" ] -> Ok Bg_origin_padding
    | [ "bg"; "origin"; "content" ] -> Ok Bg_origin_content
    | [ "bg"; "clip"; "border" ] -> Ok Bg_clip_border
    | [ "bg"; "clip"; "padding" ] -> Ok Bg_clip_padding
    | [ "bg"; "clip"; "content" ] -> Ok Bg_clip_content
    | [ "bg"; "clip"; "text" ] -> Ok Bg_clip_text
    | "bg" :: rest -> (
        match Color.shade_of_strings rest with
        | Ok (color, shade) -> Ok (Bg (color, shade))
        | Error _ -> Error (`Msg "Invalid background color"))
    | "from" :: rest when List.exists has_opacity rest -> (
        match Color.shade_and_opacity_of_strings rest with
        | Ok (color, shade, opacity) ->
            Ok (From_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "from" :: rest -> (
        match Color.shade_of_strings rest with
        | Ok (color, shade) -> Ok (From (color, shade))
        | Error _ -> Error (`Msg "Invalid from color"))
    | "via" :: rest when List.exists has_opacity rest -> (
        match Color.shade_and_opacity_of_strings rest with
        | Ok (color, shade, opacity) -> Ok (Via_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "via" :: rest -> (
        match Color.shade_of_strings rest with
        | Ok (color, shade) -> Ok (Via (color, shade))
        | Error _ -> Error (`Msg "Invalid via color"))
    | "to" :: rest when List.exists has_opacity rest -> (
        match Color.shade_and_opacity_of_strings rest with
        | Ok (color, shade, opacity) -> Ok (To_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "to" :: rest -> (
        match Color.shade_of_strings rest with
        | Ok (color, shade) -> Ok (To (color, shade))
        | Error _ -> Error (`Msg "Invalid to color"))
    | _ -> Error (`Msg "Unknown background class")
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let bg color shade = utility (Bg (color, shade))
let bg_gradient_to dir = utility (Bg_gradient_to dir)
let from_color ?(shade = 500) color = utility (From (color, shade))
let via_color ?(shade = 500) color = utility (Via (color, shade))
let to_color ?(shade = 500) color = utility (To (color, shade))
