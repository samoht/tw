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

  type position_name =
    | Pos_bottom
    | Pos_bottom_left
    | Pos_bottom_right
    | Pos_center
    | Pos_left
    | Pos_left_bottom
    | Pos_left_top
    | Pos_right
    | Pos_right_bottom
    | Pos_right_top
    | Pos_top
    | Pos_top_left
    | Pos_top_right

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
    (* Background color keywords *)
    | Bg_inherit
    (* Background image *)
    | Bg_none
    (* Background size *)
    | Bg_auto
    | Bg_cover
    | Bg_contain
    (* Background attachment *)
    | Bg_fixed
    | Bg_local
    | Bg_scroll
    (* Background repeat *)
    | Bg_repeat
    | Bg_no_repeat
    | Bg_repeat_x
    | Bg_repeat_y
    | Bg_repeat_round
    | Bg_repeat_space
    (* Background position *)
    | Bg_position of position_name
    (* Bracket notation: bg-[contain], bg-[cover] → background-size *)
    | Bg_bracket_contain
    | Bg_bracket_cover
    (* Bracket notation: bg-[length:...] or bg-[size:...] → background-size *)
    | Bg_bracket_size of string
    (* Bracket notation: bg-[50%], bg-[120px], bg-[120px_120px] →
       background-position *)
    | Bg_bracket_position of string
    (* Bracket notation: bg-[position:...] → background-position *)
    | Bg_bracket_typed_position of string
    (* Bracket notation: bg-[color:var(--x)] → background-color *)
    | Bg_bracket_color_var of string
    (* Bracket notation: bg-[var(--x)] → background-color *)
    | Bg_bracket_var of string
    (* Bracket notation: bg-[image:var(--x)] → background-image *)
    | Bg_bracket_image_var of string
    (* Bracket notation: bg-[url(...)] → background-image *)
    | Bg_bracket_url of string
    (* Bracket notation: bg-[url:var(--x)] → background-image *)
    | Bg_bracket_url_var of string
    (* Bracket notation: bg-[linear-gradient(...)] → background-image *)
    | Bg_bracket_linear_gradient of string
    (* bg-linear-to-* direction utilities *)
    | Bg_linear_to of direction
    (* bg-linear-to-*/interp - direction with interpolation modifier *)
    | Bg_linear_to_interp of direction * string
    (* bg-linear-{angle} - linear gradient with angle *)
    | Bg_linear_angle of int
    (* -bg-linear-{angle} - negated angle *)
    | Bg_linear_angle_neg of int
    (* bg-linear-{angle}/interp - angle with interpolation *)
    | Bg_linear_angle_interp of int * string
    (* -bg-linear-{angle}/interp *)
    | Bg_linear_angle_neg_interp of int * string
    (* bg-linear-[value] - bracket linear gradient value *)
    | Bg_linear_bracket of string
    (* -bg-linear-[value] - negated bracket *)
    | Bg_linear_bracket_neg of string
    (* bg-conic/interp - conic gradient with interpolation *)
    | Bg_conic_interp of string
    (* bg-conic-{angle}/interp - conic with angle and interpolation *)
    | Bg_conic_angle_interp of int * string
    (* -bg-conic-{angle}/interp *)
    | Bg_conic_angle_neg_interp of int * string
    (* bg-radial/interp - radial gradient with interpolation *)
    | Bg_radial_interp of string
    (* bg-radial-[value] - bracket radial gradient value *)
    | Bg_radial_bracket of string
    (* Bracket color/var with opacity *)
    | Bg_bracket_color_var_opacity of string * Color.opacity_modifier
    | Bg_bracket_var_opacity of string * Color.opacity_modifier
    (* bg-[length:...] - explicit length prefix for bracket size *)
    | Bg_bracket_length of string
    (* bg-position-[...] bracket notation *)
    | Bg_position_bracket of string
    (* bg-size-[...] bracket notation *)
    | Bg_size_bracket of string

  type Utility.base += Self of t

  (* Format opacity modifier for class names *)
  let opacity_suffix = function
    | Color.No_opacity -> ""
    | Color.Opacity_percent p ->
        if Float.is_integer p then Printf.sprintf "/%d" (int_of_float p)
        else Printf.sprintf "/%g" p
    | Color.Opacity_bracket_percent p ->
        if Float.is_integer p then Printf.sprintf "/[%d%%]" (int_of_float p)
        else Printf.sprintf "/[%g%%]" p
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
    | Bg_inherit -> "bg-inherit"
    | Bg_none -> "bg-none"
    | Bg_auto -> "bg-auto"
    | Bg_cover -> "bg-cover"
    | Bg_contain -> "bg-contain"
    | Bg_fixed -> "bg-fixed"
    | Bg_local -> "bg-local"
    | Bg_scroll -> "bg-scroll"
    | Bg_repeat -> "bg-repeat"
    | Bg_no_repeat -> "bg-no-repeat"
    | Bg_repeat_x -> "bg-repeat-x"
    | Bg_repeat_y -> "bg-repeat-y"
    | Bg_repeat_round -> "bg-repeat-round"
    | Bg_repeat_space -> "bg-repeat-space"
    | Bg_position Pos_bottom -> "bg-bottom"
    | Bg_position Pos_bottom_left -> "bg-bottom-left"
    | Bg_position Pos_bottom_right -> "bg-bottom-right"
    | Bg_position Pos_center -> "bg-center"
    | Bg_position Pos_left -> "bg-left"
    | Bg_position Pos_left_bottom -> "bg-left-bottom"
    | Bg_position Pos_left_top -> "bg-left-top"
    | Bg_position Pos_right -> "bg-right"
    | Bg_position Pos_right_bottom -> "bg-right-bottom"
    | Bg_position Pos_right_top -> "bg-right-top"
    | Bg_position Pos_top -> "bg-top"
    | Bg_position Pos_top_left -> "bg-top-left"
    | Bg_position Pos_top_right -> "bg-top-right"
    | Bg_bracket_contain -> "bg-[contain]"
    | Bg_bracket_cover -> "bg-[cover]"
    | Bg_bracket_size v -> "bg-[size:" ^ v ^ "]"
    | Bg_bracket_length v -> "bg-[length:" ^ v ^ "]"
    | Bg_bracket_position v -> "bg-[" ^ v ^ "]"
    | Bg_bracket_typed_position v -> "bg-[position:" ^ v ^ "]"
    | Bg_bracket_color_var v -> "bg-[color:" ^ v ^ "]"
    | Bg_bracket_var v -> "bg-[" ^ v ^ "]"
    | Bg_bracket_image_var v -> "bg-[image:" ^ v ^ "]"
    | Bg_bracket_url v -> "bg-[url(" ^ v ^ ")]"
    | Bg_bracket_url_var v -> "bg-[url:" ^ v ^ "]"
    | Bg_bracket_linear_gradient v -> "bg-[" ^ v ^ "]"
    | Bg_bracket_color_var_opacity (v, opacity) ->
        "bg-[color:" ^ v ^ "]" ^ opacity_suffix opacity
    | Bg_bracket_var_opacity (v, opacity) ->
        "bg-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Bg_linear_to dir -> (
        match dir with
        | Bottom -> "bg-linear-to-b"
        | Bottom_right -> "bg-linear-to-br"
        | Right -> "bg-linear-to-r"
        | Top_right -> "bg-linear-to-tr"
        | Top -> "bg-linear-to-t"
        | Top_left -> "bg-linear-to-tl"
        | Left -> "bg-linear-to-l"
        | Bottom_left -> "bg-linear-to-bl")
    | Bg_linear_to_interp (dir, interp) ->
        let dir_s =
          match dir with
          | Bottom -> "b"
          | Bottom_right -> "br"
          | Right -> "r"
          | Top_right -> "tr"
          | Top -> "t"
          | Top_left -> "tl"
          | Left -> "l"
          | Bottom_left -> "bl"
        in
        "bg-linear-to-" ^ dir_s ^ "/" ^ interp
    | Bg_linear_angle n -> "bg-linear-" ^ string_of_int n
    | Bg_linear_angle_neg n -> "-bg-linear-" ^ string_of_int n
    | Bg_linear_angle_interp (n, interp) ->
        "bg-linear-" ^ string_of_int n ^ "/" ^ interp
    | Bg_linear_angle_neg_interp (n, interp) ->
        "-bg-linear-" ^ string_of_int n ^ "/" ^ interp
    | Bg_linear_bracket v -> "bg-linear-[" ^ v ^ "]"
    | Bg_linear_bracket_neg v -> "-bg-linear-[" ^ v ^ "]"
    | Bg_conic_interp interp -> "bg-conic/" ^ interp
    | Bg_conic_angle_interp (n, interp) ->
        "bg-conic-" ^ string_of_int n ^ "/" ^ interp
    | Bg_conic_angle_neg_interp (n, interp) ->
        "-bg-conic-" ^ string_of_int n ^ "/" ^ interp
    | Bg_radial_interp interp -> "bg-radial/" ^ interp
    | Bg_radial_bracket v -> "bg-radial-[" ^ v ^ "]"
    | Bg_position_bracket v -> "bg-position-[" ^ v ^ "]"
    | Bg_size_bracket v -> "bg-size-[" ^ v ^ "]"

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

  (** Parse a direction suffix string to a direction value *)
  let parse_direction = function
    | "b" -> Some Bottom
    | "br" -> Some Bottom_right
    | "r" -> Some Right
    | "tr" -> Some Top_right
    | "t" -> Some Top
    | "tl" -> Some Top_left
    | "l" -> Some Left
    | "bl" -> Some Bottom_left
    | _ -> None

  (** Convert direction to CSS string for raw property output *)
  let direction_to_css_string (dir : direction) =
    match dir with
    | Bottom -> "to bottom"
    | Bottom_right -> "to bottom right"
    | Right -> "to right"
    | Top_right -> "to top right"
    | Top -> "to top"
    | Top_left -> "to top left"
    | Left -> "to left"
    | Bottom_left -> "to bottom left"

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
    let bg_var_name =
      let base = Color.pp color in
      if Color.is_base_color color then "background-color-" ^ base
      else "background-color-" ^ base ^ "-" ^ string_of_int shade
    in
    match Var.get_theme_value bg_var_name with
    | Some theme_val ->
        (* Property-scoped bg color: --background-color-<name> *)
        let tv = Var.theme Css.Color bg_var_name ~order:(5, 50) in
        let d, r = Var.binding tv (Css.hex theme_val) in
        style [ d; Css.background_color (Var r) ]
    | None ->
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

  let bg_inherit' = style [ Css.background_color Inherit ]
  let bg_none' = style [ Css.background_image None ]
  let bg_auto' = style [ Css.background_size Auto ]
  let bg_cover' = style [ Css.background_size Cover ]
  let bg_contain' = style [ Css.background_size Contain ]
  let bg_fixed' = style [ Css.background_attachment Fixed ]
  let bg_local' = style [ Css.background_attachment Local ]
  let bg_scroll' = style [ Css.background_attachment Scroll ]
  let bg_repeat' = style [ Css.background_repeat Repeat ]
  let bg_no_repeat' = style [ Css.background_repeat No_repeat ]
  let bg_repeat_x' = style [ Css.background_repeat Repeat_x ]
  let bg_repeat_y' = style [ Css.background_repeat Repeat_y ]
  let bg_repeat_round' = style [ Css.background_repeat Round ]
  let bg_repeat_space' = style [ Css.background_repeat Space ]
  let bg_position' pos = style [ Css.background_position pos ]

  (* Parse a bracket size value like "120px_120px" or "120px" *)
  let parse_bracket_size inner =
    let parts =
      String.split_on_char '_' inner |> List.filter (fun s -> s <> "")
    in
    match parts with
    | [ w; h ] -> (
        (* Two values: width height *)
        let parse_len s =
          if String.ends_with ~suffix:"px" s then
            let n =
              String.sub s 0 (String.length s - 2) |> float_of_string_opt
            in
            Option.map (fun f -> (Css.Px f : Css.length)) n
          else if String.ends_with ~suffix:"%" s then
            let n =
              String.sub s 0 (String.length s - 1) |> float_of_string_opt
            in
            Option.map (fun f -> (Css.Pct f : Css.length)) n
          else if String.ends_with ~suffix:"rem" s then
            let n =
              String.sub s 0 (String.length s - 3) |> float_of_string_opt
            in
            Option.map (fun f -> (Css.Rem f : Css.length)) n
          else None
        in
        let wl = parse_len w in
        let hl = parse_len h in
        match (wl, hl) with
        | Some w, Some h -> Some (Css.background_size (Size (w, h)))
        | _ -> None)
    | [ v ] ->
        let parse_len s =
          if String.ends_with ~suffix:"px" s then
            let n =
              String.sub s 0 (String.length s - 2) |> float_of_string_opt
            in
            Option.map (fun f -> (Css.Px f : Css.background_size)) n
          else if String.ends_with ~suffix:"%" s then
            let n =
              String.sub s 0 (String.length s - 1) |> float_of_string_opt
            in
            Option.map (fun f -> (Css.Pct f : Css.background_size)) n
          else None
        in
        Option.map Css.background_size (parse_len v)
    | _ -> None

  (* Parse a bracket position value like "120px_120px" or "120px" or "50%" *)
  let parse_bracket_position inner =
    let parts =
      String.split_on_char '_' inner |> List.filter (fun s -> s <> "")
    in
    let parse_pos_val s : Css.position_value option =
      if String.ends_with ~suffix:"px" s then
        let n = String.sub s 0 (String.length s - 2) |> float_of_string_opt in
        Option.map (fun f -> (Css.XY (Px f, Px f) : Css.position_value)) n
      else if String.ends_with ~suffix:"%" s then
        let n = String.sub s 0 (String.length s - 1) |> float_of_string_opt in
        Option.map (fun f -> (Css.XY (Pct f, Pct f) : Css.position_value)) n
      else None
    in
    match parts with
    | [ x; y ] -> (
        (* Two values - but Tailwind seems to collapse to single in some
           cases *)
        let parse_len s : Css.length option =
          if String.ends_with ~suffix:"px" s then
            let n =
              String.sub s 0 (String.length s - 2) |> float_of_string_opt
            in
            Option.map (fun f -> (Css.Px f : Css.length)) n
          else if String.ends_with ~suffix:"%" s then
            let n =
              String.sub s 0 (String.length s - 1) |> float_of_string_opt
            in
            Option.map (fun f -> (Css.Pct f : Css.length)) n
          else None
        in
        let xl = parse_len x in
        let yl = parse_len y in
        match (xl, yl) with
        | Some xv, Some yv -> Some (Css.background_position [ Css.XY (xv, yv) ])
        | _ -> None)
    | [ v ] ->
        Option.map (fun pv -> Css.background_position [ pv ]) (parse_pos_val v)
    | _ -> None

  let gradient_supports_condition =
    Css.Supports.Property
      ("background-image", "linear-gradient(in lab, red, red)")

  let gradient_property_rules =
    [
      Var.property_rule gradient_position_var;
      Var.property_rule gradient_stops_var;
    ]
    |> List.filter_map (fun x -> x)
    |> Css.concat

  (** Helper: build the 3-rule pattern for gradient direction utilities. Returns
      [base_decl; @supports { interp_decl }; bg-image rule]. *)
  let gradient_direction_rules ~base_decl ~interp_decl =
    let base_rule =
      Css.rule ~selector:(Css.Selector.class_ "_") [ base_decl ]
    in
    let supports_rule =
      Css.supports ~condition:gradient_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ interp_decl ] ]
    in
    let stops_ref = Var.reference gradient_stops_var in
    let bg_image_rule =
      Css.rule ~selector:(Css.Selector.class_ "_")
        [ Css.background_image (Linear_gradient_var stops_ref) ]
    in
    [ base_rule; supports_rule; bg_image_rule ]

  (* bg-linear-to-* with @supports for color interpolation *)
  let bg_linear_to' dir =
    let dir_val = to_spec dir in
    let dir_with_interp : Css.gradient_direction =
      With_interpolation (dir_val, In_oklab)
    in
    let base_decl, _ = Var.binding gradient_position_var dir_val in
    let interp_decl, _ = Var.binding gradient_position_var dir_with_interp in
    let rules = gradient_direction_rules ~base_decl ~interp_decl in
    style ~property_rules:gradient_property_rules ~rules:(Some rules) []

  (** Convert an interpolation modifier to its CSS string. Named modifiers map
      to specific CSS values. Bracket modifiers have underscores replaced with
      spaces. *)
  let interp_to_css_string = function
    | "oklab" -> Some "in oklab"
    | "oklch" -> Some "in oklch"
    | "hsl" -> Some "in hsl"
    | "srgb" -> Some "in srgb"
    | "shorter" -> Some "in oklch shorter hue"
    | "longer" -> Some "in oklch longer hue"
    | "increasing" -> Some "in oklch increasing hue"
    | "decreasing" -> Some "in oklch decreasing hue"
    | s when String.length s > 1 && s.[0] = '[' && s.[String.length s - 1] = ']'
      ->
        let inner = String.sub s 1 (String.length s - 2) in
        Some (String.map (fun c -> if c = '_' then ' ' else c) inner)
    | _ -> None

  (** Convert a bracket gradient value to its CSS string. "125deg" → "125deg",
      "1.3rad" → "74.4845deg", "to_bottom" → "to bottom", "circle_at_center" →
      "circle at center" *)
  let bracket_value_to_css inner =
    if String.ends_with ~suffix:"rad" inner then
      let rad_s = String.sub inner 0 (String.length inner - 3) in
      match float_of_string_opt rad_s with
      | Some rad ->
          let deg = rad *. 180.0 /. Float.pi in
          Printf.sprintf "%gdeg" deg
      | None -> String.map (fun c -> if c = '_' then ' ' else c) inner
    else String.map (fun c -> if c = '_' then ' ' else c) inner

  (** bg-linear-to-*/interp - direction with specific interpolation. Uses 3-rule
      pattern: base → [@supports] → bg-image. *)
  let bg_linear_to_interp' dir interp_str =
    let dir_val = to_spec dir in
    let base_decl, _ = Var.binding gradient_position_var dir_val in
    match interp_to_css_string interp_str with
    | Some interp_css ->
        let dir_css = direction_to_css_string dir in
        let interp_decl =
          Css.custom_property ~layer:"utilities" "--tw-gradient-position"
            (dir_css ^ " " ^ interp_css)
        in
        let rules = gradient_direction_rules ~base_decl ~interp_decl in
        style ~property_rules:gradient_property_rules ~rules:(Some rules) []
    | None -> bg_linear_to' dir

  (** [bg-linear-{angle}] with [@supports] for default oklab interpolation *)
  let bg_linear_angle' angle_deg =
    let dir_val : Css.gradient_direction =
      Angle (Deg (float_of_int angle_deg))
    in
    let dir_with_interp : Css.gradient_direction =
      With_interpolation (dir_val, In_oklab)
    in
    let base_decl, _ = Var.binding gradient_position_var dir_val in
    let interp_decl, _ = Var.binding gradient_position_var dir_with_interp in
    let rules = gradient_direction_rules ~base_decl ~interp_decl in
    style ~property_rules:gradient_property_rules ~rules:(Some rules) []

  (** [-bg-linear-{angle}] - negated angle with [@supports] *)
  let bg_linear_angle_neg' angle_deg =
    let angle_calc : Css.gradient_direction =
      Angle (Calc (Expr (Val (Deg (float_of_int angle_deg)), Mul, Num (-1.0))))
    in
    let angle_calc_interp : Css.gradient_direction =
      With_interpolation (angle_calc, In_oklab)
    in
    let base_decl, _ = Var.binding gradient_position_var angle_calc in
    let interp_decl, _ = Var.binding gradient_position_var angle_calc_interp in
    let rules = gradient_direction_rules ~base_decl ~interp_decl in
    style ~property_rules:gradient_property_rules ~rules:(Some rules) []

  (** [bg-linear-{angle}/interp] - angle with specific interpolation *)
  let bg_linear_angle_interp' angle_deg interp_str =
    let dir_val : Css.gradient_direction =
      Angle (Deg (float_of_int angle_deg))
    in
    let base_decl, _ = Var.binding gradient_position_var dir_val in
    match interp_to_css_string interp_str with
    | Some interp_css ->
        let interp_decl =
          Css.custom_property ~layer:"utilities" "--tw-gradient-position"
            (string_of_int angle_deg ^ "deg " ^ interp_css)
        in
        let rules = gradient_direction_rules ~base_decl ~interp_decl in
        style ~property_rules:gradient_property_rules ~rules:(Some rules) []
    | None -> bg_linear_angle' angle_deg

  (** [-bg-linear-{angle}/interp] *)
  let bg_linear_angle_neg_interp' angle_deg interp_str =
    let angle_calc : Css.gradient_direction =
      Angle (Calc (Expr (Val (Deg (float_of_int angle_deg)), Mul, Num (-1.0))))
    in
    let base_decl, _ = Var.binding gradient_position_var angle_calc in
    match interp_to_css_string interp_str with
    | Some interp_css ->
        let interp_decl =
          Css.custom_property ~layer:"utilities" "--tw-gradient-position"
            ("calc(" ^ string_of_int angle_deg ^ "deg * -1) " ^ interp_css)
        in
        let rules = gradient_direction_rules ~base_decl ~interp_decl in
        style ~property_rules:gradient_property_rules ~rules:(Some rules) []
    | None -> bg_linear_angle_neg' angle_deg

  (** [bg-linear-[value]] - bracket linear gradient (no [@supports]). Output:
      [--tw-gradient-position: {value}; background-image:
       linear-gradient(var(--tw-gradient-stops, {value}))] The value_str is the
      raw bracket inner; we convert rad→deg and _→space. *)
  let bg_linear_bracket' value_str =
    let css_val = bracket_value_to_css value_str in
    let position_decl =
      Css.custom_property ~layer:"utilities" "--tw-gradient-position" css_val
    in
    let stops_ref : Css.gradient_stop Css.var =
      Css.var_ref ~fallback:(Raw_fallback css_val) "tw-gradient-stops"
    in
    style ~property_rules:gradient_property_rules
      [ position_decl; Css.background_image (Linear_gradient_var stops_ref) ]

  (** -bg-linear-[value] - negated bracket *)
  let bg_linear_bracket_neg' value_str =
    let css_val = bracket_value_to_css value_str in
    let neg_str = "calc(" ^ css_val ^ " * -1)" in
    let position_decl =
      Css.custom_property ~layer:"utilities" "--tw-gradient-position" neg_str
    in
    let stops_ref : Css.gradient_stop Css.var =
      Css.var_ref ~fallback:(Raw_fallback neg_str) "tw-gradient-stops"
    in
    style ~property_rules:gradient_property_rules
      [ position_decl; Css.background_image (Linear_gradient_var stops_ref) ]

  (** [bg-conic/interp] - conic gradient with interpolation only (no
      [@supports]) *)
  let bg_conic_interp' interp_str =
    match interp_to_css_string interp_str with
    | Some interp_css ->
        let position_decl =
          Css.custom_property ~layer:"utilities" "--tw-gradient-position"
            interp_css
        in
        let stops_ref = Var.reference gradient_stops_var in
        style ~property_rules:gradient_property_rules
          [ position_decl; Css.background_image (Conic_gradient_var stops_ref) ]
    | None -> style []

  (** [bg-conic-{angle}/interp] - conic with angle and interpolation *)
  let bg_conic_angle_interp' angle_deg interp_str =
    match interp_to_css_string interp_str with
    | Some interp_css ->
        let position_css =
          "from " ^ string_of_int angle_deg ^ "deg " ^ interp_css
        in
        let position_decl =
          Css.custom_property ~layer:"utilities" "--tw-gradient-position"
            position_css
        in
        let stops_ref = Var.reference gradient_stops_var in
        style ~property_rules:gradient_property_rules
          [ position_decl; Css.background_image (Conic_gradient_var stops_ref) ]
    | None -> style []

  (** [-bg-conic-{angle}/interp] *)
  let bg_conic_angle_neg_interp' angle_deg interp_str =
    match interp_to_css_string interp_str with
    | Some interp_css ->
        let position_css =
          "from calc(" ^ string_of_int angle_deg ^ "deg * -1) " ^ interp_css
        in
        let position_decl =
          Css.custom_property ~layer:"utilities" "--tw-gradient-position"
            position_css
        in
        let stops_ref = Var.reference gradient_stops_var in
        style ~property_rules:gradient_property_rules
          [ position_decl; Css.background_image (Conic_gradient_var stops_ref) ]
    | None -> style []

  (** bg-radial/interp - radial gradient with interpolation *)
  let bg_radial_interp' interp_str =
    match interp_to_css_string interp_str with
    | Some interp_css ->
        let position_decl =
          Css.custom_property ~layer:"utilities" "--tw-gradient-position"
            interp_css
        in
        let stops_ref = Var.reference gradient_stops_var in
        style ~property_rules:gradient_property_rules
          [
            position_decl; Css.background_image (Radial_gradient_var stops_ref);
          ]
    | None -> style []

  (** bg-radial-[value] - bracket radial gradient *)
  let bg_radial_bracket' value_str =
    let css_val = bracket_value_to_css value_str in
    let position_decl =
      Css.custom_property ~layer:"utilities" "--tw-gradient-position" css_val
    in
    let stops_ref : Css.gradient_stop Css.var =
      Css.var_ref ~fallback:(Raw_fallback css_val) "tw-gradient-stops"
    in
    style ~property_rules:gradient_property_rules
      [ position_decl; Css.background_image (Radial_gradient_var stops_ref) ]

  (** Bracket color var with opacity: bg-[color:var(--x)]/50 *)
  let bg_bracket_color_var_opacity' var_str opacity =
    let bare = Parse.extract_var_name var_str in
    let var_ref : Css.color Css.var = Css.var_ref bare in
    let percent = Color.opacity_to_percent opacity in
    let fallback_decl = Css.background_color (Var var_ref) in
    let oklab_color =
      Css.color_mix ~in_space:Oklab (Var var_ref) Transparent ~percent1:percent
    in
    let oklab_decl = Css.background_color oklab_color in
    let supports_rule =
      Css.supports ~condition:Color.color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
    in
    style ~rules:(Some [ supports_rule ]) [ fallback_decl ]

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
    | Bg_inherit -> bg_inherit'
    | Bg_none -> bg_none'
    | Bg_auto -> bg_auto'
    | Bg_cover -> bg_cover'
    | Bg_contain -> bg_contain'
    | Bg_fixed -> bg_fixed'
    | Bg_local -> bg_local'
    | Bg_scroll -> bg_scroll'
    | Bg_repeat -> bg_repeat'
    | Bg_no_repeat -> bg_no_repeat'
    | Bg_repeat_x -> bg_repeat_x'
    | Bg_repeat_y -> bg_repeat_y'
    | Bg_repeat_round -> bg_repeat_round'
    | Bg_repeat_space -> bg_repeat_space'
    | Bg_position pos ->
        let pos_val : Css.position_value list =
          match pos with
          | Pos_bottom -> [ Center_bottom ]
          | Pos_bottom_left -> [ XY (Px 0., Pct 100.) ]
          | Pos_bottom_right -> [ XY (Pct 100., Pct 100.) ]
          | Pos_center -> [ Center ]
          | Pos_left -> [ XY (Px 0., Px 0.) ]
          | Pos_left_bottom -> [ XY (Px 0., Pct 100.) ]
          | Pos_left_top -> [ XY (Px 0., Px 0.) ]
          | Pos_right -> [ XY (Pct 100., Pct 100.) ]
          | Pos_right_bottom -> [ XY (Pct 100., Pct 100.) ]
          | Pos_right_top -> [ XY (Pct 100., Px 0.) ]
          | Pos_top -> [ Center_top ]
          | Pos_top_left -> [ XY (Px 0., Px 0.) ]
          | Pos_top_right -> [ XY (Pct 100., Px 0.) ]
        in
        bg_position' pos_val
    | Bg_bracket_contain -> style [ Css.background_size Contain ]
    | Bg_bracket_cover -> style [ Css.background_size Cover ]
    | Bg_bracket_size inner -> (
        match parse_bracket_size inner with
        | Some decl -> style [ decl ]
        | None -> style [ Css.background_size Auto ])
    | Bg_bracket_position inner -> (
        match parse_bracket_position inner with
        | Some decl -> style [ decl ]
        | None -> style [ Css.background_position [ Center ] ])
    | Bg_bracket_typed_position inner -> (
        match parse_bracket_position inner with
        | Some decl -> style [ decl ]
        | None -> style [ Css.background_position [ Center ] ])
    | Bg_bracket_color_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.color Css.var = Css.var_ref bare in
        style [ Css.background_color (Var var_ref) ]
    | Bg_bracket_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.color Css.var = Css.var_ref bare in
        style [ Css.background_color (Var var_ref) ]
    | Bg_bracket_image_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.background_image Css.var = Css.var_ref bare in
        style [ Css.background_image (Var var_ref) ]
    | Bg_bracket_url url -> style [ Css.background_image (Url url) ]
    | Bg_bracket_url_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.background_image Css.var = Css.var_ref bare in
        style [ Css.background_image (Var var_ref) ]
    | Bg_bracket_linear_gradient _v ->
        (* TODO: parse linear-gradient content *)
        style [ Css.background_image None ]
    | Bg_linear_to dir -> bg_linear_to' dir
    | Bg_linear_to_interp (dir, interp) -> bg_linear_to_interp' dir interp
    | Bg_linear_angle n -> bg_linear_angle' n
    | Bg_linear_angle_neg n -> bg_linear_angle_neg' n
    | Bg_linear_angle_interp (n, interp) -> bg_linear_angle_interp' n interp
    | Bg_linear_angle_neg_interp (n, interp) ->
        bg_linear_angle_neg_interp' n interp
    | Bg_linear_bracket v -> bg_linear_bracket' v
    | Bg_linear_bracket_neg v -> bg_linear_bracket_neg' v
    | Bg_conic_interp interp -> bg_conic_interp' interp
    | Bg_conic_angle_interp (n, interp) -> bg_conic_angle_interp' n interp
    | Bg_conic_angle_neg_interp (n, interp) ->
        bg_conic_angle_neg_interp' n interp
    | Bg_radial_interp interp -> bg_radial_interp' interp
    | Bg_radial_bracket v -> bg_radial_bracket' v
    | Bg_bracket_color_var_opacity (v, opacity) ->
        bg_bracket_color_var_opacity' v opacity
    | Bg_bracket_var_opacity (v, opacity) ->
        bg_bracket_color_var_opacity' v opacity
    | Bg_bracket_length inner -> (
        match parse_bracket_size inner with
        | Some decl -> style [ decl ]
        | None -> style [ Css.background_size Auto ])
    | Bg_position_bracket inner -> (
        match parse_bracket_position inner with
        | Some decl -> style [ decl ]
        | None -> style [ Css.background_position [ Center ] ])
    | Bg_size_bracket inner -> (
        match parse_bracket_size inner with
        | Some decl -> style [ decl ]
        | None -> style [ Css.background_size Auto ])

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
    (* bg-inherit sorts with other bg-color utilities *)
    | Bg_inherit -> Color.suborder_with_shade "inherit"
    (* bg-none: background-image: none *)
    | Bg_none -> 210000
    (* bg-size utilities *)
    | Bg_auto -> 300000
    | Bg_contain -> 300001
    | Bg_cover -> 300002
    (* bg-attachment utilities *)
    | Bg_fixed -> 400000
    | Bg_local -> 400001
    | Bg_scroll -> 400002
    (* bg-position utilities *)
    | Bg_position Pos_bottom -> 500000
    | Bg_position Pos_bottom_left -> 500001
    | Bg_position Pos_bottom_right -> 500002
    | Bg_position Pos_center -> 500003
    | Bg_position Pos_left -> 500004
    | Bg_position Pos_left_bottom -> 500005
    | Bg_position Pos_left_top -> 500006
    | Bg_position Pos_right -> 500007
    | Bg_position Pos_right_bottom -> 500008
    | Bg_position Pos_right_top -> 500009
    | Bg_position Pos_top -> 500010
    | Bg_position Pos_top_left -> 500011
    | Bg_position Pos_top_right -> 500012
    (* bg-repeat utilities *)
    | Bg_no_repeat -> 600000
    | Bg_repeat -> 600001
    | Bg_repeat_round -> 600002
    | Bg_repeat_space -> 600003
    | Bg_repeat_x -> 600004
    | Bg_repeat_y -> 600005
    (* Bracket size variants *)
    | Bg_bracket_contain -> 300010
    | Bg_bracket_cover -> 300011
    | Bg_bracket_size _ -> 300012
    | Bg_size_bracket _ -> 300013
    (* Bracket position variants *)
    | Bg_bracket_position _ -> 500020
    | Bg_bracket_typed_position _ -> 500021
    | Bg_position_bracket _ -> 500022
    (* Bracket color/var variants *)
    | Bg_bracket_color_var _ -> 50000
    | Bg_bracket_var _ -> 50001
    (* Bracket image variants *)
    | Bg_bracket_image_var _ -> 210010
    | Bg_bracket_url _ -> 210011
    | Bg_bracket_url_var _ -> 210012
    | Bg_bracket_linear_gradient _ -> 210013
    (* bg-linear-to *)
    | Bg_linear_to _ -> 100010
    | Bg_linear_to_interp (_, _) -> 100015
    | Bg_linear_angle _ -> 100020
    | Bg_linear_angle_neg _ -> 100025
    | Bg_linear_angle_interp (_, _) -> 100030
    | Bg_linear_angle_neg_interp (_, _) -> 100035
    | Bg_linear_bracket _ -> 100040
    | Bg_linear_bracket_neg _ -> 100045
    (* bg-conic *)
    | Bg_conic_interp _ -> 200000
    | Bg_conic_angle_interp (_, _) -> 200010
    | Bg_conic_angle_neg_interp (_, _) -> 200015
    (* bg-radial *)
    | Bg_radial_interp _ -> 200020
    | Bg_radial_bracket _ -> 200030
    (* Bracket color/var with opacity *)
    | Bg_bracket_color_var_opacity _ -> 50010
    | Bg_bracket_var_opacity _ -> 50011
    | Bg_bracket_length _ -> 300014

  (** Split a string on the first '/' into (base, modifier_opt). E.g. "r/oklab"
      → ("r", Some "oklab"), "45" → ("45", None) *)
  let split_mod s =
    match String.index_opt s '/' with
    | Some i ->
        (String.sub s 0 i, Some (String.sub s (i + 1) (String.length s - i - 1)))
    | None -> (s, None)

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
    (* Background color keywords *)
    | [ "bg"; "inherit" ] -> Ok Bg_inherit
    (* Background image *)
    | [ "bg"; "none" ] -> Ok Bg_none
    (* Background size *)
    | [ "bg"; "auto" ] -> Ok Bg_auto
    | [ "bg"; "cover" ] -> Ok Bg_cover
    | [ "bg"; "contain" ] -> Ok Bg_contain
    (* Background attachment *)
    | [ "bg"; "fixed" ] -> Ok Bg_fixed
    | [ "bg"; "local" ] -> Ok Bg_local
    | [ "bg"; "scroll" ] -> Ok Bg_scroll
    (* Background repeat *)
    | [ "bg"; "repeat" ] -> Ok Bg_repeat
    | [ "bg"; "no"; "repeat" ] -> Ok Bg_no_repeat
    | [ "bg"; "repeat"; "x" ] -> Ok Bg_repeat_x
    | [ "bg"; "repeat"; "y" ] -> Ok Bg_repeat_y
    | [ "bg"; "repeat"; "round" ] -> Ok Bg_repeat_round
    | [ "bg"; "repeat"; "space" ] -> Ok Bg_repeat_space
    (* Background position *)
    | [ "bg"; "bottom" ] -> Ok (Bg_position Pos_bottom)
    | [ "bg"; "bottom"; "left" ] -> Ok (Bg_position Pos_bottom_left)
    | [ "bg"; "bottom"; "right" ] -> Ok (Bg_position Pos_bottom_right)
    | [ "bg"; "center" ] -> Ok (Bg_position Pos_center)
    | [ "bg"; "left" ] -> Ok (Bg_position Pos_left)
    | [ "bg"; "left"; "bottom" ] -> Ok (Bg_position Pos_left_bottom)
    | [ "bg"; "left"; "top" ] -> Ok (Bg_position Pos_left_top)
    | [ "bg"; "right" ] -> Ok (Bg_position Pos_right)
    | [ "bg"; "right"; "bottom" ] -> Ok (Bg_position Pos_right_bottom)
    | [ "bg"; "right"; "top" ] -> Ok (Bg_position Pos_right_top)
    | [ "bg"; "top" ] -> Ok (Bg_position Pos_top)
    | [ "bg"; "top"; "left" ] -> Ok (Bg_position Pos_top_left)
    | [ "bg"; "top"; "right" ] -> Ok (Bg_position Pos_top_right)
    (* bg-linear-to-* direction utilities (with optional /interp modifier) *)
    | [ "bg"; "linear"; "to"; dir_mod ] -> (
        let dir_s, interp_opt = split_mod dir_mod in
        match (parse_direction dir_s, interp_opt) with
        | Some dir, None -> Ok (Bg_linear_to dir)
        | Some dir, Some interp -> Ok (Bg_linear_to_interp (dir, interp))
        | None, _ -> Error (`Msg ("Unknown direction: " ^ dir_s)))
    (* bg-linear-[value] - bracket linear gradient *)
    | [ "bg"; "linear"; bracket ] when Parse.is_bracket_value bracket ->
        let inner = Parse.bracket_inner bracket in
        Ok (Bg_linear_bracket inner)
    (* bg-linear-{angle} and bg-linear-{angle}/interp *)
    | [ "bg"; "linear"; angle_mod ] -> (
        let angle_s, interp_opt = split_mod angle_mod in
        match (int_of_string_opt angle_s, interp_opt) with
        | Some n, None -> Ok (Bg_linear_angle n)
        | Some n, Some interp -> Ok (Bg_linear_angle_interp (n, interp))
        | None, _ -> Error (`Msg ("Invalid bg-linear angle: " ^ angle_mod)))
    (* -bg-linear-[value] - negated bracket linear gradient *)
    | [ ""; "bg"; "linear"; bracket ] when Parse.is_bracket_value bracket ->
        let inner = Parse.bracket_inner bracket in
        Ok (Bg_linear_bracket_neg inner)
    (* -bg-linear-{angle} and -bg-linear-{angle}/interp *)
    | [ ""; "bg"; "linear"; angle_mod ] -> (
        let angle_s, interp_opt = split_mod angle_mod in
        match (int_of_string_opt angle_s, interp_opt) with
        | Some n, None -> Ok (Bg_linear_angle_neg n)
        | Some n, Some interp -> Ok (Bg_linear_angle_neg_interp (n, interp))
        | None, _ -> Error (`Msg ("Invalid -bg-linear angle: " ^ angle_mod)))
    (* bg-conic/interp - conic gradient with modifier only *)
    | [ "bg"; conic_mod ]
      when String.length conic_mod > 6 && String.sub conic_mod 0 6 = "conic/" ->
        let interp = String.sub conic_mod 6 (String.length conic_mod - 6) in
        Ok (Bg_conic_interp interp)
    (* bg-conic-{angle}/interp - conic with angle *)
    | [ "bg"; "conic"; angle_mod ] -> (
        let angle_s, interp_opt = split_mod angle_mod in
        match (int_of_string_opt angle_s, interp_opt) with
        | Some n, Some interp -> Ok (Bg_conic_angle_interp (n, interp))
        | _ -> Error (`Msg ("Invalid bg-conic angle: " ^ angle_mod)))
    (* -bg-conic-{angle}/interp *)
    | [ ""; "bg"; "conic"; angle_mod ] -> (
        let angle_s, interp_opt = split_mod angle_mod in
        match (int_of_string_opt angle_s, interp_opt) with
        | Some n, Some interp -> Ok (Bg_conic_angle_neg_interp (n, interp))
        | _ -> Error (`Msg ("Invalid -bg-conic angle: " ^ angle_mod)))
    (* bg-radial/interp - radial gradient with modifier only *)
    | [ "bg"; radial_mod ]
      when String.length radial_mod > 7 && String.sub radial_mod 0 7 = "radial/"
      ->
        let interp = String.sub radial_mod 7 (String.length radial_mod - 7) in
        Ok (Bg_radial_interp interp)
    (* bg-radial-[value] - bracket radial gradient *)
    | [ "bg"; "radial"; bracket ] when Parse.is_bracket_value bracket ->
        let inner = Parse.bracket_inner bracket in
        Ok (Bg_radial_bracket inner)
    (* bg-position-[...] bracket notation *)
    | [ "bg"; "position"; bracket ] when Parse.is_bracket_value bracket ->
        Ok (Bg_position_bracket (Parse.bracket_inner bracket))
    (* bg-size-[...] bracket notation *)
    | [ "bg"; "size"; bracket ] when Parse.is_bracket_value bracket ->
        Ok (Bg_size_bracket (Parse.bracket_inner bracket))
    (* Bracket notation: bg-[...] and bg-[...]/opacity *)
    | [ "bg"; bracket_stuff ]
      when String.length bracket_stuff > 1 && bracket_stuff.[0] = '[' -> (
        (* Find the matching ] for the first [ *)
        let len = String.length bracket_stuff in
        let close = ref (-1) in
        let depth = ref 0 in
        for i = 0 to len - 1 do
          if !close < 0 then
            if bracket_stuff.[i] = '[' then incr depth
            else if bracket_stuff.[i] = ']' then (
              decr depth;
              if !depth = 0 then close := i)
        done;
        let parse_opacity s =
          if String.length s > 1 && s.[0] = '[' && s.[String.length s - 1] = ']'
          then
            let inner_o = String.sub s 1 (String.length s - 2) in
            if String.ends_with ~suffix:"%" inner_o then
              let num_s = String.sub inner_o 0 (String.length inner_o - 1) in
              match float_of_string_opt num_s with
              | Some f -> Some (Color.Opacity_bracket_percent f)
              | None ->
                  if Parse.is_var inner_o then
                    Some (Color.Opacity_named inner_o)
                  else None
            else
              match float_of_string_opt inner_o with
              | Some f -> Some (Color.Opacity_arbitrary f)
              | None ->
                  if Parse.is_var inner_o then
                    Some (Color.Opacity_named inner_o)
                  else None
          else
            match float_of_string_opt s with
            | Some f -> Some (Color.Opacity_percent f)
            | None -> None
        in
        if !close >= 0 && !close + 1 < len && bracket_stuff.[!close + 1] = '/'
        then
          (* Bracket with opacity: [color:var(--x)]/50 *)
          let bracket = String.sub bracket_stuff 0 (!close + 1) in
          let opacity_str =
            String.sub bracket_stuff (!close + 2) (len - !close - 2)
          in
          let inner = Parse.bracket_inner bracket in
          match parse_opacity opacity_str with
          | Some opacity ->
              if String.length inner > 6 && String.sub inner 0 6 = "color:" then
                let var_str = String.sub inner 6 (String.length inner - 6) in
                Ok (Bg_bracket_color_var_opacity (var_str, opacity))
              else if Parse.is_var inner then
                Ok (Bg_bracket_var_opacity (inner, opacity))
              else Error (`Msg ("Unknown bg bracket value: " ^ bracket_stuff))
          | None -> Error (`Msg ("Invalid opacity: " ^ bracket_stuff))
        else
          (* Regular bracket notation: bg-[...] *)
          let inner = Parse.bracket_inner bracket_stuff in
          match inner with
          | "contain" -> Ok Bg_bracket_contain
          | "cover" -> Ok Bg_bracket_cover
          | _ when String.length inner > 7 && String.sub inner 0 7 = "length:"
            ->
              Ok
                (Bg_bracket_length
                   (String.sub inner 7 (String.length inner - 7)))
          | _ when String.length inner > 5 && String.sub inner 0 5 = "size:" ->
              Ok
                (Bg_bracket_size (String.sub inner 5 (String.length inner - 5)))
          | _ when String.length inner > 9 && String.sub inner 0 9 = "position:"
            ->
              Ok
                (Bg_bracket_typed_position
                   (String.sub inner 9 (String.length inner - 9)))
          | _ when String.length inner > 6 && String.sub inner 0 6 = "color:" ->
              Ok
                (Bg_bracket_color_var
                   (String.sub inner 6 (String.length inner - 6)))
          | _ when String.length inner > 6 && String.sub inner 0 6 = "image:" ->
              Ok
                (Bg_bracket_image_var
                   (String.sub inner 6 (String.length inner - 6)))
          | _ when String.length inner > 4 && String.sub inner 0 4 = "url:" ->
              Ok
                (Bg_bracket_url_var
                   (String.sub inner 4 (String.length inner - 4)))
          | _ when String.length inner > 4 && String.sub inner 0 4 = "url(" ->
              let url_content = String.sub inner 4 (String.length inner - 5) in
              Ok (Bg_bracket_url url_content)
          | _
            when String.length inner > 16
                 && String.sub inner 0 16 = "linear-gradient(" ->
              Ok (Bg_bracket_linear_gradient inner)
          | _ when Parse.is_var inner -> Ok (Bg_bracket_var inner)
          | _ ->
              if parse_bracket_position inner <> None then
                Ok (Bg_bracket_position inner)
              else Error (`Msg ("Unknown bg bracket value: " ^ inner)))
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
