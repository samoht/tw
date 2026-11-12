(** Gradient utilities

    What's included:
    - `bg-linear-*` - Linear gradients by direction, angle or bracket value,
      negated angles, and the `/<interpolation>` modifier.
    - `bg-radial*`, `bg-conic*` - Radial and conic gradients, with an angle, a
      bracket value or the `/<interpolation>` modifier.
    - `bg-gradient-to-*` - The v3 spellings of the linear directions.
    - `from-*`, `via-*`, `to-*` - Gradient colour stops, with an opacity
      modifier, and stop positions.
    - `via-none` - Clears the via stops.

    The background colours, images and layout are in [Backgrounds]. *)

module Css = Cascade.Css

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
  (* Gradient color source - shared by from/via/to *)
  module Color_source = struct
    (* A colour that spells itself out, with no palette behind it. *)
    type plain =
      | Current
      | Inherit
      | Transparent
      | Bracket_hex of string
      | Bracket_color_var of string
      | Bracket_var of string
      | Bracket_color of string

    (* A palette colour reads its value out of the scheme, so it has no plain
       CSS spelling. Keeping the two apart is what lets the conversion to a
       [Css.color] cover every source it is handed. *)
    type t =
      | Palette of Color.color * int * Color.opacity_modifier option
      | Plain of plain * Color.opacity_modifier option
  end

  (* Gradient position source *)
  (* A bracket stop position carries the author's text alongside the value it
     denotes, so the class name is spelled exactly as it was written. *)
  type gradient_position_source =
    | Percent of float
    | Bracket of string * Css.length_percentage

  type gradient_target = From | Via | To

  type t =
    | Bg_gradient_to of direction
    | Gradient_color of gradient_target * Color_source.t
    | Gradient_raw of
        gradient_target * string * string * Color.opacity_modifier option
    | Gradient_stop_position of gradient_target * gradient_position_source
    | Via_none (* via-none: clears the gradient's via stops *)
    (* bg-linear-to-* direction utilities *)
    | Bg_linear_to of direction
    (* bg-linear-to-*/interp - direction with interpolation modifier *)
    (* The author's modifier travels with the interpolation text it denotes, so
       the class name is spelled exactly as it was written. *)
    | Bg_linear_to_interp of direction * string * string
    (* bg-linear-{angle} - linear gradient with angle *)
    | Bg_linear_angle of int
    (* -bg-linear-{angle} - negated angle *)
    | Bg_linear_angle_neg of int
    (* bg-linear-{angle}/interp - angle with interpolation *)
    | Bg_linear_angle_interp of int * string * string
    (* -bg-linear-{angle}/interp *)
    | Bg_linear_angle_neg_interp of int * string * string
    (* bg-linear-[value] - bracket linear gradient value *)
    | Bg_linear_bracket of string
    (* -bg-linear-[value] - negated bracket *)
    | Bg_linear_bracket_neg of string
    (* bg-conic - bare conic gradient (in oklab) *)
    | Bg_conic
    (* bg-conic-{angle} - conic with angle, no interpolation modifier *)
    | Bg_conic_angle of int
    (* -bg-conic-{angle} *)
    | Bg_conic_angle_neg of int
    (* bg-conic/interp - conic gradient with interpolation *)
    | Bg_conic_interp of string * string
    (* bg-conic-{angle}/interp - conic with angle and interpolation *)
    | Bg_conic_angle_interp of int * string * string
    (* -bg-conic-{angle}/interp *)
    | Bg_conic_angle_neg_interp of int * string * string
    | Bg_conic_bracket of string
    (* bg-radial - bare radial gradient (in oklab) *)
    | Bg_radial
    (* bg-radial/interp - radial gradient with interpolation *)
    | Bg_radial_interp of string * string
    (* bg-radial-[value] - bracket radial gradient value *)
    | Bg_radial_bracket of string

  let to_class (t : t) =
    match t with
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
    | Via_none -> "via-none"
    | Gradient_color (target, src) ->
        let prefix =
          match target with From -> "from-" | Via -> "via-" | To -> "to-"
        in
        let plain_class : Color_source.plain -> string = function
          | Color_source.Current -> "current"
          | Color_source.Inherit -> "inherit"
          | Color_source.Transparent -> "transparent"
          | Color_source.Bracket_hex h -> "[#" ^ h ^ "]"
          | Color_source.Bracket_color_var v -> "[color:" ^ v ^ "]"
          | Color_source.Bracket_var v | Color_source.Bracket_color v ->
              "[" ^ v ^ "]"
        in
        let opacity_class = function
          | None -> ""
          | Some opacity -> Color.opacity_suffix opacity
        in
        let color_class : Color_source.t -> string = function
          | Color_source.Palette (color, shade, opacity) ->
              let base =
                if Color.is_shadeless color then Color.pp color
                else Color.pp color ^ "-" ^ string_of_int shade
              in
              base ^ opacity_class opacity
          | Color_source.Plain (source, opacity) ->
              plain_class source ^ opacity_class opacity
        in
        prefix ^ color_class src
    | Gradient_stop_position (target, src) -> (
        let prefix =
          match target with From -> "from-" | Via -> "via-" | To -> "to-"
        in
        match src with
        | Percent p ->
            let p_str =
              if Float.is_integer p then string_of_int (int_of_float p)
              else string_of_float p
            in
            prefix ^ p_str ^ "%"
        | Bracket (spelling, _) -> prefix ^ "[" ^ spelling ^ "]")
    | Gradient_raw (target, spelling, _, opacity) ->
        let prefix =
          match target with From -> "from-" | Via -> "via-" | To -> "to-"
        in
        prefix ^ "[" ^ spelling ^ "]"
        ^ Option.fold ~none:"" ~some:Color.opacity_suffix opacity
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
    | Bg_linear_to_interp (dir, interp, _) ->
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
    | Bg_linear_angle_interp (n, interp, _) ->
        "bg-linear-" ^ string_of_int n ^ "/" ^ interp
    | Bg_linear_angle_neg_interp (n, interp, _) ->
        "-bg-linear-" ^ string_of_int n ^ "/" ^ interp
    | Bg_linear_bracket v -> "bg-linear-[" ^ v ^ "]"
    | Bg_linear_bracket_neg v -> "-bg-linear-[" ^ v ^ "]"
    | Bg_conic -> "bg-conic"
    | Bg_conic_angle n -> "bg-conic-" ^ string_of_int n
    | Bg_conic_angle_neg n -> "-bg-conic-" ^ string_of_int n
    | Bg_conic_interp (interp, _) -> "bg-conic/" ^ interp
    | Bg_conic_angle_interp (n, interp, _) ->
        "bg-conic-" ^ string_of_int n ^ "/" ^ interp
    | Bg_conic_angle_neg_interp (n, interp, _) ->
        "-bg-conic-" ^ string_of_int n ^ "/" ^ interp
    | Bg_conic_bracket v -> "bg-conic-[" ^ v ^ "]"
    | Bg_radial -> "bg-radial"
    | Bg_radial_interp (interp, _) -> "bg-radial/" ^ interp
    | Bg_radial_bracket v -> "bg-radial-[" ^ v ^ "]"

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

  open Style
  open Css

  let name = "gradients"

  (* Tailwind sorts a layer by the rank its property table gives the property
     each rule writes. The gradient images write background-image (199), which
     they share with the background images, and the stops the gradient variables
     (202-208) after it. *)
  let property_rank = function
    | Bg_gradient_to _ | Bg_linear_to _ | Bg_linear_to_interp _
    | Bg_linear_angle _ | Bg_linear_angle_neg _ | Bg_linear_angle_interp _
    | Bg_linear_angle_neg_interp _ | Bg_linear_bracket _
    | Bg_linear_bracket_neg _ | Bg_conic | Bg_conic_angle _
    | Bg_conic_angle_neg _ | Bg_conic_interp _ | Bg_conic_angle_interp _
    | Bg_conic_angle_neg_interp _ | Bg_radial | Bg_radial_interp _
    | Bg_conic_bracket _ | Bg_radial_bracket _ ->
        199
    | Via_none -> 202
    | Gradient_color (From, _) -> 203
    | Gradient_raw (From, _, _, _) -> 203
    | Gradient_stop_position (From, _) -> 204
    | Gradient_color (Via, _) -> 205
    | Gradient_raw (Via, _, _, _) -> 205
    | Gradient_stop_position (Via, _) -> 206
    | Gradient_color (To, _) -> 207
    | Gradient_raw (To, _, _, _) -> 207
    | Gradient_stop_position (To, _) -> 208

  (* Every rank above falls before mask-image (209), so the gradients sort in
     the band the background colours open rather than with the masks. *)
  let priority _ = 20

  (* Rules sharing a slot sort by declaration count, most first, then by class
     name. In background-image's slot the native linear directions and angles
     carry an @supports fallback. The legacy [bg-gradient-to-*] aliases join the
     next group with conic, radial and bracket-linear gradients, and the plain
     images in [Backgrounds], which write nothing past background-image, close
     the property after both. The stops write one variable each and close
     theirs. *)
  let suborder t =
    let rank = property_rank t in
    match t with
    | Bg_linear_to _ | Bg_linear_to_interp _ | Bg_linear_angle _
    | Bg_linear_angle_neg _ | Bg_linear_angle_interp _
    | Bg_linear_angle_neg_interp _ ->
        Utility.Property_order.slot rank
    | Bg_gradient_to _ | Bg_linear_bracket _ | Bg_linear_bracket_neg _
    | Bg_conic | Bg_conic_angle _ | Bg_conic_angle_neg _ | Bg_conic_interp _
    | Bg_conic_angle_interp _ | Bg_conic_angle_neg_interp _ | Bg_radial
    | Bg_conic_bracket _ | Bg_radial_interp _ | Bg_radial_bracket _ ->
        Utility.Property_order.slot rank + 1
    | Gradient_color _ | Gradient_raw _ | Gradient_stop_position _ | Via_none ->
        Utility.Property_order.last rank

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

  (* A typed [gradient-direction]: a linear direction, an angle, or either with
     [in <interpolation>], set straight through [Var.binding] with no text
     round-trip. *)
  let gradient_position_decl (dir : Css.gradient_direction) =
    Var.set gradient_position_var dir

  (* Conic's [from <angle> ...] and radial's [circle at ...] are not
     [gradient_direction] values - a different grammar entirely - and an
     [/interp] bracket can carry arbitrary text ([in srgb-linear], a bare
     percentage) no typed constructor covers either. Both go through the raw
     property below, keyed off the same handle's name via [Var.css_name] rather
     than spelling "--tw-gradient-position" again beside it. *)
  let gradient_position_decl_of_string value =
    Css.custom_property ~layer:"utilities"
      (Var.css_name gradient_position_var)
      value

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
    Var.property_default Length_percentage ~initial:(Pct 0.) ~property_order:13
      ~family:`Gradient "tw-gradient-from-position"

  let gradient_via_position_var =
    Var.property_default Length_percentage ~initial:(Pct 50.) ~property_order:14
      ~family:`Gradient "tw-gradient-via-position"

  let gradient_to_position_var =
    Var.property_default Length_percentage ~initial:(Pct 100.)
      ~property_order:15 ~family:`Gradient "tw-gradient-to-position"

  let bg_gradient_to' dir =
    (* Set --tw-gradient-position to the typed direction with oklab
       interpolation *)
    let dir_val = With_interpolation (to_spec dir, In (Oklab, None)) in
    let d_position = Var.set gradient_position_var dir_val in
    (* Reference --tw-gradient-stops for linear-gradient *)
    let stops_ref = Var.reference gradient_stops_var in
    style [ d_position; Css.background_image (Linear_gradient_var stops_ref) ]

  (* A palette colour's value with the theme token it reads, or the value alone
     for an arbitrary colour, which has no token. *)

  (** Common helper for gradient color utilities *)
  let gradient_color ?theme ~prefix ~set_var ?(shade = 500) color =
    let theme_decls, gradient_color_value =
      Color.bound_color ?theme ~shade color
    in

    (* Set the appropriate gradient variable *)
    let d_var = Var.set set_var gradient_color_value in

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
          let d_stops_via = Var.set gradient_stops_var (Var via_stops_ref) in
          (d_stops_via, Some d_via_stops)
      | _ ->
          (* For from/to, reference via-stops with fallback *)
          let via_stops_ref =
            Var.reference_with_fallback gradient_via_stops_var fallback_stops
          in
          let d_stops = Var.set gradient_stops_var (Var via_stops_ref) in
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
      |> List.filter_map Fun.id |> Css.concat
    in

    (* Build declarations list *)
    let base_declarations = theme_decls @ [ d_var ] in
    let declarations =
      match d_via_stops_opt with
      | Some d_via_stops -> base_declarations @ [ d_via_stops; d_stops ]
      | None -> base_declarations @ [ d_stops ]
    in

    style ~property_rules declarations

  let gradient_supports_condition =
    Css.Supports.property "background-image" "linear-gradient(in lab, red, red)"

  (* Gradient direction utilities do NOT register property_rules. Only
     from/to/via (gradient color) utilities need the @layer properties block for
     initial values. *)
  let gradient_property_rules = Css.empty

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
      With_interpolation (dir_val, In (Oklab, None))
    in
    let base_decl = Var.set gradient_position_var dir_val in
    let interp_decl = Var.set gradient_position_var dir_with_interp in
    let rules = gradient_direction_rules ~base_decl ~interp_decl in
    style ~property_rules:gradient_property_rules ~rules:(Some rules) []

  (* The [--tw-gradient-position] text a [/interp] modifier denotes. A bracket
     carries its own text; the four hue keywords stand for their oklch spelling;
     every other modifier names a colour space, which Tailwind writes through as
     [in <space>] whether or not the space exists. [None] is a modifier Tailwind
     refuses too - a second modifier, a function, a leading dot or sign, or the
     empty one - and [of_class] declines the utility rather than leaving
     [to_style] to raise. *)
  let interp_to_css_string s =
    let is_tail c =
      (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')
      || (c >= '0' && c <= '9')
      || c = '.' || c = '-' || c = '_'
    in
    let is_head c = is_tail c && c <> '.' && c <> '-' in
    match s with
    | "shorter" -> Some "in oklch shorter hue"
    | "longer" -> Some "in oklch longer hue"
    | "increasing" -> Some "in oklch increasing hue"
    | "decreasing" -> Some "in oklch decreasing hue"
    | _ when Parse.is_bracket_value s ->
        Some (Parse.decode_underscores (Parse.bracket_inner s))
    | _ when s <> "" && is_head s.[0] && String.for_all is_tail s ->
        Some ("in " ^ s)
    | _ -> None

  (* The typed counterpart of [interp_to_css_string]: [Some] for what a
     [Css.color_interpolation] can represent (the four hue keywords, and every
     colour space cascade's reader knows), [None] for everything
     [gradient_position_decl] cannot take directly - a bracket's arbitrary text,
     or a space Tailwind writes through without validating. Callers fall back to
     [gradient_position_decl_of_string] on [None]. *)
  let interp_to_color_interpolation s : Css.color_interpolation option =
    match s with
    | "shorter" -> Some (Css.In (Oklch, Some Css.Shorter))
    | "longer" -> Some (Css.In (Oklch, Some Css.Longer))
    | "increasing" -> Some (Css.In (Oklch, Some Css.Increasing))
    | "decreasing" -> Some (Css.In (Oklch, Some Css.Decreasing))
    | _ -> (
        match
          Cascade.Cursor.try_parse_full_err Css.Values.read_color_space
            (Cascade.Cursor.of_string s)
        with
        | Ok space -> Some (Css.In (space, None))
        | Error _ -> None)

  (** Convert a bracket gradient value to its CSS string. "125deg" → "125deg",
      "1.3rad" → "74.4845deg", "to_bottom" → "to bottom", "circle_at_center" →
      "circle at center". "100grad" stays "100grad": gradians are a distinct CSS
      angle unit from radians, even though the word ends in the same three
      letters. Reading the value as a real [<angle>] tells the two apart instead
      of matching on the "rad" suffix, which "grad" also has. *)
  let bracket_value_to_css inner =
    let decoded = Parse.decode_underscores inner in
    match
      Cascade.Cursor.try_parse_full_err Css.Values.read_angle
        (Cascade.Cursor.of_string decoded)
    with
    | Ok (Css.Rad rad) ->
        let deg = rad *. 180.0 /. Float.pi in
        (* Round to 4 decimal places to match Lightning CSS *)
        Cascade.Pp.string_of_float ~max_decimals:4 deg ^ "deg"
    | Ok _ | Error _ -> decoded

  (* [interp_decl] for a typed direction [dir_val]: [With_interpolation] typed
     when [ci_opt] covers the modifier, printing [dir_val] back to CSS rather
     than re-deriving it when it does not - the direction itself is never
     rebuilt from scratch either way. *)
  let gradient_interp_decl (dir_val : Css.gradient_direction) ci_opt interp_css
      =
    match ci_opt with
    | Some ci -> gradient_position_decl (With_interpolation (dir_val, ci))
    | None ->
        let dir_css = Css.Pp.to_string Css.pp_gradient_direction dir_val in
        gradient_position_decl_of_string (dir_css ^ " " ^ interp_css)

  (** bg-linear-to-*/interp - direction with specific interpolation. Uses 3-rule
      pattern: base → [@supports] → bg-image. *)
  let bg_linear_to_interp' dir ci_opt interp_css =
    let dir_val = to_spec dir in
    let base_decl = Var.set gradient_position_var dir_val in
    let interp_decl = gradient_interp_decl dir_val ci_opt interp_css in
    let rules = gradient_direction_rules ~base_decl ~interp_decl in
    style ~property_rules:gradient_property_rules ~rules:(Some rules) []

  (** [bg-linear-{angle}] with [@supports] for default oklab interpolation *)
  let bg_linear_angle' angle_deg =
    let dir_val : Css.gradient_direction =
      Angle (Deg (float_of_int angle_deg))
    in
    let dir_with_interp : Css.gradient_direction =
      With_interpolation (dir_val, In (Oklab, None))
    in
    let base_decl = Var.set gradient_position_var dir_val in
    let interp_decl = Var.set gradient_position_var dir_with_interp in
    let rules = gradient_direction_rules ~base_decl ~interp_decl in
    style ~property_rules:gradient_property_rules ~rules:(Some rules) []

  (** [-bg-linear-{angle}] - negated angle with [@supports] *)
  let bg_linear_angle_neg' angle_deg =
    let angle_calc : Css.gradient_direction =
      Angle (Calc (Expr (Val (Deg (float_of_int angle_deg)), Mul, Num (-1.0))))
    in
    let angle_calc_interp : Css.gradient_direction =
      With_interpolation (angle_calc, In (Oklab, None))
    in
    let base_decl = Var.set gradient_position_var angle_calc in
    let interp_decl = Var.set gradient_position_var angle_calc_interp in
    let rules = gradient_direction_rules ~base_decl ~interp_decl in
    style ~property_rules:gradient_property_rules ~rules:(Some rules) []

  (** [bg-linear-{angle}/interp] - angle with specific interpolation *)
  let bg_linear_angle_interp' angle_deg ci_opt interp_css =
    let dir_val : Css.gradient_direction =
      Angle (Deg (float_of_int angle_deg))
    in
    let base_decl = Var.set gradient_position_var dir_val in
    let interp_decl = gradient_interp_decl dir_val ci_opt interp_css in
    let rules = gradient_direction_rules ~base_decl ~interp_decl in
    style ~property_rules:gradient_property_rules ~rules:(Some rules) []

  (** [-bg-linear-{angle}/interp] *)
  let bg_linear_angle_neg_interp' angle_deg ci_opt interp_css =
    let angle_calc : Css.gradient_direction =
      Angle (Calc (Expr (Val (Deg (float_of_int angle_deg)), Mul, Num (-1.0))))
    in
    let base_decl = Var.set gradient_position_var angle_calc in
    let interp_decl = gradient_interp_decl angle_calc ci_opt interp_css in
    let rules = gradient_direction_rules ~base_decl ~interp_decl in
    style ~property_rules:gradient_property_rules ~rules:(Some rules) []

  (** [bg-linear-[value]] - bracket linear gradient (no [@supports]). Output:
      [--tw-gradient-position: {value}; background-image:
       linear-gradient(var(--tw-gradient-stops, {value}))] The value_str is the
      raw bracket inner; we convert rad→deg and _→space. *)
  let bg_linear_bracket' value_str =
    let css_val = bracket_value_to_css value_str in
    let position_decl = gradient_position_decl_of_string css_val in
    let stops_ref : Css.gradient_stop Css.var =
      Var.bracket
        ~fallback:
          (Css.Syntax_fallback
             (Cascade.Cursor.remaining (Cascade.Cursor.of_string css_val)))
        "tw-gradient-stops"
    in
    style ~property_rules:gradient_property_rules
      [ position_decl; Css.background_image (Linear_gradient_var stops_ref) ]

  (** -bg-linear-[value] - negated bracket *)
  let bg_linear_bracket_neg' value_str =
    let css_val = bracket_value_to_css value_str in
    let neg_str = "calc(" ^ css_val ^ " * -1)" in
    let position_decl = gradient_position_decl_of_string neg_str in
    let stops_ref : Css.gradient_stop Css.var =
      Var.bracket
        ~fallback:
          (Css.Syntax_fallback
             (Cascade.Cursor.remaining (Cascade.Cursor.of_string neg_str)))
        "tw-gradient-stops"
    in
    style ~property_rules:gradient_property_rules
      [ position_decl; Css.background_image (Linear_gradient_var stops_ref) ]

  (* Bare [bg-conic] / [bg-radial]: default to the [in oklab] interpolation, no
     angle. Mirrors the interp renderers with a fixed position. *)

  (** [bg-conic/interp] - conic gradient with interpolation only (no
      [@supports]) *)
  let bg_conic' () =
    let position_decl = gradient_position_decl_of_string "in oklab" in
    let stops_ref = Var.reference gradient_stops_var in
    style ~property_rules:gradient_property_rules
      [ position_decl; Css.background_image (Conic_gradient_var stops_ref) ]

  let bg_radial' () =
    let position_decl = gradient_position_decl_of_string "in oklab" in
    let stops_ref = Var.reference gradient_stops_var in
    style ~property_rules:gradient_property_rules
      [ position_decl; Css.background_image (Radial_gradient_var stops_ref) ]

  (* [bg-conic-{angle}] - conic with angle, default [in oklab] interpolation. *)
  let bg_conic_angle' angle_deg =
    let position_css = "from " ^ string_of_int angle_deg ^ "deg in oklab" in
    let position_decl = gradient_position_decl_of_string position_css in
    let stops_ref = Var.reference gradient_stops_var in
    style ~property_rules:gradient_property_rules
      [ position_decl; Css.background_image (Conic_gradient_var stops_ref) ]

  let bg_conic_interp' interp_css =
    let position_decl = gradient_position_decl_of_string interp_css in
    let stops_ref = Var.reference gradient_stops_var in
    style ~property_rules:gradient_property_rules
      [ position_decl; Css.background_image (Conic_gradient_var stops_ref) ]

  (** [bg-conic-{angle}/interp] - conic with angle and interpolation *)
  let bg_conic_angle_interp' angle_deg interp_css =
    let position_css =
      "from " ^ string_of_int angle_deg ^ "deg " ^ interp_css
    in
    let position_decl = gradient_position_decl_of_string position_css in
    let stops_ref = Var.reference gradient_stops_var in
    style ~property_rules:gradient_property_rules
      [ position_decl; Css.background_image (Conic_gradient_var stops_ref) ]

  (** [-bg-conic-{angle}/interp] *)
  let bg_conic_angle_neg_interp' angle_deg interp_css =
    let position_css =
      "from calc(" ^ string_of_int angle_deg ^ "deg * -1) " ^ interp_css
    in
    let position_decl = gradient_position_decl_of_string position_css in
    let stops_ref = Var.reference gradient_stops_var in
    style ~property_rules:gradient_property_rules
      [ position_decl; Css.background_image (Conic_gradient_var stops_ref) ]

  let bg_conic_bracket' value_str =
    let css_val = bracket_value_to_css value_str in
    let position_decl = gradient_position_decl_of_string css_val in
    let stops_ref : Css.gradient_stop Css.var =
      Var.bracket
        ~fallback:
          (Css.Syntax_fallback
             (Cascade.Cursor.remaining (Cascade.Cursor.of_string css_val)))
        "tw-gradient-stops"
    in
    style ~property_rules:gradient_property_rules
      ~metadata:[ Var.metadata gradient_position_var ]
      [ position_decl; Css.background_image (Conic_gradient_var stops_ref) ]

  (** bg-radial/interp - radial gradient with interpolation *)
  let bg_radial_interp' interp_css =
    let position_decl = gradient_position_decl_of_string interp_css in
    let stops_ref = Var.reference gradient_stops_var in
    style ~property_rules:gradient_property_rules
      [ position_decl; Css.background_image (Radial_gradient_var stops_ref) ]

  (** bg-radial-[value] - bracket radial gradient *)
  let bg_radial_bracket' value_str =
    let css_val = bracket_value_to_css value_str in
    let position_decl = gradient_position_decl_of_string css_val in
    let stops_ref : Css.gradient_stop Css.var =
      Var.bracket
        ~fallback:
          (Css.Syntax_fallback
             (Cascade.Cursor.remaining (Cascade.Cursor.of_string css_val)))
        "tw-gradient-stops"
    in
    style ~property_rules:gradient_property_rules
      [ position_decl; Css.background_image (Radial_gradient_var stops_ref) ]

  (* Gradient color with opacity - generates same structure as Tailwind: 1.
     Fallback rule with hex alpha (for scheme colors) 2. @supports block with
     color-mix using theme variable 3. Separate rule with --tw-gradient-stops *)
  let gradient_color_opacity ?theme ~prefix ~set_var ?(shade = 500) color
      opacity =
    let percent = Color.opacity_to_percent opacity in
    let color_name = Color.scheme_color_name color shade in
    let scheme = Scheme.or_default theme in

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
          let d_stops_via = Var.set gradient_stops_var (Var via_stops_ref) in
          (d_stops_via, Some d_via_stops)
      | _ ->
          let via_stops_ref =
            Var.reference_with_fallback gradient_via_stops_var fallback_stops
          in
          let d_stops = Var.set gradient_stops_var (Var via_stops_ref) in
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
      |> List.filter_map Fun.id |> Css.concat
    in

    (* Without a scheme override the palette still has a hex for the colour, and
       Tailwind emits the same fallback + [@supports] pair either way. The hex
       is the fallback's alone: the theme token keeps the palette's own value,
       as every other utility declares it, or one stop would re-colour the token
       for the whole page. A project token has no palette entry, so its value
       comes from the theme instead. *)
    let hex_pair hex =
      ( Css.hex hex,
        Css.hex
          (if Color.opacity_var_bare_of opacity <> None then hex
           else Color.hex_with_alpha hex percent) )
    in
    let value_pair () =
      match Scheme.hex_color scheme color_name with
      | Some hex -> hex_pair hex
      | None -> (
          match Color.to_oklch_opt color shade with
          | Some oklch ->
              let _, fallback =
                hex_pair (Color.rgb_to_hex (Color.oklch_to_rgb oklch))
              in
              (Color.to_css ?theme color shade, fallback)
          | None ->
              let value = Color.to_css ?theme color shade in
              ( value,
                if Color.opacity_var_bare_of opacity <> None then value
                else
                  Css.color_mix ~in_space:Srgb value Css.Transparent
                    ~percent1:percent ))
    in
    match Color.opacity_keyword color with
    | Some keyword ->
        let d_var = Var.set set_var (Color.apply_alpha opacity keyword) in
        let declarations =
          match d_via_stops_opt with
          | Some d_via_stops -> [ d_var; d_via_stops; d_stops ]
          | None -> [ d_var; d_stops ]
        in
        style ~property_rules declarations
    | None ->
        (* Tailwind outputs three rules: 1. .from-X/N { --tw-gradient-from:
           <fallback> } 2. @supports { .from-X/N { --tw-gradient-from:
           color-mix(...) } } 3. .from-X/N { --tw-gradient-stops: ... } To
           match, we put fallback in props, @supports in rules, and stops as
           separate rule in rules. *)
        let color_value, fallback_value = value_pair () in
        let d_fallback = Var.set set_var fallback_value in

        (* Theme variable for @supports block *)
        let decls, color =
          Color.bound ?theme (Color.color_var color shade) color_value
        in
        let oklab_color = Color.mix_alpha opacity color in
        let d_oklab = Var.set set_var oklab_color in

        (* Build @supports block with placeholder selector *)
        let supports_rule = Color.color_mix_supports (decls @ [ d_oklab ]) in

        (* Build stops rule with placeholder selector (will be replaced) *)
        let stops_decls =
          match d_via_stops_opt with
          | Some d_via_stops -> [ d_via_stops; d_stops ]
          | None -> [ d_stops ]
        in
        let stops_rule =
          Css.rule ~selector:(Css.Selector.class_ "_") stops_decls
        in

        (* Three separate rules: fallback, @supports, stops *)
        let fallback_rule =
          Css.rule ~selector:(Css.Selector.class_ "_") [ d_fallback ]
        in
        style ~property_rules
          ~rules:(Some [ fallback_rule; supports_rule; stops_rule ])
          []

  (** Build gradient stops declarations for a given prefix (from-/via-/to-).
      Returns (d_stops, d_via_stops_opt) *)
  let gradient_stops_decls prefix =
    let position_ref = Var.reference gradient_position_var in
    let from_ref = Var.reference gradient_from_var in
    let from_pos_ref = Var.reference gradient_from_position_var in
    let to_ref = Var.reference gradient_to_var in
    let to_pos_ref = Var.reference gradient_to_position_var in
    let fallback_stops : Css.gradient_stop =
      List
        [
          Direction (Var position_ref);
          Color_percentage (Var from_ref, Some (Var from_pos_ref), None);
          Color_percentage (Var to_ref, Some (Var to_pos_ref), None);
        ]
    in
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
        let d_stops_via = Var.set gradient_stops_var (Var via_stops_ref) in
        (d_stops_via, Some d_via_stops)
    | _ ->
        let via_stops_ref =
          Var.reference_with_fallback gradient_via_stops_var fallback_stops
        in
        let d_stops = Var.set gradient_stops_var (Var via_stops_ref) in
        (d_stops, None)

  (* All gradient @property rules *)
  let gradient_all_property_rules () =
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
    |> List.filter_map Fun.id |> Css.concat

  (** Flatten stops into declaration list *)
  let stops_as_decls d_stops d_via_stops_opt =
    match d_via_stops_opt with
    | Some d_via_stops -> [ d_via_stops; d_stops ]
    | None -> [ d_stops ]

  (** Gradient with a simple color value + stops (no opacity) *)
  let gradient_simple ~theme ~prefix ~set_var color_value extra_decls =
    let color_value = Color.resolve_bracket_css_color color_value in
    let d_stops, d_via_stops_opt = gradient_stops_decls prefix in
    let property_rules = gradient_all_property_rules () in
    let stops = stops_as_decls d_stops d_via_stops_opt in
    match Color.pre_color_mix_fallback theme color_value with
    | None ->
        let d_var = Var.set set_var (Css.minify_color color_value) in
        style ~property_rules (extra_decls @ [ d_var ] @ stops)
    | Some fallback ->
        let fallback_decl = Var.set set_var fallback in
        let enhanced_decl = Var.set set_var color_value in
        let self decls = Css.rule ~selector:(Css.Selector.class_ "_") decls in
        let supports =
          Css.supports ~condition:Color.color_mix_supports_condition
            [ self [ enhanced_decl ] ]
        in
        style ~property_rules
          ~rules:
            (Some
               [ self (extra_decls @ [ fallback_decl ]); supports; self stops ])
          []

  (* A token-stream stop, forwarded as written. Under a modifier it takes the
     mix an [--alpha()] call expands to, and when that mix reads a custom
     property or [currentcolor], the pair Tailwind's polyfill writes: what a
     browser without [color-mix()] reads in the open, the mix behind the
     guard. *)
  let gradient_raw ~theme ~prefix ~set_var value opacity =
    let d_stops, d_via_stops_opt = gradient_stops_decls prefix in
    let stops = stops_as_decls d_stops d_via_stops_opt in
    let property_rules = gradient_all_property_rules () in
    let metadata = [ Var.metadata set_var ] in
    let set v =
      Css.custom_property ~layer:"utilities" (Var.css_name set_var) v
    in
    let mixed =
      match opacity with
      | Stdlib.Option.None -> value
      | Stdlib.Option.Some opacity ->
          Option.value ~default:value
            (Parse.alpha_mix ~alpha:(Color.opacity_percentage opacity) value)
    in
    match Parse.color_mix_fallback ~resolve:(Color.theme_token theme) mixed with
    | Stdlib.Option.None -> style ~property_rules ~metadata (set mixed :: stops)
    | Stdlib.Option.Some in_the_open ->
        let self decls = Css.rule ~selector:(Css.Selector.class_ "_") decls in
        style ~property_rules ~metadata
          ~rules:
            (Some
               [
                 self [ set in_the_open ];
                 Color.color_mix_supports [ set mixed ];
                 self stops;
               ])
          []

  (* Gradient with opacity: either a static value, or a fallback followed by the
     authored mix under [@supports], then the stop composition. *)
  let gradient_with_opacity ~theme ~prefix ~set_var color opacity =
    match Color.bracket_color_opacity ~theme color opacity with
    | Color.Folded value -> gradient_simple ~theme ~prefix ~set_var value []
    | Color.Guarded { fallback; mixed } ->
        let d_fallback = Var.set set_var fallback in
        let d_oklab = Var.set set_var mixed in
        let self decls = Css.rule ~selector:(Css.Selector.class_ "_") decls in
        let supports_rule =
          Css.supports ~condition:Color.color_mix_supports_condition
            [ self [ d_oklab ] ]
        in
        let d_stops, d_via_stops_opt = gradient_stops_decls prefix in
        let stops = stops_as_decls d_stops d_via_stops_opt in
        let property_rules = gradient_all_property_rules () in
        style ~property_rules
          ~rules:(Some [ self [ d_fallback ]; supports_rule; self stops ])
          []

  (** Convert gradient target to set_var and prefix *)
  let gradient_target_info = function
    | From -> ("from-", gradient_from_var, gradient_from_position_var)
    | Via -> ("via-", gradient_via_var, gradient_via_position_var)
    | To -> ("to-", gradient_to_var, gradient_to_position_var)

  (* Shared @property rules for gradient positions *)
  (* A gradient stop-position utility (from-10% etc.) registers the whole
     --tw-gradient-* @property family, like the colour utilities do, matching
     Tailwind (which declares the gradient properties as a unit). *)
  let gradient_position_property_rules () =
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
    |> List.filter_map Fun.id |> Css.concat

  (* Gradient position from a length_percentage value *)
  let gradient_position_style pos_var (value : Css.length_percentage) =
    let d_var = Var.set pos_var value in
    style ~property_rules:(gradient_position_property_rules ()) [ d_var ]

  (* The one reader for a bracket stop position, used both to decide that a
     bracket is a position and to convert it, so the two cannot disagree. A stop
     takes a [<length-percentage>]; Tailwind reads any other bracket - a
     keyword, a unitless number, the docs' [<value>] placeholder - as a colour,
     and a colour spelled that way has no typed rendering here, so it is
     refused. *)
  let parse_bracket_position_value (inner : string) :
      Css.length_percentage option =
    let var_ref value =
      let bare = Parse.extract_var_name value in
      let vr : Css.length_percentage Css.var = Var.bracket bare in
      (Css.Var vr : Css.length_percentage)
    in
    (* A data-type hint says how to read the value written after it; only a
       var() reference there names a custom property. *)
    let hinted =
      match Parse.data_type_hint inner with
      | Some (("percentage" | "length"), value) -> Some value
      | _ -> None
    in
    let value = Option.value hinted ~default:inner in
    if Parse.is_var value then Some (var_ref value)
    else Parse.arbitrary_length_percentage value

  (** Convert a var string to a typed CSS color value *)
  let color_var_ref v : Css.color =
    let bare = Parse.extract_var_name v in
    let vr : Css.color Css.var = Var.bracket bare in
    Var vr

  (* A [color:] hint says how to read the value written after it; only a var()
     reference there names a custom property, so any other spelling has to be a
     colour the reader takes. *)

  (** Convert a plain colour source to a Css.color *)
  let css_color_of_plain : Color_source.plain -> Css.color = function
    | Color_source.Current -> Css.Current
    | Color_source.Inherit -> Css.Inherit
    | Color_source.Transparent -> Css.Transparent
    (* The bracket's own spelling is the one Tailwind writes back, so [#f00]
       stays three digits here as it does for every other colour family. *)
    | Color_source.Bracket_hex h -> Color.authored_hex h
    | Color_source.Bracket_var v -> color_var_ref v
    | Color_source.Bracket_color_var v when Parse.is_var v -> color_var_ref v
    | Color_source.Bracket_color_var v | Color_source.Bracket_color v -> (
        match Color.parse_bracket_color v with
        | Some c -> c
        | None -> Css.Transparent)

  let to_style theme =
    let gradient_color_opacity ~prefix ~set_var ?(shade = 500) color opacity =
      gradient_color_opacity ~theme ~prefix ~set_var ~shade color opacity
    in
    function
    | Bg_gradient_to dir -> bg_gradient_to' dir
    | Gradient_color (target, src) -> (
        let prefix, set_var, _pos_var = gradient_target_info target in
        (* A palette colour goes through the scheme, which gives it a theme
           variable to read. *)
        match src with
        | Color_source.Palette (color, shade, None) ->
            gradient_color ~theme ~prefix ~set_var ~shade color
        | Color_source.Palette (color, shade, Some opacity) ->
            gradient_color_opacity ~prefix ~set_var ~shade color opacity
        | Color_source.Plain (source, opacity) -> (
            (* A keyword or a bracket value: the colour is known without the
               scheme. *)
            let color = css_color_of_plain source in
            match opacity with
            | None -> gradient_simple ~theme ~prefix ~set_var color []
            | Some opacity ->
                gradient_with_opacity ~theme ~prefix ~set_var color opacity))
    | Gradient_raw (target, _, value, opacity) ->
        let prefix, set_var, _ = gradient_target_info target in
        gradient_raw ~theme ~prefix ~set_var value opacity
    | Gradient_stop_position (target, src) -> (
        let _prefix, _set_var, pos_var = gradient_target_info target in
        match src with
        | Percent p -> gradient_position_style pos_var (Pct p)
        | Bracket (_, value) -> gradient_position_style pos_var value)
    | Via_none ->
        let property_rules =
          match Var.property_rule gradient_via_stops_var with
          | Some r -> r
          | None -> Css.empty
        in
        style ~property_rules [ Var.binding_initial gradient_via_stops_var ]
    | Bg_linear_to dir -> bg_linear_to' dir
    | Bg_linear_to_interp (dir, interp, css) ->
        bg_linear_to_interp' dir (interp_to_color_interpolation interp) css
    | Bg_linear_angle n -> bg_linear_angle' n
    | Bg_linear_angle_neg n -> bg_linear_angle_neg' n
    | Bg_linear_angle_interp (n, interp, css) ->
        bg_linear_angle_interp' n (interp_to_color_interpolation interp) css
    | Bg_linear_angle_neg_interp (n, interp, css) ->
        bg_linear_angle_neg_interp' n (interp_to_color_interpolation interp) css
    | Bg_linear_bracket v -> bg_linear_bracket' v
    | Bg_linear_bracket_neg v -> bg_linear_bracket_neg' v
    | Bg_conic -> bg_conic' ()
    | Bg_conic_angle n -> bg_conic_angle' n
    | Bg_conic_angle_neg n -> bg_conic_angle' (-n)
    | Bg_conic_interp (_, css) -> bg_conic_interp' css
    | Bg_conic_angle_interp (n, _, css) -> bg_conic_angle_interp' n css
    | Bg_conic_angle_neg_interp (n, _, css) -> bg_conic_angle_neg_interp' n css
    | Bg_conic_bracket value -> bg_conic_bracket' value
    | Bg_radial -> bg_radial' ()
    | Bg_radial_interp (_, css) -> bg_radial_interp' css
    | Bg_radial_bracket v -> bg_radial_bracket' v

  (** Split a string on the first '/' into (base, modifier_opt). E.g. "r/oklab"
      → ("r", Some "oklab"), "45" → ("45", None) *)
  let split_mod s =
    match String.index_opt s '/' with
    | Some i ->
        (String.sub s 0 i, Some (String.sub s (i + 1) (String.length s - i - 1)))
    | None -> (s, None)

  (* A [#] stop only names a colour when what follows is a hex spelling. The
     stop keeps the text after the [#] and hands it to the raising [Css.hex]
     when the sheet is rendered, so the parser has to decide here. *)
  let is_bracket_hex inner =
    String.starts_with ~prefix:"#" inner && Option.is_some (Css.hex_opt inner)

  let parse_gradient_color ?theme target rest =
    let gc src = Ok (Gradient_color (target, src)) in
    let plain ?opacity source = gc (Color_source.Plain (source, opacity)) in
    let palette ?opacity color shade =
      gc (Color_source.Palette (color, shade, opacity))
    in
    let gp src = Ok (Gradient_stop_position (target, src)) in
    match rest with
    (* Keywords *)
    | [ "current" ] -> plain Color_source.Current
    | [ current_str ] when String.starts_with ~prefix:"current/" current_str ->
        let _, opacity = Color.parse_opacity_modifier ?theme current_str in
        plain ~opacity Color_source.Current
    | [ "inherit" ] -> plain Color_source.Inherit
    | [ "transparent" ] -> plain Color_source.Transparent
    (* Percentage positions: from-0%, from-100% (integer only) *)
    | [ pct_str ] when String.ends_with ~suffix:"%" pct_str -> (
        let num_s = String.sub pct_str 0 (String.length pct_str - 1) in
        match Parse.decimal_int num_s with
        | Some p -> gp (Percent (float_of_int p))
        | None -> Error (`Msg "Invalid gradient position"))
    (* Bracket with opacity: [#0088cc]/50, [#0088cc]/[0.5], [var(--x)]/50 *)
    | [ bracket_opacity ] when Color.has_opacity bracket_opacity -> (
        let base, opacity_opt =
          Color.parse_opacity_modifier ?theme bracket_opacity
        in
        match opacity_opt with
        | Color.No_opacity when Parse.is_bracket_value bracket_opacity -> (
            (* No valid opacity found — treat as plain bracket *)
            let inner = Parse.bracket_inner bracket_opacity in
            match Color.bracket_color_hint inner with
            | Some var_str -> plain (Color_source.Bracket_color_var var_str)
            | None -> (
                if is_bracket_hex inner then
                  plain
                    (Color_source.Bracket_hex
                       (String.sub inner 1 (String.length inner - 1)))
                else if Parse.is_var inner then
                  plain (Color_source.Bracket_var inner)
                else
                  match parse_bracket_position_value inner with
                  | Some value -> gp (Bracket (inner, value))
                  | None -> (
                      match Parse.arbitrary_declaration_value inner with
                      | Some value ->
                          Ok (Gradient_raw (target, inner, value, None))
                      | None -> Error (`Msg "Invalid gradient stop value"))))
        | Color.No_opacity ->
            Error (`Msg "Invalid gradient bracket with opacity")
        | opacity when Parse.is_bracket_value base -> (
            let inner = Parse.bracket_inner base in
            match Color.bracket_color_hint inner with
            | Some var_str ->
                plain ~opacity (Color_source.Bracket_color_var var_str)
            | None -> (
                if is_bracket_hex inner then
                  plain ~opacity
                    (Color_source.Bracket_hex
                       (String.sub inner 1 (String.length inner - 1)))
                else if Parse.is_var inner then
                  plain ~opacity (Color_source.Bracket_var inner)
                else if Color.parse_bracket_color inner <> None then
                  plain ~opacity (Color_source.Bracket_color inner)
                else if parse_bracket_position_value inner <> None then
                  (* A length or a percentage is the stop's position, and a
                     position takes no modifier. *)
                  Error (`Msg "A gradient stop position takes no modifier")
                else
                  match Parse.arbitrary_declaration_value inner with
                  | Some value ->
                      Ok (Gradient_raw (target, inner, value, Some opacity))
                  | None -> Error (`Msg "Invalid gradient bracket with opacity")
                ))
        | _ -> (
            (* Named color with opacity *)
            match Color.shade_and_opacity_of_strings ?theme rest with
            | Ok (color, shade, opacity) -> palette ~opacity color shade
            | Error e -> Error e))
    (* Bracket notation without opacity *)
    | [ bracket ] when Parse.is_bracket_value bracket -> (
        let inner = Parse.bracket_inner bracket in
        match Color.bracket_color_hint inner with
        | Some var_str -> plain (Color_source.Bracket_color_var var_str)
        | None -> (
            if is_bracket_hex inner then
              plain
                (Color_source.Bracket_hex
                   (String.sub inner 1 (String.length inner - 1)))
            else if Parse.is_var inner then
              plain (Color_source.Bracket_var inner)
            else if Color.parse_bracket_color inner <> None then
              plain (Color_source.Bracket_color inner)
            else
              match parse_bracket_position_value inner with
              | Some value -> gp (Bracket (inner, value))
              | None -> (
                  match Parse.arbitrary_declaration_value inner with
                  | Some value -> Ok (Gradient_raw (target, inner, value, None))
                  | None -> Error (`Msg "Invalid gradient stop value"))))
    (* Named color with opacity via Color.has_opacity on rest *)
    | _ when List.exists Color.has_opacity rest -> (
        match Color.shade_and_opacity_of_strings ?theme rest with
        | Ok (color, shade, opacity) -> palette ~opacity color shade
        | Error e -> Error e)
    (* Named color *)
    | _ -> (
        match Color.shade_of_strings ?theme rest with
        | Ok (color, shade) -> palette color shade
        | Error _ -> Error (`Msg "Invalid gradient color"))

  (* [of_bg_parts parts] is [Some] for a [bg-] class of a gradient shape,
     whether or not its value reads, so a malformed one is refused here rather
     than taken for a background colour, and [None] for any other shape. *)
  let of_bg_parts parts : (_, [ `Msg of string ]) result option =
    match parts with
    | [ "bg"; "gradient"; "to"; dir ] ->
        Option.map (fun dir -> Ok (Bg_gradient_to dir)) (parse_direction dir)
    (* bg-linear-to-* direction utilities (with optional /interp modifier) *)
    | [ "bg"; "linear"; "to"; dir_mod ] ->
        Some
          (let dir_s, interp_opt = split_mod dir_mod in
           match (parse_direction dir_s, interp_opt) with
           | Some dir, None -> Ok (Bg_linear_to dir)
           | Some dir, Some interp -> (
               match interp_to_css_string interp with
               | Some css -> Ok (Bg_linear_to_interp (dir, interp, css))
               | None ->
                   Error (`Msg ("Invalid gradient interpolation: " ^ interp)))
           | None, _ -> Error (`Msg ("Unknown direction: " ^ dir_s)))
    (* bg-linear-[value] - bracket linear gradient *)
    | [ "bg"; "linear"; bracket ]
      when Parse.is_bracket_value bracket
           && Parse.is_declaration_value
                (Parse.decode_underscores (Parse.bracket_inner bracket)) ->
        let inner = Parse.bracket_inner bracket in
        Some (Ok (Bg_linear_bracket inner))
    (* bg-linear-{angle} and bg-linear-{angle}/interp *)
    | [ "bg"; "linear"; angle_mod ] ->
        Some
          (let angle_s, interp_opt = split_mod angle_mod in
           match (Parse.decimal_int angle_s, interp_opt) with
           | Some n, None -> Ok (Bg_linear_angle n)
           | Some n, Some interp -> (
               match interp_to_css_string interp with
               | Some css -> Ok (Bg_linear_angle_interp (n, interp, css))
               | None ->
                   Error (`Msg ("Invalid gradient interpolation: " ^ interp)))
           | None, _ -> Error (`Msg ("Invalid bg-linear angle: " ^ angle_mod)))
    (* -bg-linear-[value] - negated bracket linear gradient (only angles) *)
    | [ ""; "bg"; "linear"; bracket ] when Parse.is_bracket_value bracket ->
        let inner = Parse.bracket_inner bracket in
        (* Only accept angle values for negation: 125deg, 1.3rad, 100grad, etc.
           A [String.ends_with ~suffix:"rad"] check also matches "grad" (which
           ends in "rad" too), stripping "100grad" down to "100g" and rejecting
           it outright where Tailwind accepts it; reading it as a real CSS angle
           tells "grad" and "rad" apart. *)
        let is_angle =
          match
            Cascade.Cursor.try_parse_full_err Css.Values.read_angle
              (Cascade.Cursor.of_string (Parse.decode_underscores inner))
          with
          | Ok _ -> true
          | Error _ -> false
        in
        Some
          (if is_angle then Ok (Bg_linear_bracket_neg inner)
           else Error (`Msg ("Invalid -bg-linear bracket value: " ^ inner)))
    (* -bg-linear-{angle} and -bg-linear-{angle}/interp *)
    | [ ""; "bg"; "linear"; angle_mod ] ->
        Some
          (let angle_s, interp_opt = split_mod angle_mod in
           match (Parse.decimal_int angle_s, interp_opt) with
           | Some n, None -> Ok (Bg_linear_angle_neg n)
           | Some n, Some interp -> (
               match interp_to_css_string interp with
               | Some css -> Ok (Bg_linear_angle_neg_interp (n, interp, css))
               | None ->
                   Error (`Msg ("Invalid gradient interpolation: " ^ interp)))
           | None, _ -> Error (`Msg ("Invalid -bg-linear angle: " ^ angle_mod)))
    (* bg-conic - bare conic gradient *)
    | [ "bg"; "conic" ] -> Some (Ok Bg_conic)
    | [ "bg"; "conic"; bracket ] when Parse.is_bracket_value bracket ->
        let inner = Parse.bracket_inner bracket in
        Some
          (match Parse.arbitrary_declaration_value inner with
          | Some _ -> Ok (Bg_conic_bracket inner)
          | None -> Error (`Msg ("Invalid bg-conic value: " ^ inner)))
    (* bg-conic/interp - conic gradient with modifier only *)
    | [ "bg"; conic_mod ]
      when String.starts_with ~prefix:"conic/" conic_mod
           && String.length conic_mod > 6 ->
        let interp = String.sub conic_mod 6 (String.length conic_mod - 6) in
        Some
          (match interp_to_css_string interp with
          | Some css -> Ok (Bg_conic_interp (interp, css))
          | None -> Error (`Msg ("Invalid gradient interpolation: " ^ interp)))
    (* bg-conic-{angle} and bg-conic-{angle}/interp *)
    | [ "bg"; "conic"; angle_mod ] ->
        Some
          (let angle_s, interp_opt = split_mod angle_mod in
           match (Parse.decimal_int angle_s, interp_opt) with
           | Some n, None -> Ok (Bg_conic_angle n)
           | Some n, Some interp -> (
               match interp_to_css_string interp with
               | Some css -> Ok (Bg_conic_angle_interp (n, interp, css))
               | None ->
                   Error (`Msg ("Invalid gradient interpolation: " ^ interp)))
           | _ -> Error (`Msg ("Invalid bg-conic angle: " ^ angle_mod)))
    (* -bg-conic-{angle} and -bg-conic-{angle}/interp *)
    | [ ""; "bg"; "conic"; angle_mod ] ->
        Some
          (let angle_s, interp_opt = split_mod angle_mod in
           match (Parse.decimal_int angle_s, interp_opt) with
           | Some n, None -> Ok (Bg_conic_angle_neg n)
           | Some n, Some interp -> (
               match interp_to_css_string interp with
               | Some css -> Ok (Bg_conic_angle_neg_interp (n, interp, css))
               | None ->
                   Error (`Msg ("Invalid gradient interpolation: " ^ interp)))
           | _ -> Error (`Msg ("Invalid -bg-conic angle: " ^ angle_mod)))
    (* bg-radial - bare radial gradient *)
    | [ "bg"; "radial" ] -> Some (Ok Bg_radial)
    (* bg-radial/interp - radial gradient with modifier only *)
    | [ "bg"; radial_mod ]
      when String.starts_with ~prefix:"radial/" radial_mod
           && String.length radial_mod > 7 ->
        let interp = String.sub radial_mod 7 (String.length radial_mod - 7) in
        Some
          (match interp_to_css_string interp with
          | Some css -> Ok (Bg_radial_interp (interp, css))
          | None -> Error (`Msg ("Invalid gradient interpolation: " ^ interp)))
    (* bg-radial-[value] - bracket radial gradient *)
    | [ "bg"; "radial"; bracket ]
      when Parse.is_bracket_value bracket
           && Parse.is_declaration_value
                (Parse.decode_underscores (Parse.bracket_inner bracket)) ->
        let inner = Parse.bracket_inner bracket in
        Some (Ok (Bg_radial_bracket inner))
    | _ -> None

  let owns_bg_class class_name =
    Option.is_some (of_bg_parts (Parse.split_class class_name))

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match of_bg_parts parts with
    | Some result -> result
    | None -> (
        match parts with
        | "from" :: rest -> parse_gradient_color ~theme From rest
        | [ "via"; "none" ] -> Ok Via_none
        | "via" :: rest -> parse_gradient_color ~theme Via rest
        | "to" :: rest -> parse_gradient_color ~theme To rest
        | _ -> Error (`Msg "Not a gradient utility"))

  (* background-image is the one property a gradient sets that is not one of its
     own variables. [bg-linear-to-b] writes it inside the rule it carries beside
     its @supports fallback, so it names the family without taking the
     property's slot from [bg-none], which writes it on the element. *)
  let examples = [ Bg_linear_to Bottom ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let bg_gradient_to dir = utility (Bg_gradient_to dir)

let from_color ?(shade = 500) color =
  Color.check_shade ~utility:"from_color" color shade;
  utility (Gradient_color (From, Color_source.Palette (color, shade, None)))

let via_color ?(shade = 500) color =
  Color.check_shade ~utility:"via_color" color shade;
  utility (Gradient_color (Via, Color_source.Palette (color, shade, None)))

let to_color ?(shade = 500) color =
  Color.check_shade ~utility:"to_color" color shade;
  utility (Gradient_color (To, Color_source.Palette (color, shade, None)))
