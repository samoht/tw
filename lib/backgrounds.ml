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
  (* Helper to check if a string contains an opacity modifier *)
  let has_opacity s = String.contains s '/'

  (* An arbitrary url() value may already carry its own quotes (url('a.png') /
     url("a.png")). Drop a matching outer pair so the typed [Url] pretty-printer
     canonicalises it instead of double-wrapping it into the broken
     url("'a.png'"). *)
  let strip_outer_quotes s =
    let n = String.length s in
    if n >= 2 && (s.[0] = '\'' || s.[0] = '"') && s.[n - 1] = s.[0] then
      String.sub s 1 (n - 2)
    else s

  (* The named background positions, as [bg-<position>] spells them. *)
  module Position = struct
    type t =
      | Bottom
      | Bottom_left
      | Bottom_right
      | Center
      | Left
      | Left_bottom
      | Left_top
      | Right
      | Right_bottom
      | Right_top
      | Top
      | Top_left
      | Top_right
  end

  (* Gradient color source - shared by from/via/to *)
  module Color_source = struct
    type t =
      | Named of Color.color * int
      | Named_opacity of Color.color * int * Color.opacity_modifier
      | Current
      | Current_opacity of Color.opacity_modifier
      | Inherit
      | Transparent
      | Bracket_hex of string
      | Bracket_hex_opacity of string * Color.opacity_modifier
      | Bracket_color_var of string
      | Bracket_color_var_opacity of string * Color.opacity_modifier
      | Bracket_var of string
      | Bracket_var_opacity of string * Color.opacity_modifier
      | Bracket_color of string
      | Bracket_color_opacity of string * Color.opacity_modifier
  end

  (* Gradient position source *)
  (* A bracket stop position carries the author's text alongside the value it
     denotes, so the class name is spelled exactly as it was written. *)
  type gradient_position_source =
    | Percent of float
    | Bracket of string * Css.length_percentage

  type gradient_target = From | Via | To

  type t =
    | Bg of Color.color * int
    | Bg_gradient_to of direction
    | Gradient_color of gradient_target * Color_source.t
    | Gradient_stop_position of gradient_target * gradient_position_source
    | Via_none (* via-none: clears the gradient's via stops *)
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
    | Bg_position of Position.t
    (* Bracket notation: bg-[contain], bg-[cover] → background-size *)
    | Bg_bracket_contain
    | Bg_bracket_cover
    (* Bracket notation: bg-[length:...] or bg-[size:...] → background-size *)
    | Bg_bracket_size of string
    (* Bracket notation: bg-[50%], bg-[120px], bg-[120px_120px] →
       background-position *)
    | Bg_bracket_position of string * Css.position_value
    (* Bracket notation: bg-[position:...] → background-position *)
    | Bg_bracket_typed_position of string * Css.position_value
    (* Bracket notation: bg-[color:var(--x)] → background-color *)
    | Bg_bracket_color_var of string
    (* Bracket notation: bg-[var(--x)] → background-color *)
    | Bg_bracket_var of string
    (* Bracket notation: bg-[image:var(--x)] → background-image *)
    | Bg_bracket_image_var of string
    (* Bracket notation: bg-[image:<gradient/literal>] → background-image, with
       the image: hint kept in the class name *)
    | Bg_bracket_image of string * Css.background_image
    (* Bracket notation: bg-[url(...)] → background-image *)
    | Bg_bracket_url of string
    (* Bracket notation: bg-[image:url(...)] → background-image (image: hint
       forces background-image; selector keeps the image: form) *)
    | Bg_bracket_image_url of string
    (* Bracket notation: bg-[url:var(--x)] → background-image *)
    | Bg_bracket_url_var of string
    (* Bracket notation: bg-[linear-gradient(...)] → background-image *)
    | Bg_bracket_linear_gradient of string * Css.background_image
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
    (* bg-radial - bare radial gradient (in oklab) *)
    | Bg_radial
    (* bg-radial/interp - radial gradient with interpolation *)
    | Bg_radial_interp of string * string
    (* bg-radial-[value] - bracket radial gradient value *)
    | Bg_radial_bracket of string
    (* Bracket color/var with opacity *)
    | Bg_bracket_color_var_opacity of string * Color.opacity_modifier
    | Bg_bracket_var_opacity of string * Color.opacity_modifier
    (* Bracket color: bg-[#0088cc], bg-[rgba(48,163,0,0.14)], etc. Stores
       original string for class name roundtrip and typed Css.color *)
    | Bg_bracket_color of string * Css.color
    (* Bracket color with opacity: bg-[#0088cc]/50, bg-[rgba(...)]/50, etc. *)
    | Bg_bracket_color_opacity of string * Css.color * Color.opacity_modifier
    (* bg-current *)
    | Bg_current
    (* bg-current with opacity: bg-current/50, bg-current/[0.5] *)
    | Bg_current_opacity of Color.opacity_modifier
    (* bg-transparent *)
    | Bg_transparent
    (* Named color with opacity: bg-red-500/50, bg-blue-500/[0.5] *)
    | Bg_opacity of Color.color * int * Color.opacity_modifier
    (* bg-[length:...] - explicit length prefix for bracket size *)
    | Bg_bracket_length of string
    (* bg-position-[...] bracket notation *)
    | Bg_position_bracket of string * Css.position_value
    (* bg-size-[...] bracket notation *)
    | Bg_size_bracket of string

  type Utility.base += Self of t

  let to_class (t : t) =
    match t with
    | Bg (color, shade) ->
        if Color.is_shadeless color then "bg-" ^ Color.pp color
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
    | Via_none -> "via-none"
    | Gradient_color (target, src) ->
        let prefix =
          match target with From -> "from-" | Via -> "via-" | To -> "to-"
        in
        let color_class = function
          | Color_source.Named (color, shade) ->
              if Color.is_shadeless color then Color.pp color
              else Color.pp color ^ "-" ^ string_of_int shade
          | Color_source.Named_opacity (color, shade, opacity) ->
              let base =
                if Color.is_shadeless color then Color.pp color
                else Color.pp color ^ "-" ^ string_of_int shade
              in
              base ^ Color.opacity_suffix opacity
          | Color_source.Current -> "current"
          | Color_source.Current_opacity opacity ->
              "current" ^ Color.opacity_suffix opacity
          | Color_source.Inherit -> "inherit"
          | Color_source.Transparent -> "transparent"
          | Color_source.Bracket_hex h -> "[#" ^ h ^ "]"
          | Color_source.Bracket_hex_opacity (h, opacity) ->
              "[#" ^ h ^ "]" ^ Color.opacity_suffix opacity
          | Color_source.Bracket_color_var v -> "[color:" ^ v ^ "]"
          | Color_source.Bracket_color_var_opacity (v, opacity) ->
              "[color:" ^ v ^ "]" ^ Color.opacity_suffix opacity
          | Color_source.Bracket_var v -> "[" ^ v ^ "]"
          | Color_source.Bracket_var_opacity (v, opacity) ->
              "[" ^ v ^ "]" ^ Color.opacity_suffix opacity
          | Color_source.Bracket_color v -> "[" ^ v ^ "]"
          | Color_source.Bracket_color_opacity (v, opacity) ->
              "[" ^ v ^ "]" ^ Color.opacity_suffix opacity
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
    | Bg_position Position.Bottom -> "bg-bottom"
    | Bg_position Position.Bottom_left -> "bg-bottom-left"
    | Bg_position Position.Bottom_right -> "bg-bottom-right"
    | Bg_position Position.Center -> "bg-center"
    | Bg_position Position.Left -> "bg-left"
    | Bg_position Position.Left_bottom -> "bg-left-bottom"
    | Bg_position Position.Left_top -> "bg-left-top"
    | Bg_position Position.Right -> "bg-right"
    | Bg_position Position.Right_bottom -> "bg-right-bottom"
    | Bg_position Position.Right_top -> "bg-right-top"
    | Bg_position Position.Top -> "bg-top"
    | Bg_position Position.Top_left -> "bg-top-left"
    | Bg_position Position.Top_right -> "bg-top-right"
    | Bg_bracket_contain -> "bg-[contain]"
    | Bg_bracket_cover -> "bg-[cover]"
    | Bg_bracket_size v -> "bg-[size:" ^ v ^ "]"
    | Bg_bracket_length v -> "bg-[length:" ^ v ^ "]"
    | Bg_bracket_position (v, _) -> "bg-[" ^ v ^ "]"
    | Bg_bracket_typed_position (v, _) -> "bg-[position:" ^ v ^ "]"
    | Bg_bracket_color_var v -> "bg-[color:" ^ v ^ "]"
    | Bg_bracket_var v -> "bg-[" ^ v ^ "]"
    | Bg_bracket_image_var v -> "bg-[image:" ^ v ^ "]"
    | Bg_bracket_image (v, _) -> "bg-[image:" ^ v ^ "]"
    | Bg_bracket_url v -> "bg-[url(" ^ v ^ ")]"
    | Bg_bracket_image_url v -> "bg-[image:url(" ^ v ^ ")]"
    | Bg_bracket_url_var v -> "bg-[url:" ^ v ^ "]"
    | Bg_bracket_linear_gradient (v, _) -> "bg-[" ^ v ^ "]"
    | Bg_bracket_color_var_opacity (v, opacity) ->
        "bg-[color:" ^ v ^ "]" ^ Color.opacity_suffix opacity
    | Bg_bracket_var_opacity (v, opacity) ->
        "bg-[" ^ v ^ "]" ^ Color.opacity_suffix opacity
    | Bg_bracket_color (orig, _) -> "bg-[" ^ orig ^ "]"
    | Bg_bracket_color_opacity (orig, _, opacity) ->
        "bg-[" ^ orig ^ "]" ^ Color.opacity_suffix opacity
    | Bg_current -> "bg-current"
    | Bg_current_opacity opacity -> "bg-current" ^ Color.opacity_suffix opacity
    | Bg_transparent -> "bg-transparent"
    | Bg_opacity (color, shade, opacity) ->
        let base =
          if Color.is_shadeless color then "bg-" ^ Color.pp color
          else "bg-" ^ Color.pp color ^ "-" ^ string_of_int shade
        in
        base ^ Color.opacity_suffix opacity
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
    | Bg_radial -> "bg-radial"
    | Bg_radial_interp (interp, _) -> "bg-radial/" ^ interp
    | Bg_radial_bracket v -> "bg-radial-[" ^ v ^ "]"
    | Bg_position_bracket (v, _) -> "bg-position-[" ^ v ^ "]"
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

  open Style
  open Css

  let name = "backgrounds"

  (* Tailwind sorts a layer by the rank its property table gives the property
     each rule writes. The background utilities take two stretches of that
     table: background-color (198) and background-image (199) with the gradient
     variables (202-208), then background-size (251) through background-origin
     (256). The second stretch falls between the mask-image utilities and
     mask-composite, so it sorts with the masks. *)
  let property_rank = function
    | Bg _ | Bg_opacity _ | Bg_inherit | Bg_current | Bg_current_opacity _
    | Bg_transparent | Bg_bracket_color _ | Bg_bracket_color_opacity _
    | Bg_bracket_color_var _ | Bg_bracket_color_var_opacity _ | Bg_bracket_var _
    | Bg_bracket_var_opacity _ ->
        198
    | Bg_gradient_to _ | Bg_linear_to _ | Bg_linear_to_interp _
    | Bg_linear_angle _ | Bg_linear_angle_neg _ | Bg_linear_angle_interp _
    | Bg_linear_angle_neg_interp _ | Bg_linear_bracket _
    | Bg_linear_bracket_neg _ | Bg_conic | Bg_conic_angle _
    | Bg_conic_angle_neg _ | Bg_conic_interp _ | Bg_conic_angle_interp _
    | Bg_conic_angle_neg_interp _ | Bg_radial | Bg_radial_interp _
    | Bg_radial_bracket _ | Bg_bracket_image_var _ | Bg_bracket_image _
    | Bg_bracket_linear_gradient _ | Bg_bracket_url _ | Bg_bracket_image_url _
    | Bg_bracket_url_var _ | Bg_none ->
        199
    | Via_none -> 202
    | Gradient_color (From, _) -> 203
    | Gradient_stop_position (From, _) -> 204
    | Gradient_color (Via, _) -> 205
    | Gradient_stop_position (Via, _) -> 206
    | Gradient_color (To, _) -> 207
    | Gradient_stop_position (To, _) -> 208
    | Bg_auto | Bg_cover | Bg_contain | Bg_bracket_contain | Bg_bracket_cover
    | Bg_bracket_size _ | Bg_bracket_length _ | Bg_size_bracket _ ->
        251
    | Bg_fixed | Bg_local | Bg_scroll -> 252
    | Bg_clip_border | Bg_clip_padding | Bg_clip_content | Bg_clip_text -> 253
    | Bg_position _ | Bg_bracket_position _ | Bg_bracket_typed_position _
    | Bg_position_bracket _ ->
        254
    | Bg_repeat | Bg_no_repeat | Bg_repeat_x | Bg_repeat_y | Bg_repeat_round
    | Bg_repeat_space ->
        255
    | Bg_origin_border | Bg_origin_padding | Bg_origin_content -> 256

  (* The masks open their band at mask-image; every rank past it belongs to
     them, and the background properties in that stretch interleave. *)
  let mask_image_rank = 209
  let background_image_rank = 199
  let priority t = if property_rank t > mask_image_rank then 21 else 20

  (* Rules sharing a slot sort by declaration count, most first, then by class
     name. In background-image's slot the linear directions and angles carry an
     @supports fallback the other gradient functions do not, and the plain
     images write nothing past background-image, so they close it. *)
  let suborder t =
    let rank = property_rank t in
    if rank <> background_image_rank then Utility.Property_order.last rank
    else
      match t with
      | Bg_gradient_to _ | Bg_linear_to _ | Bg_linear_to_interp _
      | Bg_linear_angle _ | Bg_linear_angle_neg _ | Bg_linear_angle_interp _
      | Bg_linear_angle_neg_interp _ ->
          Utility.Property_order.slot rank
      | Bg_linear_bracket _ | Bg_linear_bracket_neg _ | Bg_conic
      | Bg_conic_angle _ | Bg_conic_angle_neg _ | Bg_conic_interp _
      | Bg_conic_angle_interp _ | Bg_conic_angle_neg_interp _ | Bg_radial
      | Bg_radial_interp _ | Bg_radial_bracket _ ->
          Utility.Property_order.slot rank + 1
      | _ -> Utility.Property_order.last rank

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
    fst (Var.binding gradient_position_var dir)

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
    let dir_val = With_interpolation (to_spec dir, In_oklab) in
    let d_position, _ = Var.binding gradient_position_var dir_val in
    (* Reference --tw-gradient-stops for linear-gradient *)
    let stops_ref = Var.reference gradient_stops_var in
    style [ d_position; Css.background_image (Linear_gradient_var stops_ref) ]

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
      let color_theme_var = Color.color_var color shade in
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

  let bg' ?theme ?(shade = 500) color =
    let bg_var_name =
      let base = Color.pp color in
      if Color.is_base_color color then "background-color-" ^ base
      else "background-color-" ^ base ^ "-" ^ string_of_int shade
    in
    match Scheme.theme_value theme bg_var_name with
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
  (* [bg-size-[8rem]] and [bg-[length:120px_120px]] take any CSS length, so read
     them with the value parser rather than a hand-rolled unit table. *)
  let parse_bracket_size inner =
    let parts =
      String.split_on_char '_' inner |> List.filter (fun s -> s <> "")
    in
    match parts with
    | [ w; h ] -> (
        match (Css.parse_length w, Css.parse_length h) with
        | Some w, Some h -> Some (Css.background_size (Size (w, h)))
        | _ -> None)
    | [ v ] -> (
        (* Keywords are valid background-size values even under a [length:...]
           hint (Tailwind emits e.g. bg-[length:cover] as
           background-size:cover). *)
        match v with
        | "cover" -> Some (Css.background_size Cover)
        | "contain" -> Some (Css.background_size Contain)
        | "auto" -> Some (Css.background_size Auto)
        | _ ->
            Option.map
              (fun l -> Css.background_size (Length l))
              (Css.parse_length v))
    | _ -> None

  (* A bracket background-position: [120px_120px], [top], [left_10px_top_20px].
     The whole [<position>] grammar, so the hand-rolled reading that only knew
     lengths and two-keyword pairs no longer drops [top] on the floor. [None]
     means the bracket is not a position, which [of_class] rejects. *)
  let parse_bracket_position inner : Css.position_value option =
    let cursor = Cascade.Cursor.of_string (Parse.decode_underscores inner) in
    match
      Cascade.Cursor.try_parse_full_err Css.Properties.read_position_value
        cursor
    with
    | Ok pos -> Some pos
    | Error _ -> None

  (* The bracket forms that also accept a var() reference read it as one. *)
  let bracket_position_value inner : Css.position_value option =
    if Parse.is_var inner then
      let ref : Css.position_value Css.var =
        Var.bracket (Parse.extract_var_name inner)
      in
      Some (Css.Var ref)
    else parse_bracket_position inner

  (* A bracket background-image: a gradient, a url(), or a comma-separated layer
     list. [None] means the bracket is not an image, which [of_class] rejects:
     [bg-[image:nope]] used to parse and then emit an empty rule. *)
  let parse_bracket_image v : Css.background_image option =
    let css_str = String.map (fun c -> if c = '_' then ' ' else c) v in
    match Css.parse_background_image css_str with
    | Some [ img ] -> Some (Css.minify_background_image img)
    | Some (_ :: _ as imgs) ->
        Some (Css.List (List.map Css.minify_background_image imgs))
    | Some [] | None -> None

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
      With_interpolation (dir_val, In_oklab)
    in
    let base_decl, _ = Var.binding gradient_position_var dir_val in
    let interp_decl, _ = Var.binding gradient_position_var dir_with_interp in
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

  (* The typed counterpart of [interp_to_css_string]: [Some] for the closed set
     an [Css.color_interpolation] can represent (the four hue keywords, the six
     named colour spaces), [None] for everything [gradient_position_decl] cannot
     take directly - a bracket's arbitrary text, or a space Tailwind writes
     through without validating. Callers fall back to
     [gradient_position_decl_of_string] on [None]. *)
  let interp_to_color_interpolation s : Css.color_interpolation option =
    match s with
    | "shorter" -> Some (Css.In_oklch (Some Css.Shorter))
    | "longer" -> Some (Css.In_oklch (Some Css.Longer))
    | "increasing" -> Some (Css.In_oklch (Some Css.Increasing))
    | "decreasing" -> Some (Css.In_oklch (Some Css.Decreasing))
    | "oklab" -> Some Css.In_oklab
    | "oklch" -> Some (Css.In_oklch None)
    | "srgb" -> Some Css.In_srgb
    | "hsl" -> Some (Css.In_hsl None)
    | "lab" -> Some Css.In_lab
    | "lch" -> Some (Css.In_lch None)
    | _ -> None

  (** Convert a bracket gradient value to its CSS string. "125deg" → "125deg",
      "1.3rad" → "74.4845deg", "to_bottom" → "to bottom", "circle_at_center" →
      "circle at center". "100grad" stays "100grad": gradians are a distinct CSS
      angle unit from radians, even though the word ends in the same three
      letters. Reading the value as a real [<angle>] tells the two apart instead
      of matching on the "rad" suffix, which "grad" also has. *)
  let bracket_value_to_css inner =
    let decoded = String.map (fun c -> if c = '_' then ' ' else c) inner in
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
    let base_decl, _ = Var.binding gradient_position_var dir_val in
    let interp_decl = gradient_interp_decl dir_val ci_opt interp_css in
    let rules = gradient_direction_rules ~base_decl ~interp_decl in
    style ~property_rules:gradient_property_rules ~rules:(Some rules) []

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
  let bg_linear_angle_interp' angle_deg ci_opt interp_css =
    let dir_val : Css.gradient_direction =
      Angle (Deg (float_of_int angle_deg))
    in
    let base_decl, _ = Var.binding gradient_position_var dir_val in
    let interp_decl = gradient_interp_decl dir_val ci_opt interp_css in
    let rules = gradient_direction_rules ~base_decl ~interp_decl in
    style ~property_rules:gradient_property_rules ~rules:(Some rules) []

  (** [-bg-linear-{angle}/interp] *)
  let bg_linear_angle_neg_interp' angle_deg ci_opt interp_css =
    let angle_calc : Css.gradient_direction =
      Angle (Calc (Expr (Val (Deg (float_of_int angle_deg)), Mul, Num (-1.0))))
    in
    let base_decl, _ = Var.binding gradient_position_var angle_calc in
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

  (** Bracket color var with opacity: bg-[color:var(--x)]/50 *)
  let bg_bracket_color_var_opacity' var_str opacity =
    let bare = Parse.extract_var_name var_str in
    let var_ref : Css.color Css.var = Var.bracket bare in
    let fallback_decl = Css.background_color (Var var_ref) in
    let oklab_color = Color.mix_alpha opacity (Css.Var var_ref) in
    let oklab_decl = Css.background_color oklab_color in
    let supports_rule =
      Css.supports ~condition:Color.color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
    in
    style ~rules:(Some [ supports_rule ]) [ fallback_decl ]

  (* Gradient color with opacity - generates same structure as Tailwind: 1.
     Fallback rule with hex alpha (for scheme colors) 2. @supports block with
     color-mix using theme variable 3. Separate rule with --tw-gradient-stops *)
  let gradient_color_opacity ?theme ~prefix ~set_var ?(shade = 500) color
      opacity =
    let percent = Color.opacity_to_percent opacity in
    let color_name = Color.scheme_color_name color shade in
    let scheme = match theme with Some t -> t | None -> Scheme.default in

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

    (* Without a scheme override the palette still has a hex for the colour, and
       Tailwind emits the same fallback + [@supports] pair either way. A project
       token has no palette entry, so its value comes from the theme instead. *)
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
          | Some oklch -> hex_pair (Color.rgb_to_hex (Color.oklch_to_rgb oklch))
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
        let d_var, _ =
          Var.binding set_var (Color.apply_alpha opacity keyword)
        in
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
        let d_fallback, _ = Var.binding set_var fallback_value in

        (* Theme variable for @supports block *)
        let color_var = Color.color_var color shade in
        let theme_decl, color_ref = Var.binding color_var color_value in
        let oklab_color = Color.mix_alpha opacity (Css.Var color_ref) in
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
    |> List.filter_map (fun x -> x)
    |> Css.concat

  (** Flatten stops into declaration list *)
  let stops_as_decls d_stops d_via_stops_opt =
    match d_via_stops_opt with
    | Some d_via_stops -> [ d_via_stops; d_stops ]
    | None -> [ d_stops ]

  (** Gradient with a simple color value + stops (no opacity) *)
  let gradient_simple ~prefix ~set_var color_value extra_decls =
    let d_var, _ = Var.binding set_var (Css.minify_color color_value) in
    let d_stops, d_via_stops_opt = gradient_stops_decls prefix in
    let property_rules = gradient_all_property_rules () in
    let declarations =
      extra_decls @ [ d_var ] @ stops_as_decls d_stops d_via_stops_opt
    in
    style ~property_rules declarations

  (* Gradient with opacity: fallback + @supports color-mix + stops *)
  let gradient_with_opacity ~prefix ~set_var (fallback_color : Css.color)
      (mix_color : Css.color) ?(extra_supports_decls = []) percent =
    let d_fallback, _ = Var.binding set_var fallback_color in
    let oklab_color =
      Css.color_mix ~in_space:Oklab mix_color Css.Transparent ~percent1:percent
    in
    let d_oklab, _ = Var.binding set_var oklab_color in
    let fallback_rule =
      Css.rule ~selector:(Css.Selector.class_ "_") [ d_fallback ]
    in
    let supports_rule =
      Css.supports ~condition:Color.color_mix_supports_condition
        [
          Css.rule ~selector:(Css.Selector.class_ "_")
            (extra_supports_decls @ [ d_oklab ]);
        ]
    in
    let d_stops, d_via_stops_opt = gradient_stops_decls prefix in
    let property_rules = gradient_all_property_rules () in
    let stops_rule =
      Css.rule ~selector:(Css.Selector.class_ "_")
        (stops_as_decls d_stops d_via_stops_opt)
    in
    style ~property_rules
      ~rules:(Some [ fallback_rule; supports_rule; stops_rule ])
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
    |> List.filter_map (fun x -> x)
    |> Css.concat

  (* Gradient position from a length_percentage value *)
  let gradient_position_style pos_var (value : Css.length_percentage) =
    let d_var, _ = Var.binding pos_var value in
    style ~property_rules:(gradient_position_property_rules ()) [ d_var ]

  (* The one reader for a bracket stop position, used both to decide that a
     bracket is a position and to convert it, so the two cannot disagree. A stop
     takes a [<length-percentage>]; Tailwind reads any other bracket - a
     keyword, a unitless number, the docs' [<value>] placeholder - as a colour,
     and a colour spelled that way has no typed rendering here, so it is
     refused. *)
  let parse_bracket_position_value (inner : string) :
      Css.length_percentage option =
    let typed_var prefix =
      let n = String.length prefix in
      if String.length inner > n && String.sub inner 0 n = prefix then
        let var_str = String.sub inner n (String.length inner - n) in
        let bare = Parse.extract_var_name var_str in
        let vr : Css.length_percentage Css.var = Var.bracket bare in
        Some (Css.Var vr : Css.length_percentage)
      else None
    in
    if Parse.is_var inner then
      let bare = Parse.extract_var_name inner in
      let vr : Css.length_percentage Css.var = Var.bracket bare in
      Some (Css.Var vr : Css.length_percentage)
    else
      match typed_var "percentage:" with
      | Some v -> Some v
      | None -> (
          match typed_var "length:" with
          | Some v -> Some v
          | None -> Parse.arbitrary_length_percentage inner)

  (** Convert a var string to a typed CSS color value *)
  let color_var_ref v : Css.color =
    let bare = Parse.extract_var_name v in
    let vr : Css.color Css.var = Var.bracket bare in
    Var vr

  (** Convert a non-named Color_source.t to a Css.color *)
  let css_color_of_source : Color_source.t -> Css.color = function
    | Color_source.Current | Color_source.Current_opacity _ -> Css.Current
    | Color_source.Inherit -> Css.Inherit
    | Color_source.Transparent -> Css.Transparent
    | Color_source.Bracket_hex h | Color_source.Bracket_hex_opacity (h, _) ->
        Css.hex h
    | Color_source.Bracket_color_var v
    | Color_source.Bracket_color_var_opacity (v, _) ->
        color_var_ref v
    | Color_source.Bracket_var v | Color_source.Bracket_var_opacity (v, _) ->
        color_var_ref v
    | Color_source.Bracket_color v | Color_source.Bracket_color_opacity (v, _)
      -> (
        match Color.parse_bracket_color v with
        | Some c -> c
        | None -> Css.Transparent)
    | Color_source.Named _ | Color_source.Named_opacity _ ->
        (* A palette colour resolves through the scheme, not into a plain CSS
           colour; [to_style] answers those before it reaches here. *)
        invalid_arg "backgrounds: a palette colour has no plain CSS colour"

  (** Extract opacity from a Color_source.t, if present *)
  let opacity_of_source : Color_source.t -> Color.opacity_modifier option =
    function
    | Color_source.Current_opacity o
    | Color_source.Bracket_hex_opacity (_, o)
    | Color_source.Bracket_color_var_opacity (_, o)
    | Color_source.Bracket_var_opacity (_, o)
    | Color_source.Bracket_color_opacity (_, o) ->
        Some o
    | Color_source.Named _ | Color_source.Named_opacity _ | Color_source.Current
    | Color_source.Inherit | Color_source.Transparent
    | Color_source.Bracket_hex _ | Color_source.Bracket_color_var _
    | Color_source.Bracket_var _ | Color_source.Bracket_color _ ->
        None

  let to_style theme =
    let gradient_color_opacity ~prefix ~set_var ?(shade = 500) color opacity =
      gradient_color_opacity ~theme ~prefix ~set_var ~shade color opacity
    in
    let bg_with_opacity c shade opacity =
      Color.bg_with_opacity ~theme c shade opacity
    in
    let bg' ?(shade = 500) color = bg' ~theme ~shade color in
    function
    | Bg (color, shade) -> bg' ~shade color
    | Bg_gradient_to dir -> bg_gradient_to' dir
    | Gradient_color (target, src) -> (
        let prefix, set_var, _pos_var = gradient_target_info target in
        (* Named colors use the scheme system with theme variables *)
        match src with
        | Color_source.Named (color, shade) ->
            gradient_color ~prefix ~set_var ~shade color
        | Color_source.Named_opacity (color, shade, opacity) ->
            gradient_color_opacity ~prefix ~set_var ~shade color opacity
        | Color_source.Bracket_hex_opacity (h, opacity) ->
            (* Hex is known at compile time: compute oklab directly *)
            let alpha = Color.opacity_to_percent opacity /. 100.0 in
            let color = Color.hex_to_oklab_alpha h alpha in
            gradient_simple ~prefix ~set_var color []
        | Color_source.Current | Color_source.Current_opacity _
        | Color_source.Inherit | Color_source.Transparent
        | Color_source.Bracket_hex _ | Color_source.Bracket_color_var _
        | Color_source.Bracket_color_var_opacity _ | Color_source.Bracket_var _
        | Color_source.Bracket_var_opacity _ | Color_source.Bracket_color _
        | Color_source.Bracket_color_opacity _ -> (
            (* A keyword or a bracket value: the colour is known without the
               scheme. Spelled out rather than swept up, so a source added to
               [Color_source.t] is a compile error here. *)
            let color = css_color_of_source src in
            match opacity_of_source src with
            | None -> gradient_simple ~prefix ~set_var color []
            | Some opacity ->
                let percent = Color.opacity_to_percent opacity in
                gradient_with_opacity ~prefix ~set_var color color percent))
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
          | Position.Bottom -> [ Center_bottom ]
          | Position.Bottom_left -> [ XY (Px 0., Pct 100.) ]
          | Position.Bottom_right -> [ XY (Pct 100., Pct 100.) ]
          | Position.Center -> [ Center ]
          | Position.Left -> [ Single (Px 0.) ]
          | Position.Left_bottom -> [ XY (Px 0., Pct 100.) ]
          | Position.Left_top -> [ XY (Px 0., Px 0.) ]
          | Position.Right -> [ Single (Pct 100.) ]
          | Position.Right_bottom -> [ XY (Pct 100., Pct 100.) ]
          | Position.Right_top -> [ XY (Pct 100., Px 0.) ]
          | Position.Top -> [ Center_top ]
          | Position.Top_left -> [ XY (Px 0., Px 0.) ]
          | Position.Top_right -> [ XY (Pct 100., Px 0.) ]
        in
        bg_position' pos_val
    | Bg_bracket_contain -> style [ Css.background_size Contain ]
    | Bg_bracket_cover -> style [ Css.background_size Cover ]
    | Bg_bracket_size inner -> (
        match parse_bracket_size inner with
        | Some decl -> style [ decl ]
        | None -> style [ Css.background_size Auto ])
    | Bg_bracket_position (_, pos) -> style [ Css.background_position [ pos ] ]
    | Bg_bracket_typed_position (_, pos) ->
        style [ Css.background_position [ pos ] ]
    | Bg_bracket_color_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.color Css.var = Var.bracket bare in
        style [ Css.background_color (Var var_ref) ]
    | Bg_bracket_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.color Css.var = Var.bracket bare in
        style [ Css.background_color (Var var_ref) ]
    | Bg_bracket_image_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.background_image Css.var = Var.bracket bare in
        style [ Css.background_image (Var var_ref) ]
    | Bg_bracket_image (_, img) -> style [ Css.background_image img ]
    | Bg_bracket_url url ->
        style [ Css.background_image (Url (strip_outer_quotes url)) ]
    | Bg_bracket_image_url url ->
        style [ Css.background_image (Url (strip_outer_quotes url)) ]
    | Bg_bracket_url_var v ->
        let bare = Parse.extract_var_name v in
        let var_ref : Css.background_image Css.var = Var.bracket bare in
        style [ Css.background_image (Var var_ref) ]
    | Bg_bracket_linear_gradient (_, img) -> style [ Css.background_image img ]
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
    | Bg_radial -> bg_radial' ()
    | Bg_radial_interp (_, css) -> bg_radial_interp' css
    | Bg_radial_bracket v -> bg_radial_bracket' v
    | Bg_bracket_color_var_opacity (v, opacity) ->
        bg_bracket_color_var_opacity' v opacity
    | Bg_bracket_var_opacity (v, opacity) ->
        bg_bracket_color_var_opacity' v opacity
    | Bg_bracket_color (_, css_color) ->
        let c =
          match Color.css_color_to_hex css_color with
          | Some h -> h
          | None -> css_color
        in
        style [ Css.background_color c ]
    | Bg_bracket_color_opacity (orig, _, opacity) ->
        let c = Color.bracket_color_to_custom orig in
        bg_with_opacity c 500 opacity
    | Bg_current -> style [ Css.background_color Css.Current ]
    | Bg_current_opacity opacity -> Color.bg_current_with_opacity ~theme opacity
    | Bg_transparent -> style [ Css.background_color (Css.hex "#0000") ]
    | Bg_opacity (color, shade, opacity) -> bg_with_opacity color shade opacity
    | Bg_bracket_length inner -> (
        match parse_bracket_size inner with
        | Some decl -> style [ decl ]
        | None -> style [ Css.background_size Auto ])
    | Bg_position_bracket (_, pos) -> style [ Css.background_position [ pos ] ]
    | Bg_size_bracket inner -> (
        match parse_bracket_size inner with
        | Some decl -> style [ decl ]
        | None when Parse.is_var inner ->
            let var_name = Parse.extract_var_name inner in
            let ref : Css.background_size Css.var = Var.bracket var_name in
            style [ Css.background_size (Var ref) ]
        | None -> style [ Css.background_size Auto ])

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
    String.length inner > 0
    && inner.[0] = '#'
    && Option.is_some (Css.hex_opt inner)

  let parse_gradient_color ?theme target rest =
    let gc src = Ok (Gradient_color (target, src)) in
    let gp src = Ok (Gradient_stop_position (target, src)) in
    match rest with
    (* Keywords *)
    | [ "current" ] -> gc Color_source.Current
    | [ current_str ] when String.starts_with ~prefix:"current/" current_str ->
        let _, opacity = Color.parse_opacity_modifier ?theme current_str in
        gc (Color_source.Current_opacity opacity)
    | [ "inherit" ] -> gc Color_source.Inherit
    | [ "transparent" ] -> gc Color_source.Transparent
    (* Percentage positions: from-0%, from-100% (integer only) *)
    | [ pct_str ] when String.ends_with ~suffix:"%" pct_str -> (
        let num_s = String.sub pct_str 0 (String.length pct_str - 1) in
        match int_of_string_opt num_s with
        | Some p -> gp (Percent (float_of_int p))
        | None -> Error (`Msg "Invalid gradient position"))
    (* Bracket with opacity: [#0088cc]/50, [#0088cc]/[0.5], [var(--x)]/50 *)
    | [ bracket_opacity ] when has_opacity bracket_opacity -> (
        let base, opacity_opt =
          Color.parse_opacity_modifier ?theme bracket_opacity
        in
        match opacity_opt with
        | Color.No_opacity when Parse.is_bracket_value bracket_opacity -> (
            (* No valid opacity found — treat as plain bracket *)
            let inner = Parse.bracket_inner bracket_opacity in
            if String.length inner > 6 && String.sub inner 0 6 = "color:" then
              let var_str = String.sub inner 6 (String.length inner - 6) in
              gc (Color_source.Bracket_color_var var_str)
            else if is_bracket_hex inner then
              gc
                (Color_source.Bracket_hex
                   (String.sub inner 1 (String.length inner - 1)))
            else if Parse.is_var inner then gc (Color_source.Bracket_var inner)
            else
              match parse_bracket_position_value inner with
              | Some value -> gp (Bracket (inner, value))
              | None -> Error (`Msg "Invalid gradient stop value"))
        | Color.No_opacity ->
            Error (`Msg "Invalid gradient bracket with opacity")
        | opacity when Parse.is_bracket_value base ->
            let inner = Parse.bracket_inner base in
            if String.length inner > 6 && String.sub inner 0 6 = "color:" then
              let var_str = String.sub inner 6 (String.length inner - 6) in
              gc (Color_source.Bracket_color_var_opacity (var_str, opacity))
            else if is_bracket_hex inner then
              gc
                (Color_source.Bracket_hex_opacity
                   (String.sub inner 1 (String.length inner - 1), opacity))
            else if Parse.is_var inner then
              gc (Color_source.Bracket_var_opacity (inner, opacity))
            else if Color.parse_bracket_color inner <> None then
              gc (Color_source.Bracket_color_opacity (inner, opacity))
            else Error (`Msg "Invalid gradient bracket with opacity")
        | _ -> (
            (* Named color with opacity *)
            match Color.shade_and_opacity_of_strings ?theme rest with
            | Ok (color, shade, opacity) ->
                gc (Color_source.Named_opacity (color, shade, opacity))
            | Error e -> Error e))
    (* Bracket notation without opacity *)
    | [ bracket ] when Parse.is_bracket_value bracket -> (
        let inner = Parse.bracket_inner bracket in
        if String.length inner > 6 && String.sub inner 0 6 = "color:" then
          let var_str = String.sub inner 6 (String.length inner - 6) in
          gc (Color_source.Bracket_color_var var_str)
        else if is_bracket_hex inner then
          gc
            (Color_source.Bracket_hex
               (String.sub inner 1 (String.length inner - 1)))
        else if Parse.is_var inner then gc (Color_source.Bracket_var inner)
        else if Color.parse_bracket_color inner <> None then
          gc (Color_source.Bracket_color inner)
        else
          match parse_bracket_position_value inner with
          | Some value -> gp (Bracket (inner, value))
          | None -> Error (`Msg "Invalid gradient stop value"))
    (* Named color with opacity via has_opacity on rest *)
    | _ when List.exists has_opacity rest -> (
        match Color.shade_and_opacity_of_strings ?theme rest with
        | Ok (color, shade, opacity) ->
            gc (Color_source.Named_opacity (color, shade, opacity))
        | Error e -> Error e)
    (* Named color *)
    | _ -> (
        match Color.shade_of_strings ?theme rest with
        | Ok (color, shade) -> gc (Color_source.Named (color, shade))
        | Error _ -> Error (`Msg "Invalid gradient color"))

  let of_class theme class_name =
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
    | [ "bg"; "transparent" ] -> Ok Bg_transparent
    | [ "bg"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = Color.parse_opacity_modifier ~theme current_str in
        match opacity with
        | Color.No_opacity when base = "current" -> Ok Bg_current
        | Color.No_opacity -> Error (`Msg ("Invalid bg: " ^ current_str))
        | _ -> Ok (Bg_current_opacity opacity))
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
    | [ "bg"; "bottom" ] -> Ok (Bg_position Position.Bottom)
    | [ "bg"; "bottom"; "left" ] -> Ok (Bg_position Position.Bottom_left)
    | [ "bg"; "bottom"; "right" ] -> Ok (Bg_position Position.Bottom_right)
    | [ "bg"; "center" ] -> Ok (Bg_position Position.Center)
    | [ "bg"; "left" ] -> Ok (Bg_position Position.Left)
    | [ "bg"; "left"; "bottom" ] -> Ok (Bg_position Position.Left_bottom)
    | [ "bg"; "left"; "top" ] -> Ok (Bg_position Position.Left_top)
    | [ "bg"; "right" ] -> Ok (Bg_position Position.Right)
    | [ "bg"; "right"; "bottom" ] -> Ok (Bg_position Position.Right_bottom)
    | [ "bg"; "right"; "top" ] -> Ok (Bg_position Position.Right_top)
    | [ "bg"; "top" ] -> Ok (Bg_position Position.Top)
    | [ "bg"; "top"; "left" ] -> Ok (Bg_position Position.Top_left)
    | [ "bg"; "top"; "right" ] -> Ok (Bg_position Position.Top_right)
    (* bg-linear-to-* direction utilities (with optional /interp modifier) *)
    | [ "bg"; "linear"; "to"; dir_mod ] -> (
        let dir_s, interp_opt = split_mod dir_mod in
        match (parse_direction dir_s, interp_opt) with
        | Some dir, None -> Ok (Bg_linear_to dir)
        | Some dir, Some interp -> (
            match interp_to_css_string interp with
            | Some css -> Ok (Bg_linear_to_interp (dir, interp, css))
            | None -> Error (`Msg ("Invalid gradient interpolation: " ^ interp))
            )
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
        | Some n, Some interp -> (
            match interp_to_css_string interp with
            | Some css -> Ok (Bg_linear_angle_interp (n, interp, css))
            | None -> Error (`Msg ("Invalid gradient interpolation: " ^ interp))
            )
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
        if is_angle then Ok (Bg_linear_bracket_neg inner)
        else Error (`Msg ("Invalid -bg-linear bracket value: " ^ inner))
    (* -bg-linear-{angle} and -bg-linear-{angle}/interp *)
    | [ ""; "bg"; "linear"; angle_mod ] -> (
        let angle_s, interp_opt = split_mod angle_mod in
        match (int_of_string_opt angle_s, interp_opt) with
        | Some n, None -> Ok (Bg_linear_angle_neg n)
        | Some n, Some interp -> (
            match interp_to_css_string interp with
            | Some css -> Ok (Bg_linear_angle_neg_interp (n, interp, css))
            | None -> Error (`Msg ("Invalid gradient interpolation: " ^ interp))
            )
        | None, _ -> Error (`Msg ("Invalid -bg-linear angle: " ^ angle_mod)))
    (* bg-conic - bare conic gradient *)
    | [ "bg"; "conic" ] -> Ok Bg_conic
    (* bg-conic/interp - conic gradient with modifier only *)
    | [ "bg"; conic_mod ]
      when String.length conic_mod > 6 && String.sub conic_mod 0 6 = "conic/"
      -> (
        let interp = String.sub conic_mod 6 (String.length conic_mod - 6) in
        match interp_to_css_string interp with
        | Some css -> Ok (Bg_conic_interp (interp, css))
        | None -> Error (`Msg ("Invalid gradient interpolation: " ^ interp)))
    (* bg-conic-{angle} and bg-conic-{angle}/interp *)
    | [ "bg"; "conic"; angle_mod ] -> (
        let angle_s, interp_opt = split_mod angle_mod in
        match (int_of_string_opt angle_s, interp_opt) with
        | Some n, None -> Ok (Bg_conic_angle n)
        | Some n, Some interp -> (
            match interp_to_css_string interp with
            | Some css -> Ok (Bg_conic_angle_interp (n, interp, css))
            | None -> Error (`Msg ("Invalid gradient interpolation: " ^ interp))
            )
        | _ -> Error (`Msg ("Invalid bg-conic angle: " ^ angle_mod)))
    (* -bg-conic-{angle} and -bg-conic-{angle}/interp *)
    | [ ""; "bg"; "conic"; angle_mod ] -> (
        let angle_s, interp_opt = split_mod angle_mod in
        match (int_of_string_opt angle_s, interp_opt) with
        | Some n, None -> Ok (Bg_conic_angle_neg n)
        | Some n, Some interp -> (
            match interp_to_css_string interp with
            | Some css -> Ok (Bg_conic_angle_neg_interp (n, interp, css))
            | None -> Error (`Msg ("Invalid gradient interpolation: " ^ interp))
            )
        | _ -> Error (`Msg ("Invalid -bg-conic angle: " ^ angle_mod)))
    (* bg-radial - bare radial gradient *)
    | [ "bg"; "radial" ] -> Ok Bg_radial
    (* bg-radial/interp - radial gradient with modifier only *)
    | [ "bg"; radial_mod ]
      when String.length radial_mod > 7 && String.sub radial_mod 0 7 = "radial/"
      -> (
        let interp = String.sub radial_mod 7 (String.length radial_mod - 7) in
        match interp_to_css_string interp with
        | Some css -> Ok (Bg_radial_interp (interp, css))
        | None -> Error (`Msg ("Invalid gradient interpolation: " ^ interp)))
    (* bg-radial-[value] - bracket radial gradient *)
    | [ "bg"; "radial"; bracket ] when Parse.is_bracket_value bracket ->
        let inner = Parse.bracket_inner bracket in
        Ok (Bg_radial_bracket inner)
    (* bg-position-[...] bracket notation *)
    | [ "bg"; "position"; bracket ] when Parse.is_bracket_value bracket -> (
        let inner = Parse.bracket_inner bracket in
        match bracket_position_value inner with
        | Some pos -> Ok (Bg_position_bracket (inner, pos))
        | None -> Error (`Msg "Invalid background-position value"))
    (* bg-size-[...] bracket notation *)
    | [ "bg"; "size"; bracket ] when Parse.is_bracket_value bracket ->
        let inner = Parse.bracket_inner bracket in
        if parse_bracket_size inner = None && not (Parse.is_var inner) then
          Error (`Msg "Invalid background-size value")
        else Ok (Bg_size_bracket inner)
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
        let parse_opacity s = Color.opacity_of_string ~theme s in
        if !close >= 0 && !close + 1 < len && bracket_stuff.[!close + 1] = '/'
        then
          (* Bracket with opacity: [color:var(--x)]/50 *)
          let bracket = String.sub bracket_stuff 0 (!close + 1) in
          let opacity_str =
            String.sub bracket_stuff (!close + 2) (len - !close - 2)
          in
          let inner = Parse.bracket_inner bracket in
          match parse_opacity opacity_str with
          | Some opacity -> (
              if String.length inner > 6 && String.sub inner 0 6 = "color:" then
                let var_str = String.sub inner 6 (String.length inner - 6) in
                Ok (Bg_bracket_color_var_opacity (var_str, opacity))
              else if Parse.is_var inner then
                (* A var() holds an unknown colour, so the alpha has to be
                   applied at run time by color-mix, not folded here. *)
                Ok (Bg_bracket_var_opacity (inner, opacity))
              else
                match Color.parse_bracket_color inner with
                | Some css_color ->
                    Ok (Bg_bracket_color_opacity (inner, css_color, opacity))
                | None ->
                    Error (`Msg ("Unknown bg bracket value: " ^ bracket_stuff)))
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
            -> (
              (* The [position:] data-type hint forces a background-position; a
                 value the grammar rejects is not a utility. It used to fall
                 through to a plausible-looking [center]. *)
              let v = String.sub inner 9 (String.length inner - 9) in
              match bracket_position_value v with
              | Some pos -> Ok (Bg_bracket_typed_position (v, pos))
              | None -> Error (`Msg ("Unknown bg bracket position: " ^ v)))
          | _ when String.length inner > 6 && String.sub inner 0 6 = "color:" ->
              Ok
                (Bg_bracket_color_var
                   (String.sub inner 6 (String.length inner - 6)))
          | _ when String.length inner > 6 && String.sub inner 0 6 = "image:"
            -> (
              (* The [image:] data-type hint forces a background-image. The
                 value is a [url(...)] literal, a [var(...)] reference, or a
                 literal image (e.g. a gradient). *)
              let v = String.sub inner 6 (String.length inner - 6) in
              if String.length v > 4 && String.sub v 0 4 = "url(" then
                Ok (Bg_bracket_image_url (String.sub v 4 (String.length v - 5)))
              else if Parse.is_var v then Ok (Bg_bracket_image_var v)
              else
                match parse_bracket_image v with
                | Some img -> Ok (Bg_bracket_image (v, img))
                | None -> Error (`Msg ("Unknown bg bracket image: " ^ v)))
          | _ when String.length inner > 4 && String.sub inner 0 4 = "url:" ->
              Ok
                (Bg_bracket_url_var
                   (String.sub inner 4 (String.length inner - 4)))
          | _ when String.length inner > 4 && String.sub inner 0 4 = "url(" ->
              let url_content = String.sub inner 4 (String.length inner - 5) in
              Ok (Bg_bracket_url url_content)
          | _ when Parse.is_var inner -> Ok (Bg_bracket_var inner)
          | _ -> (
              (* Try parsing as background-image (gradients, urls,
                 comma-separated) *)
              match parse_bracket_image inner with
              | Some img -> Ok (Bg_bracket_linear_gradient (inner, img))
              | None -> (
                  match Color.parse_bracket_color inner with
                  | Some css_color -> Ok (Bg_bracket_color (inner, css_color))
                  | None -> (
                      match parse_bracket_position inner with
                      | Some pos -> Ok (Bg_bracket_position (inner, pos))
                      | None ->
                          Error (`Msg ("Unknown bg bracket value: " ^ inner)))))
        )
    | "bg" :: rest when List.exists has_opacity rest -> (
        match Color.shade_and_opacity_of_strings ~theme rest with
        | Ok (color, shade, opacity) -> Ok (Bg_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "bg" :: rest -> (
        match Color.shade_of_strings ~theme rest with
        | Ok (color, shade) -> Ok (Bg (color, shade))
        | Error _ -> Error (`Msg "Invalid background color"))
    | "from" :: rest -> parse_gradient_color ~theme From rest
    | [ "via"; "none" ] -> Ok Via_none
    | "via" :: rest -> parse_gradient_color ~theme Via rest
    | "to" :: rest -> parse_gradient_color ~theme To rest
    | _ -> Error (`Msg "Unknown background class")

  let examples =
    [
      Bg_none;
      Bg_auto;
      Bg_no_repeat;
      Bg_fixed;
      Bg_clip_border;
      Bg_origin_border;
      Bg_current;
      Bg_position Position.Center;
    ]
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)

let bg ?opacity ?(shade = 500) color =
  Color.check_shade ~utility:"bg" color shade;
  match opacity with
  | None -> utility (Bg (color, shade))
  | Some pct -> utility (Bg_opacity (color, shade, Color.opacity_of_int pct))

let bg_gradient_to dir = utility (Bg_gradient_to dir)

let from_color ?(shade = 500) color =
  Color.check_shade ~utility:"from_color" color shade;
  utility (Gradient_color (From, Color_source.Named (color, shade)))

let via_color ?(shade = 500) color =
  Color.check_shade ~utility:"via_color" color shade;
  utility (Gradient_color (Via, Color_source.Named (color, shade)))

let to_color ?(shade = 500) color =
  Color.check_shade ~utility:"to_color" color shade;
  utility (Gradient_color (To, Color_source.Named (color, shade)))
