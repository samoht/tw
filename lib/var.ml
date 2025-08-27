(* Variable tracking for CSS composition groups

   Purpose: - Provide a typed view over Tailwind's CSS custom properties (`--*`)
   used by utilities, transforms, filters, radii, fonts, etc.

   How it interacts with Rules: - `Rules.to_css` inspects declarations via
   `Css.all_vars` to determine which variables are referenced and generates a
   minimal theme layer containing only those variables. `Var.to_css_properties`
   provides default values.

   Notes: - `to_string` maps typed variants to `--var-name`. -
   `canonical_order`/`compare` ensure stable, human‑friendly ordering in the
   generated CSS. *)

(* Layer classification for CSS variables *)
type layer = Theme | Base | Properties | Utility

(* CSS variable type as a GADT for type safety. The order is meaninful and will
   be reflected in which the variables are printed in the relevant CSS
   layers. *)
type _ t =
  (* Spacing and sizing *)
  | Spacing : Css.length t
  (* Typography *)
  | Font_sans : string t
  | Font_serif : string t
  | Font_mono : string t
  | Font_weight : string t
  | Leading : string t
  | Text_xs : Css.length t
  | Text_xs_line_height : Css.length t
  | Text_sm : Css.length t
  | Text_sm_line_height : Css.length t
  | Text_base : Css.length t
  | Text_base_line_height : Css.length t
  | Text_lg : Css.length t
  | Text_lg_line_height : Css.length t
  | Text_xl : Css.length t
  | Text_xl_line_height : Css.length t
  | Text_2xl : Css.length t
  | Text_2xl_line_height : Css.length t
  | Text_3xl : Css.length t
  | Text_3xl_line_height : Css.length t
  | Text_4xl : Css.length t
  | Text_4xl_line_height : Css.length t
  | Text_5xl : Css.length t
  | Text_5xl_line_height : Css.length t
  | Text_6xl : Css.length t
  | Text_6xl_line_height : Css.length t
  | Text_7xl : Css.length t
  | Text_7xl_line_height : Css.length t
  | Text_8xl : Css.length t
  | Text_8xl_line_height : Css.length t
  | Text_9xl : Css.length t
  | Text_9xl_line_height : Css.length t
  | Font_weight_thin : int t
  | Font_weight_extralight : int t
  | Font_weight_light : int t
  | Font_weight_normal : int t
  | Font_weight_medium : int t
  | Font_weight_semibold : int t
  | Font_weight_bold : int t
  | Font_weight_extrabold : int t
  | Font_weight_black : int t
  (* Border radius *)
  | Radius_none : Css.length t
  | Radius_sm : Css.length t
  | Radius_default : Css.length t
  | Radius_md : Css.length t
  | Radius_lg : Css.length t
  | Radius_xl : Css.length t
  | Radius_2xl : Css.length t
  | Radius_3xl : Css.length t
  (* Colors - using color name and optional shade *)
  | Color :
      string * int option
      -> Css.color t (* e.g., Color ("blue", Some 500) *)
  (* Transform variables *)
  | Translate_x : Css.length t
  | Translate_y : Css.length t
  | Translate_z : Css.length t
  | Rotate : Css.angle t
  | Skew_x : Css.angle t
  | Skew_y : Css.angle t
  | Scale_x : float t
  | Scale_y : float t
  | Scale_z : float t
  (* Filter variables *)
  | Blur : Css.length t
  | Brightness : float t
  | Contrast : float t
  | Grayscale : float t
  | Hue_rotate : Css.angle t
  | Invert : float t
  | Saturate : float t
  | Sepia : float t
  | Drop_shadow : string t
  | Drop_shadow_alpha : float t
  (* Backdrop filter variables *)
  | Backdrop_blur : Css.length t
  | Backdrop_brightness : float t
  | Backdrop_contrast : float t
  | Backdrop_grayscale : float t
  | Backdrop_hue_rotate : Css.angle t
  | Backdrop_invert : float t
  | Backdrop_saturate : float t
  | Backdrop_sepia : float t
  | Backdrop_opacity : float t
  (* Shadow and ring variables *)
  | Shadow : string t
  | Shadow_color : Css.color t
  | Shadow_alpha : float t
  | Inset_shadow : string t
  | Inset_shadow_color : Css.color t
  | Inset_shadow_alpha : float t
  | Ring_color : Css.color t
  | Ring_shadow : string t
  | Inset_ring_color : Css.color t
  | Inset_ring_shadow : string t
  | Ring_inset : string t
  | Ring_offset_width : Css.length t
  | Ring_offset_color : Css.color t
  | Ring_offset_shadow : string t
  (* Gradient variables *)
  | Gradient_from : Css.color t
  | Gradient_via : Css.color t
  | Gradient_to : Css.color t
  | Gradient_stops : string t
  | Gradient_via_stops : string t
  | Gradient_position : string t
  | Gradient_from_position : float t
  | Gradient_via_position : float t
  | Gradient_to_position : float t
  (* Border variables *)
  | Border_style : Css.border_style t
  (* Scroll snap variables *)
  | Scroll_snap_strictness : string t
  (* Transition variables *)
  | Duration : Css.duration t
  (* Default font family helpers *)
  | Default_font_family : string t
  | Default_mono_font_family : string t

(** Existential wrapper for variables of any type *)
type any = Any : _ t -> any

(* Convert a CSS variable to its string representation (with --) *)
let to_string : type a. a t -> string = function
  | Spacing -> "--spacing"
  | Font_sans -> "--font-sans"
  | Font_serif -> "--font-serif"
  | Font_mono -> "--font-mono"
  | Font_weight -> "--tw-font-weight"
  | Leading -> "--tw-leading"
  | Text_xs -> "--text-xs"
  | Text_xs_line_height -> "--text-xs--line-height"
  | Text_sm -> "--text-sm"
  | Text_sm_line_height -> "--text-sm--line-height"
  | Text_base -> "--text-base"
  | Text_base_line_height -> "--text-base--line-height"
  | Text_lg -> "--text-lg"
  | Text_lg_line_height -> "--text-lg--line-height"
  | Text_xl -> "--text-xl"
  | Text_xl_line_height -> "--text-xl--line-height"
  | Text_2xl -> "--text-2xl"
  | Text_2xl_line_height -> "--text-2xl--line-height"
  | Text_3xl -> "--text-3xl"
  | Text_3xl_line_height -> "--text-3xl--line-height"
  | Text_4xl -> "--text-4xl"
  | Text_4xl_line_height -> "--text-4xl--line-height"
  | Text_5xl -> "--text-5xl"
  | Text_5xl_line_height -> "--text-5xl--line-height"
  | Text_6xl -> "--text-6xl"
  | Text_6xl_line_height -> "--text-6xl--line-height"
  | Text_7xl -> "--text-7xl"
  | Text_7xl_line_height -> "--text-7xl--line-height"
  | Text_8xl -> "--text-8xl"
  | Text_8xl_line_height -> "--text-8xl--line-height"
  | Text_9xl -> "--text-9xl"
  | Text_9xl_line_height -> "--text-9xl--line-height"
  | Font_weight_thin -> "--font-weight-thin"
  | Font_weight_extralight -> "--font-weight-extralight"
  | Font_weight_light -> "--font-weight-light"
  | Font_weight_normal -> "--font-weight-normal"
  | Font_weight_medium -> "--font-weight-medium"
  | Font_weight_semibold -> "--font-weight-semibold"
  | Font_weight_bold -> "--font-weight-bold"
  | Font_weight_extrabold -> "--font-weight-extrabold"
  | Font_weight_black -> "--font-weight-black"
  | Radius_none -> "--radius-none"
  | Radius_sm -> "--radius-sm"
  | Radius_default -> "--radius-default"
  | Radius_md -> "--radius-md"
  | Radius_lg -> "--radius-lg"
  | Radius_xl -> "--radius-xl"
  | Radius_2xl -> "--radius-2xl"
  | Radius_3xl -> "--radius-3xl"
  | Color (name, None) -> "--color-" ^ name
  | Color (name, Some shade) ->
      Pp.str [ "--color-"; name; "-"; string_of_int shade ]
  | Translate_x -> "--tw-translate-x"
  | Translate_y -> "--tw-translate-y"
  | Translate_z -> "--tw-translate-z"
  | Rotate -> "--tw-rotate"
  | Skew_x -> "--tw-skew-x"
  | Skew_y -> "--tw-skew-y"
  | Scale_x -> "--tw-scale-x"
  | Scale_y -> "--tw-scale-y"
  | Scale_z -> "--tw-scale-z"
  | Blur -> "--tw-blur"
  | Brightness -> "--tw-brightness"
  | Contrast -> "--tw-contrast"
  | Grayscale -> "--tw-grayscale"
  | Hue_rotate -> "--tw-hue-rotate"
  | Invert -> "--tw-invert"
  | Saturate -> "--tw-saturate"
  | Sepia -> "--tw-sepia"
  | Drop_shadow -> "--tw-drop-shadow"
  | Drop_shadow_alpha -> "--tw-drop-shadow-alpha"
  | Backdrop_blur -> "--tw-backdrop-blur"
  | Backdrop_brightness -> "--tw-backdrop-brightness"
  | Backdrop_contrast -> "--tw-backdrop-contrast"
  | Backdrop_grayscale -> "--tw-backdrop-grayscale"
  | Backdrop_hue_rotate -> "--tw-backdrop-hue-rotate"
  | Backdrop_invert -> "--tw-backdrop-invert"
  | Backdrop_saturate -> "--tw-backdrop-saturate"
  | Backdrop_sepia -> "--tw-backdrop-sepia"
  | Backdrop_opacity -> "--tw-backdrop-opacity"
  | Shadow -> "--tw-shadow"
  | Shadow_color -> "--tw-shadow-color"
  | Shadow_alpha -> "--tw-shadow-alpha"
  | Inset_shadow -> "--tw-inset-shadow"
  | Inset_shadow_color -> "--tw-inset-shadow-color"
  | Inset_shadow_alpha -> "--tw-inset-shadow-alpha"
  | Ring_color -> "--tw-ring-color"
  | Ring_shadow -> "--tw-ring-shadow"
  | Inset_ring_color -> "--tw-inset-ring-color"
  | Inset_ring_shadow -> "--tw-inset-ring-shadow"
  | Ring_inset -> "--tw-ring-inset"
  | Ring_offset_width -> "--tw-ring-offset-width"
  | Ring_offset_color -> "--tw-ring-offset-color"
  | Ring_offset_shadow -> "--tw-ring-offset-shadow"
  | Gradient_from -> "--tw-gradient-from"
  | Gradient_via -> "--tw-gradient-via"
  | Gradient_to -> "--tw-gradient-to"
  | Gradient_stops -> "--tw-gradient-stops"
  | Gradient_via_stops -> "--tw-gradient-via-stops"
  | Gradient_position -> "--tw-gradient-position"
  | Gradient_from_position -> "--tw-gradient-from-position"
  | Gradient_via_position -> "--tw-gradient-via-position"
  | Gradient_to_position -> "--tw-gradient-to-position"
  | Border_style -> "--tw-border-style"
  | Scroll_snap_strictness -> "--tw-scroll-snap-strictness"
  | Duration -> "--tw-duration"
  | Default_font_family -> "--default-font-family"
  | Default_mono_font_family -> "--default-mono-font-family"

(* Get the name of a variable (without --) *)
let name : type a. a t -> string =
 fun v ->
  let s = to_string v in
  String.sub s 2 (String.length s - 2)

(* Canonical color ordering function *)
let canonical_color_order color_name =
  match color_name with
  | "red" -> 0 (* Chromatic colors in spectrum order *)
  | "orange" -> 1
  | "amber" -> 2
  | "yellow" -> 3
  | "lime" -> 4
  | "green" -> 5
  | "emerald" -> 6
  | "teal" -> 7
  | "cyan" -> 8
  | "sky" -> 9
  | "blue" -> 10
  | "indigo" -> 11
  | "violet" -> 12
  | "purple" -> 13
  | "fuchsia" -> 14
  | "pink" -> 15
  | "rose" -> 16
  | "slate" -> 17 (* Neutral colors after chromatic *)
  | "gray" -> 18
  | "zinc" -> 19
  | "neutral" -> 20
  | "stone" -> 21
  | "black" -> 100 (* Special colors at the end *)
  | "white" -> 101
  | _ -> 200 (* Unknown colors last *)

(* Compare variables for ordering within a specific layer *)
let compare_for layer (Any a) (Any b) =
  let order_for_theme : type a. a t -> int = function
    (* Design tokens first - fonts are most fundamental *)
    | Font_sans -> 0
    | Font_serif -> 1
    | Font_mono -> 2
    (* Spacing comes between font-mono and default-font-family in Tailwind *)
    | Spacing -> 3
    | Default_font_family -> 4
    | Default_mono_font_family -> 5
    (* Typography scale *)
    | Text_xs -> 20
    | Text_xs_line_height -> 21
    | Text_sm -> 22
    | Text_sm_line_height -> 23
    | Text_base -> 24
    | Text_base_line_height -> 25
    | Text_lg -> 26
    | Text_lg_line_height -> 27
    | Text_xl -> 28
    | Text_xl_line_height -> 29
    | Text_2xl -> 30
    | Text_2xl_line_height -> 31
    | Text_3xl -> 32
    | Text_3xl_line_height -> 33
    | Text_4xl -> 34
    | Text_4xl_line_height -> 35
    | Text_5xl -> 36
    | Text_5xl_line_height -> 37
    | Text_6xl -> 38
    | Text_6xl_line_height -> 39
    | Text_7xl -> 40
    | Text_7xl_line_height -> 41
    | Text_8xl -> 42
    | Text_8xl_line_height -> 43
    | Text_9xl -> 44
    | Text_9xl_line_height -> 45
    (* Font weights *)
    | Font_weight_thin -> 50
    | Font_weight_extralight -> 51
    | Font_weight_light -> 52
    | Font_weight_normal -> 53
    | Font_weight_medium -> 54
    | Font_weight_semibold -> 55
    | Font_weight_bold -> 56
    | Font_weight_extrabold -> 57
    | Font_weight_black -> 58
    | Font_weight -> 59
    | Leading -> 60
    (* Border radius - common design token *)
    | Radius_none -> 70
    | Radius_sm -> 71
    | Radius_default -> 72
    | Radius_md -> 73
    | Radius_lg -> 74
    | Radius_xl -> 75
    | Radius_2xl -> 76
    | Radius_3xl -> 77
    (* Colors - organized by color *)
    | Color (name, shade) ->
        100 + (canonical_color_order name * 100) + Option.value shade ~default:0
    (* Transform variables *)
    | Translate_x -> 1000
    | Translate_y -> 1001
    | Translate_z -> 1002
    | Rotate -> 1003
    | Skew_x -> 1004
    | Skew_y -> 1005
    | Scale_x -> 1006
    | Scale_y -> 1007
    | Scale_z -> 1008
    (* Filters and effects *)
    | Blur -> 1100
    | Brightness -> 1101
    | Contrast -> 1102
    | Grayscale -> 1103
    | Hue_rotate -> 1104
    | Invert -> 1105
    | Saturate -> 1106
    | Sepia -> 1107
    | Drop_shadow -> 1108
    | Drop_shadow_alpha -> 1109
    (* Backdrop filters *)
    | Backdrop_blur -> 1200
    | Backdrop_brightness -> 1201
    | Backdrop_contrast -> 1202
    | Backdrop_grayscale -> 1203
    | Backdrop_hue_rotate -> 1204
    | Backdrop_invert -> 1205
    | Backdrop_saturate -> 1206
    | Backdrop_sepia -> 1207
    | Backdrop_opacity -> 1208
    (* Shadows and rings *)
    | Shadow -> 1300
    | Shadow_color -> 1301
    | Shadow_alpha -> 1302
    | Inset_shadow -> 1303
    | Inset_shadow_color -> 1304
    | Inset_shadow_alpha -> 1305
    | Ring_color -> 1306
    | Ring_shadow -> 1307
    | Inset_ring_color -> 1308
    | Inset_ring_shadow -> 1309
    | Ring_inset -> 1310
    | Ring_offset_width -> 1311
    | Ring_offset_color -> 1312
    | Ring_offset_shadow -> 1313
    (* Gradients *)
    | Gradient_from -> 1400
    | Gradient_via -> 1401
    | Gradient_to -> 1402
    | Gradient_stops -> 1403
    | Gradient_via_stops -> 1404
    | Gradient_position -> 1405
    | Gradient_from_position -> 1406
    | Gradient_via_position -> 1407
    | Gradient_to_position -> 1408
    (* Other *)
    | Border_style -> 1500
    | Scroll_snap_strictness -> 1501
    | Duration -> 1502
  in

  match layer with
  | Theme -> Int.compare (order_for_theme a) (order_for_theme b)
  | Properties ->
      let order_for_props : type a. a t -> int = function
        (* Transform group *)
        | Translate_x -> 0
        | Translate_y -> 1
        | Translate_z -> 2
        | Scale_x -> 3
        | Scale_y -> 4
        | Scale_z -> 5
        | Rotate -> 6
        | Skew_x -> 7
        | Skew_y -> 8
        (* Filter group *)
        | Blur -> 100
        | Brightness -> 101
        | Contrast -> 102
        | Grayscale -> 103
        | Hue_rotate -> 104
        | Invert -> 105
        | Saturate -> 106
        | Sepia -> 107
        | Drop_shadow -> 108
        | Drop_shadow_alpha -> 109
        (* Backdrop filter group *)
        | Backdrop_blur -> 200
        | Backdrop_brightness -> 201
        | Backdrop_contrast -> 202
        | Backdrop_grayscale -> 203
        | Backdrop_hue_rotate -> 204
        | Backdrop_invert -> 205
        | Backdrop_saturate -> 206
        | Backdrop_sepia -> 207
        | Backdrop_opacity -> 208
        (* Ring group *)
        | Ring_color -> 300
        | Ring_shadow -> 301
        | Ring_inset -> 302
        | Ring_offset_width -> 303
        | Ring_offset_color -> 304
        | Ring_offset_shadow -> 305
        | Inset_ring_color -> 306
        | Inset_ring_shadow -> 307
        (* Gradient group *)
        | Gradient_from -> 400
        | Gradient_via -> 401
        | Gradient_to -> 402
        | Gradient_stops -> 403
        | Gradient_via_stops -> 404
        | Gradient_position -> 405
        | Gradient_from_position -> 406
        | Gradient_via_position -> 407
        | Gradient_to_position -> 408
        (* Border and scroll *)
        | Border_style -> 500
        | Scroll_snap_strictness -> 501
        (* Shadow group *)
        | Shadow -> 600
        | Shadow_color -> 601
        | Shadow_alpha -> 602
        | Inset_shadow -> 603
        | Inset_shadow_color -> 604
        | Inset_shadow_alpha -> 605
        (* Everything else *)
        | v -> 1000 + Hashtbl.hash v
      in
      Int.compare (order_for_props a) (order_for_props b)
  | Base | Utility ->
      (* For base and utility layers, use alphabetical ordering *)
      String.compare (to_string a) (to_string b)

(* Helper to get layer name from layer enum *)
let layer_name = function
  | Theme -> "theme"
  | Base -> "base"
  | Properties -> "properties"
  | Utility -> "utilities"

let layer_of_string = function
  | "theme" -> Some Theme
  | "base" -> Some Base
  | "properties" -> Some Properties
  | "utilities" -> Some Utility
  | _ -> None

(* Get the layer from a CSS variable *)
let layer : type a. a Css.var -> layer option =
 fun css_var ->
  match css_var.layer with None -> None | Some s -> layer_of_string s

(** Create a variable definition and handle *)
let def : type a. a t -> ?layer:layer -> a -> Css.declaration * a Css.var =
 fun var_t ?layer value ->
  let n = name var_t in
  let layer = Option.map layer_name layer in
  (* Create metadata injection for this typed variable *)
  let inj, _ = Css.meta () in
  let meta = inj (Any var_t) in
  match var_t with
  | Spacing -> Css.var ?layer ~meta n Length value
  | Font_sans -> Css.var ?layer ~meta n String value
  | Font_serif -> Css.var ?layer ~meta n String value
  | Font_mono -> Css.var ?layer ~meta n String value
  | Font_weight -> Css.var ?layer ~meta n String value
  | Leading -> Css.var ?layer ~meta n String value
  | Text_xs -> Css.var ?layer ~meta n Length value
  | Text_xs_line_height -> Css.var ?layer ~meta n Length value
  | Text_sm -> Css.var ?layer ~meta n Length value
  | Text_sm_line_height -> Css.var ?layer ~meta n Length value
  | Text_base -> Css.var ?layer ~meta n Length value
  | Text_base_line_height -> Css.var ?layer ~meta n Length value
  | Text_lg -> Css.var ?layer ~meta n Length value
  | Text_lg_line_height -> Css.var ?layer ~meta n Length value
  | Text_xl -> Css.var ?layer ~meta n Length value
  | Text_xl_line_height -> Css.var ?layer ~meta n Length value
  | Text_2xl -> Css.var ?layer ~meta n Length value
  | Text_2xl_line_height -> Css.var ?layer ~meta n Length value
  | Text_3xl -> Css.var ?layer ~meta n Length value
  | Text_3xl_line_height -> Css.var ?layer ~meta n Length value
  | Text_4xl -> Css.var ?layer ~meta n Length value
  | Text_4xl_line_height -> Css.var ?layer ~meta n Length value
  | Text_5xl -> Css.var ?layer ~meta n Length value
  | Text_5xl_line_height -> Css.var ?layer ~meta n Length value
  | Text_6xl -> Css.var ?layer ~meta n Length value
  | Text_6xl_line_height -> Css.var ?layer ~meta n Length value
  | Text_7xl -> Css.var ?layer ~meta n Length value
  | Text_7xl_line_height -> Css.var ?layer ~meta n Length value
  | Text_8xl -> Css.var ?layer ~meta n Length value
  | Text_8xl_line_height -> Css.var ?layer ~meta n Length value
  | Text_9xl -> Css.var ?layer ~meta n Length value
  | Text_9xl_line_height -> Css.var ?layer ~meta n Length value
  | Font_weight_thin -> Css.var ?layer ~meta n Int value
  | Font_weight_extralight -> Css.var ?layer ~meta n Int value
  | Font_weight_light -> Css.var ?layer ~meta n Int value
  | Font_weight_normal -> Css.var ?layer ~meta n Int value
  | Font_weight_medium -> Css.var ?layer ~meta n Int value
  | Font_weight_semibold -> Css.var ?layer ~meta n Int value
  | Font_weight_bold -> Css.var ?layer ~meta n Int value
  | Font_weight_extrabold -> Css.var ?layer ~meta n Int value
  | Font_weight_black -> Css.var ?layer ~meta n Int value
  | Radius_none -> Css.var ?layer ~meta n Length value
  | Radius_sm -> Css.var ?layer ~meta n Length value
  | Radius_default -> Css.var ?layer ~meta n Length value
  | Radius_md -> Css.var ?layer ~meta n Length value
  | Radius_lg -> Css.var ?layer ~meta n Length value
  | Radius_xl -> Css.var ?layer ~meta n Length value
  | Radius_2xl -> Css.var ?layer ~meta n Length value
  | Radius_3xl -> Css.var ?layer ~meta n Length value
  | Color (color_name, shade) ->
      let clean_name =
        match shade with
        | None -> Printf.sprintf "color-%s" color_name
        | Some s -> Printf.sprintf "color-%s-%d" color_name s
      in
      Css.var ?layer ~meta clean_name Color value
  | Translate_x -> Css.var ?layer ~meta n Length value
  | Translate_y -> Css.var ?layer ~meta n Length value
  | Translate_z -> Css.var ?layer ~meta n Length value
  | Rotate -> Css.var ?layer ~meta n Angle value
  | Skew_x -> Css.var ?layer ~meta n Angle value
  | Skew_y -> Css.var ?layer ~meta n Angle value
  | Scale_x -> Css.var ?layer ~meta n Float value
  | Scale_y -> Css.var ?layer ~meta n Float value
  | Scale_z -> Css.var ?layer ~meta n Float value
  | Blur -> Css.var ?layer ~meta n Length value
  | Brightness -> Css.var ?layer ~meta n Float value
  | Contrast -> Css.var ?layer ~meta n Float value
  | Grayscale -> Css.var ?layer ~meta n Float value
  | Invert -> Css.var ?layer ~meta n Float value
  | Saturate -> Css.var ?layer ~meta n Float value
  | Sepia -> Css.var ?layer ~meta n Float value
  | Hue_rotate -> Css.var ?layer ~meta n Angle value
  | Drop_shadow -> Css.var ?layer ~meta n String value
  | Drop_shadow_alpha -> Css.var ?layer ~meta n Float value
  | Backdrop_blur -> Css.var ?layer ~meta n Length value
  | Backdrop_brightness -> Css.var ?layer ~meta n Float value
  | Backdrop_contrast -> Css.var ?layer ~meta n Float value
  | Backdrop_grayscale -> Css.var ?layer ~meta n Float value
  | Backdrop_invert -> Css.var ?layer ~meta n Float value
  | Backdrop_saturate -> Css.var ?layer ~meta n Float value
  | Backdrop_sepia -> Css.var ?layer ~meta n Float value
  | Backdrop_opacity -> Css.var ?layer ~meta n Float value
  | Backdrop_hue_rotate -> Css.var ?layer ~meta n Angle value
  | Shadow -> Css.var ?layer ~meta n String value
  | Inset_shadow -> Css.var ?layer ~meta n String value
  | Ring_shadow -> Css.var ?layer ~meta n String value
  | Inset_ring_shadow -> Css.var ?layer ~meta n String value
  | Ring_inset -> Css.var ?layer ~meta n String value
  | Ring_offset_shadow -> Css.var ?layer ~meta n String value
  | Shadow_color -> Css.var ?layer ~meta n Color value
  | Inset_shadow_color -> Css.var ?layer ~meta n Color value
  | Ring_color -> Css.var ?layer ~meta n Color value
  | Inset_ring_color -> Css.var ?layer ~meta n Color value
  | Ring_offset_color -> Css.var ?layer ~meta n Color value
  | Shadow_alpha -> Css.var ?layer ~meta n Float value
  | Inset_shadow_alpha -> Css.var ?layer ~meta n Float value
  | Ring_offset_width -> Css.var ?layer ~meta n Length value
  | Gradient_from -> Css.var ?layer ~meta n Color value
  | Gradient_via -> Css.var ?layer ~meta n Color value
  | Gradient_to -> Css.var ?layer ~meta n Color value
  | Gradient_stops -> Css.var ?layer ~meta n String value
  | Gradient_via_stops -> Css.var ?layer ~meta n String value
  | Gradient_position -> Css.var ?layer ~meta n String value
  | Gradient_from_position -> Css.var ?layer ~meta n Float value
  | Gradient_via_position -> Css.var ?layer ~meta n Float value
  | Gradient_to_position -> Css.var ?layer ~meta n Float value
  | Border_style -> Css.var ?layer ~meta n Border_style value
  | Scroll_snap_strictness -> Css.var ?layer ~meta n String value
  | Duration -> Css.var ?layer ~meta n Duration value
  | Default_font_family -> Css.var ?layer ~meta n String value
  | Default_mono_font_family -> Css.var ?layer ~meta n String value

(** Compare two any-wrapped variables *)
let compare_any a b =
  (* Special handling for Color variants *)
  match (a, b) with
  | Any (Color (name_a, shade_a)), Any (Color (name_b, shade_b)) ->
      let name_cmp =
        Int.compare
          (canonical_color_order name_a)
          (canonical_color_order name_b)
      in
      if name_cmp <> 0 then name_cmp
      else Option.compare Int.compare shade_a shade_b
  | _ ->
      (* Use polymorphic compare directly *)
      Stdlib.compare a b

module S = Set.Make (String)

(* Set module for Var.any *)
module Set = Stdlib.Set.Make (struct
  type t = any

  let compare = compare_any (* Use the any-wrapped compare function *)
end)

(* Helper to match CSS variable names to our typed variables *)
let var_of_name name =
  match name with
  | "--spacing" -> Some (Any Spacing)
  | "--font-sans" -> Some (Any Font_sans)
  | "--font-serif" -> Some (Any Font_serif)
  | "--font-mono" -> Some (Any Font_mono)
  | "--tw-font-weight" -> Some (Any Font_weight)
  | "--tw-leading" -> Some (Any Leading)
  | "--text-xs" -> Some (Any Text_xs)
  | "--text-xs--line-height" -> Some (Any Text_xs_line_height)
  | "--text-sm" -> Some (Any Text_sm)
  | "--text-sm--line-height" -> Some (Any Text_sm_line_height)
  | "--text-base" -> Some (Any Text_base)
  | "--text-base--line-height" -> Some (Any Text_base_line_height)
  | "--text-lg" -> Some (Any Text_lg)
  | "--text-lg--line-height" -> Some (Any Text_lg_line_height)
  | "--text-xl" -> Some (Any Text_xl)
  | "--text-xl--line-height" -> Some (Any Text_xl_line_height)
  | "--text-2xl" -> Some (Any Text_2xl)
  | "--text-2xl--line-height" -> Some (Any Text_2xl_line_height)
  | "--text-3xl" -> Some (Any Text_3xl)
  | "--text-3xl--line-height" -> Some (Any Text_3xl_line_height)
  | "--text-4xl" -> Some (Any Text_4xl)
  | "--text-4xl--line-height" -> Some (Any Text_4xl_line_height)
  | "--text-5xl" -> Some (Any Text_5xl)
  | "--text-5xl--line-height" -> Some (Any Text_5xl_line_height)
  | "--text-6xl" -> Some (Any Text_6xl)
  | "--text-6xl--line-height" -> Some (Any Text_6xl_line_height)
  | "--text-7xl" -> Some (Any Text_7xl)
  | "--text-7xl--line-height" -> Some (Any Text_7xl_line_height)
  | "--text-8xl" -> Some (Any Text_8xl)
  | "--text-8xl--line-height" -> Some (Any Text_8xl_line_height)
  | "--text-9xl" -> Some (Any Text_9xl)
  | "--text-9xl--line-height" -> Some (Any Text_9xl_line_height)
  | "--font-weight-thin" -> Some (Any Font_weight_thin)
  | "--font-weight-extralight" -> Some (Any Font_weight_extralight)
  | "--font-weight-light" -> Some (Any Font_weight_light)
  | "--font-weight-normal" -> Some (Any Font_weight_normal)
  | "--font-weight-medium" -> Some (Any Font_weight_medium)
  | "--font-weight-semibold" -> Some (Any Font_weight_semibold)
  | "--font-weight-bold" -> Some (Any Font_weight_bold)
  | "--font-weight-extrabold" -> Some (Any Font_weight_extrabold)
  | "--font-weight-black" -> Some (Any Font_weight_black)
  | "--radius-none" -> Some (Any Radius_none)
  | "--radius-sm" -> Some (Any Radius_sm)
  | "--radius-default" -> Some (Any Radius_default)
  | "--radius-md" -> Some (Any Radius_md)
  | "--radius-lg" -> Some (Any Radius_lg)
  | "--radius-xl" -> Some (Any Radius_xl)
  | "--radius-2xl" -> Some (Any Radius_2xl)
  | "--radius-3xl" -> Some (Any Radius_3xl)
  | "--tw-translate-x" -> Some (Any Translate_x)
  | "--tw-translate-y" -> Some (Any Translate_y)
  | "--tw-translate-z" -> Some (Any Translate_z)
  | "--tw-rotate" -> Some (Any Rotate)
  | "--tw-skew-x" -> Some (Any Skew_x)
  | "--tw-skew-y" -> Some (Any Skew_y)
  | "--tw-scale-x" -> Some (Any Scale_x)
  | "--tw-scale-y" -> Some (Any Scale_y)
  | "--tw-scale-z" -> Some (Any Scale_z)
  | "--tw-blur" -> Some (Any Blur)
  | "--tw-brightness" -> Some (Any Brightness)
  | "--tw-contrast" -> Some (Any Contrast)
  | "--tw-grayscale" -> Some (Any Grayscale)
  | "--tw-hue-rotate" -> Some (Any Hue_rotate)
  | "--tw-invert" -> Some (Any Invert)
  | "--tw-saturate" -> Some (Any Saturate)
  | "--tw-sepia" -> Some (Any Sepia)
  | "--tw-drop-shadow" -> Some (Any Drop_shadow)
  | "--tw-drop-shadow-alpha" -> Some (Any Drop_shadow_alpha)
  | "--tw-backdrop-blur" -> Some (Any Backdrop_blur)
  | "--tw-backdrop-brightness" -> Some (Any Backdrop_brightness)
  | "--tw-backdrop-contrast" -> Some (Any Backdrop_contrast)
  | "--tw-backdrop-grayscale" -> Some (Any Backdrop_grayscale)
  | "--tw-backdrop-hue-rotate" -> Some (Any Backdrop_hue_rotate)
  | "--tw-backdrop-invert" -> Some (Any Backdrop_invert)
  | "--tw-backdrop-saturate" -> Some (Any Backdrop_saturate)
  | "--tw-backdrop-sepia" -> Some (Any Backdrop_sepia)
  | "--tw-backdrop-opacity" -> Some (Any Backdrop_opacity)
  | "--tw-shadow" -> Some (Any Shadow)
  | "--tw-shadow-color" -> Some (Any Shadow_color)
  | "--tw-shadow-alpha" -> Some (Any Shadow_alpha)
  | "--tw-inset-shadow" -> Some (Any Inset_shadow)
  | "--tw-inset-shadow-color" -> Some (Any Inset_shadow_color)
  | "--tw-inset-shadow-alpha" -> Some (Any Inset_shadow_alpha)
  | "--tw-ring-color" -> Some (Any Ring_color)
  | "--tw-ring-shadow" -> Some (Any Ring_shadow)
  | "--tw-inset-ring-color" -> Some (Any Inset_ring_color)
  | "--tw-inset-ring-shadow" -> Some (Any Inset_ring_shadow)
  | "--tw-ring-inset" -> Some (Any Ring_inset)
  | "--tw-ring-offset-width" -> Some (Any Ring_offset_width)
  | "--tw-ring-offset-color" -> Some (Any Ring_offset_color)
  | "--tw-ring-offset-shadow" -> Some (Any Ring_offset_shadow)
  | "--tw-gradient-from" -> Some (Any Gradient_from)
  | "--tw-gradient-via" -> Some (Any Gradient_via)
  | "--tw-gradient-to" -> Some (Any Gradient_to)
  | "--tw-gradient-stops" -> Some (Any Gradient_stops)
  | "--tw-gradient-via-stops" -> Some (Any Gradient_via_stops)
  | "--tw-gradient-position" -> Some (Any Gradient_position)
  | "--tw-gradient-from-position" -> Some (Any Gradient_from_position)
  | "--tw-gradient-via-position" -> Some (Any Gradient_via_position)
  | "--tw-gradient-to-position" -> Some (Any Gradient_to_position)
  | "--tw-border-style" -> Some (Any Border_style)
  | "--tw-scroll-snap-strictness" -> Some (Any Scroll_snap_strictness)
  | "--tw-duration" -> Some (Any Duration)
  | "--default-font-family" -> Some (Any Default_font_family)
  | "--default-mono-font-family" -> Some (Any Default_mono_font_family)
  | _ when String.starts_with ~prefix:"--color-" name -> (
      (* Parse color variables like "--color-blue-500" *)
      let color_part = String.sub name 8 (String.length name - 8) in
      let parts = String.split_on_char '-' color_part in
      match parts with
      | [ name ] -> Some (Any (Color (name, None)))
      | name :: shade_str :: _ -> (
          try Some (Any (Color (name, Some (int_of_string shade_str))))
          with _ -> None)
      | [] -> None)
  | _ -> None

(* Default initialisers for each group when a layer is needed *)

(* Collect usages while compiling utilities *)
type tally = {
  assigned : Set.t; (* variables written by any used utility *)
  fallback_refs : Set.t; (* variables only ever seen in fallbacks *)
  (* Keep string sets for unknown variables that don't map to Var.t *)
  unknown_assigned : S.t;
  unknown_fallback_refs : S.t;
}

let empty =
  {
    assigned = Set.empty;
    fallback_refs = Set.empty;
    unknown_assigned = S.empty;
    unknown_fallback_refs = S.empty;
  }

let tally_of_vars var_names =
  List.fold_left
    (fun (assigned, unknown) var_name ->
      (* Try to match with our typed variables using var_of_name *)
      match var_of_name var_name with
      | Some var -> (Set.add var assigned, unknown)
      | None ->
          let clean_name =
            if String.starts_with ~prefix:"--" var_name then
              String.sub var_name 2 (String.length var_name - 2)
            else var_name
          in
          (assigned, S.add clean_name unknown))
    (Set.empty, S.empty) var_names
  |> fun (assigned, unknown) ->
  {
    assigned;
    fallback_refs = Set.empty;
    unknown_assigned = unknown;
    unknown_fallback_refs = S.empty;
  }

(* Canonical order for @property rules *)
let canonical_property_order_vars : any list =
  [
    Any Gradient_position;
    Any Gradient_from;
    Any Gradient_via;
    Any Gradient_to;
    Any Gradient_stops;
    Any Gradient_via_stops;
    Any Gradient_from_position;
    Any Gradient_via_position;
    Any Gradient_to_position;
    Any Font_weight;
    Any Border_style;
    Any Scroll_snap_strictness;
    Any Shadow;
    Any Shadow_color;
    Any Shadow_alpha;
    Any Inset_shadow;
    Any Inset_shadow_color;
    Any Inset_shadow_alpha;
    Any Ring_color;
    Any Ring_shadow;
    Any Inset_ring_color;
    Any Inset_ring_shadow;
    Any Ring_inset;
    Any Ring_offset_width;
    Any Ring_offset_color;
    Any Ring_offset_shadow;
    Any Scale_x;
    Any Scale_y;
    Any Scale_z;
    Any Leading;
    Any Duration;
  ]

(* Get variables that need @property rules *)
let needs_at_property (t : tally) : any list =
  (* Collect all variables that need @property rules *)
  let all_vars = Set.union t.assigned t.fallback_refs in

  (* Check if Ring_shadow group is needed *)
  let needs_ring_shadow =
    Set.exists
      (fun (Any v) ->
        match v with
        | Shadow | Shadow_color | Shadow_alpha | Inset_shadow
        | Inset_shadow_color | Inset_shadow_alpha | Ring_color | Ring_shadow
        | Inset_ring_color | Inset_ring_shadow | Ring_inset | Ring_offset_width
        | Ring_offset_color | Ring_offset_shadow ->
            true
        | _ -> false)
      all_vars
  in

  (* Check if Gradient group is needed *)
  let needs_gradient =
    Set.exists
      (fun (Any v) ->
        match v with
        | Gradient_position | Gradient_from | Gradient_via | Gradient_to
        | Gradient_stops | Gradient_via_stops | Gradient_from_position
        | Gradient_via_position | Gradient_to_position ->
            true
        | _ -> false)
      all_vars
  in

  (* Check if Scale group is needed *)
  let needs_scale =
    Set.exists
      (fun (Any v) ->
        match v with Scale_x | Scale_y | Scale_z -> true | _ -> false)
      all_vars
  in

  let needed =
    (* If Ring_shadow group is needed, include ALL shadow/ring variables *)
    let base_needed =
      if needs_ring_shadow then
        List.fold_left
          (fun acc (Any v as wrapped) ->
            match v with
            | Shadow | Shadow_color | Shadow_alpha | Inset_shadow
            | Inset_shadow_color | Inset_shadow_alpha | Ring_color | Ring_shadow
            | Inset_ring_color | Inset_ring_shadow | Ring_inset
            | Ring_offset_width | Ring_offset_color | Ring_offset_shadow ->
                Set.add wrapped acc
            | _ -> acc)
          Set.empty canonical_property_order_vars
      else Set.empty
    in

    (* If Gradient group is needed, include ALL gradient variables *)
    let base_needed =
      if needs_gradient then
        List.fold_left
          (fun acc (Any v as wrapped) ->
            match v with
            | Gradient_position | Gradient_from | Gradient_via | Gradient_to
            | Gradient_stops | Gradient_via_stops | Gradient_from_position
            | Gradient_via_position | Gradient_to_position ->
                Set.add wrapped acc
            | _ -> acc)
          base_needed canonical_property_order_vars
      else base_needed
    in

    (* If Scale group is needed, include ALL scale variables *)
    let base_needed =
      if needs_scale then
        List.fold_left
          (fun acc (Any v as wrapped) ->
            match v with
            | Scale_x | Scale_y | Scale_z -> Set.add wrapped acc
            | _ -> acc)
          base_needed canonical_property_order_vars
      else base_needed
    in

    (* Add other needed variables *)
    Set.fold
      (fun (Any v as wrapped) acc ->
        match v with
        | Font_weight -> Set.add wrapped acc
        (* Border style needs @property when referenced but not assigned *)
        | Border_style when not (Set.mem wrapped t.assigned) ->
            Set.add wrapped acc
        (* Scroll snap strictness needs @property when used *)
        | Scroll_snap_strictness -> Set.add wrapped acc
        (* Leading needs @property only when assigned (not just referenced in
           fallback) *)
        | Leading when Set.mem wrapped t.assigned -> Set.add wrapped acc
        (* Duration needs @property when used *)
        | Duration -> Set.add wrapped acc
        | _ -> acc)
      all_vars base_needed
  in

  (* Filter canonical order list to only include needed vars *)
  List.filter (fun v -> Set.mem v needed) canonical_property_order_vars

(* Get @property configuration for a variable *)
let at_property_config (Any v) : (string * string * bool * string) option =
  let name = to_string v in
  match v with
  | Font_weight -> Some (name, "*", false, "")
  | Leading -> Some (name, "*", false, "")
  | Duration -> Some (name, "*", false, "")
  | Border_style -> Some (name, "*", false, "solid")
  | Scroll_snap_strictness -> Some (name, "*", false, "proximity")
  (* Shadow variables *)
  | Shadow | Inset_shadow | Ring_shadow | Inset_ring_shadow | Ring_offset_shadow
    ->
      Some (name, "*", false, "0 0 #0000")
  | Shadow_color | Inset_shadow_color | Ring_color | Inset_ring_color
  | Ring_inset ->
      Some (name, "*", false, "")
  | Shadow_alpha | Inset_shadow_alpha ->
      Some (name, "<percentage>", false, "100%")
  | Ring_offset_width -> Some (name, "<length>", false, "0")
  | Ring_offset_color -> Some (name, "*", false, "#fff")
  (* Gradient variables *)
  | Gradient_position | Gradient_stops | Gradient_via_stops ->
      Some (name, "*", false, "")
  | Gradient_from | Gradient_via | Gradient_to ->
      Some (name, "<color>", false, "#0000")
  | Gradient_from_position -> Some (name, "<length-percentage>", false, "0%")
  | Gradient_to_position -> Some (name, "<length-percentage>", false, "100%")
  | Gradient_via_position -> Some (name, "<length-percentage>", false, "50%")
  (* Scale transform variables *)
  | Scale_x | Scale_y | Scale_z -> Some (name, "*", false, "1")
  | _ -> None

(* Create a theme layer variable *)
let theme : type a. a t -> a -> Css.declaration * a Css.var =
 fun var_t value -> def var_t ~layer:Theme value

(* Generate default font variables as declarations and handles *)
let default_font_variables () =
  (* Define font-sans and font-mono first, so we can reference them *)
  let font_sans_def, font_sans_var =
    theme Font_sans
      "ui-sans-serif,system-ui,sans-serif,\"Apple Color Emoji\",\"Segoe UI \
       Emoji\",\"Segoe UI Symbol\",\"Noto Color Emoji\""
  in
  let font_mono_def, font_mono_var =
    theme Font_mono
      "ui-monospace,SFMono-Regular,Menlo,Monaco,Consolas,\"Liberation \
       Mono\",\"Courier New\",monospace"
  in
  (* Now use typed references via var_to_string *)
  let default_font_def, _ =
    theme Default_font_family (Css.var_to_string font_sans_var)
  in
  let default_mono_def, _ =
    theme Default_mono_font_family (Css.var_to_string font_mono_var)
  in
  [
    (font_sans_def, font_sans_var);
    (font_mono_def, font_mono_var);
    (default_font_def, font_sans_var);
    (* Use font_sans_var as the handle since that's what it refers to *)
    (default_mono_def, font_mono_var);
    (* Use font_mono_var as the handle since that's what it refers to *)
  ]

(* Generate default font variable declarations for theme layer *)
let default_font_declarations () = List.map fst (default_font_variables ())

(* Canonical list of theme variables in order *)
let canonical_theme_order =
  [
    (* Font variables first *)
    Any Font_sans;
    Any Font_serif;
    Any Font_mono;
    (* Spacing comes between font-mono and default-font in Tailwind *)
    Any Spacing;
    Any Default_font_family;
    Any Default_mono_font_family;
    (* Add more as needed - this is just the critical ones for now *)
  ]

(** Compare two CSS declarations by extracting their metadata *)
let compare_declarations layer d1 d2 =
  let _, proj = Css.meta () in
  let meta1 = Css.declaration_meta d1 in
  let meta2 = Css.declaration_meta d2 in
  match (meta1, meta2) with
  | None, None -> 0 (* No metadata, equal *)
  | None, Some _ -> 1 (* No metadata sorts after *)
  | Some _, None -> -1 (* Has metadata sorts before *)
  | Some m1, Some m2 -> (
      (* Try to extract the typed variable from metadata *)
      match ((proj m1 : any option), (proj m2 : any option)) with
      | None, None -> 0
      | None, Some _ -> 1
      | Some _, None -> -1
      | Some v1, Some v2 -> compare_for layer v1 v2)
