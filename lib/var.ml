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

(* Layer classification for CSS variables. In v4, variables live in @layer theme
   or inline within utilities; base/properties are not used for Var-defined
   variables. *)
type layer = Theme | Utility

(* CSS variable type as a GADT for type safety. The ordering is defined
   explicitly in the `order` function below, not by constructor order. *)
type _ t =
  (* Design tokens first *)
  | Font_sans : Css.font_family list t
  | Font_mono : Css.font_family list t
  (* Colors *)
  | Color :
      string * int option
      -> Css.color t (* e.g., Color ("blue", Some 500) *)
  | Spacing : Css.length t
  | Default_font_family : Css.font_family list t
  | Default_mono_font_family : Css.font_family list t
  | Default_font_feature_settings : Css.font_feature_settings t
  | Default_font_variation_settings : Css.font_variation_settings t
  | Default_mono_font_feature_settings : Css.font_feature_settings t
  | Default_mono_font_variation_settings : Css.font_variation_settings t
  | Font_serif : Css.font_family list t
  (* Typography scale *)
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
  (* Font weights *)
  | Font_weight_thin : Css.font_weight t
  | Font_weight_extralight : Css.font_weight t
  | Font_weight_light : Css.font_weight t
  | Font_weight_normal : Css.font_weight t
  | Font_weight_medium : Css.font_weight t
  | Font_weight_semibold : Css.font_weight t
  | Font_weight_bold : Css.font_weight t
  | Font_weight_extrabold : Css.font_weight t
  | Font_weight_black : Css.font_weight t
  | Font_weight : Css.font_weight t
  | Leading : Css.length t
  (* Border radius *)
  | Radius_none : Css.length t
  | Radius_sm : Css.length t
  | Radius_default : Css.length t
  | Radius_md : Css.length t
  | Radius_lg : Css.length t
  | Radius_xl : Css.length t
  | Radius_2xl : Css.length t
  | Radius_3xl : Css.length t
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
  | Ring_width : Css.length t
  (* Prose theming variables *)
  | Prose_body : Css.color t
  | Prose_headings : Css.color t
  | Prose_code : Css.color t
  | Prose_pre_code : Css.color t
  | Prose_pre_bg : Css.color t
  | Prose_th_borders : Css.color t
  | Prose_td_borders : Css.color t
  | Prose_links : Css.color t
  | Prose_quotes : Css.color t
  | Prose_quote_borders : Css.color t
  | Prose_hr : Css.color t
  | Prose_bold : Css.color t
  | Prose_lead : Css.color t
  | Prose_counters : Css.color t
  | Prose_bullets : Css.color t
  | Prose_captions : Css.color t
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
  (* Other *)
  | Border_style : Css.border_style t
  | Scroll_snap_strictness : Css.scroll_snap_strictness t
  | Duration : Css.duration t

(** Existential wrapper for variables of any type *)
type any = Any : _ t -> any

let (meta_of_var : any -> Css.meta), (var_of_meta : Css.meta -> any option) =
  Css.meta ()

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
  | Ring_width -> "--tw-ring-width"
  | Ring_shadow -> "--tw-ring-shadow"
  | Inset_ring_color -> "--tw-inset-ring-color"
  | Inset_ring_shadow -> "--tw-inset-ring-shadow"
  | Ring_inset -> "--tw-ring-inset"
  | Ring_offset_width -> "--tw-ring-offset-width"
  | Ring_offset_color -> "--tw-ring-offset-color"
  | Ring_offset_shadow -> "--tw-ring-offset-shadow"
  (* Prose vars *)
  | Prose_body -> "--tw-prose-body"
  | Prose_headings -> "--tw-prose-headings"
  | Prose_code -> "--tw-prose-code"
  | Prose_pre_code -> "--tw-prose-pre-code"
  | Prose_pre_bg -> "--tw-prose-pre-bg"
  | Prose_th_borders -> "--tw-prose-th-borders"
  | Prose_td_borders -> "--tw-prose-td-borders"
  | Prose_links -> "--tw-prose-links"
  | Prose_quotes -> "--tw-prose-quotes"
  | Prose_quote_borders -> "--tw-prose-quote-borders"
  | Prose_hr -> "--tw-prose-hr"
  | Prose_bold -> "--tw-prose-bold"
  | Prose_lead -> "--tw-prose-lead"
  | Prose_counters -> "--tw-prose-counters"
  | Prose_bullets -> "--tw-prose-bullets"
  | Prose_captions -> "--tw-prose-captions"
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
  | Default_font_feature_settings -> "--default-font-feature-settings"
  | Default_font_variation_settings -> "--default-font-variation-settings"
  | Default_mono_font_feature_settings -> "--default-mono-font-feature-settings"
  | Default_mono_font_variation_settings ->
      "--default-mono-font-variation-settings"

let pp = to_string

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

(* Get the ordering for a variable - explicit order for theme layer *)
let order : type a. a t -> int = function
  (* Design tokens - font families first *)
  | Font_sans -> 0
  | Font_serif -> 1
  | Font_mono -> 2
  (* Then colors and spacing *)
  | Color (_, _) -> 3 (* Colors come after basic fonts *)
  | Spacing -> 4
  (* Typography scale - start at 100 *)
  | Text_xs -> 100
  | Text_xs_line_height -> 101
  | Text_sm -> 102
  | Text_sm_line_height -> 103
  | Text_base -> 104
  | Text_base_line_height -> 105
  | Text_lg -> 106
  | Text_lg_line_height -> 107
  | Text_xl -> 108
  | Text_xl_line_height -> 109
  | Text_2xl -> 110
  | Text_2xl_line_height -> 111
  | Text_3xl -> 112
  | Text_3xl_line_height -> 113
  | Text_4xl -> 114
  | Text_4xl_line_height -> 115
  | Text_5xl -> 116
  | Text_5xl_line_height -> 117
  | Text_6xl -> 118
  | Text_6xl_line_height -> 119
  | Text_7xl -> 120
  | Text_7xl_line_height -> 121
  | Text_8xl -> 122
  | Text_8xl_line_height -> 123
  | Text_9xl -> 124
  | Text_9xl_line_height -> 125
  (* Font weights - start at 200 *)
  | Font_weight_thin -> 200
  | Font_weight_extralight -> 201
  | Font_weight_light -> 202
  | Font_weight_normal -> 203
  | Font_weight_medium -> 204
  | Font_weight_semibold -> 205
  | Font_weight_bold -> 206
  | Font_weight_extrabold -> 207
  | Font_weight_black -> 208
  | Font_weight -> 209
  | Leading -> 210
  (* Default font families - start at 300 *)
  | Default_font_family -> 300
  | Default_mono_font_family -> 301
  | Default_font_feature_settings -> 302
  | Default_font_variation_settings -> 303
  | Default_mono_font_feature_settings -> 304
  | Default_mono_font_variation_settings -> 305
  (* Border radius - start at 400 *)
  | Radius_none -> 400
  | Radius_sm -> 401
  | Radius_default -> 402
  | Radius_md -> 403
  | Radius_lg -> 404
  | Radius_xl -> 405
  | Radius_2xl -> 406
  | Radius_3xl -> 407
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
  (* Filter variables *)
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
  (* Backdrop filter variables *)
  | Backdrop_blur -> 1200
  | Backdrop_brightness -> 1201
  | Backdrop_contrast -> 1202
  | Backdrop_grayscale -> 1203
  | Backdrop_hue_rotate -> 1204
  | Backdrop_invert -> 1205
  | Backdrop_saturate -> 1206
  | Backdrop_sepia -> 1207
  | Backdrop_opacity -> 1208
  (* Shadow and ring variables *)
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
  | Ring_width -> 1314
  (* Prose theming variables *)
  | Prose_body -> 1320
  | Prose_headings -> 1321
  | Prose_code -> 1322
  | Prose_pre_code -> 1323
  | Prose_pre_bg -> 1324
  | Prose_th_borders -> 1325
  | Prose_td_borders -> 1326
  | Prose_links -> 1327
  | Prose_quotes -> 1328
  | Prose_quote_borders -> 1329
  | Prose_hr -> 1330
  | Prose_bold -> 1331
  | Prose_lead -> 1332
  | Prose_counters -> 1333
  | Prose_bullets -> 1334
  | Prose_captions -> 1335
  (* Gradient variables *)
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

(* Helper to compare colors that have the same base order *)
let compare_color : type a b. a t -> b t -> int =
 fun a b ->
  match (a, b) with
  | Color (name_a, shade_a), Color (name_b, shade_b) ->
      (* First compare color names *)
      let name_cmp =
        Int.compare
          (canonical_color_order name_a)
          (canonical_color_order name_b)
      in
      if name_cmp <> 0 then name_cmp
      else
        (* Same color, compare shades *)
        Option.compare Int.compare shade_a shade_b
  | _ -> 0 (* Different types with same order, keep as is *)

(* Compare variables *)
let compare (Any a) (Any b) =
  let order_a = order a in
  let order_b = order b in
  let cmp = Int.compare order_a order_b in
  if cmp <> 0 then cmp else compare_color a b

(* Helper to get layer name from layer enum *)
let layer_name = function Theme -> "theme" | Utility -> "utilities"

let layer_of_string = function
  | "theme" -> Some Theme
  | "utilities" -> Some Utility
  | _ -> None

(* Get the layer from a CSS variable *)
let layer : type a. a Css.var -> layer option =
 fun css_var ->
  match css_var.layer with None -> None | Some s -> layer_of_string s

(** Create a variable definition and handle *)
let def : type a.
    a t -> ?layer:layer -> ?fallback:a -> a -> Css.declaration * a Css.var =
 fun var_t ?layer ?fallback value ->
  let n = name var_t in
  let layer = Option.map layer_name layer in
  (* Set metadata for this variable *)
  let meta = meta_of_var (Any var_t) in
  let var ty v = Css.var ?layer ?fallback ~meta n ty v in
  match var_t with
  | Spacing -> var Length value
  | Font_sans -> var Font_family value
  | Font_serif -> var Font_family value
  | Font_mono -> var Font_family value
  | Font_weight -> var Font_weight value
  | Leading -> var Length value
  | Text_xs -> var Length value
  | Text_xs_line_height -> var Length value
  | Text_sm -> var Length value
  | Text_sm_line_height -> var Length value
  | Text_base -> var Length value
  | Text_base_line_height -> var Length value
  | Text_lg -> var Length value
  | Text_lg_line_height -> var Length value
  | Text_xl -> var Length value
  | Text_xl_line_height -> var Length value
  | Text_2xl -> var Length value
  | Text_2xl_line_height -> var Length value
  | Text_3xl -> var Length value
  | Text_3xl_line_height -> var Length value
  | Text_4xl -> var Length value
  | Text_4xl_line_height -> var Length value
  | Text_5xl -> var Length value
  | Text_5xl_line_height -> var Length value
  | Text_6xl -> var Length value
  | Text_6xl_line_height -> var Length value
  | Text_7xl -> var Length value
  | Text_7xl_line_height -> var Length value
  | Text_8xl -> var Length value
  | Text_8xl_line_height -> var Length value
  | Text_9xl -> var Length value
  | Text_9xl_line_height -> var Length value
  | Font_weight_thin -> var Font_weight value
  | Font_weight_extralight -> var Font_weight value
  | Font_weight_light -> var Font_weight value
  | Font_weight_normal -> var Font_weight value
  | Font_weight_medium -> var Font_weight value
  | Font_weight_semibold -> var Font_weight value
  | Font_weight_bold -> var Font_weight value
  | Font_weight_extrabold -> var Font_weight value
  | Font_weight_black -> var Font_weight value
  | Radius_none -> var Length value
  | Radius_sm -> var Length value
  | Radius_default -> var Length value
  | Radius_md -> var Length value
  | Radius_lg -> var Length value
  | Radius_xl -> var Length value
  | Radius_2xl -> var Length value
  | Radius_3xl -> var Length value
  | Color (color_name, shade) ->
      let clean_name =
        match shade with
        | None -> Printf.sprintf "color-%s" color_name
        | Some s -> Printf.sprintf "color-%s-%d" color_name s
      in
      Css.var ?layer ?fallback ~meta clean_name Color value
  | Translate_x -> var Length value
  | Translate_y -> var Length value
  | Translate_z -> var Length value
  | Rotate -> var Angle value
  | Skew_x -> var Angle value
  | Skew_y -> var Angle value
  | Scale_x -> var Float value
  | Scale_y -> var Float value
  | Scale_z -> var Float value
  | Blur -> var Length value
  | Brightness -> var Float value
  | Contrast -> var Float value
  | Grayscale -> var Float value
  | Invert -> var Float value
  | Saturate -> var Float value
  | Sepia -> var Float value
  | Hue_rotate -> var Angle value
  | Drop_shadow -> var String value
  | Drop_shadow_alpha -> var Float value
  | Backdrop_blur -> var Length value
  | Backdrop_brightness -> var Float value
  | Backdrop_contrast -> var Float value
  | Backdrop_grayscale -> var Float value
  | Backdrop_invert -> var Float value
  | Backdrop_saturate -> var Float value
  | Backdrop_sepia -> var Float value
  | Backdrop_opacity -> var Float value
  | Backdrop_hue_rotate -> var Angle value
  | Shadow -> var String value
  | Inset_shadow -> var String value
  | Ring_shadow -> var String value
  | Inset_ring_shadow -> var String value
  | Ring_inset -> var String value
  | Ring_offset_shadow -> var String value
  | Shadow_color -> var Color value
  | Inset_shadow_color -> var Color value
  | Ring_color -> var Color value
  | Inset_ring_color -> var Color value
  | Ring_offset_color -> var Color value
  | Shadow_alpha -> var Float value
  | Inset_shadow_alpha -> var Float value
  | Ring_offset_width -> var Length value
  (* Prose vars *)
  | Prose_body -> var Color value
  | Prose_headings -> var Color value
  | Prose_code -> var Color value
  | Prose_pre_code -> var Color value
  | Prose_pre_bg -> var Color value
  | Prose_th_borders -> var Color value
  | Prose_td_borders -> var Color value
  | Prose_links -> var Color value
  | Prose_quotes -> var Color value
  | Prose_quote_borders -> var Color value
  | Prose_hr -> var Color value
  | Prose_bold -> var Color value
  | Prose_lead -> var Color value
  | Prose_counters -> var Color value
  | Prose_bullets -> var Color value
  | Prose_captions -> var Color value
  | Ring_width -> var Length value
  | Gradient_from -> var Color value
  | Gradient_via -> var Color value
  | Gradient_to -> var Color value
  | Gradient_stops -> var String value
  | Gradient_via_stops -> var String value
  | Gradient_position -> var String value
  | Gradient_from_position -> var Float value
  | Gradient_via_position -> var Float value
  | Gradient_to_position -> var Float value
  | Border_style -> var Border_style value
  | Scroll_snap_strictness -> var Scroll_snap_strictness value
  | Duration -> var Duration value
  | Default_font_family -> var Font_family value
  | Default_mono_font_family -> var Font_family value
  | Default_font_feature_settings -> var Font_feature_settings value
  | Default_font_variation_settings -> var Font_variation_settings value
  | Default_mono_font_feature_settings -> var Font_feature_settings value
  | Default_mono_font_variation_settings -> var Font_variation_settings value

(* Layer-specific variable constructors *)
let theme : type a. a t -> ?fallback:a -> a -> Css.declaration * a Css.var =
 fun var_t ?fallback value -> def ?fallback var_t ~layer:Theme value

let utility : type a. a t -> ?fallback:a -> a -> Css.declaration * a Css.var =
 fun var_t ?fallback value -> def ?fallback var_t ~layer:Utility value

(* Create @property rule for a variable *)
let property : type a.
    a t -> syntax:string -> inherits:bool -> initial:string -> Css.property_rule
    =
 fun var_t ~syntax ~inherits ~initial ->
  let var_name = to_string var_t in
  Css.property ~name:var_name ~syntax ~inherits ~initial_value:initial ()

let canonical_theme_order =
  [ Any Font_sans; Any Font_serif; Any Font_mono; Any Spacing ]

(** Helper for metadata errors *)
let err_meta ~layer decl msg =
  let name =
    Option.value ~default:"<unnamed>" (Css.custom_declaration_name decl)
  in
  let layer_str = layer_name layer in
  let accessor = match layer with Theme -> "theme" | Utility -> "utility" in
  failwith
    (Pp.str
       [
         msg;
         " for '";
         name;
         "' in ";
         layer_str;
         " layer. Define this variable via Var.";
         accessor;
         " to attach Var metadata (e.g., Var.theme/base/properties/utility).";
       ])

(** Compare two CSS declarations by extracting their metadata. *)

let compare_declarations layer d1 d2 =
  match (Css.declaration_meta d1, Css.declaration_meta d2) with
  | Some m1, Some m2 -> (
      match (var_of_meta m1, var_of_meta m2) with
      | Some v1, Some v2 -> compare v1 v2
      | Some _, None ->
          err_meta ~layer d2 "Invalid Var metadata (var_of_meta failed)"
      | None, Some _ ->
          err_meta ~layer d1 "Invalid Var metadata (var_of_meta failed)"
      | None, None ->
          failwith "Both declarations have metadata but var_of_meta failed")
  | Some _, None -> err_meta ~layer d2 "Missing Var metadata"
  | None, Some _ -> err_meta ~layer d1 "Missing Var metadata"
  | None, None ->
      let n1 =
        Option.value ~default:"<unnamed>" (Css.custom_declaration_name d1)
      in
      let n2 =
        Option.value ~default:"<unnamed>" (Css.custom_declaration_name d2)
      in
      let layer_str = layer_name layer in
      let accessor =
        match layer with Theme -> "theme" | Utility -> "utility"
      in
      failwith
        (Pp.str
           [
             "Missing Var metadata for '";
             n1;
             "' and '";
             n2;
             "' in ";
             layer_str;
             " layer. Define these variables via Var.";
             accessor;
             " to attach metadata.";
           ])
