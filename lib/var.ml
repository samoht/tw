(* Variable tracking for CSS composition groups

   Purpose: - Provide a typed view over Tailwind’s CSS custom properties (`--*`)
   used by utilities, transforms, filters, radii, fonts, etc.

   How it interacts with Rules: - `Rules.to_css` inspects declarations via
   `Css.all_vars` to determine which variables are referenced and generates a
   minimal theme layer containing only those variables. `Var.to_css_properties`
   provides default values.

   Notes: - `to_string`/`of_string` map between typed variants and the
   `--var-name`. - `canonical_order`/`compare` ensure stable, human‑friendly
   ordering in the generated CSS. *)

(** CSS variable type covering all Tailwind CSS variables *)
type t =
  (* Spacing and sizing *)
  | Spacing
  (* Typography *)
  | Font_sans
  | Font_serif
  | Font_mono
  | Font_weight
  | Leading
  | Text_xs
  | Text_xs_line_height
  | Text_sm
  | Text_sm_line_height
  | Text_base
  | Text_base_line_height
  | Text_lg
  | Text_lg_line_height
  | Text_xl
  | Text_xl_line_height
  | Text_2xl
  | Text_2xl_line_height
  | Text_3xl
  | Text_3xl_line_height
  | Text_4xl
  | Text_4xl_line_height
  | Text_5xl
  | Text_5xl_line_height
  | Text_6xl
  | Text_6xl_line_height
  | Text_7xl
  | Text_7xl_line_height
  | Text_8xl
  | Text_8xl_line_height
  | Text_9xl
  | Text_9xl_line_height
  | Font_weight_thin
  | Font_weight_extralight
  | Font_weight_light
  | Font_weight_normal
  | Font_weight_medium
  | Font_weight_semibold
  | Font_weight_bold
  | Font_weight_extrabold
  | Font_weight_black
  (* Border radius *)
  | Radius_none
  | Radius_sm
  | Radius_default
  | Radius_md
  | Radius_lg
  | Radius_xl
  | Radius_2xl
  | Radius_3xl
  (* Colors - using color name and optional shade *)
  | Color of string * int option (* e.g., Color ("blue", Some 500) *)
  (* Transform variables *)
  | Translate_x
  | Translate_y
  | Translate_z
  | Rotate
  | Skew_x
  | Skew_y
  | Scale_x
  | Scale_y
  | Scale_z
  (* Filter variables *)
  | Blur
  | Brightness
  | Contrast
  | Grayscale
  | Hue_rotate
  | Invert
  | Saturate
  | Sepia
  | Drop_shadow
  | Drop_shadow_alpha
  (* Backdrop filter variables *)
  | Backdrop_blur
  | Backdrop_brightness
  | Backdrop_contrast
  | Backdrop_grayscale
  | Backdrop_hue_rotate
  | Backdrop_invert
  | Backdrop_saturate
  | Backdrop_sepia
  | Backdrop_opacity
  (* Shadow and ring variables *)
  | Shadow
  | Shadow_color
  | Shadow_alpha
  | Inset_shadow
  | Inset_shadow_color
  | Inset_shadow_alpha
  | Ring_color
  | Ring_shadow
  | Inset_ring_color
  | Inset_ring_shadow
  | Ring_inset
  | Ring_offset_width
  | Ring_offset_color
  | Ring_offset_shadow
  (* Gradient variables *)
  | Gradient_from
  | Gradient_via
  | Gradient_to
  | Gradient_stops
  | Gradient_via_stops
  | Gradient_position
  | Gradient_from_position
  | Gradient_via_position
  | Gradient_to_position
  (* Border variables *)
  | Border_style
  (* Scroll snap variables *)
  | Scroll_snap_strictness
  (* Transition variables *)
  | Duration
  (* Default font family helpers *)
  | Default_font_family
  | Default_mono_font_family

(** Convert a CSS variable to its string representation *)
let to_string = function
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

(** Parse a CSS variable from its string representation *)
let of_string s =
  match s with
  | "--spacing" -> Some Spacing
  | "--font-sans" -> Some Font_sans
  | "--font-serif" -> Some Font_serif
  | "--font-mono" -> Some Font_mono
  | "--tw-font-weight" -> Some Font_weight
  | "--tw-leading" -> Some Leading
  | "--text-xs" -> Some Text_xs
  | "--text-xs--line-height" -> Some Text_xs_line_height
  | "--text-sm" -> Some Text_sm
  | "--text-sm--line-height" -> Some Text_sm_line_height
  | "--text-base" -> Some Text_base
  | "--text-base--line-height" -> Some Text_base_line_height
  | "--text-lg" -> Some Text_lg
  | "--text-lg--line-height" -> Some Text_lg_line_height
  | "--text-xl" -> Some Text_xl
  | "--text-xl--line-height" -> Some Text_xl_line_height
  | "--text-2xl" -> Some Text_2xl
  | "--text-2xl--line-height" -> Some Text_2xl_line_height
  | "--text-3xl" -> Some Text_3xl
  | "--text-3xl--line-height" -> Some Text_3xl_line_height
  | "--text-4xl" -> Some Text_4xl
  | "--text-4xl--line-height" -> Some Text_4xl_line_height
  | "--text-5xl" -> Some Text_5xl
  | "--text-5xl--line-height" -> Some Text_5xl_line_height
  | "--text-6xl" -> Some Text_6xl
  | "--text-6xl--line-height" -> Some Text_6xl_line_height
  | "--text-7xl" -> Some Text_7xl
  | "--text-7xl--line-height" -> Some Text_7xl_line_height
  | "--text-8xl" -> Some Text_8xl
  | "--text-8xl--line-height" -> Some Text_8xl_line_height
  | "--text-9xl" -> Some Text_9xl
  | "--text-9xl--line-height" -> Some Text_9xl_line_height
  | "--font-weight-thin" -> Some Font_weight_thin
  | "--font-weight-extralight" -> Some Font_weight_extralight
  | "--font-weight-light" -> Some Font_weight_light
  | "--font-weight-normal" -> Some Font_weight_normal
  | "--font-weight-medium" -> Some Font_weight_medium
  | "--font-weight-semibold" -> Some Font_weight_semibold
  | "--font-weight-bold" -> Some Font_weight_bold
  | "--font-weight-extrabold" -> Some Font_weight_extrabold
  | "--font-weight-black" -> Some Font_weight_black
  | "--radius-none" -> Some Radius_none
  | "--radius-sm" -> Some Radius_sm
  | "--radius-default" -> Some Radius_default
  | "--radius-md" -> Some Radius_md
  | "--radius-lg" -> Some Radius_lg
  | "--radius-xl" -> Some Radius_xl
  | "--radius-2xl" -> Some Radius_2xl
  | "--radius-3xl" -> Some Radius_3xl
  | "--tw-translate-x" -> Some Translate_x
  | "--tw-translate-y" -> Some Translate_y
  | "--tw-translate-z" -> Some Translate_z
  | "--tw-rotate" -> Some Rotate
  | "--tw-skew-x" -> Some Skew_x
  | "--tw-skew-y" -> Some Skew_y
  | "--tw-scale-x" -> Some Scale_x
  | "--tw-scale-y" -> Some Scale_y
  | "--tw-scale-z" -> Some Scale_z
  | "--tw-blur" -> Some Blur
  | "--tw-brightness" -> Some Brightness
  | "--tw-contrast" -> Some Contrast
  | "--tw-grayscale" -> Some Grayscale
  | "--tw-hue-rotate" -> Some Hue_rotate
  | "--tw-invert" -> Some Invert
  | "--tw-saturate" -> Some Saturate
  | "--tw-sepia" -> Some Sepia
  | "--tw-drop-shadow" -> Some Drop_shadow
  | "--tw-drop-shadow-alpha" -> Some Drop_shadow_alpha
  | "--tw-backdrop-blur" -> Some Backdrop_blur
  | "--tw-backdrop-brightness" -> Some Backdrop_brightness
  | "--tw-backdrop-contrast" -> Some Backdrop_contrast
  | "--tw-backdrop-grayscale" -> Some Backdrop_grayscale
  | "--tw-backdrop-hue-rotate" -> Some Backdrop_hue_rotate
  | "--tw-backdrop-invert" -> Some Backdrop_invert
  | "--tw-backdrop-saturate" -> Some Backdrop_saturate
  | "--tw-backdrop-sepia" -> Some Backdrop_sepia
  | "--tw-backdrop-opacity" -> Some Backdrop_opacity
  | "--tw-shadow" -> Some Shadow
  | "--tw-shadow-color" -> Some Shadow_color
  | "--tw-shadow-alpha" -> Some Shadow_alpha
  | "--tw-inset-shadow" -> Some Inset_shadow
  | "--tw-inset-shadow-color" -> Some Inset_shadow_color
  | "--tw-inset-shadow-alpha" -> Some Inset_shadow_alpha
  | "--tw-ring-color" -> Some Ring_color
  | "--tw-ring-shadow" -> Some Ring_shadow
  | "--tw-inset-ring-color" -> Some Inset_ring_color
  | "--tw-inset-ring-shadow" -> Some Inset_ring_shadow
  | "--tw-ring-inset" -> Some Ring_inset
  | "--tw-ring-offset-width" -> Some Ring_offset_width
  | "--tw-ring-offset-color" -> Some Ring_offset_color
  | "--tw-ring-offset-shadow" -> Some Ring_offset_shadow
  | "--tw-gradient-from" -> Some Gradient_from
  | "--tw-gradient-via" -> Some Gradient_via
  | "--tw-gradient-to" -> Some Gradient_to
  | "--tw-gradient-stops" -> Some Gradient_stops
  | "--tw-gradient-via-stops" -> Some Gradient_via_stops
  | "--tw-gradient-position" -> Some Gradient_position
  | "--tw-gradient-from-position" -> Some Gradient_from_position
  | "--tw-gradient-via-position" -> Some Gradient_via_position
  | "--tw-gradient-to-position" -> Some Gradient_to_position
  | "--tw-border-style" -> Some Border_style
  | "--tw-scroll-snap-strictness" -> Some Scroll_snap_strictness
  | "--tw-duration" -> Some Duration
  | "--default-font-family" -> Some Default_font_family
  | "--default-mono-font-family" -> Some Default_mono_font_family
  | _ ->
      (* Try to parse color variables *)
      if String.starts_with ~prefix:"--color-" s then
        let color_part = String.sub s 8 (String.length s - 8) in
        (* Try to parse color with shade *)
        match String.split_on_char '-' color_part with
        | [] -> None
        | [ color ] -> Some (Color (color, None))
        | parts -> (
            (* Try to parse the last part as a shade number *)
            let rec split_last = function
              | [] -> None
              | [ x ] -> Some ([], x)
              | h :: t -> (
                  match split_last t with
                  | None -> None
                  | Some (rest, last) -> Some (h :: rest, last))
            in
            match split_last parts with
            | None -> None
            | Some (color_parts, shade_str) -> (
                try
                  let shade = int_of_string shade_str in
                  Some (Color (String.concat "-" color_parts, Some shade))
                with _ ->
                  (* Not a valid shade, treat whole thing as color name *)
                  Some (Color (color_part, None))))
      else None

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

(* Get canonical ordering for a variable (lower = earlier) *)
let canonical_order = function
  | Spacing -> 0
  | Font_sans -> 1
  | Font_serif -> 2
  | Font_mono -> 3
  | Text_xs | Text_xs_line_height -> 10
  | Text_sm | Text_sm_line_height -> 11
  | Text_base | Text_base_line_height -> 12
  | Text_lg | Text_lg_line_height -> 13
  | Text_xl | Text_xl_line_height -> 14
  | Text_2xl | Text_2xl_line_height -> 15
  | Text_3xl | Text_3xl_line_height -> 16
  | Text_4xl | Text_4xl_line_height -> 17
  | Text_5xl | Text_5xl_line_height -> 18
  | Text_6xl | Text_6xl_line_height -> 19
  | Text_7xl | Text_7xl_line_height -> 20
  | Text_8xl | Text_8xl_line_height -> 21
  | Text_9xl | Text_9xl_line_height -> 22
  | Font_weight_thin | Font_weight_extralight | Font_weight_light
  | Font_weight_normal | Font_weight_medium | Font_weight_semibold
  | Font_weight_bold | Font_weight_extrabold | Font_weight_black ->
      30
  | Radius_none | Radius_sm | Radius_default | Radius_md | Radius_lg | Radius_xl
  | Radius_2xl | Radius_3xl ->
      35
  | Color (name, _) -> 40 + canonical_color_order name
  | _ -> 200 (* Other variables come later *)

(** Compare two variables for canonical ordering *)
let compare a b =
  let order_a = canonical_order a in
  let order_b = canonical_order b in
  if order_a <> order_b then Int.compare order_a order_b
  else
    (* For same order group, compare string representations *)
    String.compare (to_string a) (to_string b)

let pp (v : t) = to_string v

(** Generate CSS property declarations for a variable *)
let to_css_properties (v : t) : Css.declaration list =
  let open Css in
  match v with
  | Spacing -> [ custom_property "--spacing" "0.25rem" ]
  | Font_sans ->
      [
        custom_property "--font-sans"
          "ui-sans-serif, system-ui, sans-serif, \"Apple Color Emoji\", \
           \"Segoe UI Emoji\", \"Segoe UI Symbol\", \"Noto Color Emoji\"";
      ]
  | Font_serif ->
      [
        custom_property "--font-serif"
          "ui-serif, Georgia, Cambria, \"Times New Roman\", Times, serif";
      ]
  | Font_mono ->
      [
        custom_property "--font-mono"
          "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation \
           Mono\", \"Courier New\", monospace";
      ]
  | Text_xs ->
      [
        custom_property "--text-xs" "0.75rem";
        custom_property "--text-xs--line-height" "calc(1/.75)";
      ]
  | Text_sm ->
      [
        custom_property "--text-sm" "0.875rem";
        custom_property "--text-sm--line-height" "calc(1.25/.875)";
      ]
  | Text_base ->
      [
        custom_property "--text-base" "1rem";
        custom_property "--text-base--line-height" "calc(1.5/1)";
      ]
  | Text_lg ->
      [
        custom_property "--text-lg" "1.125rem";
        custom_property "--text-lg--line-height" "calc(1.75/1.125)";
      ]
  | Text_xl ->
      [
        custom_property "--text-xl" "1.25rem";
        custom_property "--text-xl--line-height" "calc(1.75/1.25)";
      ]
  | Text_2xl ->
      [
        custom_property "--text-2xl" "1.5rem";
        custom_property "--text-2xl--line-height" "calc(2/1.5)";
      ]
  | Text_3xl ->
      [
        custom_property "--text-3xl" "1.875rem";
        custom_property "--text-3xl--line-height" "calc(2.25/1.875)";
      ]
  | Text_4xl ->
      [
        custom_property "--text-4xl" "2.25rem";
        custom_property "--text-4xl--line-height" "calc(2.5/2.25)";
      ]
  | Text_5xl ->
      [
        custom_property "--text-5xl" "3rem";
        custom_property "--text-5xl--line-height" "1";
      ]
  | Text_6xl ->
      [
        custom_property "--text-6xl" "3.75rem";
        custom_property "--text-6xl--line-height" "1";
      ]
  | Text_7xl ->
      [
        custom_property "--text-7xl" "4.5rem";
        custom_property "--text-7xl--line-height" "1";
      ]
  | Text_8xl ->
      [
        custom_property "--text-8xl" "6rem";
        custom_property "--text-8xl--line-height" "1";
      ]
  | Text_9xl ->
      [
        custom_property "--text-9xl" "8rem";
        custom_property "--text-9xl--line-height" "1";
      ]
  | Font_weight_thin -> [ custom_property "--font-weight-thin" "100" ]
  | Font_weight_extralight ->
      [ custom_property "--font-weight-extralight" "200" ]
  | Font_weight_light -> [ custom_property "--font-weight-light" "300" ]
  | Font_weight_normal -> [ custom_property "--font-weight-normal" "400" ]
  | Font_weight_medium -> [ custom_property "--font-weight-medium" "500" ]
  | Font_weight_semibold -> [ custom_property "--font-weight-semibold" "600" ]
  | Font_weight_bold -> [ custom_property "--font-weight-bold" "700" ]
  | Font_weight_extrabold -> [ custom_property "--font-weight-extrabold" "800" ]
  | Font_weight_black -> [ custom_property "--font-weight-black" "900" ]
  | Radius_none -> [ custom_property "--radius-none" "0" ]
  | Radius_sm -> [ custom_property "--radius-sm" ".125rem" ]
  | Radius_default -> [ custom_property "--radius-default" ".25rem" ]
  | Radius_md -> [ custom_property "--radius-md" ".375rem" ]
  | Radius_lg -> [ custom_property "--radius-lg" ".5rem" ]
  | Radius_xl -> [ custom_property "--radius-xl" ".75rem" ]
  | Radius_2xl -> [ custom_property "--radius-2xl" "1rem" ]
  | Radius_3xl -> [ custom_property "--radius-3xl" "1.5rem" ]
  | Color ("white", _) -> [ custom_property "--color-white" "#fff" ]
  | Color ("black", _) -> [ custom_property "--color-black" "#000" ]
  | Color (name, Some shade) -> (
      let var_name = Pp.str [ "--color-"; name; "-"; string_of_int shade ] in
      try
        let color = Color.of_string_exn name in
        [ custom_property var_name (Color.to_oklch_css color shade) ]
      with _ -> [])
  | Color (_, None) -> [] (* Base colors without shade need special handling *)
  | Default_font_family ->
      [ custom_property "--default-font-family" "var(--font-sans)" ]
  | Default_mono_font_family ->
      [ custom_property "--default-mono-font-family" "var(--font-mono)" ]
  | _ -> [] (* Variables that don't need theme layer declarations *)

module S = Set.Make (String)

(* Set module for Var.t *)
module VarSet = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)

type feature_group =
  | Translate
  | Rotate
  | Skew
  | Scale
  | Filter
  | Backdrop
  | RingShadow
  | Gradient
  | Border
  | ScrollSnap
  | Other (* catch-all; usually no properties layer *)

(* Map each variable to its feature group *)
let group_of_var (v : t) : feature_group =
  match v with
  | Translate_x | Translate_y | Translate_z -> Translate
  | Rotate -> Rotate
  | Skew_x | Skew_y -> Skew
  | Scale_x | Scale_y | Scale_z -> Scale
  | Blur | Brightness | Contrast | Grayscale | Hue_rotate | Invert | Saturate
  | Sepia | Drop_shadow | Drop_shadow_alpha ->
      Filter
  | Backdrop_blur | Backdrop_brightness | Backdrop_contrast | Backdrop_grayscale
  | Backdrop_hue_rotate | Backdrop_invert | Backdrop_saturate | Backdrop_sepia
  | Backdrop_opacity ->
      Backdrop
  | Ring_offset_shadow | Ring_shadow | Shadow | Inset_shadow | Inset_ring_shadow
  | Shadow_color | Inset_shadow_color | Ring_color | Inset_ring_color
  | Ring_inset | Shadow_alpha | Inset_shadow_alpha | Ring_offset_width
  | Ring_offset_color ->
      RingShadow
  | Gradient_from | Gradient_via | Gradient_to | Gradient_stops
  | Gradient_from_position | Gradient_via_position | Gradient_to_position
  | Gradient_position | Gradient_via_stops ->
      Gradient
  | Border_style -> Border
  | Scroll_snap_strictness -> ScrollSnap
  | Font_weight | Leading -> Other (* These get special handling *)
  | _ -> Other

(* Helper to convert string to variant for backward compatibility *)
let group_of_var_string (v : string) : feature_group =
  match of_string v with Some var -> group_of_var var | None -> Other

(* Default initialisers for each group when a layer is needed *)
let defaults_for_group = function
  | Translate ->
      [
        ("--tw-translate-x", "0");
        ("--tw-translate-y", "0");
        ("--tw-translate-z", "0");
      ]
  | Rotate -> [ ("--tw-rotate", "0") ]
  | Skew -> [ ("--tw-skew-x", "0"); ("--tw-skew-y", "0") ]
  | Scale ->
      [ ("--tw-scale-x", "1"); ("--tw-scale-y", "1"); ("--tw-scale-z", "1") ]
  | Filter ->
      [
        ("--tw-blur", "");
        ("--tw-brightness", "");
        ("--tw-contrast", "");
        ("--tw-grayscale", "");
        ("--tw-hue-rotate", "");
        ("--tw-invert", "");
        ("--tw-saturate", "");
        ("--tw-sepia", "");
        ("--tw-drop-shadow", "");
      ]
  | Backdrop ->
      [
        ("--tw-backdrop-blur", "");
        ("--tw-backdrop-brightness", "");
        ("--tw-backdrop-contrast", "");
        ("--tw-backdrop-grayscale", "");
        ("--tw-backdrop-hue-rotate", "");
        ("--tw-backdrop-invert", "");
        ("--tw-backdrop-saturate", "");
        ("--tw-backdrop-sepia", "");
        ("--tw-backdrop-opacity", "");
      ]
  | RingShadow ->
      (* Canonical order for shadow/ring variables *)
      [
        ("--tw-shadow", "0 0 #0000");
        ("--tw-shadow-color", "initial");
        ("--tw-shadow-alpha", "100%");
        ("--tw-inset-shadow", "0 0 #0000");
        ("--tw-inset-shadow-color", "initial");
        ("--tw-inset-shadow-alpha", "100%");
        ("--tw-ring-color", "initial");
        ("--tw-ring-shadow", "0 0 #0000");
        ("--tw-inset-ring-color", "initial");
        ("--tw-inset-ring-shadow", "0 0 #0000");
        ("--tw-ring-inset", "initial");
        ("--tw-ring-offset-width", "0px");
        ("--tw-ring-offset-color", "#fff");
        ("--tw-ring-offset-shadow", "0 0 #0000");
      ]
  | Gradient ->
      [
        ("--tw-gradient-position", "initial");
        ("--tw-gradient-from", "#0000");
        ("--tw-gradient-via", "#0000");
        ("--tw-gradient-to", "#0000");
        ("--tw-gradient-stops", "initial");
        ("--tw-gradient-via-stops", "initial");
        ("--tw-gradient-from-position", "0%");
        ("--tw-gradient-via-position", "50%");
        ("--tw-gradient-to-position", "100%");
      ]
  | Border -> [ ("--tw-border-style", "solid") ]
  | ScrollSnap -> [ ("--tw-scroll-snap-strictness", "proximity") ]
  | Other -> []

(* Collect usages while compiling utilities *)
type tally = {
  assigned : VarSet.t; (* variables written by any used utility *)
  fallback_refs : VarSet.t; (* variables only ever seen in fallbacks *)
  (* Keep string sets for unknown variables that don't map to Var.t *)
  unknown_assigned : S.t;
  unknown_fallback_refs : S.t;
}

let empty =
  {
    assigned = VarSet.empty;
    fallback_refs = VarSet.empty;
    unknown_assigned = S.empty;
    unknown_fallback_refs = S.empty;
  }

(* Decide which groups actually need a properties layer *)
let groups_needing_layer (t : tally) : feature_group list =
  (* Only include groups for variables that are referenced but NOT assigned *)
  let unassigned_refs = VarSet.diff t.fallback_refs t.assigned in
  let unknown_unassigned_refs =
    S.diff t.unknown_fallback_refs t.unknown_assigned
  in

  (* Check which groups need initialization based on unassigned references *)
  let groups =
    VarSet.fold
      (fun v acc ->
        match v with
        | Border_style ->
            Border
            :: acc (* Border style needs init if referenced but not assigned *)
        | _ -> ( match group_of_var v with Other -> acc | g -> g :: acc))
      unassigned_refs []
  in

  (* Handle unknown variables *)
  let unknown_groups =
    S.fold
      (fun v acc ->
        match group_of_var_string v with Other -> acc | g -> g :: acc)
      unknown_unassigned_refs []
  in

  (* Also include groups for assigned variables that are composition groups (not
     Border) *)
  let assigned_groups =
    VarSet.fold
      (fun v acc ->
        match group_of_var v with
        | Border -> acc (* Don't add Border group for assignments *)
        | Other -> acc
        | g -> g :: acc)
      t.assigned []
  in

  groups @ unknown_groups @ assigned_groups |> List.sort_uniq Stdlib.compare

(* Generate properties layer initializers for needed groups *)
let generate_properties_layer (t : tally) : (string * string) list =
  (* Get defaults for composition groups *)
  let group_defaults =
    groups_needing_layer t |> List.concat_map defaults_for_group
  in

  (* Also add any special --tw- variables that need initialization *)
  let special_vars =
    VarSet.fold
      (fun v acc ->
        match v with
        | Font_weight -> (to_string Font_weight, "initial") :: acc
        | Leading -> (to_string Leading, "initial") :: acc
        | _ -> acc)
      t.assigned []
  in

  (* Preserve canonical order - don't sort, just deduplicate *)
  let seen = Hashtbl.create 32 in
  let deduplicated =
    List.filter
      (fun (k, _) ->
        if Hashtbl.mem seen k then false
        else (
          Hashtbl.add seen k ();
          true))
      (group_defaults @ special_vars)
  in
  deduplicated

(* Canonical order for @property rules *)
let canonical_property_order_vars : t list =
  [
    Gradient_position;
    Gradient_from;
    Gradient_via;
    Gradient_to;
    Gradient_stops;
    Gradient_via_stops;
    Gradient_from_position;
    Gradient_via_position;
    Gradient_to_position;
    Font_weight;
    Border_style;
    Scroll_snap_strictness;
    Shadow;
    Shadow_color;
    Shadow_alpha;
    Inset_shadow;
    Inset_shadow_color;
    Inset_shadow_alpha;
    Ring_color;
    Ring_shadow;
    Inset_ring_color;
    Inset_ring_shadow;
    Ring_inset;
    Ring_offset_width;
    Ring_offset_color;
    Ring_offset_shadow;
    Scale_x;
    Scale_y;
    Scale_z;
    Leading;
    Duration;
  ]

(* Get variables that need @property rules *)
let needs_at_property (t : tally) : t list =
  (* Collect all variables that need @property rules *)
  let all_vars = VarSet.union t.assigned t.fallback_refs in

  (* Check if RingShadow group is needed *)
  let needs_ring_shadow =
    VarSet.exists
      (fun v ->
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
    VarSet.exists
      (fun v ->
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
    VarSet.exists
      (fun v -> match v with Scale_x | Scale_y | Scale_z -> true | _ -> false)
      all_vars
  in

  let needed =
    (* If RingShadow group is needed, include ALL shadow/ring variables *)
    let base_needed =
      if needs_ring_shadow then
        List.fold_left
          (fun acc v ->
            match v with
            | Shadow | Shadow_color | Shadow_alpha | Inset_shadow
            | Inset_shadow_color | Inset_shadow_alpha | Ring_color | Ring_shadow
            | Inset_ring_color | Inset_ring_shadow | Ring_inset
            | Ring_offset_width | Ring_offset_color | Ring_offset_shadow ->
                VarSet.add v acc
            | _ -> acc)
          VarSet.empty canonical_property_order_vars
      else VarSet.empty
    in

    (* If Gradient group is needed, include ALL gradient variables *)
    let base_needed =
      if needs_gradient then
        List.fold_left
          (fun acc v ->
            match v with
            | Gradient_position | Gradient_from | Gradient_via | Gradient_to
            | Gradient_stops | Gradient_via_stops | Gradient_from_position
            | Gradient_via_position | Gradient_to_position ->
                VarSet.add v acc
            | _ -> acc)
          base_needed canonical_property_order_vars
      else base_needed
    in

    (* If Scale group is needed, include ALL scale variables *)
    let base_needed =
      if needs_scale then
        List.fold_left
          (fun acc v ->
            match v with
            | Scale_x | Scale_y | Scale_z -> VarSet.add v acc
            | _ -> acc)
          base_needed canonical_property_order_vars
      else base_needed
    in

    (* Add other needed variables *)
    VarSet.fold
      (fun v acc ->
        match v with
        | Font_weight -> VarSet.add v acc
        (* Border style needs @property when referenced but not assigned *)
        | Border_style when not (VarSet.mem v t.assigned) -> VarSet.add v acc
        (* Scroll snap strictness needs @property when used *)
        | Scroll_snap_strictness -> VarSet.add v acc
        (* Leading needs @property only when assigned (not just referenced in
           fallback) *)
        | Leading when VarSet.mem v t.assigned -> VarSet.add v acc
        (* Duration needs @property when used *)
        | Duration -> VarSet.add v acc
        | _ -> acc)
      all_vars base_needed
  in

  (* Filter canonical order list to only include needed vars *)
  List.filter (fun v -> VarSet.mem v needed) canonical_property_order_vars

(* Get @property configuration for a variable *)
let at_property_config (v : t) : (string * string * bool * string) option =
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
