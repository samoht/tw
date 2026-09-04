(** Color conversion utilities for Tailwind v4 compatibility *)

module Css = Cascade.Css

type rgb = {
  r : int;  (** Red channel (0-255) *)
  g : int;  (** Green channel (0-255) *)
  b : int;  (** Blue channel (0-255) *)
}

type oklch = {
  l : float;  (** Lightness (0-100) *)
  c : float;  (** Chroma (0-0.4+) *)
  h : float;  (** Hue (0-360) *)
}

type color =
  | Black
  | White
  | Gray
  | Slate
  | Zinc
  | Neutral
  | Stone
  | Mauve
  | Olive
  | Mist
  | Taupe
  | Red
  | Orange
  | Amber
  | Yellow
  | Lime
  | Green
  | Emerald
  | Teal
  | Cyan
  | Sky
  | Blue
  | Indigo
  | Violet
  | Purple
  | Fuchsia
  | Pink
  | Rose
  | Hex of string
  | Rgb of { red : int; green : int; blue : int }
  | Oklch of oklch
  | Css of Css.color
  | Theme_named of string

(* The colour-space arithmetic (linearisation, OKLab, the polar OKLCh form) is
   cascade's: a second copy drifts, since a forward matrix straight to LMS and
   an inverse routed via XYZ are not exact inverses, and [#0088cc] then comes
   back as [#0288cc]. Only [linearize_channel] and [gamma_correct] stay private,
   because they are byte <-> [0, 1] float conversions that cascade's float-only
   API has no reason to own.

   [gamut_map_chroma] below stays private for a different reason. Cascade has a
   chroma search of its own, [Color_space.gamut_mapped_srgb_of_oklch], but it
   answers CSS Color 4 sec. 14.2.2 and keeps the largest chroma whose clipped
   result is within a just noticeable difference. Tailwind's published CSS
   carries the fallback lightningcss folds out of its sRGB [color-mix], and that
   one stops at the first such chroma: for [--color-blue-500] the spec answers
   [#2b7fff] and Tailwind ships [#3080ff]. Parity is with what Tailwind ships,
   so the search below is the one that runs. *)
let linearize_channel c =
  Cascade.Color_space.linear_of_srgb (float_of_int c /. 255.0)

let gamma_correct c =
  int_of_float ((Cascade.Color_space.srgb_of_linear c *. 255.0) +. 0.5)

let rgb_to_oklch rgb =
  let r_lin = linearize_channel rgb.r in
  let g_lin = linearize_channel rgb.g in
  let b_lin = linearize_channel rgb.b in
  let lab = Cascade.Color_space.oklab_of_linear_srgb (r_lin, g_lin, b_lin) in
  let ok_l, chroma, hue = Cascade.Color_space.oklch_of_oklab lab in
  { l = ok_l *. 100.0; c = chroma; h = hue }

let rgb_to_oklab rgb =
  let r_lin = linearize_channel rgb.r in
  let g_lin = linearize_channel rgb.g in
  let b_lin = linearize_channel rgb.b in
  let ok_l, ok_a, ok_b =
    Cascade.Color_space.oklab_of_linear_srgb (r_lin, g_lin, b_lin)
  in
  (ok_l *. 100.0, ok_a, ok_b)

let clip_val x = Float.max 0.0 (Float.min 1.0 x)
let clip (r, g, b) = (clip_val r, clip_val g, clip_val b)

let in_gamut (r, g, b) =
  r >= 0.0 && r <= 1.0 && g >= 0.0 && g <= 1.0 && b >= 0.0 && b <= 1.0

(* Halve the chroma at constant lightness and hue until the clipped colour is
   within one just noticeable difference of the colour searched, and take that
   first one. *)
let gamut_map_chroma ~ok_l ~cos_h ~sin_h chroma =
  let jnd = 0.02 in
  let epsilon = 0.00001 in
  (* [cos_h]/[sin_h] are precomputed once by the caller and reused across every
     iteration of the search below, so this stays a closure over them rather
     than a call to cascade's [oklab_of_oklch], which takes a hue in degrees and
     would redo the trigonometry on each call. *)
  let oklch_to_linear c =
    Cascade.Color_space.linear_srgb_of_oklab (ok_l, c *. cos_h, c *. sin_h)
  in
  let delta_e_ok (r, g, b) c =
    let lab = Cascade.Color_space.oklab_of_linear_srgb (r, g, b) in
    Cascade.Color_space.oklab_distance lab (ok_l, c *. cos_h, c *. sin_h)
  in
  let rgb = oklch_to_linear chroma in
  if in_gamut rgb then clip rgb
  else
    let min_c = ref 0.0 in
    let max_c = ref chroma in
    let result = ref None in
    while !max_c -. !min_c > epsilon && !result = None do
      let c = (!min_c +. !max_c) /. 2.0 in
      let rgb = oklch_to_linear c in
      if in_gamut rgb then min_c := c
      else
        let clipped = clip rgb in
        let de = delta_e_ok clipped c in
        if de < jnd then result := Some clipped else max_c := c
    done;
    match !result with
    | Some clipped -> clipped
    | None -> clip (oklch_to_linear !min_c)

let oklch_to_rgb oklch =
  let epsilon = 0.00001 in
  let ok_l = oklch.l /. 100.0 in
  if Float.abs (ok_l -. 1.0) < epsilon || ok_l > 1.0 then
    { r = 255; g = 255; b = 255 }
  else if ok_l < epsilon then { r = 0; g = 0; b = 0 }
  else
    let h_rad = oklch.h *. Float.pi /. 180.0 in
    let r, g, b =
      gamut_map_chroma ~ok_l ~cos_h:(cos h_rad) ~sin_h:(sin h_rad) oklch.c
    in
    let clamp x = max 0 (min 255 x) in
    {
      r = clamp (gamma_correct r);
      g = clamp (gamma_correct g);
      b = clamp (gamma_correct b);
    }

let hex_to_rgb hex =
  try
    let hex_str =
      if String.starts_with ~prefix:"#" hex then
        String.sub hex 1 (String.length hex - 1)
      else hex
    in
    let len = String.length hex_str in
    if len = 3 then
      (* Short form: #RGB -> #RRGGBB *)
      let r_char = String.make 1 hex_str.[0] in
      let g_char = String.make 1 hex_str.[1] in
      let b_char = String.make 1 hex_str.[2] in
      let r = int_of_string ("0x" ^ r_char ^ r_char) in
      let g = int_of_string ("0x" ^ g_char ^ g_char) in
      let b = int_of_string ("0x" ^ b_char ^ b_char) in
      Some { r; g; b }
    else if len = 6 then
      (* Full form: #RRGGBB *)
      let r = int_of_string ("0x" ^ String.sub hex_str 0 2) in
      let g = int_of_string ("0x" ^ String.sub hex_str 2 2) in
      let b = int_of_string ("0x" ^ String.sub hex_str 4 2) in
      Some { r; g; b }
    else None
  with Invalid_argument _ | Failure _ -> None

let rgb_to_hex rgb =
  "#" ^ Pp.hex_byte rgb.r ^ Pp.hex_byte rgb.g ^ Pp.hex_byte rgb.b

(** Add alpha to a hex color string. Returns #RRGGBBAA format. The opacity is a
    percentage (0-100). *)
let hex_with_alpha hex_str opacity_percent =
  (* Parse hex color *)
  let hex_clean =
    if String.length hex_str > 0 && hex_str.[0] = '#' then
      String.sub hex_str 1 (String.length hex_str - 1)
    else hex_str
  in
  (* The alpha byte is appended to the six RGB digits, so a shorthand has to be
     expanded first: [#fff] would otherwise give the five-digit [#fff1a]. Any
     alpha already present is the one being replaced. *)
  let hex_clean =
    let double c = String.make 2 c in
    match String.length hex_clean with
    | 3 | 4 ->
        double hex_clean.[0] ^ double hex_clean.[1] ^ double hex_clean.[2]
    | 8 -> String.sub hex_clean 0 6
    | _ -> hex_clean
  in
  (* Convert opacity percentage to 8-bit alpha value, with rounding *)
  let alpha = int_of_float ((opacity_percent /. 100.0 *. 255.0) +. 0.5) in
  let alpha_clamped = max 0 (min 255 alpha) in
  let full = hex_clean ^ Pp.hex_byte alpha_clamped in
  (* Shorten #RRGGBBAA → #RGBA when each pair is identical *)
  let len = String.length full in
  let shortened =
    if
      len = 8
      && full.[0] = full.[1]
      && full.[2] = full.[3]
      && full.[4] = full.[5]
      && full.[6] = full.[7]
    then (
      let s = Bytes.create 4 in
      Bytes.set s 0 full.[0];
      Bytes.set s 1 full.[2];
      Bytes.set s 2 full.[4];
      Bytes.set s 3 full.[6];
      Bytes.unsafe_to_string s)
    else full
  in
  "#" ^ shortened

let oklch_to_css oklch =
  let f n = Css.Pp.string_of_float ~drop_leading_zero:false ~max_decimals:n in
  String.concat ""
    [ "oklch("; f 1 oklch.l; "% "; f 3 oklch.c; " "; f 3 oklch.h; ")" ]

let hex_to_oklch_css hex =
  match hex_to_rgb hex with
  | Some rgb -> oklch_to_css (rgb_to_oklch rgb)
  | None -> hex (* Fallback to original hex if parsing fails *)

let round_n n f =
  let factor = 10.0 ** float_of_int n in
  Float.round (f *. factor) /. factor

let hex_to_oklab_alpha hex alpha : Css.color =
  match hex_to_rgb hex with
  | Some rgb ->
      let l, a, b = rgb_to_oklab rgb in
      (* Raw floats — precision is controlled at CSS emission time in
         pp_oklab *)
      Css.oklaba l a b alpha
  | None -> Css.hex hex

(* A custom colour (a hex or an rgb() the author wrote) with an alpha folded in.
   Tailwind writes the oklab form, with [none] for a channel that is zero. *)
let custom_color_with_alpha (c : color) alpha =
  let ok_l, ok_a, ok_b =
    match c with
    | Hex h -> (
        match hex_to_rgb h with
        | Some rgb -> rgb_to_oklab rgb
        | None -> (0.0, 0.0, 0.0))
    | Rgb { red; green; blue } -> rgb_to_oklab { r = red; g = green; b = blue }
    | _ -> (0.0, 0.0, 0.0)
  in
  Css.oklaba_none_zeros ok_l ok_a ok_b alpha

module Tailwind = struct
  let gray =
    [
      (50, { l = 98.5; c = 0.002; h = 247.839 });
      (100, { l = 96.7; c = 0.003; h = 264.542 });
      (200, { l = 92.8; c = 0.006; h = 264.531 });
      (300, { l = 87.2; c = 0.01; h = 258.338 });
      (400, { l = 70.7; c = 0.022; h = 261.325 });
      (500, { l = 55.1; c = 0.027; h = 264.364 });
      (600, { l = 44.6; c = 0.03; h = 256.802 });
      (700, { l = 37.3; c = 0.034; h = 259.733 });
      (800, { l = 27.8; c = 0.033; h = 256.848 });
      (900, { l = 21.0; c = 0.034; h = 264.665 });
      (950, { l = 13.0; c = 0.028; h = 261.692 });
    ]

  let blue =
    [
      (50, { l = 97.0; c = 0.014; h = 254.604 });
      (100, { l = 93.2; c = 0.032; h = 255.585 });
      (200, { l = 88.2; c = 0.059; h = 254.128 });
      (300, { l = 80.9; c = 0.105; h = 251.813 });
      (400, { l = 70.7; c = 0.165; h = 254.624 });
      (500, { l = 62.3; c = 0.214; h = 259.815 });
      (600, { l = 54.6; c = 0.245; h = 262.881 });
      (700, { l = 48.8; c = 0.243; h = 264.376 });
      (800, { l = 42.4; c = 0.199; h = 265.638 });
      (900, { l = 37.9; c = 0.146; h = 265.522 });
      (950, { l = 28.2; c = 0.091; h = 267.935 });
    ]

  let red =
    [
      (50, { l = 97.1; c = 0.013; h = 17.38 });
      (100, { l = 93.6; c = 0.032; h = 17.717 });
      (200, { l = 88.5; c = 0.062; h = 18.334 });
      (300, { l = 80.8; c = 0.114; h = 19.571 });
      (400, { l = 70.4; c = 0.191; h = 22.216 });
      (500, { l = 63.7; c = 0.237; h = 25.331 });
      (600, { l = 57.7; c = 0.245; h = 27.325 });
      (700, { l = 50.5; c = 0.213; h = 27.518 });
      (800, { l = 44.4; c = 0.177; h = 26.899 });
      (900, { l = 39.6; c = 0.141; h = 25.723 });
      (950, { l = 25.8; c = 0.092; h = 26.042 });
    ]

  let slate =
    [
      (50, { l = 98.4; c = 0.003; h = 247.858 });
      (100, { l = 96.8; c = 0.007; h = 247.896 });
      (200, { l = 92.9; c = 0.013; h = 255.508 });
      (300, { l = 86.9; c = 0.022; h = 252.894 });
      (400, { l = 70.4; c = 0.04; h = 256.788 });
      (500, { l = 55.4; c = 0.046; h = 257.417 });
      (600, { l = 44.6; c = 0.043; h = 257.281 });
      (700, { l = 37.2; c = 0.044; h = 257.287 });
      (800, { l = 27.9; c = 0.041; h = 260.031 });
      (900, { l = 20.8; c = 0.042; h = 265.755 });
      (950, { l = 12.9; c = 0.042; h = 264.695 });
    ]

  let zinc =
    [
      (50, { l = 98.5; c = 0.0; h = 0.0 });
      (100, { l = 96.7; c = 0.001; h = 286.375 });
      (200, { l = 92.0; c = 0.004; h = 286.32 });
      (300, { l = 87.1; c = 0.006; h = 286.286 });
      (400, { l = 70.5; c = 0.015; h = 286.067 });
      (500, { l = 55.2; c = 0.016; h = 285.938 });
      (600, { l = 44.2; c = 0.017; h = 285.786 });
      (700, { l = 37.0; c = 0.013; h = 285.805 });
      (800, { l = 27.4; c = 0.006; h = 286.033 });
      (900, { l = 21.0; c = 0.006; h = 285.885 });
      (950, { l = 14.1; c = 0.005; h = 285.823 });
    ]

  let neutral =
    [
      (50, { l = 98.5; c = 0.0; h = 0.0 });
      (100, { l = 97.0; c = 0.0; h = 0.0 });
      (200, { l = 92.2; c = 0.0; h = 0.0 });
      (300, { l = 87.0; c = 0.0; h = 0.0 });
      (400, { l = 70.8; c = 0.0; h = 0.0 });
      (500, { l = 55.6; c = 0.0; h = 0.0 });
      (600, { l = 43.9; c = 0.0; h = 0.0 });
      (700, { l = 37.1; c = 0.0; h = 0.0 });
      (800, { l = 26.9; c = 0.0; h = 0.0 });
      (900, { l = 20.5; c = 0.0; h = 0.0 });
      (950, { l = 14.5; c = 0.0; h = 0.0 });
    ]

  let stone =
    [
      (50, { l = 98.5; c = 0.001; h = 106.423 });
      (100, { l = 97.0; c = 0.001; h = 106.424 });
      (200, { l = 92.3; c = 0.003; h = 48.717 });
      (300, { l = 86.9; c = 0.005; h = 56.366 });
      (400, { l = 70.9; c = 0.01; h = 56.259 });
      (500, { l = 55.3; c = 0.013; h = 58.071 });
      (600, { l = 44.4; c = 0.011; h = 73.639 });
      (700, { l = 37.4; c = 0.01; h = 67.558 });
      (800, { l = 26.8; c = 0.007; h = 34.298 });
      (900, { l = 21.6; c = 0.006; h = 56.043 });
      (950, { l = 14.7; c = 0.004; h = 49.25 });
    ]

  let mauve =
    [
      (50, { l = 98.5; c = 0.0; h = 0.0 });
      (100, { l = 96.0; c = 0.003; h = 325.6 });
      (200, { l = 92.2; c = 0.005; h = 325.62 });
      (300, { l = 86.5; c = 0.012; h = 325.68 });
      (400, { l = 71.1; c = 0.019; h = 323.02 });
      (500, { l = 54.2; c = 0.034; h = 322.5 });
      (600, { l = 43.5; c = 0.029; h = 321.78 });
      (700, { l = 36.4; c = 0.029; h = 323.89 });
      (800, { l = 26.3; c = 0.024; h = 320.12 });
      (900, { l = 21.2; c = 0.019; h = 322.12 });
      (950, { l = 14.5; c = 0.008; h = 326.0 });
    ]

  let olive =
    [
      (50, { l = 98.8; c = 0.003; h = 106.5 });
      (100, { l = 96.6; c = 0.005; h = 106.5 });
      (200, { l = 93.0; c = 0.007; h = 106.5 });
      (300, { l = 88.0; c = 0.011; h = 106.6 });
      (400, { l = 73.7; c = 0.021; h = 106.9 });
      (500, { l = 58.0; c = 0.031; h = 107.3 });
      (600, { l = 46.6; c = 0.025; h = 107.3 });
      (700, { l = 39.4; c = 0.023; h = 107.4 });
      (800, { l = 28.6; c = 0.016; h = 107.4 });
      (900, { l = 22.8; c = 0.013; h = 107.4 });
      (950, { l = 15.3; c = 0.006; h = 107.1 });
    ]

  let mist =
    [
      (50, { l = 98.7; c = 0.002; h = 197.1 });
      (100, { l = 96.3; c = 0.002; h = 197.1 });
      (200, { l = 92.5; c = 0.005; h = 214.3 });
      (300, { l = 87.2; c = 0.007; h = 219.6 });
      (400, { l = 72.3; c = 0.014; h = 214.4 });
      (500, { l = 56.0; c = 0.021; h = 213.5 });
      (600, { l = 45.0; c = 0.017; h = 213.2 });
      (700, { l = 37.8; c = 0.015; h = 216.0 });
      (800, { l = 27.5; c = 0.011; h = 216.9 });
      (900, { l = 21.8; c = 0.008; h = 223.9 });
      (950, { l = 14.8; c = 0.004; h = 228.8 });
    ]

  let taupe =
    [
      (50, { l = 98.6; c = 0.002; h = 67.8 });
      (100, { l = 96.0; c = 0.002; h = 17.2 });
      (200, { l = 92.2; c = 0.005; h = 34.3 });
      (300, { l = 86.8; c = 0.007; h = 39.5 });
      (400, { l = 71.4; c = 0.014; h = 41.2 });
      (500, { l = 54.7; c = 0.021; h = 43.1 });
      (600, { l = 43.8; c = 0.017; h = 39.3 });
      (700, { l = 36.7; c = 0.016; h = 35.7 });
      (800, { l = 26.8; c = 0.011; h = 36.5 });
      (900, { l = 21.4; c = 0.009; h = 43.1 });
      (950, { l = 14.7; c = 0.004; h = 49.3 });
    ]

  let orange =
    [
      (50, { l = 98.0; c = 0.016; h = 73.684 });
      (100, { l = 95.4; c = 0.038; h = 75.164 });
      (200, { l = 90.1; c = 0.076; h = 70.697 });
      (300, { l = 83.7; c = 0.128; h = 66.29 });
      (400, { l = 75.0; c = 0.183; h = 55.934 });
      (500, { l = 70.5; c = 0.213; h = 47.604 });
      (600, { l = 64.6; c = 0.222; h = 41.116 });
      (700, { l = 55.3; c = 0.195; h = 38.402 });
      (800, { l = 47.0; c = 0.157; h = 37.304 });
      (900, { l = 40.8; c = 0.123; h = 38.172 });
      (950, { l = 26.6; c = 0.079; h = 36.259 });
    ]

  let amber =
    [
      (50, { l = 98.7; c = 0.022; h = 95.277 });
      (100, { l = 96.2; c = 0.059; h = 95.617 });
      (200, { l = 92.4; c = 0.12; h = 95.746 });
      (300, { l = 87.9; c = 0.169; h = 91.605 });
      (400, { l = 82.8; c = 0.189; h = 84.429 });
      (500, { l = 76.9; c = 0.188; h = 70.08 });
      (600, { l = 66.6; c = 0.179; h = 58.318 });
      (700, { l = 55.5; c = 0.163; h = 48.998 });
      (800, { l = 47.3; c = 0.137; h = 46.201 });
      (900, { l = 41.4; c = 0.112; h = 45.904 });
      (950, { l = 27.9; c = 0.077; h = 45.635 });
    ]

  let yellow =
    [
      (50, { l = 98.7; c = 0.026; h = 102.212 });
      (100, { l = 97.3; c = 0.071; h = 103.193 });
      (200, { l = 94.5; c = 0.129; h = 101.54 });
      (300, { l = 90.5; c = 0.182; h = 98.111 });
      (400, { l = 85.2; c = 0.199; h = 91.936 });
      (500, { l = 79.5; c = 0.184; h = 86.047 });
      (600, { l = 68.1; c = 0.162; h = 75.834 });
      (700, { l = 55.4; c = 0.135; h = 66.442 });
      (800, { l = 47.6; c = 0.114; h = 61.907 });
      (900, { l = 42.1; c = 0.095; h = 57.708 });
      (950, { l = 28.6; c = 0.066; h = 53.813 });
    ]

  let lime =
    [
      (50, { l = 98.6; c = 0.031; h = 120.757 });
      (100, { l = 96.7; c = 0.067; h = 122.328 });
      (200, { l = 93.8; c = 0.127; h = 124.321 });
      (300, { l = 89.7; c = 0.196; h = 126.665 });
      (400, { l = 84.1; c = 0.238; h = 128.85 });
      (500, { l = 76.8; c = 0.233; h = 130.85 });
      (600, { l = 64.8; c = 0.2; h = 131.684 });
      (700, { l = 53.2; c = 0.157; h = 131.589 });
      (800, { l = 45.3; c = 0.124; h = 130.933 });
      (900, { l = 40.5; c = 0.101; h = 131.063 });
      (950, { l = 27.4; c = 0.072; h = 132.109 });
    ]

  let green =
    [
      (50, { l = 98.2; c = 0.018; h = 155.826 });
      (100, { l = 96.2; c = 0.044; h = 156.743 });
      (200, { l = 92.5; c = 0.084; h = 155.995 });
      (300, { l = 87.1; c = 0.15; h = 154.449 });
      (400, { l = 79.2; c = 0.209; h = 151.711 });
      (500, { l = 72.3; c = 0.219; h = 149.579 });
      (600, { l = 62.7; c = 0.194; h = 149.214 });
      (700, { l = 52.7; c = 0.154; h = 150.069 });
      (800, { l = 44.8; c = 0.119; h = 151.328 });
      (900, { l = 39.3; c = 0.095; h = 152.535 });
      (950, { l = 26.6; c = 0.065; h = 152.934 });
    ]

  let emerald =
    [
      (50, { l = 97.9; c = 0.021; h = 166.113 });
      (100, { l = 95.0; c = 0.052; h = 163.051 });
      (200, { l = 90.5; c = 0.093; h = 164.15 });
      (300, { l = 84.5; c = 0.143; h = 164.978 });
      (400, { l = 76.5; c = 0.177; h = 163.223 });
      (500, { l = 69.6; c = 0.17; h = 162.48 });
      (600, { l = 59.6; c = 0.145; h = 163.225 });
      (700, { l = 50.8; c = 0.118; h = 165.612 });
      (800, { l = 43.2; c = 0.095; h = 166.913 });
      (900, { l = 37.8; c = 0.077; h = 168.94 });
      (950, { l = 26.2; c = 0.051; h = 172.552 });
    ]

  let teal =
    [
      (50, { l = 98.4; c = 0.014; h = 180.72 });
      (100, { l = 95.3; c = 0.051; h = 180.801 });
      (200, { l = 91.0; c = 0.096; h = 180.426 });
      (300, { l = 85.5; c = 0.138; h = 181.071 });
      (400, { l = 77.7; c = 0.152; h = 181.912 });
      (500, { l = 70.4; c = 0.14; h = 182.503 });
      (600, { l = 60.0; c = 0.118; h = 184.704 });
      (700, { l = 51.1; c = 0.096; h = 186.391 });
      (800, { l = 43.7; c = 0.078; h = 188.216 });
      (900, { l = 38.6; c = 0.063; h = 188.416 });
      (950, { l = 27.7; c = 0.046; h = 192.524 });
    ]

  let cyan =
    [
      (50, { l = 98.4; c = 0.019; h = 200.873 });
      (100, { l = 95.6; c = 0.045; h = 203.388 });
      (200, { l = 91.7; c = 0.080; h = 205.041 });
      (300, { l = 86.5; c = 0.127; h = 207.078 });
      (400, { l = 78.9; c = 0.154; h = 211.53 });
      (500, { l = 71.5; c = 0.143; h = 215.221 });
      (600, { l = 60.9; c = 0.126; h = 221.723 });
      (700, { l = 52.0; c = 0.105; h = 223.128 });
      (800, { l = 45.0; c = 0.085; h = 224.283 });
      (900, { l = 39.8; c = 0.070; h = 227.392 });
      (950, { l = 30.2; c = 0.056; h = 229.695 });
    ]

  let sky =
    [
      (50, { l = 97.7; c = 0.013; h = 236.62 });
      (100, { l = 95.1; c = 0.026; h = 236.824 });
      (200, { l = 90.1; c = 0.058; h = 230.902 });
      (300, { l = 82.8; c = 0.111; h = 230.318 });
      (400, { l = 74.6; c = 0.160; h = 232.661 });
      (500, { l = 68.5; c = 0.169; h = 237.323 });
      (600, { l = 58.8; c = 0.158; h = 241.966 });
      (700, { l = 50.0; c = 0.134; h = 242.749 });
      (800, { l = 44.3; c = 0.110; h = 240.79 });
      (900, { l = 39.1; c = 0.090; h = 240.876 });
      (950, { l = 29.3; c = 0.066; h = 243.157 });
    ]

  let indigo =
    [
      (50, { l = 96.2; c = 0.018; h = 272.314 });
      (100, { l = 93.0; c = 0.034; h = 272.788 });
      (200, { l = 87.0; c = 0.065; h = 274.039 });
      (300, { l = 78.5; c = 0.115; h = 274.713 });
      (400, { l = 67.3; c = 0.182; h = 276.935 });
      (500, { l = 58.5; c = 0.233; h = 277.117 });
      (600, { l = 51.1; c = 0.262; h = 276.966 });
      (700, { l = 45.7; c = 0.24; h = 277.023 });
      (800, { l = 39.8; c = 0.195; h = 277.366 });
      (900, { l = 35.9; c = 0.144; h = 278.697 });
      (950, { l = 25.7; c = 0.09; h = 281.288 });
    ]

  let violet =
    [
      (50, { l = 96.9; c = 0.016; h = 293.756 });
      (100, { l = 94.3; c = 0.029; h = 294.588 });
      (200, { l = 89.4; c = 0.057; h = 293.283 });
      (300, { l = 81.1; c = 0.111; h = 293.571 });
      (400, { l = 70.2; c = 0.183; h = 293.541 });
      (500, { l = 60.6; c = 0.25; h = 292.717 });
      (600, { l = 54.1; c = 0.281; h = 293.009 });
      (700, { l = 49.1; c = 0.27; h = 292.581 });
      (800, { l = 43.2; c = 0.232; h = 292.759 });
      (900, { l = 38.0; c = 0.189; h = 293.745 });
      (950, { l = 28.3; c = 0.141; h = 291.089 });
    ]

  let purple =
    [
      (50, { l = 97.7; c = 0.014; h = 308.299 });
      (100, { l = 94.6; c = 0.033; h = 307.174 });
      (200, { l = 90.2; c = 0.063; h = 306.703 });
      (300, { l = 82.7; c = 0.119; h = 306.383 });
      (400, { l = 71.4; c = 0.203; h = 305.504 });
      (500, { l = 62.7; c = 0.265; h = 303.9 });
      (600, { l = 55.8; c = 0.288; h = 302.321 });
      (700, { l = 49.6; c = 0.265; h = 301.924 });
      (800, { l = 43.8; c = 0.218; h = 303.724 });
      (900, { l = 38.1; c = 0.176; h = 304.987 });
      (950, { l = 29.1; c = 0.149; h = 302.717 });
    ]

  let fuchsia =
    [
      (50, { l = 97.7; c = 0.017; h = 320.058 });
      (100, { l = 95.2; c = 0.037; h = 318.852 });
      (200, { l = 90.3; c = 0.076; h = 319.62 });
      (300, { l = 83.3; c = 0.145; h = 321.434 });
      (400, { l = 74.0; c = 0.238; h = 322.16 });
      (500, { l = 66.7; c = 0.295; h = 322.15 });
      (600, { l = 59.1; c = 0.293; h = 322.896 });
      (700, { l = 51.8; c = 0.253; h = 323.949 });
      (800, { l = 45.2; c = 0.211; h = 324.591 });
      (900, { l = 40.1; c = 0.17; h = 325.612 });
      (950, { l = 29.3; c = 0.136; h = 325.661 });
    ]

  let pink =
    [
      (50, { l = 97.1; c = 0.014; h = 343.198 });
      (100, { l = 94.8; c = 0.028; h = 342.258 });
      (200, { l = 89.9; c = 0.061; h = 343.231 });
      (300, { l = 82.3; c = 0.120; h = 346.018 });
      (400, { l = 71.8; c = 0.202; h = 349.761 });
      (500, { l = 65.6; c = 0.241; h = 354.308 });
      (600, { l = 59.2; c = 0.249; h = 0.584 });
      (700, { l = 52.5; c = 0.223; h = 3.958 });
      (800, { l = 45.9; c = 0.187; h = 3.815 });
      (900, { l = 40.8; c = 0.153; h = 2.432 });
      (950, { l = 28.4; c = 0.109; h = 3.907 });
    ]

  let rose =
    [
      (50, { l = 96.9; c = 0.015; h = 12.422 });
      (100, { l = 94.1; c = 0.030; h = 12.580 });
      (200, { l = 89.2; c = 0.058; h = 10.001 });
      (300, { l = 81.0; c = 0.117; h = 11.638 });
      (400, { l = 71.2; c = 0.194; h = 13.428 });
      (500, { l = 64.5; c = 0.246; h = 16.439 });
      (600, { l = 58.6; c = 0.253; h = 17.585 });
      (700, { l = 51.4; c = 0.222; h = 16.935 });
      (800, { l = 45.5; c = 0.188; h = 13.697 });
      (900, { l = 41.0; c = 0.159; h = 10.272 });
      (950, { l = 27.1; c = 0.105; h = 12.094 });
    ]

  let get_color_oklch color_name shade =
    let color_map =
      match String.lowercase_ascii color_name with
      | "gray" -> gray
      | "slate" -> slate
      | "mauve" -> mauve
      | "olive" -> olive
      | "mist" -> mist
      | "taupe" -> taupe
      | "zinc" -> zinc
      | "neutral" -> neutral
      | "stone" -> stone
      | "red" -> red
      | "orange" -> orange
      | "amber" -> amber
      | "yellow" -> yellow
      | "lime" -> lime
      | "green" -> green
      | "emerald" -> emerald
      | "teal" -> teal
      | "cyan" -> cyan
      | "sky" -> sky
      | "blue" -> blue
      | "indigo" -> indigo
      | "violet" -> violet
      | "purple" -> purple
      | "fuchsia" -> fuchsia
      | "pink" -> pink
      | "rose" -> rose
      | _ -> []
    in
    List.assoc_opt shade color_map

  let get_color color_name shade =
    match get_color_oklch color_name shade with
    | Some oklch -> Some (oklch_to_css oklch)
    | None -> None
end

(* Internal helpers for parsing and formatting hex/rgb strings *)
let shorten_hex_str hex_str =
  let hex_no_hash =
    if String.starts_with ~prefix:"#" hex_str then
      String.sub hex_str 1 (String.length hex_str - 1)
    else hex_str
  in
  let len = String.length hex_no_hash in
  if len = 8 then
    let r1 = hex_no_hash.[0] and r2 = hex_no_hash.[1] in
    let g1 = hex_no_hash.[2] and g2 = hex_no_hash.[3] in
    let b1 = hex_no_hash.[4] and b2 = hex_no_hash.[5] in
    let a1 = hex_no_hash.[6] and a2 = hex_no_hash.[7] in
    if r1 = r2 && g1 = g2 && b1 = b2 && a1 = a2 then (
      if a1 = 'f' || a1 = 'F' then (
        (* #RRGGBBFF → #RGB when fully opaque *)
        let short = Bytes.create 3 in
        Bytes.set short 0 r1;
        Bytes.set short 1 g1;
        Bytes.set short 2 b1;
        Bytes.unsafe_to_string short)
      else
        (* #RRGGBBAA → #RGBA *)
        let short = Bytes.create 4 in
        Bytes.set short 0 r1;
        Bytes.set short 1 g1;
        Bytes.set short 2 b1;
        Bytes.set short 3 a1;
        Bytes.unsafe_to_string short)
    else if (a1 = 'f' || a1 = 'F') && (a2 = 'f' || a2 = 'F') then
      (* #RRGGBBFF → #RRGGBB when fully opaque *)
      String.sub hex_no_hash 0 6
    else hex_no_hash
  else if len = 6 then
    let r1 = hex_no_hash.[0] and r2 = hex_no_hash.[1] in
    let g1 = hex_no_hash.[2] and g2 = hex_no_hash.[3] in
    let b1 = hex_no_hash.[4] and b2 = hex_no_hash.[5] in
    if r1 = r2 && g1 = g2 && b1 = b2 then (
      let short = Bytes.create 3 in
      Bytes.set short 0 r1;
      Bytes.set short 1 g1;
      Bytes.set short 2 b1;
      Bytes.unsafe_to_string short)
    else hex_no_hash
  else hex_no_hash

(* An arbitrary colour reaches CSS in the spelling the class wrote it in, which
   is what Tailwind emits: [bg-[#f00]] gives [#f00] and [bg-[#ffffffff]] all
   eight digits, whatever the shortest equivalent would be. Cascade's reader is
   what carries that spelling alongside the decoded bytes; [Css.hex] keeps the
   bytes alone and the printer then spells them in full. *)
let authored_hex hex_str =
  let spelled =
    if String.starts_with ~prefix:"#" hex_str then hex_str else "#" ^ hex_str
  in
  match Css.parse_color spelled with
  | Some (Css.Authored_hex _ as c) -> c
  | Some _ | None -> Css.hex spelled

let is_rgb_call s =
  String.starts_with ~prefix:"rgb(" s && String.ends_with ~suffix:")" s

let parse_rgb_string s =
  try
    let inner = String.sub s 4 (String.length s - 5) in
    let parts = String.split_on_char ',' inner |> List.map String.trim in
    match parts with
    | [ r_str; g_str; b_str ] ->
        Some (int_of_string r_str, int_of_string g_str, int_of_string b_str)
    | _ -> None
  with Invalid_argument _ | Failure _ -> None

let hex_string_of_rgb (r, g, b) =
  let to_hex_char n =
    let c = n mod 16 in
    if c < 10 then Char.chr (c + 48) else Char.chr (c + 87)
  in
  let buf = Bytes.create 6 in
  let set_hex_byte offset n =
    Bytes.set buf offset (to_hex_char (n / 16));
    Bytes.set buf (offset + 1) (to_hex_char n)
  in
  set_hex_byte 0 r;
  set_hex_byte 2 g;
  set_hex_byte 4 b;
  Bytes.unsafe_to_string buf

(* Color constructors *)
let black = Black
let white = White
let gray = Gray
let slate = Slate
let mauve = Mauve
let olive = Olive
let mist = Mist
let taupe = Taupe
let zinc = Zinc
let neutral = Neutral
let stone = Stone
let red = Red
let orange = Orange
let amber = Amber
let yellow = Yellow
let lime = Lime
let green = Green
let emerald = Emerald
let teal = Teal
let cyan = Cyan
let sky = Sky
let blue = Blue
let indigo = Indigo
let violet = Violet
let purple = Purple
let fuchsia = Fuchsia
let pink = Pink
let rose = Rose

let hex s =
  if is_rgb_call s then
    match parse_rgb_string s with
    | Some rgb -> Hex (shorten_hex_str (hex_string_of_rgb rgb))
    | None -> Hex s
  else Hex (shorten_hex_str s)

(* Convert string name to color type *)
let of_string_exn = function
  | "black" -> Black
  | "white" -> White
  | "gray" -> Gray
  | "slate" -> Slate
  | "mauve" -> Mauve
  | "olive" -> Olive
  | "mist" -> Mist
  | "taupe" -> Taupe
  | "zinc" -> Zinc
  | "neutral" -> Neutral
  | "stone" -> Stone
  | "red" -> Red
  | "orange" -> Orange
  | "amber" -> Amber
  | "yellow" -> Yellow
  | "lime" -> Lime
  | "green" -> Green
  | "emerald" -> Emerald
  | "teal" -> Teal
  | "cyan" -> Cyan
  | "sky" -> Sky
  | "blue" -> Blue
  | "indigo" -> Indigo
  | "violet" -> Violet
  | "purple" -> Purple
  | "fuchsia" -> Fuchsia
  | "pink" -> Pink
  | "rose" -> Rose
  | s -> failwith ("Unknown color: " ^ s)

let of_string = function
  | "black" -> Ok Black
  | "white" -> Ok White
  | "gray" -> Ok Gray
  | "slate" -> Ok Slate
  | "mauve" -> Ok Mauve
  | "olive" -> Ok Olive
  | "mist" -> Ok Mist
  | "taupe" -> Ok Taupe
  | "zinc" -> Ok Zinc
  | "neutral" -> Ok Neutral
  | "stone" -> Ok Stone
  | "red" -> Ok Red
  | "orange" -> Ok Orange
  | "amber" -> Ok Amber
  | "yellow" -> Ok Yellow
  | "lime" -> Ok Lime
  | "green" -> Ok Green
  | "emerald" -> Ok Emerald
  | "teal" -> Ok Teal
  | "cyan" -> Ok Cyan
  | "sky" -> Ok Sky
  | "blue" -> Ok Blue
  | "indigo" -> Ok Indigo
  | "violet" -> Ok Violet
  | "purple" -> Ok Purple
  | "fuchsia" -> Ok Fuchsia
  | "pink" -> Ok Pink
  | "rose" -> Ok Rose
  | s ->
      let len = String.length s in
      if len >= 4 && s.[0] = '[' && s.[1] = '#' && s.[len - 1] = ']' then
        (* Arbitrary bracket hex value like [#0088cc]. Store original hex
           (unshortened) so class names preserve it. Shortening happens later in
           to_css for CSS output, through the raising [Css.hex], so only a
           spelling that constructor reads is a colour: a digit count it does
           not know ([#12345]) is as much a miss as [#0088cc]/[0.5], where the ]
           belongs to a different bracket. *)
        let hex = String.sub s 2 (len - 3) in
        if Option.is_some (Css.hex_opt hex) then Ok (Hex hex)
        else Error (`Msg ("Unknown color: " ^ s))
      else if len >= 3 && s.[0] = '[' && s.[len - 1] = ']' then
        let inner = String.sub s 1 (len - 2) in
        let normalized = Parse.decode_underscores inner in
        if Parse.is_css_color_fn normalized then
          match Css.parse_color normalized with
          | Some c -> Ok (Css c)
          | None -> Error (`Msg ("Unknown color: " ^ s))
        else Error (`Msg ("Unknown color: " ^ s))
      else Error (`Msg ("Unknown color: " ^ s))

let rgb r g b =
  if r < 0 || r > 255 then
    invalid_arg ("RGB red value " ^ string_of_int r ^ " out of range [0-255]");
  if g < 0 || g > 255 then
    invalid_arg ("RGB green value " ^ string_of_int g ^ " out of range [0-255]");
  if b < 0 || b > 255 then
    invalid_arg ("RGB blue value " ^ string_of_int b ^ " out of range [0-255]");
  Rgb { red = r; green = g; blue = b }

(* Convert color to OKLCH data for a given shade *)
let to_oklch_opt color shade =
  match color with
  | Black -> Some { l = 0.0; c = 0.0; h = 0.0 }
  | White -> Some { l = 100.0; c = 0.0; h = 0.0 }
  | Oklch oklch -> Some oklch
  | Hex h -> (
      match hex_to_rgb h with
      | Some rgb -> Some (rgb_to_oklch rgb)
      | None -> Some { l = 0.0; c = 0.0; h = 0.0 })
  | Rgb { red; green; blue } ->
      Some (rgb_to_oklch { r = red; g = green; b = blue })
  | Css c -> (
      (* Extract RGB channels from CSS color for oklch conversion *)
      match c with
      | Css.Hex { r; g; b; _ } | Css.Authored_hex { r; g; b; _ } -> (
          match hex_to_rgb (hex_string_of_rgb (r, g, b)) with
          | Some rgb -> Some (rgb_to_oklch rgb)
          | None -> Some { l = 0.0; c = 0.0; h = 0.0 })
      | Css.Rgb (Channels { r; g; b })
      | Css.Rgba { rgb = Channels { r; g; b }; _ } ->
          let chan_to_int : Css.channel -> int = function
            | Int i -> i
            | Num f -> Float.to_int (Float.round f)
            | Pct f -> Float.to_int (Float.round (f *. 2.55))
            | Var _ -> 0
            | None -> 0
          in
          Some
            (rgb_to_oklch
               { r = chan_to_int r; g = chan_to_int g; b = chan_to_int b })
      | _ -> Some { l = 0.0; c = 0.0; h = 0.0 })
  | _ ->
      (* For named colors, get OKLCH data directly from Tailwind *)
      let color_name =
        match color with
        | Gray -> "gray"
        | Slate -> "slate"
        | Zinc -> "zinc"
        | Neutral -> "neutral"
        | Stone -> "stone"
        | Mauve -> "mauve"
        | Olive -> "olive"
        | Mist -> "mist"
        | Taupe -> "taupe"
        | Red -> "red"
        | Orange -> "orange"
        | Amber -> "amber"
        | Yellow -> "yellow"
        | Lime -> "lime"
        | Green -> "green"
        | Emerald -> "emerald"
        | Teal -> "teal"
        | Cyan -> "cyan"
        | Sky -> "sky"
        | Blue -> "blue"
        | Indigo -> "indigo"
        | Violet -> "violet"
        | Purple -> "purple"
        | Fuchsia -> "fuchsia"
        | Pink -> "pink"
        | Rose -> "rose"
        | _ -> ""
      in
      Tailwind.get_color_oklch color_name shade

(* A colour outside the palette has no OKLCH of its own; reading it as black
   keeps the conversion total. Callers that can do better - the ones with a
   theme to consult - go through [to_oklch_opt] instead. *)
let to_oklch color shade =
  match to_oklch_opt color shade with
  | Some oklch -> oklch
  | None -> { l = 0.0; c = 0.0; h = 0.0 }

(* Convert color to OKLCH CSS string for a given shade *)
let to_oklch_css color shade =
  match color with
  | Black -> "#000"
  | White -> "#fff"
  | Oklch oklch -> oklch_to_css oklch
  | Hex h -> hex_to_oklch_css h
  | Rgb { red; green; blue } ->
      rgb_to_oklch { r = red; g = green; b = blue } |> oklch_to_css
  | _ -> (
      (* For named colors, get from Tailwind *)
      let color_name =
        match color with
        | Gray -> "gray"
        | Slate -> "slate"
        | Zinc -> "zinc"
        | Neutral -> "neutral"
        | Stone -> "stone"
        | Mauve -> "mauve"
        | Olive -> "olive"
        | Mist -> "mist"
        | Taupe -> "taupe"
        | Red -> "red"
        | Orange -> "orange"
        | Amber -> "amber"
        | Yellow -> "yellow"
        | Lime -> "lime"
        | Green -> "green"
        | Emerald -> "emerald"
        | Teal -> "teal"
        | Cyan -> "cyan"
        | Sky -> "sky"
        | Blue -> "blue"
        | Indigo -> "indigo"
        | Violet -> "violet"
        | Purple -> "purple"
        | Fuchsia -> "fuchsia"
        | Pink -> "pink"
        | Rose -> "rose"
        | _ -> ""
      in
      match Tailwind.get_color color_name shade with
      | Some value -> value
      | None -> "oklch(0% 0 0)" (* Fallback *))

(* Tailwind writes the powerless hue of an achromatic colour as [none] rather
   than a number. Keeping the component missing matters: a numeric hue lets the
   value fold to a plain hex, and interpolation would take hue 0 instead of the
   other colour's hue. *)
let css_color_of_oklch (o : oklch) : Css.color =
  if o.c = 0.0 then Css.oklch_none_hue o.l o.c else Css.oklch o.l o.c o.h

let oklch_node_of color shade =
  let oklch = to_oklch color shade in
  css_color_of_oklch oklch

(* A project token declared in an [\@theme] block names a colour the palette
   knows nothing about: [--color-brand] has no shades to convert, and its value
   is whatever the block bound it to. *)
let theme_named_color ?theme name =
  match Scheme.theme_value theme ("color-" ^ name) with
  | Some value -> Css.parse_color value
  | None -> None

(* The palette ([Red], [Blue], ...) is a fixed set of (colour, shade) pairs and
   its oklch nodes are immutable, so materialise each node once and share it
   across every utility and variant that uses the colour, instead of
   reconstructing an identical node per use. Built during module initialisation
   and then read-only; only the constant palette constructors are keyed here,
   [Rgb]/[Theme_named] carry open-ended payloads and build fresh. *)
let palette_nodes =
  let table = Hashtbl.create 256 in
  List.iter
    (fun (color, palette) ->
      List.iter
        (fun (shade, o) ->
          Hashtbl.replace table (color, shade) (css_color_of_oklch o))
        palette)
    [
      (Gray, Tailwind.gray);
      (Slate, Tailwind.slate);
      (Zinc, Tailwind.zinc);
      (Neutral, Tailwind.neutral);
      (Stone, Tailwind.stone);
      (Mauve, Tailwind.mauve);
      (Olive, Tailwind.olive);
      (Mist, Tailwind.mist);
      (Taupe, Tailwind.taupe);
      (Red, Tailwind.red);
      (Orange, Tailwind.orange);
      (Amber, Tailwind.amber);
      (Yellow, Tailwind.yellow);
      (Lime, Tailwind.lime);
      (Green, Tailwind.green);
      (Emerald, Tailwind.emerald);
      (Teal, Tailwind.teal);
      (Cyan, Tailwind.cyan);
      (Sky, Tailwind.sky);
      (Blue, Tailwind.blue);
      (Indigo, Tailwind.indigo);
      (Violet, Tailwind.violet);
      (Purple, Tailwind.purple);
      (Fuchsia, Tailwind.fuchsia);
      (Pink, Tailwind.pink);
      (Rose, Tailwind.rose);
    ];
  table

(* Convert color to CSS color value *)
let to_css ?theme color shade =
  match color with
  (* Tailwind writes these two palette entries in the three digits its own theme
     block spells, where every other entry is an [oklch()]. [Css.hex] keeps the
     decoded bytes alone and the printer then spells them in full. *)
  | Black -> authored_hex "#000"
  | White -> authored_hex "#fff"
  (* The class named the spelling, so that is the one CSS gets, [#] and all. *)
  | Hex hex -> authored_hex hex
  | Oklch oklch -> css_color_of_oklch oklch
  | Css c -> c
  | Theme_named name -> (
      match theme_named_color ?theme name with
      | Some c -> c
      (* Only a token the theme declares parses into [Theme_named], so a class
         never lands here; a colour built by hand out of thin air does. *)
      | None -> Css.Transparent)
  | Rgb _ -> oklch_node_of color shade
  | _ -> (
      match Hashtbl.find_opt palette_nodes (color, shade) with
      | Some node -> node
      | None -> oklch_node_of color shade)

let named_color_name = function
  | Black -> "black"
  | White -> "white"
  | Gray -> "gray"
  | Slate -> "slate"
  | Zinc -> "zinc"
  | Neutral -> "neutral"
  | Stone -> "stone"
  | Mauve -> "mauve"
  | Olive -> "olive"
  | Mist -> "mist"
  | Taupe -> "taupe"
  | Red -> "red"
  | Orange -> "orange"
  | Amber -> "amber"
  | Yellow -> "yellow"
  | Lime -> "lime"
  | Green -> "green"
  | Emerald -> "emerald"
  | Teal -> "teal"
  | Cyan -> "cyan"
  | Sky -> "sky"
  | Blue -> "blue"
  | Indigo -> "indigo"
  | Violet -> "violet"
  | Purple -> "purple"
  | Fuchsia -> "fuchsia"
  | Pink -> "pink"
  | Rose -> "rose"
  | _ -> ""

let is_named_color color = named_color_name color <> ""

(* Get the name of a color as a string *)
let to_name color =
  if is_named_color color then named_color_name color
  else
    match color with
    | Hex h ->
        let h_stripped =
          if String.starts_with ~prefix:"#" h then
            String.sub h 1 (String.length h - 1)
          else h
        in
        let pp_hex ctx h =
          Css.Pp.string ctx "[";
          Css.Pp.string ctx h;
          Css.Pp.string ctx "]"
        in
        Css.Pp.to_string ~minify:false pp_hex h_stripped
    | Rgb { red; green; blue } ->
        let pp_rgb ctx (r, g, b) =
          Css.Pp.string ctx "[rgb(";
          Css.Pp.int ctx r;
          Css.Pp.string ctx ",";
          Css.Pp.int ctx g;
          Css.Pp.string ctx ",";
          Css.Pp.int ctx b;
          Css.Pp.string ctx ")]"
        in
        Css.Pp.to_string ~minify:false pp_rgb (red, green, blue)
    | Oklch oklch ->
        let pp_oklch ctx oklch =
          Css.Pp.string ctx "[oklch(";
          Css.Pp.float ctx oklch.l;
          Css.Pp.string ctx "%,";
          Css.Pp.float ctx oklch.c;
          Css.Pp.string ctx ",";
          Css.Pp.float ctx oklch.h;
          Css.Pp.string ctx ")]"
        in
        Css.Pp.to_string ~minify:false pp_oklch oklch
    | Css Css.Transparent -> "transparent"
    | Css Css.Inherit -> "inherit"
    | Css c ->
        (* Serialize CSS color to bracket string for class names *)
        let s = Css.Pp.to_string ~minify:true Css.pp_color c in
        let s = String.map (fun c -> if c = ' ' then '_' else c) s in
        "[" ^ s ^ "]"
    | Theme_named name -> name
    | _ -> ""

(* Pretty printer for colors *)
let pp color =
  if is_named_color color then named_color_name color
  else
    match color with
    | Hex s ->
        (* Use Tailwind's arbitrary value syntax [#hex] for hex colors *)
        let hex_value =
          if String.starts_with ~prefix:"#" s then s else "#" ^ s
        in
        let pp_hex_val ctx v =
          Css.Pp.string ctx "[";
          Css.Pp.string ctx v;
          Css.Pp.string ctx "]"
        in
        Css.Pp.to_string ~minify:false pp_hex_val hex_value
    | Rgb { red; green; blue } ->
        let pp_rgb_val ctx (r, g, b) =
          Css.Pp.string ctx "Rgb(";
          Css.Pp.int ctx r;
          Css.Pp.string ctx ",";
          Css.Pp.int ctx g;
          Css.Pp.string ctx ",";
          Css.Pp.int ctx b;
          Css.Pp.string ctx ")"
        in
        Css.Pp.to_string ~minify:false pp_rgb_val (red, green, blue)
    | Oklch { l; c; h } ->
        let pp_oklch_val ctx (l, c, h) =
          Css.Pp.string ctx "Oklch(";
          Css.Pp.float ctx l;
          Css.Pp.string ctx ",";
          Css.Pp.float ctx c;
          Css.Pp.string ctx ",";
          Css.Pp.float ctx h;
          Css.Pp.string ctx ")"
        in
        Css.Pp.to_string ~minify:false pp_oklch_val (l, c, h)
    | Css Css.Transparent -> "transparent"
    | Css Css.Inherit -> "inherit"
    | Css c ->
        let s = Css.Pp.to_string ~minify:true Css.pp_color c in
        "Css(" ^ s ^ ")"
    | Theme_named name -> name
    | _ -> ""

(* Check if a color is black or white *)
let is_base_color = function Black | White -> true | _ -> false

(* Check if a color is a custom color (hex, rgb, or oklch) *)
let is_custom_color = function
  | Hex _ | Rgb _ | Oklch _ | Css _ -> true
  | _ -> false

(* CSS-wide colour keywords need no palette or theme lookup. Keep them out of
   [of_string]: a few utility families intentionally support only one of
   them. *)
let opacity_keyword = function
  | Css Css.Transparent -> Some Css.Transparent
  | Css Css.Inherit -> Some Css.Inherit
  | _ -> None

(* Check if a color is a theme-named color (no shade suffix) *)
let is_theme_named = function Theme_named _ -> true | _ -> false

(* Check if a color should NOT have a shade suffix in class names *)
let is_shadeless c = is_base_color c || is_custom_color c || is_theme_named c

(* The shades the Tailwind v4 palette defines for named colors *)
let palette_shades = [ 50; 100; 200; 300; 400; 500; 600; 700; 800; 900; 950 ]

let is_valid_shade color shade =
  is_shadeless color || List.mem shade palette_shades

let check_shade ~utility color shade =
  if not (is_valid_shade color shade) then
    invalid_arg
      (utility ^ ": " ^ pp color ^ " has no shade " ^ string_of_int shade
     ^ " (valid shades: 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 950)")

(** {1 Color Application Utilities} *)

(** Background color utilities *)

(* Every colour the palette defines, in the order Tailwind's [@theme] declares
   them: the shaded families, then the four v4.3.3 added after [stone], then the
   two base colours. Both order maps below and [all_palette_declarations] read
   this one list, so a colour added here cannot silently take the unknown-colour
   slot in either. *)
let palette_names =
  [
    "red";
    "orange";
    "amber";
    "yellow";
    "lime";
    "green";
    "emerald";
    "teal";
    "cyan";
    "sky";
    "blue";
    "indigo";
    "violet";
    "purple";
    "fuchsia";
    "pink";
    "rose";
    "slate";
    "gray";
    "zinc";
    "neutral";
    "stone";
    "mauve";
    "olive";
    "mist";
    "taupe";
    "black";
    "white";
  ]

(* Theme layer color variable ordering map. See build.mli for detailed layer
   ordering documentation. Tailwind declares no [--color-transparent] token, but
   the keyword is a colour a utility can name, so it leads the ranking. *)
let theme_color_order_map =
  List.mapi (fun index name -> (name, index)) ("transparent" :: palette_names)

(* Utilities layer color ordering map for conflict resolution. The utilities
   layer ranks the same names the theme layer does, but by its own rule:
   [transparent] and [black] lead and the rest are alphabetical. Reading the
   names off the theme map is what keeps a colour added there from silently
   taking the unknown-colour slot here. *)
let utilities_color_order_map =
  let leading = [ "transparent"; "black" ] in
  let alphabetical =
    List.sort String.compare
      (List.filter_map
         (fun (name, _) -> if List.mem name leading then None else Some name)
         theme_color_order_map)
  in
  List.mapi (fun index name -> (name, index)) (leading @ alphabetical)

(* Get theme layer order for a color variable. Returns (priority=2, suborder)
   where 2 indicates these are theme layer variables. *)
let theme_order color_name =
  match List.assoc_opt color_name theme_color_order_map with
  | Some suborder ->
      (2, suborder * 1000)
      (* Priority 2 for theme layer variables, multiply by 1000 for spacing *)
  | None -> (2, 100000)
(* Unknown colors go last within theme layer *)

(* Get utilities layer order for conflict resolution. Returns (priority,
   suborder) for utilities layer ordering. *)
let utilities_order color_name =
  match List.assoc_opt color_name utilities_color_order_map with
  | Some suborder -> (2, suborder) (* Priority 2 for color utilities *)
  | None -> (2, 100)
(* Unknown colors go last *)

(* Helper function to extract color order with shade for utilities like
   bg-blue-500, text-red-400, etc. *)
let suborder_with_shade color_part =
  (* A shadeless name like [black] or [white] has no trailing [-<digits>], and
     [utilities_order] answers for any name, so there is nothing here that can
     fail. *)
  let shadeless () =
    let _, color_order = utilities_order color_part in
    color_order * 1000
  in
  match String.rindex_opt color_part '-' with
  | None -> shadeless ()
  | Some last_dash -> (
      let color_name = String.sub color_part 0 last_dash in
      let shade_str =
        String.sub color_part (last_dash + 1)
          (String.length color_part - last_dash - 1)
      in
      match int_of_string_opt shade_str with
      | None -> shadeless ()
      | Some shade ->
          let _, color_order = utilities_order color_name in
          (color_order * 1000) + shade)

(* Get theme layer order for a color variable with shade. Formula: (priority=2,
   base_order * 1000 + shade) This ensures color variables are grouped by color
   with shades in ascending order. *)
let theme_order_with_shade color_name shade =
  let var_priority, base_order = theme_order color_name in
  (var_priority, base_order + shade)

(* Memoization table for color variables *)
(* Keyed by [(color, shade)] (shade normalised to 0 for shadeless colours, which
   ignore it) rather than the materialized name, so a cache hit skips building
   the name string entirely - and within one generation bg/text/border share the
   same colour, so hits happen even on a single cold run. *)
let color_var_cache = Domain_cache.v 128

(* Property-scoped colour variables (text-color-*, accent-color-*, ...) keyed by
   their full name, kept separate from the [(color, shade)] cache above. *)
let property_color_var_cache = Domain_cache.v 64

(* Helper to create a color variable with memoization. Creates theme layer
   variables with deterministic ordering based on color and shade. *)
let color_var color shade =
  let shadeless = is_shadeless color in
  let key = (color, if shadeless then 0 else shade) in
  Domain_cache.or_add color_var_cache key (fun () ->
      let base = pp color in
      (* The name goes after the [--] this module does not write itself, so it
         is a CSS name rather than a whole ident: [escape_name] is the exact
         serialisation for that position. A palette colour is already all name
         code points and comes back unchanged; an arbitrary one carries
         brackets, a [#] or parens, and every one of those has to be escaped for
         the result to lex as a single dashed ident. *)
      let escaped_base = Cascade.Parser.escape_name base in
      let name =
        if shadeless then "color-" ^ escaped_base
        else Pp.str [ "color-"; escaped_base; "-"; string_of_int shade ]
      in
      (* Create theme variable with deterministic theme layer order: - Base
         colors use theme_order(color_name) - Shaded colors use
         theme_order_with_shade(color_name, shade)

         Note: Tailwind v4 appears to order variables by first usage in the
         input, not by a fixed ordering. Our implementation uses a fixed
         ordering for determinism and consistency. *)
      let var_order =
        if shadeless then theme_order_with_shade base 0
        else theme_order_with_shade base shade
      in
      Var.theme Css.Color name ~order:var_order)

let color_to_string (c : color) : string =
  match c with
  | Black -> "black"
  | White -> "white"
  | Gray -> "gray"
  | Slate -> "slate"
  | Zinc -> "zinc"
  | Neutral -> "neutral"
  | Stone -> "stone"
  | Mauve -> "mauve"
  | Olive -> "olive"
  | Mist -> "mist"
  | Taupe -> "taupe"
  | Red -> "red"
  | Orange -> "orange"
  | Amber -> "amber"
  | Yellow -> "yellow"
  | Lime -> "lime"
  | Green -> "green"
  | Emerald -> "emerald"
  | Teal -> "teal"
  | Cyan -> "cyan"
  | Sky -> "sky"
  | Blue -> "blue"
  | Indigo -> "indigo"
  | Violet -> "violet"
  | Purple -> "purple"
  | Fuchsia -> "fuchsia"
  | Pink -> "pink"
  | Rose -> "rose"
  | Hex h ->
      (* Hex values stored without # by shorten_hex_str, add it back for class
         name *)
      let h_with_hash =
        if String.starts_with ~prefix:"#" h then h else "#" ^ h
      in
      Css.Pp.to_string ~minify:false
        (fun ctx s ->
          Css.Pp.string ctx "[";
          Css.Pp.string ctx s;
          Css.Pp.string ctx "]")
        h_with_hash
  | Rgb { red; green; blue } ->
      Css.Pp.to_string ~minify:false
        (fun ctx (r, g, b) ->
          Css.Pp.string ctx "[rgb(";
          Css.Pp.int ctx r;
          Css.Pp.string ctx ",";
          Css.Pp.int ctx g;
          Css.Pp.string ctx ",";
          Css.Pp.int ctx b;
          Css.Pp.string ctx ")]")
        (red, green, blue)
  | Oklch oklch ->
      Css.Pp.to_string ~minify:false
        (fun ctx o ->
          Css.Pp.string ctx "[oklch(";
          Css.Pp.float ctx o.l;
          Css.Pp.string ctx "%,";
          Css.Pp.float ctx o.c;
          Css.Pp.string ctx ",";
          Css.Pp.float ctx o.h;
          Css.Pp.string ctx ")]")
        oklch
  | Css Css.Transparent -> "transparent"
  | Css Css.Inherit -> "inherit"
  | Css c ->
      let s = Css.Pp.to_string ~minify:true Css.pp_color c in
      let s = String.map (fun ch -> if ch = ' ' then '_' else ch) s in
      "[" ^ s ^ "]"
  | Theme_named name -> name

(** Color parsing utilities *)

(* A number in an opacity modifier, with the digits the author wrote beside the
   value they denote. The class name is a selector, so it has to repeat the
   spelling: [/[25]] and [/[25.0]] are one alpha and two different classes, and
   re-printing the float picks the wrong one. *)
type opacity_number = { value : float; text : string }

(** Opacity modifier type *)
type opacity_modifier =
  | No_opacity
  | Opacity_percent of opacity_number (* e.g., /50 means 50% *)
  | Opacity_arbitrary of opacity_number (* e.g., /[0.5] means 0.5 *)
  | Opacity_bracket_percent of
      opacity_number (* e.g., /[50%] means 50% but preserves bracket form *)
  | Opacity_named of string (* e.g., /half, /custom - theme-defined names *)
  | Opacity_var of string
(* the modifier as written: /[var(--x)] or the /(--x) shorthand *)

let opacity_of_int pct =
  Opacity_percent { value = Float.of_int pct; text = string_of_int pct }

(* The class-name spelling of a modifier, without the leading [/]. *)
let pp_opacity = function
  | No_opacity -> ""
  | Opacity_percent n -> n.text
  | Opacity_bracket_percent n -> "[" ^ n.text ^ "%]"
  | Opacity_arbitrary n -> "[" ^ n.text ^ "]"
  | Opacity_named name -> name
  | Opacity_var v -> v

let opacity_suffix = function No_opacity -> "" | o -> "/" ^ pp_opacity o

(** Parse the modifier that follows the [/] in a colour class. *)
let opacity_of_string ?theme opacity_str =
  if
    String.length opacity_str > 2
    && opacity_str.[0] = '['
    && opacity_str.[String.length opacity_str - 1] = ']'
  then
    (* Arbitrary value like [0.5] or [50%] or [var(--x)] *)
    let inner = String.sub opacity_str 1 (String.length opacity_str - 2) in
    if String.ends_with ~suffix:"%" inner then
      let num_str = String.sub inner 0 (String.length inner - 1) in
      match float_of_string_opt num_str with
      | Some f -> Some (Opacity_bracket_percent { value = f; text = num_str })
      | None -> None
    else
      match float_of_string_opt inner with
      | Some f -> Some (Opacity_arbitrary { value = f; text = inner })
      | None ->
          if Parse.is_var inner then Some (Opacity_var opacity_str) else None
  else if
    String.length opacity_str > 4
    && opacity_str.[0] = '('
    && opacity_str.[String.length opacity_str - 1] = ')'
  then
    (* The [(--x)] shorthand for [[var(--x)]]. *)
    let inner = String.sub opacity_str 1 (String.length opacity_str - 2) in
    if String.length inner > 2 && String.sub inner 0 2 = "--" then
      Some (Opacity_var opacity_str)
    else None
  else
    (* Numeric value like 50 or 2.5, or named opacity like half/custom *)
    match Parse.decimal_float opacity_str with
    | Some f when f >= 0. ->
        Some (Opacity_percent { value = f; text = opacity_str })
    | _ ->
        (* Not a number — check if it's a named opacity defined in the theme
           (e.g., /half when --opacity-half exists) *)
        if
          Parse.is_valid_theme_name opacity_str
          && Scheme.theme_value theme ("opacity-" ^ opacity_str) <> None
        then Some (Opacity_named opacity_str)
        else None

(** Parse opacity modifier from a string that may contain /NN or /[N.N] *)
let parse_opacity_modifier ?theme s =
  match String.index_opt s '/' with
  | None -> (s, No_opacity)
  | Some idx -> (
      let base = String.sub s 0 idx in
      let opacity_str = String.sub s (idx + 1) (String.length s - idx - 1) in
      match opacity_of_string ?theme opacity_str with
      | Some opacity -> (base, opacity)
      | None -> (s, No_opacity))

(* The named palette colours read a [--color-<name>] token, shaded or not; the
   arbitrary spellings carry their own value and read nothing, and a
   [Theme_named] one is looked up against the theme already. *)
let palette_token color shade =
  match color with
  | Hex _ | Rgb _ | Oklch _ | Css _ | Theme_named _ -> None
  | _ ->
      let base = pp color in
      if is_shadeless color then Some ("color-" ^ base)
      else Some (Pp.str [ "color-"; base; "-"; string_of_int shade ])

(* A [@theme] block that removed the token a palette colour reads leaves the
   utility painting a variable nothing declares, so the colour stops resolving
   the way a project colour the block never declared does. *)
let palette_is_declared theme color shade =
  match (theme, palette_token color shade) with
  | Some scheme, Some token -> not (Scheme.is_removed scheme token)
  | (Some _ | None), _ -> true

(* Parse color and shade from string list. A name the palette does not know is
   still a colour when the [\@theme] block declared [--color-<name>]; such a
   token carries no shade, and its name may span several segments. *)
let shade_of_strings ?theme parts =
  let theme_named () =
    let name = String.concat "-" parts in
    if
      Parse.is_valid_theme_name name
      && Scheme.theme_value theme ("color-" ^ name) <> None
    then Ok (Theme_named name, 500)
    else Error (`Msg ("Invalid color: " ^ name))
  in
  match parts with
  | [ color_str; shade_str ] -> (
      match of_string color_str with
      | Ok color -> (
          match Parse.decimal_int shade_str with
          | Some shade
            when shade >= 0
                 && (not (is_shadeless color))
                 && is_valid_shade color shade
                 && palette_is_declared theme color shade ->
              Ok (color, shade)
          | _ -> theme_named ())
      | Error _ -> theme_named ())
  | [ color_str ] -> (
      match of_string color_str with
      | Ok color when palette_is_declared theme color 500 ->
          Ok (color, 500) (* Default shade *)
      | Ok _ | Error _ -> theme_named ())
  | [] -> Error (`Msg "No color specified")
  | _ -> theme_named ()

(* Parse color, shade, and optional opacity modifier from string list. Handles
   formats like ["red"; "500/50"] or ["red"; "500/[0.5]"] *)
let shade_and_opacity_of_strings ?theme parts =
  (* The modifier rides on the last segment however long the name: a project
     token spelled [brand-primary/50] splits into ["brand"; "primary/50"]. *)
  let theme_named () =
    match List.rev parts with
    | [] -> Error (`Msg "No color specified")
    | last :: front ->
        let base, opacity = parse_opacity_modifier ?theme last in
        let name = String.concat "-" (List.rev (base :: front)) in
        if
          Parse.is_valid_theme_name name
          && Scheme.theme_value theme ("color-" ^ name) <> None
        then Ok (Theme_named name, 500, opacity)
        else Error (`Msg ("Invalid color: " ^ name))
  in
  match parts with
  | [ color_str; shade_opacity_str ] -> (
      let shade_str, opacity =
        parse_opacity_modifier ?theme shade_opacity_str
      in
      match of_string color_str with
      | Ok color -> (
          match Parse.decimal_int shade_str with
          | Some shade
            when shade >= 0
                 && (not (is_shadeless color))
                 && is_valid_shade color shade
                 && palette_is_declared theme color shade ->
              Ok (color, shade, opacity)
          | _ -> theme_named ())
      | Error _ -> theme_named ())
  | [ color_str ] -> (
      (* Could be "current/50" or just "black" *)
      let base_str, opacity = parse_opacity_modifier ?theme color_str in
      let keyword =
        match (base_str, opacity) with
        | ( "transparent",
            ( Opacity_percent _ | Opacity_arbitrary _
            | Opacity_bracket_percent _ | Opacity_named _ | Opacity_var _ ) ) ->
            Some Css.Transparent
        | ( "inherit",
            ( Opacity_percent _ | Opacity_arbitrary _
            | Opacity_bracket_percent _ | Opacity_named _ | Opacity_var _ ) ) ->
            Some Css.Inherit
        | _ -> None
      in
      match keyword with
      | Some keyword -> Ok (Css keyword, 500, opacity)
      | None -> (
          match of_string base_str with
          | Ok color when palette_is_declared theme color 500 ->
              Ok (color, 500, opacity)
          | Ok _ | Error _ -> theme_named ()))
  | [] -> Error (`Msg "No color specified")
  | _ -> theme_named ()

(** {1 Parsing Functions} *)

module Handler = struct
  (* Which border edge a border-{side}-{color} utility paints: a physical edge,
     a logical axis (border-{inline,block}-color) or a logical edge
     (border-{inline,block}-{start,end}-color). *)
  module Side = struct
    type t =
      | Top
      | Right
      | Bottom
      | Left
      | Inline_axis
      | Block_axis
      | Inline_start
      | Inline_end
      | Block_start
      | Block_end
  end

  (* The colour value of a border-{side}-{color}: a named theme colour, an
     arbitrary bracket colour, or a keyword. *)
  module Side_color = struct
    type t =
      | Named of color * int
      | Named_opacity of color * int * opacity_modifier
      | Bracket of string * Css.color
      | Transparent
      | Current
  end

  (** Local color utility type *)
  type t =
    (* Text colors *)
    | Text of color * int
    | Text_opacity of color * int * opacity_modifier
    | Text_transparent
    | Text_current
    | Text_current_opacity of opacity_modifier
    | Text_inherit
    | Text_bracket_color of string * Css.color
      (* text-[#0088cc], text-[black] - string is original bracket content *)
    | Text_bracket_color_opacity of string * Css.color * opacity_modifier
    | Text_bracket_var of string (* text-[var(--value)] *)
    | Text_bracket_var_opacity of string * opacity_modifier
    | Text_bracket_typed_var of string (* text-[color:var(--value)] *)
    | Text_bracket_typed_var_opacity of string * opacity_modifier
    (* Border colors *)
    | Border of color * int
    | Border_opacity of color * int * opacity_modifier
    | Border_transparent
    | Border_current
    | Border_current_opacity of opacity_modifier
    | Border_bracket_color of string * Css.color
    | Border_bracket_color_opacity of string * Css.color * opacity_modifier
    | Border_side_color of Side.t * Side_color.t
    (* Accent colors *)
    | Accent of color * int
    | Accent_opacity of color * int * opacity_modifier
    | Accent_transparent
    | Accent_current
    | Accent_current_opacity of opacity_modifier
    | Accent_inherit
    | Accent_bracket_color of string * Css.color
    | Accent_bracket_color_opacity of string * Css.color * opacity_modifier
    (* Caret colors *)
    | Caret of color * int
    | Caret_opacity of color * int * opacity_modifier
    | Caret_current
    | Caret_current_opacity of opacity_modifier
    | Caret_inherit
    | Caret_transparent
    | Caret_bracket_color of string * Css.color
    | Caret_bracket_color_opacity of string * Css.color * opacity_modifier
    (* Outline colors *)
    | Outline of color * int
    | Outline_opacity of color * int * opacity_modifier
    | Outline_current
    | Outline_current_opacity of opacity_modifier
    | Outline_inherit
    | Outline_transparent
    | Outline_bracket_color of string * Css.color
      (* outline-[#0088cc], outline-[black] *)
    | Outline_bracket_color_opacity of string * Css.color * opacity_modifier
    | Outline_bracket_var of string (* outline-[var(--value)] *)
    | Outline_bracket_var_opacity of string * opacity_modifier
    | Outline_bracket_typed_var of string (* outline-[color:var(--value)] *)
    | Outline_bracket_typed_var_opacity of string * opacity_modifier
    (* Placeholder colors *)
    | Placeholder of color * int
    | Placeholder_opacity of color * int * opacity_modifier
    | Placeholder_transparent
    | Placeholder_current
    | Placeholder_current_opacity of opacity_modifier
    | Placeholder_inherit
    | Placeholder_bracket_color of string * Css.color
    | Placeholder_bracket_color_opacity of string * Css.color * opacity_modifier

  (** Extensible variant for color utilities *)

  (** Resolve the optionally-threaded theme, defaulting to the base scheme. *)
  let resolve_scheme = function Some s -> s | None -> Scheme.default

  (** Get the scheme color name for a color and shade (e.g., "red-500"). Must be
      defined before [open Css] to use the outer [color] type. *)
  let scheme_color_name (c : color) shade =
    let base = pp c in
    match c with
    | Black | White | Theme_named _ -> base
    | _ -> base ^ "-" ^ string_of_int shade

  (** Get the color value for a color and shade, checking scheme first. When
      scheme defines the color as hex, returns hex. Otherwise returns oklch. *)
  let get_color_value ?theme (c : color) shade =
    let color_name = scheme_color_name c shade in
    match Scheme.hex_color (resolve_scheme theme) color_name with
    | Some hex -> Css.hex hex
    | None -> to_css ?theme c (if is_base_color c then 500 else shade)

  (* The theme-layer declaration for a palette colour together with the typed
     reference to it, so a utility outside this module can both emit the token
     and point at it. *)
  let color_binding ?theme c shade =
    Var.binding (color_var c shade) (get_color_value ?theme c shade)

  (* [\@import "tailwindcss" theme(static)] emits the whole theme rather than
     only the tokens a utility used, so the sheet needs every palette colour.
     [transparent] is not one: Tailwind declares no token for it. *)
  let all_palette_declarations ?theme () =
    List.concat_map
      (fun name ->
        let c = of_string_exn name in
        let shades = if is_base_color c then [ 500 ] else palette_shades in
        List.map (fun sh -> fst (color_binding ?theme c sh)) shades)
      palette_names

  (* The theme-layer declaration for a colour token named like "color-red-500",
     or None when the name is not a catalogued colour token. [color_var]
     registers the token's canonical order and [get_color_value] supplies the
     typed value, so the result matches what a colour utility would emit. Used
     to emit tokens that arbitrary values reference via var() but that no colour
     utility set. *)
  (* The palette colour a [--color-*] token names, so a value that references
     the token can be rendered from the palette. *)
  let theme_color_of_name name =
    if String.length name <= 6 || String.sub name 0 6 <> "color-" then None
    else
      let rest = String.sub name 6 (String.length name - 6) in
      match shade_of_strings (String.split_on_char '-' rest) with
      | Ok (c, shade) when not (is_custom_color c) -> Some (c, shade)
      | _ -> None

  let theme_color_decl ?theme name =
    if String.length name <= 6 || String.sub name 0 6 <> "color-" then None
    else
      let rest = String.sub name 6 (String.length name - 6) in
      match shade_of_strings (String.split_on_char '-' rest) with
      | Ok (c, shade) when not (is_custom_color c) ->
          let decl, _ =
            Var.binding (color_var c shade) (get_color_value ?theme c shade)
          in
          Some decl
      | _ -> None

  (** Get a color variable for a property. Checks if a property-scoped theme
      value exists (e.g., [--accent-color-blue-500]) and if so creates a
      property-scoped variable. Otherwise falls back to the generic
      [--color-{name}] variable. *)
  let property_color_var ?theme ~property_prefix (c : color) shade =
    let color_name = scheme_color_name c shade in
    let prop_name = property_prefix ^ "-" ^ color_name in
    match Scheme.theme_value theme prop_name with
    | Some _ ->
        (* Property-scoped theme value exists, create scoped variable *)
        let name = prop_name in
        Domain_cache.or_add property_color_var_cache name (fun () ->
            let base = pp c in
            let var_order =
              if is_shadeless c then theme_order_with_shade base 0
              else theme_order_with_shade base shade
            in
            Var.theme Css.Color name ~order:var_order)
    | None ->
        (* Fall back to generic --color-{name} *)
        color_var c shade

  (** Get the color value for use with color variables. Checks for
      property-scoped theme value first, then scheme, then generic theme value,
      then converts from oklch as fallback. *)
  let property_color_value ?theme ~property_prefix (c : color) shade =
    let color_name = scheme_color_name c shade in
    let prop_name = property_prefix ^ "-" ^ color_name in
    let parse_theme_color value =
      match Css.parse_color value with
      | Some color -> color
      | None -> invalid_arg ("Invalid theme colour: " ^ value)
    in
    match Scheme.theme_value theme prop_name with
    | Some value -> parse_theme_color value
    | None -> (
        match Scheme.hex_color (resolve_scheme theme) color_name with
        | Some hex -> Css.hex hex
        | None -> (
            (* Check theme value overrides for standard color name *)
            let std_name = "color-" ^ color_name in
            match Scheme.theme_value theme std_name with
            | Some value -> parse_theme_color value
            | None -> to_css ?theme c (if is_base_color c then 500 else shade)))

  (* Aliases for names that open Css shadows: the color constructors below, and
     [Pp], whose byte formatter cascade's own [Pp] does not carry. *)
  let color_of_string = of_string
  let hex_byte = Pp.hex_byte

  open Style
  open Css

  let name = "color"

  (* Color families sort at their property's canonical rank, not together:
     border-color (rank ~65) joins border-width/style at priority 19; the
     [color] property (rank ~86) opens the late-typography block at priority 26,
     just before text-transform, and the placeholder, caret and accent colours
     close the same block after the underline offset; outline-color (rank ~92)
     joins the outline width, offset and style utilities at priority 28. The
     rest (background, ...) stay at 25. [_opacity] variants lead each group so
     the type resolves to the color [t] rather than the shadowed [Css.Border] /
     [Css.user_select] [Text] constructors. *)
  let priority = function
    | Border_opacity _ | Border _ | Border_transparent | Border_current
    | Border_current_opacity _ | Border_bracket_color _ | Border_side_color _
    | Border_bracket_color_opacity _ ->
        19
    | Text_opacity _ | Text _ | Text_transparent | Text_current
    | Text_current_opacity _ | Text_inherit | Text_bracket_color _
    | Text_bracket_color_opacity _ | Text_bracket_var _
    | Text_bracket_var_opacity _ | Text_bracket_typed_var _
    | Text_bracket_typed_var_opacity _ | Placeholder_opacity _ | Placeholder _
    | Placeholder_transparent | Placeholder_current
    | Placeholder_current_opacity _ | Placeholder_inherit
    | Placeholder_bracket_color _ | Placeholder_bracket_color_opacity _
    | Caret_opacity _ | Caret _ | Caret_transparent | Caret_current
    | Caret_current_opacity _ | Caret_inherit | Caret_bracket_color _
    | Caret_bracket_color_opacity _ | Accent_opacity _ | Accent _
    | Accent_transparent | Accent_current | Accent_current_opacity _
    | Accent_inherit | Accent_bracket_color _ | Accent_bracket_color_opacity _
      ->
        26
    | Outline_opacity _ | Outline _ | Outline_current
    | Outline_current_opacity _ | Outline_inherit | Outline_transparent
    | Outline_bracket_color _ | Outline_bracket_color_opacity _
    | Outline_bracket_var _ | Outline_bracket_var_opacity _
    | Outline_bracket_typed_var _ | Outline_bracket_typed_var_opacity _ ->
        28

  (* Helper to check if a string contains an opacity modifier *)
  let has_opacity s = String.contains s '/'

  (* Tailwind's [--alpha(<color>/<pct>)] inside an arbitrary value is
     [color-mix(in oklab, <color> <pct>, transparent)]. *)

  (** Parse a bracket inner string into a typed [Css.color], if it represents a
      valid color. Handles hex strings (with [#] prefix), CSS color functions
      like [rgb(...)], [hsl(...)], etc., and named Tailwind colors (which are
      converted to their CSS representation via [to_css]). Returns [None] for
      non-color values. *)
  let parse_alpha_call inner =
    let prefix = "--alpha(" in
    let pl = String.length prefix and n = String.length inner in
    if n > pl && String.sub inner 0 pl = prefix && inner.[n - 1] = ')' then
      let body = String.sub inner pl (n - pl - 1) in
      match String.rindex_opt body '/' with
      | Some i ->
          Some
            ( String.sub body 0 i,
              String.sub body (i + 1) (String.length body - i - 1) )
      | None -> None
    else None

  let rec parse_bracket_color (inner : string) : Css.color option =
    if Parse.is_var inner then
      (* A bare var() is a valid arbitrary color: border-[var(--x)] and its
         paren shorthand border-(--x). *)
      Some (Css.Var (Var.bracket (Parse.extract_var_name inner)))
    else
      match parse_alpha_call inner with
      | Some (color_str, pct_str) -> (
          let pct =
            let t = String.trim pct_str in
            let t =
              if String.length t > 0 && t.[String.length t - 1] = '%' then
                String.sub t 0 (String.length t - 1)
              else t
            in
            float_of_string_opt t
          in
          (* the inner colour is a raw CSS colour ([red] is the keyword, not the
             red-500 palette entry); fall back to the palette only if CSS does
             not know it. *)
          let color =
            let normalized = Parse.decode_underscores color_str in
            match Css.parse_color normalized with
            | Some c -> Some c
            | None -> parse_bracket_color color_str
          in
          match (pct, color) with
          | Some pct, Some c ->
              Some
                (Css.color_mix ~in_space:Oklab ~percent1:pct c Css.Transparent)
          | _ -> None)
      | None -> (
          (* A [#] prefix only names a colour when what follows is a hex
             spelling, so this reads the digits rather than raising on them
             inside [of_class]. Reading them through the parser is what keeps
             the spelling the bracket wrote, which is the one Tailwind emits. *)
          let starts_with_hash = String.length inner > 0 && inner.[0] = '#' in
          if starts_with_hash then
            match Css.hex_opt inner with
            | Some _ -> Some (authored_hex inner)
            | None -> None
          else
            let normalized = Parse.decode_underscores inner in
            (* Any colour CSS knows wins over the palette, keywords and system
               colours included: [[Field]] and [[light-dark(a,b)]] are values,
               not palette names. The guard used to admit only functions. *)
            match Css.parse_color normalized with
            | Some c -> Some c
            | None -> (
                match color_of_string inner with
                | Ok c -> Some (to_css c 500)
                | Error _ -> None))

  (* What a bracket value names, once the [color:]/[var(] spellings are told
     apart from a plain colour. Every colour-bearing utility (text, outline,
     ring, shadow, fill, stroke, ...) reads the same bracket this way; only the
     variant it stores the result in differs. *)
  type bracket_hint =
    | Typed_var of string (* [color:var(--x)], the var() text *)
    | Bare_var of string (* [var(--x)], the full "var(...)" text *)
    | Plain_color of Css.color (* any other colour spelling *)

  (* A data-type hint chooses the longhand and says nothing about the value, so
     a family that writes one colour longhand reads what follows any hint.
     [border-[…]] is the exception: [length:] and [line-width:] name the width,
     which the borders handler owns, and naming them in [not_mine] leaves such a
     bracket to it rather than reading the value here as a colour. *)
  let bracket_color_after_hint ?(not_mine = []) inner : Css.color option =
    match Parse.data_type_hint inner with
    | Some (hint, _) when List.mem hint not_mine -> None
    | _ -> Stdlib.Option.bind (Parse.value_after_hint inner) parse_bracket_color

  let parse_bracket_hint inner =
    if String.starts_with ~prefix:"color:" inner then
      let value = String.sub inner 6 (String.length inner - 6) in
      (* [color:] says how to read what follows it and nothing more. Only a
         var() reference names a custom property; every other spelling is the
         colour itself, so [color:red] is the colour red. *)
      if Parse.is_var value then Some (Typed_var value)
      else
        match parse_bracket_color value with
        | Some c -> Some (Plain_color c)
        | None -> None
    else if String.starts_with ~prefix:"var(" inner then Some (Bare_var inner)
    else
      match parse_bracket_color inner with
      | Some c -> Some (Plain_color c)
      | None -> None

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "text"; "transparent" ] -> Ok Text_transparent
    | [ "text"; "inherit" ] -> Ok Text_inherit
    | [ "text"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = parse_opacity_modifier ~theme current_str in
        match opacity with
        | No_opacity when base = "current" -> Ok Text_current
        | No_opacity -> Error (`Msg ("Invalid text: " ^ current_str))
        | _ -> Ok (Text_current_opacity opacity))
    | [ "text"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (parse_opacity_modifier ~theme v))
      -> (
        let base_str, opacity = parse_opacity_modifier ~theme v in
        let base_inner = Parse.bracket_inner base_str in
        match parse_bracket_hint base_inner with
        | Some (Typed_var var_part) -> (
            match opacity with
            | No_opacity -> Ok (Text_bracket_typed_var var_part)
            | _ -> Ok (Text_bracket_typed_var_opacity (var_part, opacity)))
        | Some (Bare_var v) -> (
            match opacity with
            | No_opacity -> Ok (Text_bracket_var v)
            | _ -> Ok (Text_bracket_var_opacity (v, opacity)))
        | Some (Plain_color css_color) -> (
            match opacity with
            | No_opacity -> Ok (Text_bracket_color (base_inner, css_color))
            | _ ->
                Ok (Text_bracket_color_opacity (base_inner, css_color, opacity))
            )
        | None -> Error (`Msg ("Invalid text bracket value: " ^ base_inner)))
    | "text" :: color_parts when List.exists has_opacity color_parts -> (
        match shade_and_opacity_of_strings ~theme color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Text_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "text" :: color_parts -> (
        match shade_of_strings ~theme color_parts with
        | Ok (color, shade) -> Ok (Text (color, shade))
        | Error e -> Error e)
    | [ "border"; "transparent" ] -> Ok Border_transparent
    | [ "border"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = parse_opacity_modifier ~theme current_str in
        match opacity with
        | No_opacity when base = "current" -> Ok Border_current
        | No_opacity -> Error (`Msg ("Invalid border: " ^ current_str))
        | _ -> Ok (Border_current_opacity opacity))
    | [ "border"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (parse_opacity_modifier ~theme v))
      -> (
        let base_str, opacity = parse_opacity_modifier ~theme v in
        let base_inner = Parse.bracket_inner base_str in
        match
          bracket_color_after_hint ~not_mine:[ "length"; "line-width" ]
            base_inner
        with
        | Some css_color -> (
            match opacity with
            | No_opacity -> Ok (Border_bracket_color (base_inner, css_color))
            | _ ->
                Ok
                  (Border_bracket_color_opacity (base_inner, css_color, opacity))
            )
        | None -> Error (`Msg ("Invalid border bracket value: " ^ base_inner)))
    | "border" :: side :: rest
      when rest <> []
           &&
           match side with
           | "t" | "r" | "b" | "l" | "x" | "y" | "s" | "e" | "bs" | "be" -> true
           | _ -> false -> (
        let bs =
          match side with
          | "t" -> Side.Top
          | "r" -> Side.Right
          | "b" -> Side.Bottom
          | "x" -> Side.Inline_axis
          | "y" -> Side.Block_axis
          | "s" -> Side.Inline_start
          | "e" -> Side.Inline_end
          | "bs" -> Side.Block_start
          | "be" -> Side.Block_end
          | _ -> Side.Left
        in
        match rest with
        | [ "transparent" ] ->
            Ok (Border_side_color (bs, Side_color.Transparent))
        | [ "current" ] -> Ok (Border_side_color (bs, Side_color.Current))
        | [ v ]
          when String.length v > 0 && v.[0] = '[' && Parse.is_bracket_value v
          -> (
            let inner = Parse.bracket_inner v in
            match
              bracket_color_after_hint ~not_mine:[ "length"; "line-width" ]
                inner
            with
            | Some css_color ->
                Ok
                  (Border_side_color (bs, Side_color.Bracket (inner, css_color)))
            | None -> Error (`Msg ("Invalid border side bracket: " ^ v)))
        | color_parts when List.exists has_opacity color_parts -> (
            match shade_and_opacity_of_strings ~theme color_parts with
            | Ok (color, shade, opacity) ->
                Ok
                  (Border_side_color
                     (bs, Side_color.Named_opacity (color, shade, opacity)))
            | Error e -> Error e)
        | color_parts -> (
            match shade_of_strings ~theme color_parts with
            | Ok (color, shade) ->
                Ok (Border_side_color (bs, Side_color.Named (color, shade)))
            | Error e -> Error e))
    | "border" :: color_parts when List.exists has_opacity color_parts -> (
        match shade_and_opacity_of_strings ~theme color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Border_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "border" :: color_parts -> (
        match shade_of_strings ~theme color_parts with
        | Ok (color, shade) -> Ok (Border (color, shade))
        | Error e -> Error e)
    | [ "accent"; "transparent" ] -> Ok Accent_transparent
    | [ "accent"; "inherit" ] -> Ok Accent_inherit
    | [ "accent"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = parse_opacity_modifier ~theme current_str in
        match opacity with
        | No_opacity when base = "current" -> Ok Accent_current
        | No_opacity -> Error (`Msg ("Invalid accent: " ^ current_str))
        | _ -> Ok (Accent_current_opacity opacity))
    | [ "accent"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (parse_opacity_modifier ~theme v))
      -> (
        let base_str, opacity = parse_opacity_modifier ~theme v in
        let base_inner = Parse.bracket_inner base_str in
        match bracket_color_after_hint base_inner with
        | Some css_color -> (
            match opacity with
            | No_opacity -> Ok (Accent_bracket_color (base_inner, css_color))
            | _ ->
                Ok
                  (Accent_bracket_color_opacity (base_inner, css_color, opacity))
            )
        | None -> Error (`Msg ("Invalid accent bracket value: " ^ base_inner)))
    | "accent" :: color_parts when List.exists has_opacity color_parts -> (
        match shade_and_opacity_of_strings ~theme color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Accent_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "accent" :: color_parts -> (
        match shade_of_strings ~theme color_parts with
        | Ok (color, shade) -> Ok (Accent (color, shade))
        | Error e -> Error e)
    | [ "caret"; "inherit" ] -> Ok Caret_inherit
    | [ "caret"; "transparent" ] -> Ok Caret_transparent
    | [ "caret"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = parse_opacity_modifier ~theme current_str in
        match opacity with
        | No_opacity when base = "current" -> Ok Caret_current
        | No_opacity -> Error (`Msg ("Invalid caret: " ^ current_str))
        | _ -> Ok (Caret_current_opacity opacity))
    | [ "caret"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (parse_opacity_modifier ~theme v))
      -> (
        let base_str, opacity = parse_opacity_modifier ~theme v in
        let base_inner = Parse.bracket_inner base_str in
        match bracket_color_after_hint base_inner with
        | Some css_color -> (
            match opacity with
            | No_opacity -> Ok (Caret_bracket_color (base_inner, css_color))
            | _ ->
                Ok
                  (Caret_bracket_color_opacity (base_inner, css_color, opacity))
            )
        | None -> Error (`Msg ("Invalid caret bracket value: " ^ base_inner)))
    | "caret" :: color_parts when List.exists has_opacity color_parts -> (
        match shade_and_opacity_of_strings ~theme color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Caret_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "caret" :: color_parts -> (
        match shade_of_strings ~theme color_parts with
        | Ok (color, shade) -> Ok (Caret (color, shade))
        | Error e -> Error e)
    | [ "outline"; "transparent" ] -> Ok Outline_transparent
    | [ "outline"; "inherit" ] -> Ok Outline_inherit
    | [ "outline"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = parse_opacity_modifier ~theme current_str in
        match opacity with
        | No_opacity when base = "current" -> Ok Outline_current
        | No_opacity -> Error (`Msg ("Invalid outline: " ^ current_str))
        | _ -> Ok (Outline_current_opacity opacity))
    | [ "outline"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (parse_opacity_modifier ~theme v))
      -> (
        let base_str, opacity = parse_opacity_modifier ~theme v in
        let base_inner = Parse.bracket_inner base_str in
        match parse_bracket_hint base_inner with
        | Some (Typed_var var_part) -> (
            match opacity with
            | No_opacity -> Ok (Outline_bracket_typed_var var_part)
            | _ -> Ok (Outline_bracket_typed_var_opacity (var_part, opacity)))
        | Some (Bare_var v) -> (
            match opacity with
            | No_opacity -> Ok (Outline_bracket_var v)
            | _ -> Ok (Outline_bracket_var_opacity (v, opacity)))
        | Some (Plain_color css_color) -> (
            match opacity with
            | No_opacity -> Ok (Outline_bracket_color (base_inner, css_color))
            | _ ->
                Ok
                  (Outline_bracket_color_opacity (base_inner, css_color, opacity))
            )
        | None -> Error (`Msg ("Invalid outline bracket value: " ^ base_inner)))
    | "outline" :: color_parts when List.exists has_opacity color_parts -> (
        match shade_and_opacity_of_strings ~theme color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Outline_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "outline" :: color_parts -> (
        match shade_of_strings ~theme color_parts with
        | Ok (color, shade) -> Ok (Outline (color, shade))
        | Error e -> Error e)
    | [ "placeholder"; "transparent" ] -> Ok Placeholder_transparent
    | [ "placeholder"; "inherit" ] -> Ok Placeholder_inherit
    | [ "placeholder"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = parse_opacity_modifier ~theme current_str in
        match opacity with
        | No_opacity when base = "current" -> Ok Placeholder_current
        | No_opacity -> Error (`Msg ("Invalid placeholder: " ^ current_str))
        | _ -> Ok (Placeholder_current_opacity opacity))
    | [ "placeholder"; v ]
      when String.length v > 0
           && v.[0] = '['
           && Parse.is_bracket_value (fst (parse_opacity_modifier ~theme v))
      -> (
        let base_str, opacity = parse_opacity_modifier ~theme v in
        let base_inner = Parse.bracket_inner base_str in
        match bracket_color_after_hint base_inner with
        | Some css_color -> (
            match opacity with
            | No_opacity ->
                Ok (Placeholder_bracket_color (base_inner, css_color))
            | _ ->
                Ok
                  (Placeholder_bracket_color_opacity
                     (base_inner, css_color, opacity)))
        | None ->
            Error (`Msg ("Invalid placeholder bracket value: " ^ base_inner)))
    | "placeholder" :: color_parts when List.exists has_opacity color_parts -> (
        match shade_and_opacity_of_strings ~theme color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Placeholder_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "placeholder" :: color_parts -> (
        match shade_of_strings ~theme color_parts with
        | Ok (color, shade) -> Ok (Placeholder (color, shade))
        | Error e -> Error e)
    | _ -> Error (`Msg "Not a color utility")

  (** Text color utilities *)

  let text' ?theme color shade =
    if is_custom_color color then
      let css_color = to_css color shade in
      style [ Css.color css_color ]
    else
      let color_name = scheme_color_name color shade in
      let prop_name = "text-color-" ^ color_name in
      let has_property_scoped = Scheme.theme_value theme prop_name <> None in
      let cv, color_value =
        if has_property_scoped then
          ( property_color_var ?theme ~property_prefix:"text-color" color shade,
            property_color_value ?theme ~property_prefix:"text-color" color
              shade )
        else (color_var color shade, get_color_value ?theme color shade)
      in
      let decl, color_ref = Var.binding cv color_value in
      style (decl :: [ Css.color (Var color_ref) ])

  let text_transparent = style [ Css.color (Css.hex "#0000") ]
  let text_current = style [ Css.color Current ]
  let text_inherit = style [ Css.color Inherit ]

  (** Border color utilities *)

  let border_color' ?theme color shade =
    if is_custom_color color then
      let css_color = to_css color shade in
      style [ Css.border_color css_color ]
    else
      let color_var = color_var color shade in
      let color_value = get_color_value ?theme color shade in
      let decl, color_ref = Var.binding color_var color_value in
      style (decl :: [ Css.border_color (Var color_ref) ])

  let border_transparent = style [ Css.border_color (Css.hex "#0000") ]
  let border_current = style [ Css.border_color Current ]

  (* Per-side border colour emission. *)
  let setters_of_side : Side.t -> (Css.color -> Css.declaration) list = function
    | Side.Top -> [ Css.border_top_color ]
    | Side.Right -> [ Css.border_right_color ]
    | Side.Bottom -> [ Css.border_bottom_color ]
    | Side.Left -> [ Css.border_left_color ]
    | Side.Inline_axis ->
        [ (fun c -> Css.border_inline_color (Css.logical_border_color c)) ]
    | Side.Block_axis ->
        [ (fun c -> Css.border_block_color (Css.logical_border_color c)) ]
    | Side.Inline_start -> [ Css.border_inline_start_color ]
    | Side.Inline_end -> [ Css.border_inline_end_color ]
    | Side.Block_start -> [ Css.border_block_start_color ]
    | Side.Block_end -> [ Css.border_block_end_color ]

  (** Accent color utilities *)

  let accent' ?theme color shade =
    if is_custom_color color then
      let css_color = to_css color shade in
      style [ Css.accent_color css_color ]
    else
      let color_var =
        property_color_var ?theme ~property_prefix:"accent-color" color shade
      in
      let color_value =
        property_color_value ?theme ~property_prefix:"accent-color" color shade
      in
      let decl, color_ref = Var.binding color_var color_value in
      style (decl :: [ Css.accent_color (Var color_ref) ])

  let accent_transparent = style [ Css.accent_color (Css.hex "#0000") ]
  let accent_current = style [ Css.accent_color Current ]
  let accent_inherit = style [ Css.accent_color Inherit ]

  (** Caret color utilities *)

  let caret' ?theme color shade =
    if is_custom_color color then
      let css_color = to_css color shade in
      style [ Css.caret_color css_color ]
    else
      let color_var =
        property_color_var ?theme ~property_prefix:"caret-color" color shade
      in
      let color_value =
        property_color_value ?theme ~property_prefix:"caret-color" color shade
      in
      let decl, color_ref = Var.binding color_var color_value in
      style (decl :: [ Css.caret_color (Var color_ref) ])

  let caret_current = style [ Css.caret_color Current ]
  let caret_inherit = style [ Css.caret_color Inherit ]
  let caret_transparent = style [ Css.caret_color (Css.hex "#0000") ]

  (** Outline color utilities *)

  let outline' ?theme color shade =
    if is_custom_color color then
      let css_color = to_css color shade in
      style [ Css.outline_color css_color ]
    else
      let color_var =
        property_color_var ?theme ~property_prefix:"outline-color" color shade
      in
      let color_value =
        property_color_value ?theme ~property_prefix:"outline-color" color shade
      in
      let decl, color_ref = Var.binding color_var color_value in
      style (decl :: [ Css.outline_color (Var color_ref) ])

  let outline_current = style [ Css.outline_color Current ]
  let outline_inherit = style [ Css.outline_color Inherit ]
  let outline_transparent = style [ Css.outline_color (Css.hex "#0000") ]

  (** The byte an rgb() channel stands for, when it stands for one. A var()
      reference carries no value here, and CSS Color 4's [none] takes the
      analogous channel of whatever the colour is combined with rather than
      standing for zero, so neither has a byte. *)
  let channel_to_int : Css.channel -> int option = function
    | Int i -> Some (min 255 (max 0 i))
    | Num f -> Some (min 255 (max 0 (Float.to_int (Float.round f))))
    | Pct f -> Some (min 255 (max 0 (Float.to_int (Float.round (f *. 2.55)))))
    | Var _ | None -> Stdlib.Option.None

  (** How an alpha channel spells inside a hex colour. *)
  type folded_alpha =
    | Opaque  (** no alpha channel at all *)
    | Alpha_byte of int
    | Alpha_unresolvable  (** a var() or calc(), which no hex byte carries *)

  let fold_alpha : Css.alpha -> folded_alpha = function
    | None -> Opaque
    | Num f ->
        Alpha_byte (min 255 (max 0 (Float.to_int (Float.round (f *. 255.)))))
    | Pct f ->
        Alpha_byte (min 255 (max 0 (Float.to_int (Float.round (f *. 2.55)))))
    | Var _ | Calc _ -> Alpha_unresolvable

  (** Convert a typed CSS color to a hex string for Tailwind parity *)
  let css_color_to_hex (c : Css.color) : Css.color option =
    let hex_of_bytes bytes = Some (Css.hex ("#" ^ shorten_hex_str bytes)) in
    match c with
    | Rgb (Channels { r; g; b }) -> (
        match (channel_to_int r, channel_to_int g, channel_to_int b) with
        | Some r, Some g, Some b ->
            hex_of_bytes (hex_byte r ^ hex_byte g ^ hex_byte b)
        | _ -> None)
    | Rgba { rgb = Channels { r; g; b }; a; _ } -> (
        match
          (channel_to_int r, channel_to_int g, channel_to_int b, fold_alpha a)
        with
        | Some r, Some g, Some b, Opaque ->
            hex_of_bytes (hex_byte r ^ hex_byte g ^ hex_byte b)
        | Some r, Some g, Some b, Alpha_byte a ->
            hex_of_bytes (hex_byte r ^ hex_byte g ^ hex_byte b ^ hex_byte a)
        | _ -> None)
    | Hsl _ -> (
        (* Fold through cascade's own colour path: it knows every hue unit and
           reads a bare number for saturation and lightness as the percentage of
           the same value. It leaves a colour whose channels are not all static
           alone, and that one has no hex form. *)
        match
          Cascade.Values.nonkeyword_color (Cascade.Values.normalize_color c)
        with
        | Hex { r; g; b; a } | Authored_hex { r; g; b; a; _ } ->
            let hex = hex_byte r ^ hex_byte g ^ hex_byte b in
            hex_of_bytes (if a = 255 then hex else hex ^ hex_byte a)
        | _ -> None)
    | _ -> None

  (** Resolve a typed [Css.color] to its emission form. A hex keeps the spelling
      the bracket wrote, a colour function is converted to hex where possible.
  *)
  let resolve_bracket_css_color (css_color : Css.color) : Css.color =
    match css_color with
    (* The bracket's own spelling is the one Tailwind writes back. *)
    | Authored_hex _ -> css_color
    | Hex { r; g; b; a } ->
        let value = hex_string_of_rgb (r, g, b) in
        let value = if a = 255 then value else value ^ hex_byte a in
        Css.hex ("#" ^ shorten_hex_str value)
    | _ -> (
        match css_color_to_hex css_color with
        | Some hex_c -> hex_c
        | None -> css_color)

  let outline_bracket_var_style v =
    let bare_name = Parse.extract_var_name v in
    style ~merge_key:"outline-"
      [ Css.outline_color (Css.Var (Var.bracket bare_name)) ]

  let outline_bracket_typed_var_style v =
    let bare_name = Parse.extract_var_name v in
    style ~merge_key:"outline-"
      [ Css.outline_color (Css.Var (Var.bracket bare_name)) ]

  (** Convert opacity modifier to a percentage value (0-100) *)
  let opacity_to_percent = function
    | No_opacity -> 100.0
    | Opacity_percent p -> p.value (* Already a percentage like 50 *)
    | Opacity_bracket_percent p -> p.value (* [50%] is also a percentage *)
    | Opacity_arbitrary f -> f.value *. 100.0 (* e.g., 0.5 -> 50 *)
    | Opacity_named _ | Opacity_var _ ->
        (* Named/var opacity requires variable lookup, default to 100% *)
        100.0

  (* [opacity_to_percent] answers 100 for an opacity it cannot resolve (a var or
     a theme name), so a caller cannot use that to mean "fully opaque". *)
  (* The bare custom-property name inside an opacity modifier, written either
     as [[var(--x)]] or as the [(--x)] shorthand. *)
  let opacity_var_bare v =
    let inner =
      if Parse.is_bracket_value v then Parse.bracket_inner v
      else if String.length v > 2 && v.[0] = '(' then
        String.sub v 1 (String.length v - 2)
      else v
    in
    let name = Parse.extract_var_name inner in
    if String.length name > 2 && String.sub name 0 2 = "--" then
      String.sub name 2 (String.length name - 2)
    else name

  (* The custom property an opacity modifier reads its percentage from, when it
     names one rather than giving a number. *)
  let opacity_var_name = function
    | Opacity_var v -> Some (opacity_var_bare v)
    | Opacity_named name -> Some ("opacity-" ^ Parse.extract_var_name name)
    | _ -> None

  let is_fully_opaque = function
    | Opacity_named _ | Opacity_var _ -> false
    | opacity -> opacity_to_percent opacity >= 100.

  (* The colour with the modifier's alpha applied: a percentage folds in, an
     alpha read from a var is referenced by name. *)
  let mix_alpha ?(in_space : Css.color_space = Oklab) opacity color =
    match opacity_var_name opacity with
    | Some var_name ->
        Css.color_mix_var_percent ~in_space ~var_name color Css.Transparent
    | None ->
        Css.color_mix ~in_space color Css.Transparent
          ~percent1:(opacity_to_percent opacity)

  (* A colour keyword takes the modifier's alpha through the same mix as any
     other colour. At full opacity the mix is a no-op and Tailwind writes the
     keyword itself. *)
  let apply_alpha ?(in_space : Css.color_space = Oklab) opacity color =
    if is_fully_opaque opacity then color else mix_alpha ~in_space opacity color

  (** Condition for progressive enhancement with color-mix in oklab *)
  let color_mix_supports_condition =
    Css.Supports.property "color" "color-mix(in lab, red, red)"

  (* What a browser without [color-mix()] reads. A palette colour converts to a
     plain hex carrying the alpha. A project token has no such conversion - its
     value is whatever the [\@theme] block bound it to, and that may be a colour
     space no hex can spell - so it takes the sRGB mix Tailwind writes instead.
     [value] is the colour already resolved against the theme. *)
  let opacity_fallback ?theme ~percent c shade value =
    let overridden =
      match palette_token c shade with
      | Some token -> Scheme.theme_value theme token <> None
      | None -> false
    in
    match (overridden, to_oklch_opt c shade) with
    | false, Some oklch ->
        Css.hex (hex_with_alpha (rgb_to_hex (oklch_to_rgb oklch)) percent)
    | true, _ | false, None ->
        Css.color_mix ~in_space:Srgb value Css.Transparent ~percent1:percent

  (* Resolve the colour tokens inside an arbitrary [color-mix()] through the
     render's scheme. Tailwind emits that resolved mix in sRGB as the fallback,
     then keeps the authored mix behind a [color-mix()] support query. When a
     token is unavailable at compile time (including a token removed by the
     project), the first operand is the legacy fallback instead. *)
  let theme_var_color theme name =
    match Option.bind (Scheme.token theme name) Css.parse_color with
    | Some _ as color -> color
    | None when Scheme.is_removed theme name -> None
    | None ->
        Option.map
          (fun (c, shade) -> get_color_value ~theme c shade)
          (theme_color_of_name name)

  type mix_resolution = {
    color : Css.color;
    has_dynamic_color : bool;
    unresolved : bool;
  }

  let static color = { color; has_dynamic_color = false; unresolved = false }

  let fallback_space (space : Css.color_space option) =
    match space with
    | Some (Css.Lab | Css.Oklab | Css.Lch | Css.Oklch) -> Some Css.Srgb
    | space -> space

  let rec resolve_color_theme_vars theme seen (color : Css.color) =
    match color with
    | Css.Var v -> (
        if List.mem v.name seen then
          { color; has_dynamic_color = true; unresolved = true }
        else
          match theme_var_color theme v.name with
          | None -> { color; has_dynamic_color = true; unresolved = true }
          | Some value ->
              let resolved =
                resolve_color_theme_vars theme (v.name :: seen) value
              in
              { resolved with has_dynamic_color = true })
    | Css.Current -> { color; has_dynamic_color = true; unresolved = true }
    | Css.Mix { in_space; hue; color1; percent1; color2; percent2 } ->
        let first = resolve_color_theme_vars theme seen color1 in
        let second = resolve_color_theme_vars theme seen color2 in
        let has_dynamic_color =
          first.has_dynamic_color || second.has_dynamic_color
        in
        if not has_dynamic_color then static color
        else if first.unresolved || second.unresolved then
          (* A browser without [color-mix()] can still use the first operand.
             Keep [unresolved] set so an enclosing mix follows the same rule. *)
          { first with has_dynamic_color = true; unresolved = true }
        else
          {
            color =
              Css.Mix
                {
                  in_space = fallback_space in_space;
                  hue;
                  color1 = first.color;
                  percent1;
                  color2 = second.color;
                  percent2;
                };
            has_dynamic_color = true;
            unresolved = false;
          }
    | color -> static color

  let pre_color_mix_fallback theme (color : Css.color) =
    match color with
    | Css.Mix _ ->
        let resolved = resolve_color_theme_vars theme [] color in
        if resolved.has_dynamic_color then Some resolved.color else None
    | _ -> None

  let bracket_colors_style ?merge_key ~theme ~properties css_color =
    let declarations color =
      List.map (fun property -> property color) properties
    in
    let color = resolve_bracket_css_color css_color in
    match pre_color_mix_fallback theme color with
    | None -> style ?merge_key (declarations color)
    | Some fallback ->
        let supports_block =
          Css.supports ~condition:color_mix_supports_condition
            [
              Css.rule ~selector:(Css.Selector.class_ "_") (declarations color);
            ]
        in
        style ?merge_key ~rules:(Some [ supports_block ])
          (declarations fallback)

  let bracket_color_style ?merge_key ~theme ~property css_color =
    bracket_colors_style ?merge_key ~theme ~properties:[ property ] css_color

  (* The same colour set on several properties at once: a per-side border colour
     on the [x]/[y] axes needs two. *)

  (** Generate color with opacity using progressive enhancement. Output depends
      on scheme:
      - With hex scheme: fallback is hex+alpha, [\@supports] has color-mix
      - With oklch scheme (default): fallback is color-mix(srgb), [\@supports]
        has color-mix(oklab) *)
  let colors_with_opacity_style ?theme ~properties ?property_prefix ?merge_key c
      shade opacity =
    let property_decls value = List.map (fun set -> set value) properties in
    let percent = opacity_to_percent opacity in
    match opacity_keyword c with
    | Some keyword ->
        style ?merge_key (property_decls (apply_alpha opacity keyword))
    | None when is_fully_opaque opacity && not (is_custom_color c) ->
        (* At 100% the mix is a no-op, so Tailwind writes the colour itself and
           needs neither the fallback nor the [@supports] pair. *)
        let theme_decl, color_ref =
          Var.binding (color_var c shade) (to_css c shade)
        in
        let value : Css.color = Var color_ref in
        style ?merge_key (theme_decl :: property_decls value)
    | None when is_custom_color c && opacity_var_name opacity <> None ->
        style ?merge_key (property_decls (mix_alpha opacity (to_css c shade)))
    | None when is_custom_color c ->
        (* Custom/arbitrary colors (hex, rgb): output oklab() directly. No theme
           variables, no @supports, no hex+alpha fallback. *)
        style ?merge_key
          (property_decls (custom_color_with_alpha c (percent /. 100.0)))
    | None -> (
        let scheme = resolve_scheme theme in
        let color_name = scheme_color_name c shade in
        (* Check if color is defined as hex in the scheme *)
        match Scheme.hex_color scheme color_name with
        | Some hex_value ->
            (* Scheme has hex color: use hex+alpha fallback with top-level
               @supports *)
            let hex_with_alpha = hex_with_alpha hex_value percent in
            let fallback_decls = property_decls (Css.hex hex_with_alpha) in
            (* Theme declaration for the variable *)
            let color_var = color_var c shade in
            let theme_decl, color_ref =
              Var.binding color_var (Css.hex hex_value)
            in
            (* Progressive enhancement: color-mix(in oklab, var(--color-X) NN%,
               transparent) *)
            let oklab_color =
              Css.color_mix ~in_space:Oklab (Css.Var color_ref) Css.Transparent
                ~percent1:percent
            in
            (* Create @supports block with oklab version as top-level rule. Use
               placeholder selector that rule.ml replaces with actual class. *)
            let supports_block =
              Css.supports ~condition:color_mix_supports_condition
                [
                  Css.rule ~selector:(Css.Selector.class_ "_")
                    (property_decls oklab_color);
                ]
            in
            style ?merge_key ~rules:(Some [ supports_block ])
              (theme_decl :: fallback_decls)
        | None ->
            (* Non-scheme color: use property-scoped variable if prefix given *)
            let color_var =
              match property_prefix with
              | Some prefix ->
                  property_color_var ?theme ~property_prefix:prefix c shade
              | Stdlib.Option.None -> color_var c shade
            in
            let color_value =
              match property_prefix with
              | Some prefix ->
                  property_color_value ?theme ~property_prefix:prefix c shade
              | Stdlib.Option.None ->
                  to_css ?theme c (if is_base_color c then 500 else shade)
            in
            let theme_decl, color_ref = Var.binding color_var color_value in
            (* An opacity read from a var has no percentage to fold into a hex
               fallback, so the fallback is the colour at full opacity. *)
            let fallback_decls =
              match opacity_var_name opacity with
              | Some _ -> property_decls color_value
              | None ->
                  property_decls
                    (opacity_fallback ?theme ~percent c shade color_value)
            in
            let oklab_color =
              match opacity_var_name opacity with
              | Some var_name ->
                  Css.color_mix_var_percent ~in_space:Oklab ~var_name
                    (Css.Var color_ref) Css.Transparent
              | None ->
                  Css.color_mix ~in_space:Oklab (Css.Var color_ref)
                    Css.Transparent ~percent1:percent
            in
            let supports_block =
              Css.supports ~condition:color_mix_supports_condition
                [
                  Css.rule ~selector:(Css.Selector.class_ "_")
                    (property_decls oklab_color);
                ]
            in
            style ?merge_key ~rules:(Some [ supports_block ])
              (theme_decl :: fallback_decls))

  let color_with_opacity_style ?theme ~property ?property_prefix ?merge_key c
      shade opacity =
    colors_with_opacity_style ?theme ~properties:[ property ] ?property_prefix
      ?merge_key c shade opacity

  (** Text color with opacity *)
  let text_with_opacity ?theme c shade opacity =
    let property_prefix =
      if not (is_custom_color c) then
        let color_name = scheme_color_name c shade in
        let prop_name = "text-color-" ^ color_name in
        if Scheme.theme_value theme prop_name <> None then Some "text-color"
        else None
      else None
    in
    color_with_opacity_style ?theme ~property:Css.color ?property_prefix c shade
      opacity

  (** Border color with opacity *)
  let border_with_opacity ?theme c shade opacity =
    color_with_opacity_style ?theme ~property:Css.border_color c shade opacity

  let border_side_color_style theme side value =
    let sides = setters_of_side side in
    let apply c = style (List.map (fun set -> set c) sides) in
    match value with
    | Side_color.Named (color, shade) ->
        if is_custom_color color then apply (to_css color shade)
        else
          let color_var = color_var color shade in
          let color_value = get_color_value color shade in
          let decl, color_ref = Var.binding color_var color_value in
          style
            (decl :: List.map (fun set -> set (Var color_ref : Css.color)) sides)
    | Side_color.Named_opacity (color, shade, opacity) ->
        colors_with_opacity_style ~properties:sides color shade opacity
    | Side_color.Bracket (_, css_color) ->
        bracket_colors_style ~theme ~properties:sides css_color
    | Side_color.Transparent -> apply (Css.hex "#0000")
    | Side_color.Current -> apply Css.Current

  (** Accent color with opacity *)
  let accent_with_opacity ?theme c shade opacity =
    color_with_opacity_style ?theme ~property:Css.accent_color
      ~property_prefix:"accent-color" c shade opacity

  (** Caret color with opacity *)
  let caret_with_opacity ?theme c shade opacity =
    color_with_opacity_style ?theme ~property:Css.caret_color
      ~property_prefix:"caret-color" c shade opacity

  (** Outline color with opacity *)
  let outline_with_opacity ?theme c shade opacity =
    color_with_opacity_style ?theme ~property:Css.outline_color
      ~property_prefix:"outline-color" c shade opacity

  (** Current color with opacity using color-mix with progressive enhancement *)
  let current_color_with_opacity ~property opacity =
    let property : Css.color -> Css.declaration = property in
    let percent = opacity_to_percent opacity in
    (* Fallback: just currentColor (browsers that don't support color-mix) *)
    let fallback_decl = property Css.Current in
    (* Progressive enhancement: color-mix(in oklab, currentcolor NN%,
       transparent) *)
    let oklab_color =
      Css.color_mix ~in_space:Oklab Css.Current Css.Transparent
        ~percent1:percent
    in
    let oklab_decl = property oklab_color in
    (* Create @supports block with oklab version as top-level rule. Use
       placeholder selector that rule.ml replaces with actual class. *)
    let supports_block =
      Css.supports ~condition:color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
    in
    style ~rules:(Some [ supports_block ]) [ fallback_decl ]

  (* What an opacity modifier makes of a bracket colour. [Folded] is the single
     value a modifier a browser can read on its own resolves to. [Guarded] is
     the pair the rest need: the colour itself, for a browser with no
     [color-mix()], and the mix behind an [\@supports] guard. The properties the
     two land on are the caller's, which is why this answers values rather than
     declarations - a decoration colour writes a vendor prefix alongside, and a
     divide colour hangs both on its child selector. *)
  type bracket_opacity =
    | Folded of Css.color
    | Guarded of { fallback : Css.color; mixed : Css.color }

  (* A bracket colour arrives already parsed into a typed [Css.color], and an
     opacity modifier applies to that value. Reading the bracket text back
     through the palette parser answered black for every colour the palette does
     not name.

     The modifier stays a [color-mix()] wherever Tailwind writes one: the
     bracket's contents are emitted as authored, so evaluating the mix would
     replace the colour the class named with a computed [oklab()] no class
     spells. Only a fully opaque modifier resolves, to the colour itself. *)
  let bracket_color_opacity ?(theme = Scheme.default) css_color opacity =
    let base = resolve_bracket_css_color css_color in
    if is_fully_opaque opacity then Folded base
    else
      let mixed = mix_alpha opacity base in
      match pre_color_mix_fallback theme mixed with
      | Some fallback -> Guarded { fallback; mixed }
      | None when opacity_var_name opacity <> None ->
          (* The mix reads its percentage from a custom property, which
             [pre_color_mix_fallback] does not look at: it answers on the colour
             operands alone. Without the pair a browser with no [color-mix()]
             drops the declaration and paints nothing, where Tailwind paints the
             bracket colour. *)
          Guarded { fallback = base; mixed }
      | None -> Folded mixed

  let bracket_color_opacity_style ?(theme = Scheme.default) ?merge_key ~property
      css_color opacity =
    match bracket_color_opacity ~theme css_color opacity with
    | Folded value -> style ?merge_key [ property value ]
    | Guarded { fallback; mixed } ->
        let supports_block =
          Css.supports ~condition:color_mix_supports_condition
            [ Css.rule ~selector:(Css.Selector.class_ "_") [ property mixed ] ]
        in
        style ?merge_key ~rules:(Some [ supports_block ]) [ property fallback ]

  let outline_bracket_color_opacity_style ~theme inner css_color opacity =
    let merge_key =
      if String.length inner > 0 && inner.[0] = '#' then
        (* Hex bracket colors: strip bracket+opacity for merging *)
        "outline-"
      else
        (* Named bracket colors: unique per variant to prevent merging.
           Different opacity syntaxes (e.g. /50 vs /[0.5]) produce identical CSS
           but Tailwind keeps them separate. *)
        "outline-[" ^ inner ^ "]" ^ opacity_suffix opacity
    in
    bracket_color_opacity_style ~theme ~property:Css.outline_color ~merge_key
      css_color opacity

  let outline_bracket_var_opacity_style v opacity =
    let bare_name = Parse.extract_var_name v in
    let percent = opacity_to_percent opacity in
    let var_color : Css.color = Css.Var (Var.bracket bare_name) in
    let fallback_decl = Css.outline_color var_color in
    let oklab_color =
      Css.color_mix ~in_space:Oklab var_color Css.Transparent ~percent1:percent
    in
    let oklab_decl = Css.outline_color oklab_color in
    let supports_block =
      Css.supports ~condition:color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
    in
    style ~merge_key:"outline-" ~rules:(Some [ supports_block ])
      [ fallback_decl ]

  let outline_bracket_var_opacity v opacity =
    let bare_name = Parse.extract_var_name v in
    let percent = opacity_to_percent opacity in
    let var_color : Css.color = Css.Var (Var.bracket bare_name) in
    let fallback_decl = Css.outline_color var_color in
    let oklab_color =
      Css.color_mix ~in_space:Oklab var_color Css.Transparent ~percent1:percent
    in
    let oklab_decl = Css.outline_color oklab_color in
    let supports_block =
      Css.supports ~condition:color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
    in
    style ~merge_key:"outline-" ~rules:(Some [ supports_block ])
      [ fallback_decl ]

  let with_pseudo pseudo = function
    | Style.Style s -> Style.Style { s with pseudo_suffix = Some pseudo }
    | other -> other

  let to_style theme =
    (* Shadow the scheme-reading colour helpers with theme-applied versions so
       the match arms below read from the explicitly threaded scheme. *)
    let text' color shade = text' ~theme color shade in
    let border_color' color shade = border_color' ~theme color shade in
    let accent' color shade = accent' ~theme color shade in
    let caret' color shade = caret' ~theme color shade in
    let outline' color shade = outline' ~theme color shade in
    let text_with_opacity color shade opacity =
      text_with_opacity ~theme color shade opacity
    in
    let border_with_opacity color shade opacity =
      border_with_opacity ~theme color shade opacity
    in
    let accent_with_opacity color shade opacity =
      accent_with_opacity ~theme color shade opacity
    in
    let caret_with_opacity color shade opacity =
      caret_with_opacity ~theme color shade opacity
    in
    let outline_with_opacity color shade opacity =
      outline_with_opacity ~theme color shade opacity
    in
    function
    (* [Text_opacity] leads so the match resolves to the colour [t] rather than
       to the [Text] constructor [open Css] brings into scope. *)
    | Text_opacity (color, shade, opacity) ->
        text_with_opacity color shade opacity
    | Text (color, shade) -> text' color shade
    | Text_transparent -> text_transparent
    | Text_current -> text_current
    | Text_current_opacity opacity ->
        current_color_with_opacity ~property:Css.color opacity
    | Text_inherit -> text_inherit
    | Text_bracket_color (_orig, css_color) ->
        bracket_color_style ~theme ~merge_key:"text-" ~property:Css.color
          css_color
    | Text_bracket_color_opacity (_orig, css_color, opacity) ->
        bracket_color_opacity_style ~theme ~property:Css.color css_color opacity
    | Text_bracket_var v ->
        let bare_name = Parse.extract_var_name v in
        style ~merge_key:"text-" [ Css.color (Css.Var (Var.bracket bare_name)) ]
    | Text_bracket_var_opacity (v, opacity) ->
        let bare_name = Parse.extract_var_name v in
        let percent = opacity_to_percent opacity in
        let var_color : Css.color = Css.Var (Var.bracket bare_name) in
        let fallback_decl = Css.color var_color in
        let oklab_color =
          Css.color_mix ~in_space:Oklab var_color Css.Transparent
            ~percent1:percent
        in
        let oklab_decl = Css.color oklab_color in
        let supports_block =
          Css.supports ~condition:color_mix_supports_condition
            [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
        in
        style ~merge_key:"text-" ~rules:(Some [ supports_block ])
          [ fallback_decl ]
    | Text_bracket_typed_var v ->
        let bare_name = Parse.extract_var_name v in
        style ~merge_key:"text-" [ Css.color (Css.Var (Var.bracket bare_name)) ]
    | Text_bracket_typed_var_opacity (v, opacity) ->
        let bare_name = Parse.extract_var_name v in
        let percent = opacity_to_percent opacity in
        let var_color : Css.color = Css.Var (Var.bracket bare_name) in
        let fallback_decl = Css.color var_color in
        let oklab_color =
          Css.color_mix ~in_space:Oklab var_color Css.Transparent
            ~percent1:percent
        in
        let oklab_decl = Css.color oklab_color in
        let supports_block =
          Css.supports ~condition:color_mix_supports_condition
            [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
        in
        style ~merge_key:"text-" ~rules:(Some [ supports_block ])
          [ fallback_decl ]
    | Border_side_color (side, value) ->
        border_side_color_style theme side value
    | Border (color, shade) -> border_color' color shade
    | Border_opacity (color, shade, opacity) ->
        border_with_opacity color shade opacity
    | Border_transparent -> border_transparent
    | Border_current -> border_current
    | Border_current_opacity opacity ->
        current_color_with_opacity ~property:Css.border_color opacity
    | Border_bracket_color (_orig, css_color) ->
        bracket_color_style ~theme ~merge_key:"border-"
          ~property:Css.border_color css_color
    | Border_bracket_color_opacity (_orig, css_color, opacity) ->
        bracket_color_opacity_style ~theme ~property:Css.border_color css_color
          opacity
    | Accent (color, shade) -> accent' color shade
    | Accent_opacity (color, shade, opacity) ->
        accent_with_opacity color shade opacity
    | Accent_transparent -> accent_transparent
    | Accent_current -> accent_current
    | Accent_current_opacity opacity ->
        current_color_with_opacity ~property:Css.accent_color opacity
    | Accent_inherit -> accent_inherit
    | Accent_bracket_color (_orig, css_color) ->
        bracket_color_style ~theme ~merge_key:"accent-"
          ~property:Css.accent_color css_color
    | Accent_bracket_color_opacity (_orig, css_color, opacity) ->
        bracket_color_opacity_style ~theme ~property:Css.accent_color css_color
          opacity
    | Caret (color, shade) -> caret' color shade
    | Caret_opacity (color, shade, opacity) ->
        caret_with_opacity color shade opacity
    | Caret_current -> caret_current
    | Caret_current_opacity opacity ->
        current_color_with_opacity ~property:Css.caret_color opacity
    | Caret_inherit -> caret_inherit
    | Caret_transparent -> caret_transparent
    | Caret_bracket_color (_orig, css_color) ->
        bracket_color_style ~theme ~merge_key:"caret-" ~property:Css.caret_color
          css_color
    | Caret_bracket_color_opacity (_orig, css_color, opacity) ->
        bracket_color_opacity_style ~theme ~property:Css.caret_color css_color
          opacity
    | Outline (color, shade) -> outline' color shade
    | Outline_opacity (color, shade, opacity) ->
        outline_with_opacity color shade opacity
    | Outline_current -> outline_current
    | Outline_current_opacity opacity ->
        current_color_with_opacity ~property:Css.outline_color opacity
    | Outline_inherit -> outline_inherit
    | Outline_transparent -> outline_transparent
    | Outline_bracket_color (_orig, css_color) ->
        bracket_color_style ~theme ~merge_key:"outline-"
          ~property:Css.outline_color css_color
    | Outline_bracket_color_opacity (inner, css_color, opacity) ->
        outline_bracket_color_opacity_style ~theme inner css_color opacity
    | Outline_bracket_var v -> outline_bracket_var_style v
    | Outline_bracket_var_opacity (v, opacity) ->
        outline_bracket_var_opacity_style v opacity
    | Outline_bracket_typed_var v -> outline_bracket_typed_var_style v
    | Outline_bracket_typed_var_opacity (v, opacity) ->
        outline_bracket_var_opacity v opacity
    | Placeholder (color, shade) ->
        with_pseudo Css.Selector.Placeholder (text' color shade)
    | Placeholder_opacity (color, shade, opacity) ->
        if is_fully_opaque opacity && not (is_custom_color color) then
          with_pseudo Css.Selector.Placeholder (text' color shade)
        else
          with_pseudo Css.Selector.Placeholder
            (text_with_opacity color shade opacity)
    | Placeholder_transparent ->
        with_pseudo Css.Selector.Placeholder text_transparent
    | Placeholder_current -> with_pseudo Css.Selector.Placeholder text_current
    | Placeholder_current_opacity opacity ->
        with_pseudo Css.Selector.Placeholder
          (current_color_with_opacity ~property:Css.color opacity)
    | Placeholder_inherit -> with_pseudo Css.Selector.Placeholder text_inherit
    | Placeholder_bracket_color (_orig, css_color) ->
        with_pseudo Css.Selector.Placeholder
          (bracket_color_style ~theme ~property:Css.color css_color)
    | Placeholder_bracket_color_opacity (_orig, css_color, opacity) ->
        with_pseudo Css.Selector.Placeholder
          (bracket_color_opacity_style ~theme ~property:Css.color css_color
             opacity)

  (* Suborder for the non-text color families: border first (0-9999), then the
     rest. text-color runs at priority 26 (see [priority]) with a fixed suborder
     inside the late-typography block. *)
  let suborder = function
    (* [Text_opacity] leads so the match resolves to the colour [t] rather than
       to the [Text] constructor [open Css] brings into scope. All text colors
       share suborder 8350 (priority 26, after white-space and before
       text-transform) so they sort alphabetically, matching Tailwind. *)
    | Text_opacity (color, shade, _) ->
        let _ = (color, shade) in
        8350
    | Text (color, shade) ->
        let _ = (color, shade) in
        8350
    | Text_transparent -> 8350
    | Text_current -> 8350
    | Text_current_opacity _ -> 8350
    | Text_inherit -> 8350
    | Text_bracket_color _ -> 8350
    | Text_bracket_color_opacity _ -> 8350
    | Text_bracket_var _ -> 8350
    | Text_bracket_var_opacity _ -> 8350
    | Text_bracket_typed_var _ -> 8350
    | Text_bracket_typed_var_opacity _ -> 8350
    | Border (color, shade) ->
        (* Border colors share suborder 1500 with borders.ml's named border
           colors (Border_color), at priority 19, so named and arbitrary tie and
           sort together by class name - matching Tailwind. *)
        let _ = (color, shade) in
        1500
    | Border_opacity (color, shade, _) ->
        let _ = (color, shade) in
        1500
    | Border_transparent -> 1500
    | Border_current -> 1500
    | Border_current_opacity _ -> 1500
    | Border_bracket_color _ -> 1500
    | Border_bracket_color_opacity _ -> 1500
    (* Per-side border colours sort after the all-sides ones, and side-major:
       every colour a side names writes a colour another side writes too, so
       their order decides which one wins. Tailwind runs the axes and the
       logical sides first, then the physical ones. Within a side the colours
       tie and sort by class name, so one slot per side is enough; they stay ten
       apart to leave room. *)
    | Border_side_color (side, _) -> (
        match side with
        | Side.Inline_axis -> 1600
        | Side.Block_axis -> 1610
        | Side.Inline_start -> 1620
        | Side.Inline_end -> 1630
        | Side.Block_start -> 1640
        | Side.Block_end -> 1650
        | Side.Top -> 1660
        | Side.Right -> 1670
        | Side.Bottom -> 1680
        | Side.Left -> 1690)
    (* The three colour families that close the late-typography block, in
       Tailwind's order: placeholder, then caret, then accent, all after the
       underline offset (max 69999). Each family shares one suborder so its
       members tie and sort alphabetically by class name. *)
    | Placeholder _ -> 80000
    | Placeholder_opacity _ -> 80000
    | Placeholder_transparent -> 80000
    | Placeholder_current -> 80000
    | Placeholder_current_opacity _ -> 80000
    | Placeholder_inherit -> 80000
    | Placeholder_bracket_color _ -> 80000
    | Placeholder_bracket_color_opacity _ -> 80000
    | Caret (color, shade) ->
        let _ = (color, shade) in
        81000
    | Caret_opacity (color, shade, _) ->
        let _ = (color, shade) in
        81000
    | Caret_current -> 81000
    | Caret_current_opacity _ -> 81000
    | Caret_inherit -> 81000
    | Caret_transparent -> 81000
    | Caret_bracket_color _ -> 81000
    | Caret_bracket_color_opacity _ -> 81000
    | Accent (color, shade) ->
        let _ = (color, shade) in
        82000
    | Accent_opacity (color, shade, _) ->
        let _ = (color, shade) in
        82000
    | Accent_transparent -> 82000
    | Accent_current -> 82000
    | Accent_current_opacity _ -> 82000
    | Accent_inherit -> 82000
    | Accent_bracket_color _ -> 82000
    | Accent_bracket_color_opacity _ -> 82000
    (* Outline colors run at priority 28 with the rest of the outline family
       (see [priority]): the 3000 base puts them after borders.ml's outline
       width (1999-2010) and offset (2200-2299) and before its outline styles
       (30000-30004). *)
    | Outline (color, shade) ->
        let base =
          if is_base_color color then
            suborder_with_shade (color_to_string color)
          else
            suborder_with_shade
              (color_to_string color ^ "-" ^ string_of_int shade)
        in
        3000 + base
    | Outline_opacity (color, shade, _) ->
        let base =
          if is_base_color color then
            suborder_with_shade (color_to_string color)
          else
            suborder_with_shade
              (color_to_string color ^ "-" ^ string_of_int shade)
        in
        3000 + base
    | Outline_current ->
        3000 + (4 * 1000) (* c -> between cyan(4) and emerald(5) *)
    | Outline_current_opacity _ -> 3000 + (4 * 1000)
    | Outline_inherit -> 3000 + (9 * 1000) + 999
    (* i -> after every indigo shade and before lime(10) *)
    | Outline_transparent -> 3000 + (22 * 1000)
    (* t -> between teal and violet *)
    | Outline_bracket_color _ -> 3000
    | Outline_bracket_color_opacity _ -> 3000
    | Outline_bracket_var _ -> 3000
    | Outline_bracket_var_opacity _ -> 3000
    | Outline_bracket_typed_var _ -> 3000
    | Outline_bracket_typed_var_opacity _ -> 3000

  let to_class = function
    (* [Text_opacity] leads so the match resolves to the colour [t] rather than
       to the [Text] constructor [open Css] brings into scope. *)
    | Text_opacity (c, shade, opacity) ->
        if is_shadeless c then
          "text-" ^ color_to_string c ^ opacity_suffix opacity
        else
          "text-" ^ color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Text (c, shade) ->
        if is_shadeless c then "text-" ^ color_to_string c
        else "text-" ^ color_to_string c ^ "-" ^ string_of_int shade
    | Text_transparent -> "text-transparent"
    | Text_current -> "text-current"
    | Text_current_opacity opacity -> "text-current" ^ opacity_suffix opacity
    | Text_inherit -> "text-inherit"
    | Text_bracket_color (v, _) -> "text-[" ^ v ^ "]"
    | Text_bracket_color_opacity (v, _, opacity) ->
        "text-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Text_bracket_var v -> "text-[" ^ v ^ "]"
    | Text_bracket_var_opacity (v, opacity) ->
        "text-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Text_bracket_typed_var v -> "text-[color:" ^ v ^ "]"
    | Text_bracket_typed_var_opacity (v, opacity) ->
        "text-[color:" ^ v ^ "]" ^ opacity_suffix opacity
    | Border (c, shade) ->
        if is_shadeless c then "border-" ^ color_to_string c
        else "border-" ^ color_to_string c ^ "-" ^ string_of_int shade
    | Border_opacity (c, shade, opacity) ->
        if is_shadeless c then
          "border-" ^ color_to_string c ^ opacity_suffix opacity
        else
          "border-" ^ color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Border_transparent -> "border-transparent"
    | Border_current -> "border-current"
    | Border_current_opacity opacity ->
        "border-current" ^ opacity_suffix opacity
    | Border_bracket_color (v, _) -> "border-[" ^ v ^ "]"
    | Border_side_color (side, value) ->
        let s =
          match side with
          | Side.Top -> "t"
          | Side.Right -> "r"
          | Side.Bottom -> "b"
          | Side.Left -> "l"
          | Side.Inline_axis -> "x"
          | Side.Block_axis -> "y"
          | Side.Inline_start -> "s"
          | Side.Inline_end -> "e"
          | Side.Block_start -> "bs"
          | Side.Block_end -> "be"
        in
        let v =
          match value with
          | Side_color.Named (c, shade) ->
              if is_shadeless c then color_to_string c
              else color_to_string c ^ "-" ^ string_of_int shade
          | Side_color.Named_opacity (c, shade, opacity) ->
              (if is_shadeless c then color_to_string c
               else color_to_string c ^ "-" ^ string_of_int shade)
              ^ opacity_suffix opacity
          | Side_color.Bracket (orig, _) -> "[" ^ orig ^ "]"
          | Side_color.Transparent -> "transparent"
          | Side_color.Current -> "current"
        in
        "border-" ^ s ^ "-" ^ v
    | Border_bracket_color_opacity (v, _, opacity) ->
        "border-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Accent (c, shade) ->
        if is_shadeless c then "accent-" ^ color_to_string c
        else "accent-" ^ color_to_string c ^ "-" ^ string_of_int shade
    | Accent_opacity (c, shade, opacity) ->
        if is_shadeless c then
          "accent-" ^ color_to_string c ^ opacity_suffix opacity
        else
          "accent-" ^ color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Accent_transparent -> "accent-transparent"
    | Accent_current -> "accent-current"
    | Accent_current_opacity opacity ->
        "accent-current" ^ opacity_suffix opacity
    | Accent_inherit -> "accent-inherit"
    | Accent_bracket_color (v, _) -> "accent-[" ^ v ^ "]"
    | Accent_bracket_color_opacity (v, _, opacity) ->
        "accent-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Caret (c, shade) ->
        if is_shadeless c then "caret-" ^ color_to_string c
        else "caret-" ^ color_to_string c ^ "-" ^ string_of_int shade
    | Caret_opacity (c, shade, opacity) ->
        if is_shadeless c then
          "caret-" ^ color_to_string c ^ opacity_suffix opacity
        else
          "caret-" ^ color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Caret_current -> "caret-current"
    | Caret_current_opacity opacity -> "caret-current" ^ opacity_suffix opacity
    | Caret_inherit -> "caret-inherit"
    | Caret_transparent -> "caret-transparent"
    | Caret_bracket_color (v, _) -> "caret-[" ^ v ^ "]"
    | Caret_bracket_color_opacity (v, _, opacity) ->
        "caret-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Outline (c, shade) ->
        if is_shadeless c then "outline-" ^ color_to_string c
        else "outline-" ^ color_to_string c ^ "-" ^ string_of_int shade
    | Outline_opacity (c, shade, opacity) ->
        if is_shadeless c then
          "outline-" ^ color_to_string c ^ opacity_suffix opacity
        else
          "outline-" ^ color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Outline_current -> "outline-current"
    | Outline_current_opacity opacity ->
        "outline-current" ^ opacity_suffix opacity
    | Outline_inherit -> "outline-inherit"
    | Outline_transparent -> "outline-transparent"
    | Outline_bracket_color (v, _) -> "outline-[" ^ v ^ "]"
    | Outline_bracket_color_opacity (v, _, opacity) ->
        "outline-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Outline_bracket_var v -> "outline-[" ^ v ^ "]"
    | Outline_bracket_var_opacity (v, opacity) ->
        "outline-[" ^ v ^ "]" ^ opacity_suffix opacity
    | Outline_bracket_typed_var v -> "outline-[color:" ^ v ^ "]"
    | Outline_bracket_typed_var_opacity (v, opacity) ->
        "outline-[color:" ^ v ^ "]" ^ opacity_suffix opacity
    | Placeholder (c, shade) ->
        if is_shadeless c then "placeholder-" ^ color_to_string c
        else "placeholder-" ^ color_to_string c ^ "-" ^ string_of_int shade
    | Placeholder_opacity (c, shade, opacity) ->
        if is_shadeless c then
          "placeholder-" ^ color_to_string c ^ opacity_suffix opacity
        else
          "placeholder-" ^ color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Placeholder_transparent -> "placeholder-transparent"
    | Placeholder_current -> "placeholder-current"
    | Placeholder_current_opacity opacity ->
        "placeholder-current" ^ opacity_suffix opacity
    | Placeholder_inherit -> "placeholder-inherit"
    | Placeholder_bracket_color (v, _) -> "placeholder-[" ^ v ^ "]"
    | Placeholder_bracket_color_opacity (v, _, opacity) ->
        "placeholder-[" ^ v ^ "]" ^ opacity_suffix opacity

  let examples =
    [
      Text_transparent;
      Border_transparent;
      Outline_transparent;
      Accent_transparent;
      Caret_transparent;
      Placeholder_transparent;
    ]
end

open Handler

module Utility_factory = Utility.Make (Handler)
(** Register color handler with Utility system *)

(** Re-export helper functions from Handler for use by other modules *)
let scheme_color_name = Handler.scheme_color_name

let property_color_var = Handler.property_color_var
let property_color_value = Handler.property_color_value
let opacity_to_percent = Handler.opacity_to_percent
let opacity_var_bare = Handler.opacity_var_bare
let opacity_var_bare_of = Handler.opacity_var_name
let shorten_hex_str = shorten_hex_str
let authored_hex = authored_hex
let bracket_color_opacity_style = Handler.bracket_color_opacity_style

type bracket_opacity = Handler.bracket_opacity =
  | Folded of Css.color
  | Guarded of { fallback : Css.color; mixed : Css.color }

let bracket_color_opacity = Handler.bracket_color_opacity
let css_color_to_hex = Handler.css_color_to_hex
let resolve_bracket_css_color = Handler.resolve_bracket_css_color
let pre_color_mix_fallback = Handler.pre_color_mix_fallback
let bracket_color_style = Handler.bracket_color_style
let parse_bracket_color = Handler.parse_bracket_color

type bracket_hint = Handler.bracket_hint =
  | Typed_var of string
  | Bare_var of string
  | Plain_color of Css.color

let parse_bracket_hint = Handler.parse_bracket_hint
let round_n = round_n

let hex_alpha_color ?theme c shade opacity =
  let open Handler in
  let percent = opacity_to_percent opacity in
  let color_name = scheme_color_name c shade in
  match Scheme.hex_color (resolve_scheme theme) color_name with
  | Some hex_value -> Some (hex_with_alpha hex_value percent)
  | None ->
      (* Shadeless base colours (black/white) have no scheme entry but a known
         hex, so an /opacity modifier still resolves to a colour. *)
      if is_base_color c then
        Some (hex_with_alpha (to_oklch_css c shade) percent)
      else
        (* A theme that binds palette colours to var references has no scheme
           hex; convert through oklch so the /opacity modifier still resolves,
           as [color_with_opacity_style] does for bg/text/border. *)
        let hex_value = rgb_to_hex (oklch_to_rgb (to_oklch c shade)) in
        Some (hex_with_alpha hex_value percent)

let color_mix_supports_condition = Handler.color_mix_supports_condition

(** {1 Color with Opacity Helpers}

    Generic helpers for scheme-aware color generation with progressive
    enhancement. These can be used by other modules (svg, divide) to avoid code
    duplication. *)

let custom_color_to_oklab c =
  match c with
  | Hex h -> (
      match hex_to_rgb h with
      | Some rgb -> rgb_to_oklab rgb
      | None -> (0.0, 0.0, 0.0))
  | Rgb { red; green; blue } -> rgb_to_oklab { r = red; g = green; b = blue }
  | _ -> (0.0, 0.0, 0.0)

let color_mix_supports ~decls =
  Css.supports ~condition:color_mix_supports_condition
    [ Css.rule ~selector:(Css.Selector.class_ "_") decls ]

let color_mix_supports_stmts ~stmts =
  Css.supports ~condition:color_mix_supports_condition stmts

let mix_alpha = Handler.mix_alpha
let opacity_fallback = Handler.opacity_fallback
let apply_alpha = Handler.apply_alpha

let oklab_with_supports ?theme ~property ~fallback_decl c shade opacity =
  let cvar = color_var c shade in
  let color_value = to_css ?theme c (if is_base_color c then 500 else shade) in
  let theme_decl, color_ref = Var.binding cvar color_value in
  let oklab_color = mix_alpha opacity (Css.Var color_ref) in
  let oklab_decl = property oklab_color in
  let supports_block = color_mix_supports ~decls:[ theme_decl; oklab_decl ] in
  Style.style ~rules:(Some [ supports_block ]) [ fallback_decl ]

let generic_color_with_opacity ?theme ~property c shade opacity =
  let open Handler in
  let percent = opacity_to_percent opacity in
  let alpha_var = opacity_var_name opacity <> None in
  match opacity_keyword c with
  | Some keyword -> Style.style [ property (apply_alpha opacity keyword) ]
  | None when is_fully_opaque opacity && not (is_custom_color c) ->
      (* At 100% the mix is a no-op, so Tailwind writes the colour itself. *)
      let theme_decl, color_ref =
        Var.binding (color_var c shade) (to_css ?theme c shade)
      in
      let value : Css.color = Var color_ref in
      Style.style [ theme_decl; property value ]
  | None when is_custom_color c ->
      if alpha_var then
        Style.style [ property (mix_alpha opacity (to_css c shade)) ]
      else
        let ok_l, ok_a, ok_b = custom_color_to_oklab c in
        let oklab_value = Css.oklaba ok_l ok_a ok_b (percent /. 100.0) in
        Style.style [ property oklab_value ]
  | None -> (
      let color_name = scheme_color_name c shade in
      match Scheme.hex_color (resolve_scheme theme) color_name with
      | Some hex_value ->
          let fallback_decl =
            if alpha_var then property (Css.hex hex_value)
            else property (Css.hex (hex_with_alpha hex_value percent))
          in
          oklab_with_supports ?theme ~property ~fallback_decl c shade opacity
      | None ->
          (* A palette colour renders as its oklch node; a project token as
             whatever the theme bound it to. *)
          let base = to_css ?theme c shade in
          let fallback_color =
            if alpha_var then base
            else opacity_fallback ?theme ~percent c shade base
          in
          oklab_with_supports ?theme ~property
            ~fallback_decl:(property fallback_color) c shade opacity)

let generic_current_with_opacity ?merge_key ~fallback_decl ~property opacity =
  let oklab_color = mix_alpha opacity Css.Current in
  let oklab_decl = property oklab_color in
  let supports_block =
    Css.supports ~condition:color_mix_supports_condition
      [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
  in
  Style.style ?merge_key ~rules:(Some [ supports_block ]) [ fallback_decl ]

(* Fill/stroke helpers for SVG utilities *)
let fill_with_opacity ?theme c shade opacity =
  generic_color_with_opacity ?theme
    ~property:(fun color -> Css.fill (Css.Color color))
    c shade opacity

let stroke_with_opacity ?theme c shade opacity =
  generic_color_with_opacity ?theme
    ~property:(fun color -> Css.stroke (Css.Color color))
    c shade opacity

let fill_current_with_opacity opacity =
  generic_current_with_opacity ~merge_key:"fill-current"
    ~fallback_decl:(Css.fill Current_color)
    ~property:(fun color -> Css.fill (Css.Color color))
    opacity

let stroke_current_with_opacity opacity =
  generic_current_with_opacity ~merge_key:"stroke-current"
    ~fallback_decl:(Css.stroke Current_color)
    ~property:(fun color -> Css.stroke (Css.Color color))
    opacity

let divide_opacity_via_property ?theme ~selector c shade opacity =
  let cvar =
    property_color_var ?theme ~property_prefix:"border-color" c shade
  in
  let color_value =
    property_color_value ?theme ~property_prefix:"border-color" c shade
  in
  let fallback_color =
    if Handler.opacity_var_name opacity <> None then color_value
    else
      Handler.opacity_fallback ?theme
        ~percent:(Handler.opacity_to_percent opacity)
        c shade color_value
  in
  let fallback_rule = Css.rule ~selector [ Css.border_color fallback_color ] in
  let theme_decl, color_ref = Var.binding cvar color_value in
  let oklab_color = mix_alpha opacity (Css.Var color_ref) in
  let supports_rule =
    Css.rule ~selector [ theme_decl; Css.border_color oklab_color ]
  in
  let supports_block = color_mix_supports_stmts ~stmts:[ supports_rule ] in
  Style.style ~rules:(Some [ fallback_rule; supports_block ]) []

let bg_opacity_via_property ?theme c shade opacity =
  let cvar = color_var c shade in
  let color_value = to_css ?theme c (if is_base_color c then 500 else shade) in
  let theme_decl, color_ref = Var.binding cvar color_value in
  (* An opacity read from a var has no percentage to fold into the fallback, so
     that is the colour at full opacity, as Tailwind emits. *)
  let fallback_decl =
    match Handler.opacity_var_name opacity with
    | Some _ -> Css.background_color color_value
    | None ->
        Css.background_color
          (Handler.opacity_fallback ?theme
             ~percent:(Handler.opacity_to_percent opacity)
             c shade color_value)
  in
  let oklab_decl =
    Css.background_color (mix_alpha opacity (Css.Var color_ref))
  in
  let supports_block = color_mix_supports ~decls:[ theme_decl; oklab_decl ] in
  Style.style ~rules:(Some [ supports_block ]) [ fallback_decl ]

(* Divide helpers with custom selector *)
let divide_with_opacity_selector ?theme ~selector c shade opacity =
  let open Handler in
  let percent = opacity_to_percent opacity in
  let alpha_var = opacity_var_name opacity <> None in
  match opacity_keyword c with
  | Some keyword ->
      let rule =
        Css.rule ~selector [ Css.border_color (apply_alpha opacity keyword) ]
      in
      Style.style ~rules:(Some [ rule ]) []
  | None when is_fully_opaque opacity && not (is_custom_color c) ->
      let theme_decl, color_ref =
        Var.binding (color_var c shade) (to_css ?theme c shade)
      in
      let value : Css.color = Var color_ref in
      let rule = Css.rule ~selector [ Css.border_color value ] in
      Style.style ~rules:(Some [ rule ]) [ theme_decl ]
  | None when is_custom_color c ->
      let value =
        if alpha_var then mix_alpha opacity (to_css c shade)
        else
          let ok_l, ok_a, ok_b = custom_color_to_oklab c in
          Css.oklaba ok_l ok_a ok_b (percent /. 100.0)
      in
      let rule = Css.rule ~selector [ Css.border_color value ] in
      Style.style ~rules:(Some [ rule ]) []
  | None -> (
      let color_name = scheme_color_name c shade in
      match Scheme.hex_color (resolve_scheme theme) color_name with
      | Some hex_value ->
          let hex_alpha =
            if alpha_var then hex_value else hex_with_alpha hex_value percent
          in
          let fallback_rule =
            Css.rule ~selector [ Css.border_color (Css.hex hex_alpha) ]
          in
          let cvar = color_var c shade in
          let _theme_decl, color_ref = Var.binding cvar (Css.hex hex_value) in
          let oklab_color = mix_alpha opacity (Css.Var color_ref) in
          let supports_rule =
            Css.rule ~selector [ Css.border_color oklab_color ]
          in
          let supports_block =
            color_mix_supports_stmts ~stmts:[ supports_rule ]
          in
          Style.style ~rules:(Some [ fallback_rule; supports_block ]) []
      | None -> divide_opacity_via_property ?theme ~selector c shade opacity)

let divide_with_opacity ?theme c shade opacity selector =
  divide_with_opacity_selector ?theme ~selector c shade opacity

let divide_current_with_opacity_selector ~selector opacity =
  let open Handler in
  let percent = opacity_to_percent opacity in
  (* Fallback: just currentColor (browsers that don't support color-mix) *)
  let fallback_rule = Css.rule ~selector [ Css.border_color Css.Current ] in
  let oklab_color =
    Css.color_mix ~in_space:Oklab Css.Current Css.Transparent ~percent1:percent
  in
  let supports_rule = Css.rule ~selector [ Css.border_color oklab_color ] in
  let supports_block =
    Css.supports ~condition:color_mix_supports_condition [ supports_rule ]
  in
  Style.style ~rules:(Some [ fallback_rule; supports_block ]) []

let divide_current_with_opacity opacity selector =
  divide_current_with_opacity_selector ~selector opacity

(** Background color with opacity - scheme-aware. Uses hex+alpha fallback with
    theme variable in [@supports] block. *)
let bg_with_opacity ?theme c shade opacity =
  let open Handler in
  let percent = opacity_to_percent opacity in
  match opacity_keyword c with
  | Some keyword ->
      Style.style [ Css.background_color (apply_alpha opacity keyword) ]
  | None when is_fully_opaque opacity ->
      (* 100% opacity = no transparency. Tailwind outputs the plain color var
         reference, identical to the no-opacity case. *)
      let cvar = color_var c shade in
      let color_value =
        to_css ?theme c (if is_base_color c then 500 else shade)
      in
      let _d, color_ref = Var.binding cvar color_value in
      Style.style [ Css.background_color (Var color_ref) ]
  | None when is_custom_color c ->
      let value =
        if opacity_var_name opacity <> None then
          mix_alpha opacity (to_css c shade)
        else
          let ok_l, ok_a, ok_b = custom_color_to_oklab c in
          Css.oklaba_none_zeros ok_l ok_a ok_b (percent /. 100.0)
      in
      Style.style [ Css.background_color value ]
  | None -> (
      let color_name = scheme_color_name c shade in
      match Scheme.hex_color (resolve_scheme theme) color_name with
      | Some hex_value ->
          let cvar = color_var c shade in
          let theme_decl, color_ref = Var.binding cvar (Css.hex hex_value) in
          let fallback_decl =
            if opacity_var_name opacity <> None then
              Css.background_color (Css.Var color_ref)
            else
              Css.background_color (Css.hex (hex_with_alpha hex_value percent))
          in
          let oklab_decl =
            Css.background_color (mix_alpha opacity (Css.Var color_ref))
          in
          let supports_block = color_mix_supports ~decls:[ oklab_decl ] in
          Style.style ~rules:(Some [ supports_block ])
            [ theme_decl; fallback_decl ]
      | None -> bg_opacity_via_property ?theme c shade opacity)

(** Determine the appropriate fallback for an opacity theme variable. If the
    theme defines a concrete value (e.g., "0.5"), use [Fallback (Num f)]. If the
    theme defines a var reference (e.g., "var(--custom-opacity)"), use
    [Var_fallback] with the inner var name. Otherwise fall back to the
    conventional [name-opacity] pattern. *)
let opacity_fallback_for_theme_value ?theme var_name bare :
    Css.percentage Css.fallback =
  match Scheme.theme_value theme var_name with
  | Some value when String.length value > 4 && String.sub value 0 4 = "var(" ->
      (* Theme value is a var reference like "var(--custom-opacity)" *)
      let inner = String.sub value 4 (String.length value - 5) in
      let name =
        if String.length inner > 2 && String.sub inner 0 2 = "--" then
          String.sub inner 2 (String.length inner - 2)
        else inner
      in
      Css.Var_fallback name
  | Some value -> (
      match float_of_string_opt (String.trim value) with
      | Some f -> Css.Fallback (Css.Num f)
      | None -> Css.Var_fallback (bare ^ "-opacity"))
  | None -> Css.Var_fallback (bare ^ "-opacity")

(** Background currentColor with opacity *)
let bg_current_with_opacity ?theme opacity =
  let open Handler in
  let fallback_decl = Css.background_color Css.Current in
  let oklab_color =
    match opacity with
    | Opacity_named name ->
        let bare = Parse.extract_var_name name in
        let var_name = "opacity-" ^ bare in
        let fallback = opacity_fallback_for_theme_value ?theme var_name bare in
        Css.color_mix_var_pct_fallback ~in_space:Oklab ~var_name ~fallback
          Css.Current Css.Transparent
    | Opacity_var var_str ->
        let bare = Handler.opacity_var_bare var_str in
        Css.color_mix_var_percent ~in_space:Oklab ~var_name:bare Css.Current
          Css.Transparent
    | _ ->
        let percent = opacity_to_percent opacity in
        Css.color_mix ~in_space:Oklab Css.Current Css.Transparent
          ~percent1:percent
  in
  let oklab_decl = Css.background_color oklab_color in
  let supports_block =
    Css.supports ~condition:color_mix_supports_condition
      [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
  in
  Style.style ~rules:(Some [ supports_block ]) [ fallback_decl ]

(** Public API *)
let utility = Utility_factory.v

let text ?opacity ?(shade = 500) color =
  check_shade ~utility:"text" color shade;
  match opacity with
  | None -> utility (Text (color, shade))
  | Some pct -> utility (Text_opacity (color, shade, opacity_of_int pct))

let border_color ?opacity ?(shade = 500) color =
  check_shade ~utility:"border_color" color shade;
  match opacity with
  | None -> utility (Border (color, shade))
  | Some pct -> utility (Border_opacity (color, shade, opacity_of_int pct))

let text_transparent = utility Text_transparent
let text_current = utility Text_current
let text_inherit = utility Text_inherit
let border_transparent = utility Border_transparent
let border_current = utility Border_current

let outline_color ?opacity ?(shade = 500) color =
  check_shade ~utility:"outline_color" color shade;
  match opacity with
  | None -> utility (Outline (color, shade))
  | Some pct -> utility (Outline_opacity (color, shade, opacity_of_int pct))

let outline_transparent = utility Outline_transparent
let outline_current = utility Outline_current
let outline_inherit = utility Outline_inherit

let accent ?opacity ?(shade = 500) color =
  check_shade ~utility:"accent" color shade;
  match opacity with
  | None -> utility (Accent (color, shade))
  | Some pct -> utility (Accent_opacity (color, shade, opacity_of_int pct))

let accent_current = utility Accent_current
let accent_inherit = utility Accent_inherit

let caret ?opacity ?(shade = 500) color =
  check_shade ~utility:"caret" color shade;
  match opacity with
  | None -> utility (Caret (color, shade))
  | Some pct -> utility (Caret_opacity (color, shade, opacity_of_int pct))

let caret_current = utility Caret_current
let caret_inherit = utility Caret_inherit
let caret_transparent = utility Caret_transparent
