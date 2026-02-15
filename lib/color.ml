(** Color conversion utilities for Tailwind v4 compatibility *)

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

let linearize_channel c =
  let c' = float_of_int c /. 255.0 in
  if c' <= 0.04045 then c' /. 12.92 else ((c' +. 0.055) /. 1.055) ** 2.4

let gamma_correct c =
  let c' =
    if c <= 0.0031308 then c *. 12.92
    else (1.055 *. (c ** (1.0 /. 2.4))) -. 0.055
  in
  int_of_float ((c' *. 255.0) +. 0.5)

let rgb_to_oklch rgb =
  (* Convert to linear RGB *)
  let r_lin = linearize_channel rgb.r in
  let g_lin = linearize_channel rgb.g in
  let b_lin = linearize_channel rgb.b in

  (* Convert to OKLab using the standard matrix *)
  (* M1: linear RGB to LMS *)
  let l =
    (0.4122214708 *. r_lin) +. (0.5363325363 *. g_lin) +. (0.0514459929 *. b_lin)
  in
  let m =
    (0.2119034982 *. r_lin) +. (0.6806995451 *. g_lin) +. (0.1073969566 *. b_lin)
  in
  let s =
    (0.0883024619 *. r_lin) +. (0.2817188376 *. g_lin) +. (0.6299787005 *. b_lin)
  in

  (* Apply cube root for perceptual uniformity *)
  let cbrt x = if x < 0.0 then -.(-.x ** (1.0 /. 3.0)) else x ** (1.0 /. 3.0) in

  let l' = cbrt l in
  let m' = cbrt m in
  let s' = cbrt s in

  (* M2: LMS' to Lab coordinates *)
  let ok_l =
    (0.2104542553 *. l') +. (0.7936177850 *. m') -. (0.0040720468 *. s')
  in
  let ok_a =
    (1.9779984951 *. l') -. (2.4285922050 *. m') +. (0.4505937099 *. s')
  in
  let ok_b =
    (0.0259040371 *. l') +. (0.7827717662 *. m') -. (0.8086757660 *. s')
  in

  (* Convert to LCH *)
  let lightness = ok_l *. 100.0 in
  let chroma = sqrt ((ok_a *. ok_a) +. (ok_b *. ok_b)) in
  let hue =
    let h = atan2 ok_b ok_a *. 180.0 /. Float.pi in
    if h < 0.0 then h +. 360.0 else h
  in

  { l = lightness; c = chroma; h = hue }

let oklch_to_rgb oklch =
  (* Convert LCH to Lab *)
  let ok_l = oklch.l /. 100.0 in
  let h_rad = oklch.h *. Float.pi /. 180.0 in
  let ok_a = oklch.c *. cos h_rad in
  let ok_b = oklch.c *. sin h_rad in

  (* Convert from OKLab to linear RGB *)
  let l' = ok_l +. (0.3963377774 *. ok_a) +. (0.2158037573 *. ok_b) in
  let m' = ok_l -. (0.1055613458 *. ok_a) -. (0.0638541728 *. ok_b) in
  let s' = ok_l -. (0.0894841775 *. ok_a) -. (1.2914855480 *. ok_b) in

  let l = l' *. l' *. l' in
  let m = m' *. m' *. m' in
  let s = s' *. s' *. s' in

  (* Convert to linear RGB *)
  let r_lin =
    (4.0767416621 *. l) -. (3.3077115913 *. m) +. (0.2309699292 *. s)
  in
  let g_lin =
    (-1.2684380046 *. l) +. (2.6097574011 *. m) -. (0.3413193965 *. s)
  in
  let b_lin =
    (-0.0041960863 *. l) -. (0.7034186147 *. m) +. (1.7076147010 *. s)
  in

  (* Apply gamma correction and clamp to 0-255 *)
  let clamp x = max 0 (min 255 x) in
  {
    r = clamp (gamma_correct r_lin);
    g = clamp (gamma_correct g_lin);
    b = clamp (gamma_correct b_lin);
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
  let to_hex_byte n =
    let hex = "0123456789abcdef" in
    String.make 1 hex.[n / 16] ^ String.make 1 hex.[n mod 16]
  in
  "#" ^ to_hex_byte rgb.r ^ to_hex_byte rgb.g ^ to_hex_byte rgb.b

(** Add alpha to a hex color string. Returns #RRGGBBAA format. The opacity is a
    percentage (0-100). *)
let hex_with_alpha hex_str opacity_percent =
  let to_hex_byte n =
    let hex = "0123456789abcdef" in
    String.make 1 hex.[n / 16] ^ String.make 1 hex.[n mod 16]
  in
  (* Parse hex color *)
  let hex_clean =
    if String.length hex_str > 0 && hex_str.[0] = '#' then
      String.sub hex_str 1 (String.length hex_str - 1)
    else hex_str
  in
  (* Convert opacity percentage to 8-bit alpha value, with rounding *)
  let alpha = int_of_float ((opacity_percent /. 100.0 *. 255.0) +. 0.5) in
  let alpha_clamped = max 0 (min 255 alpha) in
  "#" ^ hex_clean ^ to_hex_byte alpha_clamped

let oklch_to_css oklch =
  let pp_oklch ctx oklch =
    Css.Pp.string ctx "oklch(";
    Css.Pp.float_n 1 ctx oklch.l;
    Css.Pp.string ctx "% ";
    Css.Pp.float_n 3 ctx oklch.c;
    Css.Pp.string ctx " ";
    Css.Pp.float_n 3 ctx oklch.h;
    Css.Pp.string ctx ")"
  in
  Css.Pp.to_string ~minify:false pp_oklch oklch

let hex_to_oklch_css hex =
  match hex_to_rgb hex with
  | Some rgb -> oklch_to_css (rgb_to_oklch rgb)
  | None -> hex (* Fallback to original hex if parsing fails *)

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
  if String.length hex_no_hash <> 6 then hex_no_hash
  else
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
  | s -> Error (`Msg ("Unknown color: " ^ s))

let rgb r g b =
  if r < 0 || r > 255 then
    invalid_arg ("RGB red value " ^ string_of_int r ^ " out of range [0-255]");
  if g < 0 || g > 255 then
    invalid_arg ("RGB green value " ^ string_of_int g ^ " out of range [0-255]");
  if b < 0 || b > 255 then
    invalid_arg ("RGB blue value " ^ string_of_int b ^ " out of range [0-255]");
  Rgb { red = r; green = g; blue = b }

(* Convert color to OKLCH data for a given shade *)
let to_oklch color shade =
  match color with
  | Black -> { l = 0.0; c = 0.0; h = 0.0 }
  | White -> { l = 100.0; c = 0.0; h = 0.0 }
  | Oklch oklch -> oklch
  | Hex h -> (
      match hex_to_rgb h with
      | Some rgb -> rgb_to_oklch rgb
      | None -> { l = 0.0; c = 0.0; h = 0.0 })
  | Rgb { red; green; blue } -> rgb_to_oklch { r = red; g = green; b = blue }
  | _ -> (
      (* For named colors, get OKLCH data directly from Tailwind *)
      let color_name =
        match color with
        | Gray -> "gray"
        | Slate -> "slate"
        | Zinc -> "zinc"
        | Neutral -> "neutral"
        | Stone -> "stone"
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
      match Tailwind.get_color_oklch color_name shade with
      | Some oklch -> oklch
      | None ->
          failwith
            ("No OKLCH data for color " ^ color_name ^ " shade "
           ^ string_of_int shade))

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

(* Convert color to CSS color value *)
let to_css color shade =
  match color with
  | Black -> Css.Hex { hash = true; value = "000" }
  | White -> Css.Hex { hash = true; value = "fff" }
  | Hex hex ->
      (* For arbitrary hex colors, always output valid CSS with # prefix. Per
         MDN spec, hex colors MUST have # prefix. *)
      let hex_value =
        if String.starts_with ~prefix:"#" hex then
          String.sub hex 1 (String.length hex - 1)
        else hex
      in
      Css.Hex { hash = true; value = hex_value }
  | Oklch oklch ->
      (* Use the new Oklch constructor *)
      Css.oklch oklch.l oklch.c oklch.h
  | _ ->
      (* For other colors, get OKLCH data directly *)
      let oklch = to_oklch color shade in
      Css.oklch oklch.l oklch.c oklch.h

(* Get the name of a color as a string *)
let to_name = function
  | Black -> "black"
  | White -> "white"
  | Gray -> "gray"
  | Slate -> "slate"
  | Zinc -> "zinc"
  | Neutral -> "neutral"
  | Stone -> "stone"
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

(* Pretty printer for colors *)
let pp = function
  | Black -> "black"
  | White -> "white"
  | Gray -> "gray"
  | Slate -> "slate"
  | Zinc -> "zinc"
  | Neutral -> "neutral"
  | Stone -> "stone"
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
  | Hex s ->
      (* Use Tailwind's arbitrary value syntax [#hex] for hex colors *)
      let hex_value = if String.starts_with ~prefix:"#" s then s else "#" ^ s in
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

(* Check if a color is black or white *)
let is_base_color = function Black | White -> true | _ -> false

(* Check if a color is a custom color (hex, rgb, or oklch) *)
let is_custom_color = function Hex _ | Rgb _ | Oklch _ -> true | _ -> false

(** {1 Color Application Utilities} *)

(** Background color utilities *)

(* Theme layer color variable ordering map. See rules.mli for detailed layer
   ordering documentation. *)
let theme_color_order_map =
  [
    (* Transparent first *)
    ("transparent", 0);
    (* Regular colors with shades *)
    ("red", 1);
    ("orange", 2);
    ("amber", 3);
    ("yellow", 4);
    ("lime", 5);
    ("green", 6);
    ("emerald", 7);
    ("teal", 8);
    ("cyan", 9);
    ("sky", 10);
    ("blue", 11);
    ("indigo", 12);
    ("violet", 13);
    ("purple", 14);
    ("fuchsia", 15);
    ("pink", 16);
    ("rose", 17);
    ("slate", 18);
    ("gray", 19);
    ("zinc", 20);
    ("neutral", 21);
    ("stone", 22);
    (* Base colors come last *)
    ("black", 23);
    ("white", 24);
  ]

(* Utilities layer color ordering map for conflict resolution. *)
let utilities_color_order_map =
  [
    (* Basic colors come first *)
    ("transparent", 0);
    ("black", 1);
    ("amber", 2);
    ("blue", 3);
    ("cyan", 4);
    ("emerald", 5);
    ("fuchsia", 6);
    ("gray", 7);
    ("green", 8);
    ("indigo", 9);
    ("lime", 10);
    ("neutral", 11);
    ("orange", 12);
    ("pink", 13);
    ("purple", 14);
    ("red", 15);
    ("rose", 16);
    ("sky", 17);
    ("slate", 18);
    ("stone", 19);
    ("teal", 20);
    ("violet", 21);
    ("white", 22);
    ("yellow", 23);
    ("zinc", 24);
  ]

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
  try
    let last_dash = String.rindex color_part '-' in
    let color_name = String.sub color_part 0 last_dash in
    let shade_str =
      String.sub color_part (last_dash + 1)
        (String.length color_part - last_dash - 1)
    in
    let shade = int_of_string shade_str in
    let _, color_order = utilities_order color_name in
    (color_order * 1000) + shade
  with Not_found | Failure _ -> (
    (* Non-numeric or single-color names like "black", "white" *)
    try
      let _, color_order = utilities_order color_part in
      color_order * 1000
    with Not_found | Failure _ -> failwith ("Unknown color: " ^ color_part))

(* Get theme layer order for a color variable with shade. Formula: (priority=2,
   base_order * 1000 + shade) This ensures color variables are grouped by color
   with shades in ascending order. *)
let theme_order_with_shade color_name shade =
  let var_priority, base_order = theme_order color_name in
  (var_priority, base_order + shade)

(* Memoization table for color variables *)
let color_var_cache : (string, Css.color Var.theme) Hashtbl.t =
  Hashtbl.create 128

(* Helper to create a color variable with memoization. Creates theme layer
   variables with deterministic ordering based on color and shade. *)
let get_color_var color shade =
  let base = pp color in
  (* Escape brackets in variable names - CSS variable names can't have unescaped
     [] *)
  let escaped_base =
    let s = String.to_seq base |> List.of_seq in
    let escaped =
      List.concat_map
        (function '[' -> [ '\\'; '[' ] | ']' -> [ '\\'; ']' ] | c -> [ c ])
        s
    in
    String.of_seq (List.to_seq escaped)
  in
  let name =
    if is_base_color color then "color-" ^ escaped_base
    else "color-" ^ escaped_base ^ "-" ^ string_of_int shade
  in
  (* Check if variable already exists in cache *)
  match Hashtbl.find_opt color_var_cache name with
  | Some var -> var
  | None ->
      (* Create theme variable with deterministic theme layer order: - Base
         colors use theme_order(color_name) - Shaded colors use
         theme_order_with_shade(color_name, shade)

         Note: Tailwind v4 appears to order variables by first usage in the
         input, not by a fixed ordering. Our implementation uses a fixed
         ordering for determinism and consistency. *)
      let var_order =
        if is_base_color color then theme_order_with_shade base 0
        else theme_order_with_shade base shade
      in
      let var = Var.theme Css.Color name ~order:var_order in
      Hashtbl.add color_var_cache name var;
      var

let color_to_string (c : color) : string =
  match c with
  | Black -> "black"
  | White -> "white"
  | Gray -> "gray"
  | Slate -> "slate"
  | Zinc -> "zinc"
  | Neutral -> "neutral"
  | Stone -> "stone"
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

(** Color parsing utilities *)

(** Opacity modifier type *)
type opacity_modifier =
  | No_opacity
  | Opacity_percent of float (* e.g., /50 means 50% *)
  | Opacity_arbitrary of float (* e.g., /[0.5] means 0.5 *)
  | Opacity_named of string (* e.g., /half, /custom - theme-defined names *)

(** Parse opacity modifier from a string that may contain /NN or /[N.N] *)
let parse_opacity_modifier s =
  match String.index_opt s '/' with
  | None -> (s, No_opacity)
  | Some idx -> (
      let base = String.sub s 0 idx in
      let opacity_str = String.sub s (idx + 1) (String.length s - idx - 1) in
      if
        String.length opacity_str > 2
        && opacity_str.[0] = '['
        && opacity_str.[String.length opacity_str - 1] = ']'
      then
        (* Arbitrary value like [0.5] or [50%] *)
        let inner = String.sub opacity_str 1 (String.length opacity_str - 2) in
        if String.ends_with ~suffix:"%" inner then
          let num_str = String.sub inner 0 (String.length inner - 1) in
          match float_of_string_opt num_str with
          | Some f -> (base, Opacity_percent f)
          | None -> (s, No_opacity)
        else
          match float_of_string_opt inner with
          | Some f -> (base, Opacity_arbitrary f)
          | None -> (s, No_opacity)
      else
        (* Numeric value like 50 or 2.5, or named like half, custom *)
        match float_of_string_opt opacity_str with
        | Some f -> (base, Opacity_percent f)
        | None ->
            (* Not a number, treat as named opacity modifier *)
            if opacity_str <> "" then (base, Opacity_named opacity_str)
            else (s, No_opacity))

(* Parse color and shade from string list *)
let shade_of_strings = function
  | [ color_str; shade_str ] -> (
      match of_string color_str with
      | Ok color -> (
          match int_of_string_opt shade_str with
          | Some shade when shade >= 0 -> Ok (color, shade)
          | _ -> Error (`Msg ("Invalid shade: " ^ shade_str)))
      | Error _ -> Error (`Msg ("Invalid color: " ^ color_str)))
  | [ color_str ] -> (
      match of_string color_str with
      | Ok color -> Ok (color, 500) (* Default shade *)
      | Error _ -> Error (`Msg ("Invalid color: " ^ color_str)))
  | [] -> Error (`Msg "No color specified")
  | _ -> Error (`Msg "Too many color parts")

(* Parse color, shade, and optional opacity modifier from string list. Handles
   formats like ["red"; "500/50"] or ["red"; "500/[0.5]"] *)
let shade_and_opacity_of_strings = function
  | [ color_str; shade_opacity_str ] -> (
      let shade_str, opacity = parse_opacity_modifier shade_opacity_str in
      match of_string color_str with
      | Ok color -> (
          match int_of_string_opt shade_str with
          | Some shade when shade >= 0 -> Ok (color, shade, opacity)
          | _ -> Error (`Msg ("Invalid shade: " ^ shade_str)))
      | Error _ -> Error (`Msg ("Invalid color: " ^ color_str)))
  | [ color_str ] -> (
      (* Could be "current/50" or just "black" *)
      let base_str, opacity = parse_opacity_modifier color_str in
      match of_string base_str with
      | Ok color -> Ok (color, 500, opacity)
      | Error _ -> Error (`Msg ("Invalid color: " ^ color_str)))
  | [] -> Error (`Msg "No color specified")
  | _ -> Error (`Msg "Too many color parts")

(** {1 Parsing Functions} *)

module Handler = struct
  (** Local color utility type *)
  type t =
    (* Background colors *)
    | Bg of color * int
    | Bg_opacity of color * int * opacity_modifier
    | Bg_transparent
    | Bg_current
    | Bg_current_opacity of opacity_modifier
    (* Text colors *)
    | Text of color * int
    | Text_opacity of color * int * opacity_modifier
    | Text_transparent
    | Text_current
    | Text_current_opacity of opacity_modifier
    | Text_inherit
    (* Border colors *)
    | Border of color * int
    | Border_opacity of color * int * opacity_modifier
    | Border_transparent
    | Border_current
    | Border_current_opacity of opacity_modifier
    (* Accent colors *)
    | Accent of color * int
    | Accent_opacity of color * int * opacity_modifier
    | Accent_current
    | Accent_current_opacity of opacity_modifier
    | Accent_inherit
    (* Caret colors *)
    | Caret of color * int
    | Caret_opacity of color * int * opacity_modifier
    | Caret_current
    | Caret_current_opacity of opacity_modifier
    | Caret_inherit
    | Caret_transparent
    (* Outline colors *)
    | Outline of color * int
    | Outline_opacity of color * int * opacity_modifier
    | Outline_current
    | Outline_current_opacity of opacity_modifier
    | Outline_inherit

  (** Extensible variant for color utilities *)
  type Utility.base += Self of t

  (** Current scheme for color generation. Default uses oklch/color-mix. *)
  let current_scheme : Scheme.t ref = ref Scheme.default

  (** Set the current scheme for color generation *)
  let set_scheme scheme = current_scheme := scheme

  (** Get the scheme color name for a color and shade (e.g., "red-500"). Must be
      defined before [open Css] to use the outer [color] type. *)
  let scheme_color_name (c : color) shade =
    let base = pp c in
    if is_base_color c then base else base ^ "-" ^ string_of_int shade

  (** Get the color value for a color and shade, checking scheme first. When
      scheme defines the color as hex, returns hex. Otherwise returns oklch. *)
  let get_color_value (c : color) shade =
    let color_name = scheme_color_name c shade in
    match Scheme.get_hex_color !current_scheme color_name with
    | Some hex -> Css.hex hex
    | None -> to_css c (if is_base_color c then 500 else shade)

  open Style
  open Css

  let name = "color"
  let priority = 23

  (* Helper to check if a string contains an opacity modifier *)
  let has_opacity s = String.contains s '/'

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "bg"; "transparent" ] -> Ok Bg_transparent
    | [ "bg"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let _, opacity = parse_opacity_modifier current_str in
        match opacity with
        | No_opacity -> Ok Bg_current
        | _ -> Ok (Bg_current_opacity opacity))
    | "bg" :: color_parts when List.exists has_opacity color_parts -> (
        match shade_and_opacity_of_strings color_parts with
        | Ok (color, shade, opacity) -> Ok (Bg_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "bg" :: color_parts -> (
        match shade_of_strings color_parts with
        | Ok (color, shade) -> Ok (Bg (color, shade))
        | Error e -> Error e)
    | [ "text"; "transparent" ] -> Ok Text_transparent
    | [ "text"; "inherit" ] -> Ok Text_inherit
    | [ "text"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let _, opacity = parse_opacity_modifier current_str in
        match opacity with
        | No_opacity -> Ok Text_current
        | _ -> Ok (Text_current_opacity opacity))
    | "text" :: color_parts when List.exists has_opacity color_parts -> (
        match shade_and_opacity_of_strings color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Text_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "text" :: color_parts -> (
        match shade_of_strings color_parts with
        | Ok (color, shade) -> Ok (Text (color, shade))
        | Error e -> Error e)
    | [ "border"; "transparent" ] -> Ok Border_transparent
    | [ "border"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let _, opacity = parse_opacity_modifier current_str in
        match opacity with
        | No_opacity -> Ok Border_current
        | _ -> Ok (Border_current_opacity opacity))
    | "border" :: color_parts when List.exists has_opacity color_parts -> (
        match shade_and_opacity_of_strings color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Border_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "border" :: color_parts -> (
        match shade_of_strings color_parts with
        | Ok (color, shade) -> Ok (Border (color, shade))
        | Error e -> Error e)
    | [ "accent"; "inherit" ] -> Ok Accent_inherit
    | [ "accent"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let _, opacity = parse_opacity_modifier current_str in
        match opacity with
        | No_opacity -> Ok Accent_current
        | _ -> Ok (Accent_current_opacity opacity))
    | "accent" :: color_parts when List.exists has_opacity color_parts -> (
        match shade_and_opacity_of_strings color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Accent_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "accent" :: color_parts -> (
        match shade_of_strings color_parts with
        | Ok (color, shade) -> Ok (Accent (color, shade))
        | Error e -> Error e)
    | [ "caret"; "inherit" ] -> Ok Caret_inherit
    | [ "caret"; "transparent" ] -> Ok Caret_transparent
    | [ "caret"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let _, opacity = parse_opacity_modifier current_str in
        match opacity with
        | No_opacity -> Ok Caret_current
        | _ -> Ok (Caret_current_opacity opacity))
    | "caret" :: color_parts when List.exists has_opacity color_parts -> (
        match shade_and_opacity_of_strings color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Caret_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "caret" :: color_parts -> (
        match shade_of_strings color_parts with
        | Ok (color, shade) -> Ok (Caret (color, shade))
        | Error e -> Error e)
    | [ "outline"; "inherit" ] -> Ok Outline_inherit
    | [ "outline"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let _, opacity = parse_opacity_modifier current_str in
        match opacity with
        | No_opacity -> Ok Outline_current
        | _ -> Ok (Outline_current_opacity opacity))
    | "outline" :: color_parts when List.exists has_opacity color_parts -> (
        match shade_and_opacity_of_strings color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Outline_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "outline" :: color_parts -> (
        match shade_of_strings color_parts with
        | Ok (color, shade) -> Ok (Outline (color, shade))
        | Error e -> Error e)
    | _ -> Error (`Msg "Not a color utility")

  let bg' c shade =
    if is_custom_color c then
      let css_color = to_css c shade in
      style [ Css.background_color css_color ]
    else
      let color_var = get_color_var c shade in
      let color_value = get_color_value c shade in
      let decl, color_ref = Var.binding color_var color_value in
      style (decl :: [ Css.background_color (Css.Var color_ref) ])

  let bg_transparent = style [ Css.background_color (Css.hex "#0000") ]
  let bg_current = style [ Css.background_color Css.Current ]

  (** Text color utilities *)

  let text' color shade =
    if is_custom_color color then
      let css_color = to_css color shade in
      style [ Css.color css_color ]
    else
      let color_var = get_color_var color shade in
      let color_value = get_color_value color shade in
      let decl, color_ref = Var.binding color_var color_value in
      style (decl :: [ Css.color (Var color_ref) ])

  let text_transparent = style [ Css.color (Css.hex "#0000") ]
  let text_current = style [ Css.color Current ]
  let text_inherit = style [ Css.color Inherit ]

  (** Border color utilities *)

  let border_color' color shade =
    if is_custom_color color then
      let css_color = to_css color shade in
      style [ Css.border_color css_color ]
    else
      let color_var = get_color_var color shade in
      let color_value = get_color_value color shade in
      let decl, color_ref = Var.binding color_var color_value in
      style (decl :: [ Css.border_color (Var color_ref) ])

  let border_transparent = style [ Css.border_color (Css.hex "#0000") ]
  let border_current = style [ Css.border_color Current ]

  (** Accent color utilities *)

  let accent' color shade =
    if is_custom_color color then
      let css_color = to_css color shade in
      style [ Css.accent_color css_color ]
    else
      let color_var = get_color_var color shade in
      let color_value = get_color_value color shade in
      let decl, color_ref = Var.binding color_var color_value in
      style (decl :: [ Css.accent_color (Var color_ref) ])

  let accent_current = style [ Css.accent_color Current ]
  let accent_inherit = style [ Css.accent_color Inherit ]

  (** Caret color utilities *)

  let caret' color shade =
    if is_custom_color color then
      let css_color = to_css color shade in
      style [ Css.caret_color css_color ]
    else
      let color_var = get_color_var color shade in
      let color_value = get_color_value color shade in
      let decl, color_ref = Var.binding color_var color_value in
      style (decl :: [ Css.caret_color (Var color_ref) ])

  let caret_current = style [ Css.caret_color Current ]
  let caret_inherit = style [ Css.caret_color Inherit ]
  let caret_transparent = style [ Css.caret_color (Css.hex "#0000") ]

  (** Outline color utilities *)

  let outline' color shade =
    if is_custom_color color then
      let css_color = to_css color shade in
      style [ Css.outline_color css_color ]
    else
      let color_var = get_color_var color shade in
      let color_value = get_color_value color shade in
      let decl, color_ref = Var.binding color_var color_value in
      style (decl :: [ Css.outline_color (Var color_ref) ])

  let outline_current = style [ Css.outline_color Current ]
  let outline_inherit = style [ Css.outline_color Inherit ]

  (** Convert opacity modifier to a percentage value (0-100) *)
  let opacity_to_percent = function
    | No_opacity -> 100.0
    | Opacity_percent p -> p (* Already a percentage like 50 *)
    | Opacity_arbitrary f -> f *. 100.0 (* e.g., 0.5 -> 50 *)
    | Opacity_named _ ->
        (* Named opacity requires theme variable lookup, default to 100% *)
        100.0

  (** Condition for progressive enhancement with color-mix in oklab *)
  let color_mix_supports_condition =
    Css.Supports.Property ("color", "color-mix(in lab, red, red)")

  (** Generate color with opacity using progressive enhancement. Output depends
      on scheme:
      - With hex scheme: fallback is hex+alpha, [\@supports] has color-mix
      - With oklch scheme (default): fallback is color-mix(srgb), [\@supports]
        has color-mix(oklab) *)
  let color_with_opacity_style ~property c shade opacity =
    let percent = opacity_to_percent opacity in
    let scheme = !current_scheme in
    let color_name = scheme_color_name c shade in
    (* Check if color is defined as hex in the scheme *)
    match Scheme.get_hex_color scheme color_name with
    | Some hex_value ->
        (* Scheme has hex color: use hex+alpha fallback with top-level
           @supports *)
        let hex_with_alpha = hex_with_alpha hex_value percent in
        let fallback_decl = property (Css.hex hex_with_alpha) in
        (* Theme declaration for the variable *)
        let color_var = get_color_var c shade in
        let theme_decl, color_ref = Var.binding color_var (Css.hex hex_value) in
        (* Progressive enhancement: color-mix(in oklab, var(--color-X) NN%,
           transparent) *)
        let oklab_color =
          Css.color_mix ~in_space:Oklab (Css.Var color_ref) Css.Transparent
            ~percent1:percent
        in
        let oklab_decl = property oklab_color in
        (* Create @supports block with oklab version as top-level rule. Use
           placeholder selector that rules.ml replaces with actual class. *)
        let supports_block =
          Css.supports ~condition:color_mix_supports_condition
            [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
        in
        style ~rules:(Some [ supports_block ]) [ theme_decl; fallback_decl ]
    | None ->
        (* Default: use oklch and color-mix fallback *)
        let oklch = to_oklch c shade in
        (* Fallback: color-mix(in srgb, oklch(...) NN%, transparent) *)
        let fallback_color =
          Css.color_mix ~in_space:Srgb
            (Css.oklch oklch.l oklch.c oklch.h)
            Css.Transparent ~percent1:percent
        in
        let fallback_decl = property fallback_color in
        (* Progressive enhancement: color-mix(in oklab, var(--color-X) NN%,
           transparent) *)
        let color_var = get_color_var c shade in
        let color_value = to_css c (if is_base_color c then 500 else shade) in
        let theme_decl, color_ref = Var.binding color_var color_value in
        let oklab_color =
          Css.color_mix ~in_space:Oklab (Css.Var color_ref) Css.Transparent
            ~percent1:percent
        in
        let oklab_decl = property oklab_color in
        (* Create @supports block with oklab version as top-level rule. Use
           placeholder selector that rules.ml replaces with actual class. *)
        let supports_block =
          Css.supports ~condition:color_mix_supports_condition
            [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
        in
        style ~rules:(Some [ supports_block ]) [ theme_decl; fallback_decl ]

  (** Background color with opacity *)
  let bg_with_opacity c shade opacity =
    color_with_opacity_style ~property:Css.background_color c shade opacity

  (** Text color with opacity *)
  let text_with_opacity c shade opacity =
    color_with_opacity_style ~property:Css.color c shade opacity

  (** Border color with opacity *)
  let border_with_opacity c shade opacity =
    color_with_opacity_style ~property:Css.border_color c shade opacity

  (** Accent color with opacity *)
  let accent_with_opacity c shade opacity =
    color_with_opacity_style ~property:Css.accent_color c shade opacity

  (** Caret color with opacity *)
  let caret_with_opacity c shade opacity =
    color_with_opacity_style ~property:Css.caret_color c shade opacity

  (** Outline color with opacity *)
  let outline_with_opacity c shade opacity =
    color_with_opacity_style ~property:Css.outline_color c shade opacity

  (** Current color with opacity using color-mix with progressive enhancement *)
  let current_color_with_opacity ~property opacity =
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
       placeholder selector that rules.ml replaces with actual class. *)
    let supports_block =
      Css.supports ~condition:color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
    in
    style ~rules:(Some [ supports_block ]) [ fallback_decl ]

  let to_style = function
    | Bg (color, shade) -> bg' color shade
    | Bg_opacity (color, shade, opacity) -> bg_with_opacity color shade opacity
    | Bg_transparent -> bg_transparent
    | Bg_current -> bg_current
    | Bg_current_opacity opacity ->
        current_color_with_opacity ~property:Css.background_color opacity
    | Text (color, shade) -> text' color shade
    | Text_opacity (color, shade, opacity) ->
        text_with_opacity color shade opacity
    | Text_transparent -> text_transparent
    | Text_current -> text_current
    | Text_current_opacity opacity ->
        current_color_with_opacity ~property:Css.color opacity
    | Text_inherit -> text_inherit
    | Border (color, shade) -> border_color' color shade
    | Border_opacity (color, shade, opacity) ->
        border_with_opacity color shade opacity
    | Border_transparent -> border_transparent
    | Border_current -> border_current
    | Border_current_opacity opacity ->
        current_color_with_opacity ~property:Css.border_color opacity
    | Accent (color, shade) -> accent' color shade
    | Accent_opacity (color, shade, opacity) ->
        accent_with_opacity color shade opacity
    | Accent_current -> accent_current
    | Accent_current_opacity opacity ->
        current_color_with_opacity ~property:Css.accent_color opacity
    | Accent_inherit -> accent_inherit
    | Caret (color, shade) -> caret' color shade
    | Caret_opacity (color, shade, opacity) ->
        caret_with_opacity color shade opacity
    | Caret_current -> caret_current
    | Caret_current_opacity opacity ->
        current_color_with_opacity ~property:Css.caret_color opacity
    | Caret_inherit -> caret_inherit
    | Caret_transparent -> caret_transparent
    | Outline (color, shade) -> outline' color shade
    | Outline_opacity (color, shade, opacity) ->
        outline_with_opacity color shade opacity
    | Outline_current -> outline_current
    | Outline_current_opacity opacity ->
        current_color_with_opacity ~property:Css.outline_color opacity
    | Outline_inherit -> outline_inherit

  (* Suborder determines order within the color priority group. Tailwind orders:
     border -> bg -> text So we use: border (0-9999), bg (10000-19999), text
     (20000-29999) NOTE: Bg must be first pattern to infer local type t vs
     shadowed Css.Border *)
  let suborder = function
    | Bg (color, shade) ->
        (* All background colors use the same suborder (10000) to allow
           alphabetical sorting, matching Tailwind v4 behavior. *)
        let _ = (color, shade) in
        10000
    | Bg_opacity (color, shade, _) ->
        let _ = (color, shade) in
        10000
    | Bg_transparent -> 10000
    | Bg_current -> 10000
    | Bg_current_opacity _ -> 10000
    | Text (color, shade) ->
        (* All text colors use the same suborder (20000) to allow alphabetical
           sorting, matching Tailwind v4 behavior. *)
        let _ = (color, shade) in
        20000
    | Text_opacity (color, shade, _) ->
        let _ = (color, shade) in
        20000
    | Text_transparent -> 20000
    | Text_current -> 20000
    | Text_current_opacity _ -> 20000
    | Text_inherit -> 20000
    | Border (color, shade) ->
        (* All border colors use the same suborder (0) to allow alphabetical
           sorting, matching Tailwind v4 behavior. *)
        let _ = (color, shade) in
        0
    | Border_opacity (color, shade, _) ->
        let _ = (color, shade) in
        0
    | Border_transparent -> 0
    | Border_current -> 0
    | Border_current_opacity _ -> 0
    | Accent (color, shade) ->
        let base =
          if is_base_color color then
            suborder_with_shade (color_to_string color)
          else
            suborder_with_shade
              (color_to_string color ^ "-" ^ string_of_int shade)
        in
        (* Accent comes after ALL text colors. Since text uses 20000 +
           color*1000 and colors go up to ~25, max text suborder is ~45000. Use
           50000 base to ensure accent always comes after text regardless of
           color. *)
        50000 + base
    | Accent_opacity (color, shade, _) ->
        let base =
          if is_base_color color then
            suborder_with_shade (color_to_string color)
          else
            suborder_with_shade
              (color_to_string color ^ "-" ^ string_of_int shade)
        in
        50000 + base
    | Accent_current -> 50000
    | Accent_current_opacity _ -> 50000
    | Accent_inherit -> 50000
    (* Caret comes after accent. Alphabetical: current, inherit, [colors],
       transparent We use: - current: 60000 (c comes before colors, except
       blue=60003) - inherit: 60000 + 9*1000 = 69000 (i comes after h, before
       l=lime) - colors: 60000 + color_order * 1000 + shade (blue=3*1000,
       red=15*1000) - transparent: 60000 + 25*1000 = 85000 (t comes after all
       colors) Actually simpler: just use character-based ordering for special
       keywords *)
    | Caret (color, shade) ->
        let base =
          if is_base_color color then
            suborder_with_shade (color_to_string color)
          else
            suborder_with_shade
              (color_to_string color ^ "-" ^ string_of_int shade)
        in
        60000 + base
    | Caret_opacity (color, shade, _) ->
        let base =
          if is_base_color color then
            suborder_with_shade (color_to_string color)
          else
            suborder_with_shade
              (color_to_string color ^ "-" ^ string_of_int shade)
        in
        60000 + base
    | Caret_current ->
        60000 + (4 * 1000) (* c -> between cyan(4) and emerald(5) *)
    | Caret_current_opacity _ -> 60000 + (4 * 1000)
    | Caret_inherit ->
        60000 + (9 * 1000) (* i -> between indigo(9) and lime(10) *)
    | Caret_transparent -> 60000 + (25 * 1000)
    (* t -> after all colors (max=24) *)
    (* Outline comes after caret. Use 70000 base. *)
    | Outline (color, shade) ->
        let base =
          if is_base_color color then
            suborder_with_shade (color_to_string color)
          else
            suborder_with_shade
              (color_to_string color ^ "-" ^ string_of_int shade)
        in
        70000 + base
    | Outline_opacity (color, shade, _) ->
        let base =
          if is_base_color color then
            suborder_with_shade (color_to_string color)
          else
            suborder_with_shade
              (color_to_string color ^ "-" ^ string_of_int shade)
        in
        70000 + base
    | Outline_current ->
        70000 + (4 * 1000) (* c -> between cyan(4) and emerald(5) *)
    | Outline_current_opacity _ -> 70000 + (4 * 1000)
    | Outline_inherit -> 70000 + (9 * 1000)
  (* i -> between indigo(9) and lime(10) *)

  (* Format opacity modifier for class names *)
  let opacity_suffix = function
    | No_opacity -> ""
    | Opacity_percent p ->
        if Float.is_integer p then Printf.sprintf "/%d" (int_of_float p)
        else Printf.sprintf "/%g" p
    | Opacity_arbitrary f -> Printf.sprintf "/[%g]" f
    | Opacity_named name -> "/" ^ name

  let to_class = function
    | Bg (c, shade) ->
        if is_base_color c || is_custom_color c then "bg-" ^ color_to_string c
        else "bg-" ^ color_to_string c ^ "-" ^ string_of_int shade
    | Bg_opacity (c, shade, opacity) ->
        if is_base_color c || is_custom_color c then
          "bg-" ^ color_to_string c ^ opacity_suffix opacity
        else
          "bg-" ^ color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Bg_transparent -> "bg-transparent"
    | Bg_current -> "bg-current"
    | Bg_current_opacity opacity -> "bg-current" ^ opacity_suffix opacity
    | Text (c, shade) ->
        if is_base_color c || is_custom_color c then "text-" ^ color_to_string c
        else "text-" ^ color_to_string c ^ "-" ^ string_of_int shade
    | Text_opacity (c, shade, opacity) ->
        if is_base_color c || is_custom_color c then
          "text-" ^ color_to_string c ^ opacity_suffix opacity
        else
          "text-" ^ color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Text_transparent -> "text-transparent"
    | Text_current -> "text-current"
    | Text_current_opacity opacity -> "text-current" ^ opacity_suffix opacity
    | Text_inherit -> "text-inherit"
    | Border (c, shade) ->
        if is_base_color c || is_custom_color c then
          "border-" ^ color_to_string c
        else "border-" ^ color_to_string c ^ "-" ^ string_of_int shade
    | Border_opacity (c, shade, opacity) ->
        if is_base_color c || is_custom_color c then
          "border-" ^ color_to_string c ^ opacity_suffix opacity
        else
          "border-" ^ color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Border_transparent -> "border-transparent"
    | Border_current -> "border-current"
    | Border_current_opacity opacity ->
        "border-current" ^ opacity_suffix opacity
    | Accent (c, shade) ->
        if is_base_color c || is_custom_color c then
          "accent-" ^ color_to_string c
        else "accent-" ^ color_to_string c ^ "-" ^ string_of_int shade
    | Accent_opacity (c, shade, opacity) ->
        if is_base_color c || is_custom_color c then
          "accent-" ^ color_to_string c ^ opacity_suffix opacity
        else
          "accent-" ^ color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Accent_current -> "accent-current"
    | Accent_current_opacity opacity ->
        "accent-current" ^ opacity_suffix opacity
    | Accent_inherit -> "accent-inherit"
    | Caret (c, shade) ->
        if is_base_color c || is_custom_color c then
          "caret-" ^ color_to_string c
        else "caret-" ^ color_to_string c ^ "-" ^ string_of_int shade
    | Caret_opacity (c, shade, opacity) ->
        if is_base_color c || is_custom_color c then
          "caret-" ^ color_to_string c ^ opacity_suffix opacity
        else
          "caret-" ^ color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Caret_current -> "caret-current"
    | Caret_current_opacity opacity -> "caret-current" ^ opacity_suffix opacity
    | Caret_inherit -> "caret-inherit"
    | Caret_transparent -> "caret-transparent"
    | Outline (c, shade) ->
        if is_base_color c || is_custom_color c then
          "outline-" ^ color_to_string c
        else "outline-" ^ color_to_string c ^ "-" ^ string_of_int shade
    | Outline_opacity (c, shade, opacity) ->
        if is_base_color c || is_custom_color c then
          "outline-" ^ color_to_string c ^ opacity_suffix opacity
        else
          "outline-" ^ color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Outline_current -> "outline-current"
    | Outline_current_opacity opacity ->
        "outline-current" ^ opacity_suffix opacity
    | Outline_inherit -> "outline-inherit"
end

open Handler

(** Register color handler with Utility system *)
let () = Utility.register (module Handler)

(** Re-export helper functions from Handler for use by other modules *)
let scheme_color_name = Handler.scheme_color_name

let opacity_to_percent = Handler.opacity_to_percent

let pp_opacity = function
  | No_opacity -> ""
  | Opacity_percent pct ->
      (* Handle both integer and decimal values *)
      let rounded = Float.round pct in
      if Float.equal pct rounded then string_of_int (int_of_float pct)
      else Printf.sprintf "%g" pct
  | Opacity_arbitrary f -> "[" ^ string_of_float f ^ "]"
  | Opacity_named name -> name

let get_current_scheme () = !Handler.current_scheme

let get_hex_alpha_color c shade opacity =
  let open Handler in
  let percent = opacity_to_percent opacity in
  let color_name = scheme_color_name c shade in
  match Scheme.get_hex_color !current_scheme color_name with
  | Some hex_value -> Some (hex_with_alpha hex_value percent)
  | None -> None

let color_mix_supports_condition = Handler.color_mix_supports_condition

(** {1 Color with Opacity Helpers}

    Generic helpers for scheme-aware color generation with progressive
    enhancement. These can be used by other modules (svg, divide) to avoid code
    duplication. *)

let generic_color_with_opacity ~property c shade opacity =
  let open Handler in
  let percent = opacity_to_percent opacity in
  let color_name = scheme_color_name c shade in
  match Scheme.get_hex_color !current_scheme color_name with
  | Some hex_value ->
      let hex_alpha = hex_with_alpha hex_value percent in
      let fallback_decl = property (Css.hex hex_alpha) in
      let oklab_color =
        Css.color_mix ~in_space:Oklab (Css.hex hex_value) Css.Transparent
          ~percent1:percent
      in
      let oklab_decl = property oklab_color in
      let supports_block =
        Css.supports ~condition:color_mix_supports_condition
          [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
      in
      Style.style ~rules:(Some [ supports_block ]) [ fallback_decl ]
  | None ->
      let oklch = to_oklch c shade in
      let fallback_color =
        Css.color_mix ~in_space:Srgb
          (Css.oklch oklch.l oklch.c oklch.h)
          Css.Transparent ~percent1:percent
      in
      let fallback_decl = property fallback_color in
      let color_var = get_color_var c shade in
      let color_value = to_css c (if is_base_color c then 500 else shade) in
      let theme_decl, color_ref = Var.binding color_var color_value in
      let oklab_color =
        Css.color_mix ~in_space:Oklab (Css.Var color_ref) Css.Transparent
          ~percent1:percent
      in
      let oklab_decl = property oklab_color in
      let supports_block =
        Css.supports ~condition:color_mix_supports_condition
          [
            Css.rule ~selector:(Css.Selector.class_ "_")
              [ theme_decl; oklab_decl ];
          ]
      in
      Style.style ~rules:(Some [ supports_block ]) [ fallback_decl ]

let generic_current_with_opacity ~property opacity =
  let open Handler in
  let percent = opacity_to_percent opacity in
  let fallback_color =
    Css.color_mix ~in_space:Srgb Css.Current Css.Transparent ~percent1:percent
  in
  let fallback_decl = property fallback_color in
  let oklab_color =
    Css.color_mix ~in_space:Oklab Css.Current Css.Transparent ~percent1:percent
  in
  let oklab_decl = property oklab_color in
  let supports_block =
    Css.supports ~condition:color_mix_supports_condition
      [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
  in
  Style.style ~rules:(Some [ supports_block ]) [ fallback_decl ]

(* Fill/stroke helpers for SVG utilities *)
let fill_with_opacity c shade opacity =
  generic_color_with_opacity
    ~property:(fun color -> Css.fill (Css.Color color))
    c shade opacity

let stroke_with_opacity c shade opacity =
  generic_color_with_opacity
    ~property:(fun color -> Css.stroke (Css.Color color))
    c shade opacity

let fill_current_with_opacity opacity =
  generic_current_with_opacity
    ~property:(fun color -> Css.fill (Css.Color color))
    opacity

let stroke_current_with_opacity opacity =
  generic_current_with_opacity
    ~property:(fun color -> Css.stroke (Css.Color color))
    opacity

(* Divide helpers with custom selector *)
let divide_with_opacity_selector ~selector c shade opacity =
  let open Handler in
  let percent = opacity_to_percent opacity in
  let color_name = scheme_color_name c shade in
  match Scheme.get_hex_color !current_scheme color_name with
  | Some hex_value ->
      let hex_alpha = hex_with_alpha hex_value percent in
      let fallback_rule =
        Css.rule ~selector [ Css.border_color (Css.hex hex_alpha) ]
      in
      let oklab_color =
        Css.color_mix ~in_space:Oklab (Css.hex hex_value) Css.Transparent
          ~percent1:percent
      in
      let supports_rule = Css.rule ~selector [ Css.border_color oklab_color ] in
      let supports_block =
        Css.supports ~condition:color_mix_supports_condition [ supports_rule ]
      in
      Style.style ~rules:(Some [ fallback_rule; supports_block ]) []
  | None ->
      let oklch = to_oklch c shade in
      let fallback_color =
        Css.color_mix ~in_space:Srgb
          (Css.oklch oklch.l oklch.c oklch.h)
          Css.Transparent ~percent1:percent
      in
      let fallback_rule =
        Css.rule ~selector [ Css.border_color fallback_color ]
      in
      let color_var = get_color_var c shade in
      let theme_decl, color_ref = Var.binding color_var (to_css c shade) in
      let oklab_color =
        Css.color_mix ~in_space:Oklab (Css.Var color_ref) Css.Transparent
          ~percent1:percent
      in
      let supports_rule =
        Css.rule ~selector [ theme_decl; Css.border_color oklab_color ]
      in
      let supports_block =
        Css.supports ~condition:color_mix_supports_condition [ supports_rule ]
      in
      Style.style ~rules:(Some [ fallback_rule; supports_block ]) []

let divide_with_opacity c shade opacity selector =
  divide_with_opacity_selector ~selector c shade opacity

let divide_current_with_opacity_selector ~selector opacity =
  let open Handler in
  let percent = opacity_to_percent opacity in
  let fallback_color =
    Css.color_mix ~in_space:Srgb Css.Current Css.Transparent ~percent1:percent
  in
  let fallback_rule = Css.rule ~selector [ Css.border_color fallback_color ] in
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

(** Public API *)
let utility x = Utility.base (Self x)

let bg color shade = utility (Bg (color, shade))
let text color shade = utility (Text (color, shade))
let border_color color shade = utility (Border (color, shade))
let bg_transparent = utility Bg_transparent
let bg_current = utility Bg_current
let text_transparent = utility Text_transparent
let text_current = utility Text_current
let text_inherit = utility Text_inherit
let border_transparent = utility Border_transparent
let border_current = utility Border_current
let accent color shade = utility (Accent (color, shade))
let accent_current = utility Accent_current
let accent_inherit = utility Accent_inherit
let caret color shade = utility (Caret (color, shade))
let caret_current = utility Caret_current
let caret_inherit = utility Caret_inherit
let caret_transparent = utility Caret_transparent

(* Convenient semantic wrappers for default 500 shade *)
let bg_black = bg black 500
let bg_white = bg white 500
let bg_gray = bg gray 500
let bg_slate = bg slate 500
let bg_zinc = bg zinc 500
let bg_neutral = bg neutral 500
let bg_stone = bg stone 500
let bg_red = bg red 500
let bg_orange = bg orange 500
let bg_amber = bg amber 500
let bg_yellow = bg yellow 500
let bg_lime = bg lime 500
let bg_green = bg green 500
let bg_emerald = bg emerald 500
let bg_teal = bg teal 500
let bg_cyan = bg cyan 500
let bg_sky = bg sky 500
let bg_blue = bg blue 500
let bg_indigo = bg indigo 500
let bg_violet = bg violet 500
let bg_purple = bg purple 500
let bg_fuchsia = bg fuchsia 500
let bg_pink = bg pink 500
let bg_rose = bg rose 500
let text_black = text black 500
let text_white = text white 500
let text_gray = text gray 500
let text_slate = text slate 500
let text_zinc = text zinc 500
let text_neutral = text neutral 500
let text_stone = text stone 500
let text_red = text red 500
let text_orange = text orange 500
let text_amber = text amber 500
let text_yellow = text yellow 500
let text_lime = text lime 500
let text_green = text green 500
let text_emerald = text emerald 500
let text_teal = text teal 500
let text_cyan = text cyan 500
let text_sky = text sky 500
let text_blue = text blue 500
let text_indigo = text indigo 500
let text_violet = text violet 500
let text_purple = text purple 500
let text_fuchsia = text fuchsia 500
let text_pink = text pink 500
let text_rose = text rose 500
let border_black = border_color black 500
let border_white = border_color white 500
let border_gray = border_color gray 500
let border_slate = border_color slate 500
let border_zinc = border_color zinc 500
let border_neutral = border_color neutral 500
let border_stone = border_color stone 500
let border_red = border_color red 500
let border_orange = border_color orange 500
let border_amber = border_color amber 500
let border_yellow = border_color yellow 500
let border_lime = border_color lime 500
let border_green = border_color green 500
let border_emerald = border_color emerald 500
let border_teal = border_color teal 500
let border_cyan = border_color cyan 500
let border_sky = border_color sky 500
let border_blue = border_color blue 500
let border_indigo = border_color indigo 500
let border_violet = border_color violet 500
let border_purple = border_color purple 500
let border_fuchsia = border_color fuchsia 500
let border_pink = border_color pink 500
let border_rose = border_color rose 500
