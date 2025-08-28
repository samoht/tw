(** Color conversion utilities for Tailwind v4 compatibility *)

open Core
open Css

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

(** Convert RGB to linear RGB (remove gamma correction) *)
let linearize_channel c =
  let c' = float_of_int c /. 255.0 in
  if c' <= 0.04045 then c' /. 12.92 else ((c' +. 0.055) /. 1.055) ** 2.4

(** Convert linear RGB to gamma-corrected RGB *)
let gamma_correct c =
  let c' =
    if c <= 0.0031308 then c *. 12.92
    else (1.055 *. (c ** (1.0 /. 2.4))) -. 0.055
  in
  int_of_float ((c' *. 255.0) +. 0.5)

(** Convert RGB to OKLCH color space *)
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

(** Convert OKLCH to RGB color space *)
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

(** Parse hex color string to RGB *)
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

(** Convert RGB to hex string *)
let rgb_to_hex rgb =
  let to_hex_byte n =
    let hex = "0123456789abcdef" in
    String.make 1 hex.[n / 16] ^ String.make 1 hex.[n mod 16]
  in
  Pp.str [ "#"; to_hex_byte rgb.r; to_hex_byte rgb.g; to_hex_byte rgb.b ]

(** Format OKLCH for CSS *)
let oklch_to_css oklch =
  let l_str = Pp.float_n 1 oklch.l in
  let c_str = Pp.float_n 3 oklch.c in
  let h_str = Pp.float_n 3 oklch.h in
  Pp.str [ "oklch("; l_str; "% "; c_str; " "; h_str; ")" ]

(** Convert hex color to OKLCH CSS string *)
let hex_to_oklch_css hex =
  match hex_to_rgb hex with
  | Some rgb -> oklch_to_css (rgb_to_oklch rgb)
  | None -> hex (* Fallback to original hex if parsing fails *)

module Tailwind_colors = struct
  (* These are the actual OKLCH values used by Tailwind v4 *)
  let gray =
    [
      (50, { l = 98.5; c = 0.002; h = 247.839 });
      (100, { l = 96.7; c = 0.003; h = 264.542 });
      (200, { l = 92.8; c = 0.006; h = 264.531 });
      (300, { l = 87.2; c = 0.01; h = 258.338 });
      (400, { l = 65.1; c = 0.016; h = 264.436 });
      (500, { l = 55.1; c = 0.027; h = 264.364 });
      (600, { l = 44.6; c = 0.03; h = 256.802 });
      (700, { l = 37.3; c = 0.034; h = 259.733 });
      (800, { l = 27.8; c = 0.033; h = 256.848 });
      (900, { l = 21.0; c = 0.034; h = 264.665 });
      (950, { l = 13.1; c = 0.022; h = 264.436 });
    ]

  let blue =
    [
      (50, { l = 97.1; c = 0.014; h = 237.021 });
      (100, { l = 93.2; c = 0.032; h = 255.585 });
      (200, { l = 86.8; c = 0.061; h = 251.813 });
      (300, { l = 80.9; c = 0.105; h = 251.813 });
      (400, { l = 70.7; c = 0.165; h = 254.624 });
      (500, { l = 62.3; c = 0.214; h = 259.815 });
      (600, { l = 54.6; c = 0.245; h = 262.881 });
      (700, { l = 48.8; c = 0.243; h = 264.376 });
      (800, { l = 40.1; c = 0.19; h = 264.828 });
      (900, { l = 37.9; c = 0.146; h = 265.522 });
      (950, { l = 26.5; c = 0.136; h = 269.055 });
    ]

  let red =
    [
      (50, { l = 95.8; c = 0.019; h = 17.331 });
      (100, { l = 91.9; c = 0.037; h = 17.175 });
      (200, { l = 84.4; c = 0.073; h = 19.692 });
      (300, { l = 75.8; c = 0.133; h = 21.276 });
      (400, { l = 68.8; c = 0.191; h = 24.342 });
      (500, { l = 63.7; c = 0.237; h = 25.331 });
      (600, { l = 57.7; c = 0.245; h = 27.325 });
      (700, { l = 51.4; c = 0.204; h = 27.245 });
      (800, { l = 44.4; c = 0.177; h = 26.899 });
      (900, { l = 40.5; c = 0.147; h = 28.067 });
      (950, { l = 26.9; c = 0.097; h = 28.428 });
    ]

  let slate =
    [
      (50, { l = 98.4; c = 0.003; h = 247.858 });
      (100, { l = 96.9; c = 0.005; h = 264.542 });
      (200, { l = 93.1; c = 0.01; h = 264.531 });
      (300, { l = 87.6; c = 0.017; h = 258.338 });
      (400, { l = 71.3; c = 0.028; h = 256.802 });
      (500, { l = 55.4; c = 0.046; h = 257.417 });
      (600, { l = 46.5; c = 0.043; h = 257.362 });
      (700, { l = 39.3; c = 0.045; h = 256.848 });
      (800, { l = 29.1; c = 0.041; h = 257.284 });
      (900, { l = 20.4; c = 0.025; h = 264.665 });
      (950, { l = 13.2; c = 0.017; h = 264.436 });
    ]

  let zinc =
    [
      (50, { l = 98.5; c = 0.002; h = 247.839 });
      (100, { l = 96.5; c = 0.003; h = 264.542 });
      (200, { l = 92.5; c = 0.004; h = 264.531 });
      (300, { l = 86.9; c = 0.007; h = 258.338 });
      (400, { l = 69.8; c = 0.01; h = 264.436 });
      (500, { l = 55.2; c = 0.016; h = 285.938 });
      (600, { l = 45.2; c = 0.016; h = 256.802 });
      (700, { l = 38.4; c = 0.017; h = 264.364 });
      (800, { l = 28.1; c = 0.015; h = 256.848 });
      (900, { l = 20.8; c = 0.011; h = 264.665 });
      (950, { l = 13.1; c = 0.007; h = 264.436 });
    ]

  let neutral =
    [
      (50, { l = 98.5; c = 0.0; h = 0.0 });
      (100, { l = 96.5; c = 0.0; h = 0.0 });
      (200, { l = 92.8; c = 0.0; h = 0.0 });
      (300, { l = 87.3; c = 0.0; h = 0.0 });
      (400, { l = 70.4; c = 0.0; h = 0.0 });
      (500, { l = 55.6; c = 0.0; h = 0.0 });
      (600, { l = 45.7; c = 0.0; h = 0.0 });
      (700, { l = 38.5; c = 0.0; h = 0.0 });
      (800, { l = 28.3; c = 0.0; h = 0.0 });
      (900, { l = 21.1; c = 0.0; h = 0.0 });
      (950, { l = 13.4; c = 0.0; h = 0.0 });
    ]

  let stone =
    [
      (50, { l = 98.5; c = 0.002; h = 106.424 });
      (100, { l = 96.4; c = 0.003; h = 106.424 });
      (200, { l = 92.5; c = 0.005; h = 106.424 });
      (300, { l = 87.1; c = 0.008; h = 106.424 });
      (400, { l = 70.1; c = 0.01; h = 106.424 });
      (500, { l = 55.3; c = 0.013; h = 58.071 });
      (600, { l = 45.4; c = 0.011; h = 106.424 });
      (700, { l = 38.8; c = 0.01; h = 106.424 });
      (800, { l = 28.2; c = 0.007; h = 106.424 });
      (900, { l = 21.3; c = 0.004; h = 106.424 });
      (950, { l = 13.1; c = 0.002; h = 106.424 });
    ]

  let orange =
    [
      (50, { l = 97.8; c = 0.015; h = 73.368 });
      (100, { l = 95.5; c = 0.03; h = 73.684 });
      (200, { l = 90.5; c = 0.063; h = 73.639 });
      (300, { l = 84.1; c = 0.115; h = 69.638 });
      (400, { l = 77.0; c = 0.17; h = 62.843 });
      (500, { l = 70.5; c = 0.213; h = 47.604 });
      (* Updated to match Tailwind v4 *)
      (600, { l = 65.5; c = 0.176; h = 41.239 });
      (700, { l = 54.8; c = 0.147; h = 39.021 });
      (800, { l = 45.8; c = 0.12; h = 38.172 });
      (900, { l = 40.8; c = 0.123; h = 38.172 });
      (950, { l = 25.8; c = 0.067; h = 38.766 });
    ]

  let amber =
    [
      (50, { l = 98.7; c = 0.016; h = 95.277 });
      (100, { l = 96.2; c = 0.059; h = 95.617 });
      (200, { l = 92.8; c = 0.089; h = 95.334 });
      (300, { l = 87.5; c = 0.15; h = 94.634 });
      (400, { l = 82.3; c = 0.2; h = 91.605 });
      (500, { l = 76.9; c = 0.188; h = 70.08 });
      (* Updated to match Tailwind v4 *)
      (600, { l = 69.7; c = 0.17; h = 79.287 });
      (700, { l = 59.4; c = 0.147; h = 73.483 });
      (800, { l = 49.5; c = 0.122; h = 69.28 });
      (900, { l = 41.5; c = 0.102; h = 66.343 });
      (950, { l = 28.2; c = 0.071; h = 62.428 });
    ]

  let yellow =
    [
      (50, { l = 98.8; c = 0.019; h = 96.68 });
      (100, { l = 97.6; c = 0.046; h = 97.138 });
      (200, { l = 94.5; c = 0.129; h = 101.54 });
      (300, { l = 90.5; c = 0.175; h = 96.723 });
      (400, { l = 85.2; c = 0.199; h = 91.936 });
      (500, { l = 79.5; c = 0.184; h = 86.047 });
      (600, { l = 72.7; c = 0.169; h = 87.334 });
      (700, { l = 61.7; c = 0.142; h = 83.453 });
      (800, { l = 51.5; c = 0.118; h = 79.483 });
      (900, { l = 43.2; c = 0.099; h = 75.3 });
      (950, { l = 28.5; c = 0.066; h = 71.709 });
    ]

  let lime =
    [
      (50, { l = 97.9; c = 0.02; h = 118.604 });
      (100, { l = 95.5; c = 0.05; h = 119.552 });
      (200, { l = 90.8; c = 0.117; h = 119.772 });
      (300, { l = 85.1; c = 0.192; h = 120.298 });
      (400, { l = 77.9; c = 0.235; h = 121.721 });
      (500, { l = 76.8; c = 0.233; h = 130.85 });
      (600, { l = 64.8; c = 0.2; h = 131.684 });
      (700, { l = 50.7; c = 0.145; h = 125.521 });
      (800, { l = 41.7; c = 0.115; h = 125.366 });
      (900, { l = 36.3; c = 0.094; h = 125.638 });
      (950, { l = 24.3; c = 0.064; h = 125.808 });
    ]

  let green =
    [
      (50, { l = 97.6; c = 0.017; h = 155.826 });
      (100, { l = 94.5; c = 0.04; h = 154.449 });
      (200, { l = 88.8; c = 0.089; h = 153.713 });
      (300, { l = 81.2; c = 0.152; h = 151.363 });
      (400, { l = 71.2; c = 0.209; h = 149.579 });
      (500, { l = 72.3; c = 0.219; h = 149.579 });
      (600, { l = 54.2; c = 0.157; h = 149.579 });
      (700, { l = 46.7; c = 0.127; h = 150.069 });
      (800, { l = 38.8; c = 0.101; h = 150.592 });
      (900, { l = 33.6; c = 0.082; h = 151.566 });
      (950, { l = 22.1; c = 0.055; h = 152.535 });
    ]

  let emerald =
    [
      (50, { l = 96.9; c = 0.021; h = 167.214 });
      (100, { l = 93.2; c = 0.051; h = 166.113 });
      (200, { l = 86.2; c = 0.111; h = 164.15 });
      (300, { l = 84.5; c = 0.143; h = 164.978 });
      (400, { l = 67.3; c = 0.223; h = 160.804 });
      (500, { l = 69.6; c = 0.17; h = 162.48 });
      (600, { l = 50.2; c = 0.163; h = 160.804 });
      (700, { l = 43.5; c = 0.132; h = 162.567 });
      (800, { l = 36.6; c = 0.105; h = 163.227 });
      (900, { l = 31.6; c = 0.085; h = 164.665 });
      (950, { l = 20.8; c = 0.056; h = 166.113 });
    ]

  let teal =
    [
      (50, { l = 97.5; c = 0.014; h = 182.513 });
      (100, { l = 94.2; c = 0.037; h = 180.584 });
      (200, { l = 88.5; c = 0.084; h = 180.726 });
      (300, { l = 80.7; c = 0.141; h = 181.181 });
      (400, { l = 70.5; c = 0.17; h = 182.085 });
      (500, { l = 70.4; c = 0.14; h = 182.503 });
      (600, { l = 60.0; c = 0.118; h = 184.704 });
      (700, { l = 44.3; c = 0.105; h = 185.802 });
      (800, { l = 37.5; c = 0.084; h = 186.735 });
      (900, { l = 32.9; c = 0.068; h = 187.915 });
      (950, { l = 22.5; c = 0.047; h = 188.251 });
    ]

  let cyan =
    [
      (50, { l = 96.9; c = 0.017; h = 200.119 });
      (100, { l = 93.7; c = 0.042; h = 199.956 });
      (200, { l = 87.2; c = 0.093; h = 200.065 });
      (300, { l = 79.2; c = 0.154; h = 200.651 });
      (400, { l = 68.9; c = 0.182; h = 202.267 });
      (500, { l = 71.5; c = 0.143; h = 215.221 });
      (600, { l = 50.9; c = 0.135; h = 205.765 });
      (700, { l = 52.0; c = 0.105; h = 223.128 });
      (800, { l = 37.9; c = 0.089; h = 209.135 });
      (900, { l = 33.5; c = 0.072; h = 210.732 });
      (950, { l = 23.5; c = 0.051; h = 211.682 });
    ]

  let sky =
    [
      (50, { l = 97.5; c = 0.013; h = 214.57 });
      (100, { l = 94.1; c = 0.031; h = 214.334 });
      (200, { l = 88.2; c = 0.071; h = 213.337 });
      (300, { l = 80.9; c = 0.127; h = 213.292 });
      (400, { l = 71.2; c = 0.184; h = 213.524 });
      (500, { l = 68.5; c = 0.169; h = 237.323 });
      (600, { l = 58.8; c = 0.158; h = 241.966 });
      (700, { l = 48.2; c = 0.142; h = 221.723 });
      (800, { l = 41.8; c = 0.116; h = 224.283 });
      (900, { l = 36.9; c = 0.095; h = 225.937 });
      (950, { l = 25.7; c = 0.066; h = 228.06 });
    ]

  let indigo =
    [
      (50, { l = 96.4; c = 0.015; h = 272.314 });
      (100, { l = 93.1; c = 0.034; h = 272.937 });
      (200, { l = 86.8; c = 0.069; h = 274.039 });
      (300, { l = 79.4; c = 0.116; h = 274.713 });
      (400, { l = 70.5; c = 0.165; h = 276.935 });
      (500, { l = 58.5; c = 0.233; h = 277.117 });
      (600, { l = 53.4; c = 0.19; h = 285.786 });
      (700, { l = 47.5; c = 0.169; h = 287.163 });
      (800, { l = 40.5; c = 0.138; h = 287.999 });
      (900, { l = 35.9; c = 0.111; h = 288.494 });
      (950, { l = 24.1; c = 0.074; h = 288.846 });
    ]

  let violet =
    [
      (50, { l = 96.5; c = 0.016; h = 293.756 });
      (100, { l = 93.5; c = 0.035; h = 294.588 });
      (200, { l = 87.8; c = 0.073; h = 295.112 });
      (300, { l = 80.7; c = 0.127; h = 295.484 });
      (400, { l = 70.2; c = 0.183; h = 293.541 });
      (500, { l = 60.6; c = 0.25; h = 292.717 });
      (600, { l = 56.9; c = 0.216; h = 301.685 });
      (700, { l = 51.2; c = 0.19; h = 303.418 });
      (800, { l = 43.5; c = 0.157; h = 304.252 });
      (900, { l = 38.3; c = 0.127; h = 304.935 });
      (950, { l = 26.8; c = 0.088; h = 305.612 });
    ]

  let purple =
    [
      (50, { l = 98.2; c = 0.01; h = 314.533 });
      (100, { l = 95.8; c = 0.024; h = 315.433 });
      (200, { l = 91.3; c = 0.052; h = 315.802 });
      (300, { l = 84.7; c = 0.093; h = 315.921 });
      (400, { l = 76.5; c = 0.144; h = 316.192 });
      (500, { l = 62.7; c = 0.265; h = 303.9 });
      (600, { l = 61.2; c = 0.187; h = 317.705 });
      (700, { l = 53.6; c = 0.164; h = 318.736 });
      (800, { l = 44.6; c = 0.135; h = 319.397 });
      (900, { l = 37.5; c = 0.11; h = 319.968 });
      (950, { l = 26.9; c = 0.079; h = 320.38 });
    ]

  let fuchsia =
    [
      (50, { l = 98.2; c = 0.012; h = 328.363 });
      (100, { l = 95.9; c = 0.029; h = 328.734 });
      (200, { l = 91.7; c = 0.064; h = 328.704 });
      (300, { l = 85.5; c = 0.117; h = 328.717 });
      (400, { l = 77.8; c = 0.184; h = 329.146 });
      (500, { l = 66.7; c = 0.295; h = 322.15 });
      (600, { l = 62.2; c = 0.223; h = 330.771 });
      (700, { l = 53.9; c = 0.194; h = 331.642 });
      (800, { l = 45.2; c = 0.211; h = 324.591 });
      (900, { l = 38.7; c = 0.134; h = 332.791 });
      (950, { l = 27.5; c = 0.095; h = 333.137 });
    ]

  let pink =
    [
      (50, { l = 97.5; c = 0.014; h = 343.195 });
      (100, { l = 95.2; c = 0.03; h = 343.166 });
      (200, { l = 90.2; c = 0.063; h = 343.394 });
      (300, { l = 83.6; c = 0.115; h = 344.056 });
      (400, { l = 74.6; c = 0.18; h = 346.054 });
      (500, { l = 65.6; c = 0.241; h = 354.308 });
      (600, { l = 58.5; c = 0.225; h = 354.112 });
      (700, { l = 50.5; c = 0.195; h = 357.196 });
      (800, { l = 42.5; c = 0.163; h = 358.892 });
      (900, { l = 37.1; c = 0.135; h = 359.732 });
      (950, { l = 25.8; c = 0.093; h = 0.223 });
    ]

  let rose =
    [
      (50, { l = 97.8; c = 0.01; h = 14.096 });
      (100, { l = 95.5; c = 0.024; h = 13.219 });
      (200, { l = 89.2; c = 0.058; h = 10.001 });
      (300, { l = 84.7; c = 0.104; h = 10.449 });
      (400, { l = 76.7; c = 0.169; h = 8.861 });
      (500, { l = 64.5; c = 0.246; h = 16.439 });
      (600, { l = 59.5; c = 0.217; h = 5.342 });
      (700, { l = 51.5; c = 0.189; h = 3.813 });
      (800, { l = 43.2; c = 0.159; h = 2.599 });
      (900, { l = 38.1; c = 0.132; h = 1.829 });
      (950, { l = 26.9; c = 0.093; h = 1.145 });
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
  (* Check if the string is actually an RGB value like "rgb(29,161,242)" *)
  if String.starts_with ~prefix:"rgb(" s && String.ends_with ~suffix:")" s then
    (* Parse RGB string and convert to hex *)
    try
      let inner = String.sub s 4 (String.length s - 5) in
      (* Remove "rgb(" and ")" *)
      let parts = String.split_on_char ',' inner |> List.map String.trim in
      match parts with
      | [ r_str; g_str; b_str ] ->
          let r = int_of_string r_str in
          let g = int_of_string g_str in
          let b = int_of_string b_str in
          (* Convert RGB to hex *)
          let to_hex_char n =
            let c = n mod 16 in
            if c < 10 then Char.chr (c + 48) else Char.chr (c + 87)
          in
          let hex_of_int n =
            String.make 1 (to_hex_char (n / 16)) ^ String.make 1 (to_hex_char n)
          in
          let hex_str = "#" ^ hex_of_int r ^ hex_of_int g ^ hex_of_int b in
          Hex hex_str
      | _ -> Hex s (* Fallback if parsing fails *)
    with Invalid_argument _ | Failure _ ->
      Hex s (* Fallback if parsing fails *)
  else Hex s

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
    invalid_arg
      (Pp.str [ "RGB red value "; string_of_int r; " out of range [0-255]" ]);
  if g < 0 || g > 255 then
    invalid_arg
      (Pp.str [ "RGB green value "; string_of_int g; " out of range [0-255]" ]);
  if b < 0 || b > 255 then
    invalid_arg
      (Pp.str [ "RGB blue value "; string_of_int b; " out of range [0-255]" ]);
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
      (* For named colors, get OKLCH data directly from Tailwind_colors *)
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
      match Tailwind_colors.get_color_oklch color_name shade with
      | Some oklch -> oklch
      | None ->
          failwith
            (Pp.str
               [
                 "No OKLCH data for color ";
                 color_name;
                 " shade ";
                 string_of_int shade;
               ]))

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
      (* For named colors, get from Tailwind_colors *)
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
      match Tailwind_colors.get_color color_name shade with
      | Some value -> value
      | None -> "oklch(0% 0 0)" (* Fallback *))

(* Convert color to CSS color value *)
let to_css color shade =
  match color with
  | Black -> Css.Hex { hash = true; value = "000" }
  | White -> Css.Hex { hash = true; value = "fff" }
  | Hex hex ->
      (* For arbitrary hex colors, determine if # was present *)
      let has_hash = String.starts_with ~prefix:"#" hex in
      let hex_value =
        if has_hash then String.sub hex 1 (String.length hex - 1) else hex
      in
      (* Tailwind v4 arbitrary values: preserve original format *)
      Css.Hex { hash = false; value = hex_value }
  | Oklch oklch ->
      (* Use the new Oklch constructor *)
      Css.Oklch { l = oklch.l; c = oklch.c; h = oklch.h }
  | _ ->
      (* For other colors, get OKLCH data directly *)
      let oklch = to_oklch color shade in
      Css.Oklch { l = oklch.l; c = oklch.c; h = oklch.h }

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
      Pp.str [ "["; h_stripped; "]" ]
  | Rgb { red; green; blue } ->
      Pp.str
        [
          "[rgb(";
          string_of_int red;
          ",";
          string_of_int green;
          ",";
          string_of_int blue;
          ")]";
        ]
  | Oklch oklch ->
      Pp.str
        [
          "[oklch(";
          string_of_float oklch.l;
          "%,";
          string_of_float oklch.c;
          ",";
          string_of_float oklch.h;
          ")]";
        ]

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
  | Hex s -> Pp.str [ "Hex("; s; ")" ]
  | Rgb { red; green; blue } ->
      Pp.str
        [
          "Rgb(";
          string_of_int red;
          ",";
          string_of_int green;
          ",";
          string_of_int blue;
          ")";
        ]
  | Oklch { l; c; h } ->
      Pp.str
        [
          "Oklch(";
          string_of_float l;
          ",";
          string_of_float c;
          ",";
          string_of_float h;
          ")";
        ]

(* Check if a color is black or white *)
let is_base_color = function Black | White -> true | _ -> false

(* Check if a color is a custom color (hex, rgb, or oklch) *)
let is_custom_color = function Hex _ | Rgb _ | Oklch _ -> true | _ -> false

(** {1 Color Application Utilities} *)

(** Background color utilities *)

(* Helper to create a color variable with proper tracking *)
let get_color_var color shade =
  let default_color =
    to_css color (if is_base_color color then 500 else shade)
  in
  Var.theme
    (Var.Color (pp color, if is_base_color color then None else Some shade))
    default_color

let bg color shade =
  let class_name =
    if is_base_color color || is_custom_color color then
      Pp.str [ "bg-"; pp color ]
    else Pp.str [ "bg-"; pp color; "-"; string_of_int shade ]
  in
  if is_custom_color color then
    let css_color = to_css color shade in
    style class_name [ Css.background_color css_color ]
  else
    let var_def, css_var = get_color_var color shade in
    style class_name [ var_def; Css.background_color (Css.Var css_var) ]

let bg_transparent = style "bg-transparent" [ background_color Transparent ]
let bg_current = style "bg-current" [ background_color Current ]

(* Default color backgrounds - using shade 500 *)
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

(** Text color utilities *)

let text color shade =
  let class_name =
    if is_base_color color || is_custom_color color then
      Pp.str [ "text-"; pp color ]
    else Pp.str [ "text-"; pp color; "-"; string_of_int shade ]
  in
  if is_custom_color color then
    let css_color = to_css color shade in
    style class_name [ Css.color css_color ]
  else
    let var_def, css_var = get_color_var color shade in
    style class_name [ var_def; Css.color (Css.Var css_var) ]

let text_transparent = style "text-transparent" [ Css.color Transparent ]
let text_current = style "text-current" [ Css.color Current ]
let text_inherit = style "text-inherit" [ Css.color Inherit ]

(* Default text colors - using shade 500 *)
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

(** Border color utilities *)

let border_color color shade =
  let class_name =
    if is_base_color color || is_custom_color color then
      Pp.str [ "border-"; pp color ]
    else Pp.str [ "border-"; pp color; "-"; string_of_int shade ]
  in
  if is_custom_color color then
    let css_color = to_css color shade in
    style class_name [ Css.border_color css_color ]
  else
    let var_def, css_var = get_color_var color shade in
    style class_name [ var_def; Css.border_color (Css.Var css_var) ]

let border_transparent =
  style "border-transparent" [ Css.border_color Transparent ]

let border_current = style "border-current" [ Css.border_color Current ]

(* Default border colors - using shade 500 *)
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

(** Color parsing utilities *)

(* Parse color and shade from string list *)
let parse_color_with_shade = function
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

let classes_of_string parts =
  match parts with
  | [ "bg"; "transparent" ] -> Ok bg_transparent
  | [ "bg"; "current" ] -> Ok bg_current
  | "bg" :: color_parts -> (
      match parse_color_with_shade color_parts with
      | Ok (color, shade) -> Ok (bg color shade)
      | Error _ -> Error (`Msg "Not a background color"))
  | [ "text"; "transparent" ] -> Ok text_transparent
  | [ "text"; "current" ] -> Ok text_current
  | [ "text"; "inherit" ] -> Ok text_inherit
  | "text" :: color_parts -> (
      match parse_color_with_shade color_parts with
      | Ok (color, shade) -> Ok (text color shade)
      | Error _ -> Error (`Msg "Not a text color"))
  | [ "border"; "transparent" ] -> Ok border_transparent
  | [ "border"; "current" ] -> Ok border_current
  | "border" :: color_parts -> (
      match parse_color_with_shade color_parts with
      | Ok (color, shade) -> Ok (border_color color shade)
      | Error _ -> Error (`Msg "Not a border color"))
  | _ -> Error (`Msg "Not a color utility")
