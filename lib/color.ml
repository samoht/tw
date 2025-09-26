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
  "#" ^ to_hex_byte rgb.r ^ to_hex_byte rgb.g ^ to_hex_byte rgb.b

(** Format OKLCH for CSS *)
let oklch_to_css oklch =
  let pp_oklch ctx oklch =
    Pp.string ctx "oklch(";
    Pp.float_n 1 ctx oklch.l;
    Pp.string ctx "% ";
    Pp.float_n 3 ctx oklch.c;
    Pp.string ctx " ";
    Pp.float_n 3 ctx oklch.h;
    Pp.string ctx ")"
  in
  Pp.to_string ~minify:false pp_oklch oklch

(** Convert hex color to OKLCH CSS string *)
let hex_to_oklch_css hex =
  match hex_to_rgb hex with
  | Some rgb -> oklch_to_css (rgb_to_oklch rgb)
  | None -> hex (* Fallback to original hex if parsing fails *)

module Tailwind = struct
  (* These are the actual OKLCH values used by Tailwind v4 *)
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
      (* For arbitrary hex colors, determine if # was present *)
      let has_hash = String.starts_with ~prefix:"#" hex in
      let hex_value =
        if has_hash then String.sub hex 1 (String.length hex - 1) else hex
      in
      (* Tailwind v4 arbitrary values: preserve original format *)
      Css.Hex { hash = false; value = hex_value }
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
        Pp.string ctx "[";
        Pp.string ctx h;
        Pp.string ctx "]"
      in
      Pp.to_string ~minify:false pp_hex h_stripped
  | Rgb { red; green; blue } ->
      let pp_rgb ctx (r, g, b) =
        Pp.string ctx "[rgb(";
        Pp.int ctx r;
        Pp.string ctx ",";
        Pp.int ctx g;
        Pp.string ctx ",";
        Pp.int ctx b;
        Pp.string ctx ")]"
      in
      Pp.to_string ~minify:false pp_rgb (red, green, blue)
  | Oklch oklch ->
      let pp_oklch ctx oklch =
        Pp.string ctx "[oklch(";
        Pp.float ctx oklch.l;
        Pp.string ctx "%,";
        Pp.float ctx oklch.c;
        Pp.string ctx ",";
        Pp.float ctx oklch.h;
        Pp.string ctx ")]"
      in
      Pp.to_string ~minify:false pp_oklch oklch

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
        Pp.string ctx "[";
        Pp.string ctx v;
        Pp.string ctx "]"
      in
      Pp.to_string ~minify:false pp_hex_val hex_value
  | Rgb { red; green; blue } ->
      let pp_rgb_val ctx (r, g, b) =
        Pp.string ctx "Rgb(";
        Pp.int ctx r;
        Pp.string ctx ",";
        Pp.int ctx g;
        Pp.string ctx ",";
        Pp.int ctx b;
        Pp.string ctx ")"
      in
      Pp.to_string ~minify:false pp_rgb_val (red, green, blue)
  | Oklch { l; c; h } ->
      let pp_oklch_val ctx (l, c, h) =
        Pp.string ctx "Oklch(";
        Pp.float ctx l;
        Pp.string ctx ",";
        Pp.float ctx c;
        Pp.string ctx ",";
        Pp.float ctx h;
        Pp.string ctx ")"
      in
      Pp.to_string ~minify:false pp_oklch_val (l, c, h)

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
    (* Basic colors come first *)
    ("transparent", 0);
    ("black", 1);
    ("white", 2);
    ("red", 3);
    ("orange", 4);
    ("amber", 5);
    ("yellow", 6);
    ("lime", 7);
    ("green", 8);
    ("emerald", 9);
    ("teal", 10);
    ("cyan", 11);
    ("sky", 12);
    ("blue", 13);
    ("indigo", 14);
    ("violet", 15);
    ("purple", 16);
    ("fuchsia", 17);
    ("pink", 18);
    ("rose", 19);
    ("slate", 20);
    ("gray", 21);
    ("zinc", 22);
    ("neutral", 23);
    ("stone", 24);
  ]

(* Utilities layer color ordering map for conflict resolution. *)
let utilities_color_order_map =
  [
    (* Basic colors come first *)
    ("transparent", 0);
    ("black", 1);
    ("white", 2);
    ("amber", 3);
    ("blue", 4);
    ("cyan", 5);
    ("emerald", 6);
    ("fuchsia", 7);
    ("gray", 8);
    ("green", 9);
    ("indigo", 10);
    ("lime", 11);
    ("neutral", 12);
    ("orange", 13);
    ("pink", 14);
    ("purple", 15);
    ("red", 16);
    ("rose", 17);
    ("sky", 18);
    ("slate", 19);
    ("stone", 20);
    ("teal", 21);
    ("violet", 22);
    ("yellow", 23);
    ("zinc", 24);
  ]

(* Get theme layer order for a color variable. Returns (priority=2, suborder)
   where 2 indicates these are theme layer variables. *)
let theme_order color_name =
  match List.assoc_opt color_name theme_color_order_map with
  | Some suborder -> (2, suborder) (* Priority 2 for theme layer variables *)
  | None -> (2, 100)
(* Unknown colors go last within theme layer *)

(* Get utilities layer order for conflict resolution. Returns (priority,
   suborder) for utilities layer ordering. *)
let utilities_order color_name =
  match List.assoc_opt color_name utilities_color_order_map with
  | Some suborder -> (2, suborder) (* Priority 2 for color utilities *)
  | None -> (2, 100)
(* Unknown colors go last *)

(* Get theme layer order for a color variable with shade. Formula: (priority=2,
   base_order * 1000 + shade) This ensures color variables are grouped by color
   with shades in ascending order. *)
let theme_order_with_shade color_name shade =
  let var_priority, base_order = theme_order color_name in
  (var_priority, (base_order * 1000) + shade)

(* Memoization table for color variables *)
let color_var_cache : (string, Css.color Var.theme) Hashtbl.t =
  Hashtbl.create 128

(* Helper to create a color variable with memoization. Creates theme layer
   variables with deterministic ordering based on color and shade. *)
let get_color_var color shade =
  let base = pp color in
  let name =
    if is_base_color color then "color-" ^ base
    else "color-" ^ base ^ "-" ^ string_of_int shade
  in
  (* Check if variable already exists in cache *)
  match Hashtbl.find_opt color_var_cache name with
  | Some var -> var
  | None ->
      (* Create theme variable with deterministic theme layer order: - Base
         colors use theme_order(color_name) - Shaded colors use
         theme_order_with_shade(color_name, shade) This ensures CSS custom
         properties appear in the correct order in the theme layer. *)
      let var_order =
        if is_base_color color then theme_order base
        else theme_order_with_shade base shade
      in
      let var = Var.theme Css.Color name ~order:var_order in
      Hashtbl.add color_var_cache name var;
      var

let bg color shade =
  let class_name =
    if is_base_color color || is_custom_color color then
      let pp_class_name ctx color =
        Pp.string ctx "bg-";
        Pp.string ctx (pp color)
      in
      Pp.to_string ~minify:false pp_class_name color
    else
      let pp_class_name ctx (color, shade) =
        Pp.string ctx "bg-";
        Pp.string ctx (pp color);
        Pp.string ctx "-";
        Pp.int ctx shade
      in
      Pp.to_string ~minify:false pp_class_name (color, shade)
  in
  if is_custom_color color then
    let css_color = to_css color shade in
    style class_name [ Css.background_color css_color ]
  else
    let color_var = get_color_var color shade in
    let color_value =
      to_css color (if is_base_color color then 500 else shade)
    in
    let decl, color_ref = Var.binding color_var color_value in
    style class_name (decl :: [ Css.background_color (Css.Var color_ref) ])

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
      let pp_class_name ctx color =
        Pp.string ctx "text-";
        Pp.string ctx (pp color)
      in
      Pp.to_string ~minify:false pp_class_name color
    else
      let pp_class_name ctx (color, shade) =
        Pp.string ctx "text-";
        Pp.string ctx (pp color);
        Pp.string ctx "-";
        Pp.int ctx shade
      in
      Pp.to_string ~minify:false pp_class_name (color, shade)
  in
  if is_custom_color color then
    let css_color = to_css color shade in
    style class_name [ Css.color css_color ]
  else
    let color_var = get_color_var color shade in
    let color_value =
      to_css color (if is_base_color color then 500 else shade)
    in
    let decl, color_ref = Var.binding color_var color_value in
    style class_name (decl :: [ Css.color (Var color_ref) ])

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
      let pp_class_name ctx color =
        Pp.string ctx "border-";
        Pp.string ctx (pp color)
      in
      Pp.to_string ~minify:false pp_class_name color
    else
      let pp_class_name ctx (color, shade) =
        Pp.string ctx "border-";
        Pp.string ctx (pp color);
        Pp.string ctx "-";
        Pp.int ctx shade
      in
      Pp.to_string ~minify:false pp_class_name (color, shade)
  in
  if is_custom_color color then
    let css_color = to_css color shade in
    style class_name [ Css.border_color css_color ]
  else
    let color_var = get_color_var color shade in
    let color_value =
      to_css color (if is_base_color color then 500 else shade)
    in
    let decl, color_ref = Var.binding color_var color_value in
    style class_name (decl :: [ Css.border_color (Var color_ref) ])

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

let classes_of_string parts =
  match parts with
  | [ "bg"; "transparent" ] -> Ok bg_transparent
  | [ "bg"; "current" ] -> Ok bg_current
  | "bg" :: color_parts -> (
      match shade_of_strings color_parts with
      | Ok (color, shade) -> Ok (bg color shade)
      | Error _ -> Error (`Msg "Not a background color"))
  | [ "text"; "transparent" ] -> Ok text_transparent
  | [ "text"; "current" ] -> Ok text_current
  | [ "text"; "inherit" ] -> Ok text_inherit
  | "text" :: color_parts -> (
      match shade_of_strings color_parts with
      | Ok (color, shade) -> Ok (text color shade)
      | Error _ -> Error (`Msg "Not a text color"))
  | [ "border"; "transparent" ] -> Ok border_transparent
  | [ "border"; "current" ] -> Ok border_current
  | "border" :: color_parts -> (
      match shade_of_strings color_parts with
      | Ok (color, shade) -> Ok (border_color color shade)
      | Error _ -> Error (`Msg "Not a border color"))
  | _ -> Error (`Msg "Not a color utility")

(* Backward-compatible alias *)
