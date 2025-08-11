(** Color conversion utilities for Tailwind v4 compatibility *)

(** Abstract color type matching Tw.color *)
type t =
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

type rgb = {
  r : int;  (** Red channel (0-255) *)
  g : int;  (** Green channel (0-255) *)
  b : int;  (** Blue channel (0-255) *)
}
(** RGB color representation *)

type oklch = {
  l : float;  (** Lightness (0-100) *)
  c : float;  (** Chroma (0-0.4+) *)
  h : float;  (** Hue (0-360) *)
}
(** OKLCH color representation *)

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

  (* Convert to Lab coordinates *)
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
      let r = int_of_string ("0x" ^ String.make 2 hex_str.[0]) in
      let g = int_of_string ("0x" ^ String.make 2 hex_str.[1]) in
      let b = int_of_string ("0x" ^ String.make 2 hex_str.[2]) in
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
let rgb_to_hex rgb = Printf.sprintf "#%02x%02x%02x" rgb.r rgb.g rgb.b

(** Format OKLCH for CSS *)
let oklch_to_css oklch =
  let l_str = string_of_float (Float.round (oklch.l *. 10.0) /. 10.0) in
  let c_str = string_of_float (Float.round (oklch.c *. 1000.0) /. 1000.0) in
  let h_str = string_of_float (Float.round (oklch.h *. 1000.0) /. 1000.0) in
  Pp.str [ "oklch("; l_str; "% "; c_str; " "; h_str; ")" ]

(** Convert hex color to OKLCH CSS string *)
let hex_to_oklch_css hex =
  match hex_to_rgb hex with
  | Some rgb -> oklch_to_css (rgb_to_oklch rgb)
  | None -> hex (* Fallback to original hex if parsing fails *)

(** Predefined Tailwind v4 color values in OKLCH *)
module TailwindColors = struct
  (* These are the actual OKLCH values used by Tailwind v4 *)
  let gray =
    [
      (50, "oklch(98.5% 0.002 247.839)");
      (100, "oklch(96.7% 0.003 264.542)");
      (200, "oklch(92.8% 0.006 264.531)");
      (300, "oklch(87.2% 0.01 258.338)");
      (400, "oklch(65.1% 0.016 264.436)");
      (500, "oklch(55.1% 0.027 264.364)");
      (600, "oklch(44.6% 0.03 256.802)");
      (700, "oklch(37.3% 0.034 259.733)");
      (800, "oklch(27.8% 0.033 256.848)");
      (900, "oklch(21% 0.034 264.665)");
      (950, "oklch(13.1% 0.022 264.436)");
    ]

  let blue =
    [
      (50, "oklch(97.1% 0.014 237.021)");
      (100, "oklch(93.2% 0.032 255.585)");
      (200, "oklch(86.8% 0.061 251.813)");
      (300, "oklch(80.9% 0.105 251.813)");
      (400, "oklch(70.7% 0.165 254.624)");
      (500, "oklch(62.3% 0.214 259.815)");
      (600, "oklch(54.6% 0.245 262.881)");
      (700, "oklch(48.8% 0.243 264.376)");
      (800, "oklch(40.1% 0.19 264.828)");
      (900, "oklch(37.9% 0.146 265.522)");
      (950, "oklch(26.5% 0.136 269.055)");
    ]

  let red =
    [
      (50, "oklch(95.8% 0.019 17.331)");
      (100, "oklch(91.9% 0.037 17.175)");
      (200, "oklch(84.4% 0.073 19.692)");
      (300, "oklch(75.8% 0.133 21.276)");
      (400, "oklch(68.8% 0.191 24.342)");
      (500, "oklch(63.7% 0.237 25.331)");
      (600, "oklch(57.7% 0.245 27.325)");
      (700, "oklch(51.4% 0.204 27.245)");
      (800, "oklch(44.4% 0.177 26.899)");
      (900, "oklch(40.5% 0.147 28.067)");
      (950, "oklch(26.9% 0.097 28.428)");
    ]

  let slate =
    [
      (50, "oklch(98.6% 0.003 247.839)");
      (100, "oklch(96.9% 0.005 264.542)");
      (200, "oklch(93.1% 0.01 264.531)");
      (300, "oklch(87.6% 0.017 258.338)");
      (400, "oklch(71.3% 0.028 256.802)");
      (500, "oklch(58.7% 0.036 257.311)");
      (600, "oklch(46.5% 0.043 257.362)");
      (700, "oklch(39.3% 0.045 256.848)");
      (800, "oklch(29.1% 0.041 257.284)");
      (900, "oklch(20.4% 0.025 264.665)");
      (950, "oklch(13.2% 0.017 264.436)");
    ]

  let zinc =
    [
      (50, "oklch(98.5% 0.002 247.839)");
      (100, "oklch(96.5% 0.003 264.542)");
      (200, "oklch(92.5% 0.004 264.531)");
      (300, "oklch(86.9% 0.007 258.338)");
      (400, "oklch(69.8% 0.01 264.436)");
      (500, "oklch(56.8% 0.014 264.364)");
      (600, "oklch(45.2% 0.016 256.802)");
      (700, "oklch(38.4% 0.017 264.364)");
      (800, "oklch(28.1% 0.015 256.848)");
      (900, "oklch(20.8% 0.011 264.665)");
      (950, "oklch(13.1% 0.007 264.436)");
    ]

  let neutral =
    [
      (50, "oklch(98.5% 0 0)");
      (100, "oklch(96.5% 0 0)");
      (200, "oklch(92.8% 0 0)");
      (300, "oklch(87.3% 0 0)");
      (400, "oklch(70.4% 0 0)");
      (500, "oklch(57.2% 0 0)");
      (600, "oklch(45.7% 0 0)");
      (700, "oklch(38.5% 0 0)");
      (800, "oklch(28.3% 0 0)");
      (900, "oklch(21.1% 0 0)");
      (950, "oklch(13.4% 0 0)");
    ]

  let stone =
    [
      (50, "oklch(98.5% 0.002 106.424)");
      (100, "oklch(96.4% 0.003 106.424)");
      (200, "oklch(92.5% 0.005 106.424)");
      (300, "oklch(87.1% 0.008 106.424)");
      (400, "oklch(70.1% 0.01 106.424)");
      (500, "oklch(56.9% 0.013 106.424)");
      (600, "oklch(45.4% 0.011 106.424)");
      (700, "oklch(38.8% 0.01 106.424)");
      (800, "oklch(28.2% 0.007 106.424)");
      (900, "oklch(21.3% 0.004 106.424)");
      (950, "oklch(13.1% 0.002 106.424)");
    ]

  let orange =
    [
      (50, "oklch(97.8% 0.015 73.368)");
      (100, "oklch(95.5% 0.03 73.684)");
      (200, "oklch(90.5% 0.063 73.639)");
      (300, "oklch(84.1% 0.115 69.638)");
      (400, "oklch(77% 0.17 62.843)");
      (500, "oklch(71.3% 0.189 53.067)");
      (600, "oklch(65.5% 0.176 41.239)");
      (700, "oklch(54.8% 0.147 39.021)");
      (800, "oklch(45.8% 0.12 38.172)");
      (900, "oklch(38.7% 0.099 39.432)");
      (950, "oklch(25.8% 0.067 38.766)");
    ]

  let amber =
    [
      (50, "oklch(98.7% 0.016 95.277)");
      (100, "oklch(96.8% 0.041 95.644)");
      (200, "oklch(92.8% 0.089 95.334)");
      (300, "oklch(87.5% 0.15 94.634)");
      (400, "oklch(82.3% 0.2 91.605)");
      (500, "oklch(77.3% 0.189 86.047)");
      (600, "oklch(69.7% 0.17 79.287)");
      (700, "oklch(59.4% 0.147 73.483)");
      (800, "oklch(49.5% 0.122 69.28)");
      (900, "oklch(41.5% 0.102 66.343)");
      (950, "oklch(28.2% 0.071 62.428)");
    ]

  let yellow =
    [
      (50, "oklch(98.8% 0.019 96.68)");
      (100, "oklch(97.6% 0.046 97.138)");
      (200, "oklch(94.8% 0.105 97.438)");
      (300, "oklch(90.5% 0.175 96.723)");
      (400, "oklch(86.3% 0.206 94.781)");
      (500, "oklch(80.4% 0.191 91.605)");
      (600, "oklch(72.7% 0.169 87.334)");
      (700, "oklch(61.7% 0.142 83.453)");
      (800, "oklch(51.5% 0.118 79.483)");
      (900, "oklch(43.2% 0.099 75.3)");
      (950, "oklch(28.5% 0.066 71.709)");
    ]

  let lime =
    [
      (50, "oklch(97.9% 0.02 118.604)");
      (100, "oklch(95.5% 0.05 119.552)");
      (200, "oklch(90.8% 0.117 119.772)");
      (300, "oklch(85.1% 0.192 120.298)");
      (400, "oklch(77.9% 0.235 121.721)");
      (500, "oklch(71.8% 0.213 122.766)");
      (600, "oklch(60.7% 0.177 124.321)");
      (700, "oklch(50.7% 0.145 125.521)");
      (800, "oklch(41.7% 0.115 125.366)");
      (900, "oklch(36.3% 0.094 125.638)");
      (950, "oklch(24.3% 0.064 125.808)");
    ]

  let green =
    [
      (50, "oklch(97.6% 0.017 155.826)");
      (100, "oklch(94.5% 0.04 154.449)");
      (200, "oklch(88.8% 0.089 153.713)");
      (300, "oklch(81.2% 0.152 151.363)");
      (400, "oklch(71.2% 0.209 149.579)");
      (500, "oklch(63.2% 0.19 149.214)");
      (600, "oklch(54.2% 0.157 149.579)");
      (700, "oklch(46.7% 0.127 150.069)");
      (800, "oklch(38.8% 0.101 150.592)");
      (900, "oklch(33.6% 0.082 151.566)");
      (950, "oklch(22.1% 0.055 152.535)");
    ]

  let emerald =
    [
      (50, "oklch(96.9% 0.021 167.214)");
      (100, "oklch(93.2% 0.051 166.113)");
      (200, "oklch(86.2% 0.111 164.15)");
      (300, "oklch(77.3% 0.184 162.545)");
      (400, "oklch(67.3% 0.223 160.804)");
      (500, "oklch(58.7% 0.196 160.116)");
      (600, "oklch(50.2% 0.163 160.804)");
      (700, "oklch(43.5% 0.132 162.567)");
      (800, "oklch(36.6% 0.105 163.227)");
      (900, "oklch(31.6% 0.085 164.665)");
      (950, "oklch(20.8% 0.056 166.113)");
    ]

  let teal =
    [
      (50, "oklch(97.5% 0.014 182.513)");
      (100, "oklch(94.2% 0.037 180.584)");
      (200, "oklch(88.5% 0.084 180.726)");
      (300, "oklch(80.7% 0.141 181.181)");
      (400, "oklch(70.5% 0.17 182.085)");
      (500, "oklch(60.8% 0.152 183.292)");
      (600, "oklch(51.4% 0.127 184.699)");
      (700, "oklch(44.3% 0.105 185.802)");
      (800, "oklch(37.5% 0.084 186.735)");
      (900, "oklch(32.9% 0.068 187.915)");
      (950, "oklch(22.5% 0.047 188.251)");
    ]

  let cyan =
    [
      (50, "oklch(96.9% 0.017 200.119)");
      (100, "oklch(93.7% 0.042 199.956)");
      (200, "oklch(87.2% 0.093 200.065)");
      (300, "oklch(79.2% 0.154 200.651)");
      (400, "oklch(68.9% 0.182 202.267)");
      (500, "oklch(59.6% 0.16 204.054)");
      (600, "oklch(50.9% 0.135 205.765)");
      (700, "oklch(44.1% 0.111 207.562)");
      (800, "oklch(37.9% 0.089 209.135)");
      (900, "oklch(33.5% 0.072 210.732)");
      (950, "oklch(23.5% 0.051 211.682)");
    ]

  let sky =
    [
      (50, "oklch(97.5% 0.013 214.57)");
      (100, "oklch(94.1% 0.031 214.334)");
      (200, "oklch(88.2% 0.071 213.337)");
      (300, "oklch(80.9% 0.127 213.292)");
      (400, "oklch(71.2% 0.184 213.524)");
      (500, "oklch(63% 0.195 214.884)");
      (600, "oklch(54.9% 0.174 218.364)");
      (700, "oklch(48.2% 0.142 221.723)");
      (800, "oklch(41.8% 0.116 224.283)");
      (900, "oklch(36.9% 0.095 225.937)");
      (950, "oklch(25.7% 0.066 228.06)");
    ]

  let indigo =
    [
      (50, "oklch(96.4% 0.015 272.314)");
      (100, "oklch(93.1% 0.034 272.937)");
      (200, "oklch(86.8% 0.069 274.039)");
      (300, "oklch(79.4% 0.116 274.713)");
      (400, "oklch(70.5% 0.165 276.935)");
      (500, "oklch(60.9% 0.194 281.288)");
      (600, "oklch(53.4% 0.19 285.786)");
      (700, "oklch(47.5% 0.169 287.163)");
      (800, "oklch(40.5% 0.138 287.999)");
      (900, "oklch(35.9% 0.111 288.494)");
      (950, "oklch(24.1% 0.074 288.846)");
    ]

  let violet =
    [
      (50, "oklch(96.5% 0.016 293.756)");
      (100, "oklch(93.5% 0.035 294.588)");
      (200, "oklch(87.8% 0.073 295.112)");
      (300, "oklch(80.7% 0.127 295.484)");
      (400, "oklch(72.1% 0.187 296.424)");
      (500, "oklch(64.3% 0.221 298.781)");
      (600, "oklch(56.9% 0.216 301.685)");
      (700, "oklch(51.2% 0.19 303.418)");
      (800, "oklch(43.5% 0.157 304.252)");
      (900, "oklch(38.3% 0.127 304.935)");
      (950, "oklch(26.8% 0.088 305.612)");
    ]

  let purple =
    [
      (50, "oklch(98.2% 0.01 314.533)");
      (100, "oklch(95.8% 0.024 315.433)");
      (200, "oklch(91.3% 0.052 315.802)");
      (300, "oklch(84.7% 0.093 315.921)");
      (400, "oklch(76.5% 0.144 316.192)");
      (500, "oklch(68.5% 0.188 316.766)");
      (600, "oklch(61.2% 0.187 317.705)");
      (700, "oklch(53.6% 0.164 318.736)");
      (800, "oklch(44.6% 0.135 319.397)");
      (900, "oklch(37.5% 0.11 319.968)");
      (950, "oklch(26.9% 0.079 320.38)");
    ]

  let fuchsia =
    [
      (50, "oklch(98.2% 0.012 328.363)");
      (100, "oklch(95.9% 0.029 328.734)");
      (200, "oklch(91.7% 0.064 328.704)");
      (300, "oklch(85.5% 0.117 328.717)");
      (400, "oklch(77.8% 0.184 329.146)");
      (500, "oklch(70.2% 0.231 329.893)");
      (600, "oklch(62.2% 0.223 330.771)");
      (700, "oklch(53.9% 0.194 331.642)");
      (800, "oklch(45.3% 0.162 332.288)");
      (900, "oklch(38.7% 0.134 332.791)");
      (950, "oklch(27.5% 0.095 333.137)");
    ]

  let pink =
    [
      (50, "oklch(97.5% 0.014 343.195)");
      (100, "oklch(95.2% 0.03 343.166)");
      (200, "oklch(90.2% 0.063 343.394)");
      (300, "oklch(83.6% 0.115 344.056)");
      (400, "oklch(74.6% 0.18 346.054)");
      (500, "oklch(66.5% 0.227 349.761)");
      (600, "oklch(58.5% 0.225 354.112)");
      (700, "oklch(50.5% 0.195 357.196)");
      (800, "oklch(42.5% 0.163 358.892)");
      (900, "oklch(37.1% 0.135 359.732)");
      (950, "oklch(25.8% 0.093 0.223)");
    ]

  let rose =
    [
      (50, "oklch(97.8% 0.01 14.096)");
      (100, "oklch(95.5% 0.024 13.219)");
      (200, "oklch(90.9% 0.054 11.847)");
      (300, "oklch(84.7% 0.104 10.449)");
      (400, "oklch(76.7% 0.169 8.861)");
      (500, "oklch(68% 0.219 7.574)");
      (600, "oklch(59.5% 0.217 5.342)");
      (700, "oklch(51.5% 0.189 3.813)");
      (800, "oklch(43.2% 0.159 2.599)");
      (900, "oklch(38.1% 0.132 1.829)");
      (950, "oklch(26.9% 0.093 1.145)");
    ]

  let get_color color_name shade =
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
end

(* Convert string name to color type *)
let of_string = function
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
let hex s = Hex s
let rgb r g b = Rgb { red = r; green = g; blue = b }

(* Convert color to hex for a given shade *)
let to_hex color shade =
  match (color, shade) with
  (* Basic colors *)
  | Black, _ -> "#000000"
  | White, _ -> "#ffffff"
  | Hex h, _ -> h
  | Rgb { red; green; blue }, _ ->
      let to_hex_digit n =
        let hex = "0123456789abcdef" in
        String.make 1 hex.[n]
      in
      let byte_to_hex b =
        Pp.str [ to_hex_digit (b / 16); to_hex_digit (b mod 16) ]
      in
      Pp.str [ "#"; byte_to_hex red; byte_to_hex green; byte_to_hex blue ]
  (* Gray shades *)
  | Gray, 50 -> "#f9fafb"
  | Gray, 100 -> "#f3f4f6"
  | Gray, 200 -> "#e5e7eb"
  | Gray, 300 -> "#d1d5db"
  | Gray, 400 -> "#9ca3af"
  | Gray, 500 -> "#6b7280"
  | Gray, 600 -> "#4b5563"
  | Gray, 700 -> "#374151"
  | Gray, 800 -> "#1f2937"
  | Gray, 900 -> "#111827"
  (* Blue shades *)
  | Blue, 50 -> "#eff6ff"
  | Blue, 100 -> "#dbeafe"
  | Blue, 200 -> "#bfdbfe"
  | Blue, 300 -> "#93c5fd"
  | Blue, 400 -> "#60a5fa"
  | Blue, 500 -> "#3b82f6"
  | Blue, 600 -> "#2563eb"
  | Blue, 700 -> "#1d4ed8"
  | Blue, 800 -> "#1e40af"
  | Blue, 900 -> "#1e3a8a"
  (* Red shades *)
  | Red, 50 -> "#fef2f2"
  | Red, 100 -> "#fee2e2"
  | Red, 200 -> "#fecaca"
  | Red, 300 -> "#fca5a5"
  | Red, 400 -> "#f87171"
  | Red, 500 -> "#ef4444"
  | Red, 600 -> "#dc2626"
  | Red, 700 -> "#b91c1c"
  | Red, 800 -> "#991b1b"
  | Red, 900 -> "#7f1d1d"
  (* Add more colors as needed - for now use gray as fallback *)
  | _, _ -> "#6b7280"

(* Convert color to OKLCH CSS string for a given shade *)
let to_oklch_css color shade =
  match
    TailwindColors.get_color
      (match color with
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
      | Hex h -> hex_to_oklch_css h
      | Rgb { red; green; blue } ->
          rgb_to_oklch { r = red; g = green; b = blue } |> oklch_to_css)
      shade
  with
  | Some value -> value
  | None ->
      (* Fallback to hex conversion *)
      let hex_val = to_hex color shade in
      hex_to_oklch_css hex_val

(* Convert color to RGB string for a given shade *)
let to_rgb_string color shade =
  let hex_val = to_hex color shade in
  match hex_to_rgb hex_val with
  | Some { r; g; b } ->
      Pp.str
        [
          "rgb(";
          string_of_int r;
          " ";
          string_of_int g;
          " ";
          string_of_int b;
          ")";
        ]
  | None -> "rgb(0 0 0)" (* Fallback to black *)

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
  | Hex h -> Pp.str [ "["; h; "]" ]
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

(* Check if a color is black or white *)
let is_base_color = function Black | White -> true | _ -> false

(* Check if a color is a custom color (hex or rgb) *)
let is_custom_color = function Hex _ | Rgb _ -> true | _ -> false
