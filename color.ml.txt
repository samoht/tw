let round x = int_of_float @@ Gg.Float.round x
let positive_float x y = mod_float (mod_float x y +. y) y
let deg_to_rad d = d *. Float.pi /. 180.
let rad_to_deg r = r *. 180. /. Float.pi
let is_zero f = Float.abs f <= Float.epsilon
let is_one f = Float.abs (f -. 1.0) <= 1e-8

type gg = Gg.color

module type ColorRep = sig
  type t
  type param

  val v : ?alpha:float -> param -> param -> param -> t
  val to_gg : t -> gg
  val from_gg : gg -> t
  val to_css : t -> string
end

let pp_alpha () a = if is_one a then "" else Printf.sprintf " / %.2f" a

module Rgb_float = struct
  type t = { r : float; g : float; b : float; alpha : float }
  type param = float

  let v ?(alpha = 1.0) r g b = { r; g; b; alpha }

  let to_gg { r; g; b; alpha } =
    Gg.Color.v_srgb ~a:alpha r g b |> Gg.Color.clamp

  let from_gg t =
    let open Gg.Color in
    let color = to_srgb t in
    { r = r color; g = g color; b = b color; alpha = a color }

  let to_rgba { r; g; b; alpha } =
    let r = round (255. *. r) in
    let g = round (255. *. g) in
    let b = round (255. *. b) in
    (r, g, b, alpha)

  let to_css t =
    let r, g, b, alpha = to_rgba t in
    Printf.sprintf "rgb(%d %d %d%a)" r g b pp_alpha alpha
end

module Rgb = struct
  type t = { r : int; g : int; b : int; alpha : float }
  type param = int

  let v ?(alpha = 1.0) r g b = { r; g; b; alpha }

  let to_gg { r; g; b; alpha } =
    Gg.Color.v_srgbi ~a:alpha r g b |> Gg.Color.clamp

  let from_gg color =
    let r, g, b, alpha = Rgb_float.from_gg color |> Rgb_float.to_rgba in
    { r; g; b; alpha }

  let to_css t = Printf.sprintf "rgb(%d %d %d%a)" t.r t.g t.b pp_alpha t.alpha
end

module Hsl = struct
  type t = { h : float; s : float; l : float; alpha : float }
  type param = float

  let v ?(alpha = 1.0) h s l = { h; s; l; alpha }

  let to_gg { h; s; l; alpha } =
    let clip_hue x = if 360.0 = x then x else positive_float x 360.0 in
    let norm_hue = clip_hue h /. 60. in
    let chr = (1. -. abs_float ((2. *. l) -. 1.)) *. s in
    let m = l -. (chr /. 2.) in
    let x = chr *. (1. -. abs_float (mod_float norm_hue 2. -. 1.)) in
    let make r g b =
      Gg.Color.v_srgb ~a:alpha (r +. m) (g +. m) (b +. m) |> Gg.Color.clamp
    in
    if norm_hue < 0. then make 0. 0. 0.
    else if norm_hue < 1. then make chr x 0.
    else if norm_hue < 2. then make x chr 0.
    else if norm_hue < 3. then make 0. chr x
    else if norm_hue < 4. then make 0. x chr
    else if norm_hue < 5. then make x 0. chr
    else if norm_hue < 6. then make chr 0. x
    else make 0. 0. 0.

  let from_gg t =
    let rgba = Rgb.from_gg t in
    let red, green, blue, alpha = (rgba.r, rgba.g, rgba.b, rgba.alpha) in
    let r = float_of_int red /. 255. in
    let g = float_of_int green /. 255. in
    let b = float_of_int blue /. 255. in
    let c_max = max (max red green) blue in
    let c_min = min (min red green) blue in
    let c = c_max - c_min in
    let c' = float_of_int c /. 255. in
    let hue' c =
      if c = 0 then 0.
      else if c_max = red then positive_float ((g -. b) /. c') 6.
      else if c_max = green then ((b -. r) /. c') +. 2.
      else ((r -. g) /. c') +. 4.
    in
    let hue = 60. *. hue' c in
    let lightness = float_of_int (c_max + c_min) /. (255. *. 2.) in
    let saturation =
      if c = 0 then 0. else c' /. (1. -. abs_float ((2. *. lightness) -. 1.))
    in
    { h = hue; s = saturation; l = lightness; alpha }

  let to_css t =
    Printf.sprintf "hsl(%.2f %.2f%% %.2f%%%a)" t.h (t.s *. 100.) (t.l *. 100.)
      pp_alpha t.alpha
end

module Oklab = struct
  type t = { l : float; a : float; b : float; alpha : float }
  type param = float

  let v ?(alpha = 1.0) l a b = { l; a; b; alpha }

  let to_gg { l; a; b; alpha } =
    (* From https://bottosson.github.io/posts/oklab/#converting-from-linear-srgb-to-oklab *)
    let l' = l +. (0.3963377774 *. a) +. (0.2158037573 *. b) in
    let m = l -. (0.1055613458 *. a) -. (0.0638541728 *. b) in
    let s = l -. (0.0894841775 *. a) -. (1.2914855480 *. b) in

    let l' = l' *. l' *. l' in
    let m = m *. m *. m in
    let s = s *. s *. s in

    let r =
      (4.0767416621 *. l') -. (3.3077115913 *. m) +. (0.2309699292 *. s)
    in
    let g =
      (-1.2684380046 *. l') +. (2.6097574011 *. m) -. (0.3413193965 *. s)
    in
    let b =
      (-0.0041960863 *. l') -. (0.7034186147 *. m) +. (1.7076147010 *. s)
    in
    Gg.Color.v r g b alpha |> Gg.Color.clamp

  let from_gg t =
    (* From https://bottosson.github.io/posts/oklab/#converting-from-linear-srgb-to-oklab *)
    let r, g, b, alpha =
      let open Gg.Color in
      (r t, g t, b t, a t)
    in

    let l = (0.4122214708 *. r) +. (0.5363325363 *. g) +. (0.0514459929 *. b) in
    let m = (0.2119034982 *. r) +. (0.6806995451 *. g) +. (0.1073969566 *. b) in
    let s = (0.0883024619 *. r) +. (0.2817188376 *. g) +. (0.6299787005 *. b) in

    let l = Float.pow l (1. /. 3.) in
    let m = Float.pow m (1. /. 3.) in
    let s = Float.pow s (1. /. 3.) in

    {
      l = (0.2104542553 *. l) +. (0.7936177850 *. m) -. (0.0040720468 *. s);
      a = (1.9779984951 *. l) -. (2.4285922050 *. m) +. (0.4505937099 *. s);
      b = (0.0259040371 *. l) +. (0.7827717662 *. m) -. (0.8086757660 *. s);
      alpha;
    }

  let to_css t =
    Printf.sprintf "oklab(%.2f %.2f %.2f%a)" t.l t.a t.b pp_alpha t.alpha
end

module Oklch = struct
  type t = { l : float; c : float; h : float; alpha : float }
  type param = float

  let v ?(alpha = 1.0) l c h = { l; c; h; alpha }

  let to_gg { l; c; h; alpha } =
    let h = deg_to_rad h in
    let a = c *. cos h in
    let b = c *. sin h in
    Oklab.v ~alpha l a b |> Oklab.to_gg

  let from_gg t =
    let ok = Oklab.from_gg t in
    let l = ok.l in
    let powerless = is_zero l || is_one l in
    let c = if powerless then 0.0 else sqrt ((ok.a ** 2.0) +. (ok.b ** 2.0)) in
    let powerless = powerless || is_zero c in
    let h =
      if powerless then 0.0
      else positive_float (rad_to_deg (Float.atan2 ok.b ok.a)) 360.
    in
    { l; c; h; alpha = ok.alpha }

  let to_css t =
    Printf.sprintf "oklch(%.2f %.2f %.2fdeg%a)" t.l t.c t.h pp_alpha t.alpha
end

let of_hexstring s =
  let short = String.length s = 4 in
  if (not short) && String.length s <> 7 then None
  else
    let sub = String.sub s in
    let r, g, b =
      if short then (sub 1 1, sub 2 1, sub 3 1) else (sub 1 2, sub 3 2, sub 5 2)
    in
    let parse_hex x =
      Option.map
        (fun x -> if short then (16 * x) + x else x)
        (int_of_string_opt ("0x" ^ x))
    in
    let ( let* ) = Option.bind in
    let* r = parse_hex r in
    let* g = parse_hex g in
    let* b = parse_hex b in
    Some (Rgb.v r g b |> Rgb.to_gg)

let to_hexstring color =
  let c = Rgb.from_gg color in
  Printf.sprintf "#%02x%02x%02x" c.r c.g c.b

let black = Gg.Color.black
let white = Gg.Color.white
let gray_tone l = Oklch.v l 0. 0. |> Oklch.to_gg

let rotate_hue angle t =
  let { Oklch.l; c; h; alpha } = Oklch.from_gg t in
  Oklch.v ~alpha l c (h +. angle) |> Oklch.to_gg

let complementary = rotate_hue 180.

let lighten f t =
  let { Oklch.l; c; h; alpha } = Oklch.from_gg t in
  Oklch.v ~alpha (l +. f) c h |> Oklch.to_gg

let darken f = lighten ~-.f

let intensify f t =
  let { Oklch.l; c; h; alpha } = Oklch.from_gg t in
  Oklch.v ~alpha l (c +. f) h |> Oklch.to_gg

let desintensify f = intensify ~-.f
let lightness t = (Oklch.from_gg t).l

let contrast_ratio t1 t2 =
  let l1 = lightness t1 in
  let l2 = lightness t2 in
  if l1 > l2 then (l1 +. 0.05) /. (l2 +. 0.05) else (l2 +. 0.05) /. (l1 +. 0.05)

let is_light t = lightness t > 0.5
let readable t1 t2 = contrast_ratio t1 t2 > 4.5
let text_color t = if is_light t then black else white

let random ?(alpha = 1.0) ?(light = 1.0) ?(chroma = 0.5) () =
  let h = Random.float 360.0 in
  Oklch.v ~alpha light chroma h |> Oklch.to_gg
