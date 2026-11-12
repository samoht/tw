(** sRGB colours by the byte: hex spellings and the conversions to and from
    OKLab and OKLCh. *)

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
    (* A trailing alpha, [#rgba] or [#rrggbbaa], is not part of the colour a
       fold replaces the alpha of, so it comes off first. *)
    let hex_str =
      match String.length hex_str with
      | 4 -> String.sub hex_str 0 3
      | 8 -> String.sub hex_str 0 6
      | _ -> hex_str
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

(* The hex spelling of an sRGB colour with an alpha byte, which a fully opaque
   colour leaves off. *)
let rgba_to_hex r g b a =
  let rgb = "#" ^ Pp.hex_byte r ^ Pp.hex_byte g ^ Pp.hex_byte b in
  if a = 255 then rgb else rgb ^ Pp.hex_byte a

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

(* [oklab(from <c> l a b / <alpha>)] takes the origin's channels and the alpha
   written after the slash, so the alpha replaces the colour's own rather than
   multiplying it. A hex spelling names the channels outright, and so does a
   colour keyword; every other colour names them somewhere the class cannot
   see. *)
(* The byte an [rgb()] channel spells; a channel that reads a variable has
   none, and folds as black would. *)
let channel_byte : Css.channel -> int option = function
  | Int i -> Some i
  | Num f -> Some (Float.to_int (Float.round f))
  | Pct f -> Some (Float.to_int (Float.round (f *. 2.55)))
  | Var _ | None -> Option.None

let oklab_alpha (c : Css.color) alpha : Css.color option =
  let fold r g b =
    let ok_l, ok_a, ok_b = rgb_to_oklab { r; g; b } in
    Some (Css.oklaba ok_l ok_a ok_b alpha)
  in
  match Cascade.Values.nonkeyword_color c with
  | Css.Hex { r; g; b; _ } | Css.Authored_hex { r; g; b; _ } -> fold r g b
  (* An [rgb()] with plain channels folds the same way; its own alpha is
     replaced, as [oklab(from <colour> l a b / <alpha>)] replaces it. *)
  | Css.Rgb (Channels { r; g; b }) | Css.Rgba { rgb = Channels { r; g; b }; _ }
    -> (
      match (channel_byte r, channel_byte g, channel_byte b) with
      | Some r, Some g, Some b -> fold r g b
      | _ -> None)
  | _ -> None

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

(** The byte an rgb() channel stands for, when it stands for one. A var()
    reference carries no value here, and CSS Color 4's [none] takes the
    analogous channel of whatever the colour is combined with rather than
    standing for zero, so neither has a byte. *)
let int_of_channel : Css.channel -> int option = function
  | Css.Int i -> Some (min 255 (max 0 i))
  | Css.Num f -> Some (min 255 (max 0 (Float.to_int (Float.round f))))
  | Css.Pct f -> Some (min 255 (max 0 (Float.to_int (Float.round (f *. 2.55)))))
  | Css.Var _ | Css.None -> None

(** How an alpha channel spells inside a hex colour. *)
type folded_alpha =
  | Opaque  (** no alpha channel at all *)
  | Alpha_byte of int
  | Alpha_unresolvable  (** a var() or calc(), which no hex byte carries *)

let fold_alpha : Css.alpha -> folded_alpha = function
  | Css.None -> Opaque
  | Css.Num f ->
      Alpha_byte (min 255 (max 0 (Float.to_int (Float.round (f *. 255.)))))
  | Css.Pct f ->
      Alpha_byte (min 255 (max 0 (Float.to_int (Float.round (f *. 2.55)))))
  | Css.Var _ | Css.Calc _ -> Alpha_unresolvable

(** Convert a typed CSS color to a hex string for Tailwind parity *)
let css_color_to_hex (c : Css.color) : Css.color option =
  let hex_of_bytes bytes = Some (Css.hex ("#" ^ shorten_hex_str bytes)) in
  match c with
  | Css.Rgb (Css.Channels { r; g; b }) -> (
      match (int_of_channel r, int_of_channel g, int_of_channel b) with
      | Some r, Some g, Some b ->
          hex_of_bytes (Pp.hex_byte r ^ Pp.hex_byte g ^ Pp.hex_byte b)
      | _ -> None)
  | Css.Rgba { rgb = Css.Channels { r; g; b }; a; _ } -> (
      match
        (int_of_channel r, int_of_channel g, int_of_channel b, fold_alpha a)
      with
      | Some r, Some g, Some b, Opaque ->
          hex_of_bytes (Pp.hex_byte r ^ Pp.hex_byte g ^ Pp.hex_byte b)
      | Some r, Some g, Some b, Alpha_byte a ->
          hex_of_bytes
            (Pp.hex_byte r ^ Pp.hex_byte g ^ Pp.hex_byte b ^ Pp.hex_byte a)
      | _ -> None)
  | Css.Hsl _ -> (
      (* Fold through cascade's own colour path: it knows every hue unit and
         reads a bare number for saturation and lightness as the percentage of
         the same value. It leaves a colour whose channels are not all static
         alone, and that one has no hex form. *)
      match
        Cascade.Values.nonkeyword_color (Cascade.Values.normalize_color c)
      with
      | Css.Hex { r; g; b; a } | Css.Authored_hex { r; g; b; a; _ } ->
          let hex = Pp.hex_byte r ^ Pp.hex_byte g ^ Pp.hex_byte b in
          hex_of_bytes (if a = 255 then hex else hex ^ Pp.hex_byte a)
      | _ -> None)
  | _ -> None

(** Resolve a typed [Css.color] to its emission form. A hex keeps the spelling
    the bracket wrote, a colour function is converted to hex where possible. *)
let resolve_bracket_css_color (css_color : Css.color) : Css.color =
  match css_color with
  (* The bracket's own spelling is the one Tailwind writes back. *)
  | Css.Authored_hex _ -> css_color
  | Css.Hex { r; g; b; a } ->
      let value = hex_string_of_rgb (r, g, b) in
      let value = if a = 255 then value else value ^ Pp.hex_byte a in
      Css.hex ("#" ^ shorten_hex_str value)
  | _ -> (
      match css_color_to_hex css_color with
      | Some hex_c -> hex_c
      | None -> css_color)
