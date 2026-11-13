(** Tests for the sRGB conversions and hex spellings. *)

module Css = Cascade.Css
module Srgb = Tw__.Srgb (* Access internal module *)
open Srgb

let test_rgb_to_oklch_roundtrip () =
  let test_cases =
    [
      ({ r = 255; g = 0; b = 0 }, "Pure red");
      ({ r = 0; g = 255; b = 0 }, "Pure green");
      ({ r = 0; g = 0; b = 255 }, "Pure blue");
      ({ r = 255; g = 255; b = 255 }, "White");
      ({ r = 0; g = 0; b = 0 }, "Black");
      ({ r = 128; g = 128; b = 128 }, "Middle gray");
      ({ r = 59; g = 130; b = 246 }, "Tailwind blue-500");
      ({ r = 107; g = 114; b = 128 }, "Tailwind gray-500");
      ({ r = 239; g = 68; b = 68 }, "Tailwind red-500");
    ]
  in

  List.iter
    (fun (rgb, name) ->
      let oklch = rgb_to_oklch rgb in
      let rgb_back = oklch_to_rgb oklch in

      (* Allow small differences due to rounding *)
      let diff_r = abs (rgb.r - rgb_back.r) in
      let diff_g = abs (rgb.g - rgb_back.g) in
      let diff_b = abs (rgb.b - rgb_back.b) in

      Alcotest.(check bool)
        (name ^ " - red channel roundtrip")
        true (diff_r <= 2);
      Alcotest.(check bool)
        (name ^ " - green channel roundtrip")
        true (diff_g <= 2);
      Alcotest.(check bool)
        (name ^ " - blue channel roundtrip")
        true (diff_b <= 2))
    test_cases

let test_hex_parsing () =
  let rgb_equal =
    Alcotest.testable
      (fun fmt rgb -> Fmt.pf fmt "{ r = %d; g = %d; b = %d }" rgb.r rgb.g rgb.b)
      (fun a b -> a.r = b.r && a.g = b.g && a.b = b.b)
  in
  let rgb_option = Alcotest.option rgb_equal in

  let test_cases =
    [
      ("#ffffff", Some { r = 255; g = 255; b = 255 }, "Full white");
      ("#000000", Some { r = 0; g = 0; b = 0 }, "Full black");
      ("#ff0000", Some { r = 255; g = 0; b = 0 }, "Red");
      ("#00ff00", Some { r = 0; g = 255; b = 0 }, "Green");
      ("#0000ff", Some { r = 0; g = 0; b = 255 }, "Blue");
      ("ffffff", Some { r = 255; g = 255; b = 255 }, "White without #");
      ("#fff", Some { r = 255; g = 255; b = 255 }, "Short form white");
      ("#f00", Some { r = 255; g = 0; b = 0 }, "Short form red");
      ("#3b82f6", Some { r = 59; g = 130; b = 246 }, "Tailwind blue-500");
      ( "3b82f6",
        Some { r = 59; g = 130; b = 246 },
        "Tailwind blue-500 without #" );
      ("#gggggg", None, "Invalid hex chars");
      ("#ff", None, "Too short");
      ("#fffffff", None, "Too long");
      ("", None, "Empty string");
      ("xyz", None, "Non-hex string");
    ]
  in

  List.iter
    (fun (hex, expected, name) ->
      let result = hex_to_rgb hex in
      Alcotest.(check rgb_option) name expected result)
    test_cases

let test_rgb_to_hex () =
  let test_cases =
    [
      ({ r = 255; g = 255; b = 255 }, "#ffffff");
      ({ r = 0; g = 0; b = 0 }, "#000000");
      ({ r = 255; g = 0; b = 0 }, "#ff0000");
      ({ r = 59; g = 130; b = 246 }, "#3b82f6");
      ({ r = 107; g = 114; b = 128 }, "#6b7280");
    ]
  in

  List.iter
    (fun (rgb, expected) ->
      let result = rgb_to_hex rgb in
      Alcotest.(check string)
        (Fmt.str "RGB { r = %d; g = %d; b = %d }" rgb.r rgb.g rgb.b)
        expected result)
    test_cases

let test_edge_cases () =
  let extreme_oklch = { l = 150.0; c = 0.5; h = 45.0 } in
  let rgb = oklch_to_rgb extreme_oklch in

  Alcotest.(check bool)
    "Extreme OKLCH - red channel clamped" true
    (rgb.r >= 0 && rgb.r <= 255);
  Alcotest.(check bool)
    "Extreme OKLCH - green channel clamped" true
    (rgb.g >= 0 && rgb.g <= 255);
  Alcotest.(check bool)
    "Extreme OKLCH - blue channel clamped" true
    (rgb.b >= 0 && rgb.b <= 255);

  let negative_l = { l = -10.0; c = 0.1; h = 180.0 } in
  let rgb2 = oklch_to_rgb negative_l in
  Alcotest.(check bool)
    "Negative lightness - valid RGB" true
    (rgb2.r >= 0 && rgb2.r <= 255 && rgb2.g >= 0 && rgb2.g <= 255 && rgb2.b >= 0
   && rgb2.b <= 255)

let accuracy () =
  (* Test that our OKLCH conversion follows the OKLab specification *)
  (* Tailwind v4 designed their palette in OKLCH space first, then derived RGB *)
  (* So converting their RGB back to OKLCH won't perfectly match their original values *)
  let test_color hex_str name =
    match hex_to_rgb hex_str with
    | Some rgb ->
        let oklch = rgb_to_oklch rgb in
        (* Just verify our conversion produces valid OKLCH values *)
        Alcotest.(check bool)
          (Fmt.str "%s - valid lightness range" name)
          true
          (oklch.l >= 0.0 && oklch.l <= 100.0);
        Alcotest.(check bool)
          (Fmt.str "%s - valid chroma range" name)
          true
          (oklch.c >= 0.0 && oklch.c <= 0.5);
        Alcotest.(check bool)
          (Fmt.str "%s - valid hue range" name)
          true
          (oklch.h >= 0.0 && oklch.h <= 360.0)
    | None -> Alcotest.failf "Failed to parse hex color %s" hex_str
  in

  (* Test our conversion produces valid OKLCH values *)
  test_color "#3b82f6" "blue-500";
  test_color "#ef4444" "red-500";
  test_color "#6b7280" "gray-500"

(* [oklch(0.7 0.35 150)] is outside the sRGB gamut, so it has to be mapped onto
   a colour a display can show, and there is more than one answer. CSS Color 4
   sec. 14.2.2 halves the chroma at constant lightness and hue and keeps the
   largest one whose clipped result is within a just noticeable difference of
   the colour searched; [oklch_to_rgb] takes the first such chroma instead, and
   that is the answer Tailwind ships, because lightningcss folds the
   [color-mix(in srgb, oklch(...) ...)] fallback in Tailwind's own output the
   same way. Cascade's [Color_space.gamut_mapped_srgb_of_oklch] answers the
   spec's question, so it is not interchangeable with this one: [bg-blue-500/50]
   is where the two part company, and Tailwind's byte there is the one pinned
   below. *)
let test_out_of_gamut_oklch () =
  let requested = { l = 70.0; c = 0.35; h = 150.0 } in
  let rgb = oklch_to_rgb requested in
  Alcotest.(check string)
    "out-of-gamut oklch maps onto a colour sRGB can show" "#00c14b"
    (rgb_to_hex rgb);
  let mapped = rgb_to_oklch rgb in
  Alcotest.(check bool) "chroma is what gave way" true (mapped.c < requested.c);
  (* The search only moves along the constant-lightness, constant-hue ray, so
     the rendered colour lands within one just noticeable difference (0.02 in
     OKLab) of a point on that ray. That bounds the lightness drift directly,
     and bounds the hue drift by the angle a chord that long subtends at the
     mapped radius. *)
  let jnd = 0.02 in
  let hue_cone = Float.asin (jnd /. mapped.c) *. 180.0 /. Float.pi in
  Alcotest.(check bool)
    "lightness stays within one JND" true
    (Float.abs (mapped.l -. requested.l) /. 100.0 <= jnd);
  Alcotest.(check bool)
    "hue stays inside the JND cone" true
    (Float.abs (mapped.h -. requested.h) <= hue_cone);
  (* blue-500 is a palette colour just outside the gamut, so the choice of
     mapping shows up in a shipped utility: Tailwind's minified
     [.bg-blue-500/50] carries [#3080ff80]. *)
  let blue_500 = { l = 62.3; c = 0.214; h = 259.815 } in
  Alcotest.(check string)
    "blue-500 maps to the fallback Tailwind publishes" "#3080ff"
    (rgb_to_hex (oklch_to_rgb blue_500))

(* The alpha byte is appended to the six RGB digits, so a shorthand hex has to
   be expanded first: [#fff] with 10% alpha gave the five-digit [#fff1a], which
   cascade now rejects outright. *)
let test_shorthand_hex_alpha () =
  Alcotest.(check string)
    "a three-digit hex expands before the alpha" "#ffffff1a"
    (hex_with_alpha "#fff" 10.);
  Alcotest.(check string)
    "an existing alpha is replaced" "#ffffff1a"
    (hex_with_alpha "#ffffffcc" 10.);
  Alcotest.(check string)
    "a six-digit hex is unchanged" "#0307121a"
    (hex_with_alpha "#030712" 10.)

(* A hex collapses to its three- or four-digit form only when every channel
   repeats its digit, and a fully opaque alpha byte drops out. *)
let test_shorten_hex_str () =
  let check input expected =
    Alcotest.(check string) input expected (shorten_hex_str input)
  in
  check "#aabbcc" "abc";
  check "aabbccff" "abc";
  check "aabbccdd" "abcd";
  check "aabbcc80" "aabbcc80";
  check "123456" "123456";
  check "1234567f" "1234567f";
  check "12345" "12345"

(* A colour function folds to a hex only when every channel is a number; a
   [var()] channel has no byte to spell. *)
let test_css_color_to_hex () =
  let hex c =
    Option.map
      (Css.Pp.to_string ~minify:false Css.pp_color)
      (css_color_to_hex c)
  in
  let rgb r g b : Css.color = Css.Rgb (Css.Channels { r; g; b }) in
  Alcotest.(check (option string))
    "byte channels fold" (Some "#ff0000")
    (hex (rgb (Css.Int 255) (Css.Int 0) (Css.Int 0)));
  Alcotest.(check (option string))
    "a percentage channel folds" (Some "#00ff00")
    (hex (rgb (Css.Int 0) (Css.Pct 100.) (Css.Int 0)));
  Alcotest.(check (option string))
    "a var() channel does not" None
    (hex (rgb (Css.Var (Css.var_ref "r")) (Css.Int 0) (Css.Int 0)));
  Alcotest.(check (option string))
    "a none channel does not" None
    (hex (rgb Css.None (Css.Int 0) (Css.Int 0)))

let tests =
  [
    ("RGB to OKLCH roundtrip", `Quick, test_rgb_to_oklch_roundtrip);
    ("Hex parsing", `Quick, test_hex_parsing);
    ("RGB to hex", `Quick, test_rgb_to_hex);
    ("Edge cases", `Quick, test_edge_cases);
    ("Color accuracy", `Quick, accuracy);
    ("Out-of-gamut OKLCH", `Quick, test_out_of_gamut_oklch);
    ("Shorthand hex with alpha", `Quick, test_shorthand_hex_alpha);
    ("Shorten hex", `Quick, test_shorten_hex_str);
    ("CSS colour to hex", `Quick, test_css_color_to_hex);
  ]

let suite = ("srgb", tests)
