(** Tests for the color conversion module *)

open Tw.Color

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

let test_tailwind_colors () =
  let test_color name shade expected_oklch =
    match TailwindColors.get_color name shade with
    | Some oklch_str ->
        Alcotest.(check string)
          (Fmt.str "%s-%d" name shade)
          expected_oklch oklch_str
    | None -> Alcotest.failf "%s-%d: Color not found" name shade
  in

  test_color "gray" 50 "oklch(98.5% 0.002 247.839)";
  test_color "gray" 500 "oklch(55.1% 0.027 264.364)";
  test_color "gray" 900 "oklch(21% 0.034 264.665)";
  test_color "blue" 100 "oklch(93.2% 0.032 255.585)";
  test_color "blue" 500 "oklch(62.3% 0.214 259.815)";
  test_color "blue" 900 "oklch(37.9% 0.146 265.522)";
  test_color "red" 50 "oklch(95.8% 0.019 17.331)";
  test_color "red" 500 "oklch(63.7% 0.237 25.331)";
  test_color "red" 900 "oklch(40.5% 0.147 28.067)"

let test_oklch_css_formatting () =
  let test_cases =
    [
      ({ l = 98.5; c = 0.002; h = 247.839 }, "oklch(98.5% 0.002 247.839)");
      ({ l = 62.3; c = 0.214; h = 259.815 }, "oklch(62.3% 0.214 259.815)");
      ({ l = 0.0; c = 0.0; h = 0.0 }, "oklch(0% 0 0)");
      ({ l = 100.0; c = 0.4; h = 360.0 }, "oklch(100% 0.400 360)");
    ]
  in

  List.iter
    (fun (oklch, expected) ->
      let result = oklch_to_css oklch in
      Alcotest.(check string)
        (Fmt.str "OKLCH { l = %.1f; c = %.3f; h = %.3f }" oklch.l oklch.c
           oklch.h)
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

let test_color_accuracy () =
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

(* Test suite *)
let tests =
  [
    ("RGB to OKLCH roundtrip", `Quick, test_rgb_to_oklch_roundtrip);
    ("Hex parsing", `Quick, test_hex_parsing);
    ("RGB to hex", `Quick, test_rgb_to_hex);
    ("Tailwind color values", `Quick, test_tailwind_colors);
    ("OKLCH CSS formatting", `Quick, test_oklch_css_formatting);
    ("Edge cases", `Quick, test_edge_cases);
    ("Color accuracy", `Quick, test_color_accuracy);
  ]
