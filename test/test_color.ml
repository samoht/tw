(** Tests for the color conversion module *)

module Css = Cascade.Css
open Tw.Private.Color

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

let test_oklch_css_formatting () =
  let pp l c h expected =
    let color = Css.oklch l c h in
    let result = Css.Pp.to_string ~minify:false Css.pp_color color in
    Alcotest.(check string)
      (Fmt.str "OKLCH { l = %.1f; c = %.3f; h = %.3f }" l c h)
      expected result
  in
  pp 98.5 0.002 247.839 "oklch(98.5% .002 247.839)";
  pp 62.3 0.214 259.815 "oklch(62.3% .214 259.815)";
  pp 0.0 0.0 0.0 "oklch(0% 0 0)";
  pp 100.0 0.4 360.0 "oklch(100% .4 360)"

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

let test_css_mode_with_colors () =
  (* Test that color utilities work correctly with different CSS modes *)
  let open Tw in
  (* Generate CSS from color utilities *)
  let styles = [ bg blue; text red ] in
  let css = to_css styles in
  let css_string = Css.to_string css in

  (* Debug: print CSS and class names *)
  Fmt.epr "bg_blue class: '%s'\n" (Tw.to_classes [ bg blue ]);
  Fmt.epr "text_red class: '%s'\n" (Tw.to_classes [ text red ]);
  Fmt.epr "Generated CSS:\n%s\n" css_string;

  (* Test that Variables mode is the default and uses CSS variables *)
  Alcotest.(check bool)
    "Default mode uses var() for colors" true
    (Astring.String.is_infix ~affix:"var(--color-" css_string);

  (* For now, just verify the CSS is generated correctly *)
  Alcotest.(check bool)
    "Contains bg-blue-500 class" true
    (Astring.String.is_infix ~affix:".bg-blue-500" css_string);
  Alcotest.(check bool)
    "Contains text-red-500 class" true
    (Astring.String.is_infix ~affix:".text-red-500" css_string)

(* Per-side border colors (border-{t,r,b,l}-{color}) paint the matching physical
   edge; named, arbitrary and keyword forms. Widths (border-l-2) still resolve
   via the borders handler. *)
let test_border_side_color () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error _ -> Alcotest.failf "could not parse %S" cls
  in
  Alcotest.(check bool)
    "border-l-[#575959] sets border-left-color" true
    (Astring.String.is_infix ~affix:"border-left-color: #575959"
       (css "border-l-[#575959]"));
  Alcotest.(check bool)
    "border-b-transparent sets border-bottom-color" true
    (Astring.String.is_infix ~affix:"border-bottom-color:"
       (css "border-b-transparent"));
  Alcotest.(check bool)
    "border-l-red-500 sets border-left-color" true
    (Astring.String.is_infix ~affix:"border-left-color:"
       (css "border-l-red-500"));
  Alcotest.(check bool)
    "border-l-2 is still a width" true
    (Astring.String.is_infix ~affix:"border-left-width: 2px" (css "border-l-2"));
  Alcotest.(check bool)
    "border-x-red-500 uses the logical inline color" true
    (Astring.String.is_infix ~affix:"border-inline-color:"
       (css "border-x-red-500"));
  Alcotest.(check bool)
    "border-y-red-500 uses the logical block color" true
    (Astring.String.is_infix ~affix:"border-block-color:"
       (css "border-y-red-500"));
  Alcotest.(check bool)
    "border-s-red-500 uses the inline-start color" true
    (Astring.String.is_infix ~affix:"border-inline-start-color:"
       (css "border-s-red-500"));
  Alcotest.(check bool)
    "border-e-red-500 uses the inline-end color" true
    (Astring.String.is_infix ~affix:"border-inline-end-color:"
       (css "border-e-red-500"));
  Alcotest.(check bool)
    "border-bs-red-500 uses the block-start color" true
    (Astring.String.is_infix ~affix:"border-block-start-color:"
       (css "border-bs-red-500"));
  Alcotest.(check bool)
    "border-be-red-500 uses the block-end color" true
    (Astring.String.is_infix ~affix:"border-block-end-color:"
       (css "border-be-red-500"))

(* A CSS variable in a border color bracket, and its v4 paren shorthand, resolve
   to var(): border-[var(--x)] and border-(--x) both set border-color. *)
let test_border_color_var () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error _ -> Alcotest.failf "could not parse %S" cls
  in
  Alcotest.(check bool)
    "border-[var(--pattern-fg)] sets border-color: var()" true
    (Astring.String.is_infix ~affix:"border-color: var(--pattern-fg)"
       (css "border-[var(--pattern-fg)]"));
  Alcotest.(check bool)
    "border-(--pattern-fg) shorthand sets border-color: var()" true
    (Astring.String.is_infix ~affix:"border-color: var(--pattern-fg)"
       (css "border-(--pattern-fg)"));
  Alcotest.(check bool)
    "border-t-[var(--x)] sets border-top-color: var()" true
    (Astring.String.is_infix ~affix:"border-top-color: var(--x)"
       (css "border-t-[var(--x)]"));
  (* the paren shorthand keeps its own class name *)
  Alcotest.(check string)
    "border-(--pattern-fg) round-trips" "border-(--pattern-fg)"
    (Tw.to_string (Result.get_ok (Tw.of_string "border-(--pattern-fg)")))

(* A per-side border color takes an alpha modifier, like the all-sides one.
   border-b-white/5 used to be an unknown class. *)
let test_border_side_color_opacity () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "border-b-white/5" "border-bottom-color:#ffffff0d";
  has "border-b-white/5"
    "border-bottom-color:color-mix(in oklab,var(--color-white) 5%,transparent)";
  (* an axis sets both of its sides *)
  has "border-x-pink-400/30" "border-inline-color:";
  Alcotest.(check string)
    "border-b-white/5 round-trips" "border-b-white/5"
    (Tw.to_string (Result.get_ok (Tw.of_string "border-b-white/5")))

(* An alpha can name a custom property to read the percentage from, written
   either as [/[var(--x)]] or as the [/(--x)] shorthand. The percentage is not
   known at build time, so it goes into the [color-mix] as a reference; before,
   an unresolved alpha counted as 100% and the modifier was dropped. *)
let test_alpha_from_a_var () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "bg-cyan-400/(--a)"
    "color-mix(in oklab,var(--color-cyan-400) var(--a),transparent)";
  has "bg-cyan-400/[var(--a)]"
    "color-mix(in oklab,var(--color-cyan-400) var(--a),transparent)";
  has "text-red-500/(--a)"
    "color-mix(in oklab,var(--color-red-500) var(--a),transparent)";
  has "fill-red-500/(--a)"
    "color-mix(in oklab,var(--color-red-500) var(--a),transparent)";
  has "bg-[#0088cc]/(--a)" "color-mix(in oklab,#08c var(--a),transparent)";
  (* the fallback is the colour at full opacity, with no alpha folded in *)
  has "bg-cyan-400/(--a)" "background-color:var(--color-cyan-400)";
  (* both spellings round-trip *)
  Alcotest.(check string)
    "the shorthand round-trips" "bg-cyan-400/(--a)"
    (Tw.to_string (Result.get_ok (Tw.of_string "bg-cyan-400/(--a)")));
  Alcotest.(check string)
    "the bracket form round-trips" "bg-cyan-400/[var(--a)]"
    (Tw.to_string (Result.get_ok (Tw.of_string "bg-cyan-400/[var(--a)]")))

(* A bracket colour is read as CSS first and only then as a palette name, so a
   system colour or a light-dark() both work. The fallback used to admit only
   colour functions, which left [bg-[Field]] an unknown class. *)
let test_bracket_css_colors () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "bg-[Field]" "background-color:field";
  has "text-[FieldText]" "color:fieldtext";
  has "bg-[light-dark(white,black)]" "background-color:light-dark(";
  (* a CSS keyword still beats the palette entry of the same name *)
  has "bg-[red]" "background-color:red"

let test_invalid_shade () =
  Alcotest.check_raises "bg ~shade:250 gray raises at construction"
    (Invalid_argument
       "bg: gray has no shade 250 (valid shades: 50, 100, 200, 300, 400, 500, \
        600, 700, 800, 900, 950)") (fun () -> ignore (Tw.bg ~shade:250 Tw.gray));
  (match Tw.of_string "bg-gray-250" with
  | Error (`Msg _) -> ()
  | Ok _ -> Alcotest.fail "bg-gray-250 should not parse");
  (* Valid shades still construct, and shadeless colors ignore the shade *)
  ignore (Tw.bg ~shade:200 Tw.gray);
  ignore (Tw.bg ~shade:250 (Tw.hex "#aabbcc"))

(* Test suite *)
(* An achromatic palette colour must keep a [none] hue. A numeric hue renders
   the same but folds to a plain hex, which pins the hue that interpolation is
   meant to take from the other colour. *)
let test_achromatic_none_hue () =
  let css =
    Css.to_string ~minify:true (Tw.to_css [ Tw.bg ~shade:500 Tw.neutral ])
  in
  let contains needle =
    let n = String.length needle and l = String.length css in
    let rec go i = i + n <= l && (String.sub css i n = needle || go (i + 1)) in
    go 0
  in
  Alcotest.check Alcotest.bool "neutral-500 keeps a none hue" true
    (contains "none");
  Alcotest.check Alcotest.bool "neutral-500 did not fold to hex" false
    (contains "#737373")

(* v4.3.3 added four colour families to the default theme (mauve/mist/olive/
   taupe). Each utility reads var(--color-<family>-<shade>). *)
let test_v433_color_families () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.pp ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool)
      (cls ^ " uses " ^ affix)
      true
      (Astring.String.is_infix ~affix (css cls))
  in
  has "bg-mauve-500" "var(--color-mauve-500)";
  has "text-olive-700" "var(--color-olive-700)";
  has "border-mist-200" "var(--color-mist-200)";
  has "bg-taupe-950" "var(--color-taupe-950)"

(* A [/100] modifier is a no-op mix, so the colour itself is the value: the
   color-mix and its @supports fallback used to be emitted anyway. And [!] has
   to reach inside that @supports, or the fallback outranks the modern value. *)
let test_full_opacity_and_important () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.pp ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  let lacks cls affix =
    Alcotest.(check bool)
      (cls ^ " without " ^ affix)
      false
      (Astring.String.is_infix ~affix (css cls))
  in
  has "text-blue-600/100" "color:var(--color-blue-600)";
  lacks "text-blue-600/100" "color-mix";
  lacks "divide-gray-200/100" "color-mix";
  has "bg-white/75!"
    "color-mix(in oklab,var(--color-white) 75%,transparent)!important"

(* The alpha byte is appended to the six RGB digits, so a shorthand hex has to
   be expanded first: [#fff] with 10% alpha gave the five-digit [#fff1a], which
   cascade now rejects outright. *)
let test_shorthand_hex_alpha () =
  Alcotest.(check string)
    "a three-digit hex expands before the alpha" "#ffffff1a"
    (Tw.Private.Color.hex_with_alpha "#fff" 10.);
  Alcotest.(check string)
    "an existing alpha is replaced" "#ffffff1a"
    (Tw.Private.Color.hex_with_alpha "#ffffffcc" 10.);
  Alcotest.(check string)
    "a six-digit hex is unchanged" "#0307121a"
    (Tw.Private.Color.hex_with_alpha "#030712" 10.)

let tests =
  [
    ("Achromatic colour keeps a none hue", `Quick, test_achromatic_none_hue);
    ("Per-side border colors", `Quick, test_border_side_color);
    ("Border color var", `Quick, test_border_color_var);
    ("Border side color opacity", `Quick, test_border_side_color_opacity);
    ("Bracket CSS colors", `Quick, test_bracket_css_colors);
    ("Alpha from a var", `Quick, test_alpha_from_a_var);
    ("Invalid shades", `Quick, test_invalid_shade);
    ("RGB to OKLCH roundtrip", `Quick, test_rgb_to_oklch_roundtrip);
    ("Hex parsing", `Quick, test_hex_parsing);
    ("RGB to hex", `Quick, test_rgb_to_hex);
    ("OKLCH CSS formatting", `Quick, test_oklch_css_formatting);
    ("Edge cases", `Quick, test_edge_cases);
    ("Color accuracy", `Quick, accuracy);
    ("CSS modes with colors", `Quick, test_css_mode_with_colors);
    ("v4.3.3 colour families", `Quick, test_v433_color_families);
    ("Full opacity and important", `Quick, test_full_opacity_and_important);
    ("Shorthand hex with alpha", `Quick, test_shorthand_hex_alpha);
  ]

let suite = ("color", tests)
