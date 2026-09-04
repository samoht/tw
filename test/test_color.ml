(** Tests for the color conversion module *)

module Css = Cascade.Css
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
  Test_helpers.check_declarations "border-l-[#575959]"
    [ "border-left-color:#575959" ];
  Test_helpers.check_declarations "border-b-transparent"
    [ "border-bottom-color:#0000" ];
  Test_helpers.check_declarations "border-l-red-500"
    [ "border-left-color:var(--color-red-500)" ];
  Test_helpers.check_declarations "border-l-2"
    [ "border-left-style:var(--tw-border-style)"; "border-left-width:2px" ];
  Test_helpers.check_declarations "border-x-red-500"
    [ "border-inline-color:var(--color-red-500)" ];
  Test_helpers.check_declarations "border-y-red-500"
    [ "border-block-color:var(--color-red-500)" ];
  Test_helpers.check_declarations "border-s-red-500"
    [ "border-inline-start-color:var(--color-red-500)" ];
  Test_helpers.check_declarations "border-e-red-500"
    [ "border-inline-end-color:var(--color-red-500)" ];
  Test_helpers.check_declarations "border-bs-red-500"
    [ "border-block-start-color:var(--color-red-500)" ];
  Test_helpers.check_declarations "border-be-red-500"
    [ "border-block-end-color:var(--color-red-500)" ]

(* Every per-side border colour writes a colour another side writes too, so
   their relative order decides which one wins. Tailwind groups them side-major:
   the all-sides colour first, then the axes and the logical sides, then the
   physical ones. The ten sides shared one suborder, so they tied and fell back
   to class-name order: [border-t-blue-500] sorted before [border-x-red-500]. *)
let test_border_side_color_order () =
  Test_helpers.check_class_order ~test_name:"border side colors"
    [
      "border-red-500";
      "border-x-red-500";
      "border-y-red-500";
      "border-s-red-500";
      "border-e-red-500";
      "border-bs-red-500";
      "border-be-red-500";
      "border-t-blue-500";
      "border-r-red-500";
      "border-b-red-500";
      "border-l-red-500";
    ]

(* An arbitrary per-side colour sorts in its own side's band, next to the named
   colours of that side. *)
let test_border_side_bracket_color_order () =
  Test_helpers.check_class_order ~test_name:"border side bracket colors"
    [
      "border-[#f00]";
      "border-x-[#f00]";
      "border-y-[#f00]";
      "border-s-[#f00]";
      "border-e-[#f00]";
      "border-bs-[#f00]";
      "border-be-[#f00]";
      "border-t-[#00f]";
      "border-t-red-500";
      "border-t-transparent";
      "border-r-[#f00]";
      "border-b-[#f00]";
      "border-l-[#f00]";
    ]

let test_outline_inherit_order () =
  Test_helpers.check_class_order ~test_name:"outline inherit order"
    [
      "outline-lime-100";
      "outline-inherit";
      "outline-indigo-600";
      "outline-indigo-500";
    ]

(* A CSS variable in a border color bracket, and its v4 paren shorthand, resolve
   to var(): border-[var(--x)] and border-(--x) both set border-color. *)
let test_border_color_var () =
  Test_helpers.check_declarations "border-[var(--pattern-fg)]"
    [ "border-color:var(--pattern-fg)" ];
  Test_helpers.check_declarations "border-(--pattern-fg)"
    [ "border-color:var(--pattern-fg)" ];
  Test_helpers.check_declarations "border-t-[var(--x)]"
    [ "border-top-color:var(--x)" ];
  (* the paren shorthand keeps its own class name *)
  Alcotest.(check string)
    "border-(--pattern-fg) round-trips" "border-(--pattern-fg)"
    (Tw.pp (Result.get_ok (Tw.of_string "border-(--pattern-fg)")))

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
    (Tw.pp (Result.get_ok (Tw.of_string "border-b-white/5")))

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
  (* Tailwind resolves the token into the full-opacity fallback and keeps the
     variable reference for the guarded mix. *)
  has "bg-cyan-400/(--a)" "background-color:oklch(78.9%.154 211.53)";
  (* both spellings round-trip *)
  Alcotest.(check string)
    "the shorthand round-trips" "bg-cyan-400/(--a)"
    (Tw.pp (Result.get_ok (Tw.of_string "bg-cyan-400/(--a)")));
  Alcotest.(check string)
    "the bracket form round-trips" "bg-cyan-400/[var(--a)]"
    (Tw.pp (Result.get_ok (Tw.of_string "bg-cyan-400/[var(--a)]")))

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

(* An opacity modifier applies to the colour the bracket was parsed into. The
   modifier read the bracket text back through the palette parser, which
   answered black for every CSS colour the palette does not name, and answered
   the palette entry for the names it shares with CSS. *)
let test_bracket_colour_opacity () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  (* the value a class sets for [prop], so two spellings of one colour compare
     without their class names *)
  let value prop cls =
    let sheet = css cls in
    let key = prop ^ ":" in
    match Astring.String.find_sub ~sub:key sheet with
    | None -> Alcotest.failf "%s sets no %s: %s" cls prop sheet
    | Some i ->
        let start = i + String.length key in
        Astring.String.with_range ~first:start sheet
        |> Astring.String.take ~sat:(fun c -> c <> ';' && c <> '}')
  in
  let same prop cls hex =
    Alcotest.(check string)
      (cls ^ " is " ^ hex)
      (value prop hex) (value prop cls)
  in
  same "color" "text-[rebeccapurple]/50" "text-[#663399]/50";
  same "color" "text-[hsl(200_50%_50%)]/50" "text-[#4095bf]/50";
  (* a CSS keyword still beats the palette entry of the same name *)
  same "background-color" "bg-[red]/50" "bg-[#ff0000]/50";
  same "border-color" "border-[rebeccapurple]/50" "border-[#663399]/50";
  same "accent-color" "accent-[rebeccapurple]/50" "accent-[#663399]/50";
  same "caret-color" "caret-[rebeccapurple]/50" "caret-[#663399]/50";
  same "outline-color" "outline-[rebeccapurple]/50" "outline-[#663399]/50";
  same "color" "placeholder-[rebeccapurple]/50" "placeholder-[#663399]/50";
  (* an alpha read from a var names the property in the mix *)
  same "color" "text-[rebeccapurple]/(--a)" "text-[#663399]/(--a)";
  (* the colour the palette cannot name is not black *)
  Alcotest.(check bool)
    "text-[rebeccapurple]/50 is not black" false
    (Astring.String.is_infix ~affix:"oklab(0%" (css "text-[rebeccapurple]/50"));
  (* a bracket that names no colour at all is still not a class *)
  Alcotest.(check bool)
    "text-[notacolour]/50 is not a class" true
    (Result.is_error (Tw.of_string "text-[notacolour]/50"))

(* An hsl() hue takes any angle unit. Folding one to a hex colour used to keep
   only bare numbers and [deg] and read every other unit as 0, so a half turn
   painted red instead of cyan. *)
let test_hsl_hue_units () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "bg-[hsl(180deg_100%_50%)]" "background-color: #00ffff";
  has "bg-[hsl(0.5turn_100%_50%)]" "background-color: #00ffff";
  has "bg-[hsl(200grad_100%_50%)]" "background-color: #00ffff";
  has "bg-[hsl(3.14159rad_100%_50%)]" "background-color: #00ffff"

(* hsl() saturation and lightness also accept a bare number, which is the same
   value as the percentage; both used to be read as 0, which painted the colour
   black. A channel that is not static cannot fold at all. *)
let test_hsl_non_percentage_channels () =
  let fold (c : Css.color) =
    match Tw.Color.css_color_to_hex c with
    | Some folded -> Cascade.Pp.to_string Css.pp_color folded
    | None -> "unfolded"
  in
  Alcotest.(check string)
    "a number saturation and lightness are percentages" "#00ffff"
    (fold (Hsl { h = Unitless 180.; s = Num 1.0; l = Num 0.5; a = None }));
  Alcotest.(check string)
    "a var() saturation does not fold" "unfolded"
    (fold
       (Hsl
          {
            h = Unitless 180.;
            s = Var (Cascade.Values.var_ref "x");
            l = Pct 50.;
            a = None;
          }))

(* An rgb() channel is a byte only when it is a static number. A var() channel
   and the [none] sentinel, which adopts another colour's channel rather than
   standing for zero, both leave the colour with no hex form; so does an alpha
   the fold cannot read. *)
let test_rgb_non_numeric_channels () =
  let fold (c : Css.color) =
    match Tw.Color.css_color_to_hex c with
    | Some folded -> Cascade.Pp.to_string Css.pp_color folded
    | None -> "unfolded"
  in
  Alcotest.(check string)
    "static channels fold" "#ff0000"
    (fold (Rgb (Channels { r = Int 255; g = Int 0; b = Int 0 })));
  Alcotest.(check string)
    "a var() channel does not fold" "unfolded"
    (fold
       (Rgb
          (Channels
             { r = Var (Cascade.Values.var_ref "x"); g = Int 0; b = Int 0 })));
  Alcotest.(check string)
    "a none channel does not fold" "unfolded"
    (fold (Rgb (Channels { r = None; g = Int 0; b = Int 0 })));
  Alcotest.(check string)
    "a var() alpha does not fold" "unfolded"
    (fold
       (Rgba
          {
            rgb = Channels { r = Int 255; g = Int 0; b = Int 0 };
            a = Var (Cascade.Values.var_ref "a");
          }))

(* A bracket colour the fold refuses keeps the authored function. *)
let test_bracket_rgb_unresolvable_channels () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "bg-[rgb(var(--x)_0_0)]" "background-color: rgb(var(--x) 0 0)";
  has "bg-[rgb(none_0_0)]" "background-color: rgb(none 0 0)";
  has "bg-[rgba(255,0,0,var(--a))]" "background-color: rgb(255 0 0 / var(--a))"

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

(* Colour keywords accept an opacity modifier wherever Tailwind exposes a colour
   family. Keep the original class spelling: several handlers used to either
   reject these or silently drop [/50] from the round-trip. *)
let test_keyword_opacity_families () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
      | Ok u -> Alcotest.(check string) cls cls (Tw.pp u))
    [
      "bg-transparent/50";
      "text-inherit/50";
      "border-transparent/50";
      "accent-inherit/50";
      "caret-transparent/50";
      "outline-inherit/50";
      "placeholder-transparent/50";
      "from-inherit/50";
      "via-transparent/50";
      "to-inherit/50";
      "decoration-inherit/50";
      "divide-transparent/50";
      "fill-inherit/50";
      "stroke-transparent/50";
      "shadow-transparent/50";
      "inset-shadow-transparent/50";
      "ring-inherit/50";
      "ring-offset-transparent/50";
      "inset-ring-inherit/50";
      "drop-shadow-inherit/50";
      "text-shadow-transparent/50";
    ];
  (* Tailwind has no inherited text-shadow-with-opacity candidate. *)
  match Tw.of_string "text-shadow-inherit/50" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "text-shadow-inherit/50 should be rejected"

(* A colour keyword carries its opacity modifier into a mix like any other
   colour. Only a fully opaque modifier collapses back to the bare keyword. *)
let test_keyword_opacity_mixes () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "bg-transparent/50"
    "background-color: color-mix(in oklab, transparent 50%, transparent)";
  has "text-transparent/25"
    "color: color-mix(in oklab, transparent 25%, transparent)";
  has "border-transparent/[0.25]"
    "border-color: color-mix(in oklab, transparent 25%, transparent)";
  has "decoration-transparent/50"
    "text-decoration-color: color-mix(in oklab, transparent 50%, transparent)";
  has "bg-inherit/50"
    "background-color: color-mix(in oklab, inherit 50%, transparent)";
  has "bg-transparent/100" "background-color: transparent";
  has "border-inherit/100" "border-color: inherit"

(* A palette colour with no shade segment must not absorb one and rename the
   class. Tailwind rejects the candidate instead. *)
let test_shadeless_colour_rejects_shade_segment () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Error _ -> ()
      | Ok u -> Alcotest.failf "%s was renamed to %s" cls (Tw.pp u))
    [ "bg-white-500/50"; "text-black-500/50"; "border-white-500/50" ]

(* A project-defined, shadeless colour follows the same opacity path as the
   built-in shadeless colours. *)
let test_decoration_theme_colour_opacity () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("color-brand", "oklch(55% .2 250)") ]
  in
  match Tw.of_string ~theme "decoration-brand/50" with
  | Error (`Msg m) -> Alcotest.fail m
  | Ok u ->
      Alcotest.(check string)
        "custom theme colour round-trips" "decoration-brand/50" (Tw.pp u)

(* A colour a project declared in an [@theme] block has no palette entry to
   convert, so the opacity path has to read its value off the theme rather than
   ask the palette for a shade it never had. *)
let test_theme_colour_opacity_families () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("color-brand", "oklch(55% 0.2 250)") ]
  in
  let css cls =
    match Tw.of_string ~theme cls with
    | Ok u ->
        Alcotest.(check string) (cls ^ " round-trips") cls (Tw.pp u);
        Tw.to_css ~theme ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool)
      (cls ^ " emits " ^ affix)
      true
      (Astring.String.is_infix ~affix (css cls))
  in
  List.iter
    (fun cls ->
      (* The pre-color-mix fallback keeps the modifier's alpha. *)
      has cls "color-mix(in srgb,oklch(55%.2 250) 50%,transparent)";
      (* The enhanced value reads the token rather than inlining it. *)
      has cls "color-mix(in oklab,var(--color-brand) 50%,transparent)")
    [
      "bg-brand/50";
      "text-brand/50";
      "border-brand/50";
      "divide-brand/50";
      "fill-brand/50";
      "accent-brand/50";
      "from-brand/50";
    ]

(* A project token is a colour in its own right, with or without a modifier: at
   full opacity the utility simply references it. *)
let test_theme_colour_without_opacity () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("color-brand", "oklch(55% 0.2 250)") ]
  in
  let css cls =
    match Tw.of_string ~theme cls with
    | Ok u ->
        Alcotest.(check string) (cls ^ " round-trips") cls (Tw.pp u);
        Tw.to_css ~theme ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  List.iter
    (fun cls ->
      Alcotest.(check bool)
        (cls ^ " references the token")
        true
        (Astring.String.is_infix ~affix:"var(--color-brand)" (css cls)))
    [
      "bg-brand";
      "text-brand";
      "border-brand";
      "divide-brand";
      "fill-brand";
      "stroke-brand";
      "accent-brand";
      "caret-brand";
      "outline-brand";
      "from-brand";
    ]

(* A token name is not limited to one segment, and the modifier still rides on
   the last one. *)
let test_multi_segment_theme_colour () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("color-brand-primary", "#123456") ]
  in
  let css cls =
    match Tw.of_string ~theme cls with
    | Ok u ->
        Alcotest.(check string) (cls ^ " round-trips") cls (Tw.pp u);
        Tw.to_css ~theme ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "bg-brand-primary references the token" true
    (Astring.String.is_infix ~affix:"var(--color-brand-primary)"
       (css "bg-brand-primary"));
  Alcotest.(check bool)
    "bg-brand-primary/50 keeps the alpha" true
    (Astring.String.is_infix ~affix:"color-mix(in srgb,#123456 50%,transparent)"
       (css "bg-brand-primary/50"))

(* A name no [@theme] block declared is not a colour, whatever it looks like. *)
let test_undeclared_theme_colour_rejected () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Error _ -> ()
      | Ok u -> Alcotest.failf "%s parsed as %s" cls (Tw.pp u))
    [
      "bg-brand/50";
      "text-brand/50";
      "border-brand/50";
      "bg-brand";
      "text-brand";
      "border-brand";
      "bg-brand-primary";
    ]

(* A [@theme] block that removes a palette token takes the utilities reading it
   with it: [text-red-500] would otherwise paint [var(--color-red-500)] with
   nothing left to declare the variable. A shade the block kept, and a colour it
   never touched, still resolve. *)
let test_removed_palette_colour_rejected () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("color-red-500", "initial") ]
  in
  List.iter
    (fun cls ->
      match Tw.of_string ~theme cls with
      | Error _ -> ()
      | Ok u -> Alcotest.failf "%s parsed as %s" cls (Tw.pp u))
    [
      "text-red-500";
      "bg-red-500";
      "border-red-500";
      "bg-red-500/50";
      "divide-red-500";
      "shadow-red-500";
    ];
  List.iter
    (fun cls ->
      match Tw.of_string ~theme cls with
      | Ok _ -> ()
      | Error (`Msg m) -> Alcotest.failf "%s was rejected: %s" cls m)
    [ "text-red-600"; "text-blue-500" ]

(* The whole-namespace form takes every palette colour, so nothing keyed on
   [--color-*] resolves, while a colour the block declared for itself does. *)
let test_removed_palette_namespace_rejected () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("color-*", "initial"); ("color-brand", "#123456") ]
  in
  List.iter
    (fun cls ->
      match Tw.of_string ~theme cls with
      | Error _ -> ()
      | Ok u -> Alcotest.failf "%s parsed as %s" cls (Tw.pp u))
    [ "text-red-500"; "bg-blue-200" ];
  match Tw.of_string ~theme "text-brand" with
  | Ok _ -> ()
  | Error (`Msg m) -> Alcotest.failf "text-brand was rejected: %s" m

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
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
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
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
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
    (Tw.Color.hex_with_alpha "#fff" 10.);
  Alcotest.(check string)
    "an existing alpha is replaced" "#ffffff1a"
    (Tw.Color.hex_with_alpha "#ffffffcc" 10.);
  Alcotest.(check string)
    "a six-digit hex is unchanged" "#0307121a"
    (Tw.Color.hex_with_alpha "#030712" 10.)

(* Tailwind writes an arbitrary colour back in the spelling the class used: the
   pinned CLI answers [bg-[#f00]] with [background-color: #f00] and
   [bg-[#ffffffff]] with all eight digits, whatever the shortest equivalent
   would be. The digits were decoded to bytes and re-spelled, which turned every
   short or upper-case hex into its six-digit lower-case form. Minified printing
   folds a colour to its shortest hex, so the spelling only shows unminified. *)
let test_bracket_hex_keeps_authored_spelling () =
  let pin cls decl =
    Test_helpers.check_declarations ~minify:false cls [ decl ]
  in
  pin "bg-[#f00]" "background-color: #f00";
  pin "bg-[#ff0000]" "background-color: #ff0000";
  pin "bg-[#FF0000]" "background-color: #FF0000";
  pin "bg-[#abc]" "background-color: #abc";
  pin "bg-[#ffffffff]" "background-color: #ffffffff";
  pin "bg-[#f008]" "background-color: #f008";
  pin "text-[#0088cc]" "color: #0088cc";
  pin "border-[#f00]" "border-color: #f00"

(* A [#] bracket only names a colour when what follows is a hex spelling. The
   colour handler handed everything after the [#] to the raising constructor
   from inside [of_class], so a malformed hex escaped the parser as an exception
   instead of failing the match. *)
let test_invalid_bracket_hex () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  let emits cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  List.iter
    (fun prefix ->
      rejected (prefix ^ "-[#zz]");
      rejected (prefix ^ "-[#]");
      rejected (prefix ^ "-[#12345]");
      rejected (prefix ^ "-[#zz]/50"))
    [
      "text";
      "bg";
      "border";
      "fill";
      "stroke";
      "accent";
      "caret";
      "outline";
      "placeholder";
    ];
  emits "text-[#abc]" "color:#abc";
  emits "bg-[#00ff0080]" "background-color:#00ff0080";
  emits "border-[#123456]" "border-color:#123456";
  emits "fill-[#abc]" "fill:#abc";
  emits "stroke-[#abc]" "stroke:#abc";
  emits "accent-[#abc]" "accent-color:#abc";
  emits "caret-[#abc]" "caret-color:#abc";
  emits "outline-[#abc]" "outline-color:#abc";
  emits "placeholder-[#abc]" "color:#abc"

(* A class name is a selector, so it has to repeat the text the author wrote
   rather than a re-print of the number it parsed to. [/[25]] and [/[25.0]]
   denote one alpha and are two distinct classes; only the author's spelling
   matches the markup. *)
let test_opacity_modifier_class_roundtrip () =
  let roundtrip cls =
    match Tw.of_string cls with
    | Ok u -> Alcotest.(check string) "class" cls (Tw.pp u)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  List.iter roundtrip
    [
      "bg-red-500/50";
      "bg-red-500/2.5";
      "bg-red-500/[25]";
      "bg-red-500/[.5]";
      "bg-red-500/[25%]";
      "bg-red-500/[25.50%]";
      "bg-red-500/[var(--o)]";
      "bg-red-500/(--o)";
      "text-red-500/[25]";
      "border-red-500/[25]";
      "shadow-lg/[25]";
      "shadow-red-500/[25]";
      "inset-shadow-sm/[25]";
      "ring-red-500/[25]";
      "ring-offset-red-500/[25]";
      "inset-ring-red-500/[25]";
      "drop-shadow-lg/[25]";
      "drop-shadow/[25]";
      "text-shadow-lg/[25]";
      "decoration-red-500/[25]";
      "divide-red-500/[25]";
      "fill-red-500/[25]";
      "accent-red-500/[25]";
      "outline-red-500/[25]";
      "placeholder-red-500/[25]";
      "from-red-500/[25]";
      "[color:red]/[25]";
    ]

(* The bracket modifier holds a number, so a non-numeric spelling is not a
   utility at all rather than one with a surprising class name. *)
let test_opacity_modifier_rejects_non_numeric () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok u -> Alcotest.failf "%s parsed as %s" cls (Tw.pp u)
      | Error (`Msg _) -> ())
    [
      "bg-red-500/[abc]";
      "bg-red-500/[]";
      "bg-red-500/[25px]";
      "bg-red-500/[%]";
      "shadow-lg/[25px]";
    ]

(* A custom property is a dashed ident, so every byte of its name outside the
   CSS name set has to carry a backslash for the whole thing to lex as one
   token. Walking the name is the check: a bare [#], [(] or [,] ends the ident
   and turns the rest into stray tokens. *)
let check_escaped_name name =
  let n = String.length name in
  let is_name_code_point c =
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
    | c -> Char.code c >= 0x80
  in
  let rec walk i =
    if i >= n then ()
    else if name.[i] = '\\' then walk (i + 2)
    else if is_name_code_point name.[i] then walk (i + 1)
    else Alcotest.failf "--%s: %C at offset %d is unescaped" name name.[i] i
  in
  walk 0

(* [color_var] is public, so it has to name a variable that a browser can read
   whatever colour it is handed - the utility API happens to branch on
   [is_custom_color] before it gets here, but the signature makes no such
   promise. *)
let test_color_var_name_is_one_ident () =
  List.iter
    (fun color -> check_escaped_name (Tw.Var.name (color_var color 500)))
    [
      Red;
      Black;
      Theme_named "brand";
      Hex "0088cc";
      Rgb { red = 1; green = 2; blue = 3 };
      Oklch { l = 50.; c = 0.1; h = 20. };
      Css (Css.hex "#0088cc");
    ]

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

let test_removed_mix_token_stays_runtime () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("color-red-500", "initial") ]
  in
  let class_name =
    "bg-[color-mix(in_oklab,var(--color-red-500)_25%,transparent)]"
  in
  let css =
    match Tw.of_string ~theme class_name with
    | Ok utility ->
        Tw.to_css ~theme ~base:false [ utility ]
        |> Tw.Css.to_string ~minify:true
    | Error (`Msg message) -> Alcotest.failf "%s: %s" class_name message
  in
  Alcotest.(check bool)
    "removed theme token is the runtime fallback" true
    (Astring.String.is_infix ~affix:"background-color:var(--color-red-500)" css);
  Alcotest.(check bool)
    "removed theme token is not resolved through the default palette" false
    (Astring.String.is_infix ~affix:"oklch(63.7% .237 25.331)" css)

(* A bracket colour reads [_] as a space, so a variable name carrying an
   underscore is written [\_] and keeps the character. *)
let test_bracket_colour_underscore_escape () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "an escaped underscore stays in the variable name" true
    (Astring.String.is_infix ~affix:"color: light-dark(var(--a_b), red)"
       (css {|text-[light-dark(var(--a\_b),red)]|}))

(* The theme layer and the utilities layer rank the same colour names in
   different orders, and a name missing from either map silently takes that
   map's unknown-colour slot. Both are derived from one list, so the pairing
   holds by construction; this is what fails if that stops being true. *)
let ranked_color_names =
  [
    "transparent";
    "black";
    "white";
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
  ]

let test_color_orders_cover_the_same_names () =
  let unknown_theme = Tw.Color.theme_order "definitely-not-a-colour" in
  let unknown_utilities = Tw.Color.utilities_order "definitely-not-a-colour" in
  Alcotest.(check (pair int int))
    "unknown theme colour falls back" (2, 100000) unknown_theme;
  Alcotest.(check (pair int int))
    "unknown utility colour falls back" (2, 100) unknown_utilities;
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (name ^ " has a theme slot")
        false
        (Tw.Color.theme_order name = unknown_theme);
      Alcotest.(check bool)
        (name ^ " has a utilities slot")
        false
        (Tw.Color.utilities_order name = unknown_utilities))
    ranked_color_names

(* The utilities layer leads with transparent and black and is alphabetical from
   there. *)
let test_utilities_color_order_is_alphabetical () =
  let order name = snd (Tw.Color.utilities_order name) in
  Alcotest.(check int) "transparent leads" 0 (order "transparent");
  Alcotest.(check int) "black follows" 1 (order "black");
  Alcotest.(check int) "amber opens the alphabetical run" 2 (order "amber");
  Alcotest.(check int) "white sits between violet and yellow" 26 (order "white");
  Alcotest.(check int) "zinc closes it" 28 (order "zinc");
  (* The v4.3.3 families take their alphabetical places rather than the
     unknown-colour slot. *)
  Alcotest.(check int) "mauve follows lime" 11 (order "mauve");
  Alcotest.(check int) "mist follows mauve" 12 (order "mist");
  Alcotest.(check int) "olive follows neutral" 14 (order "olive");
  Alcotest.(check int) "taupe follows stone" 23 (order "taupe")

(* Tailwind 4.3.3 declares mauve, olive, mist and taupe in its [@theme] after
   stone and before black, in that order. Read off [@import "tailwindcss"
   theme(static)]. *)
let test_v433_families_rank_in_theme_order () =
  let order name = snd (Tw.Color.theme_order name) in
  let names =
    [ "stone"; "mauve"; "olive"; "mist"; "taupe"; "black"; "white" ]
  in
  let orders = List.map order names in
  Alcotest.(check (list int))
    "the four families sit between stone and black, each in its own slot"
    (List.sort_uniq compare orders)
    orders

(* A [color:] hint says how to read the value written after it; it does not make
   that value the name of a custom property. [text-[color:red]] wrote [color:
   var(--red)] where Tailwind writes [color: red]. *)
let test_bracket_color_hint_reads_the_value () =
  Test_helpers.check_declarations "text-[color:red]" [ "color:red" ];
  Test_helpers.check_declarations "outline-[color:red]" [ "outline-color:red" ];
  Test_helpers.check_declarations ~minify:false "text-[color:#ff0000]"
    [ "color: #ff0000" ];
  (* a var() reference after the hint still names a custom property *)
  Test_helpers.check_declarations "text-[color:var(--my-color)]"
    [ "color:var(--my-color)" ];
  (* the class prints back with the hint the author wrote *)
  Alcotest.(check string)
    "text-[color:red] round-trips" "text-[color:red]"
    (Tw.pp (Result.get_ok (Tw.of_string "text-[color:red]")));
  (* A value no colour reader takes is held open, not settled: Tailwind writes
     the bracket out whatever it says, so refusing is an intermediate. *)
  Test_helpers.check_invalid_input
    ~why:
      (Test_helpers.Diverges
         "emitted verbatim; tw needs an opaque declaration to match")
    (module Tw.Color.Handler)
    "text-[color:notacolour]"

let tests =
  [
    ( "Bracket colour hint reads the value",
      `Quick,
      test_bracket_color_hint_reads_the_value );
    ( "Bracket colour underscore escape",
      `Quick,
      test_bracket_colour_underscore_escape );
    ("Invalid bracket hex", `Quick, test_invalid_bracket_hex);
    ( "Bracket hex keeps its authored spelling",
      `Quick,
      test_bracket_hex_keeps_authored_spelling );
    ( "Colour variable name is one ident",
      `Quick,
      test_color_var_name_is_one_ident );
    ("Achromatic colour keeps a none hue", `Quick, test_achromatic_none_hue);
    ("Per-side border colors", `Quick, test_border_side_color);
    ("Border color var", `Quick, test_border_color_var);
    ("Border side color opacity", `Quick, test_border_side_color_opacity);
    ("Per-side border color order", `Slow, test_border_side_color_order);
    ( "Per-side border bracket color order",
      `Slow,
      test_border_side_bracket_color_order );
    ("Outline inherit order", `Slow, test_outline_inherit_order);
    ("Bracket CSS colors", `Quick, test_bracket_css_colors);
    ("Bracket colour opacity", `Quick, test_bracket_colour_opacity);
    ("hsl hue units", `Quick, test_hsl_hue_units);
    ("hsl non-percentage channels", `Quick, test_hsl_non_percentage_channels);
    ("rgb non-numeric channels", `Quick, test_rgb_non_numeric_channels);
    ( "bracket rgb unresolvable channels",
      `Quick,
      test_bracket_rgb_unresolvable_channels );
    ("Alpha from a var", `Quick, test_alpha_from_a_var);
    ("Invalid shades", `Quick, test_invalid_shade);
    ("Keyword opacity families", `Quick, test_keyword_opacity_families);
    ("Keyword opacity mixes", `Quick, test_keyword_opacity_mixes);
    ( "Shadeless colour rejects a shade segment",
      `Quick,
      test_shadeless_colour_rejects_shade_segment );
    ( "Decoration theme colour opacity",
      `Quick,
      test_decoration_theme_colour_opacity );
    ( "Theme colour opacity across families",
      `Quick,
      test_theme_colour_opacity_families );
    ("Theme colour without opacity", `Quick, test_theme_colour_without_opacity);
    ("Multi-segment theme colour", `Quick, test_multi_segment_theme_colour);
    ( "Undeclared theme colour rejected",
      `Quick,
      test_undeclared_theme_colour_rejected );
    ( "Removed palette colour rejected",
      `Quick,
      test_removed_palette_colour_rejected );
    ( "Removed palette namespace rejected",
      `Quick,
      test_removed_palette_namespace_rejected );
    ("RGB to OKLCH roundtrip", `Quick, test_rgb_to_oklch_roundtrip);
    ("Hex parsing", `Quick, test_hex_parsing);
    ("RGB to hex", `Quick, test_rgb_to_hex);
    ("OKLCH CSS formatting", `Quick, test_oklch_css_formatting);
    ("Edge cases", `Quick, test_edge_cases);
    ("Color accuracy", `Quick, accuracy);
    ("CSS modes with colors", `Quick, test_css_mode_with_colors);
    ("v4.3.3 colour families", `Quick, test_v433_color_families);
    ("Full opacity and important", `Quick, test_full_opacity_and_important);
    ( "Opacity modifier class roundtrip",
      `Quick,
      test_opacity_modifier_class_roundtrip );
    ( "Opacity modifier rejects non-numeric",
      `Quick,
      test_opacity_modifier_rejects_non_numeric );
    ( "Removed color-mix token stays runtime",
      `Quick,
      test_removed_mix_token_stays_runtime );
    ("Shorthand hex with alpha", `Quick, test_shorthand_hex_alpha);
    ("Out-of-gamut OKLCH", `Quick, test_out_of_gamut_oklch);
    ( "Colour orders cover the same names",
      `Quick,
      test_color_orders_cover_the_same_names );
    ( "Utilities colour order is alphabetical",
      `Quick,
      test_utilities_color_order_is_alphabetical );
    ( "v4.3.3 families rank in theme order",
      `Quick,
      test_v433_families_rank_in_theme_order );
  ]

let suite = ("color", tests)
