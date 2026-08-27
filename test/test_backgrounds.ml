open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Backgrounds.Handler)

let test_bg_colors () =
  check "bg-red-500";
  check "bg-blue-600";
  check "bg-green-700"

let test_gradient_direction () =
  let u = Tw.Backgrounds.bg_gradient_to Tw.Backgrounds.Bottom in
  Alcotest.check string "bg-gradient-to-b" "bg-gradient-to-b"
    (Tw.Utility.to_class u)

let test_gradient_colors () =
  let open Tw in
  let from = Backgrounds.from_color Color.red in
  let via = Backgrounds.via_color Color.blue ~shade:600 in
  let to_ = Backgrounds.to_color Color.green in
  Alcotest.check string "from-red-500" "from-red-500" (Utility.to_class from);
  Alcotest.check string "via-blue-600" "via-blue-600" (Utility.to_class via);
  Alcotest.check string "to-green-500" "to-green-500" (Utility.to_class to_)

(* via-none clears the gradient's via stops by resetting the channel var to the
   CSS initial keyword. *)
let test_via_none () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.check string "via-none round-trips" "via-none"
    (Tw.pp (Result.get_ok (Tw.of_string "via-none")));
  Alcotest.(check bool)
    "via-none sets --tw-gradient-via-stops:initial" true
    (Astring.String.is_infix ~affix:"--tw-gradient-via-stops:initial"
       (css "via-none"))

(* Bare bg-radial / bg-conic (and bg-conic-{angle}) set --tw-gradient-position
   to the default oklab interpolation and the matching gradient image; they used
   to be unknown classes (only the /interp and bracket forms were handled). *)
let test_radial_conic () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "bg-radial emits radial-gradient" true
    (Astring.String.is_infix ~affix:"radial-gradient(var(--tw-gradient-stops))"
       (css "bg-radial"));
  Alcotest.(check bool)
    "bg-conic emits conic-gradient" true
    (Astring.String.is_infix ~affix:"conic-gradient(var(--tw-gradient-stops))"
       (css "bg-conic"));
  Alcotest.(check bool)
    "bg-radial sets position in oklab" true
    (Astring.String.is_infix ~affix:"--tw-gradient-position: in oklab"
       (css "bg-radial"));
  Alcotest.(check bool)
    "bg-conic-180 sets from 180deg" true
    (Astring.String.is_infix
       ~affix:"--tw-gradient-position: from 180deg in oklab"
       (css "bg-conic-180"))

let test_of_string_invalid () =
  (* Invalid background utilities *)
  let test_invalid =
    Test_helpers.check_invalid_parts (module Tw.Backgrounds.Handler)
  in

  (* Invalid gradient direction *)
  test_invalid [ "bg"; "gradient"; "to" ];
  (* Missing direction *)
  test_invalid [ "bg"; "gradient"; "to"; "invalid" ];
  (* Invalid direction *)
  test_invalid [ "bg"; "gradient"; "to"; "x" ];

  (* Invalid direction *)

  (* Invalid from/via/to colors *)
  test_invalid [ "from" ];
  (* Missing color *)
  test_invalid [ "from"; "invalid" ];
  (* Invalid color *)
  test_invalid [ "via" ];
  (* Missing color *)
  test_invalid [ "via"; "notacolor" ];
  (* Invalid color *)
  test_invalid [ "to" ];
  (* Missing color *)
  test_invalid [ "to"; "xyz" ];

  (* Invalid color *)

  (* Invalid prefixes *)
  test_invalid [ "bg" ];
  (* Incomplete *)
  test_invalid [ "bg"; "gradient" ];
  (* Incomplete *)
  test_invalid [ "unknown"; "red" ]
(* Unknown prefix *)

(* bg-[image:<gradient>] emits the literal background-image; it used to wrap the
   value in a bogus var(--radial-gradient(...)). var() and url() image values
   are unchanged. *)
(* Regression: keyword background-size values under a [length:...] hint
   (bg-[length:cover]) used to fall through to background-size:auto because
   parse_bracket_size only handled numeric lengths. *)
let test_bracket_length_keywords () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "bg-[length:cover] emits background-size: cover" true
    (Astring.String.is_infix ~affix:"background-size: cover"
       (css "bg-[length:cover]"));
  Alcotest.(check bool)
    "bg-[length:contain] emits background-size: contain" true
    (Astring.String.is_infix ~affix:"background-size: contain"
       (css "bg-[length:contain]"))

(* A two-axis bg-position bracket mixes a keyword edge with a length, e.g.
   bg-position-[center_-100px] -> background-position: 50% -100px. *)
let test_bg_position_bracket_keyword_length () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "bg-position-[center_-100px] keeps both axes" true
    (Astring.String.is_infix ~affix:"background-position: 50% -100px"
       (css "bg-position-[center_-100px]"));
  Alcotest.(check bool)
    "bg-position-[left_top] keeps the edge keywords" true
    (Astring.String.is_infix ~affix:"background-position: left top"
       (css "bg-position-[left_top]"))

(* A background-position bracket takes the whole CSS grammar: a single edge
   keyword, and the four-value edge/offset form. Both used to fall through the
   hand-rolled parser to a silent [center]. *)
let test_bracket_position_grammar () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "bg-[position:top]" "background-position: top";
  has "bg-[position:left_10px_top_20px]"
    "background-position: left 10px top 20px";
  has "bg-position-[top]" "background-position: top";
  has "bg-[top]" "background-position: top";
  (* the lengths form is unchanged *)
  has "bg-[position:120px_120px]" "background-position: 120px 120px";
  has "bg-position-[center_-100px]" "background-position: 50% -100px"

(* A bracket value the property cannot take is not a utility. [bg-[image:...]]
   used to emit an empty rule and [bg-[position:...]] a plausible-looking
   [center]: no CSS the class asked for, and no diagnostic. *)
let test_invalid_bracket_value () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  let accepted cls =
    match Tw.of_string cls with
    | Ok _ -> ()
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  rejected "bg-[image:nope]";
  rejected "bg-[position:nope]";
  accepted "bg-[image:radial-gradient(white,black)]";
  accepted "bg-[image:var(--x)]";
  accepted "bg-[image:url(/a.png)]";
  accepted "bg-[position:120px_120px]"

let test_bracket_image_literal () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "bg-[image:radial-gradient(...)] emits the literal gradient" true
    (Astring.String.is_infix ~affix:"background-image: radial-gradient("
       (css "bg-[image:radial-gradient(white,black)]"));
  Alcotest.(check bool)
    "no bogus var() wrapping" false
    (Astring.String.is_infix ~affix:"var(--radial-gradient"
       (css "bg-[image:radial-gradient(white,black)]"));
  Alcotest.(check bool)
    "bg-[image:var(--x)] still references the var" true
    (Astring.String.is_infix ~affix:"background-image: var(--x)"
       (css "bg-[image:var(--x)]"))

(* An arbitrary gradient angle in radians is converted to degrees. A negative
   angle used to come out as its floor plus a positive fraction, so
   bg-linear-[-0.5rad] rendered -29.3521deg instead of -28.6479deg. *)
let test_bracket_gradient_radians () =
  let css_of cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css_of cls))
  in
  has "bg-linear-[-0.5rad]" "--tw-gradient-position: -28.6479deg";
  has "bg-linear-[1.3rad]" "--tw-gradient-position: 74.4845deg"

(* [-bg-linear-[value]] accepts only angles, gated by a check that used to spell
   "is this an angle" as [String.ends_with ~suffix:"rad" value]. That suffix
   also matches "grad" (gradians end in "rad" too), so it stripped "100grad"
   down to "100g", failed to read it as a number, and rejected the class
   outright where Tailwind accepts it and negates the value as calc(100grad *
   -1). Reading the bracket as a real CSS angle tells grad and rad apart; a
   non-angle bracket like [to_bottom] still has to be rejected. *)
let test_bracket_gradient_negated_angle_units () =
  let css_of cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css_of cls))
  in
  has "-bg-linear-[100grad]" "calc(100grad * -1)";
  match Tw.of_string "-bg-linear-[to_bottom]" with
  | Ok _ -> Alcotest.fail "expected -bg-linear-[to_bottom] to be rejected"
  | Error _ -> ()

let suborder_matches_tailwind () =
  let open Tw in
  let colors = [ red; blue; green; yellow; purple; pink ] in
  let shades = [ 50; 100; 200; 300; 400; 500; 600; 700; 800; 900 ] in
  let utilities =
    List.concat_map
      (fun color -> List.map (fun shade -> bg ~shade color) shades)
      colors
  in
  let shuffled = Test_helpers.shuffle utilities in

  Test_helpers.check_ordering_matches
    ~test_name:"backgrounds suborder matches Tailwind" shuffled

(* Tailwind's property table runs the background properties in two stretches:
   background-color and background-image with the gradient variables come first,
   then background-size through background-origin, which sit between the
   mask-image utilities and mask-composite. The masks have to interleave with
   that second stretch. *)
let order_matches_tailwind () =
  let classes =
    [
      "bg-red-500";
      "bg-linear-to-r";
      "bg-conic";
      "bg-none";
      "via-none";
      "from-red-500";
      "to-90%";
      "mask-t-from-50%";
      "mask-circle";
      "mask-none";
      "bg-cover";
      "bg-fixed";
      "bg-clip-text";
      "bg-center";
      "bg-repeat-x";
      "bg-origin-border";
      "mask-add";
      "mask-alpha";
      "mask-cover";
      "mask-top";
      "mask-repeat-x";
      "mask-origin-border";
    ]
  in
  Test_helpers.check_class_order
    ~test_name:"background and mask order matches Tailwind" classes

(* A gradient and a background colour both end up in background-image and
   background-color, and the gradient stops share the --tw-gradient-* slots.
   Palette colours are left out: tw declares the theme token as a hex where
   Tailwind keeps oklch, which [tw --diff] already reports on its own. *)
let rendering_matches_tailwind () =
  let classes =
    [
      "bg-current";
      "bg-transparent";
      "bg-black";
      "bg-white";
      "bg-linear-to-r";
      "bg-linear-to-b";
      "from-current";
      "via-transparent";
      "to-black";
      "from-50%";
      "bg-cover";
      "bg-contain";
      "bg-center";
      "bg-top";
      "bg-no-repeat";
      "bg-repeat-x";
      "bg-fixed";
      "bg-local";
    ]
  in
  Test_helpers.check_rendering_matches
    ~test_name:"backgrounds render like Tailwind"
    (List.map (fun c -> Result.get_ok (Tw.of_string c)) classes)

(* An arbitrary url() with its own quotes must not be double-wrapped: tw used to
   emit the broken url("'/img/x.png'"); it now canonicalises to a valid
   url(). *)
let test_bg_arbitrary_url () =
  let css_of cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error _ -> Alcotest.failf "could not parse %S" cls
  in
  List.iter
    (fun cls ->
      let css = css_of cls in
      Alcotest.(check bool)
        (cls ^ " emits a valid url()")
        true
        (Astring.String.is_infix ~affix:"url(/img/x.png)" css);
      Alcotest.(check bool)
        (cls ^ " does not double-quote")
        false
        (Astring.String.is_infix ~affix:"url(\"'" css))
    [
      "bg-[url('/img/x.png')]";
      "bg-[url(\"/img/x.png\")]";
      "bg-[url(/img/x.png)]";
      (* The image: data-type hint forces background-image; the url() must not
         be mis-read as a var (it used to emit var(--url(/img/x.png))). *)
      "bg-[image:url(/img/x.png)]";
    ]

(* Arbitrary rgb()/rgba() gradient stops set the gradient colour rather than
   being silently dropped as a position (they used to produce no
   --tw-gradient-from). *)
let test_gradient_rgba_stop () =
  let css_of cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error _ -> Alcotest.failf "could not parse %S" cls
  in
  Alcotest.(check bool)
    "from-[rgba(...)] sets --tw-gradient-from" true
    (Astring.String.is_infix ~affix:"--tw-gradient-from"
       (css_of "from-[rgba(5,74,218,0.60)]"));
  Alcotest.(check bool)
    "to-[rgb(...)] sets --tw-gradient-to" true
    (Astring.String.is_infix ~affix:"--tw-gradient-to"
       (css_of "to-[rgb(16,26,50,0.60)]"))

(* A gradient stop-position utility (from-10%) registers the whole
   --tw-gradient-* @property family, like the colour utilities, matching the
   CLI. It used to register only the three *-position properties. *)
let test_gradient_stop_position_properties () =
  let css =
    match Tw.of_string "from-10%" with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error _ -> Alcotest.fail "could not parse from-10%"
  in
  Alcotest.(check bool)
    "from-10% sets --tw-gradient-from-position" true
    (Astring.String.is_infix ~affix:"--tw-gradient-from-position: 10%" css);
  Alcotest.(check bool)
    "from-10% registers @property --tw-gradient-from" true
    (Astring.String.is_infix ~affix:"@property --tw-gradient-from" css);
  Alcotest.(check bool)
    "from-10% registers @property --tw-gradient-stops" true
    (Astring.String.is_infix ~affix:"@property --tw-gradient-stops" css)

(* A var() background colour with an alpha modifier defers the alpha to
   color-mix: the variable's value is unknown at build time, so it cannot be
   folded into a literal colour. *)
let test_bg_var_opacity () =
  let css_of cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error _ -> Alcotest.failf "could not parse %S" cls
  in
  Alcotest.(check bool)
    "bg-[var(--x)]/50 mixes at run time" true
    (Astring.String.is_infix
       ~affix:"color-mix(in oklab, var(--x) 50%, transparent)"
       (css_of "bg-[var(--x)]/50"))

(* A gradient stop bracket is a colour or a stop position; the docs' [<value>]
   placeholder is neither, and it used to land as [0%]. *)
let test_invalid_gradient_stop () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  let accepted cls =
    match Tw.of_string cls with
    | Ok _ -> ()
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  rejected "from-[<value>]";
  rejected "via-[<value>]";
  rejected "to-[<value>]";
  accepted "from-[25%]";
  accepted "from-[var(--x)]";
  accepted "from-[#0088cc]"

(* A [#] gradient stop is only a colour when what follows is a hex spelling. The
   stop reader kept the text after the [#] as-is and the raising constructor saw
   it when the sheet was rendered, so a malformed hex escaped as an exception
   instead of failing the parse. *)
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
    [ "from"; "via"; "to" ];
  emits "from-[#fff]" "--tw-gradient-from:#fff";
  emits "via-[#abc]" "--tw-gradient-via:#abc";
  emits "to-[#123456]" "--tw-gradient-to:#123456"

(* A bracket stop position is read with the CSS length-percentage grammar, so a
   unit the reader does not name is not rendered as a zero position. *)
let test_gradient_stop_position_units () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  emits "from-[1rem]" "--tw-gradient-from-position:1rem";
  emits "via-[10vw]" "--tw-gradient-via-position:10vw";
  emits "to-[2em]" "--tw-gradient-to-position:2em";
  emits "from-[calc(10%_+_2px)]" "--tw-gradient-from-position:calc(10% + 2px)";
  (* the spellings the reader already named keep their value *)
  emits "from-[50%]" "--tw-gradient-from-position:50%";
  emits "from-[50px]" "--tw-gradient-from-position:50px";
  emits "from-[length:var(--my-position)]"
    "--tw-gradient-from-position:var(--my-position)";
  (* the class name is spelled as it was written *)
  Alcotest.(check string)
    "from-[1rem] round-trips" "from-[1rem]"
    (Tw.pp (Result.get_ok (Tw.of_string "from-[1rem]")))

(* A bracket that is not a length-percentage is not a stop position. Tailwind
   reads those as a colour, which has no typed spelling here, so the class is
   refused rather than rendered as a zero position. *)
let test_gradient_stop_position_not_a_length () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok u ->
        Alcotest.failf "expected %s to be rejected, got %s" cls
          (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true)
    | Error _ -> ()
  in
  rejected "from-[fit-content]";
  rejected "from-[none]";
  rejected "to-[max-content]";
  rejected "from-[0]";
  rejected "from-[1zz]";
  rejected "via-[12px3]"

(* A gradient interpolation modifier names a colour space. Tailwind writes an
   unknown one through as [in <space>], so only the shapes it refuses are
   refused here; the ones it took used to raise out of [to_css], and the linear
   forms silently dropped the modifier instead. *)
let test_gradient_interpolation () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok u ->
        Alcotest.failf "expected %s to be rejected, got %s" cls
          (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true)
    | Error _ -> ()
  in
  let css_of cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css_of cls))
  in
  (* A second modifier, a function, a leading dot or sign, and the empty
     modifier: Tailwind emits nothing for any of them. *)
  rejected "bg-conic-45/oklab/foo";
  rejected "bg-conic/foo(1)";
  rejected "bg-conic/.5";
  rejected "bg-conic/[]";
  rejected "bg-radial/foo(1)";
  (* An unknown colour space still names one. *)
  has "bg-conic/foo" "--tw-gradient-position: in foo";
  has "bg-conic-45/999" "--tw-gradient-position: from 45deg in 999";
  has "bg-radial/foo" "--tw-gradient-position: in foo";
  has "bg-conic/oklab" "--tw-gradient-position: in oklab";
  has "bg-conic/shorter" "--tw-gradient-position: in oklch shorter hue";
  has "bg-conic/[in_hsl_longer_hue]" "--tw-gradient-position: in hsl longer hue";
  (* The linear forms carry the modifier into the @supports rule rather than
     dropping it. *)
  has "bg-linear-45/foo" "--tw-gradient-position: 45deg in foo";
  has "bg-linear-to-r/foo" "--tw-gradient-position: to right in foo"

let tests =
  [
    test_case "gradient interpolation" `Quick test_gradient_interpolation;
    test_case "gradient stop position units" `Quick
      test_gradient_stop_position_units;
    test_case "gradient stop position is a length-percentage" `Quick
      test_gradient_stop_position_not_a_length;
    test_case "invalid bracket hex" `Quick test_invalid_bracket_hex;
    test_case "bg colors" `Quick test_bg_colors;
    test_case "invalid gradient stop" `Quick test_invalid_gradient_stop;
    test_case "bg var color with opacity" `Quick test_bg_var_opacity;
    test_case "bg arbitrary url quoting" `Quick test_bg_arbitrary_url;
    test_case "arbitrary rgba gradient stop" `Quick test_gradient_rgba_stop;
    test_case "gradient stop-position @property family" `Quick
      test_gradient_stop_position_properties;
    test_case "gradient direction" `Quick test_gradient_direction;
    test_case "bracket gradient angle in radians" `Quick
      test_bracket_gradient_radians;
    test_case "negated bracket gradient angle units" `Quick
      test_bracket_gradient_negated_angle_units;
    test_case "bracket image literal" `Quick test_bracket_image_literal;
    test_case "bracket length keywords" `Quick test_bracket_length_keywords;
    test_case "bg-position bracket keyword+length" `Quick
      test_bg_position_bracket_keyword_length;
    test_case "bracket position grammar" `Quick test_bracket_position_grammar;
    test_case "invalid bracket value" `Quick test_invalid_bracket_value;
    test_case "bare radial and conic gradients" `Quick test_radial_conic;
    test_case "gradient colors" `Quick test_gradient_colors;
    test_case "via-none" `Quick test_via_none;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
    test_case "backgrounds suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "background and mask order matches Tailwind" `Slow
      order_matches_tailwind;
    test_case "backgrounds render like Tailwind" `Slow
      rendering_matches_tailwind;
  ]

let suite = ("backgrounds", tests)
