open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Backgrounds.Handler)

(* The stop list a from- or to- colour writes beside its own channel: the via
   slot is left to its fallback, which is the whole list without it. *)
let stops =
  "--tw-gradient-stops: var(--tw-gradient-via-stops, \
   var(--tw-gradient-position), var(--tw-gradient-from) \
   var(--tw-gradient-from-position), var(--tw-gradient-to) \
   var(--tw-gradient-to-position))"

(* A via- colour fills that slot instead, so it writes the list out and points
   --tw-gradient-stops at it. *)
let via_stops =
  "--tw-gradient-via-stops: var(--tw-gradient-position), \
   var(--tw-gradient-from) var(--tw-gradient-from-position), \
   var(--tw-gradient-via) var(--tw-gradient-via-position), \
   var(--tw-gradient-to) var(--tw-gradient-to-position)"

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
  Alcotest.check string "via-none round-trips" "via-none"
    (Tw.pp (Result.get_ok (Tw.of_string "via-none")));
  (* The whole list: clearing the via stops is all the utility does, so a second
     declaration beside it would be the regression. *)
  Test_helpers.check_declarations ~minify:false "via-none"
    [ "--tw-gradient-via-stops: initial" ]

(* Bare bg-radial / bg-conic (and bg-conic-{angle}) set --tw-gradient-position
   to the default oklab interpolation and the matching gradient image; they used
   to be unknown classes (only the /interp and bracket forms were handled). *)
let test_radial_conic () =
  (* Each writes the position channel and the image, in that order, and nothing
     else. The affixes these replace checked one declaration each, so bg-radial
     was never held to emitting only a radial gradient. *)
  Test_helpers.check_declarations ~minify:false "bg-radial"
    [
      "--tw-gradient-position: in oklab";
      "background-image: radial-gradient(var(--tw-gradient-stops))";
    ];
  Test_helpers.check_declarations ~minify:false "bg-conic"
    [
      "--tw-gradient-position: in oklab";
      "background-image: conic-gradient(var(--tw-gradient-stops))";
    ];
  Test_helpers.check_declarations ~minify:false "bg-conic-180"
    [
      "--tw-gradient-position: from 180deg in oklab";
      "background-image: conic-gradient(var(--tw-gradient-stops))";
    ]

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

  (* Gradient positions, gradient angles and the opacity modifier are all plain
     decimal: read as OCaml literals, [from-0x50%] named itself [.from-80%] and
     [bg-red-500/0x50] mixed at 80%. *)
  let invalid =
    Test_helpers.check_invalid_input (module Tw.Backgrounds.Handler)
  in
  invalid "from-0x50%";
  invalid "from-050%";
  invalid "via-1_0%";
  invalid "bg-linear-0x45";
  invalid "bg-linear-045";
  invalid "bg-conic-0x45";
  invalid "bg-red-500/0x50";
  invalid "bg-red-500/1_0";
  invalid "bg-red-500/04";
  invalid "bg-red-500/1.50";
  invalid "bg-red-0500";

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
  Test_helpers.check_declarations ~minify:false "bg-[length:cover]"
    [ "background-size: cover" ];
  Test_helpers.check_declarations ~minify:false "bg-[length:contain]"
    [ "background-size: contain" ]

(* A data-type hint chooses which longhand a bracket lands in and says nothing
   about the value. [bg-position-] and [bg-size-] each write one longhand, so
   every hint lands there and the reader is handed what follows it; the hint
   stays in the class name, which is what the markup carries. Both readers were
   given the hint as well, read nothing, and the classes were refused where
   Tailwind writes the value through. *)
let test_bg_position_and_size_peel_a_hint () =
  List.iter
    (fun (cls, decl) ->
      match Tw.of_string cls with
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
      | Ok u ->
          Alcotest.(check string) "class round-trips" cls (Tw.pp u);
          (* The whole list, which is where "lands in one longhand" is actually
             said: an affix passes just as well beside a second declaration the
             hint was not supposed to produce. *)
          Test_helpers.check_declarations ~minify:false cls [ decl ])
    [
      ("bg-position-[position:50%]", "background-position: 50%");
      ("bg-position-[foo:50%]", "background-position: 50%");
      ("bg-size-[length:10px_20px]", "background-size: 10px 20px");
      ("bg-size-[foo:cover]", "background-size: cover");
    ]

(* A two-axis bg-position bracket mixes a keyword edge with a length, e.g.
   bg-position-[center_-100px] -> background-position: 50% -100px. *)
let test_bg_position_bracket_keyword_length () =
  (* [center] resolves to its percentage, where the CLI writes the keyword; the
     two are one position and the canonical differ folds them. *)
  Test_helpers.check_declarations ~minify:false "bg-position-[center_-100px]"
    [ "background-position: 50% -100px" ];
  Test_helpers.check_declarations ~minify:false "bg-position-[left_top]"
    [ "background-position: left top" ]

(* A background-position bracket takes the whole CSS grammar: a single edge
   keyword, and the four-value edge/offset form. Both used to fall through the
   hand-rolled parser to a silent [center]. *)
let test_bracket_position_grammar () =
  let writes cls value =
    Test_helpers.check_declarations ~minify:false cls
      [ "background-position: " ^ value ]
  in
  writes "bg-[position:top]" "top";
  writes "bg-[position:left_10px_top_20px]" "left 10px top 20px";
  writes "bg-position-[top]" "top";
  writes "bg-[top]" "top";
  (* the lengths form is unchanged *)
  writes "bg-[position:120px_120px]" "120px 120px";
  writes "bg-position-[center_-100px]" "50% -100px"

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
  (* The whole declaration, so "no bogus var() wrapping" is said by the value
     rather than by a second search for a string that must not appear. The named
     colours come out as hex: cascade prints a colour canonically, and the CLI's
     keyword is the same colour. *)
  Test_helpers.check_declarations ~minify:false
    "bg-[image:radial-gradient(white,black)]"
    [ "background-image: radial-gradient(#ffffff, #000000)" ];
  Test_helpers.check_declarations ~minify:false "bg-[image:var(--x)]"
    [ "background-image: var(--x)" ]

(* An arbitrary gradient angle in radians is converted to degrees. A negative
   angle used to come out as its floor plus a positive fraction, so
   bg-linear-[-0.5rad] rendered -29.3521deg instead of -28.6479deg. *)
let test_bracket_gradient_radians () =
  let writes cls angle =
    Test_helpers.check_declarations ~minify:false cls
      [
        "--tw-gradient-position: " ^ angle;
        "background-image: linear-gradient(var(--tw-gradient-stops, " ^ angle
        ^ "))";
      ]
  in
  writes "bg-linear-[-0.5rad]" "-28.6479deg";
  writes "bg-linear-[1.3rad]" "74.4845deg"

(* [-bg-linear-[value]] accepts only angles, gated by a check that used to spell
   "is this an angle" as [String.ends_with ~suffix:"rad" value]. That suffix
   also matches "grad" (gradians end in "rad" too), so it stripped "100grad"
   down to "100g", failed to read it as a number, and rejected the class
   outright where Tailwind accepts it and negates the value as calc(100grad *
   -1). Reading the bracket as a real CSS angle tells grad and rad apart; a
   non-angle bracket like [to_bottom] still has to be rejected. *)
let test_bracket_gradient_negated_angle_units () =
  Test_helpers.check_declarations ~minify:false "-bg-linear-[100grad]"
    [
      "--tw-gradient-position: calc(100grad * -1)";
      "background-image: linear-gradient(var(--tw-gradient-stops, calc(100grad \
       * -1)))";
    ];
  match Tw.of_string "-bg-linear-[to_bottom]" with
  | Ok _ -> Alcotest.fail "expected -bg-linear-[to_bottom] to be rejected"
  | Error _ -> ()

(* The positive (non-negated) [bg-linear-[<value>]] bracket shares the same "is
   this a rad value" question as the negated form: a [String.ends_with
   ~suffix:"rad"] test also matches "grad", since gradians end in "rad" too.
   bg-linear-[100grad] must keep its gradian unit rather than being read as a
   radian value with a stray "g" prefix. *)
let test_bracket_gradient_grad_unit () =
  Test_helpers.check_declarations ~minify:false "bg-linear-[100grad]"
    [
      "--tw-gradient-position: 100grad";
      "background-image: linear-gradient(var(--tw-gradient-stops, 100grad))";
    ]

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

(* The v3 bg-gradient-to-* spellings are compatibility aliases in the modern
   linear-gradient band, after the native direction candidates. *)
let legacy_gradient_alias_order_matches_tailwind () =
  Test_helpers.check_class_order ~test_name:"legacy gradient alias order"
    [
      "bg-gradient-to-r";
      "bg-linear-to-b";
      "bg-linear-45";
      "bg-gradient-to-t";
      "bg-linear-to-r";
      "bg-linear-to-t";
      "bg-conic";
      "bg-radial";
      "bg-linear-[45deg]";
    ]

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
  List.iter
    (fun cls ->
      (* The whole declaration, so the double-quoting this test was written for
         is ruled out by the value rather than by a second search for a string
         that must not appear. *)
      Test_helpers.check_declarations ~minify:false cls
        [ "background-image: url(/img/x.png)" ])
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
  (* The whole list. The affixes these replace were prefixes of the [-position]
     channels, so either would have passed on a utility that set only a position
     - the very thing the test was written to rule out. *)
  Test_helpers.check_declarations ~minify:false "from-[rgba(5,74,218,0.60)]"
    [
      "--tw-gradient-from: #054ada99";
      "--tw-gradient-stops: var(--tw-gradient-via-stops, \
       var(--tw-gradient-position), var(--tw-gradient-from) \
       var(--tw-gradient-from-position), var(--tw-gradient-to) \
       var(--tw-gradient-to-position))";
    ];
  Test_helpers.check_declarations ~minify:false "to-[rgb(16,26,50,0.60)]"
    [
      "--tw-gradient-to: #101a3299";
      "--tw-gradient-stops: var(--tw-gradient-via-stops, \
       var(--tw-gradient-position), var(--tw-gradient-from) \
       var(--tw-gradient-from-position), var(--tw-gradient-to) \
       var(--tw-gradient-to-position))";
    ]

(* A gradient stop-position utility (from-10%) registers the whole
   --tw-gradient-* @property family, like the colour utilities, matching the
   CLI. It used to register only the three *-position properties. *)
let test_gradient_stop_position_properties () =
  let css =
    match Tw.of_string "from-10%" with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error _ -> Alcotest.fail "could not parse from-10%"
  in
  (* A stop position writes only its own channel; the rest of the family is
     registered rather than set, which is what the @property checks below read.
     An @property rule is not a declaration, so those two stay on a
     substring. *)
  Test_helpers.check_declarations ~minify:false "from-10%"
    [ "--tw-gradient-from-position: 10%" ];
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
  (* Both arms: an unguarded fallback leaving the var() bare, then the mix under
     @supports. The affix this replaces named only the second. *)
  Test_helpers.check_declarations ~minify:false "bg-[var(--x)]/50"
    [
      "background-color: var(--x)";
      "background-color: color-mix(in oklab, var(--x) 50%, transparent)";
    ]

(* Tailwind forwards a declaration-safe arbitrary stop even when it is neither a
   valid colour nor a valid stop position. *)
let test_gradient_stop_token_stream () =
  let accepted cls =
    match Tw.of_string cls with
    | Ok _ -> ()
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  accepted "from-[<value>]";
  accepted "via-[<value>]";
  accepted "to-[<value>]";
  accepted "from-[25%]";
  accepted "from-[var(--x)]";
  accepted "from-[#0088cc]"

(* A malformed colour can still be one safe declaration value, so Tailwind
   forwards it and leaves rejection to the browser. *)
let test_arbitrary_bracket_color_token_stream () =
  let accepted cls =
    match Tw.of_string cls with
    | Ok u -> ignore (Tw.to_css ~base:false [ u ])
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  List.iter
    (fun prefix ->
      accepted (prefix ^ "-[#zz]");
      accepted (prefix ^ "-[#]");
      accepted (prefix ^ "-[#12345]");
      accepted (prefix ^ "-[#zz]/50"))
    [ "from"; "via"; "to" ];
  (* The malformed value reaches the sheet verbatim, and the stop list it is
     spliced into comes with it. *)
  Test_helpers.check_declarations ~minify:false "from-[#fff]"
    [ "--tw-gradient-from: #fff"; stops ];
  Test_helpers.check_declarations ~minify:false "via-[#abc]"
    [
      "--tw-gradient-via: #abc";
      via_stops;
      "--tw-gradient-stops: var(--tw-gradient-via-stops)";
    ];
  Test_helpers.check_declarations ~minify:false "to-[#123456]"
    [ "--tw-gradient-to: #123456"; stops ]

(* A bracket stop position is read with the CSS length-percentage grammar, so a
   unit the reader does not name is not rendered as a zero position. *)
let test_gradient_stop_position_units () =
  let emits cls decl =
    Test_helpers.check_declarations ~minify:false cls [ decl ]
  in
  (* One declaration each: a stop position leaves the colour channels alone. *)
  emits "from-[1rem]" "--tw-gradient-from-position: 1rem";
  emits "via-[10vw]" "--tw-gradient-via-position: 10vw";
  emits "to-[2em]" "--tw-gradient-to-position: 2em";
  emits "from-[calc(10%_+_2px)]" "--tw-gradient-from-position: calc(10% + 2px)";
  (* the spellings the reader already named keep their value *)
  emits "from-[50%]" "--tw-gradient-from-position: 50%";
  emits "from-[50px]" "--tw-gradient-from-position: 50px";
  emits "from-[length:var(--my-position)]"
    "--tw-gradient-from-position: var(--my-position)";
  (* the class name is spelled as it was written *)
  Alcotest.(check string)
    "from-[1rem] round-trips" "from-[1rem]"
    (Tw.pp (Result.get_ok (Tw.of_string "from-[1rem]")))

(* A bracket that is not a length-percentage is forwarded through the colour
   channel, matching Tailwind's arbitrary token-stream behavior. *)
let test_gradient_stop_position_token_stream () =
  let accepted cls =
    match Tw.of_string cls with
    | Ok u -> ignore (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  accepted "from-[fit-content]";
  accepted "from-[none]";
  accepted "to-[max-content]";
  accepted "from-[0]";
  accepted "from-[1zz]";
  accepted "via-[12px3]"

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
  let has cls shape position =
    Test_helpers.check_declarations ~minify:false cls
      [
        "--tw-gradient-position: " ^ position;
        "background-image: " ^ shape ^ "-gradient(var(--tw-gradient-stops))";
      ]
  in
  (* A second modifier, a function, a leading dot or sign, and the empty
     modifier: Tailwind emits nothing for any of them. *)
  rejected "bg-conic-45/oklab/foo";
  rejected "bg-conic/foo(1)";
  rejected "bg-conic/.5";
  rejected "bg-conic/[]";
  rejected "bg-radial/foo(1)";
  (* An unknown colour space still names one. *)
  has "bg-conic/foo" "conic" "in foo";
  has "bg-conic-45/999" "conic" "from 45deg in 999";
  has "bg-radial/foo" "radial" "in foo";
  has "bg-conic/oklab" "conic" "in oklab";
  has "bg-conic/shorter" "conic" "in oklch shorter hue";
  has "bg-conic/[in_hsl_longer_hue]" "conic" "in hsl longer hue";
  (* The linear forms carry the modifier into the @supports rule rather than
     dropping it, so they write the channel twice: the angle alone for a browser
     without interpolation, then the interpolated form. The affixes these
     replace named only the second, and so never said what the fallback a
     browser actually reaches is. *)
  let has_linear cls angle =
    Test_helpers.check_declarations ~minify:false cls
      [
        "--tw-gradient-position: " ^ angle;
        "--tw-gradient-position: " ^ angle ^ " in foo";
        "background-image: linear-gradient(var(--tw-gradient-stops))";
      ]
  in
  has_linear "bg-linear-45/foo" "45deg";
  has_linear "bg-linear-to-r/foo" "to right"

(* An arbitrary value writes a space as [_] and a literal underscore as [\_], so
   a file name or a gradient position carrying an underscore is written with the
   escape rather than losing the character. *)
let test_arbitrary_underscore_escape () =
  Test_helpers.check_declarations ~minify:false {|bg-[url('a\_b.png')]|}
    [ "background-image: url(a_b.png)" ];
  (* The gradient carries the same value into its image, which is where an
     escape lost in one of the two readings would show. *)
  Test_helpers.check_declarations ~minify:false {|bg-linear-[to\_bottom]|}
    [
      "--tw-gradient-position: to_bottom";
      "background-image: linear-gradient(var(--tw-gradient-stops, to_bottom))";
    ]

(* A [url()] is CSS source, so a [\]] in it is one character of the URL, not a
   backslash of its own: [url(a\]b)] and [url(a]b)] name the same file, and
   Tailwind emits the first verbatim. tw used to hand cascade the backslash as
   well and emit the double-escaped url("a\\]b"). *)
let test_bracket_url_escape () =
  let has cls decl =
    Test_helpers.check_declarations ~minify:false cls [ decl ]
  in
  has {|bg-[url(a\]b)]|} "background-image: url(a]b)";
  has {|bg-[image:url(a\]b)]|} "background-image: url(a]b)";
  (* A quoted []] needs no escape either. Tailwind keeps the quotes the class
     wrote; dropping them is cascade's canonical spelling of the same URL. *)
  has "bg-[url('a]b')]" "background-image: url(a]b)";
  (* A url argument keeps a bare [_], which is part of the file name. *)
  has "bg-[url('a_b.png')]" "background-image: url(a_b.png)"

(* A data-type hint says how to read the value written after it; it does not
   make that value the name of a custom property. [bg-[color:red]] wrote
   [background-color: var(--red)] where Tailwind writes [background-color:
   red]. *)
let test_bracket_data_type_hint_reads_the_value () =
  check_declarations "bg-[color:red]" [ "background-color:red" ];
  check_declarations "bg-[url:url(a.png)]" [ "background-image:url(a.png)" ];
  check_declarations "from-[color:red]"
    [
      "--tw-gradient-from:red";
      "--tw-gradient-stops:var(--tw-gradient-via-stops,var(--tw-gradient-position),var(--tw-gradient-from) \
       var(--tw-gradient-from-position),var(--tw-gradient-to) \
       var(--tw-gradient-to-position))";
    ];
  check_declarations "from-[percentage:40%]"
    [ "--tw-gradient-from-position:40%" ];
  (* a var() reference after the hint still names a custom property *)
  check_declarations "bg-[color:var(--my-color)]"
    [ "background-color:var(--my-color)" ];
  (* the class prints back with the hint the author wrote *)
  Alcotest.(check string)
    "bg-[color:red] round-trips" "bg-[color:red]"
    (Tw.pp (Result.get_ok (Tw.of_string "bg-[color:red]")));
  (* A value no colour reader takes is held open, not settled: Tailwind writes
     the bracket out whatever it says, so refusing is an intermediate. *)
  check_invalid_input
    ~why:(Diverges "emitted verbatim; tw needs an opaque declaration to match")
    (module Tw.Backgrounds.Handler)
    "bg-[color:notacolour]"

(* Tailwind knows two spellings for the hint that names a background-position,
   and [bg-[…]] routes on the one the author wrote while the class name keeps
   it. *)
let test_percentage_hint_names_a_position () =
  check_declarations "bg-[percentage:50%]" [ "background-position:50%" ];
  check_declarations "bg-[position:50%]" [ "background-position:50%" ];
  List.iter check [ "bg-[percentage:50%]"; "bg-[position:50%]" ];
  check_invalid_input
    ~why:(Diverges "emitted verbatim; tw needs an opaque declaration to match")
    (module Tw.Backgrounds.Handler)
    "bg-[percentage:notaposition]"

let tests =
  [
    test_case "percentage hint names a position" `Quick
      test_percentage_hint_names_a_position;
    test_case "bracket data-type hint reads the value" `Quick
      test_bracket_data_type_hint_reads_the_value;
    test_case "arbitrary underscore escape" `Quick
      test_arbitrary_underscore_escape;
    test_case "bracket url escape" `Quick test_bracket_url_escape;
    test_case "gradient interpolation" `Quick test_gradient_interpolation;
    test_case "gradient stop position units" `Quick
      test_gradient_stop_position_units;
    test_case "gradient stop position token stream" `Quick
      test_gradient_stop_position_token_stream;
    test_case "arbitrary bracket color token stream" `Quick
      test_arbitrary_bracket_color_token_stream;
    test_case "bg colors" `Quick test_bg_colors;
    test_case "gradient stop token stream" `Quick
      test_gradient_stop_token_stream;
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
    test_case "bracket gradient grad unit" `Quick
      test_bracket_gradient_grad_unit;
    test_case "bracket image literal" `Quick test_bracket_image_literal;
    test_case "bracket length keywords" `Quick test_bracket_length_keywords;
    test_case "bg-position bracket keyword+length" `Quick
      test_bg_position_bracket_keyword_length;
    test_case "bg-position and bg-size peel a data-type hint" `Quick
      test_bg_position_and_size_peel_a_hint;
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
    test_case "legacy gradient alias order matches Tailwind" `Quick
      legacy_gradient_alias_order_matches_tailwind;
    test_case "backgrounds render like Tailwind" `Slow
      rendering_matches_tailwind;
  ]

let suite = ("backgrounds", tests)
