open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Backgrounds.Handler)

let test_bg_colors () =
  check "bg-red-500";
  check "bg-blue-600";
  check "bg-green-700"

let test_of_string_invalid () =
  (* Invalid background utilities *)
  let test_invalid =
    Test_helpers.check_invalid_parts (module Tw.Backgrounds.Handler)
  in
  let invalid =
    Test_helpers.check_invalid_input (module Tw.Backgrounds.Handler)
  in
  (* The opacity modifier is plain decimal: read as an OCaml literal,
     [bg-red-500/0x50] mixed at 80%. *)
  invalid "bg-red-500/0x50";
  invalid "bg-red-500/1_0";
  invalid "bg-red-500/04";
  invalid "bg-red-500/1.50";
  invalid "bg-red-0500";
  (* Incomplete *)
  test_invalid [ "bg" ];
  test_invalid [ "bg"; "gradient" ];
  (* Unknown prefix *)
  test_invalid [ "unknown"; "red" ];
  (* A gradient shape is the gradients' to read, whether its value reads or not,
     and a gradient stop is theirs too. *)
  let gradient =
    Test_helpers.check_invalid_input ~why:Another_handler
      (module Tw.Backgrounds.Handler)
  in
  gradient "bg-linear-to-r";
  gradient "bg-conic";
  gradient "from-red-500";
  invalid "bg-linear-0x45"

(* A keyword background-size value under a [length:...] hint (bg-[length:cover])
   is the keyword. *)
let test_bracket_length_keywords () =
  Test_helpers.check_declarations ~minify:false "bg-[length:cover]"
    [ "background-size: cover" ];
  Test_helpers.check_declarations ~minify:false "bg-[length:contain]"
    [ "background-size: contain" ]

(* Tailwind substitutes [--alpha()] and [--spacing()] wherever they stand in an
   arbitrary value, so a gradient spelling one is the image with the
   [color-mix()] in its place, written to [background-image]. *)
let test_bracket_gradient_alpha_fn () =
  Test_helpers.check_declarations ~minify:false
    "bg-[linear-gradient(--alpha(red/50%),blue)]"
    [
      "background-image: linear-gradient(color-mix(in oklab, red 50%, \
       transparent), #0000ff)";
    ];
  Test_helpers.check_declarations ~minify:false
    "bg-[linear-gradient(to_right,--spacing(4),blue)]"
    [
      "background-image: linear-gradient(to right, calc(var(--spacing) * 4), \
       #0000ff)";
    ]

(* Tailwind's colour-mix polyfill applies to a bracket image the way it applies
   to any declaration: a gradient whose stop is a [color-mix()] reading a custom
   property or [currentcolor] is written with the mix's first colour in the open
   and as written behind the colour-mix guard, hinted or not. *)
let test_bracket_image_mix_polyfill () =
  let pair cls ~open_ ~guarded =
    Test_helpers.check_declarations ~minify:false cls
      [ "background-image: " ^ open_; "background-image: " ^ guarded ]
  in
  let mix colour alpha =
    "color-mix(in oklab, " ^ colour ^ " " ^ alpha ^ ", transparent)"
  in
  pair "bg-[linear-gradient(--alpha(red/var(--o)),blue)]"
    ~open_:"linear-gradient(red, #0000ff)"
    ~guarded:("linear-gradient(" ^ mix "red" "var(--o)" ^ ", #0000ff)");
  (* [currentColor] is cascade's spelling of the keyword outside a mix *)
  pair "bg-[image:linear-gradient(--alpha(currentcolor/50%),blue)]"
    ~open_:"linear-gradient(currentColor, #0000ff)"
    ~guarded:("linear-gradient(" ^ mix "currentcolor" "50%" ^ ", #0000ff)");
  pair "bg-[linear-gradient(color-mix(in_oklab,red_var(--o),transparent),blue)]"
    ~open_:"linear-gradient(red, #0000ff)"
    ~guarded:("linear-gradient(" ^ mix "red" "var(--o)" ^ ", #0000ff)")

(* A data-type hint chooses which longhand a bracket lands in and says nothing
   about the value. [bg-position-] and [bg-size-] each write one longhand, so
   every hint lands there and the reader is handed what follows it; the hint
   stays in the class name, which is what the markup carries. Tailwind writes
   the value through. *)
let test_bg_position_and_size_peel_a_hint () =
  List.iter
    (fun (cls, decl) ->
      match Tw.of_string cls with
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
      | Ok u ->
          Alcotest.(check string) "class round-trips" cls (Tw.pp u);
          (* The whole list, which is where "lands in one longhand" is said: an
             affix also passes beside a second declaration. *)
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
   keyword, and the four-value edge/offset form. *)
let test_bracket_position_grammar () =
  let writes cls value =
    Test_helpers.check_declarations ~minify:false cls
      [ "background-position: " ^ value ]
  in
  writes "bg-[position:top]" "top";
  writes "bg-[position:left_10px_top_20px]" "left 10px top 20px";
  writes "bg-position-[top]" "top";
  writes "bg-[top]" "top";
  (* the lengths form *)
  writes "bg-[position:120px_120px]" "120px 120px";
  writes "bg-position-[center_-100px]" "50% -100px"

(* A bracket value the property cannot take goes to that property anyway: the
   hint chose the longhand, and the browser discards what it cannot read. What
   is ruled out is an answer that is neither: an empty rule, or a substituted
   value such as [center]. *)
let test_invalid_bracket_value () =
  let accepted cls =
    match Tw.of_string cls with
    | Ok _ -> ()
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  check_declarations "bg-[image:nope]" [ "background-image:nope" ];
  check_declarations "bg-[position:nope]" [ "background-position:nope" ];
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

(* An arbitrary url() with its own quotes is not double-wrapped: it
   canonicalises to a valid url(). *)
let test_bg_arbitrary_url () =
  List.iter
    (fun cls ->
      (* The whole declaration, so the value itself rules out double quoting. *)
      Test_helpers.check_declarations ~minify:false cls
        [ "background-image: url(/img/x.png)" ])
    [
      "bg-[url('/img/x.png')]";
      "bg-[url(\"/img/x.png\")]";
      "bg-[url(/img/x.png)]";
      (* The image: data-type hint forces background-image; the url() is not
         read as a var. *)
      "bg-[image:url(/img/x.png)]";
    ]

(* A var() background colour with an alpha modifier defers the alpha to
   color-mix: the variable's value is unknown at build time, so it cannot be
   folded into a literal colour. *)
let test_bg_var_opacity () =
  (* Both arms: an unguarded fallback leaving the var() bare, then the mix under
     @supports. *)
  Test_helpers.check_declarations ~minify:false "bg-[var(--x)]/50"
    [
      "background-color: var(--x)";
      "background-color: color-mix(in oklab, var(--x) 50%, transparent)";
    ]

(* An arbitrary value writes a space as [_] and a literal underscore as [\_], so
   a file name carrying an underscore is written with the escape rather than
   losing the character. *)
let test_arbitrary_underscore_escape () =
  Test_helpers.check_declarations ~minify:false {|bg-[url('a\_b.png')]|}
    [ "background-image: url(a_b.png)" ]

(* A [url()] is CSS source, so a [\]] in it is one character of the URL, not a
   backslash of its own: [url(a\]b)] and [url(a]b)] name the same file, and
   Tailwind emits the first verbatim, with no second escape. *)
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
   make that value the name of a custom property: Tailwind writes
   [bg-[color:red]] as [background-color: red]. *)
let test_bracket_data_type_hint_reads_the_value () =
  check_declarations "bg-[color:red]" [ "background-color:red" ];
  check_declarations "bg-[url:url(a.png)]" [ "background-image:url(a.png)" ];
  (* a var() reference after the hint still names a custom property *)
  check_declarations "bg-[color:var(--my-color)]"
    [ "background-color:var(--my-color)" ];
  (* the class prints back with the hint the author wrote *)
  Alcotest.(check string)
    "bg-[color:red] round-trips" "bg-[color:red]"
    (Tw.pp (Result.get_ok (Tw.of_string "bg-[color:red]")));
  (* The colour is this family's last resort, so a value no colour reader takes
     still reaches [background-color], forwarded verbatim. *)
  check_declarations "bg-[color:notacolour]" [ "background-color:notacolour" ];
  check_declarations "bg-[notacolour]" [ "background-color:notacolour" ];
  (* An unknown hint lands there too, and so does a bracket that opens with
     [url(] but holds no url. *)
  check_declarations "bg-[foo:50%]" [ "background-color:50%" ];
  check_declarations "bg-[url(x.png)_center]"
    [ "background-color:url(x.png) center" ]

(* Tailwind knows two spellings for the hint that names a background-position,
   and [bg-[…]] routes on the one the author wrote while the class name keeps
   it. *)
let test_percentage_hint_names_a_position () =
  check_declarations "bg-[percentage:50%]" [ "background-position:50%" ];
  check_declarations "bg-[position:50%]" [ "background-position:50%" ];
  List.iter check [ "bg-[percentage:50%]"; "bg-[position:50%]" ];
  (* Either spelling of the hint names the longhand, so a value the position
     grammar declines still lands on [background-position]. *)
  check_declarations "bg-[percentage:notaposition]"
    [ "background-position:notaposition" ];
  check_declarations "bg-[position:notaposition]"
    [ "background-position:notaposition" ]

(* A bracket that opens with a var() and goes on past it is no custom property
   reference: Tailwind writes it as it stands. *)
let test_var_prefixed_bracket () =
  List.iter Test_helpers.check_class_matches_tailwind
    [
      "bg-[var(--a)_1px]";
      "bg-[var(--a),foo]";
      "bg-position-[var(--a)_1px]";
      "bg-position-[var(--a),foo]";
      "bg-size-[var(--a)_1px]";
      "bg-size-[var(--a),foo]";
    ]

let tests =
  [
    Alcotest.test_case "var-prefixed bracket" `Quick test_var_prefixed_bracket;
    test_case "percentage hint names a position" `Quick
      test_percentage_hint_names_a_position;
    test_case "bracket data-type hint reads the value" `Quick
      test_bracket_data_type_hint_reads_the_value;
    test_case "arbitrary underscore escape" `Quick
      test_arbitrary_underscore_escape;
    test_case "bracket url escape" `Quick test_bracket_url_escape;
    test_case "bg colors" `Quick test_bg_colors;
    test_case "bg var color with opacity" `Quick test_bg_var_opacity;
    test_case "bg arbitrary url quoting" `Quick test_bg_arbitrary_url;
    test_case "bracket image literal" `Quick test_bracket_image_literal;
    test_case "bracket length keywords" `Quick test_bracket_length_keywords;
    test_case "bg-position bracket keyword+length" `Quick
      test_bg_position_bracket_keyword_length;
    test_case "bracket gradient --alpha()" `Quick test_bracket_gradient_alpha_fn;
    test_case "bracket image colour-mix polyfill" `Quick
      test_bracket_image_mix_polyfill;
    test_case "bg-position and bg-size peel a data-type hint" `Quick
      test_bg_position_and_size_peel_a_hint;
    test_case "bracket position grammar" `Quick test_bracket_position_grammar;
    test_case "invalid bracket value" `Quick test_invalid_bracket_value;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
    test_case "backgrounds suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "background and mask order matches Tailwind" `Slow
      order_matches_tailwind;
    test_case "backgrounds render like Tailwind" `Slow
      rendering_matches_tailwind;
  ]

let suite = ("backgrounds", tests)
