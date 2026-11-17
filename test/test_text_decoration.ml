open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Text_decoration.Handler)

let test_text_decoration () =
  check "underline";
  check "overline";
  check "line-through";
  check "no-underline";
  (* decoration color/thickness *)
  check "decoration-from-font";
  check "decoration-2";
  check "decoration-blue-500"

(* A shade the palette does not define is not a colour. [decoration-*] read the
   shade without checking it, so the class was accepted and then referenced a
   variable no theme declares. *)
let test_decoration_undefined_shade () =
  let rejected =
    Test_helpers.check_invalid_class ~why:Test_helpers.Not_a_utility
  in
  let accepted cls =
    match Tw.of_string cls with
    | Ok _ -> ()
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  rejected "decoration-red-999";
  rejected "decoration-red-0";
  rejected "decoration-red-42";
  rejected "decoration-red-999/50";
  accepted "decoration-red-500";
  accepted "decoration-red-950";
  accepted "decoration-red-500/50"

(* [underline-offset-[N]] is a bracket value, not a spelling of the bare
   [underline-offset-N] step: it names its own rule and its value carries the
   unit the author wrote rather than the [px] the scale supplies. Folding the
   two together emitted [.underline-offset-0] for [underline-offset-[0.0]], a
   rule nothing selects. *)
let test_underline_offset_bracket_keeps_its_class () =
  List.iter
    (fun (cls, escaped, value) ->
      match Tw.of_string cls with
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
      | Ok u ->
          Alcotest.(check string) "class round-trips" cls (Tw.pp u);
          (* The whole selector, not an affix of it: the rule this test is
             about, [.underline-offset-0] for [underline-offset-[0.0]], is a
             selector the affix for the bracket form would simply miss rather
             than contradict. And the value, which says the unit the author
             wrote survived. *)
          Alcotest.(check (list string))
            (cls ^ " selects itself") [ escaped ]
            (Test_helpers.selectors_of_utility u);
          Test_helpers.check_declarations ~minify:false cls
            [ "text-underline-offset: " ^ value ])
    [
      ("underline-offset-[0.0]", {|.underline-offset-\[0\.0\]|}, "0");
      ("underline-offset-[3px]", {|.underline-offset-\[3px\]|}, "3px");
      ("underline-offset-[1.50px]", {|.underline-offset-\[1\.50px\]|}, "1.50px");
      ( "underline-offset-[calc(1px+2px)]",
        {|.underline-offset-\[calc\(1px\+2px\)\]|},
        "calc(1px + 2px)" );
      ( "-underline-offset-[3px]",
        {|.-underline-offset-\[3px\]|},
        "calc(3px * -1)" );
    ];
  (* the bare step keeps its own class and the [px] the scale gives it *)
  check_declarations "underline-offset-3" [ "text-underline-offset:3px" ]

(* An arbitrary decoration thickness takes any CSS length unit, not just the px
   the hand-rolled suffix parser knew; the percentage form keeps its em
   conversion. *)
let test_decoration_bracket_thickness () =
  let thickness cls value =
    check_declarations cls [ "text-decoration-thickness:" ^ value ]
  in
  thickness "decoration-[3rem]" "3rem";
  thickness "decoration-[2px]" "2px";
  thickness "decoration-[2ch]" "2ch";
  thickness "decoration-[50%]" ".5em";
  (* The bracket goes through the arbitrary-value decoder, so an escaped space
     and a [--spacing()] call both read as lengths. *)
  thickness "decoration-[calc(1rem_+_2px)]" "calc(1rem + 2px)";
  thickness "decoration-[--spacing(2)]" "calc(var(--spacing)*2)"

(* Tailwind treats a unitless arbitrary decoration value as a colour candidate,
   whether it is written as an integer or with a fractional part, and writes it
   into text-decoration-color for the browser to drop rather than painting a
   pixel thickness. *)
let test_decoration_bracket_unitless_is_color () =
  List.iter
    (fun v ->
      Test_helpers.check_declarations ~minify:false
        ("decoration-[" ^ v ^ "]")
        [ "text-decoration-color: " ^ v ])
    [ "0"; "2"; "1.5"; ".5" ]

(* A bracket Tailwind reads as neither a length nor a percentage is a colour,
   whatever it holds, and one holding math is a thickness. A value neither
   reader takes is written as the author spelled it, so the browser drops the
   declaration and keeps the selector. *)
let test_decoration_bracket_falls_through () =
  let colour cls value =
    Test_helpers.check_declarations ~minify:false cls
      [ "text-decoration-color: " ^ value ]
  in
  let thickness cls value =
    Test_helpers.check_declarations ~minify:false cls
      [ "text-decoration-thickness: " ^ value ]
  in
  colour "decoration-[foo]" "foo";
  colour "decoration-[--c]" "--c";
  colour "decoration-[auto]" "auto";
  colour "decoration-[notacolour]" "notacolour";
  colour "decoration-[url(x.png)_center]" "url(x.png) center";
  colour "decoration-[<value>]" "<value>";
  colour "decoration-[.]" ".";
  colour "decoration-[1e]" "1e";
  colour "decoration-[#zz]" "#zz";
  colour "decoration-[#12345]" "#12345";
  colour "decoration-[bogus:foo]" "foo";
  colour "decoration-[color:foo]" "foo";
  thickness "decoration-[calc(1+2)]" "calc(1 + 2)";
  thickness "decoration-[foo_calc(1+2)]" "foo calc(1 + 2)";
  thickness "decoration-[length:foo]" "foo";
  thickness "decoration-[percentage:foo]" "foo";
  Alcotest.(check string)
    "the class keeps its hint" "decoration-[bogus:foo]"
    (Tw.pp (Result.get_ok (Tw.of_string "decoration-[bogus:foo]")));
  Test_helpers.check_invalid_input
    (module Tw.Text_decoration.Handler)
    "decoration-[a;b]";
  (* Tailwind writes a thickness only without a modifier. *)
  Test_helpers.check_invalid_input
    (module Tw.Text_decoration.Handler)
    "decoration-[length:foo]/50";
  Test_helpers.check_invalid_input
    ~why:
      (Test_helpers.Diverges
         "the raw token is mixed verbatim; tw refuses a modifier on a bracket \
          no colour reader took, as the colour families do")
    (module Tw.Text_decoration.Handler)
    "decoration-[foo]/50"

(* A shadeless decoration colour takes an opacity modifier the same way every
   other colour family does, and keeps its shadeless class name. *)
let test_decoration_shadeless_opacity () =
  check "decoration-white/50";
  check "decoration-black/50";
  check "decoration-white/[0.5]";
  (* the folded sRGB fallback, then the [color-mix] enhancement under its
     [@supports] guard, aliased for WebKit *)
  let mixed cls hex token =
    check_declarations cls
      [
        "text-decoration-color:" ^ hex;
        "-webkit-text-decoration-color:color-mix(in oklab,var(" ^ token
        ^ ") 50%,transparent)";
        "text-decoration-color:color-mix(in oklab,var(" ^ token
        ^ ") 50%,transparent)";
      ]
  in
  mixed "decoration-white/50" "#ffffff80" "--color-white";
  mixed "decoration-black/50" "#00000080" "--color-black";
  let rejected =
    Test_helpers.check_invalid_class ~why:Test_helpers.Not_a_utility
  in
  rejected "decoration-2/50";
  rejected "decoration-nosuchcolor/50";
  rejected "decoration-white/"

(* A modifier reading a custom property mixes that property into the guarded
   value, on [currentcolor] and on a bracket [var()] as on a palette colour.
   Both arms folded the modifier to a percentage, which a var() has none of, so
   the mix said [100%] and the modifier was dropped. *)
let test_decoration_opacity_var () =
  let mixed cls fallback colour =
    check_declarations cls
      (fallback
      @ [
          "-webkit-text-decoration-color:color-mix(in oklab," ^ colour
          ^ " var(--o),transparent)";
          "text-decoration-color:color-mix(in oklab," ^ colour
          ^ " var(--o),transparent)";
        ])
  in
  mixed "decoration-current/(--o)"
    [ "text-decoration-color:currentColor" ]
    "currentcolor";
  mixed "decoration-current/[var(--o)]"
    [ "text-decoration-color:currentColor" ]
    "currentcolor";
  mixed "decoration-[var(--c)]/(--o)"
    [
      "-webkit-text-decoration-color:var(--c)"; "text-decoration-color:var(--c)";
    ]
    "var(--c)";
  mixed "decoration-[color:var(--c)]/(--o)"
    [
      "-webkit-text-decoration-color:var(--c)"; "text-decoration-color:var(--c)";
    ]
    "var(--c)"

(* The pre-color-mix fallback has to carry the modifier's alpha, or a browser
   without [color-mix()] paints the decoration fully opaque. A palette colour
   folds the alpha into a hex; a project token, whose value the theme supplies,
   takes the sRGB mix instead. *)
let test_decoration_opacity_fallback () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("color-brand", "oklch(55% 0.2 250)") ]
  in
  (* All three arms: the fallback, then the enhancement aliased for WebKit and
     unaliased. The affixes these replace named the fallback alone for the first
     three, so nothing held the enhancement to carrying the same alpha the
     fallback folded in - which is the whole subject of this test. *)
  let emits cls fallback enhanced =
    Test_helpers.check_declarations ~theme cls
      [
        "text-decoration-color:" ^ fallback;
        "-webkit-text-decoration-color:" ^ enhanced;
        "text-decoration-color:" ^ enhanced;
      ]
  in
  emits "decoration-red-500/50" "#fb2c3680"
    "color-mix(in oklab,var(--color-red-500) 50%,transparent)";
  emits "decoration-white/50" "#ffffff80"
    "color-mix(in oklab,var(--color-white) 50%,transparent)";
  emits "decoration-brand/50"
    "color-mix(in srgb,oklch(55%.2 250) 50%,transparent)"
    "color-mix(in oklab,var(--color-brand) 50%,transparent)";
  (* An alpha read from a var has no percentage to fold in, so the fallback is
     the colour itself and the enhanced value reads the var. *)
  emits "decoration-brand/(--a)" "oklch(55%.2 250)"
    "color-mix(in oklab,var(--color-brand) var(--a),transparent)"

(* A bracket colour CSS names without spelling it as a function - a named
   colour, a keyword - is a decoration colour too. The reader admitted only a
   [#] hex and a colour function, so [decoration-[rebeccapurple]] was an unknown
   class, with or without an opacity modifier. *)
let test_decoration_bracket_named_color () =
  check_declarations "decoration-[rebeccapurple]"
    [ "text-decoration-color:rebeccapurple" ];
  check_declarations "decoration-[currentColor]"
    [ "text-decoration-color:currentColor" ];
  (* the modifier mixes into the colour the bracket named, not into black *)
  Test_helpers.check_declarations ~minify:false "decoration-[rebeccapurple]/50"
    [
      "text-decoration-color: color-mix(in oklab, rebeccapurple 50%, \
       transparent)";
    ]

(* A [#] bracket is only a typed decoration colour when what follows is a hex
   spelling; any other is written as the author spelled it. The decoration
   reader handed everything after the [#] to the raising constructor from inside
   [of_class], so a malformed hex escaped the parser as an exception. *)
let test_invalid_decoration_bracket_hex () =
  check_declarations "decoration-[#ff0000]" [ "text-decoration-color:#f00" ];
  check_declarations ~minify:false "decoration-[#]"
    [ "text-decoration-color: #" ];
  Test_helpers.check_invalid_input
    ~why:
      (Test_helpers.Diverges
         "the raw token is mixed verbatim; tw refuses a modifier on a bracket \
          no colour reader took, as the colour families do")
    (module Tw.Text_decoration.Handler)
    "decoration-[#zz]/50"

(* A negated bracket the length reader cannot read is [calc(<value> * -1)], with
   the value after any hint, as Tailwind writes it. *)
let test_negated_token_stream () =
  List.iter Test_helpers.check_class_matches_tailwind
    [
      "-underline-offset-[foo]";
      "-underline-offset-[45deg]";
      "-underline-offset-[foo:45deg]";
      "-underline-offset-[url(x.png)_center]";
    ]

(* A bracket that opens with a var() and goes on past it is no custom property
   reference: Tailwind writes it as it stands. *)
let test_var_prefixed_bracket () =
  List.iter Test_helpers.check_class_matches_tailwind
    [
      "decoration-[var(--a)_1px]";
      "decoration-[var(--a),foo]";
      "underline-offset-[var(--a)_1px]";
      "underline-offset-[var(--a),foo]";
    ]

(* A hint the family does not know names no reading of its own, so Tailwind
   writes the value after it the way it writes the unhinted bracket. *)
let test_unknown_hint () =
  List.iter Test_helpers.check_class_matches_tailwind
    [ "decoration-[foo:red]"; "decoration-[foo:2px]" ]

let tests =
  [
    Alcotest.test_case "unknown hint" `Quick test_unknown_hint;
    Alcotest.test_case "var-prefixed bracket" `Quick test_var_prefixed_bracket;
    test_case "negated token stream" `Quick test_negated_token_stream;
    test_case "text decoration" `Quick test_text_decoration;
    test_case "decoration undefined colour shade" `Quick
      test_decoration_undefined_shade;
    test_case "underline-offset-[...] keeps its bracket" `Quick
      test_underline_offset_bracket_keeps_its_class;
    test_case "decoration bracket thickness" `Quick
      test_decoration_bracket_thickness;
    test_case "decoration bracket falls through" `Quick
      test_decoration_bracket_falls_through;
    test_case "unitless decoration bracket is a colour" `Quick
      test_decoration_bracket_unitless_is_color;
    test_case "decoration shadeless opacity" `Quick
      test_decoration_shadeless_opacity;
    test_case "decoration opacity from a var" `Quick test_decoration_opacity_var;
    test_case "decoration opacity fallback" `Quick
      test_decoration_opacity_fallback;
    test_case "decoration bracket named colour" `Quick
      test_decoration_bracket_named_color;
    test_case "invalid decoration bracket hex" `Quick
      test_invalid_decoration_bracket_hex;
  ]

let suite = ("text_decoration", tests)
