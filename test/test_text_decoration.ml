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
  let rejected cls =
    match Tw.of_string cls with
    | Ok u ->
        Alcotest.failf "expected %s to be rejected, got %s" cls
          (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true)
    | Error _ -> ()
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
   conversion. A bracket that is not a length is rejected by the parser rather
   than raising once the sheet is rendered. *)
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
  thickness "decoration-[--spacing(2)]" "calc(var(--spacing)*2)";
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "decoration-[.]";
  rejected "decoration-[1e]"

(* Tailwind treats a unitless arbitrary decoration value as a colour candidate,
   whether it is written as an integer or with a fractional part. The resulting
   colour declaration is invalid CSS and has no browser effect; TW must keep the
   candidate accepted without turning it into a visible pixel thickness. *)
let test_decoration_bracket_unitless_is_color () =
  (* The whole list, which is empty: the class is accepted and writes nothing.
     That is a divergence from the CLI, which writes the invalid
     [text-decoration-color: 1.5] under the token-stream contract; see
     docs/token-stream-contract.md. Pinning the empty list rather than searching
     for a thickness that must not appear is what makes the divergence visible,
     and what will fail when the contract is honoured here. *)
  List.iter
    (fun cls -> Test_helpers.check_declarations cls [])
    [
      "decoration-[0]"; "decoration-[2]"; "decoration-[1.5]"; "decoration-[.5]";
    ]

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
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
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
    ];
  (* a bracket naming no colour and no thickness is still not a class *)
  Alcotest.(check bool)
    "decoration-[notacolour] is not a class" true
    (Result.is_error (Tw.of_string "decoration-[notacolour]"))

(* A [#] bracket is only a decoration colour when what follows is a hex
   spelling. The decoration reader handed everything after the [#] to the
   raising constructor from inside [of_class], so a malformed hex escaped the
   parser as an exception instead of failing the match. *)
let test_invalid_decoration_bracket_hex () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "decoration-[#zz]";
  rejected "decoration-[#]";
  rejected "decoration-[#12345]";
  rejected "decoration-[#zz]/50";
  check_declarations "decoration-[#ff0000]" [ "text-decoration-color:#f00" ]

let tests =
  [
    test_case "text decoration" `Quick test_text_decoration;
    test_case "decoration undefined colour shade" `Quick
      test_decoration_undefined_shade;
    test_case "underline-offset-[...] keeps its bracket" `Quick
      test_underline_offset_bracket_keeps_its_class;
    test_case "decoration bracket thickness" `Quick
      test_decoration_bracket_thickness;
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
