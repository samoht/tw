let check = Test_helpers.check_handler_roundtrip (module Tw.Text_shadow.Handler)

let test_roundtrip () =
  check "text-shadow-none";
  check "text-shadow-2xs";
  check "text-shadow-xs";
  check "text-shadow-sm";
  check "text-shadow-md";
  check "text-shadow-lg"

let test_invalid () =
  Test_helpers.check_invalid_input
    (module Tw.Text_shadow.Handler)
    "text-shadow-foo";
  (* Bare `text-shadow` is not a v4 utility (the CLI emits nothing); only the
     named scale `text-shadow-{2xs,xs,sm,md,lg}` is valid. *)
  Test_helpers.check_invalid_input (module Tw.Text_shadow.Handler) "text-shadow"

let parse s = Result.get_ok (Tw.of_string s)

(* The v4.3.1 default text-shadow scale: text-shadow-2xs uses alpha .15
   (#00000026), not the .1 (#0000001a) tw emitted before theme-threading. *)
let test_default_scale () =
  let css =
    Tw.to_css ~base:false [ parse "text-shadow-2xs" ]
    |> Tw.Css.to_string ~minify:true
  in
  Alcotest.(check bool)
    "text-shadow-2xs default is #00000026 (alpha .15)" true
    (Astring.String.is_infix ~affix:"#00000026" css)

(* A threaded @theme override for the text-shadow token flows through to the
   inlined value (here .1 = #0000001a), which is impossible without
   threading. *)
let test_theme_override () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("text-shadow-2xs", "0px 1px 0px rgb(0 0 0 / 0.1)") ]
  in
  let css =
    Tw.to_css ~theme ~base:false [ parse "text-shadow-2xs" ]
    |> Tw.Css.to_string ~minify:true
  in
  Alcotest.(check bool)
    "text-shadow-2xs @theme override flows to #0000001a" true
    (Astring.String.is_infix ~affix:"#0000001a" css)

(* Default palette colours stay in their authored OKLCH space, matching the
   fallback Tailwind emits before its guarded [color-mix()] declaration. *)
let test_palette_color_keeps_oklch () =
  let css =
    Tw.to_css ~base:false [ parse "text-shadow-sky-300" ]
    |> Tw.Css.to_string ~minify:true
  in
  Alcotest.(check bool)
    "text-shadow-sky-300 keeps its palette OKLCH value" true
    (Astring.String.is_infix
       ~affix:"--tw-text-shadow-color:oklch(82.8%.111 230.318)" css)

(* An arbitrary text-shadow reads every CSS length, not the px/rem/em subset. A
   token that is not a length used to drop out of the list and shift its
   neighbours along, so [0 1ch 2px] became a two-length [0 2px]. *)
let test_arbitrary_lengths () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits affix cls =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  emits "text-shadow: 0 1ch 2px var(--tw-text-shadow-color, #000)"
    "text-shadow-[0_1ch_2px_#000]";
  emits "text-shadow: 0 1ch 2px var(--tw-text-shadow-color, oklab(0% 0 0 / .5))"
    "text-shadow-[0_1ch_2px_#000]/50";
  match Tw.of_string "text-shadow-[0_bogus_2px]" with
  | Ok _ -> Alcotest.fail "expected text-shadow-[0_bogus_2px] to be rejected"
  | Error _ -> ()

(* An arbitrary text-shadow takes a colour function for its colour, the same as
   the box-shadow utilities. The reader knew only a [#] hex and a var(), so a
   function made the whole value stop being a shadow. A static one folds to its
   hex form; one with a channel that has no byte value stays as written. *)
let test_arbitrary_color_function () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  emits "text-shadow-[0_1px_rgb(255,0,0)]" "var(--tw-text-shadow-color,#f00)";
  emits "text-shadow-[0_1px_hsl(180deg_100%_50%)]"
    "var(--tw-text-shadow-color,#0ff)";
  emits "text-shadow-[0_1px_oklch(0.5_0.2_180)]"
    "var(--tw-text-shadow-color,oklch(";
  emits "text-shadow-[0_1px_rgb(var(--x)_0_0)]"
    "var(--tw-text-shadow-color,rgb(var(--x) 0 0))";
  (* An opacity modifier takes the alpha through oklab, the same as a [#] hex
     colour does. *)
  emits "text-shadow-[0_1px_rgb(255,0,0)]/50"
    "var(--tw-text-shadow-color,oklab(62.79553606%.22486306 .1258463/.5))";
  rejected "text-shadow-[0_1px_rgb(zz)]"

let test_arbitrary_colour_opacity () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "text-shadow-[0_0_8px_oklch(50%_0.2_250)]/50"
    "var(--tw-text-shadow-color,color-mix(";
  has "text-shadow-[0_0_8px_#f00]/[var(--x)]" "--tw-text-shadow-alpha:var(--x)";
  has "text-shadow-[0_0_8px_#f00]/[var(--x)]" "oklab(from"

(* An arbitrary text-shadow takes a named colour, the same as the box-shadow
   twin [shadow-[0_1px_2px_red]] does. The reader recognised a [#] hex, a var()
   and a colour function and nothing else, so a name fell through to the length
   slot, failed to read as a length, and took the whole utility down with it. *)
let test_arbitrary_named_colour () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  emits "text-shadow-[0_1px_2px_red]"
    "text-shadow:0 1px 2px var(--tw-text-shadow-color,red)";
  emits "text-shadow-[1px_1px_rebeccapurple]"
    "text-shadow:1px 1px var(--tw-text-shadow-color,rebeccapurple)";
  emits "text-shadow-[0_1px_2px_currentColor]"
    "var(--tw-text-shadow-color,currentcolor)";
  (* a word that names neither a length nor a colour is still not a shadow *)
  rejected "text-shadow-[0_1px_notacolour]"

(* A [#] value is only a colour when what follows is a hex spelling, both as the
   whole bracket and as the colour of an arbitrary shadow. The reader kept the
   text after the [#] as-is and the raising constructor saw it when the sheet
   was rendered, so a malformed hex escaped as an exception instead of failing
   the parse. *)
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
  rejected "text-shadow-[#zz]";
  rejected "text-shadow-[#]";
  rejected "text-shadow-[#12345]";
  rejected "text-shadow-[#zz]/50";
  rejected "text-shadow-[0_1px_2px_#zz]";
  emits "text-shadow-[#abc]" "--tw-text-shadow-color:#abc";
  emits "text-shadow-[0_1px_2px_#ff0000]"
    "text-shadow:0 1px 2px var(--tw-text-shadow-color,#f00)"

(* A bracket colour is a colour whatever spelling it takes. The bare-colour arm
   reached only the hex reader, so a name, an [oklch()] or an [rgb()] fell
   through to the arbitrary-shadow reader and was rejected outright. *)
let test_bracket_plain_colour () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  emits "text-shadow-[red]" "--tw-text-shadow-color:red";
  emits "text-shadow-[red]"
    "color-mix(in oklab,red var(--tw-text-shadow-alpha),transparent)";
  emits "text-shadow-[oklch(0.7_0.1_200)]"
    "--tw-text-shadow-color:oklch(.7 .1 200)";
  (* a colour function with a byte value for every channel folds to its hex
     spelling, the same as the arbitrary-shadow reader does with one *)
  emits "text-shadow-[rgb(255_0_0)]" "--tw-text-shadow-color:#f00";
  (* the modifier folds into the colour: sRGB for the plain fallback, oklab for
     the value the @supports block guards *)
  emits "text-shadow-[red]/50"
    "--tw-text-shadow-color:color-mix(in srgb,red 50%,transparent)";
  emits "text-shadow-[red]/50"
    "color-mix(in oklab,color-mix(in oklab,red 50%,transparent) \
     var(--tw-text-shadow-alpha),transparent)";
  (* a modifier reading a custom property has no percentage a plain fallback can
     hold, so only the guarded value mixes *)
  emits "text-shadow-[red]/[var(--x)]" "--tw-text-shadow-color:red";
  emits "text-shadow-[red]/[var(--x)]"
    "color-mix(in oklab,color-mix(in oklab,red var(--x),transparent) \
     var(--tw-text-shadow-alpha),transparent)"

(* The [color:] hint says the payload is a colour, not that it names a variable.
   Every payload was read as a variable name, so [text-shadow-[color:red]]
   emitted [var(--red)] where Tailwind emits [red]. A [var()] payload still
   reads as one, and the class name keeps the hint. *)
let test_colour_hint_takes_a_colour () =
  let value cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (value cls))
  in
  has "text-shadow-[color:red]" "--tw-text-shadow-color:red";
  has "text-shadow-[color:var(--x)]" "--tw-text-shadow-color:var(--x)";
  (* A var() with an opacity modifier keeps the plain reference as the fallback
     and mixes only inside the @supports guard, so it must not take the colour
     path even though var() parses as a colour. *)
  has "text-shadow-[color:var(--x)]/50" "--tw-text-shadow-color:var(--x)";
  (* The hint survives into the class name, so the class reads back. *)
  has "text-shadow-[color:red]" ".text-shadow-\\[color\\:red\\]"

(* The [shadow:] hint says the payload is a shadow, not that it names a
   variable. [text-shadow-[shadow:12px_12px_#0088cc]] wrote [text-shadow:
   var(--12px_12px_#0088cc)]. *)
let test_shadow_hint_takes_a_shadow () =
  Test_helpers.check_declarations "text-shadow-[shadow:12px_12px_#0088cc]"
    [ "text-shadow:12px 12px var(--tw-text-shadow-color,#08c)" ];
  (* a var() reference after the hint still names a custom property *)
  Test_helpers.check_declarations "text-shadow-[shadow:var(--value)]"
    [ "text-shadow:var(--value)" ];
  (* the class prints back with the hint the author wrote *)
  Alcotest.(check string)
    "text-shadow-[shadow:12px_12px_#0088cc] round-trips"
    "text-shadow-[shadow:12px_12px_#0088cc]"
    (Tw.pp
       (Result.get_ok (Tw.of_string "text-shadow-[shadow:12px_12px_#0088cc]")));
  (* A payload the shadow reader refuses is held open, not settled: Tailwind
     writes the bracket out whatever it says, so refusing is an intermediate. *)
  Test_helpers.check_invalid_input
    ~why:
      (Test_helpers.Diverges
         "emitted verbatim; tw needs an opaque declaration to match")
    (module Tw.Text_shadow.Handler)
    "text-shadow-[shadow:notashadow]"

(* The shadow's parts are separated by the [_] that stands for a space, so a
   variable name carrying an underscore of its own is written [\_]. *)
let test_underscore_escape () =
  let value cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "an escaped underscore stays in the variable name" true
    (Astring.String.is_infix
       ~affix:"text-shadow: 0 0 1px var(--tw-text-shadow-color, var(--a_b))"
       (value {|text-shadow-[0_0_1px_var(--a\_b)]|}))

let tests =
  [
    Alcotest.test_case "colour hint takes a colour" `Quick
      test_colour_hint_takes_a_colour;
    Alcotest.test_case "shadow hint takes a shadow" `Quick
      test_shadow_hint_takes_a_shadow;
    Alcotest.test_case "underscore escape" `Quick test_underscore_escape;
  ]
  @ Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "arbitrary lengths" `Quick test_arbitrary_lengths;
      Alcotest.test_case "default scale (v4.3.1)" `Quick test_default_scale;
      Alcotest.test_case "@theme override threads through" `Quick
        test_theme_override;
      Alcotest.test_case "palette color keeps OKLCH" `Quick
        test_palette_color_keeps_oklch;
      Alcotest.test_case "arbitrary colour function" `Quick
        test_arbitrary_color_function;
      Alcotest.test_case "arbitrary colour opacity" `Quick
        test_arbitrary_colour_opacity;
      Alcotest.test_case "arbitrary named colour" `Quick
        test_arbitrary_named_colour;
      Alcotest.test_case "invalid bracket hex" `Quick test_invalid_bracket_hex;
      Alcotest.test_case "bracket plain colour" `Quick test_bracket_plain_colour;
    ]

let suite = ("text_shadow", tests)
