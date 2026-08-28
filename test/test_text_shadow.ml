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
  emits "text-shadow: 0 1ch 2px var(--tw-text-shadow-color, #000000)"
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

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "arbitrary lengths" `Quick test_arbitrary_lengths;
      Alcotest.test_case "default scale (v4.3.1)" `Quick test_default_scale;
      Alcotest.test_case "@theme override threads through" `Quick
        test_theme_override;
      Alcotest.test_case "arbitrary colour function" `Quick
        test_arbitrary_color_function;
      Alcotest.test_case "arbitrary colour opacity" `Quick
        test_arbitrary_colour_opacity;
      Alcotest.test_case "arbitrary named colour" `Quick
        test_arbitrary_named_colour;
      Alcotest.test_case "invalid bracket hex" `Quick test_invalid_bracket_hex;
    ]

let suite = ("text_shadow", tests)
