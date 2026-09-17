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

(* The v4.3.1 default text-shadow scale: text-shadow-2xs uses alpha .15
   (#00000026), not the .1 (#0000001a) tw emitted before theme-threading. *)
let test_default_scale () =
  (* The whole declaration. A bare hex could match the theme binding rather than
     the utility, which is exactly what this test is about. *)
  Test_helpers.check_declarations "text-shadow-2xs"
    [ "text-shadow:0px 1px 0px var(--tw-text-shadow-color,#00000026)" ]

(* A threaded @theme override for the text-shadow token flows through to the
   inlined value (here .1 = #0000001a), which is impossible without
   threading. *)
let test_theme_override () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("text-shadow-2xs", "0px 1px 0px rgb(0 0 0 / 0.1)") ]
  in
  Test_helpers.check_declarations ~theme "text-shadow-2xs"
    [ "text-shadow:0px 1px 0px var(--tw-text-shadow-color,#0000001a)" ]

(* Default palette colours stay in their authored OKLCH space, matching the
   fallback Tailwind emits before its guarded [color-mix()] declaration. *)
let test_palette_color_keeps_oklch () =
  (* Both arms: the unguarded fallback in its authored OKLCH, then the mix
     behind the guard. The affix named the first alone. *)
  Test_helpers.check_declarations "text-shadow-sky-300"
    [
      "--tw-text-shadow-color:oklch(82.8%.111 230.318)";
      "--tw-text-shadow-color:color-mix(in oklab,var(--color-sky-300) \
       var(--tw-text-shadow-alpha),transparent)";
    ]

(* An arbitrary text-shadow reads every CSS length, not the px/rem/em subset. A
   token that is not a length used to drop out of the list and shift its
   neighbours along, so [0 1ch 2px] became a two-length [0 2px]. *)
let test_arbitrary_lengths () =
  (* The whole list, which is what says the bare form writes one declaration and
     the modified form writes two: the alpha channel travels with the value, and
     the substring could not see it either way. *)
  Test_helpers.check_declarations ~minify:false "text-shadow-[0_1ch_2px_#000]"
    [ "text-shadow: 0 1ch 2px var(--tw-text-shadow-color, #000)" ];
  (* The colour folds through oklab where the CLI writes the relative-colour
     form; the two are one colour and the canonical differ reports no
     difference. *)
  Test_helpers.check_declarations ~minify:false
    "text-shadow-[0_1ch_2px_#000]/50"
    [
      "--tw-text-shadow-alpha: 50%";
      "text-shadow: 0 1ch 2px var(--tw-text-shadow-color, oklab(0% 0 0 / .5))";
    ];
  match Tw.of_string "text-shadow-[0_bogus_2px]" with
  | Ok _ -> Alcotest.fail "expected text-shadow-[0_bogus_2px] to be rejected"
  | Error _ -> ()

(* An arbitrary text-shadow takes a colour function for its colour, the same as
   the box-shadow utilities. The reader knew only a [#] hex and a var(), so a
   function made the whole value stop being a shadow. A static one folds to its
   hex form; one with a channel that has no byte value stays as written. *)
let test_arbitrary_color_function () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  (* The whole declaration, offsets included: an affix on the colour slot said
     nothing about the lengths in front of it. *)
  let emits cls shadow =
    Test_helpers.check_declarations cls [ "text-shadow:" ^ shadow ]
  in
  emits "text-shadow-[0_1px_rgb(255,0,0)]"
    "0 1px var(--tw-text-shadow-color,#f00)";
  emits "text-shadow-[0_1px_hsl(180deg_100%_50%)]"
    "0 1px var(--tw-text-shadow-color,#0ff)";
  emits "text-shadow-[0_1px_oklch(0.5_0.2_180)]"
    "0 1px var(--tw-text-shadow-color,oklch(.5 .2 180))";
  emits "text-shadow-[0_1px_rgb(var(--x)_0_0)]"
    "0 1px var(--tw-text-shadow-color,rgb(var(--x) 0 0))";
  (* An opacity modifier takes the alpha through oklab, the same as a [#] hex
     colour does, and sets the alpha channel beside the shadow. *)
  Test_helpers.check_declarations "text-shadow-[0_1px_rgb(255,0,0)]/50"
    [
      "--tw-text-shadow-alpha:50%";
      "text-shadow:0 1px \
       var(--tw-text-shadow-color,oklab(62.79553606%.22486306 .1258463/.5))";
    ];
  rejected "text-shadow-[0_1px_rgb(zz)]"

(* A colour no hex spells takes the modifier's alpha in place of its own, as
   Tailwind's [oklab(from <colour> l a b / <alpha>)] does, and the relative form
   is what the sheet carries; a [color-mix()] would multiply the two alphas.

   Where the alpha reads a custom property there is nothing to fold, so the
   authored colour stands unguarded and the relative colour goes behind the
   guard. Folding that unguarded value through oklab at full opacity paints an
   opaque shadow in a browser with no relative colours, where Tailwind paints
   the authored colour. *)
let test_arbitrary_colour_opacity () =
  Test_helpers.check_declarations "text-shadow-[0_0_8px_oklch(50%_0.2_250)]/50"
    [
      "--tw-text-shadow-alpha:50%";
      "text-shadow:0 0 8px var(--tw-text-shadow-color,oklab(from oklch(50%.2 \
       250) l a b/.5))";
    ];
  Test_helpers.check_declarations ~minify:false
    "text-shadow-[0_0_8px_#f00]/[var(--x)]"
    [
      "--tw-text-shadow-alpha: var(--x)";
      "text-shadow: 0 0 8px var(--tw-text-shadow-color, #f00)";
      "text-shadow: 0 0 8px var(--tw-text-shadow-color, oklab(from #f00 l a \
       b/var(--x)))";
    ]

(* An arbitrary text-shadow takes a named colour, the same as the box-shadow
   twin [shadow-[0_1px_2px_red]] does. The reader recognised a [#] hex, a var()
   and a colour function and nothing else, so a name fell through to the length
   slot, failed to read as a length, and took the whole utility down with it. *)
let test_arbitrary_named_colour () =
  let emits cls decl = Test_helpers.check_declarations cls [ decl ] in
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
    "text-shadow:0 1px 2px var(--tw-text-shadow-color,currentcolor)";
  (* a word that names neither a length nor a colour is still not a shadow *)
  rejected "text-shadow-[0_1px_notacolour]"

(* A [#] value is only a colour when what follows is a hex spelling, both as the
   whole bracket and as the colour of an arbitrary shadow. The reader kept the
   text after the [#] as-is and the raising constructor saw it when the sheet
   was rendered, so a malformed hex escaped as an exception instead of failing
   the parse. *)
let test_invalid_bracket_hex () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "text-shadow-[#zz]";
  rejected "text-shadow-[#]";
  rejected "text-shadow-[#12345]";
  rejected "text-shadow-[#zz]/50";
  rejected "text-shadow-[0_1px_2px_#zz]";
  (* A bare colour writes the channel twice, unguarded then mixed. *)
  Test_helpers.check_declarations "text-shadow-[#abc]"
    [
      "--tw-text-shadow-color:#abc";
      "--tw-text-shadow-color:color-mix(in oklab,#abc \
       var(--tw-text-shadow-alpha),transparent)";
    ];
  Test_helpers.check_declarations "text-shadow-[0_1px_2px_#ff0000]"
    [ "text-shadow:0 1px 2px var(--tw-text-shadow-color,#f00)" ]

(* A bracket colour is a colour whatever spelling it takes. The bare-colour arm
   reached only the hex reader, so a name, an [oklch()] or an [rgb()] fell
   through to the arbitrary-shadow reader and was rejected outright. *)
let test_bracket_plain_colour () =
  (* Both arms of each: unguarded, then the mix behind the @supports guard. *)
  let emits cls colour =
    Test_helpers.check_declarations cls
      [
        "--tw-text-shadow-color:" ^ colour;
        "--tw-text-shadow-color:color-mix(in oklab," ^ colour
        ^ " var(--tw-text-shadow-alpha),transparent)";
      ]
  in
  emits "text-shadow-[red]" "red";
  emits "text-shadow-[oklch(0.7_0.1_200)]" "oklch(.7 .1 200)";
  (* a colour function with a byte value for every channel folds to its hex
     spelling, the same as the arbitrary-shadow reader does with one *)
  emits "text-shadow-[rgb(255_0_0)]" "#f00";
  (* Where the modifier folds, the two arms stop agreeing, so they are spelled
     out rather than derived from one colour. *)
  let arms cls fallback guarded =
    Test_helpers.check_declarations cls
      [
        "--tw-text-shadow-color:" ^ fallback;
        "--tw-text-shadow-color:color-mix(in oklab," ^ guarded
        ^ " var(--tw-text-shadow-alpha),transparent)";
      ]
  in
  (* the modifier folds into the colour: sRGB for the plain fallback, oklab for
     the value the @supports block guards *)
  arms "text-shadow-[red]/50" "color-mix(in srgb,red 50%,transparent)"
    "color-mix(in oklab,red 50%,transparent)";
  (* a modifier reading a custom property has no percentage a plain fallback can
     hold, so only the guarded value mixes *)
  arms "text-shadow-[red]/[var(--x)]" "red"
    "color-mix(in oklab,red var(--x),transparent)"

(* The [color:] hint says the payload is a colour, not that it names a variable.
   Every payload was read as a variable name, so [text-shadow-[color:red]]
   emitted [var(--red)] where Tailwind emits [red]. A [var()] payload still
   reads as one, and the class name keeps the hint. *)
let test_colour_hint_takes_a_colour () =
  let has cls colour mixed =
    Test_helpers.check_declarations cls
      [
        "--tw-text-shadow-color:" ^ colour;
        "--tw-text-shadow-color:color-mix(in oklab," ^ mixed
        ^ " var(--tw-text-shadow-alpha),transparent)";
      ]
  in
  has "text-shadow-[color:red]" "red" "red";
  has "text-shadow-[color:var(--x)]" "var(--x)" "var(--x)";
  (* A var() with an opacity modifier keeps the plain reference as the fallback
     and mixes only inside the @supports guard, so it must not take the colour
     path even though var() parses as a colour. *)
  has "text-shadow-[color:var(--x)]/50" "var(--x)"
    "color-mix(in oklab,var(--x) 50%,transparent)";
  (* The hint survives into the class name, so the class reads back. *)
  Alcotest.(check bool)
    "the hint is in the selector" true
    (List.mem {|.text-shadow-\[color\:red\]|}
       (Test_helpers.selectors_of_utility
          (Result.get_ok (Tw.of_string "text-shadow-[color:red]"))))

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
  Test_helpers.check_declarations ~minify:false
    {|text-shadow-[0_0_1px_var(--a\_b)]|}
    [ "text-shadow: 0 0 1px var(--tw-text-shadow-color, var(--a_b))" ]

(* A project [--text-shadow-<name>] is a text-shadow utility, with the modifier
   the scale takes: [text-shadow-pop] and [text-shadow-pop/50] were unknown
   classes. Its layers read as a box shadow's do, a trailing [var()] being the
   colour, and under a modifier a [var()] layer keeps its authored colour in the
   open and takes the alpha behind the relative colour guard, with the
   [color-mix()] guard for a [currentcolor] layer nested inside. *)
let test_project_token () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [
        ("text-shadow-pop", "0 1px 0 teal");
        ("text-shadow-glow", "0 0 8px currentColor, 0 0 2px var(--g)");
      ]
  in
  Test_helpers.check_declarations ~theme "text-shadow-pop"
    [ "text-shadow:0 1px 0 var(--tw-text-shadow-color,teal)" ];
  Test_helpers.check_declarations ~theme "text-shadow-pop/50"
    [
      "--tw-text-shadow-alpha:50%";
      "text-shadow:0 1px 0 \
       var(--tw-text-shadow-color,oklab(54.31225655%-.08964706 -.0236338/.5))";
    ];
  Test_helpers.check_declarations ~theme "text-shadow-glow"
    [
      "text-shadow:0 0 8px var(--tw-text-shadow-color,currentcolor),0 0 2px \
       var(--tw-text-shadow-color,var(--g))";
    ];
  let sheet =
    match Tw.of_string ~theme "text-shadow-glow/50" with
    | Ok u ->
        Tw.to_css ~theme ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "text-shadow-glow/50: %s" m
  in
  Alcotest.(check bool)
    "text-shadow-glow/50 nests the color-mix guard in the relative one" true
    (Astring.String.is_infix
       ~affix:
         {|@supports(color:lab(from red l a b)){.text-shadow-glow\/50{text-shadow:0 0 8px var(--tw-text-shadow-color,currentcolor),0 0 2px var(--tw-text-shadow-color,oklab(from var(--g) l a b/.5))}@supports(color:color-mix(in lab,red,red)){.text-shadow-glow\/50{text-shadow:0 0 8px var(--tw-text-shadow-color,color-mix(in oklab,currentcolor 50%,transparent)),0 0 2px var(--tw-text-shadow-color,oklab(from var(--g) l a b/.5))}}}|}
       sheet);
  Alcotest.(check bool)
    "an undeclared text-shadow name is rejected" true
    (Result.is_error (Tw.of_string ~theme "text-shadow-nope"))

(* A text shadow read whole from a custom property takes a modifier, under the
   [shadow:] hint as well: the alpha channel is set and the value kept, as
   Tailwind writes it. The bare form was refused, and the hinted one dropped the
   modifier from the name and the value; a hinted literal shadow folds the alpha
   as the plain bracket does. *)
let test_var_shadow_takes_a_modifier () =
  Test_helpers.check_declarations "text-shadow-[var(--s)]/50"
    [ "--tw-text-shadow-alpha:50%"; "text-shadow:var(--s)" ];
  Test_helpers.check_declarations "text-shadow-[shadow:var(--s)]/50"
    [ "--tw-text-shadow-alpha:50%"; "text-shadow:var(--s)" ];
  Test_helpers.check_declarations "text-shadow-[shadow:0_1px_red]/50"
    [
      "--tw-text-shadow-alpha:50%";
      "text-shadow:0 1px \
       var(--tw-text-shadow-color,oklab(62.79553606%.22486306 .1258463/.5))";
    ]

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
      Alcotest.test_case "project token" `Quick test_project_token;
      Alcotest.test_case "var shadow takes a modifier" `Quick
        test_var_shadow_takes_a_modifier;
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
