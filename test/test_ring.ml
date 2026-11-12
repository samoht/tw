module Css = Cascade.Css
open Alcotest

let check = Test_helpers.check_handler_roundtrip (module Tw.Ring.Handler)

(* A ring sets its own variable and composes the same five channels as every
   other box-shadow utility. *)
let composes_box_shadow =
  "box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)"

let test_ring_of_string_valid () =
  check "ring";
  check "ring-0";
  check "ring-1";
  check "ring-2";
  check "ring-4";
  check "ring-8";
  (* v4 accepts any bare integer width, not just the fixed scale *)
  check "ring-3";
  check "ring-5";
  check "ring-12";
  check "ring-inset";
  (* shadeless theme colours, with and without /opacity *)
  check "ring-black";
  check "ring-white/10";
  check "inset-ring-black";
  check "inset-ring-white/10"

(* A colour the project's [@theme] declares names a ring colour the way a
   shadeless palette colour does, with an optional [/opacity], on the ring, the
   inset ring and the offset. The three arms read the palette alone, so
   [ring-brand] was an unknown class where [shadow-brand] was not. *)
let test_ring_theme_colour () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("color-brand", "#123456") ]
  in
  Test_helpers.check_declarations ~theme "ring-brand"
    [ "--tw-ring-color:var(--color-brand)" ];
  Test_helpers.check_declarations ~theme "ring-offset-brand"
    [ "--tw-ring-offset-color:var(--color-brand)" ];
  Test_helpers.check_declarations ~theme "inset-ring-brand"
    [ "--tw-inset-ring-color:var(--color-brand)" ];
  Test_helpers.check_declarations ~theme "ring-brand/50"
    [
      "--tw-ring-color:#12345680";
      "--tw-ring-color:color-mix(in oklab,var(--color-brand) 50%,transparent)";
    ]

let test_ring_width_order () =
  Test_helpers.check_class_order ~test_name:"ring width order"
    [ "ring-8"; "ring-4"; "ring-3"; "ring-2"; "ring-1"; "ring-0"; "ring" ]

(* ring-black / ring-white (shadeless theme colours) parse with an optional
   /opacity; a shaded colour without a shade (ring-red) stays rejected. *)
let test_ring_shadeless_color () =
  Test_helpers.check_declarations "ring-black"
    [ "--tw-ring-color:var(--color-black)" ];
  (* The plain declaration carries the folded sRGB colour and the [@supports]
     copy the [color-mix] enhancement, so both are pinned. *)
  Test_helpers.check_declarations "ring-white/10"
    [
      "--tw-ring-color:#ffffff1a";
      "--tw-ring-color:color-mix(in oklab,var(--color-white) 10%,transparent)";
    ];
  (* Palette colours (blue-500) also apply the /opacity modifier on a var-ref
     theme; the ring family resolves it via oklab like bg/text do. *)
  Test_helpers.check_declarations "ring-blue-500/50"
    [
      "--tw-ring-color:#3080ff80";
      "--tw-ring-color:color-mix(in oklab,var(--color-blue-500) \
       50%,transparent)";
    ];
  Test_helpers.check_declarations "inset-ring-gray-950/10"
    [
      "--tw-inset-ring-color:#0307121a";
      "--tw-inset-ring-color:color-mix(in oklab,var(--color-gray-950) \
       10%,transparent)";
    ];
  match Tw.of_string "ring-red" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "ring-red (no shade) should be rejected"

(* ring-inset registers the ring/shadow @property family, like the other ring
   utilities; it used to emit only the --tw-ring-inset declaration. *)
let test_ring_inset_property_rules () =
  let open Tw in
  let css =
    match of_string "ring-inset" with
    | Ok u -> to_css ~base:false [ u ] |> Css.to_string
    | Error (`Msg m) -> Alcotest.failf "ring-inset: %s" m
  in
  Test_helpers.check_declarations "ring-inset" [ "--tw-ring-inset:inset" ];
  (* The [@property] rule is not a declaration on the class, so it is read off
     the sheet. *)
  Alcotest.check bool "ring-inset registers @property --tw-ring-shadow" true
    (Astring.String.is_infix ~affix:"@property --tw-ring-shadow" css)

(* The ring, inset ring and ring offset colours take the same modifier the same
   way: a palette colour, [currentcolor] or a bracket [var()] whose modifier
   reads a custom property mixes that property into the guarded value. Each
   family kept its own copies of the opacity arms, and every one folded the
   modifier to a percentage, so the guarded mix said [100%]. *)
let test_ring_colour_opacity_var () =
  let mixed var cls fallback colour =
    Test_helpers.check_declarations ~minify:false cls
      [
        var ^ ": " ^ fallback;
        var ^ ": color-mix(in oklab, " ^ colour ^ " var(--o), transparent)";
      ]
  in
  let family var prefix =
    mixed var (prefix ^ "-red-500/(--o)") "#fb2c36" "var(--color-red-500)";
    mixed var (prefix ^ "-current/[var(--o)]") "currentColor" "currentcolor";
    mixed var (prefix ^ "-[var(--c)]/(--o)") "var(--c)" "var(--c)";
    mixed var (prefix ^ "-[color:var(--c)]/[var(--o)]") "var(--c)" "var(--c)"
  in
  family "--tw-ring-color" "ring";
  family "--tw-inset-ring-color" "inset-ring";
  family "--tw-ring-offset-color" "ring-offset"

(* A named [--opacity-*] token is read off the theme before a bracket is told
   apart from a colour name, or [ring-[#123456]/half] is a palette colour called
   [[#123456]] and paints [var(--color-\[\#123456\])]. The fallback carries the
   percentage the token resolves to, in the sRGB mix Tailwind writes before its
   minifier folds it to a hex. *)
let test_ring_bracket_hex_named_opacity () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("opacity-half", "50%") ]
  in
  Test_helpers.check_declarations ~theme ~minify:false "ring-[#123456]/half"
    [
      "--tw-ring-color: color-mix(in srgb, #123456 50%, transparent)";
      "--tw-ring-color: color-mix(in oklab, #123456 var(--opacity-half), \
       transparent)";
    ];
  Test_helpers.check_declarations ~theme ~minify:false
    "ring-offset-[#123456]/half"
    [
      "--tw-ring-offset-color: color-mix(in srgb, #123456 50%, transparent)";
      "--tw-ring-offset-color: color-mix(in oklab, #123456 \
       var(--opacity-half), transparent)";
    ]

(* A shadeless palette colour names the ring offset the way it names the ring:
   Tailwind 4.3.3 writes [--tw-ring-offset-color:var(--color-white)] for
   [ring-offset-white], and folds an opacity into [#ffffff80] beside a
   [color-mix()]. Only the ring had a shadeless arm, so [ring-offset-white] and
   [ring-offset-black] reached the width reader and were unknown classes. A bare
   number after [ring-offset-] is still a width. *)
let test_ring_offset_shadeless_color () =
  Test_helpers.check_declarations "ring-offset-white"
    [ "--tw-ring-offset-color:var(--color-white)" ];
  Test_helpers.check_declarations "ring-offset-black"
    [ "--tw-ring-offset-color:var(--color-black)" ];
  Test_helpers.check_declarations "ring-offset-white/50"
    [
      "--tw-ring-offset-color:#ffffff80";
      "--tw-ring-offset-color:color-mix(in oklab,var(--color-white) \
       50%,transparent)";
    ];
  (* The class is spelled as the author wrote it: a shadeless colour has no
     shade to print, so [ring-offset-white] must not come back as
     [ring-offset-white-500]. *)
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok u -> Alcotest.(check string) (cls ^ " round-trips") cls (Tw.pp u)
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m)
    [
      "ring-offset-white";
      "ring-offset-black";
      "ring-offset-white/50";
      "ring-offset-2";
    ]

(* A bracket ring width is the ring [ring-2] draws with its width replaced: it
   reads [--tw-ring-inset] and [--tw-ring-color], and declares them, as the
   scale does. [ring-[3px]] left both out of the properties layer, so the sheet
   dropped the reads and the ring lost its inset toggle and its colour. A width
   takes no opacity: Tailwind compiles nothing for [ring-[3px]/50]. *)
let test_ring_bracket_width_reads_its_variables () =
  Test_helpers.check_declarations "ring-[3px]"
    [
      "--tw-ring-shadow:var(--tw-ring-inset,) 0 0 0 calc(3px + \
       var(--tw-ring-offset-width)) var(--tw-ring-color,currentcolor)";
      composes_box_shadow;
    ];
  let css =
    match Tw.of_string "ring-[3px]" with
    | Ok u -> Tw.to_css ~base:false [ u ]
    | Error (`Msg m) -> Alcotest.failf "ring-[3px]: %s" m
  in
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (name ^ " is declared in the properties layer")
        true
        (Test_helpers.has_var_in_layer name "properties" css))
    [ "--tw-ring-inset"; "--tw-ring-color" ];
  List.iter
    (fun cls ->
      Alcotest.(check bool)
        (cls ^ " is rejected") true
        (Result.is_error (Tw.of_string cls)))
    [
      "ring-[3px]/50";
      "ring-[length:3px]/50";
      "ring-offset-[3px]/50";
      "inset-ring-[3px]/50";
    ]

(* A token-stream ring colour under a modifier read from a custom property is
   the pair Tailwind's polyfill writes: the bare value in the open and the mix,
   reading the property the class named, behind the colour-mix guard. A named
   token reads the theme's percentage into an sRGB mix in the open. The raw arm
   folded either modifier to [100%] and wrote nothing beside it. *)
let test_raw_ring_colour_opacity_var () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("opacity-half", "50%") ]
  in
  let pair cls channel ~open_ ~alpha =
    Test_helpers.check_declarations ~theme ~minify:false cls
      [
        channel ^ ": " ^ open_;
        channel ^ ": color-mix(in oklab, foo(1) " ^ alpha ^ ", transparent)";
      ]
  in
  pair "ring-[foo(1)]/(--o)" "--tw-ring-color" ~open_:"foo(1)" ~alpha:"var(--o)";
  pair "ring-[foo(1)]/[var(--o)]" "--tw-ring-color" ~open_:"foo(1)"
    ~alpha:"var(--o)";
  pair "ring-offset-[foo(1)]/(--o)" "--tw-ring-offset-color" ~open_:"foo(1)"
    ~alpha:"var(--o)";
  pair "inset-ring-[foo(1)]/(--o)" "--tw-inset-ring-color" ~open_:"foo(1)"
    ~alpha:"var(--o)";
  pair "ring-[foo(1)]/half" "--tw-ring-color"
    ~open_:"color-mix(in srgb, foo(1) 50%, transparent)"
    ~alpha:"var(--opacity-half)";
  (* A percentage needs no guard. *)
  Test_helpers.check_declarations ~minify:false "ring-[foo(1)]/50"
    [ "--tw-ring-color: color-mix(in oklab, foo(1) 50%, transparent)" ]

(* A ring colour sets its channel and registers nothing: Tailwind writes no
   [@property] for [ring-[foo(1)]], as it writes none for [ring-red-500]. The
   raw arm registered the whole shadow family, which put an [@layer properties]
   block under a class that declares one custom property. *)
let test_raw_ring_colour_registers_nothing () =
  List.iter
    (fun cls ->
      let css =
        match Tw.of_string cls with
        | Ok u -> Tw.to_css ~base:false [ u ] |> Css.to_string
        | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
      in
      Alcotest.check bool
        (cls ^ " registers no @property")
        false
        (Astring.String.is_infix ~affix:"@property" css))
    [
      "ring-[foo(1)]";
      "ring-[foo(1)]/50";
      "ring-offset-[foo(1)]";
      "inset-ring-[foo(1)]";
    ]

let tests =
  [
    test_case "ring of_string - valid values" `Quick test_ring_of_string_valid;
    test_case "ring theme colour" `Quick test_ring_theme_colour;
    test_case "ring width order" `Slow test_ring_width_order;
    test_case "ring shadeless color opacity" `Quick test_ring_shadeless_color;
    test_case "ring-inset @property family" `Quick
      test_ring_inset_property_rules;
    test_case "ring colour opacity from a var" `Quick
      test_ring_colour_opacity_var;
    test_case "ring bracket hex named opacity" `Quick
      test_ring_bracket_hex_named_opacity;
    test_case "ring offset shadeless color" `Quick
      test_ring_offset_shadeless_color;
    test_case "ring bracket width reads its variables" `Quick
      test_ring_bracket_width_reads_its_variables;
    test_case "token-stream ring colour opacity from a var" `Quick
      test_raw_ring_colour_opacity_var;
    test_case "token-stream ring colour registers nothing" `Quick
      test_raw_ring_colour_registers_nothing;
  ]

let suite = ("ring", tests)
