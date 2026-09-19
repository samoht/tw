module Css = Cascade.Css
open Alcotest

let check = Test_helpers.check_handler_roundtrip (module Tw.Effects.Handler)

(* Every box-shadow utility composes the same five channels; only the channel it
   sets ahead of this differs. Naming it keeps the expectations below readable
   without weakening them: it is one string, compared whole. *)
let composes_box_shadow =
  "box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)"

let of_string_valid () =
  (* Box shadow *)
  check "shadow";
  check "shadow-2xs";
  check "shadow-xs";
  check "shadow-sm";
  check "shadow-md";
  check "shadow-lg";
  check "shadow-xl";
  check "shadow-2xl";
  check "shadow-inner";
  check "shadow-none";

  (* Opacity *)
  check "opacity-0";
  check "opacity-5";
  check "opacity-10";
  check "opacity-25";
  check "opacity-50";
  check "opacity-75";
  check "opacity-100";

  (* Mix blend mode *)
  check "mix-blend-normal";
  check "mix-blend-multiply";
  check "mix-blend-screen";
  check "mix-blend-overlay"

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

(* A project shadow takes a [/opacity] the way a built-in size does: the
   modifier's alpha replaces every layer's, as Tailwind's relative [oklab()]
   does, and [--tw-shadow-alpha] carries it. [shadow-card/50] was an unknown
   class. An arbitrary shadow whose colour carries an alpha of its own folds the
   same way; the colour's alpha was kept or multiplied instead. *)
let test_project_shadow_opacity () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [
        ("shadow-card", "0 1px 2px rgb(0 0 0 / 0.1)");
        ("inset-shadow-deep", "inset 0 4px 8px rgb(0 0 0 / 0.2)");
      ]
  in
  let composition =
    "box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow),var(--tw-ring-offset-shadow),var(--tw-ring-shadow),var(--tw-shadow)"
  in
  Test_helpers.check_declarations ~theme "shadow-card/50"
    [
      "--tw-shadow-alpha:50%";
      "--tw-shadow:0 1px 2px var(--tw-shadow-color,oklab(0%0 0/.5))";
      composition;
    ];
  Test_helpers.check_declarations ~theme "inset-shadow-deep/50"
    [
      "--tw-inset-shadow-alpha:50%";
      "--tw-inset-shadow:inset 0 4px 8px var(--tw-inset-shadow-color,oklab(0%0 \
       0/.5))";
      composition;
    ];
  List.iter
    (fun cls ->
      Test_helpers.check_declarations cls
        [
          "--tw-shadow-alpha:50%";
          "--tw-shadow:0 1px 2px var(--tw-shadow-color,oklab(0%0 0/.5))";
          composition;
        ])
    [
      "shadow-[0_1px_2px_#0000001a]/50"; "shadow-[0_1px_2px_rgb(0,0,0,0.1)]/50";
    ]

(* An [inset-shadow-[...]] bracket that spells [inset] itself is refused. The
   utility supplies the keyword, so Tailwind writes the author's on top of it,
   [inset inset 0 1px red]: a value a registered [syntax: "*"] property holds
   and the [box-shadow] composition then cannot compute, so the element draws no
   shadow at all. That value has no typed form; refusing the class draws nothing
   for it, where tw used to draw the shadow the author did not get.
   [check_invalid_input] does not apply: Tailwind compiles the class. *)
let test_inset_shadow_bracket_refuses_inset () =
  List.iter
    (fun cls ->
      Alcotest.(check bool)
        (cls ^ " spells inset itself")
        true
        (Result.is_error (Tw.of_string cls)))
    [
      "inset-shadow-[inset_0_1px_red]";
      "inset-shadow-[inset_0_1px_red]/50";
      "inset-shadow-[0_1px_red,inset_0_2px_blue]";
    ];
  Alcotest.(check bool)
    "a bracket without inset is the inset shadow" true
    (Result.is_ok (Tw.of_string "inset-shadow-[0_1px_red]"))

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

(* A filter utility sets its own channel and recomposes the whole filter list
   from every channel, so the composition is the same string whichever one is
   set. The backdrop families write the [-webkit-] alias first. *)
let test_filters_css_generation () =
  let filter_composition =
    "filter:var(--tw-blur,)var(--tw-brightness,)var(--tw-contrast,)var(--tw-grayscale,)var(--tw-hue-rotate,)var(--tw-invert,)var(--tw-saturate,)var(--tw-sepia,)var(--tw-drop-shadow,)"
  in
  let backdrop_composition prefix =
    prefix
    ^ ":var(--tw-backdrop-blur,)var(--tw-backdrop-brightness,)var(--tw-backdrop-contrast,)var(--tw-backdrop-grayscale,)var(--tw-backdrop-hue-rotate,)var(--tw-backdrop-invert,)var(--tw-backdrop-opacity,)var(--tw-backdrop-saturate,)var(--tw-backdrop-sepia,)"
  in
  let backdrop channel value =
    [
      channel ^ ":" ^ value;
      backdrop_composition "-webkit-backdrop-filter";
      backdrop_composition "backdrop-filter";
    ]
  in
  Test_helpers.check_declarations "blur"
    [ "--tw-blur:blur(8px)"; filter_composition ];
  Test_helpers.check_declarations "backdrop-blur-lg"
    (backdrop "--tw-backdrop-blur" "blur(var(--blur-lg))");
  Test_helpers.check_declarations "backdrop-brightness-125"
    (backdrop "--tw-backdrop-brightness" "brightness(125%)");
  Test_helpers.check_declarations "backdrop-opacity-50"
    (backdrop "--tw-backdrop-opacity" "opacity(50%)")

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

let of_string_invalid () =
  (* Invalid effects values *)
  let fail_maybe input =
    let class_name = String.concat "-" input in
    match Tw.Effects.Handler.of_class Tw.Scheme.default class_name with
    | Ok _ -> fail ("Expected error for: " ^ class_name)
    | Error _ -> ()
  in

  fail_maybe [ "shadow"; "3xl" ];
  (* Invalid shadow size *)
  fail_maybe [ "opacity"; "110" ];
  (* Invalid opacity value *)
  fail_maybe [ "mix"; "blend"; "invalid" ];
  (* Invalid blend mode *)
  fail_maybe [ "unknown" ]
(* Unknown effects type *)

let suborder_matches_tailwind () =
  let open Tw in
  let utilities =
    [
      shadow_sm;
      shadow;
      shadow_md;
      shadow_lg;
      shadow_none;
      opacity 0;
      opacity 50;
      opacity 100;
    ]
  in
  let shuffled = Test_helpers.shuffle utilities in

  Test_helpers.check_ordering_matches
    ~test_name:"effects suborder matches Tailwind" shuffled

(* A shadow size and a shadow colour meet in --tw-shadow, so which one an
   element ends up painting is only settled once the sheet is rendered. *)
let rendering_matches_tailwind () =
  let classes =
    [
      "shadow-2xs";
      "shadow-xs";
      "shadow-sm";
      "shadow";
      "shadow-md";
      "shadow-lg";
      "shadow-none";
      (* The keyword colours conflict with the sizes just as well as palette
         colours; palette fallback syntax has a focused regression below. *)
      "shadow-current";
      "shadow-transparent";
      "inset-shadow-sm";
      "opacity-0";
      "opacity-50";
      "opacity-100";
      "mix-blend-multiply";
    ]
  in
  Test_helpers.check_rendering_matches ~test_name:"effects render like Tailwind"
    (List.map (fun c -> Result.get_ok (Tw.of_string c)) classes)

(* shadow-2xl's default shadow alpha is .25 (#00000040) in v4, not the .10
   (#0000001a) the smaller shadows use. *)
let test_shadow_2xl_alpha () =
  Test_helpers.check_declarations "shadow-2xl"
    [
      "--tw-shadow:0 25px 50px -12px var(--tw-shadow-color,#00000040)";
      composes_box_shadow;
    ]

(* The two smallest box-shadow sizes (alpha .05 = #0000000d): 2xs is a single 0
   1px shadow with no blur, xs is 0 1px 2px 0. *)
let test_shadow_small_sizes () =
  Test_helpers.check_declarations "shadow-2xs"
    [
      "--tw-shadow:0 1px var(--tw-shadow-color,#0000000d)"; composes_box_shadow;
    ];
  Test_helpers.check_declarations "shadow-xs"
    [
      "--tw-shadow:0 1px 2px 0 var(--tw-shadow-color,#0000000d)";
      composes_box_shadow;
    ]

(* The v4.3.1 default inset-shadow scale is inset-shadow-{2xs,xs,sm} plus
   inset-shadow-none. Bare inset-shadow and md/lg/xl/2xl do not exist. *)
let test_inset_shadow_roundtrip () =
  check "inset-shadow-none";
  check "inset-shadow-2xs";
  check "inset-shadow-xs";
  check "inset-shadow-sm"

let test_inset_shadow_invalid () =
  (* Bare inset-shadow has no v4.3.1 default token, and md/lg/xl/2xl were
     removed from the scale. *)
  Test_helpers.check_invalid_input (module Tw.Effects.Handler) "inset-shadow";
  Test_helpers.check_invalid_input (module Tw.Effects.Handler) "inset-shadow-md";
  Test_helpers.check_invalid_input
    (module Tw.Effects.Handler)
    "inset-shadow-2xl"

(* The default scale (alpha .05 = #0000000d): 2xs is a single inset shadow with
   no blur ([inset 0 1px]); sm is [inset 0 2px 4px]. *)
let test_inset_shadow_default_scale () =
  Test_helpers.check_declarations "inset-shadow-2xs"
    [
      "--tw-inset-shadow:inset 0 1px var(--tw-inset-shadow-color,#0000000d)";
      composes_box_shadow;
    ];
  Test_helpers.check_declarations "inset-shadow-sm"
    [
      "--tw-inset-shadow:inset 0 2px 4px var(--tw-inset-shadow-color,#0000000d)";
      composes_box_shadow;
    ]

(* A threaded @theme override for an inset-shadow token flows through to the
   inlined value. The default inset-shadow-sm is [inset 0 2px 4px]; with the
   override below it becomes [inset 0 1px 1px], which is impossible without
   theme threading. *)
let test_inset_shadow_theme_override () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("inset-shadow-sm", "inset 0 1px 1px rgb(0 0 0 / 0.05)") ]
  in
  (* The whole list, which is where "drops the default" is said: it used to be a
     second search for a spelling that must not appear, and a sheet with no rule
     in it satisfies that too. *)
  Test_helpers.check_declarations ~theme "inset-shadow-sm"
    [
      "--tw-inset-shadow:inset 0 1px 1px var(--tw-inset-shadow-color,rgb(0 0 \
       0/.05))";
      composes_box_shadow;
    ]

(* The override is read through the CSS shadow grammar, so it carries whatever
   that grammar allows: a length in any unit, a fourth length for the spread,
   and the colour in the spelling the project wrote. The reader used to take a
   [px] or [rem] suffix and a three-length body only, and answered nothing for
   the rest, which put the built-in [inset 0 2px 4px] in the sheet and lost the
   override without saying so. *)
let test_inset_shadow_theme_override_grammar () =
  (* The whole list per override. "The default is not substituted" was a search
     for a spelling that must not appear; the list says it by construction, and
     also that the colour the project wrote reached the fallback slot. *)
  let holds ~override body =
    let theme =
      Tw.Scheme.with_overrides Tw.Scheme.default
        [ ("inset-shadow-sm", override) ]
    in
    Test_helpers.check_declarations ~theme "inset-shadow-sm"
      [
        "--tw-inset-shadow:" ^ body
        ^ " var(--tw-inset-shadow-color,rgb(0 0 0/.05))";
        composes_box_shadow;
      ]
  in
  (* an em override keeps its unit *)
  holds ~override:"inset 0 0.125em 0.25em rgb(0 0 0 / 0.05)"
    "inset 0 .125em .25em";
  (* a fourth length is the spread *)
  holds ~override:"inset 0 1px 2px 3px rgb(0 0 0 / 0.05)" "inset 0 1px 2px 3px"

(* A shadeless colour has no shade segment, so shadow-white never reached the
   colour parse: the size cases claimed the segment and rejected it. The class
   name drops the shade too, or it comes back as shadow-white-500. *)
let test_shadeless_shadow_colors () =
  Test_helpers.check_declarations "shadow-white"
    [
      "--tw-shadow-color:#fff";
      "--tw-shadow-color:color-mix(in oklab,var(--color-white) \
       var(--tw-shadow-alpha),transparent)";
    ];
  Test_helpers.check_declarations "shadow-white/10"
    [
      "--tw-shadow-color:#ffffff1a";
      "--tw-shadow-color:color-mix(in oklab,color-mix(in \
       oklab,var(--color-white) 10%,transparent) \
       var(--tw-shadow-alpha),transparent)";
    ];
  Test_helpers.check_declarations "inset-shadow-white"
    [
      "--tw-inset-shadow-color:#fff";
      "--tw-inset-shadow-color:color-mix(in oklab,var(--color-white) \
       var(--tw-inset-shadow-alpha),transparent)";
    ];
  Test_helpers.check_declarations "inset-shadow-white/20"
    [
      "--tw-inset-shadow-color:#fff3";
      "--tw-inset-shadow-color:color-mix(in oklab,color-mix(in \
       oklab,var(--color-white) 20%,transparent) \
       var(--tw-inset-shadow-alpha),transparent)";
    ]

(* A default palette token is already an OKLCH colour. Tailwind keeps that value
   as the unguarded shadow-colour fallback; converting it to sRGB hex changes
   wide-gamut colours before the [color-mix()] enhancement applies. *)
let test_palette_shadow_colors_keep_oklch () =
  Test_helpers.check_declarations "shadow-indigo-500"
    [
      "--tw-shadow-color:oklch(58.5%.233 277.117)";
      "--tw-shadow-color:color-mix(in oklab,var(--color-indigo-500) \
       var(--tw-shadow-alpha),transparent)";
    ];
  Test_helpers.check_declarations "inset-shadow-indigo-500"
    [
      "--tw-inset-shadow-color:oklch(58.5%.233 277.117)";
      "--tw-inset-shadow-color:color-mix(in oklab,var(--color-indigo-500) \
       var(--tw-inset-shadow-alpha),transparent)";
    ]

(* shadow-inner is a shadow shape like the others: it sets --tw-shadow and
   composes, rather than writing box-shadow directly. *)
let test_shadow_inner () =
  Test_helpers.check_declarations "shadow-inner"
    [
      "--tw-shadow:inset 0 2px 4px 0 var(--tw-shadow-color,#0000000d)";
      composes_box_shadow;
    ]

(* A shadow list is one shadow per layer. The single-shadow reading also drops
   the spread, so anything with a comma goes to the value parser. *)
let test_arbitrary_shadow_list () =
  Test_helpers.check_declarations
    "shadow-[-5px_10px_15px_-3px_var(--shadow-color),-5px_4px_6px_-4px_var(--shadow-color)]"
    [
      "--tw-shadow:-5px 10px 15px -3px \
       var(--tw-shadow-color,var(--shadow-color)),-5px 4px 6px -4px \
       var(--tw-shadow-color,var(--shadow-color))";
      composes_box_shadow;
    ]

(* A single arbitrary shadow reads every CSS length, not the px/rem/em subset,
   and keeps its spread. A token that is not a length makes the whole value not
   a shadow rather than dropping out and shifting its neighbours along. *)
let test_arbitrary_shadow_lengths () =
  let shadow cls value =
    Test_helpers.check_declarations cls
      [ "--tw-shadow:" ^ value; composes_box_shadow ]
  in
  let inset_shadow cls value =
    Test_helpers.check_declarations cls
      [ "--tw-inset-shadow:" ^ value; composes_box_shadow ]
  in
  let with_alpha cls alpha value =
    Test_helpers.check_declarations cls
      [
        "--tw-shadow-alpha:" ^ alpha;
        "--tw-shadow:" ^ value;
        composes_box_shadow;
      ]
  in
  with_alpha "shadow-[0_1ch_2px_#000]/50" "50%"
    "0 1ch 2px var(--tw-shadow-color,oklab(0%0 0/.5))";
  with_alpha "shadow-[0_1px_2px_3px_#000]/50" "50%"
    "0 1px 2px 3px var(--tw-shadow-color,oklab(0%0 0/.5))";
  with_alpha "shadow-[0_1ch_2px_3vmin_#000]/50" "50%"
    "0 1ch 2px 3vmin var(--tw-shadow-color,oklab(0%0 0/.5))";
  (* inset-shadow reads its arbitrary value through the same parser, so a token
     that is not a length shifts nothing along there either. *)
  inset_shadow "inset-shadow-[0_1ch_2px_#000]"
    "inset 0 1ch 2px var(--tw-inset-shadow-color,#000)";
  inset_shadow "inset-shadow-[0_1px_2px_3px_#000]"
    "inset 0 1px 2px 3px var(--tw-inset-shadow-color,#000)";
  shadow "shadow-[0_bogus_2px]" "0 var(--tw-shadow-color,bogus) 2px";
  with_alpha "shadow-[0_bogus_2px]/50" "50%"
    "0 var(--tw-shadow-color,oklab(from bogus l a b / 50%)) 2px";
  inset_shadow "inset-shadow-[0_bogus_2px]"
    "inset 0 var(--tw-inset-shadow-color,bogus) 2px"

(* An arbitrary inset shadow whose colour is one CSS knows by name. Two readers
   decide the bracket between them: the one that accepts it as a shadow reads
   every colour spelling, the one that then builds the value knows only a hex, a
   var() and a colour function. A named colour passed the first and failed the
   second, so the whole shadow fell out as [inset-shadow-none] - lengths, spread
   and colour together. A token that is no colour at all, such as [bogus], never
   reached the second reader and was right throughout. *)
let test_arbitrary_inset_shadow_named_colour () =
  let inset_shadow cls value =
    Test_helpers.check_declarations cls
      [ "--tw-inset-shadow:" ^ value; composes_box_shadow ]
  in
  inset_shadow "inset-shadow-[0_0_red]"
    "inset 0 0 var(--tw-inset-shadow-color,red)";
  inset_shadow "inset-shadow-[0_0_0_1px_red]"
    "inset 0 0 0 1px var(--tw-inset-shadow-color,red)";
  inset_shadow "inset-shadow-[shadow:0_0_0_1px_red]"
    "inset 0 0 0 1px var(--tw-inset-shadow-color,red)";
  inset_shadow "inset-shadow-[0_0_red,0_0_blue]"
    "inset 0 0 var(--tw-inset-shadow-color,red),inset 0 0 \
     var(--tw-inset-shadow-color,blue)"

(* The same bracket under an opacity modifier. The reader that builds the value
   there has no fall-back to the reader that accepted the bracket, so a colour
   keyword, a leading colour, the [inset] keyword and a layer list all lost the
   whole shadow and the modifier with it.

   Tailwind writes the colour as [oklab(from <colour> l a b / <alpha>)] and lets
   its minifier fold it, so a colour whose value is fixed where it is written
   arrives folded and unguarded; the modifier's alpha replaces the colour's own
   rather than multiplying it.

   The two neighbours are pinned alongside because a fix to one reader can break
   the other: a hex already folded correctly, and a token that is no colour at
   all never reaches this reader. *)
let test_arbitrary_shadow_named_colour_opacity () =
  let red_50 = "oklab(62.79553606%.22486306 .1258463/.5)" in
  let shadow cls value =
    Test_helpers.check_declarations cls
      [ "--tw-shadow-alpha:50%"; "--tw-shadow:" ^ value; composes_box_shadow ]
  in
  let inset_shadow cls value =
    Test_helpers.check_declarations cls
      [
        "--tw-inset-shadow-alpha:50%";
        "--tw-inset-shadow:" ^ value;
        composes_box_shadow;
      ]
  in
  shadow "shadow-[0_0_red]/50" ("0 0 var(--tw-shadow-color," ^ red_50 ^ ")");
  inset_shadow "inset-shadow-[0_0_0_1px_red]/50"
    ("inset 0 0 0 1px var(--tw-inset-shadow-color," ^ red_50 ^ ")");
  shadow "shadow-[inset_0_0_red]/50"
    ("inset 0 0 var(--tw-shadow-color," ^ red_50 ^ ")");
  inset_shadow "inset-shadow-[0_0_red,0_0_0_1px_red]/50"
    ("inset 0 0 var(--tw-inset-shadow-color," ^ red_50
   ^ "),inset 0 0 0 1px var(--tw-inset-shadow-color," ^ red_50 ^ ")");
  (* The hex neighbour, which folds to the same colour. *)
  shadow "shadow-[0_0_#f00]/50" ("0 0 var(--tw-shadow-color," ^ red_50 ^ ")");
  (* The neighbour with no colour at all: [bogus] is no colour CSS knows, so the
     bracket is not a shadow and the whole value is written through as text. *)
  shadow "shadow-[0_bogus_2px]/50"
    "0 var(--tw-shadow-color,oklab(from bogus l a b / 50%)) 2px";
  (* A colour written first. cascade's reader moves it to the end, which is a
     difference against Tailwind in its own right and holds with or without the
     modifier; what the modifier must not do is lose the shadow. *)
  Test_helpers.check_declarations "shadow-[red_0_0]"
    [ "--tw-shadow:0 0 var(--tw-shadow-color,red)"; composes_box_shadow ];
  shadow "shadow-[red_0_0]/50" ("0 0 var(--tw-shadow-color," ^ red_50 ^ ")")

(* An alpha that reads a custom property has no percentage to fold, so the
   authored colour - the keyword the class wrote, not its computed [oklab()] -
   stands unguarded and the relative colour goes behind the guard, the way a
   bracket hex already does. *)
let test_arbitrary_shadow_named_colour_var_opacity () =
  Test_helpers.check_declarations "shadow-[0_0_red]/[var(--x)]"
    [
      "--tw-shadow-alpha:var(--x)";
      "--tw-shadow:0 0 var(--tw-shadow-color,red)";
      "--tw-shadow:0 0 var(--tw-shadow-color,oklab(from red l a b/var(--x)))";
      composes_box_shadow;
    ];
  Test_helpers.check_declarations "inset-shadow-[0_0_0_1px_red]/[var(--x)]"
    [
      "--tw-inset-shadow-alpha:var(--x)";
      "--tw-inset-shadow:inset 0 0 0 1px var(--tw-inset-shadow-color,red)";
      "--tw-inset-shadow:inset 0 0 0 1px \
       var(--tw-inset-shadow-color,oklab(from red l a b/var(--x)))";
      composes_box_shadow;
    ]

(* [shadow-[<colour>]/<alpha>] where the colour is one no hex spells: the
   modifier's alpha takes the place of the colour's own, as Tailwind's
   [oklab(from <colour> l a b / <alpha>)] does, and the relative form is what
   the sheet carries. A [color-mix()] would multiply the two alphas instead.

   Where the alpha reads a custom property there is nothing to fold, so the
   authored colour stands unguarded and the relative colour goes behind the
   guard. The box-shadow half folded that unguarded value through oklab at full
   opacity, which paints an opaque shadow in a browser with no relative colours
   where Tailwind paints the authored colour; the inset twin already left it
   alone, so the two halves of one family disagreed. *)
let test_arbitrary_shadow_colour_opacity () =
  Test_helpers.check_declarations "shadow-[0_0_8px_oklch(50%_0.2_250)]/50"
    [
      "--tw-shadow-alpha:50%";
      "--tw-shadow:0 0 8px var(--tw-shadow-color,oklab(from oklch(50%.2 250) l \
       a b/.5))";
      composes_box_shadow;
    ];
  Test_helpers.check_declarations "inset-shadow-[0_0_8px_oklch(50%_0.2_250)]/50"
    [
      "--tw-inset-shadow-alpha:50%";
      "--tw-inset-shadow:inset 0 0 8px var(--tw-inset-shadow-color,oklab(from \
       oklch(50%.2 250) l a b/.5))";
      composes_box_shadow;
    ];
  Test_helpers.check_declarations "shadow-[0_0_8px_#f00]/[var(--x)]"
    [
      "--tw-shadow-alpha:var(--x)";
      "--tw-shadow:0 0 8px var(--tw-shadow-color,#f00)";
      "--tw-shadow:0 0 8px var(--tw-shadow-color,oklab(from #f00 l a \
       b/var(--x)))";
      composes_box_shadow;
    ];
  Test_helpers.check_declarations "inset-shadow-[0_0_8px_#f00]/[var(--x)]"
    [
      "--tw-inset-shadow-alpha:var(--x)";
      "--tw-inset-shadow:inset 0 0 8px var(--tw-inset-shadow-color,#f00)";
      "--tw-inset-shadow:inset 0 0 8px var(--tw-inset-shadow-color,oklab(from \
       #f00 l a b/var(--x)))";
      composes_box_shadow;
    ];
  (* Minified printing folds a colour to its shortest hex, so the authored
     three-digit spelling the CLI keeps only shows unminified. *)
  Test_helpers.check_declarations ~minify:false
    "shadow-[0_0_8px_#f00]/[var(--x)]"
    [
      "--tw-shadow-alpha: var(--x)";
      "--tw-shadow: 0 0 8px var(--tw-shadow-color, #f00)";
      "--tw-shadow: 0 0 8px var(--tw-shadow-color, oklab(from #f00 l a \
       b/var(--x)))";
      "box-shadow: var(--tw-inset-shadow), var(--tw-inset-ring-shadow), \
       var(--tw-ring-offset-shadow), var(--tw-ring-shadow), var(--tw-shadow)";
    ];
  Test_helpers.check_declarations ~minify:false
    "inset-shadow-[0_0_8px_#f00]/[var(--x)]"
    [
      "--tw-inset-shadow-alpha: var(--x)";
      "--tw-inset-shadow: inset 0 0 8px var(--tw-inset-shadow-color, #f00)";
      "--tw-inset-shadow: inset 0 0 8px var(--tw-inset-shadow-color, \
       oklab(from #f00 l a b/var(--x)))";
      "box-shadow: var(--tw-inset-shadow), var(--tw-inset-ring-shadow), \
       var(--tw-ring-offset-shadow), var(--tw-ring-shadow), var(--tw-shadow)";
    ]

(* A bracket colour with no sRGB hex - [oklch()] and the other wide-gamut
   spellings - has no hex to fold the modifier's alpha into, so the alpha has to
   stay a mix. Tailwind writes the plain fallback in sRGB and the guarded value
   in oklab; the colour used to fall through untouched, which painted the shadow
   fully opaque and dropped the modifier. *)
let test_bracket_colour_opacity_without_hex () =
  Test_helpers.check_declarations "shadow-[oklch(0.7_0.1_200)]/50"
    [
      "--tw-shadow-color:color-mix(in srgb,oklch(.7 .1 200) 50%,transparent)";
      "--tw-shadow-color:color-mix(in oklab,color-mix(in oklab,oklch(.7 .1 \
       200) 50%,transparent) var(--tw-shadow-alpha),transparent)";
    ];
  Test_helpers.check_declarations "inset-shadow-[oklch(0.7_0.1_200)]/50"
    [
      "--tw-inset-shadow-color:color-mix(in srgb,oklch(.7 .1 200) \
       50%,transparent)";
      "--tw-inset-shadow-color:color-mix(in oklab,color-mix(in oklab,oklch(.7 \
       .1 200) 50%,transparent) var(--tw-inset-shadow-alpha),transparent)";
    ]

(* A hex bracket colour whose modifier reads a custom property keeps that
   property inside the guarded [color-mix]. The hex arm folded the modifier into
   an alpha byte, which a var() has no value for, so the modifier was dropped
   and the shadow painted fully opaque.

   Read unminified: the CLI keeps the [#f00] the class wrote, and minified
   printing folds a colour to its shortest hex, so an expanded [#ff0000] reads
   the same as [#f00] there. *)
let test_bracket_hex_opacity_var () =
  (* the plain fallback has no percentage to hold, so it keeps the hex *)
  Test_helpers.check_declarations ~minify:false "shadow-[#f00]/[var(--x)]"
    [
      "--tw-shadow-color: #f00";
      "--tw-shadow-color: color-mix(in oklab, color-mix(in oklab, #f00 \
       var(--x), transparent) var(--tw-shadow-alpha), transparent)";
    ];
  Test_helpers.check_declarations ~minify:false "inset-shadow-[#f00]/[var(--x)]"
    [
      "--tw-inset-shadow-color: #f00";
      "--tw-inset-shadow-color: color-mix(in oklab, color-mix(in oklab, #f00 \
       var(--x), transparent) var(--tw-inset-shadow-alpha), transparent)";
    ]

(* A palette colour, [currentcolor] or a [color:var()] whose modifier reads a
   custom property mixes that property into the guarded value, the way the
   bracket hex above does. The shared channel builders folded every modifier to
   a percentage, which a var() has none of, so the guarded mix said [100%] and
   the modifier was dropped. The plain fallback keeps the colour whole: a var()
   gives it no percentage to carry. *)
let test_palette_colour_opacity_var () =
  let mixed colour alpha =
    "color-mix(in oklab, color-mix(in oklab, " ^ colour
    ^ " var(--o), transparent) var(" ^ alpha ^ "), transparent)"
  in
  Test_helpers.check_declarations ~minify:false "shadow-red-500/(--o)"
    [
      "--tw-shadow-color: #fb2c36";
      "--tw-shadow-color: " ^ mixed "var(--color-red-500)" "--tw-shadow-alpha";
    ];
  Test_helpers.check_declarations ~minify:false "inset-shadow-red-500/(--o)"
    [
      "--tw-inset-shadow-color: #fb2c36";
      "--tw-inset-shadow-color: "
      ^ mixed "var(--color-red-500)" "--tw-inset-shadow-alpha";
    ];
  Test_helpers.check_declarations ~minify:false "shadow-current/[var(--o)]"
    [
      "--tw-shadow-color: currentColor";
      "--tw-shadow-color: " ^ mixed "currentcolor" "--tw-shadow-alpha";
    ];
  Test_helpers.check_declarations ~minify:false "shadow-[color:var(--c)]/(--o)"
    [
      "--tw-shadow-color: var(--c)";
      "--tw-shadow-color: " ^ mixed "var(--c)" "--tw-shadow-alpha";
    ]

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

(* An [--alpha()] standing among a bracket shadow's lengths is the colour it
   denotes, as it is anywhere in an arbitrary value: Tailwind writes the
   [color-mix()] in its place and the shadow reads on. The bracket read the call
   only as a whole value, so a shadow spelling one was refused. *)
let test_bracket_shadow_alpha_fn () =
  let composition =
    "box-shadow: var(--tw-inset-shadow), var(--tw-inset-ring-shadow), \
     var(--tw-ring-offset-shadow), var(--tw-ring-shadow), var(--tw-shadow)"
  in
  Test_helpers.check_declarations ~minify:false
    "shadow-[0_0_0_1px_--alpha(red/50%)]"
    [
      "--tw-shadow: 0 0 0 1px var(--tw-shadow-color, color-mix(in oklab, red \
       50%, transparent))";
      composition;
    ];
  Test_helpers.check_declarations ~minify:false
    "inset-shadow-[0_0_0_1px_--alpha(red/0.5)]"
    [
      "--tw-inset-shadow: inset 0 0 0 1px var(--tw-inset-shadow-color, \
       color-mix(in oklab, red 50%, transparent))";
      composition;
    ]

(* Tailwind's colour-mix polyfill applies to a bracket shadow the way it applies
   to any declaration: a layer whose colour is a [color-mix()] reading a custom
   property or [currentcolor] is written with the mix's first colour behind the
   family's channel in the open, and as written behind the colour-mix guard,
   with the composition after both. A mix the author spelled out reads the same
   as one an [--alpha()] expands to. tw wrote the mix alone. *)
let test_bracket_shadow_mix_polyfill () =
  let composition =
    "box-shadow: var(--tw-inset-shadow), var(--tw-inset-ring-shadow), \
     var(--tw-ring-offset-shadow), var(--tw-ring-shadow), var(--tw-shadow)"
  in
  let pair cls channel ~lengths ~colour ~mixed =
    Test_helpers.check_declarations ~minify:false cls
      [
        channel ^ ": " ^ lengths ^ " var(" ^ channel ^ "-color, " ^ colour ^ ")";
        channel ^ ": " ^ lengths ^ " var(" ^ channel ^ "-color, " ^ mixed ^ ")";
        composition;
      ]
  in
  let mix colour alpha =
    "color-mix(in oklab, " ^ colour ^ " " ^ alpha ^ ", transparent)"
  in
  pair "shadow-[0_0_0_1px_--alpha(red/var(--o))]" "--tw-shadow"
    ~lengths:"0 0 0 1px" ~colour:"red" ~mixed:(mix "red" "var(--o)");
  pair "shadow-[0_0_0_1px_--alpha(currentcolor/50%)]" "--tw-shadow"
    ~lengths:"0 0 0 1px" ~colour:"currentcolor"
    ~mixed:(mix "currentcolor" "50%");
  pair "inset-shadow-[0_0_0_1px_--alpha(red/var(--o))]" "--tw-inset-shadow"
    ~lengths:"inset 0 0 0 1px" ~colour:"red" ~mixed:(mix "red" "var(--o)");
  pair "inset-shadow-[0_0_0_1px_color-mix(in_oklab,red_var(--o),transparent)]"
    "--tw-inset-shadow" ~lengths:"inset 0 0 0 1px" ~colour:"red"
    ~mixed:(mix "red" "var(--o)");
  (* every layer keeps its place, and one that needs no polyfill is written as
     it is on both sides *)
  Test_helpers.check_declarations ~minify:false
    "shadow-[0_0_0_1px_--alpha(red/var(--o)),0_0_2px_blue]"
    [
      "--tw-shadow: 0 0 0 1px var(--tw-shadow-color, red), 0 0 2px \
       var(--tw-shadow-color, blue)";
      "--tw-shadow: 0 0 0 1px var(--tw-shadow-color, " ^ mix "red" "var(--o)"
      ^ "), 0 0 2px var(--tw-shadow-color, blue)";
      composition;
    ]

(* A named [--opacity-*] token is read off the theme before the bracket, or the
   modifier stays glued to it: [shadow-[color:var(--c)]/half] was a shadow
   spelled [[color:var(--c)]/half], a class no markup carries, where Tailwind
   mixes [var(--opacity-half)] into the colour. A shadow size under the token is
   the size alone: Tailwind reads no alpha off a named modifier there and writes
   the shadow as if none were given. *)
let test_bracket_named_opacity () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("opacity-half", "50%") ]
  in
  let mixed family cls fallback colour =
    Test_helpers.check_declarations ~theme ~minify:false cls
      [
        "--tw-" ^ family ^ "-color: " ^ fallback;
        "--tw-" ^ family ^ "-color: color-mix(in oklab, color-mix(in oklab, "
        ^ colour ^ " var(--opacity-half), transparent) var(--tw-" ^ family
        ^ "-alpha), transparent)";
      ]
  in
  mixed "shadow" "shadow-[color:var(--c)]/half" "var(--c)" "var(--c)";
  mixed "shadow" "shadow-[#123456]/half"
    "color-mix(in srgb, #123456 50%, transparent)" "#123456";
  mixed "inset-shadow" "inset-shadow-[color:var(--c)]/half" "var(--c)"
    "var(--c)";
  let composition =
    "box-shadow: var(--tw-inset-shadow), var(--tw-inset-ring-shadow), \
     var(--tw-ring-offset-shadow), var(--tw-ring-shadow), var(--tw-shadow)"
  in
  Test_helpers.check_declarations ~theme ~minify:false "shadow-[var(--c)]/half"
    [ "--tw-shadow: var(--c)"; composition ];
  Test_helpers.check_declarations ~theme ~minify:false
    "shadow-[0_0_1px_red]/half"
    [ "--tw-shadow: 0 0 1px var(--tw-shadow-color, red)"; composition ]

(* A bracket alpha modifier with no [%] sign (shadow-lg/[25]) tracks the
   modifier's own written text in --tw-shadow-alpha, the way Tailwind does,
   rather than scaling it into a percentage: the alpha the shadow paints with
   comes from a separate, correctly-scaled computation, so --tw-shadow-alpha
   here is a plain, unconverted echo of what the class wrote. *)
let test_shadow_bracket_alpha_tracking () =
  (* The [lacks] checks that used to sit beside each list here - "no
     --tw-shadow-alpha:2500%" - said nothing the list does not already say, and
     passed on a sheet with no rule in it. *)
  let shadow_lg alpha painted =
    [
      "--tw-shadow-alpha:" ^ alpha;
      "--tw-shadow:0 10px 15px -3px var(--tw-shadow-color," ^ painted
      ^ "),0 4px 6px -4px var(--tw-shadow-color," ^ painted ^ ")";
      composes_box_shadow;
    ]
  in
  Test_helpers.check_declarations "shadow-lg/[25]"
    (shadow_lg "25" "oklab(0%0 0/25)");
  Test_helpers.check_declarations "shadow-[0_1px_2px_#000]/[25]"
    [
      "--tw-shadow-alpha:25";
      "--tw-shadow:0 1px 2px var(--tw-shadow-color,oklab(0%0 0/25))";
      composes_box_shadow;
    ];
  Test_helpers.check_declarations "inset-shadow-sm/[25]"
    [
      "--tw-inset-shadow-alpha:25";
      "--tw-inset-shadow:inset 0 2px 4px var(--tw-inset-shadow-color,oklab(0%0 \
       0/25))";
      composes_box_shadow;
    ];
  (* A bracket alpha that does carry a [%] sign, or the plain percent form, both
     keep behaving as a percentage. *)
  Test_helpers.check_declarations "shadow-lg/[25%]"
    (shadow_lg "25%" "oklab(0%0 0/.25)");
  Test_helpers.check_declarations "shadow-lg/50"
    (shadow_lg "50%" "oklab(0%0 0/.5)")

(* Tailwind forwards a declaration-safe arbitrary shadow token stream even when
   it is not a valid shadow value. *)
let test_arbitrary_shadow_token_stream () =
  let accepted cls =
    match Tw.of_string cls with
    | Ok _ -> ()
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  accepted "shadow-[<value>]";
  accepted "inset-shadow-[<value>]";
  accepted "shadow-[0_1px_2px_#000]";
  accepted "inset-shadow-[0_1px_2px_#000]"

(* A [#] bracket only names a shadow or ring colour when what follows is a hex
   spelling. The bracket-colour reader handed everything after the [#] to the
   raising constructor from inside [of_class], so a malformed hex escaped the
   parser as an exception instead of failing the match. *)
let test_arbitrary_bracket_color_token_stream () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  List.iter
    (fun prefix ->
      ignore (css (prefix ^ "-[#zz]"));
      ignore (css (prefix ^ "-[#]"));
      ignore (css (prefix ^ "-[#12345]"));
      ignore (css (prefix ^ "-[#zz]/50")))
    [ "shadow"; "ring"; "inset-shadow"; "inset-ring"; "ring-offset" ];
  (* The colour of an arbitrary shadow is read the same way. *)
  ignore (css "shadow-[0_1px_2px_#zz]");
  ignore (css "shadow-[0_1px_2px_#12345]");
  ignore (css "shadow-[0_1px_2px_#zz]/50");
  ignore (css "inset-shadow-[0_1px_2px_#zz]");
  Test_helpers.check_declarations "shadow-[#abc]"
    [
      "--tw-shadow-color:#abc";
      "--tw-shadow-color:color-mix(in oklab,#abc \
       var(--tw-shadow-alpha),transparent)";
    ];
  Test_helpers.check_declarations "ring-[#123456]" [ "--tw-ring-color:#123456" ];
  Test_helpers.check_declarations "inset-shadow-[#abc]"
    [
      "--tw-inset-shadow-color:#abc";
      "--tw-inset-shadow-color:color-mix(in oklab,#abc \
       var(--tw-inset-shadow-alpha),transparent)";
    ];
  Test_helpers.check_declarations "inset-ring-[#abc]"
    [ "--tw-inset-ring-color:#abc" ];
  Test_helpers.check_declarations "ring-offset-[#abc]"
    [ "--tw-ring-offset-color:#abc" ];
  Test_helpers.check_declarations "shadow-[0_1px_2px_#000]"
    [ "--tw-shadow:0 1px 2px var(--tw-shadow-color,#000)"; composes_box_shadow ];
  Test_helpers.check_declarations "inset-shadow-[0_1px_2px_#000]"
    [
      "--tw-inset-shadow:inset 0 1px 2px var(--tw-inset-shadow-color,#000)";
      composes_box_shadow;
    ]

(* A shade the palette does not define is not a colour. These utilities read the
   shade without checking it, so the class was accepted and then rendered a
   fabricated black or a reference to a variable no theme declares. *)
let test_undefined_shade () =
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
  rejected "shadow-red-999";
  rejected "inset-shadow-red-999";
  rejected "ring-red-999";
  rejected "ring-offset-red-999";
  rejected "inset-ring-red-999";
  rejected "shadow-red-0";
  rejected "shadow-red-550";
  rejected "ring-red-42";
  rejected "shadow-red-999/50";
  accepted "shadow-red-500";
  accepted "inset-shadow-red-500";
  accepted "ring-red-950";
  accepted "ring-offset-red-50";
  accepted "inset-ring-red-500";
  accepted "shadow-red-500/50"

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

(* [opacity-[<n>]] names its class after the bracket, so the number has to come
   back out spelled as the author wrote it rather than re-printed. *)
let test_arbitrary_opacity_spelling () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
      | Ok u -> Alcotest.(check string) (cls ^ " round-trips") cls (Tw.pp u))
    [ "opacity-[0.5]"; "opacity-[0.50]"; "opacity-[.5]"; "opacity-[1]" ]

(* An empty bracket names no value, and the pinned CLI emits nothing for it. *)
let test_arbitrary_opacity_rejects_empty () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok u -> Alcotest.failf "%s parsed as %s" cls (Tw.pp u)
      | Error (`Msg _) -> ())
    [ "opacity-[]" ]

(* The bracket is a token stream Tailwind hands to the declaration unvalidated.
   It goes through the arbitrary-value pipeline, not OCaml's number reader, so
   [calc()] reaches the property and a spelling only OCaml reads as a number
   ([0x4], [1_0]) is emitted as written rather than folded to [4] and [10]. A
   word is not a number and is passed through the same way. *)
let test_arbitrary_opacity_token_stream () =
  Test_helpers.check_declarations "opacity-[calc(1+2)]"
    [ "opacity:calc(1 + 2)" ];
  Test_helpers.check_declarations "opacity-[0x4]" [ "opacity:0x4" ];
  Test_helpers.check_declarations "opacity-[1_0]" [ "opacity:1 0" ];
  Test_helpers.check_declarations "opacity-[abc]" [ "opacity:abc" ]

(* A [--shadow-*] or [--inset-shadow-*] token the project declared in its
   [@theme] names a shadow the built-in scale has no slot for. Tailwind
   generates the utility from each, routing the colour through the family's
   shadow-colour channel; tw rejected both outright. *)
let test_project_shadow_tokens () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [
        ("shadow-halo", "0 0 8px #f00");
        ("inset-shadow-dent", "inset 0 1px 2px #000");
      ]
  in
  let emits decl cls =
    Test_helpers.check_declarations ~theme ~minify:false cls
      [
        decl;
        "box-shadow: var(--tw-inset-shadow), var(--tw-inset-ring-shadow), \
         var(--tw-ring-offset-shadow), var(--tw-ring-shadow), var(--tw-shadow)";
      ]
  in
  emits "--tw-shadow: 0 0 8px var(--tw-shadow-color, #f00)" "shadow-halo";
  emits "--tw-inset-shadow: inset 0 1px 2px var(--tw-inset-shadow-color, #000)"
    "inset-shadow-dent";
  Alcotest.(check bool)
    "an undeclared shadow name is rejected" true
    (Result.is_error (Tw.of_string ~theme "shadow-nope"))

(* A shadow's parts are separated by the [_] that stands for a space, so a
   variable name carrying an underscore of its own is written [\_]. *)
let test_shadow_underscore_escape () =
  Test_helpers.check_declarations {|shadow-[0_0_0_1px_var(--a\_b)]|}
    [
      "--tw-shadow:0 0 0 1px var(--tw-shadow-color,var(--a_b))";
      composes_box_shadow;
    ]

(* A data-type hint says how to read the value written after it; it does not
   make that value the name of a custom property. [shadow-[shadow:...]] wrote
   [--tw-shadow: var(--0_0_0_1px_red)] and [ring-[length:3px]] read [3px] as a
   variable name. *)
let test_bracket_data_type_hint_reads_the_value () =
  Test_helpers.check_declarations "shadow-[shadow:0_0_0_1px_red]"
    [ "--tw-shadow:0 0 0 1px var(--tw-shadow-color,red)"; composes_box_shadow ];
  Test_helpers.check_declarations "ring-[length:3px]"
    [
      "--tw-ring-shadow:var(--tw-ring-inset,) 0 0 0 calc(3px + \
       var(--tw-ring-offset-width)) var(--tw-ring-color,currentcolor)";
      composes_box_shadow;
    ];
  (* a var() reference after the hint still names a custom property *)
  Test_helpers.check_declarations "shadow-[shadow:var(--value)]"
    [ "--tw-shadow:var(--value)"; composes_box_shadow ];
  (* the class prints back with the hint the author wrote *)
  Alcotest.(check string)
    "ring-[length:3px] round-trips" "ring-[length:3px]"
    (Tw.pp (Result.get_ok (Tw.of_string "ring-[length:3px]")));
  (* A payload the shadow reader refuses is held open, not settled: Tailwind
     writes the bracket out whatever it says, so refusing is an intermediate. *)
  Test_helpers.check_invalid_input
    ~why:
      (Test_helpers.Diverges
         "emitted verbatim; tw needs an opaque declaration to match")
    (module Tw.Effects.Handler)
    "shadow-[shadow:notashadow]"

(* A project shadow reads a trailing [var()] as its colour, the way Tailwind
   does: it takes the lengths and what is left is the colour. The shadow grammar
   read it as the next length slot, so [--shadow-card: 0 1px 2px
   var(--card-shadow)] painted in [currentcolor] with [--card-shadow] as the
   spread. Under a modifier the reference is a value only the browser can
   resolve: the authored colour stays in the open and the relative form goes
   behind the guard. *)
let test_project_shadow_trailing_var_colour () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [
        ("shadow-card", "0 1px 2px var(--card-shadow)");
        ("inset-shadow-lip", "inset 0 1px var(--lip)");
      ]
  in
  Test_helpers.check_declarations ~theme "shadow-card"
    [
      "--tw-shadow:0 1px 2px var(--tw-shadow-color,var(--card-shadow))";
      composes_box_shadow;
    ];
  Test_helpers.check_declarations ~theme "shadow-card/50"
    [
      "--tw-shadow-alpha:50%";
      "--tw-shadow:0 1px 2px var(--tw-shadow-color,var(--card-shadow))";
      "--tw-shadow:0 1px 2px var(--tw-shadow-color,oklab(from \
       var(--card-shadow) l a b/.5))";
      composes_box_shadow;
    ];
  Test_helpers.check_declarations ~theme "inset-shadow-lip/50"
    [
      "--tw-inset-shadow-alpha:50%";
      "--tw-inset-shadow:inset 0 1px var(--tw-inset-shadow-color,var(--lip))";
      "--tw-inset-shadow:inset 0 1px var(--tw-inset-shadow-color,oklab(from \
       var(--lip) l a b/.5))";
      composes_box_shadow;
    ]

(* A shadow list under a modifier, as Tailwind writes it. A [var()] colour, or
   an alpha read from a custom property, asks for the relative colour guard, and
   every layer then keeps its authored colour in the open - a literal colour
   beside the [var()] is left as written, not folded. A [currentcolor] layer
   takes the alpha through a [color-mix()] behind its own guard, which nests
   inside the relative one when both are needed. A list of literal colours with
   a literal alpha folds outright, and the scale's own shadows go the same way,
   [shadow-inner] included, which had no modifier at all. *)
let test_shadow_list_alpha_guards () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("shadow-stack", "0 1px 2px, 0 2px var(--c)") ]
  in
  let sheet cls =
    match Tw.of_string ~theme cls with
    | Ok u ->
        Tw.to_css ~theme ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool)
      (cls ^ " has " ^ affix)
      true
      (Astring.String.is_infix ~affix (sheet cls))
  in
  has "shadow-stack/50"
    {|.shadow-stack\/50{--tw-shadow-alpha:50%;--tw-shadow:0 1px 2px var(--tw-shadow-color,currentcolor),0 2px var(--tw-shadow-color,var(--c))}@supports(color:lab(from red l a b)){.shadow-stack\/50{--tw-shadow:0 1px 2px var(--tw-shadow-color,currentcolor),0 2px var(--tw-shadow-color,oklab(from var(--c) l a b/.5))}@supports(color:color-mix(in lab,red,red)){.shadow-stack\/50{--tw-shadow:0 1px 2px var(--tw-shadow-color,color-mix(in oklab,currentcolor 50%,transparent)),0 2px var(--tw-shadow-color,oklab(from var(--c) l a b/.5))}}}|};
  Test_helpers.check_declarations "shadow-[0_1px_2px_red,0_2px_var(--c)]/50"
    [
      "--tw-shadow-alpha:50%";
      "--tw-shadow:0 1px 2px var(--tw-shadow-color,red),0 2px \
       var(--tw-shadow-color,var(--c))";
      "--tw-shadow:0 1px 2px var(--tw-shadow-color,oklab(62.79553606%.22486306 \
       .1258463/.5)),0 2px var(--tw-shadow-color,oklab(from var(--c) l a \
       b/.5))";
      composes_box_shadow;
    ];
  Test_helpers.check_declarations "shadow-lg/[var(--o)]"
    [
      "--tw-shadow-alpha:var(--o)";
      "--tw-shadow:0 10px 15px -3px var(--tw-shadow-color,#0000001a),0 4px 6px \
       -4px var(--tw-shadow-color,#0000001a)";
      "--tw-shadow:0 10px 15px -3px var(--tw-shadow-color,oklab(from #0000001a \
       l a b/var(--o))),0 4px 6px -4px var(--tw-shadow-color,oklab(from \
       #0000001a l a b/var(--o)))";
      composes_box_shadow;
    ];
  Test_helpers.check_declarations "shadow-inner/50"
    [
      "--tw-shadow-alpha:50%";
      "--tw-shadow:inset 0 2px 4px 0 var(--tw-shadow-color,oklab(0%0 0/.5))";
      composes_box_shadow;
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

(* A shadow read whole from a custom property takes a modifier: there is no
   colour in the value to fold the alpha into, so Tailwind sets the alpha
   channel and leaves the value as it is. The class was refused, and under the
   [shadow:] hint the modifier was dropped from the name and the value. *)
let test_var_shadow_takes_a_modifier () =
  Test_helpers.check_declarations "shadow-[var(--s)]/50"
    [ "--tw-shadow-alpha:50%"; "--tw-shadow:var(--s)"; composes_box_shadow ];
  Test_helpers.check_declarations "inset-shadow-[var(--s)]/50"
    [
      "--tw-inset-shadow-alpha:50%";
      "--tw-inset-shadow:inset var(--s)";
      composes_box_shadow;
    ];
  Test_helpers.check_declarations "shadow-[shadow:var(--s)]/50"
    [ "--tw-shadow-alpha:50%"; "--tw-shadow:var(--s)"; composes_box_shadow ];
  Test_helpers.check_declarations "shadow-[shadow:0_1px_red]/50"
    [
      "--tw-shadow-alpha:50%";
      "--tw-shadow:0 1px var(--tw-shadow-color,oklab(62.79553606%.22486306 \
       .1258463/.5))";
      composes_box_shadow;
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
    test_case "token-stream ring colour opacity from a var" `Quick
      test_raw_ring_colour_opacity_var;
    test_case "token-stream ring colour registers nothing" `Quick
      test_raw_ring_colour_registers_nothing;
    test_case "bracket data-type hint reads the value" `Quick
      test_bracket_data_type_hint_reads_the_value;
    test_case "shadow underscore escape" `Quick test_shadow_underscore_escape;
    test_case "arbitrary bracket color token stream" `Quick
      test_arbitrary_bracket_color_token_stream;
    test_case "project shadow tokens" `Quick test_project_shadow_tokens;
    test_case "ring theme colour" `Quick test_ring_theme_colour;
    test_case "project shadow opacity" `Quick test_project_shadow_opacity;
    test_case "project shadow trailing var colour" `Quick
      test_project_shadow_trailing_var_colour;
    test_case "shadow list alpha guards" `Quick test_shadow_list_alpha_guards;
    test_case "ring bracket width reads its variables" `Quick
      test_ring_bracket_width_reads_its_variables;
    test_case "var shadow takes a modifier" `Quick
      test_var_shadow_takes_a_modifier;
    test_case "inset shadow bracket refuses inset" `Quick
      test_inset_shadow_bracket_refuses_inset;
    test_case "shadow bracket alpha tracking" `Quick
      test_shadow_bracket_alpha_tracking;
    test_case "undefined colour shade" `Quick test_undefined_shade;
    test_case "shadeless shadow colors" `Quick test_shadeless_shadow_colors;
    test_case "palette shadow colors keep OKLCH" `Quick
      test_palette_shadow_colors_keep_oklch;
    test_case "shadow-inner" `Quick test_shadow_inner;
    test_case "arbitrary shadow list" `Quick test_arbitrary_shadow_list;
    test_case "arbitrary shadow lengths" `Quick test_arbitrary_shadow_lengths;
    test_case "arbitrary inset shadow named colour" `Quick
      test_arbitrary_inset_shadow_named_colour;
    test_case "arbitrary shadow named colour opacity" `Quick
      test_arbitrary_shadow_named_colour_opacity;
    test_case "arbitrary shadow named colour var opacity" `Quick
      test_arbitrary_shadow_named_colour_var_opacity;
    test_case "arbitrary shadow colour opacity" `Quick
      test_arbitrary_shadow_colour_opacity;
    test_case "bracket colour opacity without a hex" `Quick
      test_bracket_colour_opacity_without_hex;
    test_case "bracket hex opacity from a var" `Quick
      test_bracket_hex_opacity_var;
    test_case "palette colour opacity from a var" `Quick
      test_palette_colour_opacity_var;
    test_case "ring colour opacity from a var" `Quick
      test_ring_colour_opacity_var;
    test_case "ring bracket hex named opacity" `Quick
      test_ring_bracket_hex_named_opacity;
    test_case "bracket shadow --alpha()" `Quick test_bracket_shadow_alpha_fn;
    test_case "bracket shadow colour-mix polyfill" `Quick
      test_bracket_shadow_mix_polyfill;
    test_case "bracket shadow named opacity" `Quick test_bracket_named_opacity;
    test_case "shadow bracket alpha tracking" `Quick
      test_shadow_bracket_alpha_tracking;
    test_case "arbitrary shadow token stream" `Quick
      test_arbitrary_shadow_token_stream;
    test_case "shadow-2xl default alpha" `Quick test_shadow_2xl_alpha;
    test_case "shadow-2xs/xs small sizes" `Quick test_shadow_small_sizes;
    test_case "inset-shadow roundtrip" `Quick test_inset_shadow_roundtrip;
    test_case "inset-shadow invalid (bare/md/2xl)" `Quick
      test_inset_shadow_invalid;
    test_case "inset-shadow default scale (v4.3.1)" `Quick
      test_inset_shadow_default_scale;
    test_case "inset-shadow @theme override threads through" `Quick
      test_inset_shadow_theme_override;
    test_case "inset-shadow @theme override reads the whole grammar" `Quick
      test_inset_shadow_theme_override_grammar;
    test_case "effects of_string - valid values" `Quick of_string_valid;
    test_case "effects of_string - invalid values" `Quick of_string_invalid;
    test_case "ring of_string - valid values" `Quick test_ring_of_string_valid;
    test_case "ring width order" `Slow test_ring_width_order;
    test_case "ring-inset @property family" `Quick
      test_ring_inset_property_rules;
    test_case "ring shadeless color opacity" `Quick test_ring_shadeless_color;
    test_case "ring offset shadeless color" `Quick
      test_ring_offset_shadeless_color;
    test_case "filters css generation" `Quick test_filters_css_generation;
    test_case "effects suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "arbitrary opacity spelling" `Quick
      test_arbitrary_opacity_spelling;
    test_case "arbitrary opacity rejects empty" `Quick
      test_arbitrary_opacity_rejects_empty;
    test_case "arbitrary opacity token stream" `Quick
      test_arbitrary_opacity_token_stream;
    test_case "effects render like Tailwind" `Slow rendering_matches_tailwind;
  ]

let suite = ("effects", tests)
