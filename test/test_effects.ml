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
  let css =
    Tw.to_css ~theme ~base:false
      [ Result.get_ok (Tw.of_string ~theme "inset-shadow-sm") ]
    |> Tw.Css.to_string ~minify:true
  in
  Alcotest.(check bool)
    "inset-shadow-sm @theme override flows to [inset 0 1px 1px]" true
    (Astring.String.is_infix ~affix:"inset 0 1px 1px" css);
  Alcotest.(check bool)
    "inset-shadow-sm @theme override drops the default [inset 0 2px 4px]" false
    (Astring.String.is_infix ~affix:"inset 0 2px 4px" css)

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
  (* inset-shadow reads its arbitrary value through the same parser, with no
     [Css.parse_shadow] fallback to hide the dropped tokens. *)
  inset_shadow "inset-shadow-[0_1ch_2px_#000]"
    "inset 0 1ch 2px var(--tw-inset-shadow-color,#000)";
  inset_shadow "inset-shadow-[0_1px_2px_3px_#000]"
    "inset 0 1px 2px 3px var(--tw-inset-shadow-color,#000)";
  shadow "shadow-[0_bogus_2px]" "0 var(--tw-shadow-color,bogus) 2px";
  with_alpha "shadow-[0_bogus_2px]/50" "50%"
    "0 var(--tw-shadow-color,oklab(from bogus l a b / 50%)) 2px";
  inset_shadow "inset-shadow-[0_bogus_2px]"
    "inset 0 var(--tw-inset-shadow-color,bogus) 2px"

(* [shadow-[<colour>]/<alpha>] where the bracket already carries a [%] alpha:
   the modifier folds into a [color-mix] rather than into a hex byte.

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
      "--tw-shadow:0 0 8px var(--tw-shadow-color,color-mix(in \
       oklab,oklch(50%.2 250) 50%,transparent))";
      composes_box_shadow;
    ];
  Test_helpers.check_declarations "inset-shadow-[0_0_8px_oklch(50%_0.2_250)]/50"
    [
      "--tw-inset-shadow-alpha:50%";
      "--tw-inset-shadow:inset 0 0 8px \
       var(--tw-inset-shadow-color,color-mix(in oklab,oklch(50%.2 250) \
       50%,transparent))";
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

(* A bracket alpha modifier with no [%] sign (shadow-lg/[25]) tracks the
   modifier's own written text in --tw-shadow-alpha, the way Tailwind does,
   rather than scaling it into a percentage: the alpha the shadow paints with
   comes from a separate, correctly-scaled computation, so --tw-shadow-alpha
   here is a plain, unconverted echo of what the class wrote. *)
let test_shadow_bracket_alpha_tracking () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let lacks cls affix =
    Alcotest.(check bool) cls false (Astring.String.is_infix ~affix (css cls))
  in
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
  lacks "shadow-lg/[25]" "--tw-shadow-alpha:2500%";
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
  lacks "inset-shadow-sm/[25]" "--tw-inset-shadow-alpha:2500%";
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
  let css cls =
    match Tw.of_string ~theme cls with
    | Ok u -> Tw.to_css ~theme ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits affix cls =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
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

let tests =
  [
    test_case "bracket data-type hint reads the value" `Quick
      test_bracket_data_type_hint_reads_the_value;
    test_case "shadow underscore escape" `Quick test_shadow_underscore_escape;
    test_case "arbitrary bracket color token stream" `Quick
      test_arbitrary_bracket_color_token_stream;
    test_case "project shadow tokens" `Quick test_project_shadow_tokens;
    test_case "shadow bracket alpha tracking" `Quick
      test_shadow_bracket_alpha_tracking;
    test_case "undefined colour shade" `Quick test_undefined_shade;
    test_case "shadeless shadow colors" `Quick test_shadeless_shadow_colors;
    test_case "palette shadow colors keep OKLCH" `Quick
      test_palette_shadow_colors_keep_oklch;
    test_case "shadow-inner" `Quick test_shadow_inner;
    test_case "arbitrary shadow list" `Quick test_arbitrary_shadow_list;
    test_case "arbitrary shadow lengths" `Quick test_arbitrary_shadow_lengths;
    test_case "arbitrary shadow colour opacity" `Quick
      test_arbitrary_shadow_colour_opacity;
    test_case "bracket colour opacity without a hex" `Quick
      test_bracket_colour_opacity_without_hex;
    test_case "bracket hex opacity from a var" `Quick
      test_bracket_hex_opacity_var;
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
    test_case "effects of_string - valid values" `Quick of_string_valid;
    test_case "effects of_string - invalid values" `Quick of_string_invalid;
    test_case "ring of_string - valid values" `Quick test_ring_of_string_valid;
    test_case "ring width order" `Slow test_ring_width_order;
    test_case "ring-inset @property family" `Quick
      test_ring_inset_property_rules;
    test_case "ring shadeless color opacity" `Quick test_ring_shadeless_color;
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
