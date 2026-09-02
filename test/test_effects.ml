module Css = Cascade.Css
open Alcotest

let check = Test_helpers.check_handler_roundtrip (module Tw.Effects.Handler)

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
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.check bool "ring-black sets ring color" true
    (Astring.String.is_infix ~affix:"--tw-ring-color" (css "ring-black"));
  Alcotest.check bool "ring-white/10 uses color-mix" true
    (Astring.String.is_infix ~affix:"color-mix" (css "ring-white/10"));
  (* Palette colours (blue-500) also apply the /opacity modifier on a var-ref
     theme; the ring family resolves it via oklab like bg/text do. *)
  Alcotest.check bool "ring-blue-500/50 applies opacity" true
    (Astring.String.is_infix ~affix:"color-mix" (css "ring-blue-500/50"));
  Alcotest.check bool "inset-ring-gray-950/10 applies opacity" true
    (Astring.String.is_infix ~affix:"color-mix" (css "inset-ring-gray-950/10"));
  match Tw.of_string "ring-red" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "ring-red (no shade) should be rejected"

let test_filters_css_generation () =
  (* Spot-check a few filter/backdrop utilities *)
  let open Tw in
  let css =
    Build.to_css
      [
        Filters.blur;
        Filters.backdrop_blur_lg;
        Filters.backdrop_brightness 125;
        Filters.backdrop_opacity 50.;
      ]
    |> Css.to_string
  in
  Alcotest.check bool "has filter property" true
    (Astring.String.is_infix ~affix:"filter:" css);
  Alcotest.check bool "has backdrop-filter property" true
    (Astring.String.is_infix ~affix:"backdrop-filter:" css)

(* ring-inset registers the ring/shadow @property family, like the other ring
   utilities; it used to emit only the --tw-ring-inset declaration. *)
let test_ring_inset_property_rules () =
  let open Tw in
  let css =
    match of_string "ring-inset" with
    | Ok u -> to_css ~base:false [ u ] |> Css.to_string
    | Error (`Msg m) -> Alcotest.failf "ring-inset: %s" m
  in
  Alcotest.check bool "ring-inset sets --tw-ring-inset" true
    (Astring.String.is_infix ~affix:"--tw-ring-inset: inset" css);
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
      (* A palette colour would only re-report the known theme-token gap
         (--color-indigo-500 as a hex where Tailwind keeps oklch), which [tw
         --diff] already reports; the keyword colours conflict with the sizes
         just as well. *)
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
  let css = Tw.to_css ~base:false [ Tw.shadow_2xl ] |> Tw.Css.to_string in
  Alcotest.(check bool)
    "shadow-2xl uses #00000040 alpha" true
    (Astring.String.is_infix ~affix:"#00000040" css)

(* The two smallest box-shadow sizes (alpha .05 = #0000000d): 2xs is a single 0
   1px shadow with no blur, xs is 0 1px 2px 0. *)
let test_shadow_small_sizes () =
  let css u = Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true in
  Alcotest.(check bool)
    "shadow-2xs uses #0000000d" true
    (Astring.String.is_infix ~affix:"#0000000d" (css Tw.shadow_2xs));
  Alcotest.(check bool)
    "shadow-xs uses #0000000d" true
    (Astring.String.is_infix ~affix:"#0000000d" (css Tw.shadow_xs));
  Alcotest.(check bool)
    "shadow-xs has a 2px blur" true
    (Astring.String.is_infix ~affix:"1px 2px" (css Tw.shadow_xs))

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
  let css cls =
    Tw.to_css ~base:false [ Result.get_ok (Tw.of_string cls) ]
    |> Tw.Css.to_string ~minify:true
  in
  Alcotest.(check bool)
    "inset-shadow-2xs uses #0000000d" true
    (Astring.String.is_infix ~affix:"#0000000d" (css "inset-shadow-2xs"));
  Alcotest.(check bool)
    "inset-shadow-2xs has no blur ([inset 0 1px])" true
    (Astring.String.is_infix ~affix:"inset 0 1px var(" (css "inset-shadow-2xs"));
  Alcotest.(check bool)
    "inset-shadow-sm is [inset 0 2px 4px]" true
    (Astring.String.is_infix ~affix:"inset 0 2px 4px" (css "inset-shadow-sm"))

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
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "shadow-white" ".shadow-white{--tw-shadow-color:#fff}";
  has "shadow-white/10" ".shadow-white\\/10{--tw-shadow-color:#ffffff1a}";
  has "inset-shadow-white" ".inset-shadow-white{--tw-inset-shadow-color:#fff}";
  has "inset-shadow-white/20"
    ".inset-shadow-white\\/20{--tw-inset-shadow-color:#fff3}"

(* A default palette token is already an OKLCH colour. Tailwind keeps that value
   as the unguarded shadow-colour fallback; converting it to sRGB hex changes
   wide-gamut colours before the [color-mix()] enhancement applies. *)
let test_palette_shadow_colors_keep_oklch () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "shadow-indigo-500" "--tw-shadow-color:oklch(58.5%.233 277.117)";
  has "inset-shadow-indigo-500"
    "--tw-inset-shadow-color:oklch(58.5%.233 277.117)"

(* shadow-inner is a shadow shape like the others: it sets --tw-shadow and
   composes, rather than writing box-shadow directly. *)
let test_shadow_inner () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let out = css "shadow-inner" in
  Alcotest.(check bool)
    "shadow-inner sets --tw-shadow" true
    (Astring.String.is_infix ~affix:"--tw-shadow:inset 0 2px 4px 0 " out);
  Alcotest.(check bool)
    "shadow-inner composes box-shadow" true
    (Astring.String.is_infix
       ~affix:"box-shadow:var(--tw-inset-shadow),var(--tw-inset-ring-shadow)"
       out)

(* A shadow list is one shadow per layer. The single-shadow reading also drops
   the spread, so anything with a comma goes to the value parser. *)
let test_arbitrary_shadow_list () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "both layers survive with their spread" true
    (Astring.String.is_infix
       ~affix:
         "--tw-shadow:-5px 10px 15px -3px \
          var(--tw-shadow-color,var(--shadow-color)),-5px 4px 6px -4px \
          var(--tw-shadow-color,var(--shadow-color))"
       (css
          "shadow-[-5px_10px_15px_-3px_var(--shadow-color),-5px_4px_6px_-4px_var(--shadow-color)]"))

(* A single arbitrary shadow reads every CSS length, not the px/rem/em subset,
   and keeps its spread. A token that is not a length makes the whole value not
   a shadow rather than dropping out and shifting its neighbours along. *)
let test_arbitrary_shadow_lengths () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits affix cls =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  emits "--tw-shadow: 0 1ch 2px var(--tw-shadow-color, oklab(0% 0 0 / .5))"
    "shadow-[0_1ch_2px_#000]/50";
  emits "--tw-shadow: 0 1px 2px 3px var(--tw-shadow-color, oklab(0% 0 0 / .5))"
    "shadow-[0_1px_2px_3px_#000]/50";
  emits
    "--tw-shadow: 0 1ch 2px 3vmin var(--tw-shadow-color, oklab(0% 0 0 / .5))"
    "shadow-[0_1ch_2px_3vmin_#000]/50";
  (* inset-shadow reads its arbitrary value through the same parser, with no
     [Css.parse_shadow] fallback to hide the dropped tokens. *)
  emits
    "--tw-inset-shadow: inset 0 1ch 2px var(--tw-inset-shadow-color, #000000)"
    "inset-shadow-[0_1ch_2px_#000]";
  emits
    "--tw-inset-shadow: inset 0 1px 2px 3px var(--tw-inset-shadow-color, \
     #000000)"
    "inset-shadow-[0_1px_2px_3px_#000]";
  match Tw.of_string "shadow-[0_bogus_2px]" with
  | Ok _ -> Alcotest.fail "expected shadow-[0_bogus_2px] to be rejected"
  | Error _ -> ()

let test_arbitrary_shadow_colour_opacity () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "shadow-[0_0_8px_oklch(50%_0.2_250)]/50"
    "var(--tw-shadow-color,color-mix(";
  has "inset-shadow-[0_0_8px_oklch(50%_0.2_250)]/50"
    "var(--tw-inset-shadow-color,color-mix(";
  has "shadow-[0_0_8px_#f00]/[var(--x)]" "--tw-shadow-alpha:var(--x)";
  has "shadow-[0_0_8px_#f00]/[var(--x)]" "oklab(from";
  has "inset-shadow-[0_0_8px_#f00]/[var(--x)]"
    "--tw-inset-shadow-alpha:var(--x)";
  has "inset-shadow-[0_0_8px_#f00]/[var(--x)]" "oklab(from"

(* A bracket colour with no sRGB hex - [oklch()] and the other wide-gamut
   spellings - has no hex to fold the modifier's alpha into, so the alpha has to
   stay a mix. Tailwind writes the plain fallback in sRGB and the guarded value
   in oklab; the colour used to fall through untouched, which painted the shadow
   fully opaque and dropped the modifier. *)
let test_bracket_colour_opacity_without_hex () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  let lacks cls affix =
    Alcotest.(check bool) cls false (Astring.String.is_infix ~affix (css cls))
  in
  has "shadow-[oklch(0.7_0.1_200)]/50" "--tw-shadow-color:color-mix(in srgb,";
  lacks "shadow-[oklch(0.7_0.1_200)]/50" "--tw-shadow-color:oklch(";
  has "inset-shadow-[oklch(0.7_0.1_200)]/50"
    "--tw-inset-shadow-color:color-mix(in srgb,";
  lacks "inset-shadow-[oklch(0.7_0.1_200)]/50" "--tw-inset-shadow-color:oklch("

(* A hex bracket colour whose modifier reads a custom property keeps that
   property inside the guarded [color-mix]. The hex arm folded the modifier into
   an alpha byte, which a var() has no value for, so the modifier was dropped
   and the shadow painted fully opaque. *)
let test_bracket_hex_opacity_var () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  (* the plain fallback has no percentage to hold, so it keeps the hex *)
  has "shadow-[#f00]/[var(--x)]" "--tw-shadow-color:#f00";
  has "shadow-[#f00]/[var(--x)]"
    "color-mix(in oklab,color-mix(in oklab,#f00 var(--x),transparent) \
     var(--tw-shadow-alpha),transparent)";
  has "inset-shadow-[#f00]/[var(--x)]" "--tw-inset-shadow-color:#f00";
  has "inset-shadow-[#f00]/[var(--x)]"
    "color-mix(in oklab,color-mix(in oklab,#f00 var(--x),transparent) \
     var(--tw-inset-shadow-alpha),transparent)"

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
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  let lacks cls affix =
    Alcotest.(check bool) cls false (Astring.String.is_infix ~affix (css cls))
  in
  has "shadow-lg/[25]" "--tw-shadow-alpha:25;";
  lacks "shadow-lg/[25]" "--tw-shadow-alpha:2500%";
  has "shadow-[0_1px_2px_#000]/[25]" "--tw-shadow-alpha:25;";
  has "inset-shadow-sm/[25]" "--tw-inset-shadow-alpha:25;";
  lacks "inset-shadow-sm/[25]" "--tw-inset-shadow-alpha:2500%";
  (* A bracket alpha that does carry a [%] sign, or the plain percent form, both
     keep behaving as a percentage. *)
  has "shadow-lg/[25%]" "--tw-shadow-alpha:25%";
  has "shadow-lg/50" "--tw-shadow-alpha:50%"

(* An arbitrary shadow that is not a shadow is not a utility: it used to fall
   back to the zero shadow. *)
let test_invalid_arbitrary_shadow () =
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
  rejected "shadow-[<value>]";
  rejected "inset-shadow-[<value>]";
  accepted "shadow-[0_1px_2px_#000]";
  accepted "inset-shadow-[0_1px_2px_#000]"

(* A [#] bracket only names a shadow or ring colour when what follows is a hex
   spelling. The bracket-colour reader handed everything after the [#] to the
   raising constructor from inside [of_class], so a malformed hex escaped the
   parser as an exception instead of failing the match. *)
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
  List.iter
    (fun prefix ->
      rejected (prefix ^ "-[#zz]");
      rejected (prefix ^ "-[#]");
      rejected (prefix ^ "-[#12345]");
      rejected (prefix ^ "-[#zz]/50"))
    [ "shadow"; "ring"; "inset-shadow"; "inset-ring"; "ring-offset" ];
  (* The colour of an arbitrary shadow is read the same way. *)
  rejected "shadow-[0_1px_2px_#zz]";
  rejected "shadow-[0_1px_2px_#12345]";
  rejected "shadow-[0_1px_2px_#zz]/50";
  rejected "inset-shadow-[0_1px_2px_#zz]";
  emits "shadow-[#abc]" "--tw-shadow-color:#abc";
  emits "ring-[#123456]" "--tw-ring-color:#123456";
  emits "inset-shadow-[#abc]" "--tw-inset-shadow-color:#abc";
  emits "inset-ring-[#abc]" "--tw-inset-ring-color:#abc";
  emits "ring-offset-[#abc]" "--tw-ring-offset-color:#abc";
  emits "shadow-[0_1px_2px_#000]"
    "--tw-shadow:0 1px 2px var(--tw-shadow-color,#000)";
  emits "inset-shadow-[0_1px_2px_#000]"
    "--tw-inset-shadow:inset 0 1px 2px var(--tw-inset-shadow-color,#000)"

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

(* The bracket holds a number, so a word is not an opacity. *)
let test_arbitrary_opacity_rejects_non_number () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok u -> Alcotest.failf "%s parsed as %s" cls (Tw.pp u)
      | Error (`Msg _) -> ())
    [ "opacity-[abc]"; "opacity-[]" ]

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

let tests =
  [
    test_case "invalid bracket hex" `Quick test_invalid_bracket_hex;
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
    test_case "invalid arbitrary shadow" `Quick test_invalid_arbitrary_shadow;
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
    test_case "arbitrary opacity rejects non-number" `Quick
      test_arbitrary_opacity_rejects_non_number;
    test_case "effects render like Tailwind" `Slow rendering_matches_tailwind;
  ]

let suite = ("effects", tests)
