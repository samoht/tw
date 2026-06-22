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
  check "ring-inset"

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

let tests =
  [
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
    test_case "filters css generation" `Quick test_filters_css_generation;
    test_case "effects suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
  ]

let suite = ("effects", tests)
