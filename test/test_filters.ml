open Alcotest

let check class_name =
  match Tw.Filters.Handler.of_class Tw.Scheme.default class_name with
  | Ok u ->
      check string "filters class" class_name (Tw.Filters.Handler.to_class u)
  | Error (`Msg msg) -> fail msg

let test_blur () =
  check "blur-sm";
  check "blur-2xl"

(* drop-shadow-xs is a v4.3.1 default size (0 1px 1px, alpha .05); it
   round-trips and references the --drop-shadow-xs theme token. *)
let test_drop_shadow_xs () =
  check "drop-shadow-xs";
  let css =
    Tw.to_css [ Result.get_ok (Tw.of_string "drop-shadow-xs") ]
    |> Tw.Css.to_string ~minify:true
  in
  Alcotest.(check bool)
    "references var(--drop-shadow-xs)" true
    (Astring.String.is_infix ~affix:"var(--drop-shadow-xs)" css);
  Alcotest.(check bool)
    "emits --drop-shadow-xs default" true
    (Astring.String.is_infix ~affix:"--drop-shadow-xs:" css)

(* drop-shadow-<color> resolves the colour itself (oklch) for the srgb fallback
   instead of requiring a scheme hex; it used to raise on the default theme
   (which has no hex colours). *)
let test_drop_shadow_color () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "drop-shadow-red-500 sets the drop-shadow color" true
    (Astring.String.is_infix ~affix:"--tw-drop-shadow-color"
       (css "drop-shadow-red-500"));
  Alcotest.(check bool)
    "drop-shadow-red-500/50 uses color-mix" true
    (Astring.String.is_infix ~affix:"color-mix" (css "drop-shadow-red-500/50"))

let test_backdrop () =
  check "backdrop-opacity-50";
  check "backdrop-invert"

let suborder_matches_tailwind () =
  let open Tw in
  let shuffled =
    Test_helpers.shuffle
      [ blur_sm; blur; blur_2xl; backdrop_blur; backdrop_opacity 50. ]
  in

  Test_helpers.check_ordering_matches
    ~test_name:"filters suborder matches Tailwind" shuffled

(* backdrop-blur-N must reference the unified v4 --blur-N token (not the dropped
   --backdrop-blur-N) and emit the shipped --blur-N decl. *)
let test_backdrop_blur_token () =
  let css = Tw.to_css [ Tw.backdrop_blur_sm ] |> Tw.Css.pp ~minify:true in
  Alcotest.(check bool)
    "references var(--blur-sm)" true
    (Astring.String.is_infix ~affix:"var(--blur-sm)" css);
  Alcotest.(check bool)
    "emits --blur-sm:8px" true
    (Astring.String.is_infix ~affix:"--blur-sm:8px" css)

let tests =
  [
    test_case "blur" `Quick test_blur;
    test_case "drop-shadow-xs (v4.3.1 size)" `Quick test_drop_shadow_xs;
    test_case "drop-shadow color (default theme)" `Quick test_drop_shadow_color;
    test_case "backdrop" `Quick test_backdrop;
    test_case "backdrop-blur token" `Quick test_backdrop_blur_token;
    test_case "filters suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
  ]

let suite = ("filters", tests)
