let check = Test_helpers.check_handler_roundtrip (module Tw.Masks.Handler)

let test_roundtrip () =
  check "mask-none";
  check "mask-add";
  check "mask-exclude";
  check "mask-intersect";
  check "mask-subtract";
  check "mask-alpha";
  check "mask-luminance";
  check "mask-match";
  check "mask-type-alpha";
  check "mask-type-luminance";
  check "mask-auto";
  check "mask-clip-border";
  check "mask-clip-padding";
  check "mask-clip-content";
  check "mask-clip-fill";
  check "mask-clip-stroke";
  check "mask-clip-view";
  check "mask-no-clip";
  check "mask-origin-border";
  check "mask-origin-padding";
  check "mask-origin-content";
  check "mask-origin-fill";
  check "mask-origin-stroke";
  check "mask-origin-view"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Masks.Handler) "mask-foo";
  Test_helpers.check_invalid_input (module Tw.Masks.Handler) "mask"

(* The typed mask constructors are newly exposed in tw.mli; check a sample
   across the families agrees with the parser on class names. *)
let test_typed () =
  Test_helpers.check_typed_class "mask-clip-border" Tw.mask_clip_border;
  Test_helpers.check_typed_class "mask-no-clip" Tw.mask_no_clip;
  Test_helpers.check_typed_class "mask-add" Tw.mask_add;
  Test_helpers.check_typed_class "mask-none" Tw.mask_none;
  Test_helpers.check_typed_class "mask-alpha" Tw.mask_alpha;
  Test_helpers.check_typed_class "mask-origin-border" Tw.mask_origin_border;
  Test_helpers.check_typed_class "mask-center" Tw.mask_center;
  Test_helpers.check_typed_class "mask-top-right" Tw.mask_top_right;
  Test_helpers.check_typed_class "mask-repeat" Tw.mask_repeat;
  Test_helpers.check_typed_class "mask-cover" Tw.mask_cover;
  Test_helpers.check_typed_class "mask-type-alpha" Tw.mask_type_alpha

(* [mask-[<image>]] takes any background-image, not only a linear-gradient. *)
let test_bracket_image () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.pp ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "radial-gradient reaches mask-image" true
    (Astring.String.is_infix ~affix:"mask-image:radial-gradient(white,black)"
       (css "mask-[radial-gradient(white,black)]"));
  check "mask-[radial-gradient(white,black)]";
  check "mask-[conic-gradient(white,black)]";
  check "mask-[linear-gradient(white,black)]"

(* A mask takes one image and one position per layer, comma-separated. The
   single-[url(...)] reading used to swallow the comma, and a position list was
   rejected outright. *)
let test_bracket_layer_list () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.pp ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "two url layers stay two" true
    (Astring.String.is_infix ~affix:"mask-image:url(/a.png),url(/b.png)"
       (css "mask-[url(/a.png),url(/b.png)]"));
  Alcotest.(check bool)
    "two positions stay two" true
    (Astring.String.is_infix ~affix:"mask-position:30% 50%,70% 50%"
       (css "mask-position-[30%_50%,70%_50%]"))

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "typed constructors" `Quick test_typed;
      Alcotest.test_case "arbitrary mask image" `Quick test_bracket_image;
      Alcotest.test_case "arbitrary mask layer list" `Quick
        test_bracket_layer_list;
    ]

let suite = ("masks", tests)
