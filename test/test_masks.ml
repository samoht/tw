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
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
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
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
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

(* An arbitrary value the property cannot take is not a utility: these used to
   fall back to [auto] / [center] and emit a plausible-looking declaration. *)
let test_invalid_bracket_value () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "mask-size-[<value>]";
  rejected "mask-position-[<value>]";
  rejected "mask-[position:nope]"

(* A mask-position bracket takes the whole CSS grammar, the same as
   background-position: a single edge keyword and the edge/offset form. Both
   used to fall through the hand-rolled parser to a silent [center]. *)
let test_bracket_position_grammar () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "mask-[position:top]" "mask-position:top";
  has "mask-position-[top]" "mask-position:top";
  has "mask-[top]" "mask-position:top";
  (* the lengths and layer-list forms are unchanged *)
  has "mask-[position:10px_20px]" "mask-position:10px 20px";
  has "mask-position-[30%_50%,70%_50%]" "mask-position:30% 50%,70% 50%"

(* Masks sit between the backgrounds and fill/stroke, and the mask-gradient
   utilities lead them. Sharing padding's slot interleaved the two families with
   the padding rules, which breaks the adjacency the block merging relies on. *)
let order_matches_tailwind () =
  let classes =
    [
      "bg-red-500";
      "mask-x-from-20%";
      "mask-b-from-50%";
      "mask-none";
      "mask-top";
      "mask-cover";
      "mask-repeat-x";
      "fill-blue-500";
      "stroke-green-500";
      "p-4";
      "pt-2";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  Test_helpers.check_ordering_matches ~test_name:"mask order matches Tailwind"
    (Test_helpers.shuffle utilities)

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "typed constructors" `Quick test_typed;
      Alcotest.test_case "arbitrary mask image" `Quick test_bracket_image;
      Alcotest.test_case "arbitrary mask layer list" `Quick
        test_bracket_layer_list;
      Alcotest.test_case "invalid bracket value" `Quick
        test_invalid_bracket_value;
      Alcotest.test_case "bracket position grammar" `Quick
        test_bracket_position_grammar;
      Alcotest.test_case "order matches Tailwind" `Slow order_matches_tailwind;
    ]

let suite = ("masks", tests)
