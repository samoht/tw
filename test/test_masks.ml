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

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [ Alcotest.test_case "typed constructors" `Quick test_typed ]

let suite = ("masks", tests)
