let check = Test_helpers.check_handler_roundtrip (module Tw.Scroll.Handler)

let test_roundtrip () =
  check "scroll-m-0";
  check "scroll-m-4";
  check "scroll-mx-2";
  check "scroll-my-4";
  check "scroll-mt-8";
  check "scroll-mr-2";
  check "scroll-mb-4";
  check "scroll-ml-6";
  check "scroll-p-0";
  check "scroll-p-4";
  check "scroll-px-2";
  check "scroll-py-4";
  check "scroll-pt-8";
  check "scroll-pr-2";
  check "scroll-pb-4";
  check "scroll-pl-6"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Scroll.Handler) "scroll";
  Test_helpers.check_invalid_input (module Tw.Scroll.Handler) "scroll-foo"

(* Typed scroll-margin/padding constructors (newly exposed in tw.mli) take a
   [float] argument; check they agree with the parser on class names. *)
let test_typed () =
  Test_helpers.check_typed_class "scroll-m-4" (Tw.scroll_m 4.);
  Test_helpers.check_typed_class "scroll-mx-2" (Tw.scroll_mx 2.);
  Test_helpers.check_typed_class "scroll-mt-8" (Tw.scroll_mt 8.);
  Test_helpers.check_typed_class "scroll-me-6" (Tw.scroll_me 6.);
  Test_helpers.check_typed_class "scroll-p-4" (Tw.scroll_p 4.);
  Test_helpers.check_typed_class "scroll-py-2" (Tw.scroll_py 2.);
  Test_helpers.check_typed_class "scroll-pt-8" (Tw.scroll_pt 8.);
  Test_helpers.check_typed_class "scroll-ps-6" (Tw.scroll_ps 6.)

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [ Alcotest.test_case "typed constructors" `Quick test_typed ]

let suite = ("scroll", tests)
