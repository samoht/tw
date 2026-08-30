let check = Test_helpers.check_handler_roundtrip (module Tw.Zoom.Handler)

let test_roundtrip () =
  check "zoom-50";
  check "zoom-100";
  check "zoom-[var(--zoom)]"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Zoom.Handler) "zoom";
  Test_helpers.check_invalid_input (module Tw.Zoom.Handler) "zoom-1.5";
  Test_helpers.check_invalid_input (module Tw.Zoom.Handler) "zoom-unknown"

(* Tailwind emits [zoom-*] between the transforms and the animations. It sorted
   with the margins instead, at the priority a family gets when nobody has
   placed it, and no canonical comparison could see that: [zoom] shares a
   property with none of them. *)
let test_zoom_sorts_after_the_transforms () =
  Test_helpers.check_class_order ~test_name:"zoom band"
    [
      "m-4";
      "translate-x-4";
      "rotate-45";
      "transform-gpu";
      "zoom-50";
      "animate-spin";
      "cursor-pointer";
      "p-4";
    ]

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "sorts after the transforms" `Quick
        test_zoom_sorts_after_the_transforms;
    ]

let suite = ("zoom", tests)
