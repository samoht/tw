let check = Test_helpers.check_handler_roundtrip (module Tw.Zoom.Handler)

let test_roundtrip () =
  check "zoom-50";
  check "zoom-100";
  check "zoom-[var(--zoom)]"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Zoom.Handler) "zoom";
  Test_helpers.check_invalid_input (module Tw.Zoom.Handler) "zoom-1.5";
  Test_helpers.check_invalid_input (module Tw.Zoom.Handler) "zoom-unknown";
  (* A zoom percentage is written in plain decimal. *)
  Test_helpers.check_invalid_input (module Tw.Zoom.Handler) "zoom-0x4";
  Test_helpers.check_invalid_input (module Tw.Zoom.Handler) "zoom-04";
  Test_helpers.check_invalid_input (module Tw.Zoom.Handler) "zoom-1_0"

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

(* The paren shorthand is Tailwind's first zoom candidate, before the numeric
   scale and bracket-arbitrary values. *)
let test_zoom_candidate_boundary () =
  Test_helpers.check_class_order ~test_name:"zoom candidate boundary"
    [ "zoom-125"; "zoom-(--preview-zoom)"; "zoom-[1.1]"; "zoom-75"; "zoom-100" ]

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "sorts after the transforms" `Quick
        test_zoom_sorts_after_the_transforms;
      Alcotest.test_case "candidate boundary" `Quick
        test_zoom_candidate_boundary;
    ]

let suite = ("zoom", tests)
