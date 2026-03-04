open Alcotest

let check class_name =
  match Tw.Filters.Handler.of_class class_name with
  | Ok u ->
      check string "filters class" class_name (Tw.Filters.Handler.to_class u)
  | Error (`Msg msg) -> fail msg

let test_blur () =
  check "blur-sm";
  check "blur-2xl"

let test_backdrop () =
  check "backdrop-opacity-50";
  check "backdrop-invert"

let suborder_matches_tailwind () =
  let open Tw in
  let shuffled =
    Test_helpers.shuffle
      [ blur_sm; blur; blur_2xl; backdrop_blur; backdrop_opacity 50 ]
  in

  Test_helpers.check_ordering_matches
    ~test_name:"filters suborder matches Tailwind" shuffled

let tests =
  [
    test_case "blur" `Quick test_blur;
    test_case "backdrop" `Quick test_backdrop;
    test_case "filters suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
  ]

let suite = ("filters", tests)
