open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Filters.Handler.of_string parts with
  | Ok u ->
      check string "filters class" expected
        (Tw.Style.pp (Tw.Filters.Handler.to_style u))
  | Error (`Msg msg) -> fail msg

let test_blur () =
  check [ "blur"; "sm" ];
  check [ "blur"; "2xl" ]

let test_backdrop () =
  check [ "backdrop"; "opacity"; "50" ];
  check [ "backdrop"; "invert" ]

let all_utilities () =
  let open Tw in
  [ blur_sm; blur; blur_2xl; backdrop_blur; backdrop_opacity 50 ]

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"filters suborder matches Tailwind" shuffled

let tests =
  [
    test_case "blur" `Quick test_blur;
    test_case "backdrop" `Quick test_backdrop;
    test_case "filters suborder matches Tailwind" `Slow
      suborder_matches_tailwind;
  ]

let suite = ("filters", tests)
