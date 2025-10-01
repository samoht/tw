open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Filters.of_string parts with
  | Ok u -> check string "filters class" expected (Tw.Style.pp (Tw.Filters.to_style u))
  | Error (`Msg msg) -> fail msg

let test_blur () =
  check [ "blur"; "sm" ];
  check [ "blur"; "2xl" ]

let test_backdrop () =
  check [ "backdrop"; "opacity"; "50" ];
  check [ "backdrop"; "invert" ]

let tests =
  [
    test_case "blur" `Quick test_blur; test_case "backdrop" `Quick test_backdrop;
  ]

let suite = ("filters", tests)
