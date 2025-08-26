open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Animations.of_string parts with
  | Ok t -> check string "animations class" expected (Tw.Core.pp t)
  | Error (`Msg msg) -> fail msg

let test_transitions () =
  check [ "transition"; "none" ];
  check [ "transition"; "opacity" ];
  check [ "transition" ]

let test_animations () =
  check [ "animate"; "none" ];
  check [ "animate"; "spin" ];
  check [ "animate"; "bounce" ]

let test_duration_delay () =
  check [ "duration"; "300" ];
  check [ "delay"; "150" ]

let tests =
  [
    test_case "transitions" `Quick test_transitions;
    test_case "animations" `Quick test_animations;
    test_case "duration + delay" `Quick test_duration_delay;
  ]

let suite = ("animations", tests)
