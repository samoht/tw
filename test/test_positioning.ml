open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Positioning.of_string parts with
  | Ok t -> check string "positioning class" expected (Tw.Core.pp t)
  | Error (`Msg msg) -> fail msg

let test_inset_and_z () =
  check [ "inset"; "0" ];
  check [ "z"; "10" ]

let test_negative () = check [ "-"; "top"; "4" ]

let tests =
  [
    test_case "inset and z" `Quick test_inset_and_z;
    test_case "negative top" `Quick test_negative;
  ]

let suite = ("positioning", tests)
