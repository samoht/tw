open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Positioning.of_string parts with
  | Ok util ->
      let t = Tw.Positioning.to_style util in
      check string "positioning class" expected (Tw.Style.pp t)
  | Error (`Msg msg) -> fail msg

let test_inset_and_z () =
  check [ "inset"; "0" ];
  check [ "z"; "10" ]

let test_negative () = check [ "-"; "top"; "4" ]

let test_position_utilities () =
  check [ "static" ];
  check [ "fixed" ];
  check [ "absolute" ];
  check [ "relative" ];
  check [ "sticky" ]

let tests =
  [
    test_case "inset and z" `Quick test_inset_and_z;
    test_case "negative top" `Quick test_negative;
    test_case "position utilities" `Quick test_position_utilities;
  ]

let suite = ("positioning", tests)
