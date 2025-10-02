open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Cursor.Handler.of_string parts with
  | Ok result ->
      let style = Tw.Cursor.Handler.to_style result in
      Alcotest.check string "cursor class name" expected (Tw.Style.pp style)
  | Error (`Msg msg) -> fail msg

let of_string_valid () =
  check [ "cursor"; "auto" ];
  check [ "cursor"; "default" ];
  check [ "cursor"; "pointer" ];
  check [ "cursor"; "wait" ];
  check [ "cursor"; "move" ];
  check [ "cursor"; "not"; "allowed" ];
  check [ "cursor"; "text" ];
  check [ "cursor"; "crosshair" ];
  check [ "cursor"; "help" ];
  check [ "cursor"; "grab" ];
  check [ "cursor"; "grabbing" ]

let of_string_invalid () =
  let fail_maybe input =
    match Tw.Cursor.Handler.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "cursor" ];
  fail_maybe [ "cursor"; "invalid" ];
  fail_maybe []

let tests =
  [
    test_case "cursor of_string - valid values" `Quick of_string_valid;
    test_case "cursor of_string - invalid values" `Quick of_string_invalid;
  ]

let suite = ("cursor", tests)
