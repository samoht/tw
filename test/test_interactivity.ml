open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Interactivity.of_string parts with
  | Ok t -> check string "interactivity class" expected (Tw.Core.pp t)
  | Error (`Msg msg) -> fail msg

let test_cursor_select () =
  check [ "cursor"; "pointer" ];
  check [ "select"; "none" ]

let test_scroll_snap () =
  check [ "scroll"; "smooth" ];
  check [ "snap"; "center" ]

let tests =
  [
    test_case "cursor+select" `Quick test_cursor_select;
    test_case "scroll+snap" `Quick test_scroll_snap;
  ]

let suite = ("interactivity", tests)
