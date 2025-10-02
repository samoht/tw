open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Interactivity.Handler.of_string parts with
  | Ok u ->
      check string "interactivity class" expected
        (Tw.Style.pp (Tw.Interactivity.Handler.to_style u))
  | Error (`Msg msg) -> fail msg

let test_select () = check [ "select"; "none" ]

let test_scroll_snap () =
  check [ "scroll"; "smooth" ];
  check [ "snap"; "center" ]

let test_of_string_invalid () =
  (* Invalid interactivity utilities *)
  let test_invalid input =
    match Tw.Interactivity.Handler.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  (* Invalid select values *)
  test_invalid [ "select" ];
  (* Missing value *)
  test_invalid [ "select"; "invalid" ];

  (* Invalid value *)

  (* Invalid scroll values *)
  test_invalid [ "scroll" ];
  (* Missing value *)
  test_invalid [ "scroll"; "invalid" ];

  (* Invalid value *)

  (* Invalid snap values *)
  test_invalid [ "snap" ];
  (* Missing value *)
  test_invalid [ "snap"; "invalid" ];
  (* Invalid value *)
  test_invalid [ "snap"; "align" ];

  (* Incomplete snap-align-none *)

  (* Invalid resize values *)
  test_invalid [ "resize"; "invalid" ];

  (* Invalid value *)

  (* Invalid pointer events *)
  test_invalid [ "pointer"; "events" ];
  (* Missing value *)
  test_invalid [ "pointer"; "events"; "invalid" ];

  (* Invalid value *)

  (* Invalid will-change *)
  test_invalid [ "will"; "change" ];
  (* Missing value *)
  test_invalid [ "will"; "change"; "invalid" ];

  (* Invalid value *)

  (* Invalid prefixes *)
  test_invalid [ "appearance" ];
  (* Incomplete *)
  test_invalid [ "unknown" ];
  (* Unknown *)
  test_invalid []
(* Empty *)

let tests =
  [
    test_case "select" `Quick test_select;
    test_case "scroll+snap" `Quick test_scroll_snap;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
  ]

let suite = ("interactivity", tests)
