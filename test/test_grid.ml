open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Grid.Handler.of_string parts with
  | Ok result ->
      let style = Tw.Grid.Handler.to_style result in
      Alcotest.check string "grid class name" expected (Tw.Style.pp style)
  | Error (`Msg msg) -> fail msg

let of_string_valid () =
  (* Column placement *)
  check [ "col"; "auto" ];
  check [ "col"; "span"; "1" ];
  check [ "col"; "span"; "2" ];
  check [ "col"; "span"; "3" ];
  check [ "col"; "span"; "6" ];
  check [ "col"; "span"; "12" ];
  check [ "col"; "span"; "full" ];
  check [ "col"; "start"; "1" ];
  check [ "col"; "start"; "2" ];
  check [ "col"; "start"; "auto" ];
  check [ "col"; "end"; "1" ];
  check [ "col"; "end"; "7" ];
  check [ "col"; "end"; "auto" ];

  (* Row placement *)
  check [ "row"; "auto" ];
  check [ "row"; "span"; "1" ];
  check [ "row"; "span"; "2" ];
  check [ "row"; "span"; "3" ];
  check [ "row"; "span"; "6" ];
  check [ "row"; "span"; "12" ];
  check [ "row"; "span"; "full" ];
  check [ "row"; "start"; "1" ];
  check [ "row"; "start"; "2" ];
  check [ "row"; "start"; "auto" ];
  check [ "row"; "end"; "1" ];
  check [ "row"; "end"; "7" ];
  check [ "row"; "end"; "auto" ]

let of_string_invalid () =
  let fail_maybe input =
    match Tw.Grid.Handler.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "col" ];
  (* Missing value *)
  fail_maybe [ "col"; "span" ];
  (* Missing span value *)
  fail_maybe [ "col"; "span"; "0" ];
  (* Out of range (below 1) *)
  fail_maybe [ "col"; "span"; "13" ];
  (* Out of range (above 12) *)
  fail_maybe [ "col"; "span"; "invalid" ];
  (* Invalid value *)
  fail_maybe [ "col"; "start" ];
  (* Missing start value *)
  fail_maybe [ "col"; "end" ];

  (* Missing end value *)
  fail_maybe [ "row" ];
  (* Missing value *)
  fail_maybe [ "row"; "span" ];
  (* Missing span value *)
  fail_maybe [ "row"; "span"; "0" ];
  (* Out of range (below 1) *)
  fail_maybe [ "row"; "span"; "13" ];
  (* Out of range (above 12) *)
  fail_maybe [ "row"; "span"; "invalid" ];
  (* Invalid value *)
  fail_maybe [ "row"; "start" ];
  (* Missing start value *)
  fail_maybe [ "row"; "end" ]
(* Missing end value *)

let tests =
  [
    test_case "grid of_string - valid values" `Quick of_string_valid;
    test_case "grid of_string - invalid values" `Quick of_string_invalid;
  ]

let suite = ("grid", tests)
