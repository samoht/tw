open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Padding.Handler.of_string parts with
  | Ok result ->
      let style = Tw.Padding.Handler.to_style result in
      Alcotest.check string "padding class name" expected (Tw.Style.pp style)
  | Error (`Msg msg) -> fail msg

let of_string_valid () =
  check [ "p"; "0" ];
  check [ "p"; "1" ];
  check [ "p"; "4" ];
  check [ "p"; "px" ];
  check [ "p"; "0.5" ];
  check [ "p"; "1.5" ];

  check [ "px"; "4" ];
  check [ "px"; "0" ];
  check [ "px"; "8" ];
  check [ "py"; "2" ];
  check [ "py"; "6" ];

  check [ "pt"; "2" ];
  check [ "pr"; "4" ];
  check [ "pb"; "6" ];
  check [ "pl"; "8" ]

let of_string_invalid () =
  let fail_maybe input =
    match Tw.Padding.Handler.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "p" ];
  (* Missing value *)
  fail_maybe [ "p"; "invalid" ];
  (* Invalid value *)
  fail_maybe [ "p"; "-1" ];
  (* Negative not allowed for padding *)
  fail_maybe [ "px"; "auto" ];
  (* Auto not valid for px *)
  fail_maybe [ "py"; "auto" ]
(* Auto not valid for py *)

let tests =
  [
    test_case "padding of_string - valid values" `Quick of_string_valid;
    test_case "padding of_string - invalid values" `Quick of_string_invalid;
  ]

let suite = ("padding", tests)
