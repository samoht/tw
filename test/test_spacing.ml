open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Spacing.of_string parts with
  | Ok result ->
      Alcotest.check string "spacing class name" expected (Tw.Core.pp result)
  | Error (`Msg msg) -> fail msg

let of_string_valid () =
  check [ "p"; "0" ];
  check [ "p"; "1" ];
  check [ "p"; "4" ];
  check [ "p"; "px" ];
  check [ "p"; "0.5" ];
  check [ "p"; "1.5" ];

  check [ "m"; "0" ];
  check [ "m"; "1" ];
  check [ "m"; "auto" ];
  check [ "-m"; "1" ];
  check [ "-m"; "4" ];

  check [ "px"; "4" ];
  check [ "py"; "2" ];
  check [ "mx"; "auto" ];
  check [ "my"; "8" ];

  check [ "pt"; "2" ];
  check [ "pr"; "4" ];
  check [ "pb"; "6" ];
  check [ "pl"; "8" ];
  check [ "mt"; "auto" ];
  check [ "mr"; "0" ];

  check [ "gap"; "2" ];
  check [ "gap"; "x"; "4" ];
  check [ "gap"; "y"; "6" ];

  check [ "space"; "x"; "2" ];
  check [ "space"; "y"; "4" ];
  check [ "-space"; "x"; "1" ];
  check [ "-space"; "y"; "2" ]

let of_string_invalid () =
  (* Invalid spacing values *)
  let fail_maybe input =
    match Tw.Spacing.of_string input with
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
  fail_maybe [ "py"; "auto" ];
  (* Auto not valid for py *)
  fail_maybe [ "gap"; "auto" ];
  (* Auto not valid for gap *)
  fail_maybe [ "space" ];
  (* Missing axis *)
  fail_maybe [ "space"; "z"; "2" ]
(* Invalid axis *)

let tests =
  [
    test_case "spacing of_string - valid values" `Quick of_string_valid;
    test_case "spacing of_string - invalid values" `Quick of_string_invalid;
  ]

let suite = ("spacing", tests)
