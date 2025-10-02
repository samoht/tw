open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Flex.Handler.of_string parts with
  | Ok result ->
      let style = Tw.Flex.Handler.to_style result in
      Alcotest.check string "flex class name" expected (Tw.Style.pp style)
  | Error (`Msg msg) -> fail msg

let of_string_valid () =
  (* Display *)
  check [ "flex" ];
  check [ "inline"; "flex" ];

  (* Direction *)
  check [ "flex"; "row" ];
  check [ "flex"; "row"; "reverse" ];
  check [ "flex"; "col" ];
  check [ "flex"; "col"; "reverse" ];

  (* Wrap *)
  check [ "flex"; "wrap" ];
  check [ "flex"; "wrap"; "reverse" ];
  check [ "flex"; "nowrap" ];

  (* Flex shortcuts *)
  check [ "flex"; "1" ];
  check [ "flex"; "auto" ];
  check [ "flex"; "initial" ];
  check [ "flex"; "none" ];

  (* Grow/Shrink *)
  check [ "flex"; "grow" ];
  check [ "flex"; "grow"; "0" ];
  check [ "flex"; "shrink" ];
  check [ "flex"; "shrink"; "0" ];

  (* Basis *)
  check [ "basis"; "0" ];
  check [ "basis"; "1" ];
  check [ "basis"; "auto" ];
  check [ "basis"; "full" ];

  (* Order *)
  check [ "order"; "1" ];
  check [ "order"; "2" ];
  check [ "order"; "3" ];
  check [ "order"; "4" ];
  check [ "order"; "5" ];
  check [ "order"; "6" ];
  check [ "order"; "first" ];
  check [ "order"; "last" ];
  check [ "order"; "none" ]

let of_string_invalid () =
  let fail_maybe input =
    match Tw.Flex.Handler.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "flex"; "invalid" ];
  fail_maybe [ "basis" ];
  fail_maybe [ "order" ];
  fail_maybe [ "order"; "0" ];
  fail_maybe [ "order"; "7" ];
  fail_maybe []

let tests =
  [
    test_case "flex of_string - valid values" `Quick of_string_valid;
    test_case "flex of_string - invalid values" `Quick of_string_invalid;
  ]

let suite = ("flex", tests)
