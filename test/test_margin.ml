open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Margin.Handler.of_string parts with
  | Ok result ->
      let style = Tw.Margin.Handler.to_style result in
      Alcotest.check string "margin class name" expected (Tw.Style.pp style)
  | Error (`Msg msg) -> fail msg

let of_string_valid () =
  check [ "m"; "0" ];
  check [ "m"; "1" ];
  check [ "m"; "4" ];
  check [ "m"; "px" ];
  check [ "m"; "0.5" ];
  check [ "m"; "1.5" ];
  check [ "m"; "auto" ];
  check [ "-m"; "1" ];
  check [ "-m"; "4" ];

  check [ "mx"; "auto" ];
  check [ "mx"; "0" ];
  check [ "mx"; "4" ];
  check [ "-mx"; "2" ];
  check [ "-mx"; "4" ];
  check [ "my"; "8" ];
  check [ "my"; "2" ];
  check [ "-my"; "2" ];
  check [ "-my"; "8" ];

  check [ "mt"; "auto" ];
  check [ "mt"; "0" ];
  check [ "mt"; "2" ];
  check [ "mr"; "0" ];
  check [ "mr"; "4" ];
  check [ "mb"; "6" ];
  check [ "ml"; "8" ];
  check [ "-mt"; "2" ];
  check [ "-mr"; "4" ];
  check [ "-mb"; "6" ];
  check [ "-ml"; "8" ]

let of_string_invalid () =
  let fail_maybe input =
    match Tw.Margin.Handler.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "m" ];
  (* Missing value *)
  fail_maybe [ "m"; "invalid" ]
(* Invalid value *)

let tests =
  [
    test_case "margin of_string - valid values" `Quick of_string_valid;
    test_case "margin of_string - invalid values" `Quick of_string_invalid;
  ]

let suite = ("margin", tests)
