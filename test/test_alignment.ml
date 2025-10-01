open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Alignment.of_string parts with
  | Ok result ->
      let style = Tw.Alignment.to_style result in
      Alcotest.check string "alignment class name" expected (Tw.Style.pp style)
  | Error (`Msg msg) -> fail msg

let of_string_valid () =
  (* Justify content *)
  check [ "justify"; "start" ];
  check [ "justify"; "end" ];
  check [ "justify"; "center" ];
  check [ "justify"; "between" ];
  check [ "justify"; "around" ];
  check [ "justify"; "evenly" ];

  (* Align items *)
  check [ "items"; "start" ];
  check [ "items"; "end" ];
  check [ "items"; "center" ];
  check [ "items"; "baseline" ];
  check [ "items"; "stretch" ];

  (* Align content *)
  check [ "content"; "start" ];
  check [ "content"; "end" ];
  check [ "content"; "center" ];
  check [ "content"; "between" ];
  check [ "content"; "around" ];
  check [ "content"; "evenly" ];
  check [ "content"; "stretch" ];

  (* Align self *)
  check [ "self"; "auto" ];
  check [ "self"; "start" ];
  check [ "self"; "end" ];
  check [ "self"; "center" ];
  check [ "self"; "baseline" ];
  check [ "self"; "stretch" ];

  (* Justify items *)
  check [ "justify"; "items"; "start" ];
  check [ "justify"; "items"; "end" ];
  check [ "justify"; "items"; "center" ];
  check [ "justify"; "items"; "stretch" ];

  (* Justify self *)
  check [ "justify"; "self"; "auto" ];
  check [ "justify"; "self"; "start" ];
  check [ "justify"; "self"; "end" ];
  check [ "justify"; "self"; "center" ];
  check [ "justify"; "self"; "stretch" ];

  (* Place content *)
  check [ "place"; "content"; "start" ];
  check [ "place"; "content"; "end" ];
  check [ "place"; "content"; "center" ];
  check [ "place"; "content"; "between" ];
  check [ "place"; "content"; "around" ];
  check [ "place"; "content"; "evenly" ];
  check [ "place"; "content"; "stretch" ];

  (* Place items *)
  check [ "place"; "items"; "start" ];
  check [ "place"; "items"; "end" ];
  check [ "place"; "items"; "center" ];
  check [ "place"; "items"; "stretch" ];

  (* Place self *)
  check [ "place"; "self"; "auto" ];
  check [ "place"; "self"; "start" ];
  check [ "place"; "self"; "end" ];
  check [ "place"; "self"; "center" ];
  check [ "place"; "self"; "stretch" ]

let of_string_invalid () =
  let fail_maybe input =
    match Tw.Alignment.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "justify" ];
  fail_maybe [ "justify"; "invalid" ];
  fail_maybe [ "items" ];
  fail_maybe [ "content" ];
  fail_maybe [ "self" ];
  fail_maybe [ "place" ];
  fail_maybe [ "place"; "content" ];
  fail_maybe []

let tests =
  [
    test_case "alignment of_string - valid values" `Quick of_string_valid;
    test_case "alignment of_string - invalid values" `Quick of_string_invalid;
  ]

let suite = ("alignment", tests)
