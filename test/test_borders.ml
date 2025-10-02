open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Borders.Handler.of_string parts with
  | Ok result ->
      let style = Tw.Borders.Handler.to_style result in
      Alcotest.check string "border class name" expected (Tw.Style.pp style)
  | Error (`Msg msg) -> fail msg

let of_string_valid () =
  check [ "border" ];
  check [ "border"; "0" ];
  check [ "border"; "2" ];
  check [ "border"; "4" ];
  check [ "border"; "8" ];

  check [ "border"; "t" ];
  check [ "border"; "r" ];
  check [ "border"; "b" ];
  check [ "border"; "l" ];
  check [ "border"; "x" ];
  check [ "border"; "y" ];

  check [ "border"; "t"; "2" ];
  check [ "border"; "r"; "4" ];

  check [ "border"; "solid" ];
  check [ "border"; "dashed" ];
  check [ "border"; "dotted" ];
  check [ "border"; "double" ];
  check [ "border"; "none" ];

  check [ "rounded" ];
  check [ "rounded"; "none" ];
  check [ "rounded"; "sm" ];
  check [ "rounded"; "md" ];
  check [ "rounded"; "lg" ];
  check [ "rounded"; "xl" ];
  check [ "rounded"; "2xl" ];
  check [ "rounded"; "3xl" ];
  check [ "rounded"; "full" ];

  check [ "rounded"; "t" ];
  check [ "rounded"; "r" ];
  check [ "rounded"; "b" ];
  check [ "rounded"; "l" ];

  check [ "rounded"; "tl" ];
  check [ "rounded"; "tr" ];
  check [ "rounded"; "br" ];
  check [ "rounded"; "bl" ];

  check [ "rounded"; "t"; "lg" ];
  check [ "rounded"; "tl"; "2xl" ]

let of_string_invalid () =
  (* Invalid border values *)
  let fail_maybe input =
    match Tw.Borders.Handler.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "border"; "3" ];
  (* Invalid width *)
  fail_maybe [ "border"; "invalid" ];
  (* Invalid style *)
  fail_maybe [ "border"; "z" ];
  (* Invalid side *)
  fail_maybe [ "rounded"; "4xl" ];
  (* Invalid size *)
  fail_maybe [ "rounded"; "z" ];
  (* Invalid corner *)
  fail_maybe [ "unknown" ]
(* Unknown border type *)

let tests =
  [
    test_case "borders of_string - valid values" `Quick of_string_valid;
    test_case "borders of_string - invalid values" `Quick of_string_invalid;
  ]

let suite = ("borders", tests)
