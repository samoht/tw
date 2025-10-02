open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Transforms.Handler.of_string parts with
  | Ok t ->
      check string "transforms class" expected
        (Tw.Style.pp (Tw.Transforms.Handler.to_style t))
  | Error (`Msg msg) -> fail msg

let test_translate_rotate () =
  check [ "translate"; "x"; "4" ];
  check [ "rotate"; "90" ]

let test_of_string_invalid () =
  (* Invalid transform utilities *)
  let test_invalid input =
    match Tw.Transforms.Handler.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  (* Invalid translate - missing value *)
  test_invalid [ "translate"; "x" ];
  test_invalid [ "translate"; "y" ];
  test_invalid [ "translate"; "z" ];

  (* Invalid rotate - missing value *)
  test_invalid [ "rotate" ];
  test_invalid [ "rotate"; "x" ];
  test_invalid [ "rotate"; "y" ];
  test_invalid [ "rotate"; "z" ];

  (* Invalid scale - missing value *)
  test_invalid [ "scale" ];
  test_invalid [ "scale"; "x" ];
  test_invalid [ "scale"; "y" ];
  test_invalid [ "scale"; "z" ];

  (* Invalid skew - missing value *)
  test_invalid [ "skew"; "x" ];
  test_invalid [ "skew"; "y" ];

  (* Invalid perspective - missing value *)
  test_invalid [ "perspective" ];

  (* Invalid perspective origin *)
  test_invalid [ "perspective"; "origin" ];
  test_invalid [ "perspective"; "origin"; "invalid" ];

  (* Invalid transform style *)
  test_invalid [ "transform"; "style" ];
  test_invalid [ "transform"; "style"; "invalid" ];

  (* Invalid prefixes *)
  test_invalid [ "translate" ];
  (* Missing axis *)
  test_invalid [ "scale"; "invalid"; "100" ];
  (* Invalid axis *)
  test_invalid []
(* Empty *)

let tests =
  [
    test_case "translate+rotate" `Quick test_translate_rotate;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
  ]

let suite = ("transforms", tests)
