open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Sizing.of_string parts with
  | Ok result ->
      Alcotest.check string "sizing class name" expected (Tw.Core.pp result)
  | Error (`Msg msg) -> fail msg

let test_widths () =
  check [ "w"; "0" ];
  check [ "w"; "1" ];
  check [ "w"; "4" ];
  check [ "w"; "px" ];
  check [ "w"; "0.5" ];
  check [ "w"; "1/2" ];
  check [ "w"; "1/3" ];
  check [ "w"; "2/3" ];
  check [ "w"; "1/4" ];
  check [ "w"; "3/4" ];
  check [ "w"; "full" ];
  check [ "w"; "screen" ];
  check [ "w"; "min" ];
  check [ "w"; "max" ];
  check [ "w"; "fit" ];
  check [ "w"; "auto" ]

let test_heights () =
  check [ "h"; "0" ];
  check [ "h"; "4" ];
  check [ "h"; "full" ];
  check [ "h"; "screen" ];
  check [ "h"; "1/2" ]

let test_min_sizes () =
  check [ "min"; "w"; "0" ];
  check [ "min"; "w"; "full" ];
  check [ "min"; "h"; "0" ];
  check [ "min"; "h"; "screen" ]

let test_max_sizes () =
  check [ "max"; "w"; "none" ];
  check [ "max"; "w"; "xs" ];
  check [ "max"; "w"; "sm" ];
  check [ "max"; "w"; "md" ];
  check [ "max"; "w"; "lg" ];
  check [ "max"; "w"; "xl" ];
  check [ "max"; "w"; "2xl" ];
  check [ "max"; "w"; "7xl" ];
  check [ "max"; "w"; "full" ];
  check [ "max"; "w"; "screen"; "sm" ];
  check [ "max"; "h"; "4" ];
  check [ "max"; "h"; "full" ];
  check [ "max"; "h"; "screen" ]

let test_square_sizes () =
  check [ "size"; "0" ];
  check [ "size"; "4" ];
  check [ "size"; "full" ];
  check [ "size"; "1/2" ]

let test_sizing_of_string_invalid () =
  (* Invalid sizing values *)
  let test_invalid input =
    match Tw.Sizing.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  test_invalid [ "w" ];
  (* Missing value *)
  test_invalid [ "w"; "invalid" ];
  (* Invalid value *)
  test_invalid [ "w"; "1/5" ];
  (* Invalid fraction *)
  test_invalid [ "h"; "1/7" ];
  (* Invalid fraction *)
  test_invalid [ "min" ];
  (* Missing dimension *)
  test_invalid [ "min"; "z"; "4" ];
  (* Invalid dimension *)
  test_invalid [ "max"; "w"; "8xl" ];
  (* Invalid max size *)
  test_invalid [ "max"; "w"; "screen"; "3xl" ];
  (* Invalid screen size *)
  test_invalid [ "unknown"; "4" ]
(* Unknown sizing type *)

let tests =
  [
    test_case "widths" `Quick test_widths;
    test_case "heights" `Quick test_heights;
    test_case "min sizes" `Quick test_min_sizes;
    test_case "max sizes" `Quick test_max_sizes;
    test_case "square sizes" `Quick test_square_sizes;
    test_case "sizing of_string - invalid values" `Quick
      test_sizing_of_string_invalid;
  ]

let suite = ("sizing", tests)
