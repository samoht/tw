open Alcotest

let test_sizing_of_string_valid () =
  (* Valid sizing values *)
  let test_valid input expected =
    match Tw.Sizing.of_string input with
    | Ok result -> check string "sizing class name" expected (Tw.Core.pp result)
    | Error (`Msg msg) -> fail msg
  in

  test_valid [ "w"; "0" ] "w-0";
  test_valid [ "w"; "1" ] "w-1";
  test_valid [ "w"; "4" ] "w-4";
  test_valid [ "w"; "px" ] "w-px";
  test_valid [ "w"; "0.5" ] "w-0.5";
  test_valid [ "w"; "1/2" ] "w-1/2";
  test_valid [ "w"; "1/3" ] "w-1/3";
  test_valid [ "w"; "2/3" ] "w-2/3";
  test_valid [ "w"; "1/4" ] "w-1/4";
  test_valid [ "w"; "3/4" ] "w-3/4";
  test_valid [ "w"; "full" ] "w-full";
  test_valid [ "w"; "screen" ] "w-screen";
  test_valid [ "w"; "min" ] "w-min";
  test_valid [ "w"; "max" ] "w-max";
  test_valid [ "w"; "fit" ] "w-fit";
  test_valid [ "w"; "auto" ] "w-auto";

  test_valid [ "h"; "0" ] "h-0";
  test_valid [ "h"; "4" ] "h-4";
  test_valid [ "h"; "full" ] "h-full";
  test_valid [ "h"; "screen" ] "h-screen";
  test_valid [ "h"; "1/2" ] "h-1/2";

  test_valid [ "min"; "w"; "0" ] "min-w-0";
  test_valid [ "min"; "w"; "full" ] "min-w-full";
  test_valid [ "min"; "h"; "0" ] "min-h-0";
  test_valid [ "min"; "h"; "screen" ] "min-h-screen";

  test_valid [ "max"; "w"; "none" ] "max-w-none";
  test_valid [ "max"; "w"; "xs" ] "max-w-xs";
  test_valid [ "max"; "w"; "sm" ] "max-w-sm";
  test_valid [ "max"; "w"; "md" ] "max-w-md";
  test_valid [ "max"; "w"; "lg" ] "max-w-lg";
  test_valid [ "max"; "w"; "xl" ] "max-w-xl";
  test_valid [ "max"; "w"; "2xl" ] "max-w-2xl";
  test_valid [ "max"; "w"; "7xl" ] "max-w-7xl";
  test_valid [ "max"; "w"; "full" ] "max-w-full";
  test_valid [ "max"; "w"; "screen"; "sm" ] "max-w-screen-sm";

  test_valid [ "max"; "h"; "4" ] "max-h-4";
  test_valid [ "max"; "h"; "full" ] "max-h-full";
  test_valid [ "max"; "h"; "screen" ] "max-h-screen";

  test_valid [ "size"; "0" ] "size-0";
  test_valid [ "size"; "4" ] "size-4";
  test_valid [ "size"; "full" ] "size-full";
  test_valid [ "size"; "1/2" ] "size-1/2"

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
    test_case "sizing of_string - valid values" `Quick
      test_sizing_of_string_valid;
    test_case "sizing of_string - invalid values" `Quick
      test_sizing_of_string_invalid;
  ]
