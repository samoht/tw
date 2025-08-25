open Alcotest

let test_borders_of_string_valid () =
  (* Valid border values *)
  let test_valid input expected =
    match Tw.Borders.of_string input with
    | Ok result -> check string "border class name" expected (Tw.Core.pp result)
    | Error (`Msg msg) -> fail msg
  in

  test_valid [ "border" ] "border";
  test_valid [ "border"; "0" ] "border-0";
  test_valid [ "border"; "2" ] "border-2";
  test_valid [ "border"; "4" ] "border-4";
  test_valid [ "border"; "8" ] "border-8";

  test_valid [ "border"; "t" ] "border-t";
  test_valid [ "border"; "r" ] "border-r";
  test_valid [ "border"; "b" ] "border-b";
  test_valid [ "border"; "l" ] "border-l";
  test_valid [ "border"; "x" ] "border-x";
  test_valid [ "border"; "y" ] "border-y";

  test_valid [ "border"; "t"; "2" ] "border-t-2";
  test_valid [ "border"; "r"; "4" ] "border-r-4";

  test_valid [ "border"; "solid" ] "border-solid";
  test_valid [ "border"; "dashed" ] "border-dashed";
  test_valid [ "border"; "dotted" ] "border-dotted";
  test_valid [ "border"; "double" ] "border-double";
  test_valid [ "border"; "none" ] "border-none";

  test_valid [ "rounded" ] "rounded";
  test_valid [ "rounded"; "none" ] "rounded-none";
  test_valid [ "rounded"; "sm" ] "rounded-sm";
  test_valid [ "rounded"; "md" ] "rounded-md";
  test_valid [ "rounded"; "lg" ] "rounded-lg";
  test_valid [ "rounded"; "xl" ] "rounded-xl";
  test_valid [ "rounded"; "2xl" ] "rounded-2xl";
  test_valid [ "rounded"; "3xl" ] "rounded-3xl";
  test_valid [ "rounded"; "full" ] "rounded-full";

  test_valid [ "rounded"; "t" ] "rounded-t";
  test_valid [ "rounded"; "r" ] "rounded-r";
  test_valid [ "rounded"; "b" ] "rounded-b";
  test_valid [ "rounded"; "l" ] "rounded-l";

  test_valid [ "rounded"; "tl" ] "rounded-tl";
  test_valid [ "rounded"; "tr" ] "rounded-tr";
  test_valid [ "rounded"; "br" ] "rounded-br";
  test_valid [ "rounded"; "bl" ] "rounded-bl";

  test_valid [ "rounded"; "t"; "lg" ] "rounded-t-lg";
  test_valid [ "rounded"; "tl"; "2xl" ] "rounded-tl-2xl"

let test_borders_of_string_invalid () =
  (* Invalid border values *)
  let test_invalid input =
    match Tw.Borders.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  test_invalid [ "border"; "3" ];
  (* Invalid width *)
  test_invalid [ "border"; "invalid" ];
  (* Invalid style *)
  test_invalid [ "border"; "z" ];
  (* Invalid side *)
  test_invalid [ "rounded"; "4xl" ];
  (* Invalid size *)
  test_invalid [ "rounded"; "z" ];
  (* Invalid corner *)
  test_invalid [ "unknown" ]
(* Unknown border type *)

let tests =
  [
    test_case "borders of_string - valid values" `Quick
      test_borders_of_string_valid;
    test_case "borders of_string - invalid values" `Quick
      test_borders_of_string_invalid;
  ]
