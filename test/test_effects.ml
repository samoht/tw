open Alcotest

let test_effects_of_string_valid () =
  (* Valid effects values *)
  let test_valid input expected =
    match Tw.Effects.of_string input with
    | Ok result ->
        check string "effects class name" expected (Tw.Core.pp result)
    | Error (`Msg msg) -> fail msg
  in

  (* Box shadow *)
  test_valid [ "shadow" ] "shadow";
  test_valid [ "shadow"; "sm" ] "shadow-sm";
  test_valid [ "shadow"; "md" ] "shadow-md";
  test_valid [ "shadow"; "lg" ] "shadow-lg";
  test_valid [ "shadow"; "xl" ] "shadow-xl";
  test_valid [ "shadow"; "2xl" ] "shadow-2xl";
  test_valid [ "shadow"; "inner" ] "shadow-inner";
  test_valid [ "shadow"; "none" ] "shadow-none";

  (* Opacity *)
  test_valid [ "opacity"; "0" ] "opacity-0";
  test_valid [ "opacity"; "5" ] "opacity-5";
  test_valid [ "opacity"; "10" ] "opacity-10";
  test_valid [ "opacity"; "25" ] "opacity-25";
  test_valid [ "opacity"; "50" ] "opacity-50";
  test_valid [ "opacity"; "75" ] "opacity-75";
  test_valid [ "opacity"; "100" ] "opacity-100";

  (* Mix blend mode *)
  test_valid [ "mix"; "blend"; "normal" ] "mix-blend-normal";
  test_valid [ "mix"; "blend"; "multiply" ] "mix-blend-multiply";
  test_valid [ "mix"; "blend"; "screen" ] "mix-blend-screen";
  test_valid [ "mix"; "blend"; "overlay" ] "mix-blend-overlay"

let test_effects_of_string_invalid () =
  (* Invalid effects values *)
  let test_invalid input =
    match Tw.Effects.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  test_invalid [ "shadow"; "3xl" ];
  (* Invalid shadow size *)
  test_invalid [ "opacity"; "110" ];
  (* Invalid opacity value *)
  test_invalid [ "mix"; "blend"; "invalid" ];
  (* Invalid blend mode *)
  test_invalid [ "unknown" ]
(* Unknown effects type *)

let tests =
  [
    test_case "effects of_string - valid values" `Quick
      test_effects_of_string_valid;
    test_case "effects of_string - invalid values" `Quick
      test_effects_of_string_invalid;
  ]
