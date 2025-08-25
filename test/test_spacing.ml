open Alcotest

let test_spacing_of_string_valid () =
  (* Valid spacing values *)
  let test_valid input expected =
    match Tw.Spacing.of_string input with
    | Ok result ->
        check string "spacing class name" expected (Tw.Core.pp result)
    | Error (`Msg msg) -> fail msg
  in

  test_valid [ "p"; "0" ] "p-0";
  test_valid [ "p"; "1" ] "p-1";
  test_valid [ "p"; "4" ] "p-4";
  test_valid [ "p"; "px" ] "p-px";
  test_valid [ "p"; "0.5" ] "p-0.5";
  test_valid [ "p"; "1.5" ] "p-1.5";

  test_valid [ "m"; "0" ] "m-0";
  test_valid [ "m"; "1" ] "m-1";
  test_valid [ "m"; "auto" ] "m-auto";
  test_valid [ "-m"; "1" ] "-m-1";
  test_valid [ "-m"; "4" ] "-m-4";

  test_valid [ "px"; "4" ] "px-4";
  test_valid [ "py"; "2" ] "py-2";
  test_valid [ "mx"; "auto" ] "mx-auto";
  test_valid [ "my"; "8" ] "my-8";

  test_valid [ "pt"; "2" ] "pt-2";
  test_valid [ "pr"; "4" ] "pr-4";
  test_valid [ "pb"; "6" ] "pb-6";
  test_valid [ "pl"; "8" ] "pl-8";
  test_valid [ "mt"; "auto" ] "mt-auto";
  test_valid [ "mr"; "0" ] "mr-0";

  test_valid [ "gap"; "2" ] "gap-2";
  test_valid [ "gap"; "x"; "4" ] "gap-x-4";
  test_valid [ "gap"; "y"; "6" ] "gap-y-6";

  test_valid [ "space"; "x"; "2" ] "space-x-2";
  test_valid [ "space"; "y"; "4" ] "space-y-4";
  test_valid [ "-space"; "x"; "1" ] "-space-x-1";
  test_valid [ "-space"; "y"; "2" ] "-space-y-2"

let test_spacing_of_string_invalid () =
  (* Invalid spacing values *)
  let test_invalid input =
    match Tw.Spacing.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  test_invalid [ "p" ];
  (* Missing value *)
  test_invalid [ "p"; "invalid" ];
  (* Invalid value *)
  test_invalid [ "p"; "-1" ];
  (* Negative not allowed for padding *)
  test_invalid [ "px"; "auto" ];
  (* Auto not valid for px *)
  test_invalid [ "py"; "auto" ];
  (* Auto not valid for py *)
  test_invalid [ "gap"; "auto" ];
  (* Auto not valid for gap *)
  test_invalid [ "space" ];
  (* Missing axis *)
  test_invalid [ "space"; "z"; "2" ]
(* Invalid axis *)

let tests =
  [
    test_case "spacing of_string - valid values" `Quick
      test_spacing_of_string_valid;
    test_case "spacing of_string - invalid values" `Quick
      test_spacing_of_string_invalid;
  ]
