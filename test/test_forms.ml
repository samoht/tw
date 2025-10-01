open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Forms.of_string parts with
  | Ok u ->
      check string "forms class" expected (Tw.Style.pp (Tw.Forms.to_style u))
  | Error (`Msg msg) -> fail msg

let test_inputs () =
  check [ "form"; "input" ];
  check [ "form"; "checkbox" ]

let test_of_string_invalid () =
  (* Invalid form utilities *)
  let test_invalid input =
    match Tw.Forms.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  (* Invalid form types *)
  test_invalid [ "form" ];
  (* Missing type *)
  test_invalid [ "form"; "invalid" ];
  (* Invalid type *)
  test_invalid [ "form"; "button" ];

  (* Not supported *)

  (* Invalid formats *)
  test_invalid [ "input" ];
  (* Missing form prefix *)
  test_invalid [ "checkbox" ];
  (* Missing form prefix *)
  test_invalid [ "form"; "input"; "extra" ];
  (* Extra tokens *)
  test_invalid []
(* Empty input *)

let tests =
  [
    test_case "inputs" `Quick test_inputs;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
  ]

let suite = ("forms", tests)
