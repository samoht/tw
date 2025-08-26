open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Forms.of_string parts with
  | Ok t -> check string "forms class" expected (Tw.Core.pp t)
  | Error (`Msg msg) -> fail msg

let test_inputs () =
  check [ "form"; "input" ];
  check [ "form"; "checkbox" ]

let tests = [ test_case "inputs" `Quick test_inputs ]
let suite = ("forms", tests)
