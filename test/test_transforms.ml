open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Transforms.of_string parts with
  | Ok t -> check string "transforms class" expected (Tw.Core.pp t)
  | Error (`Msg msg) -> fail msg

let test_translate_rotate () =
  check [ "translate"; "x"; "4" ];
  check [ "rotate"; "90" ]

let tests = [ test_case "translate+rotate" `Quick test_translate_rotate ]
let suite = ("transforms", tests)
