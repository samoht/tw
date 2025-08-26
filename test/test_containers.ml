open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Containers.of_string parts with
  | Ok t -> check string "containers class" expected (Tw.Core.pp t)
  | Error (`Msg msg) -> fail msg

let test_container_types () =
  check [ "container"; "type"; "size" ];
  check [ "container"; "type"; "normal" ]

let test_container_name () = check [ "container"; "sidebar" ]

let tests =
  [
    test_case "types" `Quick test_container_types;
    test_case "name" `Quick test_container_name;
  ]

let suite = ("containers", tests)
