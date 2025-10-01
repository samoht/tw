open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Containers.of_string parts with
  | Ok t ->
      let style = Tw.Containers.to_style t in
      check string "containers class" expected (Tw.Style.pp style)
  | Error (`Msg msg) -> fail msg

let test_container_types () =
  check [ "container"; "type"; "size" ];
  check [ "container"; "type"; "normal" ]

let test_container_name () = check [ "container"; "sidebar" ]

let test_of_string_invalid () =
  (* Invalid container utilities *)
  let test_invalid input =
    match Tw.Containers.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  (* Invalid type values - these don't match the patterns *)
  test_invalid [ "container"; "type"; "invalid" ];
  (* Invalid type value *)
  test_invalid [ "container"; "type"; "block" ];
  (* Invalid type *)
  test_invalid [ "container"; "type"; "inline" ];

  (* Incomplete inline-size *)

  (* Invalid container formats *)
  test_invalid [ "container" ];
  (* Missing qualifier *)
  test_invalid [ "container"; "type"; "size"; "extra" ];
  (* Extra tokens *)
  test_invalid [ "not"; "container" ];
  (* Wrong prefix *)
  test_invalid [];
  (* Empty input *)
  test_invalid [ "type"; "size" ]
(* Missing container prefix *)

let tests =
  [
    test_case "types" `Quick test_container_types;
    test_case "name" `Quick test_container_name;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
  ]

let suite = ("containers", tests)
