open Alcotest

let check class_name =
  match Tw.Containers.Handler.of_class class_name with
  | Ok t ->
      check string "containers class" class_name
        (Tw.Containers.Handler.to_class t)
  | Error (`Msg msg) -> fail msg

let test_container_types () =
  check "container-type-size";
  check "container-type-normal"

let test_container_name () = check "container-sidebar"

let test_of_string_invalid () =
  (* Invalid container utilities *)
  let test_invalid input =
    let class_name = String.concat "-" input in
    match Tw.Containers.Handler.of_class class_name with
    | Ok _ -> fail ("Expected error for: " ^ class_name)
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

let all_utilities () =
  let open Tw in
  [ container_type_size; container_type_inline_size ]

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"containers suborder matches Tailwind" shuffled

let tests =
  [
    test_case "types" `Quick test_container_types;
    test_case "name" `Quick test_container_name;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
    test_case "containers suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
  ]

let suite = ("containers", tests)
