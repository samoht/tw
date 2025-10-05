open Alcotest

let test_inputs () =
  (* Form utilities were removed from Tailwind v4 core *)
  let test_not_supported input =
    match Tw.Forms.Handler.of_class input with
    | Ok _ -> fail ("Expected error for: " ^ input)
    | Error _ -> ()
  in
  test_not_supported "form-input";
  test_not_supported "form-checkbox"

let test_of_string_invalid () =
  (* Invalid form utilities *)
  let test_invalid input =
    let class_name = String.concat "-" input in
    match Tw.Forms.Handler.of_class class_name with
    | Ok _ -> fail ("Expected error for: " ^ class_name)
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

let all_utilities () =
  (* Form utilities are not part of Tailwind v4 core *)
  []

let suborder_matches_tailwind () =
  (* No form utilities exist in Tailwind v4, so this should pass with empty
     list *)
  let shuffled = Test_helpers.shuffle (all_utilities ()) in
  Test_helpers.check_ordering_matches
    ~test_name:"forms suborder matches Tailwind" shuffled

let tests =
  [
    test_case "inputs" `Quick test_inputs;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
    test_case "forms suborder matches Tailwind" `Quick suborder_matches_tailwind;
  ]

let suite = ("forms", tests)
