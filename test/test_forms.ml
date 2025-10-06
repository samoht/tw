open Alcotest

let check class_name =
  match Tw.Forms.Handler.of_class class_name with
  | Ok u -> check string "forms class" class_name (Tw.Forms.Handler.to_class u)
  | Error (`Msg msg) -> fail msg

let test_inputs () =
  check "form-input";
  check "form-checkbox"

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
  let open Tw in
  [ form_input; form_checkbox; form_radio; form_select; form_textarea ]

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in
  Test_helpers.check_ordering_matches ~forms:true
    ~test_name:"forms suborder matches Tailwind" shuffled

let tests =
  [
    test_case "inputs" `Quick test_inputs;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
    test_case "forms suborder matches Tailwind" `Quick suborder_matches_tailwind;
  ]

let suite = ("forms", tests)
