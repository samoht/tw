open Alcotest

(* The style function no longer accepts a class name parameter *)
let test_style_creation () =
  let _t = Tw.Style.style [] in
  check bool "style created" true true

let test_equal_modifier () =
  let open Tw.Style in
  check bool "same nested modifier" true
    (equal_modifier (Not (Data_state "open")) (Not (Data_state "open")));
  check bool "different nested modifier" false
    (equal_modifier (Not (Data_state "open")) (Not (Data_state "closed")))

let tests =
  [
    test_case "style creation" `Quick test_style_creation;
    test_case "modifier equality" `Quick test_equal_modifier;
  ]

let suite = ("style", tests)
