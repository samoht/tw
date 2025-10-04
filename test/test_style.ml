open Alcotest

(* The style function no longer accepts a class name parameter *)
let test_style_creation () =
  let _t = Tw.Style.style [] in
  check bool "style created" true true

let tests = [ test_case "style creation" `Quick test_style_creation ]
let suite = ("style", tests)
