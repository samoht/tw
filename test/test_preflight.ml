open Alcotest

let stylesheet_nonempty () =
  let stylesheet = Tw.preflight () in
  let rules = Css.rules stylesheet in
  check bool "preflight has rules" true (List.length rules > 0)

let tests = [ test_case "stylesheet is non-empty" `Quick stylesheet_nonempty ]
let suite = ("preflight", tests)
