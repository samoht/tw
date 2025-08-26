open Alcotest

let test_stylesheet_nonempty () =
  let rules = Tw.preflight () in
  check bool "preflight has rules" true (List.length rules > 0)

let tests =
  [ test_case "stylesheet is non-empty" `Quick test_stylesheet_nonempty ]

let suite = ("preflight", tests)
