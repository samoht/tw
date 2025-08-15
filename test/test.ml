(* Main test runner *)

let () =
  Alcotest.run "tw"
    ([ Test_tw.suite; Test_color.suite; Test_version.suite ]
    @ Test_css.suite @ Test_pp.suite @ Test_prose.suite)
