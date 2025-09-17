(* Test runner for tools library *)

let () =
  Alcotest.run "tools"
    [
      Test_css_debug.suite;
      Test_tailwind_gen.suite;
      Test_css_compare.suite;
      Test_diff_format.suite;
    ]
