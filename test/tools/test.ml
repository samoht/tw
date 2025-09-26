(* Test runner for tools library *)

let () =
  Tw_tools.Tailwind_gen.with_stats @@ fun () ->
  Alcotest.run "tools"
    [
      Test_css_debug.suite;
      Test_tailwind_gen.suite;
      Test_css_compare.suite;
      Test_diff_format.suite;
    ]
