(* Test runner for tools library *)

let () =
  Tw_tools.Tailwind_gen.with_stats @@ fun () ->
  Alcotest.run "tools"
    [
      Test_tailwind_gen.suite;
      Test_css_compare.suite;
      Test_string_diff.suite;
      ("tree_diff", Test_tree_diff.suite);
    ]
