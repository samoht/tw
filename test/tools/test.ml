(* Test runner for tools library *)

let () =
  Tw_tools.Cascade_provenance.report ();
  Tw_tools.Tailwind_gen.with_stats @@ fun () ->
  Alcotest.run "tools"
    [
      Test_entrypoint.suite;
      Test_tailwind_import.suite;
      Test_entry_theme.suite;
      Test_custom_utility.suite;
      Test_custom_variant.suite;
      Test_negated_variant.suite;
      Test_value_fn.suite;
      Test_author_css.suite;
      Test_routing.suite;
      Test_project.suite;
      Test_tailwind_gen.suite;
      Test_parity_compare.suite;
    ]
