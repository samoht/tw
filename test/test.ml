(* Main test runner *)

let () =
  Alcotest.run "tw"
    ([
       Test_tw.suite;
       Test_color.suite;
       Test_spacing.suite;
       Test_borders.suite;
       Test_sizing.suite;
       Test_layout.suite;
       Test_typography.suite;
       Test_effects.suite;
       Test_aspect.suite;
       Test_clipping.suite;
       Test_animations.suite;
       Test_backgrounds.suite;
       Test_containers.suite;
       Test_core.suite;
       Test_filters.suite;
       Test_forms.suite;
       Test_interactivity.suite;
       Test_parse.suite;
       Test_positioning.suite;
       Test_rules.suite;
       Test_transforms.suite;
       Test_var.suite;
     ]
    @ Test_pp.suite @ Test_prose.suite)
