(* Main test runner *)

let () =
  Alcotest.run "tw"
    ([
       Test_tw.suite;
       Test_color.suite;
       ("spacing", Test_spacing.tests);
       ("borders", Test_borders.tests);
       ("sizing", Test_sizing.tests);
       ("layout", Test_layout.tests);
       ("typography", Test_typography.tests);
       ("effects", Test_effects.tests);
       ("aspect", Test_aspect.tests);
       ("clipping", Test_clipping.tests);
     ]
    @ Test_pp.suite @ Test_prose.suite)
