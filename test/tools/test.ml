(* Test runner for tools library *)

let () =
  Tw_tools.Tailwind_gen.with_stats @@ fun () ->
  Alcotest.run "tools" [ Test_entrypoint.suite; Test_tailwind_gen.suite ]
