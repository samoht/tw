(* Main test runner with timing stats *)

let with_stats name f =
  let t0 = Unix.gettimeofday () in
  let finally ok =
    let dt = Unix.gettimeofday () -. t0 in
    match ok with
    | `Ok -> Fmt.epr "[%s] completed in %.3fs@." name dt
    | `Error -> Fmt.epr "[%s] failed in %.3fs@." name dt
  in
  match f () with
  | () -> finally `Ok
  | exception exn ->
      finally `Error;
      raise exn

let () =
  with_stats "tw" @@ fun () ->
  Alcotest.run "tw"
    [
      Test_tw.suite;
      Test_svg.suite;
      Test_tables.suite;
      Test_accessibility.suite;
      Test_flow.suite;
      Test_color.suite;
      Test_spacing.suite;
      Test_borders.suite;
      Test_sizing.suite;
      Test_layout.suite;
      Test_typography.suite;
      Test_effects.suite;
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
      Test_preflight.suite;
      Test_transforms.suite;
      Test_var.suite;
      Test_modifiers.suite;
      Test_pp.suite;
      Test_prose.suite;
    ]
