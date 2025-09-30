open Cmdliner

(* Define --seed option *)
let seed =
  let doc = "Random seed for property-based tests (for reproducibility)" in
  Arg.(value & opt (some int) None & info [ "seed" ] ~docv:"SEED" ~doc)

(* Initialize random seed and return test suites *)
let setup =
  let init = function
    | Some s ->
        Random.init s;
        Fmt.epr "Using random seed: %d\n%!" s
    | None ->
        (* Generate a seed from system entropy (time + process id) *)
        let s =
          int_of_float (Unix.gettimeofday () *. 1000.0) lxor Unix.getpid ()
        in
        Random.init s;
        Fmt.epr "Using random seed: %d (use --seed %d to reproduce)\n%!" s s
  in
  Term.(const init $ seed)

let () =
  (* Use Alcotest.run_with_args for proper CLI integration *)
  Tw_tools.Tailwind_gen.with_stats @@ fun () ->
  Alcotest.run_with_args "tw" setup
    (* Return test suites *)
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
