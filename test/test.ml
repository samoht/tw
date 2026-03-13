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
      Test_flex.suite;
      Test_flex_props.suite;
      Test_alignment.suite;
      Test_grid.suite;
      Test_grid_item.suite;
      Test_grid_template.suite;
      Test_gap.suite;
      Test_cursor.suite;
      Test_arbitrary.suite;
      Test_color.suite;
      Test_margin.suite;
      Test_padding.suite;
      Test_borders.suite;
      Test_sizing.suite;
      Test_layout.suite;
      Test_typography.suite;
      Test_effects.suite;
      Test_clipping.suite;
      Test_animations.suite;
      Test_backgrounds.suite;
      Test_containers.suite;
      Test_style.suite;
      Test_filters.suite;
      Test_forms.suite;
      Test_interactivity.suite;
      Test_parse.suite;
      Test_position.suite;
      Test_output.suite;
      Test_rule.suite;
      Test_build.suite;
      Test_sort.suite;
      Test_preflight.suite;
      Test_transforms.suite;
      Test_var.suite;
      Test_modifiers.suite;
      Test_pp.suite;
      Test_prose.suite;
      Test_theme.suite;
      Test_utility.suite;
      Test_spacing.suite;
      Test_box_sizing.suite;
      Test_columns.suite;
      Test_contain.suite;
      Test_divide.suite;
      Test_field_sizing.suite;
      Test_flex_layout.suite;
      Test_mask_gradient.suite;
      Test_masks.suite;
      Test_overflow.suite;
      Test_overflow_wrap.suite;
      Test_overscroll.suite;
      Test_property.suite;
      Test_scheme.suite;
      Test_scroll.suite;
      Test_text_shadow.suite;
      Test_touch.suite;
      Test_transitions.suite;
    ]
