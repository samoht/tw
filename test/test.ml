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
      Test_display.suite;
      Test_flex.suite;
      Test_flex_props.suite;
      Test_alignment.suite;
      Test_grid.suite;
      Test_grid_item.suite;
      Test_grid_template.suite;
      Test_gap.suite;
      Test_cursor.suite;
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
      Test_rules.suite;
      Test_preflight.suite;
      Test_transforms.suite;
      Test_var.suite;
      Test_modifiers.suite;
      Test_pp.suite;
      Test_prose.suite;
      Test_theme.suite;
      Test_utility.suite;
      Test_spacing.suite;
    ]

(* Debug test for escape parsing *)
let test_escape_in_selector () =
  let test_css = ".inset-3\\/4 { inset: 75%; }" in
  Printf.printf "\n=== ESCAPE DEBUG ===\n";
  Printf.printf "Input: %S\n" test_css;
  Printf.printf "Chars: ";
  for i = 0 to min 14 (String.length test_css - 1) do
    Printf.printf "[%d]='%c'(0x%02x) " i test_css.[i] (Char.code test_css.[i])
  done;
  Printf.printf "\n";
  match Css.of_string test_css with
  | Ok _ -> Printf.printf "✓ Parsed OK\n"
  | Error e ->
      Printf.printf "✗ Error: %s\n" (Css.pp_parse_error e);
      Alcotest.fail "parse failed"

let () = test_escape_in_selector ()

let () =
  Printf.printf "Testing CSS escape parsing...\n";
  let css = ".w-1\\/2 { width: 50%%; }" in
  Printf.printf "Input: %S\n" css;
  match Css.of_string css with
  | Ok stylesheet ->
      Printf.printf "✓ SUCCESS! Parsed correctly.\n";
      Printf.printf "Output: %s\n" (Css.to_string stylesheet)
  | Error e ->
      Printf.printf "✗ FAILED:\n%s\n" (Css.pp_parse_error e);
      exit 1
