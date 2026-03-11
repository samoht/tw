(** CSS parser fuzz tests — main entry point.

    Registers all fuzz test modules and runs them with Crowbar. *)

let run_suite (_name, tests) = List.iter (fun f -> f ()) tests

let () =
  List.iter run_suite
    [
      Fuzz_reader.suite;
      Fuzz_selector.suite;
      Fuzz_values.suite;
      Fuzz_stylesheet.suite;
      Fuzz_supports.suite;
      Fuzz_font_face.suite;
      Fuzz_keyframe.suite;
    ]
