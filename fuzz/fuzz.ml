(** CSS parser fuzz tests — main entry point.

    Registers all fuzz test modules and runs them with Crowbar. *)

let () =
  Fuzz_reader.run ();
  Fuzz_selector.run ();
  Fuzz_values.run ();
  Fuzz_stylesheet.run ();
  Fuzz_supports.run ();
  Fuzz_font_face.run ();
  Fuzz_keyframe.run ()
