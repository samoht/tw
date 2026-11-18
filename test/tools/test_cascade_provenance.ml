open Alcotest

let dune_project depends =
  String.concat "\n"
    [
      "(lang dune 3.0)";
      "(package";
      " (name tw)";
      " (depends";
      depends;
      "  ocaml))";
    ]

(* The bound is shown whatever form it takes: a lower bound alone or a range. *)
let test_constraint () =
  let read depends =
    Tw_tools.Cascade_provenance.constraint_of_dune_project
      (dune_project depends)
  in
  check (option string) "a lower bound" (Some "(cascade (>= 1.3.0))")
    (read "  (cascade (>= 1.3.0))");
  check (option string) "a range" (Some "(cascade (and (>= 1.2.0) (< 2.0)))")
    (read "  (cascade (and (>= 1.2.0) (< 2.0)))");
  check (option string) "no cascade" None (read "  (re (>= 1.0))")

let tests = [ test_case "dune-project constraint" `Quick test_constraint ]
let suite = ("cascade_provenance", tests)
