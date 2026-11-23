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

let git dir args =
  let cmd =
    Filename.quote_command "git"
      ([ "-C"; dir; "-c"; "user.name=t"; "-c"; "user.email=t@t" ] @ args)
  in
  if Sys.command (cmd ^ " >/dev/null 2>&1") <> 0 then
    Alcotest.failf "git %s failed" (String.concat " " args)

(* A checkout parked on a commit names no branch. *)
let test_detached_position () =
  let dir = Filename.temp_dir "provenance" "" in
  Fun.protect ~finally:(fun () ->
      ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; dir ])))
  @@ fun () ->
  git dir [ "init"; "-q" ];
  git dir [ "commit"; "-q"; "--allow-empty"; "-m"; "a" ];
  git dir [ "checkout"; "-q"; "--detach" ];
  let position =
    Tw_tools.Cascade_provenance.checkout_position (Filename.quote dir)
  in
  check bool position true
    (String.starts_with ~prefix:"detached HEAD," position)

let tests =
  [
    test_case "dune-project constraint" `Quick test_constraint;
    test_case "detached checkout" `Quick test_detached_position;
  ]

let suite = ("cascade_provenance", tests)
