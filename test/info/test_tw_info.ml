let test_non_empty () =
  Alcotest.(check bool) "version is non-empty" true (Tw_info.version <> "")

(* The version string is spliced into an OCaml literal by a printf rule; a
   quote, backslash, newline or space in it means the rule mangled the
   PROJECT_VERSION value. *)
let test_well_formed () =
  String.iter
    (fun c ->
      let bad = c = '"' || c = '\\' || c = '\n' || c = ' ' in
      Alcotest.(check bool) (Printf.sprintf "version has no %C" c) false bad)
    Tw_info.version

let suite =
  ( "tw_info",
    [
      Alcotest.test_case "non-empty" `Quick test_non_empty;
      Alcotest.test_case "well-formed" `Quick test_well_formed;
    ] )
