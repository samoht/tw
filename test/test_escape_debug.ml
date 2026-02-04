let test_escape () =
  let test_css = ".inset-3\\/4 { inset: 75%; }" in

  Printf.printf "\n=== ESCAPE TEST ===\n%!";
  Printf.printf "Input CSS: %S\n%!" test_css;
  Printf.printf "First 15 characters:\n%!";
  for i = 0 to min 14 (String.length test_css - 1) do
    Printf.printf "  [%2d] '%c' (0x%02x)\n%!" i test_css.[i]
      (Char.code test_css.[i])
  done;

  match Css.of_string test_css with
  | Ok stylesheet ->
      Printf.printf "\n✓ SUCCESS: Parsed!\n%!";
      Printf.printf "Output:\n%s\n%!" (Css.to_string stylesheet);
      Alcotest.(check pass) "should parse" () ()
  | Error msg ->
      Printf.printf "\n✗ ERROR:\n%s\n%!" (Css.pp_parse_error msg);
      Alcotest.fail "Failed to parse escaped selector"

let () =
  Alcotest.run "escape_debug"
    [ ("escape", [ Alcotest.test_case "parse \\/" `Quick test_escape ]) ]
