let test_escape () =
  let test_css = ".inset-3\\/4 { inset: 75%; }" in
  match Css.of_string test_css with
  | Ok _stylesheet -> ()
  | Error e ->
      Alcotest.fail
        (Printf.sprintf "Failed to parse escaped selector: %s"
           (Css.pp_parse_error e))

let suite =
  let open Alcotest in
  ( "escape_debug",
    [ test_case "parse backslash escape in selector" `Quick test_escape ] )
