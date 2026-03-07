let test_escape_in_selector () =
  let test_css = ".inset-3\\/4 { inset: 75%; }" in
  match Css.of_string test_css with
  | Ok _stylesheet -> ()
  | Error e ->
      Alcotest.fail
        (Fmt.str "Failed to parse escaped selector: %s" (Css.pp_parse_error e))

let tests =
  Alcotest.
    [
      test_case "parse backslash escape in selector" `Quick
        test_escape_in_selector;
    ]

let suite = ("parse", tests)
