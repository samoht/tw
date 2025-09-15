open Alcotest
open Tw_tools.Css_debug

let test_format_css () =
  let minified = ".test{color:red;padding:10px}" in
  let formatted = format_css minified in
  check bool "formatted contains newlines" true
    (Astring.String.is_infix ~affix:"\n" formatted);
  check bool "formatted contains .test" true
    (Astring.String.is_infix ~affix:".test" formatted)

let test_extract_rule () =
  let css = ".test { color: red; } .other { color: blue; }" in
  let rule = extract_rule css ".test" in
  check (option string) "extract existing rule" (Some "color: red;") rule;
  let missing = extract_rule css ".missing" in
  check (option string) "extract missing rule" None missing

let test_first_diff () =
  let css1 = "abc" in
  let css2 = "abc" in
  check
    (option (triple int string string))
    "identical strings" None (first_diff css1 css2);

  let css1 = "abc" in
  let css2 = "axc" in
  match first_diff css1 css2 with
  | Some (pos, _, _) -> check int "diff position" 1 pos
  | None -> fail "Expected difference"

let test_write_temp_css () =
  let css = ".test { color: red; }" in
  let path = write_temp_css "test" css in
  check bool "temp file created" true (Sys.file_exists path);
  (* Clean up *)
  try Sys.remove path with Sys_error _ -> ()

let tests =
  [
    test_case "format CSS" `Quick test_format_css;
    test_case "extract rule" `Quick test_extract_rule;
    test_case "find first diff" `Quick test_first_diff;
    test_case "write temp CSS" `Quick test_write_temp_css;
  ]

let suite = ("css_debug", tests)
