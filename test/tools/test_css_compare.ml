open Alcotest
open Tw_tools.Css_compare

let test_strip_header () =
  let css_with_header = "/*!header*/\nbody { color: red; }" in
  let css_without_header = "body { color: red; }" in
  check string "strip header" "body { color: red; }"
    (strip_header css_with_header);
  check string "no header to strip" css_without_header
    (strip_header css_without_header)

let test_compare_identical () =
  let css = ".test { color: red; padding: 10px; }" in
  check bool "identical CSS" true (compare_css css css)

let test_compare_different_order () =
  let css1 = ".test { color: red; padding: 10px; }" in
  let css2 = ".test { padding: 10px; color: red; }" in
  check bool "different property order" true (compare_css css1 css2)

let test_compare_different_selectors () =
  let css1 = ".test1 { color: red; }" in
  let css2 = ".test2 { color: red; }" in
  check bool "different selectors" false (compare_css css1 css2)

let test_format_diff () =
  let css1 = ".test { color: red; }" in
  let css2 = ".test { color: blue; }" in
  let diff = format_diff css1 css2 in
  check bool "diff contains mismatch message" true
    (Astring.String.is_infix ~affix:"differ" diff)

let tests =
  [
    test_case "strip header" `Quick test_strip_header;
    test_case "compare identical CSS" `Quick test_compare_identical;
    test_case "compare different property order" `Quick
      test_compare_different_order;
    test_case "compare different selectors" `Quick
      test_compare_different_selectors;
    test_case "format diff" `Quick test_format_diff;
  ]

let suite = ("css_compare", tests)
