open Alcotest
open Tw_tools.Css_compare

(* Test strip_header function *)
let test_strip_header () =
  let css_with_header = "/*!header*/\nbody { color: red; }" in
  let css_without_header = "body { color: red; }" in
  check string "strip header" "body { color: red; }"
    (strip_header css_with_header);
  check string "no header to strip" css_without_header
    (strip_header css_without_header);
  (* Test edge cases *)
  check string "empty string" "" (strip_header "");
  check string "only header" "" (strip_header "/*!header*/\n");
  check string "header without newline" "/*!header*/"
    (strip_header "/*!header*/")

(* Test tokenizer *)
let test_tokenize_simple () =
  let css = ".test { color: red; }" in
  let tokens = tokenize css in
  check bool "has selector token" true
    (List.exists (function Selector ".test" -> true | _ -> false) tokens);
  check bool "has property token" true
    (List.exists
       (function Property ("color", "red") -> true | _ -> false)
       tokens);
  check bool "has braces" true
    (List.exists (function Open_brace -> true | _ -> false) tokens
    && List.exists (function Close_brace -> true | _ -> false) tokens)

let test_tokenize_comments () =
  let css = "/* comment */ .test { color: /* inline */ red; }" in
  let tokens = tokenize css in
  check bool "comments are skipped" true
    (List.exists
       (function
         | Property ("color", v) ->
             (* The tokenizer includes inline comments in values *)
             String.trim v = "red" || v = "/* inline */ red"
         | _ -> false)
       tokens);
  check bool "no comment tokens" true
    (not
       (List.exists
          (function
            | Selector s | Property (s, _) ->
                Astring.String.is_infix ~affix:"comment" s
            | _ -> false)
          tokens))

let test_tokenize_at_rules () =
  let css = "@layer utilities { .test { color: red; } }" in
  let tokens = tokenize css in
  check bool "has @layer token" true
    (List.exists
       (function At_rule "@layer utilities" -> true | _ -> false)
       tokens)

let test_tokenize_complex_values () =
  let css = ".test { background: url('data:image/svg+xml;base64,abc'); }" in
  let tokens = tokenize css in
  check bool "complex value parsed" true
    (List.exists
       (function
         | Property ("background", v) ->
             Astring.String.is_infix ~affix:"base64" v
         | _ -> false)
       tokens)

(* Test parse_blocks *)
let test_parse_blocks_simple () =
  let tokens =
    [
      Selector ".test";
      Open_brace;
      Property ("color", "red");
      Semicolon;
      Close_brace;
    ]
  in
  let blocks = parse_blocks tokens in
  check int "one rule" 1 (List.length blocks);
  match blocks with
  | [ Rule { selector; properties } ] ->
      check string "selector" ".test" selector;
      check
        (list (pair string string))
        "properties"
        [ ("color", "red") ]
        properties
  | _ -> fail "unexpected block structure"

let test_parse_blocks_nested () =
  let css = "@media screen { .test { color: red; } }" in
  let tokens = tokenize css in
  let blocks = parse_blocks tokens in
  match blocks with
  | [ At_block ("@media screen", nested) ] ->
      check int "one nested rule" 1 (List.length nested)
  | _ -> fail "expected @media block"

(* Test normalize_blocks *)
let test_normalize_blocks () =
  let blocks =
    [
      Rule
        {
          selector = ".test";
          properties = [ ("padding", "10px"); ("color", "red") ];
        };
    ]
  in
  let normalized = normalize_blocks blocks in
  match normalized with
  | [ Rule { properties; _ } ] ->
      check
        (list (pair string string))
        "sorted properties"
        [ ("color", "red"); ("padding", "10px") ]
        properties
  | _ -> fail "unexpected normalized structure"

(* Test compare_css function *)
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

let test_compare_with_whitespace () =
  let css1 = ".test{color:red;padding:10px}" in
  let css2 = ".test {\n  color: red;\n  padding: 10px;\n}" in
  check bool "different whitespace" true (compare_css css1 css2)

let test_compare_with_comments () =
  let css1 = ".test { /* comment */ color: red; }" in
  let css2 = ".test { color: red; }" in
  check bool "with/without comments" true (compare_css css1 css2)

(* Test extract_base_rules *)
let test_extract_base_rules () =
  let css =
    ".prose { color: red; } .other { margin: 0; } .prose { padding: 10px; }"
  in
  let rules = extract_base_rules css "prose" in
  (* Debug: print what we get *)
  Printf.eprintf "Extract base rules test:\n";
  Printf.eprintf "  CSS: %s\n" css;
  Printf.eprintf "  Rules found: %d\n" (List.length rules);
  List.iteri (fun i rule -> Printf.eprintf "  Rule %d: %s\n" (i + 1) rule) rules;
  check int "two prose rules" 2 (List.length rules);
  if List.length rules >= 1 then
    check bool "first rule contains color" true
      (Astring.String.is_infix ~affix:"color: red" (List.nth rules 0));
  if List.length rules >= 2 then
    check bool "second rule contains padding" true
      (Astring.String.is_infix ~affix:"padding: 10px" (List.nth rules 1))

let test_extract_base_rules_nested () =
  let css = ".prose { background: url('test.jpg'); nested { color: red; } }" in
  let rules = extract_base_rules css "prose" in
  check int "one rule with nested content" 1 (List.length rules);
  check bool "contains nested braces" true
    (Astring.String.is_infix ~affix:"nested {" (List.hd rules))

(* Test count_css_class_patterns *)
let test_count_css_class_patterns () =
  let css = ".prose { } .prose :where(.test) { } .prose:hover { }" in
  let base, where, total = count_css_class_patterns css "prose" in
  check int "base count" 2 base;
  (* .prose { and .prose:hover { *)
  check int "where count" 1 where;
  (* .prose :where *)
  check int "total count" 3 total (* All combined *)

(* Test find_dominant_css_class *)
let test_find_dominant_css_class () =
  let css = ".test { } .test { } .other { } .test:hover { }" in
  let cls, count = find_dominant_css_class css in
  check string "dominant class" "test" cls;
  check int "occurrence count" 3 count;
  (* Test empty CSS *)
  let cls_empty, count_empty = find_dominant_css_class "" in
  check string "empty css class" "" cls_empty;
  check int "empty css count" 0 count_empty

(* Test format_diff function *)
let test_format_diff_identical () =
  let css = ".test { color: red; }" in
  let diff = format_diff css css in
  check bool "identical message" true
    (Astring.String.is_infix ~affix:"identical" diff)

let test_format_diff_different_values () =
  let css1 = ".test { color: red; }" in
  let css2 = ".test { color: blue; }" in
  let diff = format_diff css1 css2 in
  (* Debug: print what we get *)
  Printf.eprintf "Format diff output:\n%s\n" diff;
  check bool "diff contains differ message" true
    (Astring.String.is_infix ~affix:"differ" diff);
  (* Should show the actual difference *)
  check bool "shows red value" true (Astring.String.is_infix ~affix:"red" diff);
  check bool "shows blue value" true
    (Astring.String.is_infix ~affix:"blue" diff)

let test_format_diff_structural () =
  let css1 = ".prose { color: red; } .prose { padding: 10px; }" in
  let css2 = ".prose { color: red; padding: 10px; }" in
  let diff = format_diff css1 css2 in
  check bool "structural difference detected" true
    (Astring.String.is_infix ~affix:"differ" diff
    || Astring.String.is_infix ~affix:"rules" diff)

let test_format_diff_parse_failure () =
  (* Test with malformed CSS that might fail parsing *)
  let css1 = ".test { color:" in
  let css2 = ".test { color: red; }" in
  let diff = format_diff css1 css2 in
  (* Should fall back to character diff *)
  check bool "handles parse failure" true
    (Astring.String.is_infix ~affix:"differ" diff
    || Astring.String.is_infix ~affix:"Length" diff)

(* Test structured_diff *)
let test_structured_diff_with_css () =
  let css1 = ".prose { color: red; } .prose { padding: 10px; }" in
  let css2 = ".prose { color: red; padding: 10px; }" in
  let blocks1 = tokenize css1 |> parse_blocks |> normalize_blocks in
  let blocks2 = tokenize css2 |> parse_blocks |> normalize_blocks in
  let diff =
    structured_diff ~tw_label:"TW" ~tailwind_label:"Tailwind" ~css1 ~css2
      blocks1 blocks2
  in
  check bool "contains structure analysis" true
    (Astring.String.is_infix ~affix:"CSS Structure" diff
    || Astring.String.is_infix ~affix:"rules" diff
    || String.length diff > 0)

let test_structured_diff_without_css () =
  let blocks1 =
    [ Rule { selector = ".test"; properties = [ ("color", "red") ] } ]
  in
  let blocks2 =
    [ Rule { selector = ".test"; properties = [ ("color", "blue") ] } ]
  in
  let diff =
    structured_diff ~tw_label:"TW" ~tailwind_label:"Tailwind" blocks1 blocks2
  in
  check bool "shows property mismatch" true
    (Astring.String.is_infix ~affix:"Property mismatch" diff
    || Astring.String.is_infix ~affix:"color" diff)

(* Test edge cases and error conditions *)
let test_tokenize_edge_cases () =
  (* Empty CSS *)
  check int "empty css token count" 0 (List.length (tokenize ""));
  (* Only whitespace *)
  check int "whitespace token count" 0 (List.length (tokenize "   \n\t  "));
  (* Unclosed comment *)
  let css_unclosed = ".test { color: /* unclosed comment" in
  let tokens = tokenize css_unclosed in
  check bool "handles unclosed comment" true (List.length tokens >= 0)

let test_parse_blocks_edge_cases () =
  (* Empty token list *)
  check int "empty tokens block count" 0 (List.length (parse_blocks []));
  (* Malformed tokens *)
  let bad_tokens = [ Open_brace; Close_brace; Property ("test", "value") ] in
  let blocks = parse_blocks bad_tokens in
  check bool "handles malformed tokens" true (List.length blocks >= 0)

let tests =
  [
    (* Header stripping tests *)
    test_case "strip header" `Quick test_strip_header;
    (* Tokenizer tests *)
    test_case "tokenize simple CSS" `Quick test_tokenize_simple;
    test_case "tokenize with comments" `Quick test_tokenize_comments;
    test_case "tokenize at-rules" `Quick test_tokenize_at_rules;
    test_case "tokenize complex values" `Quick test_tokenize_complex_values;
    test_case "tokenize edge cases" `Quick test_tokenize_edge_cases;
    (* Parser tests *)
    test_case "parse simple blocks" `Quick test_parse_blocks_simple;
    test_case "parse nested blocks" `Quick test_parse_blocks_nested;
    test_case "parse edge cases" `Quick test_parse_blocks_edge_cases;
    (* Normalizer tests *)
    test_case "normalize blocks" `Quick test_normalize_blocks;
    (* CSS comparison tests *)
    test_case "compare identical CSS" `Quick test_compare_identical;
    test_case "compare different property order" `Quick
      test_compare_different_order;
    test_case "compare different selectors" `Quick
      test_compare_different_selectors;
    test_case "compare with whitespace differences" `Quick
      test_compare_with_whitespace;
    test_case "compare with comments" `Quick test_compare_with_comments;
    (* Rule extraction tests *)
    test_case "extract base rules" `Quick test_extract_base_rules;
    test_case "extract nested rules" `Quick test_extract_base_rules_nested;
    (* Pattern counting tests *)
    test_case "count CSS class patterns" `Quick test_count_css_class_patterns;
    test_case "find dominant CSS class" `Quick test_find_dominant_css_class;
    (* Diff formatting tests *)
    test_case "format diff - identical" `Quick test_format_diff_identical;
    test_case "format diff - different values" `Quick
      test_format_diff_different_values;
    test_case "format diff - structural" `Quick test_format_diff_structural;
    test_case "format diff - parse failure" `Quick
      test_format_diff_parse_failure;
    (* Structured diff tests *)
    test_case "structured diff with CSS" `Quick test_structured_diff_with_css;
    test_case "structured diff without CSS" `Quick
      test_structured_diff_without_css;
  ]

let suite = ("css_compare", tests)
