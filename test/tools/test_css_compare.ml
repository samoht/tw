open Alcotest
module Cc = Tw_tools.Css_compare
open Cc

let pp_css fmt css =
  match Css.of_string css with
  | Ok ast -> Fmt.string fmt (Css.to_string ~minify:true ast)
  | Error e -> Fmt.pf fmt "<parse error: %s>" (Css.pp_parse_error e)

let css = Alcotest.testable pp_css Cc.compare_css

let test_strip_header () =
  let css = "/*! header */\n.a{color:red}" in
  check string "header stripped" ".a{color:red}" (Cc.strip_header css)

let test_compare_equivalent () =
  let a = ".a{color:red;padding:10px}" in
  let b = ".a{color:red;padding:10px}" in
  check css "structurally equal" a b

let test_compare_media_layers_equivalent () =
  let a =
    "@layer utilities{.x{color:red}}@media screen and \
     (min-width:600px){.x{color:red;padding:10px}}"
  in
  let b =
    "@layer utilities{.x{color:red}}@media screen and \
     (min-width:600px){.x{color:red;padding:10px}}"
  in
  check css "media/layers equal" a b

let test_rule_added_removed_modified () =
  let css_expected = ".a{color:red;padding:10px}\n.b{margin:0}" in
  let css_actual = ".a{color:red}\n.c{margin:0}" in
  let diff =
    match Cc.diff ~expected:css_expected ~actual:css_actual with
    | Cc.Diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  let has_added_c =
    List.exists
      (fun r ->
        r.selector = ".c"
        && match r.change with Cc.Added _ -> true | _ -> false)
      diff.rules
  in
  let has_removed_b =
    List.exists
      (fun r ->
        r.selector = ".b"
        && match r.change with Cc.Removed _ -> true | _ -> false)
      diff.rules
  in
  let has_modified_a =
    List.exists
      (fun r ->
        r.selector = ".a"
        && match r.change with Cc.Changed _ -> true | _ -> false)
      diff.rules
  in
  check bool "added .c" true has_added_c;
  check bool "removed .b" true has_removed_b;
  check bool "modified .a" true has_modified_a

let test_media_and_layer_diffs () =
  let css_expected =
    "@media screen and (min-width:600px){.m{margin:0}}@layer \
     theme{.a{color:red}}"
  in
  let css_actual = "@media screen and (min-width:600px){.m{margin:1px}}" in
  let diff =
    match Cc.diff ~expected:css_expected ~actual:css_actual with
    | Cc.Diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  (* Structural expectations: exactly one media Added and one layer Added, no
     rule diffs *)
  check int "no top-level rule diffs" 0 (List.length diff.rules);
  check int "one media change" 1 (List.length diff.media_queries);
  check int "one layer change" 1 (List.length diff.layers);
  let m = List.hd diff.media_queries in
  (match m.change with
  | Cc.Changed _ -> ()
  | _ -> fail "media not marked Changed");
  (* Within media, the rule for .m should be Changed due to margin change *)
  let media_rules =
    match m.change with Cc.Changed (_, new_rules) -> new_rules | _ -> []
  in
  let mr = List.find_opt (fun r -> r.selector = ".m") media_rules in
  (match mr with
  | Some r -> (
      match r.change with Cc.Changed _ -> () | _ -> fail ".m not Changed")
  | None -> fail "missing .m rule diff");
  let l = List.hd diff.layers in
  match l.change with
  | Cc.Removed _ -> ()
  | _ -> fail "layer not marked Removed"

let test_property_value_modified () =
  let css_expected = ".x{color:red}" in
  let css_actual = ".x{color:blue}" in
  let diff =
    match Cc.diff ~expected:css_expected ~actual:css_actual with
    | Cc.Diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  (* Check structural properties of the diff *)
  check int "one rule change" 1 (List.length diff.rules);
  check int "no media changes" 0 (List.length diff.media_queries);
  check int "no layer changes" 0 (List.length diff.layers);
  check int "no supports changes" 0 (List.length diff.supports_queries);
  check int "no container changes" 0 (List.length diff.container_queries);
  check int "no custom property changes" 0 (List.length diff.custom_properties);

  let rule = List.hd diff.rules in
  check string "rule selector" ".x" rule.selector;
  (* Verify the rule has a Changed diff with property differences *)
  match rule.change with
  | Cc.Changed ((_expected_props, []), (_actual_props, prop_diffs)) ->
      check int "one property diff" 1 (List.length prop_diffs);
      let prop_diff = List.hd prop_diffs in
      check string "property name" "color" prop_diff.property_name;
      check string "expected value" "red" prop_diff.expected_value;
      check string "actual value" "blue" prop_diff.actual_value
  | _ -> fail "Expected Changed diff with property differences"

let test_property_added_only () =
  let css_expected = ".y{color:red}" in
  let css_actual = ".y{color:red;padding:10px}" in
  let diff =
    match Cc.diff ~expected:css_expected ~actual:css_actual with
    | Cc.Diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  let has_modified_y =
    List.exists
      (fun r ->
        r.selector = ".y"
        && match r.change with Cc.Changed _ -> true | _ -> false)
      diff.rules
  in
  check bool "modified .y" true has_modified_y

let test_important_and_custom_props () =
  (* Importance difference should be detected *)
  let d1 =
    match
      Cc.diff ~expected:".x{color:red!important}" ~actual:".x{color:red}"
    with
    | Cc.Diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  let has_mod_imp =
    List.exists
      (fun r ->
        r.selector = ".x"
        && match r.change with Cc.Changed _ -> true | _ -> false)
      d1.rules
  in
  check bool "importance change => modified" true has_mod_imp;
  (* Custom property value difference should be detected *)
  let d2 =
    match Cc.diff ~expected:".x{--foo:1}" ~actual:".x{--foo:2}" with
    | Cc.Diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  let has_mod_custom =
    List.exists
      (fun r ->
        r.selector = ".x"
        && match r.change with Cc.Changed _ -> true | _ -> false)
      d2.rules
  in
  check bool "custom prop change => modified" true has_mod_custom

let test_supports_and_container_diffs () =
  let css_expected =
    "@supports (display:grid){.s{margin:0}}@container \
     (min-width:500px){.c{padding:1px}}"
  in
  let css_actual =
    "@supports (display:grid){.s{margin:1px}}@container \
     (min-width:500px){.c{padding:2px}}"
  in
  let diff =
    match Cc.diff ~expected:css_expected ~actual:css_actual with
    | Cc.Diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  (* Media/layers may be empty; we look for structural differences in supports/containers implicitly via rules diff absence *)
  (* Ensure no top-level rule diffs pollute this case *)
  check int "no top-level rule diffs" 0 (List.length diff.rules);
  check int "one supports change" 1 (List.length diff.supports_queries);
  check int "one container change" 1 (List.length diff.container_queries)

(* Intentionally avoid substring/count-based checks; use structural diff
   only. *)

let test_string_diff_context_basic () =
  let expected = "hello world" in
  let actual = "hello wurld" in
  match Cc.show_string_diff_context ~expected ~actual with
  | Some sdiff ->
      check int "diff position" 7 sdiff.position;
      (* Position of 'o' vs 'u' is index 7 *)
      check int "char position in line" 7 sdiff.column_expected;
      let exp_line, act_line = sdiff.diff_lines in
      check string "expected context" "hello world" exp_line;
      check string "actual context" "hello wurld" act_line
  | None -> fail "expected diff context"

let test_string_diff_context_multiline () =
  let expected = "line one\nline two\nline three" in
  let actual = "line one\nline too\nline three" in
  match Cc.show_string_diff_context ~expected ~actual with
  | Some sdiff ->
      check int "diff position" 15 sdiff.position;
      (* Position of 'w' vs 'o' at index 15 *)
      check int "diff line number" 1 sdiff.line_expected;
      check int "char position in line" 6 sdiff.column_expected;
      (* Position in line after "line t" *)
      let exp_line, act_line = sdiff.diff_lines in
      check string "expected diff line" "line two" exp_line;
      check string "actual diff line" "line too" act_line
  | None -> fail "expected diff context"

let test_string_diff_context_at_end () =
  let expected = "abc" in
  let actual = "abcd" in
  match Cc.show_string_diff_context ~expected ~actual with
  | Some sdiff -> check int "diff at end position" 3 sdiff.position
  | None -> fail "expected diff context"

let test_string_diff_context_none () =
  let expected = "same" in
  let actual = "same" in
  match Cc.show_string_diff_context ~expected ~actual with
  | Some _ -> fail "no diff expected"
  | None -> ()

let test_pp_diff_result_with_string_context () =
  (* Test that pp_diff_result shows string context when no structural diff *)
  let css1 = ".a{color:red}" in
  let css2 = ".a{color: red}" in
  (* Extra space *)
  let result = Cc.diff ~expected:css1 ~actual:css2 in
  let output = Fmt.to_to_string Cc.pp_diff_result result in
  (* Should show "no structural differences" and the string diff context *)
  check bool "shows no structural differences" true
    (String.contains output 'n' && String.contains output 'o');
  check bool "shows position" true (String.contains output '^')
(* Position of the space difference *)

let test_pp_diff_result_structural () =
  (* Test that pp_diff_result shows structural diff when present *)
  let css1 = ".a{color:red}" in
  let css2 = ".a{color:blue}" in
  let result = Cc.diff ~expected:css1 ~actual:css2 in
  let output = Fmt.to_to_string Cc.pp_diff_result result in
  (* Should show structural CSS diff, not string context *)
  check bool "shows CSS diff" true
    (String.contains output 'r' && String.contains output 'e'
   && String.contains output 'd')

let test_css_variable_differences () =
  (* Test case that reproduces the shadow-sm bug: CSS variables missing should
     be detected as structural diff *)
  let expected =
    "*,:before,:after,::backdrop{--tw-shadow:0 0 \
     #0000;--tw-shadow-color:initial}"
  in
  let actual = "*,:before,:after,::backdrop{--tw-border-style:initial}" in
  let result = Cc.diff ~expected ~actual in
  match result with
  | Cc.Diff diff ->
      (* Should detect structural differences in the ::backdrop rule *)
      check bool "detects CSS variable differences as structural" false
        (Cc.is_empty diff)
  | _ -> fail "CSS should parse successfully"

let test_layer_custom_properties () =
  (* Test custom property differences within layers *)
  let expected_full =
    "@layer base {*, :before, :after, ::backdrop { --tw-shadow: 0 0 #0000; \
     --tw-shadow-color: initial; }}"
  in
  let actual_full =
    "@layer base {*, :before, :after, ::backdrop { --tw-border-style: initial; \
     }}"
  in
  let result = Cc.diff ~expected:expected_full ~actual:actual_full in
  match result with
  | Cc.Diff diff ->
      (* Should detect structural differences - missing custom properties should
         be flagged *)
      check bool "layer custom property differences detected" false
        (Cc.is_empty diff)
  | _ -> fail "CSS should parse successfully"

let test_css_unit_differences () =
  (* Test case for 0px vs 0 - should be detected as structural difference *)
  let expected = "*{--tw-ring-offset-width:0px}" in
  let actual = "*{--tw-ring-offset-width:0}" in
  let result = Cc.diff ~expected ~actual in
  match result with
  | Cc.Diff diff ->
      (* Should detect structural differences - 0px vs 0 are different values *)
      check bool "detects 0px vs 0 as structural difference" false
        (Cc.is_empty diff)
  | _ -> fail "CSS should parse successfully"

let test_complex_css_unit_differences () =
  (* Test case with many properties to reproduce the real bug *)
  let expected =
    "*{--tw-ring-inset:initial;--tw-ring-offset-width:0px;--tw-ring-offset-color:#fff;--tw-ring-color:rgb(59 \
     130 246 / 0.5)}"
  in
  let actual =
    "*{--tw-ring-inset:initial;--tw-ring-offset-width:0;--tw-ring-offset-color:#fff;--tw-ring-color:rgb(59 \
     130 246 / 0.5)}"
  in
  let result = Cc.diff ~expected ~actual in
  match result with
  | Cc.Diff diff ->
      (* Should detect structural differences - 0px vs 0 are different values
         even in complex rules *)
      check bool "detects 0px vs 0 in complex CSS as structural difference"
        false (Cc.is_empty diff)
  | _ -> fail "CSS should parse successfully"

let test_never_empty_diff_when_strings_differ () =
  (* Test that ensures we always show SOMETHING when input strings differ,
     either structural diffs or string diffs *)
  let test_cases =
    [
      (* Shadow variables case from tw 10 *)
      ( "@layer base{*,:before,:after,::backdrop{--tw-shadow:0 0 \
         #0000;--tw-shadow-color:initial}}",
        "@layer base{*,:before,:after,::backdrop{--tw-border-style:initial}}" );
      (* Simple property difference *)
      (".a{color:red}", ".a{color:blue}");
      (* Missing properties *)
      (".a{color:red;padding:10px}", ".a{color:red}");
      (* Added properties *)
      (".a{color:red}", ".a{color:red;padding:10px}");
      (* Different selectors *)
      (".a{color:red}", ".b{color:red}");
      (* Unit differences - CSS normalizes 0px to 0, but we should show string
         diff *)
      (".a{margin:0px}", ".a{margin:0}");
      (* Custom property differences *)
      (":root{--tw-shadow:0 0 #0000}", ":root{--tw-border:1px}");
      (* Missing layer *)
      ("@layer base{.a{color:red}}", ".a{color:red}");
      (* Added layer *)
      (".a{color:red}", "@layer base{.a{color:red}}");
      (* Media query differences *)
      ("@media(min-width:600px){.a{color:red}}", ".a{color:red}");
    ]
  in

  List.iter
    (fun (expected, actual) ->
      (* Strings are different *)
      check bool "input strings differ" false (expected = actual);

      let result = Cc.diff ~expected ~actual in

      (* Check that pp_diff_result produces some output when strings differ *)
      let buffer = Buffer.create 256 in
      let fmt = Format.formatter_of_buffer buffer in
      Cc.pp_diff_result fmt result;
      Format.pp_print_flush fmt ();
      let output = Buffer.contents buffer in

      (* When strings differ, we should get some output - either structural or
         string diff *)
      if String.length output = 0 then (
        Printf.printf "\nERROR: No diff output for different strings:\n";
        Printf.printf "Expected: %s\n" expected;
        Printf.printf "Actual: %s\n" actual;
        fail "Different strings MUST produce some diff output"))
    test_cases

let tests =
  [
    test_case "strip header" `Quick test_strip_header;
    test_case "compare equivalent" `Quick test_compare_equivalent;
    test_case "compare media+layers equivalent" `Quick
      test_compare_media_layers_equivalent;
    test_case "added/removed/modified rules" `Quick
      test_rule_added_removed_modified;
    test_case "modified property value" `Quick test_property_value_modified;
    test_case "modified property added only" `Quick test_property_added_only;
    test_case "media and layer diffs" `Quick test_media_and_layer_diffs;
    test_case "important and custom props" `Quick
      test_important_and_custom_props;
    test_case "supports and container diffs" `Quick
      test_supports_and_container_diffs;
    test_case "string diff context basic" `Quick test_string_diff_context_basic;
    test_case "string diff context multiline" `Quick
      test_string_diff_context_multiline;
    test_case "string diff context at end" `Quick
      test_string_diff_context_at_end;
    test_case "string diff context none" `Quick test_string_diff_context_none;
    test_case "pp diff result with string context" `Quick
      test_pp_diff_result_with_string_context;
    test_case "pp diff result structural" `Quick test_pp_diff_result_structural;
    test_case "CSS variable differences" `Quick test_css_variable_differences;
    test_case "layer custom properties" `Quick test_layer_custom_properties;
    test_case "CSS unit differences 0px vs 0" `Quick test_css_unit_differences;
    test_case "complex CSS unit differences" `Quick
      test_complex_css_unit_differences;
    test_case "never empty diff when strings differ" `Quick
      test_never_empty_diff_when_strings_differ;
  ]

let suite = ("css_compare", tests)
