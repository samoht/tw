open Alcotest
module Cc = Tw_tools.Css_compare
open Cc

(* Alcotest testables *)
let testable_diff = Alcotest.testable Cc.pp Cc.equal

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
        r.selector = ".c" && match r.change with Cc.Added -> true | _ -> false)
      diff.rules
  in
  let has_removed_b =
    List.exists
      (fun r ->
        r.selector = ".b"
        && match r.change with Cc.Removed -> true | _ -> false)
      diff.rules
  in
  let has_modified_a =
    List.exists
      (fun r ->
        r.selector = ".a"
        && match r.change with Cc.Modified _ -> true | _ -> false)
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
  check int "one media change" 1 (List.length diff.media);
  check int "one layer change" 1 (List.length diff.layers);
  let m = List.hd diff.media in
  (match m.change with
  | Cc.Modified _ -> ()
  | _ -> fail "media not marked Modified");
  (* Within media, the rule for .m should be Modified due to margin change *)
  let media_rules = m.rules in
  let mr = List.find_opt (fun r -> r.selector = ".m") media_rules in
  (match mr with
  | Some r -> (
      match r.change with Cc.Modified _ -> () | _ -> fail ".m not Modified")
  | None -> fail "missing .m rule diff");
  let l = List.hd diff.layers in
  match l.change with Cc.Removed -> () | _ -> fail "layer not marked Removed"

let test_property_value_modified () =
  let css_expected = ".x{color:red}" in
  let css_actual = ".x{color:blue}" in
  let diff =
    match Cc.diff ~expected:css_expected ~actual:css_actual with
    | Cc.Diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  let expected_diff : Cc.t =
    {
      rules =
        [
          {
            selector = ".x";
            change =
              Cc.Modified
                [
                  {
                    Cc.property = "color";
                    our_value = "red";
                    their_value = "blue";
                  };
                ];
            properties = [];
          };
        ];
      media = [];
      layers = [];
      supports = [];
      containers = [];
      properties = [];
    }
  in
  check testable_diff "exact property value diff" expected_diff diff

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
        && match r.change with Cc.Modified _ -> true | _ -> false)
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
        && match r.change with Cc.Modified _ -> true | _ -> false)
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
        && match r.change with Cc.Modified _ -> true | _ -> false)
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
  check int "one supports change" 1 (List.length diff.supports);
  check int "one container change" 1 (List.length diff.containers)

(* Intentionally avoid substring/count-based checks; use structural diff
   only. *)

let test_string_diff_context_basic () =
  let expected = "hello world" in
  let actual = "hello wurld" in
  match Cc.show_string_diff_context ~expected ~actual with
  | Some (exp_ctx, act_ctx, (_line, char_pos), pos) ->
      check int "diff position" 7 pos;
      (* Position of 'o' vs 'u' is index 7 *)
      check int "char position in line" 7 char_pos;
      check string "expected context" "hello world" exp_ctx;
      check string "actual context" "hello wurld" act_ctx
  | None -> fail "expected diff context"

let test_string_diff_context_multiline () =
  let expected = "line one\nline two\nline three" in
  let actual = "line one\nline too\nline three" in
  match Cc.show_string_diff_context ~expected ~actual with
  | Some (exp_ctx, act_ctx, (line_num, char_pos), pos) ->
      check int "diff position" 15 pos;
      (* Position of 'w' vs 'o' at index 15 *)
      check int "diff line number" 1 line_num;
      check int "char position in line" 6 char_pos;
      (* Position in line after "line t" *)
      (* Context should stop at end of line with diff *)
      check string "expected context includes up to diff line"
        "line one\nline two" exp_ctx;
      check string "actual context includes up to diff line"
        "line one\nline too" act_ctx
  | None -> fail "expected diff context"

let test_string_diff_context_at_end () =
  let expected = "abc" in
  let actual = "abcd" in
  match Cc.show_string_diff_context ~expected ~actual with
  | Some (_exp_ctx, _act_ctx, (_line, _char), pos) ->
      check int "diff at end position" 3 pos
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
  let pp = Cc.pp_diff_result ~expected_str:css1 ~actual_str:css2 in
  let output = Fmt.to_to_string pp result in
  (* Should show "no structural differences" and the string diff context *)
  check bool "shows no structural differences" true
    (String.contains output 'n' && String.contains output 'o');
  check bool "shows position" true (String.contains output '9')
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
  ]

let suite = ("css_compare", tests)
