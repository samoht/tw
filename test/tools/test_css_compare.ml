open Alcotest
module CC = Tw_tools.Css_compare
open CC

(* Alcotest testables *)
let testable_diff = Alcotest.testable CC.pp CC.equal

let pp_css fmt css =
  match Css.of_string css with
  | Ok ast -> Fmt.string fmt (Css.to_string ~minify:true ast)
  | Error e -> Fmt.pf fmt "<parse error: %s>" (Css.pp_parse_error e)

let css = Alcotest.testable pp_css CC.compare_css

let test_strip_header () =
  let css = "/*! header */\n.a{color:red}" in
  check string "header stripped" ".a{color:red}" (CC.strip_header css)

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
    match CC.diff ~expected:css_expected ~actual:css_actual with
    | CC.Diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  let has_added_c =
    List.exists
      (fun r ->
        r.selector = ".c" && match r.change with CC.Added -> true | _ -> false)
      diff.rules
  in
  let has_removed_b =
    List.exists
      (fun r ->
        r.selector = ".b"
        && match r.change with CC.Removed -> true | _ -> false)
      diff.rules
  in
  let has_modified_a =
    List.exists
      (fun r ->
        r.selector = ".a"
        && match r.change with CC.Modified _ -> true | _ -> false)
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
    match CC.diff ~expected:css_expected ~actual:css_actual with
    | CC.Diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  (* Structural expectations: exactly one media Added and one layer Added, no
     rule diffs *)
  check int "no top-level rule diffs" 0 (List.length diff.rules);
  check int "one media change" 1 (List.length diff.media);
  check int "one layer change" 1 (List.length diff.layers);
  let m = List.hd diff.media in
  (match m.change with
  | CC.Modified _ -> ()
  | _ -> fail "media not marked Modified");
  (* Within media, the rule for .m should be Modified due to margin change *)
  let media_rules = m.rules in
  let mr = List.find_opt (fun r -> r.selector = ".m") media_rules in
  (match mr with
  | Some r -> (
      match r.change with CC.Modified _ -> () | _ -> fail ".m not Modified")
  | None -> fail "missing .m rule diff");
  let l = List.hd diff.layers in
  match l.change with CC.Removed -> () | _ -> fail "layer not marked Removed"

let test_property_value_modified () =
  let css_expected = ".x{color:red}" in
  let css_actual = ".x{color:blue}" in
  let diff =
    match CC.diff ~expected:css_expected ~actual:css_actual with
    | CC.Diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  let expected_diff : CC.t =
    {
      rules =
        [
          {
            selector = ".x";
            change =
              CC.Modified
                [
                  {
                    CC.property = "color";
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
    }
  in
  check testable_diff "exact property value diff" expected_diff diff

let test_property_added_only () =
  let css_expected = ".y{color:red}" in
  let css_actual = ".y{color:red;padding:10px}" in
  let diff =
    match CC.diff ~expected:css_expected ~actual:css_actual with
    | CC.Diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  let has_modified_y =
    List.exists
      (fun r ->
        r.selector = ".y"
        && match r.change with CC.Modified _ -> true | _ -> false)
      diff.rules
  in
  check bool "modified .y" true has_modified_y

let test_important_and_custom_props () =
  (* Importance difference should be detected *)
  let d1 =
    match
      CC.diff ~expected:".x{color:red!important}" ~actual:".x{color:red}"
    with
    | CC.Diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  let has_mod_imp =
    List.exists
      (fun r ->
        r.selector = ".x"
        && match r.change with CC.Modified _ -> true | _ -> false)
      d1.rules
  in
  check bool "importance change => modified" true has_mod_imp;
  (* Custom property value difference should be detected *)
  let d2 =
    match CC.diff ~expected:".x{--foo:1}" ~actual:".x{--foo:2}" with
    | CC.Diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  let has_mod_custom =
    List.exists
      (fun r ->
        r.selector = ".x"
        && match r.change with CC.Modified _ -> true | _ -> false)
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
    match CC.diff ~expected:css_expected ~actual:css_actual with
    | CC.Diff d -> d
    | _ -> failwith "Both CSS should parse"
  in
  (* Media/layers may be empty; we look for structural differences in supports/containers implicitly via rules diff absence *)
  (* Ensure no top-level rule diffs pollute this case *)
  check int "no top-level rule diffs" 0 (List.length diff.rules);
  check int "one supports change" 1 (List.length diff.supports);
  check int "one container change" 1 (List.length diff.containers)

(* Intentionally avoid substring/count-based checks; use structural diff
   only. *)

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
  ]

let suite = ("css_compare", tests)
