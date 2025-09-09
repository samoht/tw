(** Tests for CSS Optimize module *)

open Alcotest
open Css.Optimize
open Css.Declaration
open Css.Values
open Css.Properties

let hex_color s = Hex { hash = true; value = s }
let to_string pp v = Css.Pp.to_string ~minify:true pp v

(* Generic check function for optimize types *)
let check_value name pp reader ?expected input =
  let expected = Option.value ~default:input expected in
  (* First pass: parse + print equals expected (minified) *)
  let t = Css.Reader.of_string input in
  let v = reader t in
  let s = to_string pp v in
  check string (Fmt.str "%s %s" name input) expected s;
  (* Roundtrip stability: read printed output and ensure idempotent printing *)
  let t2 = Css.Reader.of_string s in
  let v2 = reader t2 in
  let s2 = to_string pp v2 in
  check string (Fmt.str "roundtrip %s %s" name input) s s2

(** Test declaration deduplication *)
let test_deduplicate_declarations () =
  (* Test case: later declaration wins *)
  let decls =
    [
      declaration Color (hex_color "ff0000");
      declaration Color (hex_color "0000ff");
    ]
  in
  let deduped = deduplicate_declarations decls in
  check int "single color property remains" 1 (List.length deduped);

  (* Test case: !important wins over normal *)
  let decls_important =
    [
      declaration Color (hex_color "ff0000");
      declaration ~important:true Color (hex_color "00ff00");
      declaration Color (hex_color "0000ff");
    ]
  in
  let deduped_important = deduplicate_declarations decls_important in
  check int "single color property remains" 1 (List.length deduped_important);
  (match List.hd deduped_important with
  | Declaration { important = true; _ } -> ()
  | _ -> fail "Expected !important declaration to win");

  (* Test case: custom properties *)
  let custom_decls =
    [
      custom_property "--color1" "red";
      custom_property "--color1" "blue";
      custom_property "--color2" "green";
    ]
  in
  let deduped_custom = deduplicate_declarations custom_decls in
  check int "two custom properties remain" 2 (List.length deduped_custom)

(** Test buggy property duplication *)
let test_duplicate_buggy_properties () =
  (* Test -webkit-transform duplication *)
  let decls = [ declaration Transform [ Rotate (Deg 45.) ] ] in
  let duplicated = duplicate_buggy_properties decls in
  (* Should add -webkit-transform *)
  check bool "adds webkit prefix" true
    (List.length duplicated > List.length decls)

(** Test rule optimization *)
let test_optimize_single_rule () =
  let selector = Css.Selector.class_ "test" in
  let decls =
    [
      declaration Color (hex_color "ff0000");
      declaration Color (hex_color "0000ff");
      declaration Background_color (hex_color "ffffff");
    ]
  in
  let rule : Css.Stylesheet.rule = { selector; declarations = decls } in
  let optimized = optimize_single_rule rule in

  (* Check that duplicate color declarations are removed *)
  let color_count =
    List.fold_left
      (fun acc decl ->
        match decl with
        | Declaration { property = Color; _ } -> acc + 1
        | _ -> acc)
      0 optimized.declarations
  in
  check int "only one color declaration remains" 1 color_count

(** Test rule merging *)
let test_merge_rules () =
  let selector = Css.Selector.class_ "test" in
  let rule1 : Css.Stylesheet.rule =
    { selector; declarations = [ declaration Color (hex_color "ff0000") ] }
  in
  let rule2 : Css.Stylesheet.rule =
    {
      selector;
      declarations = [ declaration Background_color (hex_color "0000ff") ];
    }
  in

  let merged = merge_rules [ rule1; rule2 ] in
  check int "rules with same selector are merged" 1 (List.length merged);

  let merged_rule = List.hd merged in
  check int "merged rule has both declarations" 2
    (List.length merged_rule.declarations)

(** Test selector grouping *)
let test_group_selectors () =
  let decls = [ declaration Color (hex_color "ff0000") ] in
  let rule1 : Css.Stylesheet.rule =
    { selector = Css.Selector.class_ "a"; declarations = decls }
  in
  let rule2 : Css.Stylesheet.rule =
    { selector = Css.Selector.class_ "b"; declarations = decls }
  in
  let rule3 : Css.Stylesheet.rule =
    { selector = Css.Selector.class_ "c"; declarations = decls }
  in

  (* Test combine_identical_rules function *)
  let grouped = combine_identical_rules [ rule1; rule2; rule3 ] in
  check int "rules with same declarations are grouped" 1 (List.length grouped);

  (* Check that selector is a list *)
  let grouped_rule = List.hd grouped in
  check bool "grouped selector is a list" true
    (Css.Selector.is_compound_list grouped_rule.selector)

(** Test complete stylesheet optimization *)
let test_optimize () =
  let selector1 = Css.Selector.class_ "test" in
  let selector2 = Css.Selector.class_ "other" in

  let rule1 : Css.Stylesheet.rule =
    {
      selector = selector1;
      declarations =
        [
          declaration Color (hex_color "ff0000");
          declaration Color (hex_color "0000ff");
        ];
    }
  in
  let rule2 : Css.Stylesheet.rule =
    {
      selector = selector1;
      declarations = [ declaration Background_color (hex_color "ffffff") ];
    }
  in
  let rule3 : Css.Stylesheet.rule =
    {
      selector = selector2;
      declarations = [ declaration Color (hex_color "0000ff") ];
    }
  in

  let stylesheet =
    { Css.Stylesheet.empty with rules = [ rule1; rule2; rule3 ] }
  in

  let optimized = optimize stylesheet in

  (* Should merge rule1 and rule2 since they have same selector *)
  check bool "optimization reduces rule count" true
    (List.length optimized.rules < List.length stylesheet.rules)

(** Test media query optimization *)
let test_optimize_media_queries () =
  let selector = Css.Selector.class_ "test" in
  let rule : Css.Stylesheet.rule =
    {
      selector;
      declarations =
        [
          declaration Color (hex_color "ff0000");
          declaration Color (hex_color "0000ff");
        ];
    }
  in

  let media_rule : Css.Stylesheet.media_rule =
    { media_condition = "screen"; media_rules = [ rule ] }
  in

  let stylesheet =
    { Css.Stylesheet.empty with media_queries = [ media_rule ] }
  in

  let optimized = optimize stylesheet in

  (* Check that declarations within media queries are also deduplicated *)
  let optimized_media = List.hd optimized.media_queries in
  let optimized_rule = List.hd optimized_media.media_rules in
  let color_count =
    List.fold_left
      (fun acc decl ->
        match decl with
        | Declaration { property = Color; _ } -> acc + 1
        | _ -> acc)
      0 optimized_rule.declarations
  in
  check int "media rule declarations are deduplicated" 1 color_count

(** Test layer optimization *)
let test_optimize_layers () =
  let selector = Css.Selector.class_ "test" in
  let rule : Css.Stylesheet.rule =
    {
      selector;
      declarations =
        [
          declaration Color (hex_color "ff0000");
          declaration Color (hex_color "0000ff");
        ];
    }
  in

  let layer : Css.Stylesheet.layer_rule =
    {
      layer = "utilities";
      rules = [ Css.Stylesheet.Rule rule ];
      media_queries = [];
      container_queries = [];
      supports_queries = [];
    }
  in

  let stylesheet = { Css.Stylesheet.empty with layers = [ layer ] } in

  let optimized = optimize stylesheet in

  (* Check that layer rules are optimized *)
  let optimized_layer = List.hd optimized.layers in
  match List.hd optimized_layer.rules with
  | Css.Stylesheet.Rule optimized_rule ->
      let color_count =
        List.fold_left
          (fun acc decl ->
            match decl with
            | Declaration { property = Color; _ } -> acc + 1
            | _ -> acc)
          0 optimized_rule.declarations
      in
      check int "layer rule declarations are deduplicated" 1 color_count
  | _ -> fail "Expected Rule in layer"

let optimize_tests =
  [
    ("deduplicate declarations", `Quick, test_deduplicate_declarations);
    ("duplicate buggy properties", `Quick, test_duplicate_buggy_properties);
    ("optimize single rule", `Quick, test_optimize_single_rule);
    ("merge rules", `Quick, test_merge_rules);
    ("group selectors", `Quick, test_group_selectors);
    ("optimize stylesheet", `Quick, test_optimize);
    ("optimize media queries", `Quick, test_optimize_media_queries);
    ("optimize layers", `Quick, test_optimize_layers);
  ]

let suite = [ ("optimize", optimize_tests) ]
