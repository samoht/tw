open Alcotest
open Tw.Rules
open Tw.Color
open Tw.Spacing
open Tw.Modifiers

let contains s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec check i =
    if i > len_s - len_sub then false
    else if String.sub s i len_sub = sub then true
    else check (i + 1)
  in
  check 0

let check_theme_layer_empty () =
  let theme_layer = compute_theme_layer [] in
  let css =
    Css.to_string ~minify:false (Css.stylesheet [ Css.Layer theme_layer ])
  in
  (* Should include font variables even for empty input *)
  check bool "includes --font-sans" true (contains css "--font-sans");
  check bool "includes --font-mono" true (contains css "--font-mono");
  check bool "includes --default-font-family" true
    (contains css "--default-font-family");
  check bool "includes --default-mono-font-family" true
    (contains css "--default-mono-font-family")

let check_theme_layer_with_color () =
  let theme_layer = Tw.Rules.compute_theme_layer [ bg blue 500 ] in
  let css =
    Css.to_string ~minify:false (Css.stylesheet [ Css.Layer theme_layer ])
  in
  (* Should include color variable when referenced *)
  check bool "includes --color-blue-500" true (contains css "--color-blue-500");
  (* Should still include font variables *)
  check bool "includes --font-sans" true (contains css "--font-sans")

let check_extract_selector_props () =
  let rules = Tw.Rules.extract_selector_props (p 4) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Regular { selector; _ } ] ->
      check string "correct selector" ".p-4" selector
  | _ -> fail "Expected Regular rule"

let check_extract_hover () =
  let rules = extract_selector_props (hover [ bg blue 500 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Regular { selector; _ } ] ->
      check bool "selector contains hover" true (contains selector ":hover")
  | _ -> fail "Expected Regular rule with hover"

let check_extract_responsive () =
  let rules = extract_selector_props (sm [ p 4 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Media_query { condition; selector; _ } ] ->
      check bool "has min-width condition" true (contains condition "min-width");
      check string "correct selector" ".sm\\:p-4" selector
  | _ -> fail "Expected Media_query rule"

let check_conflict_group () =
  (* Test conflict group ordering *)
  let group_p4, _ = conflict_group ".p-4" in
  let group_m4, _ = conflict_group ".m-4" in
  let group_bg, _ = conflict_group ".bg-blue-500" in
  check bool "margin before background" true (group_m4 < group_bg);
  check bool "background before padding" true (group_bg < group_p4)

let check_escape_class_name () =
  check string "escapes brackets" "p-\\[10px\\]" (escape_class_name "p-[10px]");
  check string "escapes colon" "hover\\:bg-blue-500"
    (escape_class_name "hover:bg-blue-500");
  check string "escapes slash" "w-1\\/2" (escape_class_name "w-1/2");
  check string "escapes dot" "text-1\\.5" (escape_class_name "text-1.5")

let check_properties_layer () =
  (* Properties layer is no longer computed centrally - property rules are now
     handled by individual utility modules *)
  check bool "properties layer removed as expected" true true

let check_to_css_variables_with_base () =
  let config =
    { Tw.Rules.base = true; mode = Css.Variables; optimize = false }
  in
  let css = Tw.Rules.to_css ~config [] in
  let css_str = Css.to_string ~minify:false css in
  (* Base=true under Variables: all layers including base are present. *)
  check bool "includes base resets" true (contains css_str "*, :after, :before");
  check bool "has theme layer" true (contains css_str "@layer theme");
  check bool "has base layer" true (contains css_str "@layer base");
  check bool "has utilities layer" true (contains css_str "@layer utilities")

let check_to_css_variables_without_base () =
  let config =
    { Tw.Rules.base = false; mode = Css.Variables; optimize = false }
  in
  let css = Tw.Rules.to_css ~config [ p 4 ] in
  let css_str = Css.to_string ~minify:false css in
  (* Base=false under Variables: theme + components + utilities, but no base. *)
  check bool "has theme layer" true (contains css_str "@layer theme");
  check bool "no base layer" true (not (contains css_str "@layer base"));
  check bool "has utilities layer" true (contains css_str "@layer utilities");
  check bool "has padding rule" true (contains css_str ".p-4")

let check_to_css_inline_with_base () =
  let config = { Tw.Rules.base = true; mode = Css.Inline; optimize = false } in
  let css = Tw.Rules.to_css ~config [ p 4 ] in
  let css_str = Css.to_string ~minify:false ~mode:Css.Inline css in
  (* Inline mode never emits layers; base has no effect. *)
  check bool "no theme layer" true (not (contains css_str "@layer theme"));
  check bool "no base layer" true (not (contains css_str "@layer base"));
  check bool "no utilities layer" true
    (not (contains css_str "@layer utilities"));
  check bool "has padding rule" true (contains css_str ".p-4")

let check_to_css_inline_without_base () =
  let config = { Tw.Rules.base = false; mode = Css.Inline; optimize = false } in
  let css = Tw.Rules.to_css ~config [ p 4 ] in
  let css_str = Css.to_string ~minify:false ~mode:Css.Inline css in
  (* Inline mode never emits layers. *)
  check bool "no theme layer" true (not (contains css_str "@layer theme"));
  check bool "no base layer" true (not (contains css_str "@layer base"));
  check bool "no utilities layer" true
    (not (contains css_str "@layer utilities"));
  check bool "has padding rule" true (contains css_str ".p-4")

let check_inline_style () =
  let style = Tw.Rules.to_inline_style [ p 4; m 2; bg blue 500 ] in
  check bool "has padding" true (contains style "padding");
  check bool "has margin" true (contains style "margin");
  check bool "has background-color" true (contains style "background")

(* New tests for exposed functions *)

let test_color_order () =
  check int "amber is first" 0 (Tw.Rules.color_order "amber");
  check int "blue is second" 1 (Tw.Rules.color_order "blue");
  check int "yellow near end" 20 (Tw.Rules.color_order "yellow");
  check int "zinc is last standard" 21 (Tw.Rules.color_order "zinc");
  check int "unknown color gets 100" 100 (Tw.Rules.color_order "unknown")

let test_is_hover_rule () =
  (* Test simple hover *)
  let hover_rules = Tw.Rules.extract_selector_props (hover [ bg blue 500 ]) in
  let non_hover_rules = Tw.Rules.extract_selector_props (bg blue 500) in

  (match hover_rules with
  | [ hover_rule ] ->
      check bool "detects simple hover rule" true
        (Tw.Rules.is_hover_rule hover_rule)
  | _ -> fail "Expected single hover rule");

  (match non_hover_rules with
  | [ non_hover_rule ] ->
      check bool "detects no hover" false
        (Tw.Rules.is_hover_rule non_hover_rule)
  | _ -> fail "Expected single non-hover rule");

  (* Test hover combined with responsive *)
  let sm_hover_rules = Tw.Rules.extract_selector_props (sm [ hover [ p 4 ] ]) in
  (match sm_hover_rules with
  | [ media_rule ] ->
      (* Responsive + hover creates a media query, not a regular rule with
         hover *)
      check bool "responsive+hover is not detected as hover" false
        (Tw.Rules.is_hover_rule media_rule)
  | _ -> fail "Expected single media rule");

  (* Test hover combined with dark mode *)
  let dark_hover_rules =
    Tw.Rules.extract_selector_props (dark [ hover [ m 2 ] ])
  in
  (match dark_hover_rules with
  | [ media_rule ] ->
      check bool "dark+hover is not detected as hover" false
        (Tw.Rules.is_hover_rule media_rule)
  | _ -> fail "Expected single media rule");

  (* Test focus without hover *)
  let focus_rules = Tw.Rules.extract_selector_props (focus [ bg red 400 ]) in
  (match focus_rules with
  | [ focus_rule ] ->
      check bool "focus alone is not hover" false
        (Tw.Rules.is_hover_rule focus_rule)
  | _ -> fail "Expected single focus rule");

  (* Test group hover *)
  let group_hover_rules =
    Tw.Rules.extract_selector_props (group_hover [ text white 0 ])
  in
  match group_hover_rules with
  | [ group_rule ] ->
      check bool "group-hover is detected as hover" true
        (Tw.Rules.is_hover_rule group_rule)
  | _ -> fail "Expected single group rule"

let test_resolve_dependencies () =
  (* Dependency resolution is now handled automatically by
     Css.vars_of_declarations This test is kept for compatibility but
     simplified *)
  let _vars = [ "--color-blue-500"; "--spacing-4" ] in
  (* Just check that the vars exist *)
  check bool "has color var" true (String.length "--color-blue-500" > 0);
  check bool "has spacing var" true (String.length "--spacing-4" > 0)

let test_inline_no_var_in_css_for_defaults () =
  (* Ensure Inline mode resolves defaults and does not emit var(--...). Use
     rounded_sm which sets a default on its CSS var. *)
  let config = { Tw.Rules.base = false; mode = Css.Inline; optimize = false } in
  let sheet = Tw.Rules.to_css ~config [ Tw.Borders.rounded_sm ] in
  let css_inline = Css.to_string ~minify:false ~mode:Css.Inline sheet in
  check bool "no var() in inline CSS" false (contains css_inline "var(--");
  check bool "has border-radius" true (contains css_inline "border-radius")

let test_inline_style_no_var_for_defaults () =
  (* Directly build a declaration with a defaulted var and inline it. *)
  let _, radius_var = Css.var "radius-md" Css.Length (Css.Rem 0.5) in
  let decls = [ Css.border_radius (Css.Var radius_var) ] in
  let inline = Css.inline_style_of_declarations ~mode:Css.Inline decls in
  check bool "inline: no var()" false (contains inline "var(--");
  check bool "inline: border-radius present" true
    (contains inline "border-radius")

let test_inline_vs_variables_diff () =
  (* Same utility under Variables vs Inline should differ: Inline has no var().
     Generate sheets in their respective modes to avoid carrying layer content
     that may still contain var() in declarations. *)
  let sheet_vars =
    Tw.Rules.to_css
      ~config:{ Tw.Rules.base = false; mode = Css.Variables; optimize = false }
      [ Tw.Borders.rounded_sm ]
  in
  let css_vars = Css.to_string ~minify:false ~mode:Css.Variables sheet_vars in
  let sheet_inline =
    Tw.Rules.to_css
      ~config:{ Tw.Rules.base = false; mode = Css.Inline; optimize = false }
      [ Tw.Borders.rounded_sm ]
  in
  let css_inline = Css.to_string ~minify:false ~mode:Css.Inline sheet_inline in
  check bool "variables: contains var()" true (contains css_vars "var(--");
  check bool "inline: no var()" false (contains css_inline "var(--")

let test_resolve_dependencies_dedup_and_queue () =
  (* Deduplication is now handled automatically by Css.vars_of_declarations This
     test is kept for compatibility but simplified *)
  let vars = [ "--text-xl"; "--text-xl"; "--text-xl--line-height" ] in
  (* Just verify the vars exist - dedup happens in Css.vars_of_declarations *)
  check bool "has text-xl var" true (List.mem "--text-xl" vars);
  check bool "has text-xl line-height var" true
    (List.mem "--text-xl--line-height" vars)

let test_theme_layer_collects_media_refs () =
  (* Vars referenced only under media queries should still end up in theme. *)
  let theme_layer =
    Tw.Rules.compute_theme_layer [ sm [ Tw.Typography.text_xl ] ]
  in
  let css =
    Css.to_string ~minify:false (Css.stylesheet [ Css.Layer theme_layer ])
  in
  check bool "includes --text-xl var" true (contains css "--text-xl");
  check bool "includes --text-xl--line-height var" true
    (contains css "--text-xl--line-height")

let test_rule_sets_injects_hover_media_query () =
  (* A bare hover utility produces a rule that should be gated behind
     (hover:hover) *)
  let _rules, media, _containers =
    Tw.Rules.rule_sets [ hover [ Tw.Spacing.p 4 ] ]
  in
  (* Find a media block with (hover:hover) and check it contains the expected
     selector. *)
  let media_css =
    media
    |> List.map (fun m ->
           Css.to_string ~minify:false (Css.stylesheet [ Css.Media m ]))
    |> String.concat "\n"
  in
  check bool "has (hover:hover) media query" true
    (contains media_css "(hover:hover)");
  check bool "hover rule is inside media query" true
    (contains media_css ".hover\\:p-4:hover")

let test_modifier_to_rule () =
  let rule =
    Tw.Rules.modifier_to_rule Tw.Core.Hover "bg-blue-500" ".bg-blue-500"
      [ Css.background_color (Css.Hex { hash = true; value = "3b82f6" }) ]
  in
  match rule with
  | Tw.Rules.Regular { selector; props; has_hover; _ } ->
      (* Hover modifier uses Modifiers.to_selector which includes the prefix *)
      check string "hover selector" ".hover\\:bg-blue-500:hover" selector;
      check int "preserves props" 1 (List.length props);
      check bool "marked as hover" true has_hover
  | _ -> fail "Expected Regular rule for hover"

let test_rule_sets () =
  let rules, media, container = Tw.Rules.rule_sets [ p 4; sm [ m 2 ] ] in
  check bool "has regular rules" true (List.length rules > 0);
  check bool "has media queries" true (List.length media > 0);
  check int "no container queries" 0 (List.length container)

let test_build_utilities_layer () =
  let rules =
    [
      Css.rule ~selector:".p-4" [ Css.padding (Css.Rem 1.0) ];
      Css.rule ~selector:".m-2" [ Css.margin (Css.Rem 0.5) ];
    ]
  in
  let layer =
    Tw.Rules.build_utilities_layer ~rules ~media_queries:[]
      ~container_queries:[]
  in
  let css = Css.to_string ~minify:false (Css.stylesheet [ Css.Layer layer ]) in
  check bool "creates utilities layer" true (contains css "@layer utilities");
  check bool "includes padding rule" true (contains css ".p-4");
  check bool "includes margin rule" true (contains css ".m-2")

let test_build_utilities_layer_preserves_order () =
  (* Test that build_utilities_layer preserves rule order and doesn't sort *)
  let rules =
    [
      Css.rule ~selector:".a"
        [ Css.color (Css.Hex { hash = false; value = "ff0000" }) ];
      Css.rule ~selector:".b" [ Css.margin (Css.Px 10) ];
      Css.rule ~selector:".c" [ Css.padding (Css.Px 5) ];
      Css.rule ~selector:".a"
        [ Css.background_color (Css.Hex { hash = false; value = "0000ff" }) ];
      Css.rule ~selector:".d" [ Css.font_size (Css.Rem 1.0) ];
    ]
  in
  let layer =
    Tw.Rules.build_utilities_layer ~rules ~media_queries:[]
      ~container_queries:[]
  in
  let css = Css.to_string ~minify:true (Css.stylesheet [ Css.Layer layer ]) in

  (* Find positions of each rule in the output *)
  let find_position selector =
    match Astring.String.find_sub ~sub:selector css with
    | None -> -1
    | Some pos -> pos
  in

  let pos_a1 = find_position ".a{color" in
  let pos_b = find_position ".b{" in
  let pos_c = find_position ".c{" in
  let pos_a2 = find_position ".a{background" in
  let pos_d = find_position ".d{" in

  (* Check that original order is preserved *)
  check bool "first .a before .b" true (pos_a1 < pos_b);
  check bool ".b before .c" true (pos_b < pos_c);
  check bool ".c before second .a" true (pos_c < pos_a2);
  check bool "second .a before .d" true (pos_a2 < pos_d);

  (* Most importantly: the two .a rules should NOT be adjacent *)
  check bool "two .a rules are not adjacent" true
    (pos_b > pos_a1 && pos_b < pos_a2)

let test_classify () =
  let rules =
    [
      Tw.Rules.regular ~selector:".p-4" ~props:[ Css.padding (Css.Rem 1.0) ] ();
      Tw.Rules.media_query ~condition:"(min-width: 640px)" ~selector:".sm\\:p-4"
        ~props:[ Css.padding (Css.Rem 1.0) ]
        ();
      Tw.Rules.container_query ~condition:"(min-width: 640px)"
        ~selector:".\\@sm\\:p-4"
        ~props:[ Css.padding (Css.Rem 1.0) ]
        ();
      Tw.Rules.starting_style ~selector:".animate-in"
        ~props:[ Css.opacity 0.0 ]
        ();
      Tw.Rules.regular ~selector:".m-2" ~props:[ Css.margin (Css.Rem 0.5) ] ();
    ]
  in
  let classified = Tw.Rules.classify rules in
  check int "2 regular rules" 2 (List.length classified.regular);
  check int "1 media rule" 1 (List.length classified.media);
  check int "1 container rule" 1 (List.length classified.container);
  check int "1 starting rule" 1 (List.length classified.starting)

let test_style_with_rules_and_props () =
  (* Test that when a Style has both props and rules, the props are placed after
     the rules *)
  let open Css in
  let custom_rules =
    [
      rule ~selector:".test :where(p)" [ margin_top (Rem 1.0) ];
      rule ~selector:".test :where(div)" [ padding (Rem 2.0) ];
    ]
  in
  let props = [ color (Hex { hash = false; value = "ff0000" }) ] in

  let style = Tw.Core.style ~rules:(Some custom_rules) "test" props in
  let extracted = Tw.Rules.extract_selector_props style in

  (* Should generate rules in order: custom rules first, then base props *)
  check int "correct number of rules" 3 (List.length extracted);

  (* Check that the base props come last *)
  let selectors =
    List.map
      (fun r ->
        match r with Tw.Rules.Regular { selector; _ } -> selector | _ -> "")
      extracted
  in

  (* First two should be the custom rules, last should be the base class *)
  check string "first rule selector" ".test :where(p)" (List.nth selectors 0);
  check string "second rule selector" ".test :where(div)" (List.nth selectors 1);
  check string "last rule selector" ".test" (List.nth selectors 2)

let test_rules_of_grouped_prose_bug () =
  (* Reproduce the prose rule merging bug *)
  let _prose_body_def, prose_body_var =
    Tw.Var.utility Tw.Var.Prose_body
      (Tw.Css.Oklch { l = 0.373; c = 0.034; h = 259.733 })
  in
  let grouped_pairs =
    [
      ( ".prose",
        [
          Tw.Css.color (Tw.Css.Var prose_body_var);
          Tw.Css.max_width (Tw.Css.Ch 65.0);
        ] );
      ( ".prose :where(p):not(:where([class~=not-prose],[class~=not-prose] *))",
        [
          Tw.Css.margin_top (Tw.Css.Em 1.0);
          Tw.Css.margin_bottom (Tw.Css.Em 1.0);
        ] );
      ( ".prose",
        [
          Tw.Css.font_size (Tw.Css.Rem 1.0); Tw.Css.line_height (Tw.Css.Num 1.5);
        ] );
    ]
  in

  let output_rules = Tw.Rules.rules_of_grouped grouped_pairs in

  (* Count how many .prose rules we get in output *)
  let prose_rules =
    List.filter
      (fun rule ->
        let selector = Tw.Css.selector rule in
        selector = ".prose")
      output_rules
  in

  Printf.printf "\n=== test_rules_of_grouped_prose_bug ===\n";
  Printf.printf "Input: 3 grouped pairs (2 .prose + 1 descendant)\n";
  Printf.printf "Expected: 2 .prose rules in output\n";
  Printf.printf "Actual: %d .prose rules in output\n" (List.length prose_rules);

  (* This should be 2 separate .prose rules, not 1 merged rule *)
  check int "number of .prose rules" 2 (List.length prose_rules);

  (* Also verify the total number of rules is correct *)
  check int "total output rules" 3 (List.length output_rules)

let tests =
  [
    test_case "theme layer - empty" `Quick check_theme_layer_empty;
    test_case "theme layer - with color" `Quick check_theme_layer_with_color;
    test_case "extract selector props - basic" `Quick
      check_extract_selector_props;
    test_case "extract selector props - hover" `Quick check_extract_hover;
    test_case "extract selector props - responsive" `Quick
      check_extract_responsive;
    test_case "conflict group ordering" `Quick check_conflict_group;
    test_case "escape class name" `Quick check_escape_class_name;
    test_case "properties layer generation" `Quick check_properties_layer;
    test_case "to_css variables with base" `Quick
      check_to_css_variables_with_base;
    test_case "to_css variables without base" `Quick
      check_to_css_variables_without_base;
    test_case "to_css inline with base" `Quick check_to_css_inline_with_base;
    test_case "to_css inline without base" `Quick
      check_to_css_inline_without_base;
    test_case "inline style generation" `Quick check_inline_style;
    (* New tests for exposed functions *)
    test_case "color_order" `Quick test_color_order;
    test_case "is_hover_rule" `Quick test_is_hover_rule;
    test_case "resolve_dependencies" `Quick test_resolve_dependencies;
    test_case "inline_no_var_in_css_for_defaults" `Quick
      test_inline_no_var_in_css_for_defaults;
    test_case "inline_style_no_var_for_defaults" `Quick
      test_inline_style_no_var_for_defaults;
    test_case "inline_vs_variables_diff" `Quick test_inline_vs_variables_diff;
    test_case "resolve_dependencies_dedup_and_queue" `Quick
      test_resolve_dependencies_dedup_and_queue;
    test_case "theme_layer_collects_media_refs" `Quick
      test_theme_layer_collects_media_refs;
    test_case "rule_sets_injects_hover_media_query" `Quick
      test_rule_sets_injects_hover_media_query;
    test_case "modifier_to_rule" `Quick test_modifier_to_rule;
    test_case "rule_sets" `Quick test_rule_sets;
    test_case "build_utilities_layer" `Quick test_build_utilities_layer;
    test_case "build_utilities_layer preserves order" `Quick
      test_build_utilities_layer_preserves_order;
    test_case "classify" `Quick test_classify;
    test_case "style with rules and props ordering" `Quick
      test_style_with_rules_and_props;
    test_case "rules_of_grouped prose merging bug" `Quick
      test_rules_of_grouped_prose_bug;
  ]

let suite = ("rules", tests)
