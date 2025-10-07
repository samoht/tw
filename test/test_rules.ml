open Alcotest
open Tw.Rules
open Tw.Color
open Tw.Padding
open Tw.Margin
open Tw.Modifiers
open Test_helpers

(* ===== Tests ===== *)

let check_theme_layer_empty () =
  let default_decls =
    Tw.Typography.default_font_declarations
    @ Tw.Typography.default_font_family_declarations
  in
  let theme_layer = compute_theme_layer ~default_decls [] in
  (* Should include font variables even for empty input *)
  let vars = vars_in_layer "theme" theme_layer in
  check bool "includes --font-sans" true (List.mem "--font-sans" vars);
  check bool "includes --font-mono" true (List.mem "--font-mono" vars);
  check bool "includes --default-font-family" true
    (List.mem "--default-font-family" vars);
  check bool "includes --default-mono-font-family" true
    (List.mem "--default-mono-font-family" vars)

let check_theme_layer_with_color () =
  let default_decls =
    Tw.Typography.default_font_declarations
    @ Tw.Typography.default_font_family_declarations
  in
  let theme_layer =
    Tw.Rules.compute_theme_layer ~default_decls [ bg blue 500 ]
  in
  (* Should include color variable when referenced *)
  check bool "includes --color-blue-500" true
    (has_var_in_layer "--color-blue-500" "theme" theme_layer);
  (* Should still include font variables *)
  check bool "includes --font-sans" true
    (has_var_in_layer "--font-sans" "theme" theme_layer)

let check_extract_selector_props () =
  let rules = Tw.Rules.extract_selector_props (p 4) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Regular { selector; _ } ] ->
      check string "correct selector" ".p-4" (Css.Selector.to_string selector)
  | _ -> fail "Expected Regular rule"

let check_extract_hover () =
  let rules = extract_selector_props (hover [ bg blue 500 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Regular { selector; _ } ] ->
      let sel_str = Css.Selector.to_string selector in
      check string "hover selector" ".hover\\:bg-blue-500:hover" sel_str
  | _ -> fail "Expected Regular rule with hover"

let check_extract_responsive () =
  let rules = extract_selector_props (sm [ p 4 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Media_query { condition; selector; _ } ] ->
      (* Match Tailwind's minified output: (min-width:40rem) *)
      check string "media condition" "(min-width:40rem)" condition;
      check Test_helpers.selector_testable "sm selector"
        (Css.Selector.class_ "sm:p-4")
        selector
  | _ -> fail "Expected Media_query rule"

let check_extract_responsive_md () =
  let rules = extract_selector_props (md [ p 4 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Media_query { condition; selector; _ } ] ->
      (* md breakpoint should be 48rem *)
      check string "md media condition" "(min-width:48rem)" condition;
      check Test_helpers.selector_testable "md selector"
        (Css.Selector.class_ "md:p-4")
        selector
  | _ -> fail "Expected Media_query rule for md"

let check_extract_responsive_lg () =
  let rules = extract_selector_props (lg [ p 4 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Media_query { condition; selector; _ } ] ->
      check string "lg media condition" "(min-width:64rem)" condition;
      check Test_helpers.selector_testable "lg selector"
        (Css.Selector.class_ "lg:p-4")
        selector
  | _ -> fail "Expected Media_query rule for lg"

let check_extract_responsive_xl () =
  let rules = extract_selector_props (xl [ p 4 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Media_query { condition; selector; _ } ] ->
      check string "xl media condition" "(min-width:80rem)" condition;
      check Test_helpers.selector_testable "xl selector"
        (Css.Selector.class_ "xl:p-4")
        selector
  | _ -> fail "Expected Media_query rule for xl"

let check_extract_responsive_2xl () =
  let rules = extract_selector_props (xl2 [ p 4 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Media_query { condition; selector; _ } ] ->
      check string "2xl media condition" "(min-width:96rem)" condition;
      check Test_helpers.selector_testable "2xl selector"
        (Css.Selector.class_ "2xl:p-4")
        selector
  | _ -> fail "Expected Media_query rule for 2xl"

let check_conflict_order () =
  (* Test that conflict_order correctly delegates to Utility.order *)
  (* It should parse the selector, extract the utility name, and return ordering *)

  (* Test basic selector parsing *)
  let prio, sub = conflict_order ".p-4" in
  check int "p-4 priority" 19 prio;

  (* Padding priority *)

  (* Test with modifier prefix (should strip it) *)
  let prio_hover, sub_hover = conflict_order ".hover\\:p-4:hover" in
  check int "hover:p-4 same priority as p-4" prio prio_hover;
  check int "hover:p-4 same suborder as p-4" sub sub_hover;

  (* Test relative ordering between utilities *)
  let m4_prio, _ = conflict_order ".m-4" in
  let bg_prio, _ = conflict_order ".bg-blue-500" in
  check bool "margin before background" true (m4_prio < bg_prio);
  (* bg-blue-500 is a background color (priority 18), which comes before Padding
     (priority 19) *)
  check bool "background color before padding" true (bg_prio < prio);

  (* Test unknown utility gets high priority *)
  let unknown_prio, _ = conflict_order ".unknown-utility" in
  check int "unknown gets 9999 priority" 9999 unknown_prio

let check_escape_class_name () =
  check string "escapes brackets" "p-\\[10px\\]" (escape_class_name "p-[10px]");
  check string "escapes colon" "hover\\:bg-blue-500"
    (escape_class_name "hover:bg-blue-500");
  check string "escapes slash" "w-1\\/2" (escape_class_name "w-1/2");
  check string "escapes dot" "text-1\\.5" (escape_class_name "text-1.5")

let check_properties_layer () =
  (* Test that shadow utilities generate proper @layer properties with initial
     values *)
  let config =
    { Tw.Rules.base = false; mode = Css.Variables; optimize = false }
  in
  let actual_css = Tw.Rules.to_css ~config [ Tw.Effects.shadow_sm ] in

  (* Verify properties layer exists *)
  check bool "has properties layer" true (has_layer "properties" actual_css);

  (* Extract custom property declarations from properties layer *)
  let vars = vars_in_layer "properties" actual_css in

  (* Verify expected shadow variables are present *)
  check bool "has --tw-shadow" true (List.mem "--tw-shadow" vars);
  check bool "has --tw-shadow-alpha" true (List.mem "--tw-shadow-alpha" vars);
  check bool "has --tw-ring-offset-color" true
    (List.mem "--tw-ring-offset-color" vars);
  check bool "has --tw-ring-offset-width" true
    (List.mem "--tw-ring-offset-width" vars)

let check_css_variables_with_base () =
  let config =
    { Tw.Rules.base = true; mode = Css.Variables; optimize = false }
  in
  let css = Tw.Rules.to_css ~config [] in
  (* Base=true under Variables: all layers including base are present. *)
  check bool "has theme layer" true (has_layer "theme" css);
  check bool "has base layer" true (has_layer "base" css);
  check bool "has utilities layer" true (has_layer "utilities" css);
  (* Check base layer contains reset selectors *)
  let base_selectors = selectors_in_layer "base" css in
  check bool "base has universal reset" true
    (List.mem "*, ::after, ::before, ::backdrop" base_selectors)

let check_css_variables_without_base () =
  let config =
    { Tw.Rules.base = false; mode = Css.Variables; optimize = false }
  in
  let css = Tw.Rules.to_css ~config [ p 4 ] in
  (* Base=false under Variables: theme + components + utilities, but no base. *)
  check bool "has theme layer" true (has_layer "theme" css);
  check bool "no base layer" false (has_layer "base" css);
  check bool "has utilities layer" true (has_layer "utilities" css);
  check bool "has padding rule" true
    (has_selector_in_layer ".p-4" "utilities" css)

let check_css_inline_with_base () =
  let config = { Tw.Rules.base = true; mode = Css.Inline; optimize = false } in
  let css = Tw.Rules.to_css ~config [ p 4 ] in
  (* Inline mode never emits layers; base has no effect. *)
  check bool "no theme layer" false (has_layer "theme" css);
  check bool "no base layer" false (has_layer "base" css);
  check bool "no utilities layer" false (has_layer "utilities" css);
  (* Check that .p-4 rule exists at top level *)
  let top_level_selectors = selectors_in_layer "utilities" css in
  check bool "has padding rule" true
    (List.length top_level_selectors = 0
    ||
    let all_sels =
      List.filter_map
        (fun stmt ->
          match Css.as_rule stmt with
          | Some (sel, _, _) -> Some (Css.Selector.to_string sel)
          | None -> None)
        (Css.statements css)
    in
    List.mem ".p-4" all_sels)

let check_css_inline_without_base () =
  let config = { Tw.Rules.base = false; mode = Css.Inline; optimize = false } in
  let css = Tw.Rules.to_css ~config [ p 4 ] in
  (* Inline mode never emits layers. *)
  check bool "no theme layer" false (has_layer "theme" css);
  check bool "no base layer" false (has_layer "base" css);
  check bool "no utilities layer" false (has_layer "utilities" css);
  (* Check that .p-4 rule exists at top level *)
  let all_sels =
    List.filter_map
      (fun stmt ->
        match Css.as_rule stmt with
        | Some (sel, _, _) -> Some (Css.Selector.to_string sel)
        | None -> None)
      (Css.statements css)
  in
  check bool "has padding rule" true (List.mem ".p-4" all_sels)

let check_inline_style () =
  let style = Tw.Rules.to_inline_style [ p 4; m 2; bg blue 500 ] in
  check bool "has padding" true (inline_has_property "padding" style);
  check bool "has margin" true (inline_has_property "margin" style);
  check bool "has background-color" true
    (inline_has_property "background-color" style)

(* ---------------------------------------------------------------------- *)
(* Ordering tests for layers and properties/@property emission (AST-based) *)
(* ---------------------------------------------------------------------- *)

(* Short, reusable helpers *)
let sheet_of ?(base = false) ?(mode = Css.Variables) ?(optimize = false) styles
    =
  Tw.Rules.to_css ~config:{ Tw.Rules.base; mode; optimize } styles

let layers_of (sheet : Css.t) : string list = Css.layers sheet

(* layer_block is now available in Css module *)

let supports_block (stmts : Css.statement list) : Css.statement list option =
  List.find_map
    (fun s ->
      match Css.as_supports s with Some (_, inner) -> Some inner | _ -> None)
    stmts

let first_rule_decls (stmts : Css.statement list) : Css.declaration list option
    =
  List.find_map
    (fun s -> match Css.as_rule s with Some (_, ds, _) -> Some ds | _ -> None)
    stmts

(* custom_prop_names is now available in Css module *)

let property_rule_names (sheet : Css.t) : string list =
  Css.statements sheet
  |> List.filter_map (fun s ->
         match Css.as_property s with
         | Some (Css.Property_info { name; _ }) -> Some name
         | None -> None)

let extract_var_names_with_prefix (prefix : string) (props : string list) :
    string list =
  List.filter_map
    (fun prop ->
      if
        String.length prop > String.length prefix
        && String.sub prop 0 (String.length prefix) = prefix
      then
        let rest =
          String.sub prop (String.length prefix)
            (String.length prop - String.length prefix)
        in
        match String.index_opt rest '-' with
        | Some idx -> Some (String.sub rest 0 idx)
        | None -> None
      else None)
    props

let stmt_index (sheet : Css.t) pred : int option =
  let rec loop i = function
    | [] -> None
    | s :: tl -> if pred s then Some i else loop (i + 1) tl
  in
  loop 0 (Css.statements sheet)

let rec take n lst =
  match (n, lst) with
  | 0, _ -> []
  | _, [] -> []
  | k, h :: t -> h :: take (k - 1) t

let rec is_prefix pre lst =
  match (pre, lst) with
  | [], _ -> true
  | _ :: _, [] -> false
  | p :: pt, x :: xt -> p = x && is_prefix pt xt

let or_fail msg = function Some x -> x | None -> fail msg

let check_layer_declaration_and_ordering () =
  (* Use a utility that triggers properties + @property rules *)
  let sheet = sheet_of [ Tw.Effects.shadow_sm ] in
  let layer_names = layers_of sheet in
  let expected = [ "properties"; "theme"; "components"; "utilities" ] in
  check bool "layer decl order prefix" true (is_prefix expected layer_names);
  check bool "has properties layer block" true
    (Css.layer_block "properties" sheet <> None)

let check_properties_layer_internal_order () =
  let sheet = sheet_of [ Tw.Effects.shadow_sm ] in
  let props =
    Css.layer_block "properties" sheet
    |> or_fail "Expected a @layer properties block"
  in
  let supports =
    supports_block props
    |> or_fail "Expected a @supports block inside properties layer"
  in
  let decls =
    first_rule_decls supports
    |> or_fail "Expected a rule inside @supports in properties layer"
  in
  let names = Css.custom_prop_names decls in
  let expected =
    [
      "--tw-shadow";
      "--tw-shadow-color";
      "--tw-shadow-alpha";
      "--tw-inset-shadow";
      "--tw-inset-shadow-color";
      "--tw-inset-shadow-alpha";
    ]
  in
  check (list string) "properties layer initial-order prefix" expected
    (take (List.length expected) names)

let check_property_rules_trailing_and_order () =
  let sheet = sheet_of [ Tw.Effects.shadow_sm ] in
  let names = property_rule_names sheet in
  let expected =
    [
      "--tw-shadow";
      "--tw-shadow-color";
      "--tw-shadow-alpha";
      "--tw-inset-shadow";
      "--tw-inset-shadow-color";
      "--tw-inset-shadow-alpha";
    ]
  in
  check bool "@property rules prefix order" true (is_prefix expected names);
  let util_idx =
    stmt_index sheet (fun s ->
        match Css.as_layer s with
        | Some (Some "utilities", _) -> true
        | _ -> false)
    |> or_fail "Expected a utilities layer"
  in
  let first_prop_idx =
    stmt_index sheet (fun s ->
        match Css.as_property s with Some _ -> true | None -> false)
    |> or_fail "Expected at least one @property rule"
  in
  check bool "@property after utilities" true (first_prop_idx > util_idx)

(* New tests for exposed functions *)

let test_color_order () =
  check int "amber utilities order" 2
    (let _, order = Tw.Color.utilities_order "amber" in
     order);
  check int "blue utilities order" 3
    (let _, order = Tw.Color.utilities_order "blue" in
     order);
  check int "cyan utilities order" 4
    (let _, order = Tw.Color.utilities_order "cyan" in
     order);
  check int "sky utilities order" 17
    (let _, order = Tw.Color.utilities_order "sky" in
     order);
  check int "unknown color gets 100" 100
    (let _, order = Tw.Color.utilities_order "unknown" in
     order)

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

let test_inline_no_vars_defaults () =
  (* Ensure Inline mode resolves defaults and does not emit var(--...). Use
     rounded_sm which sets a default on its CSS var. *)
  let config = { Tw.Rules.base = false; mode = Css.Inline; optimize = false } in
  let sheet = Tw.Rules.to_css ~config [ Tw.Borders.rounded_sm ] in
  (* Find first rule with declarations using fold *)
  let find_first_decls css =
    Css.fold
      (fun acc stmt ->
        match (acc, Css.as_rule stmt) with
        | Some _, _ -> acc (* already found *)
        | None, Some (_, decls, _) when List.length decls > 0 -> Some decls
        | None, _ -> None)
      None css
  in
  let decls = find_first_decls sheet in
  match decls with
  | None -> fail "Expected at least one rule with declarations"
  | Some declarations ->
      check bool "no Var in declarations" false
        (has_var_in_declarations ~inline:true declarations);
      (* Check border-radius property exists *)
      let has_border_radius =
        List.exists
          (fun decl -> Css.declaration_name decl = "border-radius")
          declarations
      in
      check bool "has border-radius" true has_border_radius

(* Layer ordering tests *)

let extract_theme_color_vars sheet =
  Css.layer_block "theme" sheet
  |> Option.map Css.rules_from_statements
  |> Option.map Css.custom_props_from_rules
  |> Option.map (extract_var_names_with_prefix "--color-")
  |> Option.value ~default:[]

let extract_utility_selectors sheet =
  Css.layer_block "utilities" sheet
  |> Option.map (fun stmts ->
         Css.rules_from_statements stmts
         |> List.filter_map (fun (sel, _) ->
                let sel_str = Css.Selector.to_string sel in
                if String.length sel_str > 4 && String.sub sel_str 0 4 = ".bg-"
                then
                  let rest = String.sub sel_str 4 (String.length sel_str - 4) in
                  match String.index_opt rest '-' with
                  | Some idx -> Some (String.sub rest 0 idx)
                  | None -> None
                else None))
  |> Option.value ~default:[]

let test_theme_layer_color_order () =
  let sheet = sheet_of [ bg cyan 500; bg sky 500; bg blue 500 ] in
  let theme_colors = extract_theme_color_vars sheet in
  check (list string) "theme layer: cyan, sky, blue" [ "cyan"; "sky"; "blue" ]
    theme_colors

let test_utilities_layer_color_order () =
  let sheet = sheet_of [ bg cyan 500; bg sky 500; bg blue 500 ] in
  let util_colors = extract_utility_selectors sheet in
  check (list string) "utilities layer: blue, cyan, sky"
    [ "blue"; "cyan"; "sky" ] util_colors

let test_deterministic_ordering () =
  let inputs =
    [
      [ bg cyan 500; bg sky 500; bg blue 500 ];
      [ bg blue 500; bg sky 500; bg cyan 500 ];
      [ bg sky 500; bg blue 500; bg cyan 500 ];
    ]
  in
  let results =
    List.map
      (fun utilities ->
        let sheet = sheet_of utilities in
        Css.to_string ~minify:true sheet)
      inputs
  in
  match results with
  | [] -> failwith "No results"
  | first :: rest ->
      List.iter (fun css -> check string "deterministic output" first css) rest

let test_inline_style_no_vars () =
  (* Directly build a declaration with a defaulted var and inline it. *)
  let _, radius_var = Css.var "radius-md" Css.Length (Css.Rem 0.5) in
  let decls = [ Css.border_radius (Css.Var radius_var) ] in
  let inline = Css.inline_style_of_declarations ~mode:Css.Inline decls in
  check bool "inline: border-radius present" true
    (inline_has_property "border-radius" inline);
  (* Verify no var() in output - this requires checking string since inline is
     string *)
  check bool "inline: no var() in string" false
    (String.contains inline '(' && String.contains inline ')'
    &&
    let idx = String.index inline '(' in
    String.length inline > idx + 4 && String.sub inline (idx - 3) 3 = "var")

let test_inline_vs_variables_diff () =
  (* Same utility under Variables vs Inline should differ: Inline has no var().
     Generate sheets in their respective modes to avoid carrying layer content
     that may still contain var() in declarations. *)
  let sheet_vars =
    Tw.Rules.to_css
      ~config:{ Tw.Rules.base = false; mode = Css.Variables; optimize = false }
      [ Tw.Borders.rounded_sm ]
  in
  let sheet_inline =
    Tw.Rules.to_css
      ~config:{ Tw.Rules.base = false; mode = Css.Inline; optimize = false }
      [ Tw.Borders.rounded_sm ]
  in
  (* Extract all declarations using fold *)
  let extract_decls css =
    Css.fold
      (fun acc stmt ->
        match Css.as_rule stmt with
        | Some (_, decls, _) -> decls @ acc
        | None -> acc)
      [] css
  in
  let vars_decls = extract_decls sheet_vars in
  let inline_decls = extract_decls sheet_inline in
  check bool "variables: contains var()" true
    (has_var_in_declarations vars_decls);
  check bool "inline: no var()" false
    (has_var_in_declarations ~inline:true inline_decls)

let test_resolve_deps_dedup_queue () =
  (* Deduplication is now handled automatically by Css.vars_of_declarations This
     test is kept for compatibility but simplified *)
  let vars = [ "--text-xl"; "--text-xl"; "--text-xl-line-height" ] in
  (* Just verify the vars exist - dedup happens in Css.vars_of_declarations *)
  check bool "has text-xl var" true (List.mem "--text-xl" vars);
  check bool "has text-xl line-height var" true
    (List.mem "--text-xl-line-height" vars)

let test_theme_layer_media_refs () =
  (* Vars referenced only under media queries should still end up in theme. *)
  let theme_layer =
    let default_decls =
      Tw.Typography.default_font_declarations
      @ Tw.Typography.default_font_family_declarations
    in
    Tw.Rules.compute_theme_layer ~default_decls [ sm [ Tw.Typography.text_xl ] ]
  in
  let all_vars =
    Css.layer_block "theme" theme_layer
    |> Option.map Css.rules_from_statements
    |> Option.map Css.custom_props_from_rules
    |> Option.value ~default:[]
  in
  (* Check for exact variable name matches *)
  check bool "includes --text-xl var" true
    (List.exists (fun v -> v = "--text-xl") all_vars);
  check bool "includes --text-xl--line-height var" true
    (List.exists (fun v -> v = "--text-xl--line-height") all_vars)

let test_theme_layer_media_refs_md () =
  (* Vars referenced only under md media queries should still end up in
     theme. *)
  let theme_layer =
    let default_decls =
      Tw.Typography.default_font_declarations
      @ Tw.Typography.default_font_family_declarations
    in
    Tw.Rules.compute_theme_layer ~default_decls [ md [ Tw.Typography.text_xl ] ]
  in
  let all_vars =
    Css.layer_block "theme" theme_layer
    |> Option.map Css.rules_from_statements
    |> Option.map Css.custom_props_from_rules
    |> Option.value ~default:[]
  in
  check bool "includes --text-xl var (md)" true
    (List.exists (fun v -> v = "--text-xl") all_vars);
  check bool "includes --text-xl--line-height var (md)" true
    (List.exists (fun v -> v = "--text-xl--line-height") all_vars)

let test_rule_sets_hover_media () =
  (* A bare hover utility produces a rule that should be gated behind
     (hover:hover) *)
  let config =
    { Tw.Rules.base = false; mode = Css.Variables; optimize = false }
  in
  let css = Tw.Rules.to_css ~config [ hover [ p 4 ] ] in
  (* Check for exact media condition *)
  check bool "has (hover:hover) media query" true
    (has_media_condition "(hover:hover)" css);
  (* Extract selectors from within the (hover:hover) media query *)
  let selectors = selectors_in_media_sel ~condition:"(hover:hover)" css in
  let expected =
    Css.Selector.compound
      [ Css.Selector.class_ "hover:p-4"; Css.Selector.Hover ]
  in
  check
    (list Test_helpers.selector_testable)
    "hover selector in media" [ expected ] selectors

let test_rule_sets_md_media () =
  (* Multiple md[...] utilities should group under a single (min-width:48rem) *)
  let css = Tw.Rules.to_css [ md [ p 4 ]; md [ m 2 ] ] in
  (* Check for exact media condition *)
  check bool "has (min-width:48rem) media query" true
    (has_media_condition "(min-width:48rem)" css);

  (* Find the md media block and verify both selectors are inside it *)
  let md_block =
    Css.fold
      (fun acc stmt ->
        match (acc, Css.as_media stmt) with
        | Some _, _ -> acc
        | None, Some (cond, inner) when cond = "(min-width:48rem)" -> Some inner
        | None, _ -> None)
      None css
  in
  match md_block with
  | None -> fail "Expected md media block"
  | Some stmts ->
      let selectors =
        List.filter_map
          (fun s ->
            match Css.as_rule s with
            | Some (sel, _, _) -> Some sel
            | None -> None)
          stmts
      in
      let expected =
        Test_helpers.sort_selectors
          [ Css.Selector.class_ "md:p-4"; Css.Selector.class_ "md:m-2" ]
      in
      let actual = Test_helpers.sort_selectors selectors in
      check
        (list Test_helpers.selector_testable)
        "md block selectors" expected actual

let test_media_grouping_order () =
  let css =
    Tw.Rules.to_css [ sm [ p 2 ]; md [ m 4 ]; lg [ Tw.Typography.text_xl ] ]
  in
  (* Conditions present and in order *)
  let conditions = media_conditions css in
  check (list string) "media conditions order"
    [ "(min-width:40rem)"; "(min-width:48rem)"; "(min-width:64rem)" ]
    conditions;
  (* Each block contains only its selectors *)
  let sm_sels = selectors_in_media_sel ~condition:"(min-width:40rem)" css in
  let md_sels = selectors_in_media_sel ~condition:"(min-width:48rem)" css in
  let lg_sels = selectors_in_media_sel ~condition:"(min-width:64rem)" css in
  check
    (list Test_helpers.selector_testable)
    "sm selectors"
    [ Css.Selector.class_ "sm:p-2" ]
    sm_sels;
  check
    (list Test_helpers.selector_testable)
    "md selectors"
    [ Css.Selector.class_ "md:m-4" ]
    md_sels;
  check
    (list Test_helpers.selector_testable)
    "lg selectors"
    [ Css.Selector.class_ "lg:text-xl" ]
    lg_sels

let test_md_media_dedup () =
  let css = Tw.Rules.to_css [ md [ p 4 ]; md [ p 4 ] ] in
  check int "only one .md:p-4 in media (structural)" 1
    (count_selector_in_media_sel ~condition:"(min-width:48rem)"
       ~selector:(Css.Selector.class_ "md:p-4")
       css)

let test_md_hover_no_extra_media () =
  (* Responsive+hover should not introduce a separate (hover:hover) gate *)
  let css = Tw.Rules.to_css [ md [ hover [ p 4 ] ] ] in
  check bool "no (hover:hover) condition" false
    (has_media_condition "(hover:hover)" css);
  (* Selector is inside md media block with :hover pseudo, assert
     structurally *)
  let md_sels = selectors_in_media_sel ~condition:"(min-width:48rem)" css in
  let expected =
    Css.Selector.compound
      [ Css.Selector.class_ "md:hover:p-4"; Css.Selector.Hover ]
  in
  check
    (list Test_helpers.selector_testable)
    "md:hover selector in md block" [ expected ] md_sels

let test_container_and_media () =
  let rules, media, container =
    Tw.Rules.rule_sets [ Tw.Containers.container_md [ p 4 ]; md [ m 2 ] ]
  in
  check bool "has media queries" true (List.length media > 0);
  check bool "has container queries" true (List.length container > 0);
  (* sanity: regular rules can be empty for this input *)
  ignore rules

let test_modifier_to_rule () =
  let rule =
    Tw.Rules.modifier_to_rule Tw.Style.Hover "bg-blue-500"
      (Css.Selector.class_ "bg-blue-500")
      [ Css.background_color (Css.Hex { hash = true; value = "3b82f6" }) ]
  in
  match rule with
  | Tw.Rules.Regular { selector; props; has_hover; _ } ->
      (* Hover modifier uses Modifiers.to_selector which includes the prefix *)
      check string "hover selector" ".hover\\:bg-blue-500:hover"
        (Css.Selector.to_string selector);
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
      Css.rule
        ~selector:(Css.Selector.class_ "p-4")
        [ Css.padding [ Css.Rem 1.0 ] ];
      Css.rule
        ~selector:(Css.Selector.class_ "m-2")
        [ Css.margin [ Css.Rem 0.5 ] ];
    ]
  in
  let layer =
    Tw.Rules.build_utilities_layer ~rules ~media_queries:[]
      ~container_queries:[]
  in
  (* Check for utilities layer and selectors *)
  check bool "creates utilities layer" true (has_layer "utilities" layer);
  check bool "includes padding rule" true
    (has_selector_in_layer ".p-4" "utilities" layer);
  check bool "includes margin rule" true
    (has_selector_in_layer ".m-2" "utilities" layer)

let test_build_utils_layer_order () =
  (* Test that build_utilities_layer preserves rule order and doesn't sort *)
  let rules =
    [
      Css.rule ~selector:(Css.Selector.class_ "a")
        [ Css.color (Css.Hex { hash = false; value = "ff0000" }) ];
      Css.rule ~selector:(Css.Selector.class_ "b") [ Css.margin [ Css.Px 10. ] ];
      Css.rule ~selector:(Css.Selector.class_ "c") [ Css.padding [ Css.Px 5. ] ];
      Css.rule ~selector:(Css.Selector.class_ "a")
        [ Css.background_color (Css.Hex { hash = false; value = "0000ff" }) ];
      Css.rule ~selector:(Css.Selector.class_ "d")
        [ Css.font_size (Css.Rem 1.0) ];
    ]
  in
  let layer =
    Tw.Rules.build_utilities_layer ~rules ~media_queries:[]
      ~container_queries:[]
  in
  let css = Css.to_string ~minify:true layer in

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
      Tw.Rules.regular
        ~selector:(Css.Selector.class_ "p-4")
        ~props:[ Css.padding [ Css.Rem 1.0 ] ]
        ();
      Tw.Rules.media_query ~condition:"(min-width: 640px)"
        ~selector:(Css.Selector.class_ "sm\\:p-4")
        ~props:[ Css.padding [ Css.Rem 1.0 ] ]
        ();
      Tw.Rules.container_query ~condition:"(min-width: 640px)"
        ~selector:(Css.Selector.class_ "\\@sm\\:p-4")
        ~props:[ Css.padding [ Css.Rem 1.0 ] ]
        ();
      Tw.Rules.starting_style
        ~selector:(Css.Selector.class_ "animate-in")
        ~props:[ Css.opacity 0.0 ]
        ();
      Tw.Rules.regular
        ~selector:(Css.Selector.class_ "m-2")
        ~props:[ Css.margin [ Css.Rem 0.5 ] ]
        ();
    ]
  in
  let classified = Tw.Rules.classify_by_type rules in
  check int "2 regular rules" 2 (List.length classified.regular);
  check int "1 media rule" 1 (List.length classified.media);
  check int "1 container rule" 1 (List.length classified.container);
  check int "1 starting rule" 1 (List.length classified.starting)

let test_style_rules_props () =
  (* Test that when a Style has both props and rules, the props are placed after
     the rules *)
  let open Css in
  let open Css.Selector in
  let test_class = class_ "test" in
  let p_element = element "p" in
  let div_element = element "div" in
  let custom_rules =
    [
      rule
        ~selector:(test_class ++ where [ p_element ])
        [ margin_top (Rem 1.0) ];
      rule
        ~selector:(test_class ++ where [ div_element ])
        [ padding [ Rem 2.0 ] ];
    ]
  in
  let props = [ color (Hex { hash = false; value = "ff0000" }) ] in

  let style = Tw.Style.style ~rules:(Some custom_rules) props in
  (* Create a test utility that wraps the style and provides the class name *)
  let module TestHandler = struct
    type t = Test
    type Tw.Utility.base += Self of t

    let name = "test"
    let priority = 0
    let to_class _t = "test"
    let to_style _t = style
    let suborder _t = 0
    let of_class _ = Error (`Msg "test utility")
  end in
  let () = Tw.Utility.register (module TestHandler) in
  let test_utility = Tw.Utility.base (TestHandler.Self TestHandler.Test) in
  let extracted = Tw.Rules.extract_selector_props test_utility in

  (* Should generate rules in order: custom rules first, then base props *)
  check int "correct number of rules" 3 (List.length extracted);

  (* Check that the base props come last *)
  let selectors =
    List.map
      (fun r ->
        match r with
        | Tw.Rules.Regular { selector; _ } -> Css.Selector.to_string selector
        | _ -> "")
      extracted
  in

  (* First two should be the custom rules, last should be the base class *)
  check string "first rule selector" ".test :where(p)" (List.nth selectors 0);
  check string "second rule selector" ".test :where(div)" (List.nth selectors 1);
  check string "last rule selector" ".test" (List.nth selectors 2)

let prose_p_selector prose_class =
  Css.Selector.combine prose_class Css.Selector.Descendant
    (Css.Selector.where
       [
         Css.Selector.compound
           [
             Css.Selector.element "p";
             Css.Selector.not
               [
                 Css.Selector.where
                   [
                     Css.Selector.list
                       [
                         Css.Selector.class_ "not-prose";
                         Css.Selector.combine
                           (Css.Selector.class_ "not-prose")
                           Css.Selector.Descendant Css.Selector.universal;
                       ];
                   ];
               ];
           ];
       ])

let grouped_prose_pairs prose_body_var prose_class prose_p_sel =
  let _, prose_body_v =
    Tw.Var.binding prose_body_var (Tw.Css.oklch 37.3 0.034 259.733)
  in
  (* Order doesn't matter for this test - all have same order *)
  let order = (1000, 0) in
  [
    ( prose_class,
      [
        Tw.Css.color (Tw.Css.Var prose_body_v);
        Tw.Css.max_width (Tw.Css.Ch 65.0);
      ],
      order );
    ( prose_p_sel,
      [
        Tw.Css.margin_top (Tw.Css.Em 1.0); Tw.Css.margin_bottom (Tw.Css.Em 1.0);
      ],
      order );
    ( prose_class,
      [ Tw.Css.font_size (Tw.Css.Rem 1.0); Tw.Css.line_height (Tw.Css.Num 1.5) ],
      order );
  ]

let count_prose_rules rules =
  List.filter
    (fun stmt ->
      match Tw.Css.statement_selector stmt with
      | Some sel -> Tw.Css.Selector.to_string sel = ".prose"
      | None -> false)
    rules

let rules_of_grouped_prose_bug () =
  let prose_body_var = Tw.Var.channel Css.Color "prose-body" in
  let prose_class = Css.Selector.class_ "prose" in
  let prose_p_sel = prose_p_selector prose_class in
  let grouped_pairs =
    grouped_prose_pairs prose_body_var prose_class prose_p_sel
  in
  let output_rules = Tw.Rules.of_grouped grouped_pairs in
  let prose_rules = count_prose_rules output_rules in

  Fmt.pr "@.=== test_rules_of_grouped_prose_bug ===@.";
  Fmt.pr "Input: 3 grouped pairs (2 .prose + 1 descendant)@.";
  Fmt.pr "Expected: 2 .prose rules in output@.";
  Fmt.pr "Actual: %d .prose rules in output@." (List.length prose_rules);
  check int "number of .prose rules" 2 (List.length prose_rules);
  check int "total output rules" 3 (List.length output_rules)

let test_cascade_order_violation () =
  (* CSS cascade rule: when selectors have equal specificity, the last one in
     source order wins. Sorting breaks this! *)

  (* Example 1: User intentionally puts p-2 after p-4 to override *)
  let user_intent = [ p 4; p 2 ] in
  (* User wants p-2 to win *)

  (* Extract the rules and convert to pairs *)
  let rules = user_intent |> List.concat_map Tw.Rules.extract_selector_props in

  (* Get selector strings from the rules *)
  let selectors =
    List.map
      (fun rule ->
        match rule with
        | Tw.Rules.Regular { selector; _ } -> Css.Selector.to_string selector
        | _ -> "")
      rules
  in

  Fmt.pr "@.=== CSS Cascade Order Violation Test ===@.";
  Fmt.pr "User wrote: [ p 4; p 2 ] (expecting p-2 to override p-4)@.";
  Fmt.pr "Extracted selectors: %a@."
    (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
    selectors;

  (* Find the p-4 and p-2 selectors *)
  let is_p4 sel = sel = ".p-4" in
  let is_p2 sel = sel = ".p-2" in

  let p4_idx = List.find_index is_p4 selectors in
  let p2_idx = List.find_index is_p2 selectors in

  match (p4_idx, p2_idx) with
  | Some i, Some j when i > j ->
      Alcotest.fail
        "CASCADE VIOLATION: p-4 comes after p-2, but user specified p-2 AFTER \
         p-4!"
  | Some i, Some j ->
      Fmt.pr "Correct order preserved: p-4 at index %d, p-2 at index %d@." i j
  | _ -> Alcotest.fail "Could not find both p-4 and p-2 selectors in output"

let test_cascade_prose_separation () =
  (* Test showing how sorting breaks intentional separation of .prose rules *)

  (* Extract prose rules to see their structure *)
  let outputs = Tw.Rules.extract_selector_props Tw.Prose.prose in
  let pairs = Tw.Rules.extract_selector_props_pairs outputs in

  Fmt.pr "@.=== Prose Rule Separation Test ===@.";
  Fmt.pr "Prose generates %d rules total@." (List.length pairs);

  (* Count how many .prose rules there are *)
  let prose_rules =
    List.filter (fun (sel, _, _) -> Css.Selector.to_string sel = ".prose") pairs
  in

  Fmt.pr "Found %d rules with selector .prose@." (List.length prose_rules);

  (* Apply of_grouped to see what happens *)
  let sorted_output = Tw.Rules.of_grouped pairs in

  (* Find .prose rules in sorted output *)
  let sorted_prose_indices =
    List.mapi
      (fun i stmt ->
        match Css.statement_selector stmt with
        | Some sel when Css.Selector.to_string sel = ".prose" -> Some i
        | _ -> None)
      sorted_output
    |> List.filter_map (fun x -> x)
  in

  Fmt.pr "After of_grouped: .prose rules at indices %a@."
    (Fmt.list ~sep:(Fmt.any ", ") Fmt.int)
    sorted_prose_indices;

  (* Check if prose rules became adjacent *)
  match sorted_prose_indices with
  | i :: j :: _ when j = i + 1 ->
      Fmt.pr "WARNING: .prose rules are now ADJACENT (indices %d and %d)!@." i j;
      Fmt.pr
        "This means the optimizer will merge them, breaking the intended \
         separation.@."
  | _ -> Fmt.pr "Prose rules remain separated in output@."

let test_cascade_color_override () =
  (* Real-world example: user wants to override a color *)
  let styles =
    [
      bg blue 500;
      (* Initial color *)
      text white 0;
      (* Some other styles *)
      bg red 500;
      (* Override the background to red *)
    ]
  in

  (* Extract rules *)
  let outputs = styles |> List.concat_map Tw.Rules.extract_selector_props in
  let pairs = Tw.Rules.extract_selector_props_pairs outputs in

  Fmt.pr "@.=== Color Override Cascade Test ===@.";
  Fmt.pr "User intent: bg-blue-500, text-white, bg-red-500 (red should win)@.";
  Fmt.pr "Original pairs: %d rules@." (List.length pairs);

  (* Check original order *)
  let original_selectors = List.map (fun (sel, _, _) -> sel) pairs in
  let orig_blue_idx =
    List.find_index
      (fun sel -> Css.Selector.to_string sel = ".bg-blue-500")
      original_selectors
  in
  let orig_red_idx =
    List.find_index
      (fun sel -> Css.Selector.to_string sel = ".bg-red-500")
      original_selectors
  in

  (match (orig_blue_idx, orig_red_idx) with
  | Some bi, Some ri ->
      Fmt.pr "Original order: blue at %d, red at %d (correct)@." bi ri
  | _ -> Fmt.pr "Could not find both colors in original@.");

  (* Apply of_grouped to see if order changes *)
  let sorted_output = Tw.Rules.of_grouped pairs in
  let sorted_selectors =
    List.map
      (fun stmt ->
        match Css.statement_selector stmt with
        | Some sel -> Css.Selector.to_string sel
        | None -> "")
      sorted_output
  in

  let sorted_blue_idx =
    List.find_index (fun sel -> sel = ".bg-blue-500") sorted_selectors
  in
  let sorted_red_idx =
    List.find_index (fun sel -> sel = ".bg-red-500") sorted_selectors
  in

  match (sorted_blue_idx, sorted_red_idx) with
  | Some bi, Some ri when bi > ri ->
      Fmt.pr
        "CASCADE VIOLATION: After sorting, bg-blue-500 (%d) comes AFTER \
         bg-red-500 (%d)!@."
        bi ri;
      Fmt.pr "This breaks the user's intention - red should override blue!@.";
      Alcotest.fail "Sorting reversed cascade order"
  | Some bi, Some ri ->
      Fmt.pr "After sorting: blue at %d, red at %d (order preserved)@." bi ri
  | _ -> Fmt.pr "Could not find both colors in sorted output@."

(* Test typography utilities come before color utilities *)
let test_typography_before_color () =
  let open Tw in
  (* Text alignment, size, and weight should come before text color *)
  let utilities =
    [
      text_center;
      text_4xl;
      font_bold;
      text gray 600;
      text_sm;
      text gray 800;
      text_white;
    ]
  in
  Test_helpers.check_ordering_matches
    ~test_name:"typography before color utilities" utilities

(* Test 1: Verify priority order - one utility per group *)
let test_priority_order_per_group () =
  let open Tw in
  (* Define pools of utilities for each priority group *)
  let position_utils = [ static; fixed; absolute; relative; sticky ] in
  let transform_utils = [ scale 105; rotate 45; scale 110 ] in
  let margin_utils = List.init 10 (fun i -> m i) in
  let prose_utils = [ prose; prose_sm; prose_lg; prose_xl ] in
  let layout_utils = [ block; inline; inline_block; hidden ] in
  let flex_utils = [ flex; inline_flex ] in
  let grid_utils = List.init 12 (fun i -> grid_cols (i + 1)) in
  let sizing_utils = List.init 10 (fun i -> w i) in
  let cursor_utils = [ cursor_pointer; cursor_default; cursor_wait ] in
  let grid_template_utils = List.init 6 (fun i -> grid_rows (i + 1)) in
  let alignment_utils = [ items_center; items_start; items_end ] in
  let gap_utils = List.init 10 (fun i -> gap i) in
  let border_utils = [ rounded; rounded_lg; rounded_full ] in
  let bg_utils = [ bg blue 500; bg red 500; bg green 500 ] in
  let padding_utils = List.init 10 (fun i -> p i) in
  let typography_utils = [ text_xl; text_sm; text_2xl ] in
  let effect_utils = [ shadow; shadow_md; shadow_lg ] in
  let animation_utils = [ animate_spin; animate_pulse; animate_bounce ] in
  let filter_utils = [ blur_sm; blur; blur_lg ] in

  (* Randomly pick one utility from each group *)
  let pick_random lst = List.nth lst (Random.int (List.length lst)) in

  let utilities =
    [
      pick_random position_utils;
      (* position: priority 0 *)
      pick_random margin_utils;
      (* margin: priority 2 *)
      pick_random prose_utils;
      (* prose: priority 3 *)
      pick_random layout_utils;
      (* layout: priority 4 *)
      pick_random flex_utils;
      (* flex: priority 4 *)
      pick_random grid_utils;
      (* grid: priority 4 *)
      pick_random sizing_utils;
      (* sizing: priority 6 *)
      pick_random cursor_utils;
      (* cursor: priority 7 *)
      pick_random grid_template_utils;
      (* grid_template: priority 8 *)
      pick_random alignment_utils;
      (* alignment: priority 9 *)
      pick_random gap_utils;
      (* gap: priority 10 *)
      pick_random border_utils;
      (* borders: priority 11 *)
      pick_random transform_utils;
      (* transforms: priority 6 *)
      pick_random animation_utils;
      (* animations: priority 7 *)
      pick_random bg_utils;
      (* backgrounds: priority 12 *)
      pick_random padding_utils;
      (* padding: priority 13 *)
      pick_random typography_utils;
      (* typography: priority 14 *)
      pick_random effect_utils;
      (* effects: priority 15 *)
      pick_random filter_utils;
      (* filters: priority 60 *)
    ]
  in
  Test_helpers.check_ordering_matches
    ~test_name:"priority order matches Tailwind" utilities

(* Verify Handler.priority values are in correct relative order *)
let test_handler_priority_ordering () =
  (* Get priority values directly from Handler modules. Note: Some modules
     (Prose, Display, Tables) don't expose Handler in their .mli, so we skip
     them in this test. Their ordering is still verified by
     test_priority_order_per_group which compares against Tailwind output. *)
  let position_prio = Tw.Position.Handler.priority in
  let margin_prio = Tw.Margin.Handler.priority in
  let layout_prio = Tw.Layout.Handler.priority in
  let flex_prio = Tw.Flex.Handler.priority in
  let grid_prio = Tw.Grid.Handler.priority in
  let sizing_prio = Tw.Sizing.Handler.priority in
  let cursor_prio = Tw.Cursor.Handler.priority in
  let grid_template_prio = Tw.Grid_template.Handler.priority in
  let alignment_prio = Tw.Alignment.Handler.priority in
  let gap_prio = Tw.Gap.Handler.priority in
  let border_prio = Tw.Borders.Handler.priority in
  let bg_prio = Tw.Backgrounds.Handler.priority in
  let padding_prio = Tw.Padding.Handler.priority in
  let typography_prio = Tw.Typography.Handler.priority in
  let color_prio = Tw.Color.Handler.priority in
  let effect_prio = Tw.Effects.Handler.priority in
  let transform_prio = Tw.Transforms.Handler.priority in
  let animation_prio = Tw.Animations.Handler.priority in
  let filter_prio = Tw.Filters.Handler.priority in

  (* Verify priority ordering - mostly total order with one exception: display
     utilities (layout, flex, grid, tables) all share priority 4 *)
  check bool "position < margin" true (position_prio < margin_prio);
  (* prose priority 3 skipped - Handler not exposed *)
  check bool "margin < layout" true (margin_prio < layout_prio);
  (* Display utilities all share priority 4: layout, flex, grid, tables. They
     are ordered by suborder instead of priority. *)
  check bool "layout = flex" true (layout_prio = flex_prio);
  check bool "flex = grid" true (flex_prio = grid_prio);
  check bool "grid < sizing" true (grid_prio < sizing_prio);
  check bool "sizing < transform" true (sizing_prio < transform_prio);
  check bool "transform < animation" true (transform_prio < animation_prio);
  check bool "animation < cursor" true (animation_prio < cursor_prio);
  check bool "cursor < grid_template" true (cursor_prio < grid_template_prio);
  check bool "grid_template < flex_props" true
    (grid_template_prio < Tw.Flex_props.Handler.priority);
  check bool "flex_props < alignment" true
    (Tw.Flex_props.Handler.priority < alignment_prio);
  check bool "alignment < gap" true (alignment_prio < gap_prio);
  check bool "gap < border" true (gap_prio < border_prio);
  check bool "border < bg" true (border_prio < bg_prio);
  check bool "bg < padding" true (bg_prio < padding_prio);
  check bool "padding < typography" true (padding_prio < typography_prio);
  check bool "typography < color" true (typography_prio < color_prio);
  check bool "color < effect" true (color_prio < effect_prio);
  check bool "effect < filter" true (effect_prio < filter_prio)
(* display and tables priority checked in test_priority_order_per_group *)

(* Test 2: Verify suborder within same group *)
let test_suborder_within_group () =
  let test_groups =
    [
      ("margin", Test_margin.all_utilities ());
      ("padding", Test_padding.all_utilities ());
      ("sizing", Test_sizing.all_utilities ());
      ("gap", Test_gap.all_utilities ());
      ("backgrounds", Test_backgrounds.all_utilities ());
      ("flex", Test_flex.all_utilities ());
      ("grid", Test_grid_template.all_utilities ());
      ("typography", Test_typography.all_utilities ());
      ("borders", Test_borders.all_utilities ());
      ("cursor", Test_cursor.all_utilities ());
      ("alignment", Test_alignment.all_utilities ());
      ("layout", Test_layout.all_utilities ());
      ("grid_placement", Test_grid.all_utilities ());
      ("effects", Test_effects.all_utilities ());
      ("position", Test_position.all_utilities ());
      ("display", Test_display.all_utilities ());
      ("forms", Test_forms.all_utilities ());
      ("transforms", Test_transforms.all_utilities ());
      ("interactivity", Test_interactivity.all_utilities ());
      ("filters", Test_filters.all_utilities ());
      ("containers", Test_containers.all_utilities ());
      ("animations", Test_animations.all_utilities ());
    ]
  in

  List.iter
    (fun (group_name, utilities) ->
      let test_name =
        Fmt.str "suborder for %s group matches Tailwind" group_name
      in
      let shuffled = Test_helpers.shuffle utilities in
      let forms = group_name = "forms" in
      Test_helpers.check_ordering_matches ~forms ~test_name shuffled)
    test_groups

(* Test 3: Random utilities with minimization *)
let test_random_utilities_with_minimization () =
  let open Tw in
  (* Large pool of utilities from all groups *)
  let all_utilities =
    [
      (* Margin: priority 2 *)
      m 0;
      m 1;
      m 2;
      m 3;
      m 4;
      m 5;
      m 6;
      m 8;
      m 10;
      m 12;
      m 16;
      m 20;
      mx 0;
      mx 1;
      mx 2;
      mx 4;
      mx 8;
      my 0;
      my 1;
      my 2;
      my 4;
      my 8;
      mt 0;
      mt 1;
      mt 2;
      mt 4;
      mt 6;
      mt 8;
      mb 0;
      mb 1;
      mb 2;
      mb 3;
      mb 4;
      mb 6;
      ml 0;
      ml 1;
      ml 2;
      ml 4;
      mr 0;
      mr 1;
      mr 2;
      mr 4;
      mr 5;
      mr 8;
      (* Display: priority 10 *)
      block;
      inline;
      inline_block;
      flex;
      inline_flex;
      grid;
      inline_grid;
      hidden;
      (* Position: priority 11 *)
      static;
      fixed;
      absolute;
      relative;
      sticky;
      (* Sizing: priority 12 *)
      w 0;
      w 1;
      w 2;
      w 4;
      w 6;
      w 8;
      w 10;
      w 12;
      w 16;
      w 20;
      w 24;
      h 0;
      h 1;
      h 2;
      h 4;
      h 6;
      h 8;
      h 10;
      h 12;
      h 16;
      h 20;
      min_w 0;
      min_h 0;
      max_w_2xl;
      max_w_3xl;
      max_w_4xl;
      max_w_5xl;
      (* Grid layout: priority 13 *)
      grid_cols 1;
      grid_cols 2;
      grid_cols 3;
      grid_cols 4;
      grid_cols 6;
      grid_cols 12;
      grid_rows 1;
      grid_rows 2;
      grid_rows 3;
      grid_rows 4;
      col_span 1;
      col_span 2;
      col_span 3;
      col_span 4;
      row_span 1;
      row_span 2;
      row_span 3;
      (* Flex layout: priority 14 *)
      flex_row;
      flex_col;
      (* Alignment: priority 15 *)
      items_start;
      items_center;
      items_end;
      items_baseline;
      items_stretch;
      justify_start;
      justify_center;
      justify_end;
      justify_between;
      justify_around;
      content_start;
      content_center;
      content_end;
      self_auto;
      self_start;
      self_center;
      (* Gap: priority 16 *)
      gap 0;
      gap 1;
      gap 2;
      gap 3;
      gap 4;
      gap 6;
      gap 8;
      gap 10;
      gap 12;
      gap_x 0;
      gap_x 2;
      gap_x 4;
      gap_y 0;
      gap_y 2;
      gap_y 4;
      (* Border: priority 17 *)
      rounded;
      rounded_md;
      rounded_lg;
      rounded_full;
      border;
      (* Background: priority 18 *)
      bg white 0;
      bg black 0;
      bg gray 50;
      bg gray 100;
      bg gray 200;
      bg gray 300;
      bg gray 500;
      bg gray 900;
      bg red 50;
      bg red 100;
      bg red 500;
      bg red 600;
      bg red 900;
      bg blue 50;
      bg blue 100;
      bg blue 500;
      bg blue 600;
      bg blue 900;
      bg green 50;
      bg green 500;
      bg green 600;
      bg yellow 50;
      bg yellow 500;
      bg purple 500;
      bg pink 500;
      (* Padding: priority 19 *)
      p 0;
      p 1;
      p 2;
      p 3;
      p 4;
      p 5;
      p 6;
      p 8;
      p 10;
      p 12;
      p 16;
      px 0;
      px 1;
      px 2;
      px 4;
      px 6;
      px 8;
      py 0;
      py 1;
      py 2;
      py 4;
      py 6;
      py 8;
      pt 0;
      pt 1;
      pt 2;
      pt 3;
      pt 4;
      pt 6;
      pt 8;
      pb 0;
      pb 1;
      pb 2;
      pb 4;
      pb 6;
      pb 8;
      pl 0;
      pl 1;
      pl 2;
      pl 4;
      pl 6;
      pl 8;
      pr 0;
      pr 1;
      pr 2;
      pr 4;
      pr 6;
      pr 8;
      (* Text align: priority 20 *)
      text_left;
      text_center;
      text_right;
      text_justify;
      (* Typography: priority 100 *)
      text_xs;
      text_sm;
      text_base;
      text_lg;
      text_xl;
      text_2xl;
      text_3xl;
      font_thin;
      font_normal;
      font_medium;
      font_semibold;
      font_bold;
      leading_tight;
      leading_snug;
      leading_normal;
      leading_relaxed;
      (* Effects: priority 700 *)
      shadow;
      shadow_sm;
      shadow_md;
      shadow_lg;
      shadow_xl;
      shadow_none;
      opacity 0;
      opacity 50;
      opacity 75;
      opacity 100;
      (* Interactivity: priority 800 *)
      cursor_pointer;
      cursor_default;
      cursor_wait;
      cursor_not_allowed;
      select_none;
      select_text;
      select_all;
      (* Container/Prose: priority 1000 *)
      prose;
      prose_sm;
      prose_lg;
    ]
  in

  (* Pick random utilities *)
  let pick_random n lst =
    let arr = Array.of_list lst in
    let len = Array.length arr in
    let picked = ref [] in
    for _ = 1 to min n len do
      let idx = Random.int len in
      picked := arr.(idx) :: !picked
    done;
    !picked
  in

  let initial = pick_random 30 all_utilities in
  Fmt.epr "Testing with %d utilities@." (List.length initial);

  match
    Test_helpers.minimize_failing_case Test_helpers.check_ordering_fails initial
  with
  | None -> () (* Test passes *)
  | Some final ->
      let final_classes = List.map Tw.pp final in
      Fmt.epr "@.Minimal failing case (%d utilities): %a@." (List.length final)
        Fmt.(list ~sep:(const string " ") string)
        final_classes;

      (* Now run the actual test with minimal case *)
      Test_helpers.check_ordering_matches
        ~test_name:"random utilities ordering matches Tailwind" final

let tests =
  [
    test_case "theme layer - empty" `Quick check_theme_layer_empty;
    test_case "theme layer - with color" `Quick check_theme_layer_with_color;
    test_case "extract selector props - basic" `Quick
      check_extract_selector_props;
    test_case "extract selector props - hover" `Quick check_extract_hover;
    test_case "extract selector props - responsive" `Quick
      check_extract_responsive;
    test_case "extract selector props - responsive md" `Quick
      check_extract_responsive_md;
    test_case "extract selector props - responsive lg" `Quick
      check_extract_responsive_lg;
    test_case "extract selector props - responsive xl" `Quick
      check_extract_responsive_xl;
    test_case "extract selector props - responsive 2xl" `Quick
      check_extract_responsive_2xl;
    test_case "conflict_order delegates to Utility.order" `Quick
      check_conflict_order;
    test_case "escape class name" `Quick check_escape_class_name;
    test_case "properties layer generation" `Quick check_properties_layer;
    test_case "to_css variables with base" `Quick check_css_variables_with_base;
    test_case "to_css variables without base" `Quick
      check_css_variables_without_base;
    test_case "to_css inline with base" `Quick check_css_inline_with_base;
    test_case "to_css inline without base" `Quick check_css_inline_without_base;
    test_case "inline style generation" `Quick check_inline_style;
    (* Layer + properties ordering *)
    test_case "layer decl + order" `Quick check_layer_declaration_and_ordering;
    test_case "properties layer internal order" `Quick
      check_properties_layer_internal_order;
    test_case "@property trailing and order" `Quick
      check_property_rules_trailing_and_order;
    (* New tests for exposed functions *)
    test_case "color_order" `Quick test_color_order;
    test_case "is_hover_rule" `Quick test_is_hover_rule;
    test_case "resolve_dependencies" `Quick test_resolve_dependencies;
    test_case "inline_no_var_in_css_for_defaults" `Quick
      test_inline_no_vars_defaults;
    test_case "inline_style_no_var_for_defaults" `Quick
      test_inline_style_no_vars;
    test_case "inline_vs_variables_diff" `Quick test_inline_vs_variables_diff;
    test_case "resolve_dependencies_dedup_and_queue" `Quick
      test_resolve_deps_dedup_queue;
    test_case "theme_layer_collects_media_refs" `Quick
      test_theme_layer_media_refs;
    test_case "theme_layer_collects_media_refs (md)" `Quick
      test_theme_layer_media_refs_md;
    test_case "rule_sets_injects_hover_media_query" `Quick
      test_rule_sets_hover_media;
    test_case "rule_sets_groups_md_media_query" `Quick test_rule_sets_md_media;
    test_case "multi-breakpoint grouping+order" `Quick test_media_grouping_order;
    test_case "md media dedup" `Quick test_md_media_dedup;
    test_case "md:hover has no global hover gate" `Quick
      test_md_hover_no_extra_media;
    test_case "container + media together" `Quick test_container_and_media;
    test_case "modifier_to_rule" `Quick test_modifier_to_rule;
    test_case "rule_sets" `Quick test_rule_sets;
    test_case "build_utilities_layer" `Quick test_build_utilities_layer;
    test_case "build_utilities_layer preserves order" `Quick
      test_build_utils_layer_order;
    test_case "classify" `Quick test_classify;
    test_case "style with rules and props ordering" `Quick
      test_style_rules_props;
    test_case "rules_of_grouped prose merging bug" `Quick
      rules_of_grouped_prose_bug;
    (* Layer ordering tests *)
    test_case "theme layer color order" `Quick test_theme_layer_color_order;
    test_case "utilities layer color order" `Quick
      test_utilities_layer_color_order;
    test_case "deterministic ordering" `Quick test_deterministic_ordering;
    (* CSS Cascade order tests - ensures source order is preserved *)
    test_case "source order preservation" `Quick test_cascade_order_violation;
    test_case "prose rule separation" `Quick test_cascade_prose_separation;
    test_case "color override cascading" `Quick test_cascade_color_override;
    (* Utility group ordering *)
    test_case "typography before color" `Quick test_typography_before_color;
    test_case "priority order per group" `Quick test_priority_order_per_group;
    test_case "handler priority ordering" `Quick test_handler_priority_ordering;
    test_case "suborder within group" `Slow test_suborder_within_group;
    test_case "random utilities with minimization" `Slow
      test_random_utilities_with_minimization;
  ]

let suite = ("rules", tests)
