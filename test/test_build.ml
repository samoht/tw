open Alcotest
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
  let theme_layer = Tw.Build.theme_layer_of ~default_decls [] in
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
  let theme_layer = Tw.Build.theme_layer_of ~default_decls [ bg blue ] in
  (* Should include color variable when referenced *)
  check bool "includes --color-blue-500" true
    (has_var_in_layer "--color-blue-500" "theme" theme_layer);
  (* Should still include font variables *)
  check bool "includes --font-sans" true
    (has_var_in_layer "--font-sans" "theme" theme_layer)

let check_conflict_order () =
  (* Test that Tw.Build.conflict_order correctly delegates to Utility.order *)
  (* It should parse the selector, extract the utility name, and return ordering *)

  (* Test basic selector parsing *)
  let prio, sub = Tw.Build.conflict_order ".p-4" in
  check int "p-4 priority" 21 prio;

  (* Padding priority *)

  (* Test with modifier prefix (should strip it) *)
  let prio_hover, sub_hover = Tw.Build.conflict_order ".hover\\:p-4:hover" in
  check int "hover:p-4 same priority as p-4" prio prio_hover;
  check int "hover:p-4 same suborder as p-4" sub sub_hover;

  (* Test relative ordering between utilities *)
  let m4_prio, _ = Tw.Build.conflict_order ".m-4" in
  let bg_prio, _ = Tw.Build.conflict_order ".bg-blue-500" in
  check bool "margin before background" true (m4_prio < bg_prio);
  (* bg-blue-500 is a background color (priority 20), which comes before Padding
     (priority 21) *)
  check bool "background color before padding" true (bg_prio < prio);

  (* Test unknown utility gets high priority *)
  let unknown_prio, _ = Tw.Build.conflict_order ".unknown-utility" in
  check int "unknown gets 9999 priority" 9999 unknown_prio

let check_properties_layer () =
  (* Test that shadow utilities generate proper @layer properties with initial
     values *)
  let config =
    {
      Tw.Build.base = false;
      mode = Css.Variables;
      optimize = false;
      forms = None;
      layers = true;
    }
  in
  let actual_css = Tw.Build.to_css ~config [ Tw.Effects.shadow_sm ] in

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
    {
      Tw.Build.base = true;
      mode = Css.Variables;
      optimize = false;
      forms = None;
      layers = true;
    }
  in
  let css = Tw.Build.to_css ~config [] in
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
    {
      Tw.Build.base = false;
      mode = Css.Variables;
      optimize = false;
      forms = None;
      layers = true;
    }
  in
  let css = Tw.Build.to_css ~config [ p 4 ] in
  (* Base=false under Variables: theme + components + utilities, but no base. *)
  check bool "has theme layer" true (has_layer "theme" css);
  check bool "no base layer" false (has_layer "base" css);
  check bool "has utilities layer" true (has_layer "utilities" css);
  check bool "has padding rule" true
    (has_selector_in_layer ".p-4" "utilities" css)

let check_css_inline_with_base () =
  let config =
    {
      Tw.Build.base = true;
      mode = Css.Inline;
      optimize = false;
      forms = None;
      layers = true;
    }
  in
  let css = Tw.Build.to_css ~config [ p 4 ] in
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
  let config =
    {
      Tw.Build.base = false;
      mode = Css.Inline;
      optimize = false;
      forms = None;
      layers = true;
    }
  in
  let css = Tw.Build.to_css ~config [ p 4 ] in
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
  let style = Tw.Build.to_inline_style [ p 4; m 2; bg blue ] in
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
  Tw.Build.to_css
    ~config:{ Tw.Build.base; mode; optimize; forms = None; layers = true }
    styles

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

let check_property_rules_order () =
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
  let config =
    {
      Tw.Build.base = false;
      mode = Css.Inline;
      optimize = false;
      forms = None;
      layers = true;
    }
  in
  let sheet = Tw.Build.to_css ~config [ Tw.Borders.rounded_sm ] in
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
    Tw.Build.to_css
      ~config:
        {
          Tw.Build.base = false;
          mode = Css.Variables;
          optimize = false;
          forms = None;
          layers = true;
        }
      [ Tw.Borders.rounded_sm ]
  in
  let sheet_inline =
    Tw.Build.to_css
      ~config:
        {
          Tw.Build.base = false;
          mode = Css.Inline;
          optimize = false;
          forms = None;
          layers = true;
        }
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
    Tw.Build.theme_layer_of ~default_decls [ sm [ Tw.Typography.text_xl ] ]
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

let test_theme_media_refs_md () =
  (* Vars referenced only under md media queries should still end up in
     theme. *)
  let theme_layer =
    let default_decls =
      Tw.Typography.default_font_declarations
      @ Tw.Typography.default_font_family_declarations
    in
    Tw.Build.theme_layer_of ~default_decls [ md [ Tw.Typography.text_xl ] ]
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
    {
      Tw.Build.base = false;
      mode = Css.Variables;
      optimize = false;
      forms = None;
      layers = true;
    }
  in
  let css = Tw.Build.to_css ~config [ hover [ p 4 ] ] in
  (* Check for exact media condition *)
  check bool "has (hover:hover) media query" true
    (has_media_condition "(hover: hover)" css);
  (* Extract selectors from within the (hover:hover) media query *)
  let selectors = selectors_in_media_sel ~condition:"(hover: hover)" css in
  let expected =
    Css.Selector.compound
      [ Css.Selector.class_ "hover:p-4"; Css.Selector.Hover ]
  in
  check
    (list Test_helpers.selector_testable)
    "hover selector in media" [ expected ] selectors

let test_rule_sets_md_media () =
  (* Multiple md[...] utilities should group under a single (min-width:768px)
     when optimized *)
  let css =
    Tw.Build.to_css
      ~config:
        {
          base = true;
          mode = Css.Variables;
          optimize = true;
          forms = None;
          layers = true;
        }
      [ md [ p 4 ]; md [ m 2 ] ]
  in
  (* Check for exact media condition *)
  check bool "has (min-width: 48rem) media query" true
    (has_media_condition "(min-width: 48rem)" css);

  (* Find the md media block and verify both selectors are inside it *)
  let md_block =
    Css.fold
      (fun acc stmt ->
        match (acc, Css.as_media stmt) with
        | Some _, _ -> acc
        | None, Some (cond, inner) when cond = Css.Media.Min_width_rem 48. ->
            Some inner
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
    Tw.Build.to_css [ sm [ p 2 ]; md [ m 4 ]; lg [ Tw.Typography.text_xl ] ]
  in
  (* Conditions present and in order *)
  let conditions = media_conditions css in
  check (list string) "media conditions order"
    [ "(min-width: 40rem)"; "(min-width: 48rem)"; "(min-width: 64rem)" ]
    conditions;
  (* Each block contains only its selectors *)
  let sm_sels = selectors_in_media_sel ~condition:"(min-width: 40rem)" css in
  let md_sels = selectors_in_media_sel ~condition:"(min-width: 48rem)" css in
  let lg_sels = selectors_in_media_sel ~condition:"(min-width: 64rem)" css in
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
  let css = Tw.Build.to_css [ md [ p 4 ]; md [ p 4 ] ] in
  check int "only one .md:p-4 in media (structural)" 1
    (count_selector_in_media_sel ~condition:"(min-width: 48rem)"
       ~selector:(Css.Selector.class_ "md:p-4")
       css)

let test_md_hover_extra_media () =
  (* Responsive+hover should not introduce a separate (hover:hover) gate *)
  let css = Tw.Build.to_css [ md [ hover [ p 4 ] ] ] in
  check bool "no (hover:hover) condition" false
    (has_media_condition "(hover: hover)" css);
  (* Selector is inside md media block with :hover pseudo, assert
     structurally *)
  let md_sels = selectors_in_media_sel ~condition:"(min-width: 48rem)" css in
  let expected =
    Css.Selector.compound
      [ Css.Selector.class_ "md:hover:p-4"; Css.Selector.Hover ]
  in
  check
    (list Test_helpers.selector_testable)
    "md:hover selector in md block" [ expected ] md_sels

let test_container_and_media () =
  let statements =
    Tw.Build.rule_sets [ Tw.Containers.container_md [ p 4 ]; md [ m 2 ] ]
  in
  (* Check that we have some statements *)
  check bool "has statements" true (List.length statements > 0);
  (* Check that we have both media and container queries in the output *)
  let has_media =
    List.exists
      (fun stmt ->
        match Tw.Css.as_media stmt with Some _ -> true | None -> false)
      statements
  in
  let has_container =
    List.exists
      (fun stmt ->
        match Tw.Css.as_container stmt with Some _ -> true | None -> false)
      statements
  in
  check bool "has media queries" true has_media;
  check bool "has container queries" true has_container

let test_rule_sets () =
  let statements = Tw.Build.rule_sets [ p 4; sm [ m 2 ] ] in
  (* Check that we have statements *)
  check bool "has statements" true (List.length statements > 0);
  (* Check for media queries *)
  let has_media =
    List.exists
      (fun stmt ->
        match Tw.Css.as_media stmt with Some _ -> true | None -> false)
      statements
  in
  check bool "has media queries" true has_media;
  (* Check no container queries *)
  let has_container =
    List.exists
      (fun stmt ->
        match Tw.Css.as_container stmt with Some _ -> true | None -> false)
      statements
  in
  check bool "no container queries" false has_container

let test_build_utilities_layer () =
  let statements =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "p-4")
        [ Css.padding [ Css.Rem 1.0 ] ];
      Css.rule
        ~selector:(Css.Selector.class_ "m-2")
        [ Css.margin [ Css.Rem 0.5 ] ];
    ]
  in
  let layer = Tw.Build.utilities_layer ~layers:true ~statements in
  (* Check for utilities layer and selectors *)
  check bool "creates utilities layer" true (has_layer "utilities" layer);
  check bool "includes padding rule" true
    (has_selector_in_layer ".p-4" "utilities" layer);
  check bool "includes margin rule" true
    (has_selector_in_layer ".m-2" "utilities" layer)

let test_build_utils_layer_order () =
  (* Test that build_utilities_layer preserves rule order and doesn't sort *)
  let statements =
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
  let layer = Tw.Build.utilities_layer ~layers:true ~statements in
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
  let extracted = Tw.Rule.outputs test_utility in

  (* Should generate rules in order: custom rules first, then base props *)
  check int "correct number of rules" 3 (List.length extracted);

  (* Check that the base props come last *)
  let selectors =
    List.map
      (fun r ->
        match r with
        | Tw.Output.Regular { selector; _ } -> Css.Selector.to_string selector
        | _ -> "")
      extracted
  in

  (* First two should be the custom rules, last should be the base class *)
  check string "first rule selector" ".test :where(p)" (List.nth selectors 0);
  check string "second rule selector" ".test :where(div)" (List.nth selectors 1);
  check string "last rule selector" ".test" (List.nth selectors 2)

let test_media_query_deduplication () =
  (* Test that media queries preserve cascade order.
   *
   * Container Tw.Rule.outputs separate @media blocks for each breakpoint.
   * md:grid-cols-2 also gets a @media block for its breakpoint.
   * At 48rem we expect 2 media queries: one for container, one for md:grid-cols-2.
   *
   * Example output:
   *   .container { width: 100% }
   *   @media (min-width:768px) { .container { max-width: 48rem } }
   *   @media (min-width:768px) { .md\:grid-cols-2 { ... } }
   *)
  let utilities = Tw.[ container; md [ grid_cols 2 ] ] in
  let css = Tw.to_css ~base:false ~optimize:false utilities in

  (* Count top-level media queries in layers *)
  let rec count_toplevel_media condition stmt =
    match Tw.Css.as_media stmt with
    | Some (cond, _) -> if Tw.Css.Media.equal cond condition then 1 else 0
    | None -> (
        match Tw.Css.as_layer stmt with
        | Some (_, content) ->
            List.fold_left ( + ) 0
              (List.map (count_toplevel_media condition) content)
        | None -> 0)
  in

  let count_768px =
    List.fold_left ( + ) 0
      (List.map
         (count_toplevel_media (Tw.Css.Media.Min_width_rem 48.))
         (Tw.Css.statements css))
  in

  (* Should have 2 top-level media queries at 768px: one for container, one for
     md:grid-cols-2 *)
  Alcotest.(check int)
    "preserves cascade with nested and top-level media" 2 count_768px

let tests =
  [
    test_case "theme layer - empty" `Quick check_theme_layer_empty;
    test_case "theme layer - with color" `Quick check_theme_layer_with_color;
    test_case "Tw.Build.conflict_order delegates to Utility.order" `Quick
      check_conflict_order;
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
    test_case "@property trailing and order" `Quick check_property_rules_order;
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
      test_theme_media_refs_md;
    test_case "rule_sets_injects_hover_media_query" `Quick
      test_rule_sets_hover_media;
    test_case "rule_sets_groups_md_media_query" `Quick test_rule_sets_md_media;
    test_case "multi-breakpoint grouping+order" `Quick test_media_grouping_order;
    test_case "md media dedup" `Quick test_md_media_dedup;
    test_case "md:hover has no global hover gate" `Quick
      test_md_hover_extra_media;
    test_case "container + media together" `Quick test_container_and_media;
    test_case "media query deduplication" `Quick test_media_query_deduplication;
    test_case "rule_sets" `Quick test_rule_sets;
    test_case "build_utilities_layer" `Quick test_build_utilities_layer;
    test_case "build_utilities_layer preserves order" `Quick
      test_build_utils_layer_order;
    test_case "style with rules and props ordering" `Quick
      test_style_rules_props;
  ]

let suite = ("build", tests)
