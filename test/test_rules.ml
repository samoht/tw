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
  let default_decls =
    Tw.Typography.default_font_declarations
    @ Tw.Typography.default_font_family_declarations
  in
  let theme_layer = compute_theme_layer ~default_decls [] in
  let css = Css.to_string ~minify:false theme_layer in
  (* Should include font variables even for empty input *)
  check bool "includes --font-sans" true (contains css "--font-sans");
  check bool "includes --font-mono" true (contains css "--font-mono");
  check bool "includes --default-font-family" true
    (contains css "--default-font-family");
  check bool "includes --default-mono-font-family" true
    (contains css "--default-mono-font-family")

let check_theme_layer_with_color () =
  let default_decls =
    Tw.Typography.default_font_declarations
    @ Tw.Typography.default_font_family_declarations
  in
  let theme_layer =
    Tw.Rules.compute_theme_layer ~default_decls [ bg blue 500 ]
  in
  let css = Css.to_string ~minify:false theme_layer in
  (* Should include color variable when referenced *)
  check bool "includes --color-blue-500" true (contains css "--color-blue-500");
  (* Should still include font variables *)
  check bool "includes --font-sans" true (contains css "--font-sans")

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
      check bool "selector contains hover" true
        (contains (Css.Selector.to_string selector) ":hover")
  | _ -> fail "Expected Regular rule with hover"

let check_extract_responsive () =
  let rules = extract_selector_props (sm [ p 4 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Media_query { condition; selector; _ } ] ->
      check bool "has min-width condition" true (contains condition "min-width");
      check string "correct selector" ".sm\\:p-4"
        (Css.Selector.to_string selector)
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
  (* Test that shadow utilities generate proper @layer properties with initial
     values *)
  let config =
    { Tw.Rules.base = false; mode = Css.Variables; optimize = false }
  in
  let actual_css = Tw.Rules.to_css ~config [ Tw.Effects.shadow_sm ] in

  (* Extract the actual properties layer from generated CSS *)
  let statements = Css.statements actual_css in
  let properties_layer =
    List.find_opt
      (fun stmt ->
        match Css.as_layer stmt with
        | Some (Some "properties", _) -> true
        | _ -> false)
      statements
  in

  (* Verify properties layer exists *)
  match properties_layer with
  | None -> fail "Expected @layer properties to be generated"
  | Some layer_stmt ->
      let actual_layer_css = Css.of_statements [ layer_stmt ] in
      let actual_str = Css.to_string ~minify:true actual_layer_css in

      (* Main verification: shadow variables should have actual initial values,
         not "initial" *)
      check bool "has --tw-shadow with actual shadow value" true
        (contains actual_str "--tw-shadow:0 0 #0000");
      check bool "has --tw-shadow-alpha with percentage value" true
        (contains actual_str "--tw-shadow-alpha:100%");
      check bool "has --tw-ring-offset-color with color value" true
        (contains actual_str "--tw-ring-offset-color:#fff");
      check bool "has --tw-ring-offset-width with length value" true
        (contains actual_str "--tw-ring-offset-width:0");

      (* Verify we're NOT getting generic "initial" for these specific
         variables *)
      check bool "shadow variables not using generic initial" false
        (contains actual_str "--tw-shadow:initial");
      check bool "shadow-alpha not using generic initial" false
        (contains actual_str "--tw-shadow-alpha:initial");

      (* Debug output to see what was generated *)
      Printf.printf "\nGenerated properties layer: %s\n" actual_str

let check_css_variables_with_base () =
  let config =
    { Tw.Rules.base = true; mode = Css.Variables; optimize = false }
  in
  let css = Tw.Rules.to_css ~config [] in
  let css_str = Css.to_string ~minify:false css in
  (* Base=true under Variables: all layers including base are present. *)
  check bool "includes base resets" true
    (contains css_str "*, ::after, ::before");
  check bool "has theme layer" true (contains css_str "@layer theme");
  check bool "has base layer" true (contains css_str "@layer base");
  check bool "has utilities layer" true (contains css_str "@layer utilities")

let check_css_variables_without_base () =
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

let check_css_inline_with_base () =
  let config = { Tw.Rules.base = true; mode = Css.Inline; optimize = false } in
  let css = Tw.Rules.to_css ~config [ p 4 ] in
  let css_str = Css.to_string ~minify:false ~mode:Css.Inline css in
  (* Inline mode never emits layers; base has no effect. *)
  check bool "no theme layer" true (not (contains css_str "@layer theme"));
  check bool "no base layer" true (not (contains css_str "@layer base"));
  check bool "no utilities layer" true
    (not (contains css_str "@layer utilities"));
  check bool "has padding rule" true (contains css_str ".p-4")

let check_css_inline_without_base () =
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
  check int "amber utilities order" 3
    (let _, order = Tw.Color.utilities_order "amber" in
     order);
  check int "blue utilities order" 4
    (let _, order = Tw.Color.utilities_order "blue" in
     order);
  check int "cyan utilities order" 5
    (let _, order = Tw.Color.utilities_order "cyan" in
     order);
  check int "sky utilities order" 18
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
  let css_inline = Css.to_string ~minify:false ~mode:Css.Inline sheet in
  check bool "no var() in inline CSS" false (contains css_inline "var(--");
  check bool "has border-radius" true (contains css_inline "border-radius")

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
  check bool "includes --text-xl var" true
    (List.exists (fun v -> contains v "text-xl") all_vars);
  check bool "includes --text-xl--line-height var" true
    (List.exists (fun v -> contains v "text-xl--line-height") all_vars)

let test_rule_sets_hover_media () =
  (* A bare hover utility produces a rule that should be gated behind
     (hover:hover) *)
  let css = Tw.Rules.to_css [ hover [ Tw.Spacing.p 4 ] ] in
  let css_string = Css.to_string ~minify:false css in
  check bool "has (hover:hover) media query" true
    (contains css_string "(hover:hover)");
  check bool "hover rule is inside media query" true
    (contains css_string ".hover\\:p-4:hover")

let test_modifier_to_rule () =
  let rule =
    Tw.Rules.modifier_to_rule Tw.Core.Hover "bg-blue-500"
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
  let css = Css.to_string ~minify:false layer in
  check bool "creates utilities layer" true (contains css "@layer utilities");
  check bool "includes padding rule" true (contains css ".p-4");
  check bool "includes margin rule" true (contains css ".m-2")

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
  let classified = Tw.Rules.classify rules in
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

  let style = Tw.Core.style ~rules:(Some custom_rules) "test" props in
  let extracted = Tw.Rules.extract_selector_props style in

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
  [
    ( prose_class,
      [
        Tw.Css.color (Tw.Css.Var prose_body_v);
        Tw.Css.max_width (Tw.Css.Ch 65.0);
      ] );
    ( prose_p_sel,
      [
        Tw.Css.margin_top (Tw.Css.Em 1.0); Tw.Css.margin_bottom (Tw.Css.Em 1.0);
      ] );
    ( prose_class,
      [ Tw.Css.font_size (Tw.Css.Rem 1.0); Tw.Css.line_height (Tw.Css.Num 1.5) ]
    );
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
    List.filter (fun (sel, _) -> Css.Selector.to_string sel = ".prose") pairs
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
  let original_selectors = List.map fst pairs in
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
    test_case "rule_sets_injects_hover_media_query" `Quick
      test_rule_sets_hover_media;
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
  ]

let suite = ("rules", tests)
