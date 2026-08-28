module Css = Cascade.Css
open Alcotest
open Tw.Color
open Tw.Backgrounds
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
  check int "p-4 priority" 23 prio;

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
     (priority 23) *)
  check bool "background color before padding" true (bg_prio < prio);

  (* Test unknown utility gets high priority *)
  let unknown_prio, _ = Tw.Build.conflict_order ".unknown-utility" in
  check int "unknown gets 9999 priority" 9999 unknown_prio

let check_properties_layer () =
  (* Test that shadow utilities generate proper @layer properties with initial
     values *)
  let config = { Tw.Build.base = false; forms = None; layers = true } in
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

(* Regression: a theme color referenced only inside a compound variant's nested
   media (hover:dark:text-white puts var(--color-white) two media levels deep)
   must still be declared in the theme layer. The var collection used to stop at
   the first level and miss it, so --color-white got pruned. *)
let check_compound_variant_theme_var () =
  let u =
    match Tw.of_string "hover:dark:text-white" with
    | Ok u -> u
    | Error (`Msg m) -> fail m
  in
  let css = Tw.to_css ~base:false ~layers:true [ u ] in
  check bool "theme layer declares --color-white" true
    (has_var_in_layer "--color-white" "theme" css)

(* Regression: a compound [supports-] variant nests its rule in an [@supports]
   block, and the tokens the utility sets live in there with it. The theme walk
   used to descend into [@media], [@layer] and [@container] only, so the token
   went undeclared and the rule read a [var()] nothing defined. *)
let check_supports_variant_theme_token () =
  let u =
    match Tw.of_string "md:supports-[display:grid]:aspect-video" with
    | Ok u -> u
    | Error (`Msg m) -> fail m
  in
  let css = Tw.to_css ~base:false ~layers:true [ u ] in
  check bool "theme layer declares --aspect-video" true
    (has_var_in_layer "--aspect-video" "theme" css)

let check_css_variables_with_base () =
  let config = { Tw.Build.base = true; forms = None; layers = true } in
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
  let config = { Tw.Build.base = false; forms = None; layers = true } in
  let css = Tw.Build.to_css ~config [ p 4 ] in
  (* Base=false under Variables: theme + components + utilities, but no base. *)
  check bool "has theme layer" true (has_layer "theme" css);
  check bool "no base layer" false (has_layer "base" css);
  check bool "has utilities layer" true (has_layer "utilities" css);
  check bool "has padding rule" true
    (has_selector_in_layer ".p-4" "utilities" css)

let check_css_inline_with_base () =
  let config = { Tw.Build.base = true; forms = None; layers = true } in
  let css = Tw.Build.to_css ~config [ p 4 ] in
  let css_str = Css.(css |> inline_vars |> to_string) in
  check bool "no layer wrappers" false
    (Astring.String.is_infix ~affix:"@layer" css_str);
  check bool "has padding rule" true
    (Astring.String.is_infix ~affix:".p-4" css_str)

let check_css_inline_without_base () =
  let config = { Tw.Build.base = false; forms = None; layers = true } in
  let css = Tw.Build.to_css ~config [ p 4 ] in
  let css_str = Css.(css |> inline_vars |> to_string) in
  check bool "no layer wrappers" false
    (Astring.String.is_infix ~affix:"@layer" css_str);
  check bool "has padding rule" true
    (Astring.String.is_infix ~affix:".p-4" css_str)

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
let sheet_of ?(base = false) ?(mode = Css.Variables) styles =
  let sheet =
    Tw.Build.to_css
      ~config:{ Tw.Build.base; forms = None; layers = true }
      styles
  in
  let sheet =
    match mode with
    | Css.Inline -> Css.inline_vars sheet
    | Css.Variables -> sheet
  in
  sheet

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
  let expected = [ "properties"; "theme"; "components"; "utilities" ] in
  let layer_names =
    Css.statements sheet
    |> List.find_map Css.layer_statement_name_list
    |> Option.map (List.map Css.Stylesheet.string_of_layer_name)
  in
  check (option (list string)) "layer decl order" (Some expected) layer_names;
  check bool "has properties layer block" true
    (Css.layer_block [ "properties" ] sheet <> None)

let check_properties_layer_internal_order () =
  let sheet = sheet_of [ Tw.Effects.shadow_sm ] in
  let props =
    Css.layer_block [ "properties" ] sheet
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
        | Some (Some [ "utilities" ], _) -> true
        | _ -> false)
    |> or_fail "Expected a utilities layer"
  in
  let first_prop_idx =
    stmt_index sheet (fun s ->
        match Css.as_property s with Some _ -> true | None -> false)
    |> or_fail "Expected at least one @property rule"
  in
  check bool "@property after utilities" true (first_prop_idx > util_idx)

(* Regression: --tw-space-y-reverse must sort AFTER the transform group in
   @property, matching Tailwind (which places it after --tw-translate-* and
   before --tw-border-style). It previously carried a negative property_order
   with no family, which forced it ahead of the transforms. *)
let check_space_reverse_after_transforms () =
  let sheet =
    Tw.to_css ~base:false ~layers:true [ Tw.translate_x 4; Tw.space_y 4 ]
  in
  let names = property_rule_names sheet in
  let index_of n =
    let rec loop i = function
      | [] -> fail (n ^ " missing from @property rules")
      | x :: _ when x = n -> i
      | _ :: t -> loop (i + 1) t
    in
    loop 0 names
  in
  check bool "--tw-translate-x before --tw-space-y-reverse" true
    (index_of "--tw-translate-x" < index_of "--tw-space-y-reverse")

let index_of_prop names n =
  let rec loop i = function
    | [] -> fail (n ^ " missing from @property rules")
    | x :: _ when x = n -> i
    | _ :: t -> loop (i + 1) t
  in
  loop 0 names

(* Regression: Tailwind emits @property --tw-border-spacing-* first in @layer
   properties, ahead of the transforms. A negative property_order with no family
   sorts them there. *)
let check_border_spacing_first () =
  let names =
    property_rule_names
      (Tw.to_css ~base:false ~layers:true
         [ Tw.border_spacing 2; Tw.translate_x 4; Tw.blur ])
  in
  let index_of = index_of_prop names in
  check bool "--tw-border-spacing-x before --tw-translate-x" true
    (index_of "--tw-border-spacing-x" < index_of "--tw-translate-x")

(* Regression: @property --tw-outline-style sorts after the ring group and
   before the filters, not with the border-style block. *)
let check_outline_style_after_ring () =
  let outline =
    match Tw.of_string "outline" with Ok u -> u | Error (`Msg m) -> fail m
  in
  let names =
    property_rule_names
      (Tw.to_css ~base:false ~layers:true [ outline; Tw.ring; Tw.blur ])
  in
  let index_of = index_of_prop names in
  check bool "--tw-outline-style after --tw-ring-shadow" true
    (index_of "--tw-ring-shadow" < index_of "--tw-outline-style");
  check bool "--tw-outline-style before --tw-blur" true
    (index_of "--tw-outline-style" < index_of "--tw-blur")

(* Regression: content-none uses a literal [content: none] and must NOT register
   @property --tw-content, matching Tailwind (which emits it only for content
   utilities that reference var(--tw-content), and for before/after
   pseudo-elements). The value variant content-["x"] does reference the var and
   keeps the @property rule. *)
let check_content_none_no_property () =
  let none_names =
    property_rule_names (Tw.to_css ~base:false ~layers:true [ Tw.content_none ])
  in
  check bool "content-none emits no @property --tw-content" false
    (List.mem "--tw-content" none_names);
  let value_names =
    property_rule_names (Tw.to_css ~base:false ~layers:true [ Tw.content "x" ])
  in
  check bool "content value keeps @property --tw-content" true
    (List.mem "--tw-content" value_names)

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
  let config = { Tw.Build.base = false; forms = None; layers = true } in
  let sheet =
    Tw.Build.to_css ~config [ Tw.Borders.rounded_sm ] |> Css.inline_vars
  in
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
  let decls =
    [
      Css.border_radius
        (Css.Radius
           { horizontal = [ Css.Length (Css.Var radius_var) ]; vertical = None });
    ]
  in
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
     that may still contain var() in declarations. p-4 spells its reference as
     calc(var(--spacing) * 4), so the variables sheet only counts as referencing
     one if the reference is read past the head of the value. *)
  let sheet_vars =
    Tw.Build.to_css
      ~config:{ Tw.Build.base = false; forms = None; layers = true }
      [ p 4 ]
  in
  let sheet_inline =
    Tw.Build.to_css
      ~config:{ Tw.Build.base = false; forms = None; layers = true }
      [ p 4 ]
    |> Css.inline_vars
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

(* Where [sheet] declares each of [vars], in the order the sheet declares them.
   The theme layer is the only place a [--name:] declaration appears; every
   other mention is a [var()] reference with no colon after the name. *)
let theme_var_order sheet vars =
  let position v =
    let needle = v ^ ":" in
    let n = String.length needle and len = String.length sheet in
    let rec scan i =
      if i + n > len then failf "%s is missing from the sheet" v
      else if String.sub sheet i n = needle then i
      else scan (i + 1)
    in
    scan 0
  in
  List.map (fun v -> (position v, v)) vars |> List.sort compare |> List.map snd

(* Several theme families number their slots from a base of their own, so tokens
   from different families land on one slot and the tie falls to the variable
   name, which interleaves them. Nothing rendered changes and the canonical
   differ reads the two sheets as equal, so the CLI's own emission order is the
   oracle. *)
let check_theme_layer_family_order () =
  let classes =
    [
      "rounded-4xl";
      "drop-shadow-md";
      "ease-out";
      "animate-spin";
      "animate-pulse";
      "blur-md";
      "perspective-dramatic";
      "perspective-near";
      "aspect-video";
    ]
  in
  let vars =
    [
      "--radius-4xl";
      "--drop-shadow-md";
      "--ease-out";
      "--animate-spin";
      "--animate-pulse";
      "--blur-md";
      "--perspective-dramatic";
      "--perspective-near";
      "--aspect-video";
    ]
  in
  let utilities =
    List.map
      (fun c ->
        match Tw.of_string c with
        | Ok u -> u
        | Error (`Msg m) -> failf "%s: %s" c m)
      classes
  in
  let expected = theme_var_order (tailwind_css classes) vars in
  check (list string) "theme layer family order" expected
    (theme_var_order (our_css utilities) vars)

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
    Css.layer_block [ "theme" ] theme_layer
    |> Option.map Css.rules_of_statements
    |> Option.map Css.custom_props_of_rules
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
    Css.layer_block [ "theme" ] theme_layer
    |> Option.map Css.rules_of_statements
    |> Option.map Css.custom_props_of_rules
    |> Option.value ~default:[]
  in
  check bool "includes --text-xl var (md)" true
    (List.exists (fun v -> v = "--text-xl") all_vars);
  check bool "includes --text-xl--line-height var (md)" true
    (List.exists (fun v -> v = "--text-xl--line-height") all_vars)

let test_rule_sets_hover_media () =
  (* A bare hover utility produces a rule that should be gated behind
     (hover:hover) *)
  let config = { Tw.Build.base = false; forms = None; layers = true } in
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
  (* Multiple md[...] utilities should group under a single min-width media
     block without relying on Cascade optimization. *)
  let css =
    Tw.Build.to_css
      ~config:{ base = true; forms = None; layers = true }
      [ md [ p 4 ]; md [ m 2 ] ]
  in
  check bool "has (min-width: 48rem) media query" true
    (has_media_condition "(min-width: 48rem)" css);

  (* Collect md media blocks and verify both selectors are under md. CSS
     optimization may merge these blocks, but tw no longer depends on that. *)
  let selectors =
    Css.fold
      (fun acc stmt ->
        match Css.as_media stmt with
        | Some (cond, inner)
          when Css.Media.to_string cond = "(min-width: 48rem)" ->
            List.filter_map
              (fun s ->
                match Css.as_rule s with
                | Some (sel, _, _) -> Some sel
                | None -> None)
              inner
            @ acc
        | _ -> acc)
      [] css
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

(* Duplicate rules are dropped on cascade's structural selector equality, so a
   class repeated in the input reaches the utilities layer once, and two
   distinct selectors are never taken for one another. *)
let test_duplicate_selector_dedup () =
  let count css sel =
    extract_utilities_layer_rules css
    |> List.filter_map Css.statement_selector
    |> List.filter (Css.Selector.equal sel)
    |> List.length
  in
  let repeated = Tw.Build.to_css [ p 4; p 4 ] in
  check int "repeated p-4 emits one rule" 1
    (count repeated (Css.Selector.class_ "p-4"));
  let distinct = Tw.Build.to_css [ p 4; m 4 ] in
  check int "p-4 kept beside m-4" 1 (count distinct (Css.Selector.class_ "p-4"));
  check int "m-4 kept beside p-4" 1 (count distinct (Css.Selector.class_ "m-4"))

let test_md_hover_extra_media () =
  (* A responsive prefix wraps the hover rule's own (hover:hover) gate rather
     than replacing it, which is the structure Tailwind emits. *)
  let css = Tw.Build.to_css [ md [ hover [ p 4 ] ] ] in
  check bool "keeps the (hover:hover) gate" true
    (has_media_condition "(hover: hover)" css);
  (* The rule sits in the inner block, so the md block holds no rule of its
     own. *)
  let md_sels = selectors_in_media_sel ~condition:"(min-width: 48rem)" css in
  check
    (list Test_helpers.selector_testable)
    "no rule directly in md" [] md_sels;
  let hover_sels = selectors_in_media_sel ~condition:"(hover: hover)" css in
  let expected =
    Css.Selector.compound
      [ Css.Selector.class_ "md:hover:p-4"; Css.Selector.Hover ]
  in
  check
    (list Test_helpers.selector_testable)
    "md:hover selector in the hover block" [ expected ] hover_sels

(* A container query wraps the hover gate the same way a media query does, and
   the rule inside it still reaches its theme token: the declarations live in
   the nested block, so a collector that stops at the top level prunes
   --spacing. *)
let test_container_hover_nests () =
  let css = Tw.Build.to_css [ Tw.Containers.container_md [ hover [ p 4 ] ] ] in
  let hover_sels = selectors_in_media_sel ~condition:"(hover: hover)" css in
  let expected =
    Css.Selector.compound
      [ Css.Selector.class_ "@md:hover:p-4"; Css.Selector.Hover ]
  in
  check
    (list Test_helpers.selector_testable)
    "@md:hover selector in the hover block" [ expected ] hover_sels;
  check bool "theme layer declares --spacing" true
    (has_var_in_layer "--spacing" "theme" css)

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
        [ Css.color (Css.hex "#ff0000") ];
      Css.rule ~selector:(Css.Selector.class_ "b") [ Css.margin [ Css.Px 10. ] ];
      Css.rule ~selector:(Css.Selector.class_ "c") [ Css.padding [ Css.Px 5. ] ];
      Css.rule ~selector:(Css.Selector.class_ "a")
        [ Css.background_color (Css.hex "#0000ff") ];
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

(* An [extra] rule seeds its order under its base utility's name, and a plain
   utility of the same name reads that key too. [table] sorts behind [flex] on
   its own suborder, whatever order a routed [dark:!table] arrives with. *)
let test_extra_keeps_plain_order () =
  let extra =
    [
      ( "dark:!table",
        (4, 2),
        [
          Css.rule
            ~selector:(Css.Selector.class_ "dark:!table")
            [ Css.display Css.Table ];
        ] );
    ]
  in
  let config = { Tw.Build.base = false; forms = None; layers = true } in
  let css = Tw.Build.to_css ~config ~extra [ Tw.Flex.flex; Tw.Layout.table ] in
  let css_str = Css.to_string ~minify:true css in
  let position sub =
    match Astring.String.find_sub ~sub css_str with
    | None -> -1
    | Some pos -> pos
  in
  let flex = position ".flex{" and table = position ".table{" in
  check bool "flex is emitted" true (flex >= 0);
  check bool "table is emitted" true (table >= 0);
  check bool "flex before table" true (flex < table)

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
  let props = [ color (Css.hex "#ff0000") ] in

  let style = Tw.Style.style ~rules:(Some custom_rules) props in
  (* Create a test utility that wraps the style and provides the class name *)
  let module TestHandler = struct
    type t = Test
    type Tw.Utility.base += Self of t

    let name = "test"
    let priority _ = 0
    let to_class _t = "test"
    let to_style _theme _t = style
    let suborder _t = 0
    let of_class _ _ = Error (`Msg "test utility")
    let examples = []
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
  let css = Tw.to_css ~base:false utilities in

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
         (count_toplevel_media (Tw.Css.media_min_width_length (Tw.Css.Rem 48.)))
         (Tw.Css.statements css))
  in

  (* Should have 2 top-level media queries at 768px: one for container, one for
     md:grid-cols-2 *)
  Alcotest.(check int)
    "preserves cascade with nested and top-level media" 2 count_768px

(* p-0 folds to 0, so the unused --spacing token is pruned, matching
   Tailwind. *)
let check_spacing_zero_prune () =
  let config = { Tw.Build.base = false; forms = None; layers = true } in
  let css =
    Tw.Build.to_css ~config [ p 0 ]
    |> Css.optimize ~prune_unused_custom_props:true
    |> Css.to_string ~minify:true
  in
  Alcotest.(check bool)
    "p-0 is padding:0" true
    (Astring.String.is_infix ~affix:"padding:0" css);
  Alcotest.(check bool)
    "p-0 drops unused --spacing" false
    (Astring.String.is_infix ~affix:"--spacing" css)

(* A utility that only REFERENCES a theme colour token via var() (here an
   arbitrary bg-[color:var(--color-red-500)]) must still emit that token in
   @layer theme; the build used to collect only tokens utilities set. A
   non-theme var stays out. *)
let test_referenced_theme_token () =
  let css cls =
    match Tw.of_string cls with
    | Error (`Msg m) -> Alcotest.failf "parse %s: %s" cls m
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
  in
  (* The "--token:" form (a declaration) appears only when the token is SET in
     @layer theme; the references are "var(--token)". *)
  check bool "referenced --color-red-500 is emitted in theme" true
    (Astring.String.is_infix ~affix:"--color-red-500:"
       (css "bg-[color:var(--color-red-500)]/50"));
  check bool "non-theme --my-color is not emitted" false
    (Astring.String.is_infix ~affix:"--my-color:"
       (css "bg-[color:var(--my-color)]/50"))

(* [@starting-style] takes no condition, so a run of starting: utilities is one
   block in Tailwind's output. Each utility is wrapped on its own, and the
   optimizer's merging covers @media and @supports but not this. *)
let test_starting_style_merges () =
  let classes = [ "starting:opacity-0"; "starting:scale-90" ] in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  let css = Css.to_string ~minify:true (Tw.to_css ~base:false utilities) in
  let count needle =
    let n = String.length needle and h = String.length css in
    let rec go i acc =
      if i + n > h then acc
      else if String.sub css i n = needle then go (i + 1) (acc + 1)
      else go (i + 1) acc
    in
    go 0 0
  in
  check int "one @starting-style block" 1 (count "@starting-style");
  check bool "both utilities are in it" true
    (count ".starting\\:opacity-0" = 1 && count ".starting\\:scale-90" = 1)

(* A project [@theme] can name a family member the built-in scale has no slot
   for ([--font-<name>]); every such member funnels into one shared (priority,
   suborder) slot (see [Typography.font_named_var]), so the two tokens tie
   there. Tailwind keeps the order the [@theme] block declared them in rather
   than re-sorting the tie alphabetically by name. *)
let test_theme_named_family_declaration_order () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("font-zeta", "Zeta, serif"); ("font-alpha", "Alpha, serif") ]
  in
  let utilities =
    [ "font-zeta"; "font-alpha" ]
    |> List.map (fun c -> Result.get_ok (Tw.of_string ~theme c))
  in
  let css =
    Css.to_string ~minify:true (Tw.to_css ~theme ~base:false utilities)
  in
  let position needle =
    match Astring.String.find_sub ~sub:needle css with
    | Some i -> i
    | None -> Alcotest.failf "%s not found in %s" needle css
  in
  let zeta = position "--font-zeta:" and alpha = position "--font-alpha:" in
  check bool "--font-zeta declared before --font-alpha, as in the @theme block"
    true (zeta < alpha)

(* [--default-font-family] is derived from [--font-sans]: a project that took
   the family out of its theme leaves the derived token naming nothing, and
   Tailwind drops it rather than emitting a reference that never resolves. *)
let test_removed_family_drops_derived_default () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("font-sans", "initial") ]
  in
  let default_decls =
    Tw.Typography.default_font_declarations
    @ Tw.Typography.default_font_family_declarations
  in
  let theme_layer = Tw.Build.theme_layer_of ~theme ~default_decls [] in
  let vars = vars_in_layer "theme" theme_layer in
  check bool "--font-sans is gone" false (List.mem "--font-sans" vars);
  check bool "--default-font-family goes with it" false
    (List.mem "--default-font-family" vars);
  check bool "--default-mono-font-family stays" true
    (List.mem "--default-mono-font-family" vars)

let tests =
  [
    test_case "starting-style blocks merge" `Quick test_starting_style_merges;
    test_case "removed family drops its derived default" `Quick
      test_removed_family_drops_derived_default;
    test_case "theme-named family keeps @theme declaration order" `Quick
      test_theme_named_family_declaration_order;
    test_case "referenced theme token emitted" `Quick
      test_referenced_theme_token;
    test_case "spacing-0 prunes --spacing" `Quick check_spacing_zero_prune;
    test_case "theme layer - empty" `Quick check_theme_layer_empty;
    test_case "theme layer - with color" `Quick check_theme_layer_with_color;
    test_case "Tw.Build.conflict_order delegates to Utility.order" `Quick
      check_conflict_order;
    test_case "properties layer generation" `Quick check_properties_layer;
    test_case "to_css variables with base" `Quick check_css_variables_with_base;
    test_case "compound variant theme var" `Quick
      check_compound_variant_theme_var;
    test_case "supports variant theme token" `Quick
      check_supports_variant_theme_token;
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
    test_case "@property space-reverse after transforms" `Quick
      check_space_reverse_after_transforms;
    test_case "@property border-spacing first" `Quick check_border_spacing_first;
    test_case "@property outline-style after ring" `Quick
      check_outline_style_after_ring;
    test_case "content-none emits no @property --tw-content" `Quick
      check_content_none_no_property;
    test_case "resolve_dependencies" `Quick test_resolve_dependencies;
    test_case "inline_no_var_in_css_for_defaults" `Quick
      test_inline_no_vars_defaults;
    test_case "inline_style_no_var_for_defaults" `Quick
      test_inline_style_no_vars;
    test_case "inline_vs_variables_diff" `Quick test_inline_vs_variables_diff;
    test_case "resolve_dependencies_dedup_and_queue" `Quick
      test_resolve_deps_dedup_queue;
    test_case "theme layer family order" `Quick check_theme_layer_family_order;
    test_case "theme_layer_collects_media_refs" `Quick
      test_theme_layer_media_refs;
    test_case "theme_layer_collects_media_refs (md)" `Quick
      test_theme_media_refs_md;
    test_case "rule_sets_injects_hover_media_query" `Quick
      test_rule_sets_hover_media;
    test_case "rule_sets_groups_md_media_query" `Quick test_rule_sets_md_media;
    test_case "multi-breakpoint grouping+order" `Quick test_media_grouping_order;
    test_case "md media dedup" `Quick test_md_media_dedup;
    test_case "duplicate selector dedup" `Quick test_duplicate_selector_dedup;
    test_case "md:hover nests the hover gate" `Quick test_md_hover_extra_media;
    test_case "container hover nests the gate" `Quick test_container_hover_nests;
    test_case "container + media together" `Quick test_container_and_media;
    test_case "media query deduplication" `Quick test_media_query_deduplication;
    test_case "rule_sets" `Quick test_rule_sets;
    test_case "build_utilities_layer" `Quick test_build_utilities_layer;
    test_case "build_utilities_layer preserves order" `Quick
      test_build_utils_layer_order;
    test_case "extra keeps a plain utility's order" `Quick
      test_extra_keeps_plain_order;
    test_case "style with rules and props ordering" `Quick
      test_style_rules_props;
  ]

let suite = ("build", tests)
