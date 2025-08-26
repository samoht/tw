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
  | [ Regular (selector, _) ] -> check string "correct selector" ".p-4" selector
  | _ -> fail "Expected Regular rule"

let check_extract_hover () =
  let rules = extract_selector_props (hover [ bg blue 500 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Regular (selector, _) ] ->
      check bool "selector contains hover" true (contains selector ":hover")
  | _ -> fail "Expected Regular rule with hover"

let check_extract_responsive () =
  let rules = extract_selector_props (sm [ p 4 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Media_query (condition, selector, _) ] ->
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
  (* Test that properties layer is created when needed *)
  (* When shadow utilities are used, they reference unassigned composition variables *)
  let rules =
    [
      Css.rule ~selector:".shadow-sm"
        [
          Css.custom_property "--tw-shadow" "0 1px 2px 0 rgb(0 0 0 / 0.05)";
          Css.custom_property ~deps:[ "--tw-shadow-color" ]
            "--tw-shadow-colored" "0 1px 2px 0 var(--tw-shadow-color)";
          (* box-shadow uses composition: it references --tw-ring-offset-shadow
             and --tw-ring-shadow which aren't assigned here *)
          Css.box_shadow
            "var(--tw-ring-offset-shadow, 0 0 #0000), var(--tw-ring-shadow, 0 \
             0 #0000), var(--tw-shadow)";
        ];
    ]
  in
  let layer_opt, at_props = compute_properties_layer rules in

  (* Properties layer should be created when composition variables are
     referenced but not assigned *)
  let should_have_layer =
    (* We reference --tw-shadow-color but don't assign it, so should create
       layer *)
    true
  in

  check bool "properties layer created when needed" should_have_layer
    (layer_opt <> None);
  check bool "no @property rules for tw composition vars" true
    (List.length at_props = 0)

let check_to_css_reset () =
  let css = Tw.Rules.to_css ~reset:true [] in
  let css_str = Css.to_string ~minify:false css in
  (* Check that reset rules are included *)
  check bool "includes reset rules" true (contains css_str "*, :after, :before");
  check bool "has theme layer" true (contains css_str "@layer theme");
  check bool "has base layer" true (contains css_str "@layer base");
  check bool "has utilities layer" true (contains css_str "@layer utilities")

let check_to_css_no_reset () =
  let css = Tw.Rules.to_css ~reset:false [ p 4 ] in
  let css_str = Css.to_string ~minify:false css in
  (* No reset means no layers, just raw rules *)
  check bool "no theme layer" false (contains css_str "@layer theme");
  check bool "has padding rule" true (contains css_str ".p-4")

let check_inline_style () =
  let style = Tw.Rules.to_inline_style [ p 4; m 2; bg blue 500 ] in
  check bool "has padding" true (contains style "padding");
  check bool "has margin" true (contains style "margin");
  check bool "has background-color" true (contains style "background")

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
    test_case "to_css with reset" `Quick check_to_css_reset;
    test_case "to_css without reset" `Quick check_to_css_no_reset;
    test_case "inline style generation" `Quick check_inline_style;
  ]

let suite = ("rules", tests)
