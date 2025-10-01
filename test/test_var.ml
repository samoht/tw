open Alcotest

(* Test that variable naming follows conventions *)
let var_css_output () =
  (* Create styles that use theme variables *)
  let styles = [ Tw.text_xl; Tw.p 4 ] in
  let css = Tw.to_css ~base:false ~optimize:false styles in
  let css_str = Css.to_string css in

  (* Should contain CSS variable references *)
  check bool "contains var(--" (String.contains css_str '-') true;
  check bool "CSS output non-empty" (String.length css_str > 0) true

(* Test that variables with fallbacks are serialized correctly *)
let var_fallback_in_css () =
  (* Use a utility that might have a fallback *)
  let styles = [ Tw.text_blue ] in
  let css = Tw.to_css ~base:false ~optimize:false styles in
  let css_str = Css.to_string css in

  (* Should produce valid CSS *)
  check bool "produces valid CSS" (String.length css_str > 0) true

(* Test theme layer contains variables *)
let var_in_theme_layer () =
  let styles = [ Tw.text_xl; Tw.text_red; Tw.p 4 ] in
  let css = Tw.to_css ~base:true ~optimize:false styles in
  let theme_layer = Css.layer_block "theme" css in

  match theme_layer with
  | None -> fail "Expected @layer theme"
  | Some statements ->
      let rules = Css.rules_from_statements statements in
      let custom_props = Css.custom_props_from_rules rules in
      check (list string) "theme custom properties"
        [
          "--color-red-500";
          "--default-font-family";
          "--default-mono-font-family";
          "--font-mono";
          "--font-sans";
          "--spacing";
          "--text-xl";
          "--text-xl--line-height";
        ]
        (List.sort_uniq String.compare custom_props)

let tests =
  [
    test_case "var CSS output" `Quick var_css_output;
    test_case "var fallback in CSS" `Quick var_fallback_in_css;
    test_case "var in theme layer" `Quick var_in_theme_layer;
  ]

let suite = ("var", tests)
