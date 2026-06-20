module Css = Cascade.Css
(* Tests for prose typography utilities *)

open Tw

let test_classes () =
  Alcotest.(check string) "prose base class" "prose" (pp prose);
  Alcotest.(check string) "prose-sm class" "prose-sm" (pp prose_sm);
  Alcotest.(check string) "prose-lg class" "prose-lg" (pp prose_lg);
  Alcotest.(check string) "prose-xl class" "prose-xl" (pp prose_xl);
  Alcotest.(check string) "prose-2xl class" "prose-2xl" (pp prose_2xl);
  Alcotest.(check string) "prose-gray class" "prose-gray" (pp prose_gray);
  Alcotest.(check string) "prose-slate class" "prose-slate" (pp prose_slate)

let test_combinations () =
  let combined = to_classes [ prose; prose_lg ] in
  Alcotest.(check string) "prose with size" "prose prose-lg" combined;

  let with_theme = to_classes [ prose; prose_slate ] in
  Alcotest.(check string) "prose with theme" "prose prose-slate" with_theme;

  let complex = to_classes [ prose; prose_xl; prose_gray; mx_auto ] in
  Alcotest.(check string)
    "complex prose" "prose prose-xl prose-gray mx-auto" complex

let test_css_generation () =
  (* Test that prose generates CSS rules *)
  let css = to_css [ prose ] in
  let css_string = Css.to_string css in

  (* Check that CSS variables are included *)
  Alcotest.(check bool)
    "has prose body variable" true
    (Astring.String.is_infix ~affix:"--tw-prose-body" css_string);

  (* Check that prose class is generated *)
  Alcotest.(check bool)
    "has prose class" true
    (Astring.String.is_infix ~affix:".prose" css_string);

  (* Check that descendant selectors are generated *)
  Alcotest.(check bool)
    "has prose h1 selector" true
    (Astring.String.is_infix
       ~affix:".prose :where(h1):not(:where([class~=\"not-prose\"]" css_string)

let test_inline_styles () =
  (* Prose utilities can generate inline styles from their rules, but CSS
     variables are filtered out *)
  let inline = to_inline_style [ prose ] in
  Alcotest.(check bool)
    "prose generates inline styles" true
    (String.length inline > 0);

  (* Check that CSS variables are filtered out - no "--" should appear *)
  let has_css_vars = Astring.String.is_infix ~affix:"--" inline in
  Alcotest.(check bool) "no CSS variables in inline styles" false has_css_vars;

  (* Color variants only set CSS variables, so they have no inline styles *)
  let inline_gray = to_inline_style [ prose_gray ] in
  Alcotest.(check string) "prose-gray has no inline styles" "" inline_gray

(* prose-invert remaps the palette to the inverted vars; prose-orange overrides
   the link accent colours. Both used to be no-ops / unknown. *)
let test_color_variants () =
  let invert = Css.to_string (to_css ~base:false [ prose_invert ]) in
  Alcotest.(check bool)
    "prose-invert remaps body to the invert var" true
    (Astring.String.is_infix ~affix:"var(--tw-prose-invert-body)" invert);
  let orange = Css.to_string (to_css ~base:false [ prose_orange ]) in
  Alcotest.(check bool)
    "prose-orange sets the link accent" true
    (Astring.String.is_infix ~affix:"--tw-prose-links" orange
    && Astring.String.is_infix ~affix:"--tw-prose-invert-links" orange)

let suite =
  ( "prose",
    [
      Alcotest.test_case "classes" `Quick test_classes;
      Alcotest.test_case "color variants" `Quick test_color_variants;
      Alcotest.test_case "combinations" `Quick test_combinations;
      Alcotest.test_case "CSS generation" `Quick test_css_generation;
      Alcotest.test_case "inline styles" `Quick test_inline_styles;
    ] )
